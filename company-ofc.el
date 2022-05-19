;; -*- lexical-binding: t -*-

(require 'cl-lib)

;; -----------------------------------------------------------------------------
;; settings

(defgroup company-ofc nil
  "fuzzy completion backends for company-mode of emacs"
  :prefix "company-ofc-"
  :link '(info-link "(emacs)company-ofc")
  :group 'matching)

(defcustom company-ofc-min-token-len 4
  "minimum length to trigger completion"
  :type 'integer)

(defcustom company-ofc-debug nil
  "toggle this flag to get internal details for debugging"
  :type 'boolean)

(defcustom company-ofc-token-charset "0-9a-zA-Z_"
  "valid token characters in regexp"
  :type 'string)

(defconst company-ofc-token-pattern (concat "[" company-ofc-token-charset "]+"))
(defconst company-ofc-token-delim (concat "[^" company-ofc-token-charset "]+"))

;; -----------------------------------------------------------------------------
;; struct definitions

;; `filename` is where this token occurs
;; `freq` is the used frequency of `token` in its life time
;; `edis` is the edit distance between `token` and current input and is modifed on the fly
;; `matched-regions` is a list of matched regions `((start1 . end1) (start2 . end2) ...)`
(cl-defstruct company-ofc--candidate-s token filename freq edis matched-regions)

;; `candidate-list` is a list of `company-ofc--candidate-s` instances
(cl-defstruct company-ofc--matched-info-s downcased-input candidate-list)

;; -----------------------------------------------------------------------------
;; global variables

(define-hash-table-test 'company-ofc--kv-map-cmp-func (lambda (a b) (string= a b)) 'sxhash)
(defvar company-ofc--filename2hash (make-hash-table :test 'company-ofc--kv-map-cmp-func))

(defvar company-ofc--matched-info-stack '())

;; -----------------------------------------------------------------------------

(defun company-ofc--generic-list-find (list predicate)
  (cl-dolist (element list)
    (if (funcall predicate element)
        (cl-return element))))

(defun company-ofc--find-candidates-in-list (token candidate-list)
  (company-ofc--generic-list-find candidate-list
                                  (lambda (candidate)
                                    (string= token (company-ofc--candidate-s-token candidate)))))

(defun company-ofc--find-or-insert-candidate-list (candidate-list token freq filename)
  "callers should gurantee that `candidate-list` is not null"
  (let ((candidate (car candidate-list)))
    (if (string= token (company-ofc--candidate-s-token candidate))
        candidate
      (let ((rest (cdr candidate-list)))
        (if (null rest)
            (setcdr candidate-list (list (make-company-ofc--candidate-s :token token :freq freq :filename filename)))
          (company-ofc--find-or-insert-candidate-list rest token freq filename))))))

(defun company-ofc--buffer2string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun company-ofc--for-each-token-in-buffer (buffer callback-func)
  (dolist (token (split-string (company-ofc--buffer2string buffer) company-ofc-token-delim t))
    (funcall callback-func token)))

(defun company-ofc--add-token-to-hash (token freq filename file-hash)
  (let ((key (downcase token)))
    (let ((candidate-list (gethash key file-hash nil)))
      (if (not candidate-list)
          (puthash key (list (make-company-ofc--candidate-s :token token :freq freq :filename filename)) file-hash)
        (company-ofc--find-or-insert-candidate-list candidate-list token freq filename)))))

(defun company-ofc--make-hash-for-buffer (buffer)
  (let ((file-path (buffer-file-name buffer)))
    (when file-path
      (let ((filename (file-name-nondirectory file-path))
            (file-hash (make-hash-table :test 'company-ofc--kv-map-cmp-func)))
        (company-ofc--for-each-token-in-buffer buffer
                                               (lambda (token)
                                                 (when (>= (length token) company-ofc-min-token-len)
                                                   (company-ofc--add-token-to-hash token 0 filename file-hash))))
        (puthash file-path file-hash company-ofc--filename2hash)))))

(defun company-ofc--init ()
  (company-ofc--make-hash-for-buffer (current-buffer)))

(defun company-ofc--update-buffer-hash (buffer file-path old-file-hash new-file-hash)
  "rebuilds file hashes, but keeps `freq` of each token"
  (let ((token-filename (file-name-nondirectory file-path)))
    (company-ofc--for-each-token-in-buffer
     buffer
     (lambda (token)
       (let ((old-candidate-list (gethash (downcase token) old-file-hash nil)))
         (if old-candidate-list
             (let ((old-candidate (company-ofc--find-candidates-in-list token old-candidate-list)))
               (if old-candidate
                   (company-ofc--add-token-to-hash (company-ofc--candidate-s-token old-candidate)
                                                   (company-ofc--candidate-s-freq old-candidate)
                                                   token-filename
                                                   new-file-hash)
                 (company-ofc--add-token-to-hash token 0 token-filename new-file-hash)))
           (company-ofc--add-token-to-hash token 0 token-filename new-file-hash)))))))

(add-hook 'after-save-hook
          (lambda ()
            (setq company-ofc--matched-info-stack '()) ;; clear matched candidates
            (let ((file-path (buffer-file-name)))
              (let ((old-file-hash (gethash file-path company-ofc--filename2hash nil)))
                (if (not old-file-hash)
                    (company-ofc--make-hash-for-buffer (current-buffer))
                  (let ((new-file-hash (make-hash-table :test 'company-ofc--kv-map-cmp-func)))
                    (company-ofc--update-buffer-hash (current-buffer) file-path old-file-hash new-file-hash)
                    (puthash file-path new-file-hash company-ofc--filename2hash)))))))

(add-hook 'kill-buffer-hook (lambda ()
                              (remhash (buffer-file-name) company-ofc--filename2hash)))

(defun company-ofc--do-fuzzy-compare (pattern pattern-length text text-length &optional matched-hook-func)
  "tells if `pattern` is part of `text`."
  (when (<= pattern-length text-length)
    (let ((text-idx 0)
          (pattern-idx 0))
      (while (and (< text-idx text-length)
                  (< pattern-idx pattern-length))
        (when (eq (elt text text-idx) (elt pattern pattern-idx))
          (when matched-hook-func
            (funcall matched-hook-func text-idx))
          (cl-incf pattern-idx))
        (cl-incf text-idx))
      (= pattern-idx pattern-length))))

(defun company-ofc--get-annotation (token)
  (let ((c (company-ofc--find-candidates-in-list
            token
            (company-ofc--matched-info-s-candidate-list (car company-ofc--matched-info-stack)))))
    (when c
      (if company-ofc-debug
          (format " -> freq: [%d], edis: [%d], file: [%s]"
                  (company-ofc--candidate-s-freq c)
                  (company-ofc--candidate-s-edis c)
                  (company-ofc--candidate-s-filename c))
        (concat "[" (company-ofc--candidate-s-filename c) "]")))))

(defun company-ofc--record-matched-region (text-idx matched-regions)
  "updates the matched regions in the form of `((start1 . end1) (start2 . end2))` and returns it."
  (let ((last-region (car (last matched-regions))))
    ;; merge current matched position to previous if they are adjacent
    (if (and last-region
             (= text-idx (cdr last-region)))
        (progn
          (cl-incf (cdr last-region))
          matched-regions)
      (append matched-regions (list (cons text-idx (+ 1 text-idx)))))))

(defun company-ofc--find-candidates-from-scratch (downcased-input input-length)
  (let ((candidate-result '()))
    (maphash (lambda (file-path file-hash)
               (maphash (lambda (token candidate-list)
                          (let ((token-length (length token))
                                (matched-regions '()))
                            (when (company-ofc--do-fuzzy-compare
                                   downcased-input input-length token token-length
                                   (lambda (text-idx)
                                     (setq matched-regions (company-ofc--record-matched-region text-idx matched-regions))))
                              (cl-dolist (element candidate-list)
                                (setf (company-ofc--candidate-s-matched-regions element) matched-regions))
                              (setq candidate-result (append candidate-list candidate-result)))))
                        file-hash))
             company-ofc--filename2hash)
    candidate-result))

(defun company-ofc--find-candidates-from-list (downcased-input input-length candidate-list)
  (let ((candidate-result '()))
    (dolist (candidate candidate-list)
      (let ((token (company-ofc--candidate-s-token candidate))
            (matched-regions '()))
        (when (company-ofc--do-fuzzy-compare
               downcased-input input-length (downcase token) (length token)
               (lambda (text-idx)
                 (setq matched-regions (company-ofc--record-matched-region text-idx matched-regions))))
          (setf (company-ofc--candidate-s-matched-regions candidate) matched-regions)
          (setq candidate-result (append candidate-result (list candidate))))))
    candidate-result))

(defun company-ofc--find-matched-candidate-in-stack (downcased-input input-length)
  (company-ofc--generic-list-find company-ofc--matched-info-stack
                                  (lambda (matched-candidate)
                                    (let ((prefix (company-ofc--matched-info-s-downcased-input matched-candidate)))
                                      ;; better to use this predicate, but a len-based one is ok here
                                      ;; (company-ofc--do-fuzzy-compare prefix (length prefix) downcased-input input-length)
                                      (< (length prefix) input-length)))))

(defun company-ofc--do-calc-edit-distance (a a-len b b-len)
  (let ((a-seq (number-sequence 1 a-len))
        (b-seq (number-sequence 1 b-len))
        (dis-vec (make-vector (+ 1 b-len) 0)))
    (cl-dolist (i b-seq)
      (aset dis-vec i i))
    (cl-dolist (i a-seq)
      (let ((old (- i 1)))
        (aset dis-vec 0 i)
        (cl-dolist (j b-seq)
          (let ((tmp (aref dis-vec j)))
            (if (eq (elt a (- i 1))
                    (elt b (- j 1)))
                (aset dis-vec j old)
              (aset dis-vec j (+ 1 (min (aref dis-vec j)
                                        (aref dis-vec (- j 1))
                                        old))))
            (setq old tmp)))))
    (aref dis-vec b-len)))

(defun company-ofc--calc-edit-distance (a a-len b b-len)
  (if (< a-len b-len)
      (company-ofc--do-calc-edit-distance b b-len a a-len)
    (company-ofc--do-calc-edit-distance a a-len b b-len)))

(defun company-ofc--sort-candidates (input input-length candidate-result)
  "sort candidates by their edit-distances and used-frequencies"
  ;; calc edit distance for each candidate
  (cl-dolist (candidate candidate-result)
    (let ((token (company-ofc--candidate-s-token candidate)))
      (setf (company-ofc--candidate-s-edis candidate)
            (company-ofc--calc-edit-distance input input-length
                                             token (length token)))))
  (cl-stable-sort candidate-result
                  (lambda (a b)
                    (let ((freq-a (company-ofc--candidate-s-freq a))
                          (freq-b (company-ofc--candidate-s-freq b)))
                      (if (> freq-a freq-b)
                          t
                        (when (= freq-a freq-b)
                          (< (company-ofc--candidate-s-edis a) (company-ofc--candidate-s-edis b))))))))

(defun company-ofc--find-candidates (input)
  (let ((input-length (length input)))
    (if (< input-length company-ofc-min-token-len)
        (setq company-ofc--matched-info-stack '()) ;; clear matched candidates
      (let ((downcased-input (downcase input)))
        (let ((matched-candidate (company-ofc--find-matched-candidate-in-stack downcased-input input-length))
              (candidate-result '()))
          (if matched-candidate
              (setq candidate-result
                    (company-ofc--find-candidates-from-list downcased-input input-length
                                                            (company-ofc--matched-info-s-candidate-list matched-candidate)))
            (setq candidate-result
                  (company-ofc--find-candidates-from-scratch downcased-input input-length)))
          (when candidate-result
            (setq candidate-result (company-ofc--sort-candidates input input-length candidate-result))
            (push (make-company-ofc--matched-info-s :downcased-input downcased-input
                                                    :candidate-list candidate-result)
                  company-ofc--matched-info-stack)
            (delete-dups (mapcar 'company-ofc--candidate-s-token candidate-result))))))))

(defun company-ofc--grab-suffix (pattern)
  (when (looking-at pattern)
    (match-string 0)))

(defun company-ofc--grab-prefix (pattern)
  (when (looking-back pattern (line-beginning-position) t)
    (match-string 0)))

(defun company-ofc--post-completion (token)
  ;; update matched candidates of each file
  (dolist (candidate (company-ofc--matched-info-s-candidate-list (car company-ofc--matched-info-stack)))
    (when (string= token (company-ofc--candidate-s-token candidate))
      (cl-incf (company-ofc--candidate-s-freq candidate))))
  ;; remove overlapping suffix
  (let ((suffix (company-ofc--grab-suffix company-ofc-token-pattern)))
    (when suffix
      (let ((suffix-length (length suffix)))
        (when (company-ofc--do-fuzzy-compare (downcase suffix) suffix-length
                                             (downcase token) (length token))
          (delete-char suffix-length)))))
  ;; clear matched candidates
  (setq company-ofc--matched-info-stack '()))

(defun company-ofc--get-matched-info (token)
  (when company-ofc--matched-info-stack
    (let ((candidate (company-ofc--generic-list-find
                      (company-ofc--matched-info-s-candidate-list (car company-ofc--matched-info-stack))
                      (lambda (candidate)
                        (string= token (company-ofc--candidate-s-token candidate))))))
      (when candidate
        (company-ofc--candidate-s-matched-regions candidate)))))

;; -----------------------------------------------------------------------------

(defun company-ofc (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (init (company-ofc--init))
    (prefix (company-ofc--grab-prefix company-ofc-token-pattern))
    (candidates (company-ofc--find-candidates arg))
    (match (company-ofc--get-matched-info arg))
    (annotation (company-ofc--get-annotation arg))
    (post-completion (company-ofc--post-completion arg))
    (sorted t) ;; tell company not to sort the result again
    (no-cache t)))

(provide 'company-ofc)
