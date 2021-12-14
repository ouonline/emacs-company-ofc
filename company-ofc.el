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
(cl-defstruct candidate-s token filename freq edis matched-regions)

;; `candidate-list` is a list of `candidate-s` instances
(cl-defstruct matched-info-s downcased-input candidate-list)

;; -----------------------------------------------------------------------------
;; global variables

(define-hash-table-test 'ofc-kv-map-cmp-func (lambda (a b) (string= a b)) 'sxhash)
(defvar g-filename2hash (make-hash-table :test 'ofc-kv-map-cmp-func))

(defvar g-matched-info-stack '())

;; -----------------------------------------------------------------------------

(defun generic-list-find (list predicate)
  (cl-dolist (element list)
    (if (funcall predicate element)
        (cl-return element))))

(defun find-candidate-in-list (token candidate-list)
  (generic-list-find candidate-list
                     (lambda (candidate)
                       (string= token (candidate-s-token candidate)))))

(defun find-or-insert-candidate-list (candidate-list token freq filename)
  "callers should gurantee that `candidate-list` is not null"
  (let ((candidate (car candidate-list)))
    (if (string= token (candidate-s-token candidate))
        candidate
      (let ((rest (cdr candidate-list)))
        (if (null rest)
            (setcdr candidate-list (list (make-candidate-s :token token :freq freq :filename filename)))
          (find-or-insert-candidate-list rest token freq filename))))))

(defun buffer2string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun for-each-token-in-buffer (buffer callback-func)
  (dolist (token (split-string (buffer2string buffer) company-ofc-token-delim t))
    (funcall callback-func token)))

(defun add-token-to-hash (token freq filename file-hash)
  (let ((key (downcase token)))
    (let ((candidate-list (gethash key file-hash nil)))
      (if (not candidate-list)
          (puthash key (list (make-candidate-s :token token :freq freq :filename filename)) file-hash)
        (find-or-insert-candidate-list candidate-list token freq filename)))))

(defun make-hash-for-buffer (buffer)
  (let ((file-path (buffer-file-name buffer)))
    (when file-path
      (let ((filename (file-name-nondirectory file-path))
            (file-hash (make-hash-table :test 'ofc-kv-map-cmp-func)))
        (for-each-token-in-buffer buffer
                                  (lambda (token)
                                    (when (>= (length token) company-ofc-min-token-len)
                                      (add-token-to-hash token 0 filename file-hash))))
        (puthash file-path file-hash g-filename2hash)))))

(defun company-ofc-init ()
  (make-hash-for-buffer (current-buffer)))

(defun update-buffer-hash (buffer file-path old-file-hash new-file-hash)
  "rebuilds file hashes, but keeps `freq` of each token"
  (let ((token-filename (file-name-nondirectory file-path)))
    (for-each-token-in-buffer buffer
                              (lambda (token)
                                (let ((old-candidate-list (gethash (downcase token) old-file-hash nil)))
                                  (if old-candidate-list
                                      (let ((old-candidate (find-candidate-in-list token old-candidate-list)))
                                        (if old-candidate
                                            (add-token-to-hash (candidate-s-token old-candidate)
                                                               (candidate-s-freq old-candidate)
                                                               token-filename
                                                               new-file-hash)
                                          (add-token-to-hash token 0 token-filename new-file-hash)))
                                    (add-token-to-hash token 0 token-filename new-file-hash)))))))

(add-hook 'after-save-hook
          (lambda ()
            (setq g-matched-info-stack '()) ;; clear matched candidates
            (let ((file-path (buffer-file-name)))
              (let ((old-file-hash (gethash file-path g-filename2hash nil)))
                (if (not old-file-hash)
                    (make-hash-for-buffer (current-buffer))
                  (let ((new-file-hash (make-hash-table :test 'ofc-kv-map-cmp-func)))
                    (update-buffer-hash (current-buffer) file-path old-file-hash new-file-hash)
                    (puthash file-path new-file-hash g-filename2hash)))))))

(add-hook 'kill-buffer-hook (lambda ()
                              (remhash (buffer-file-name) g-filename2hash)))

(defun do-fuzzy-compare (pattern pattern-length text text-length &optional matched-hook-func)
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

(defun company-ofc-get-annotation (token)
  (let ((c (find-candidate-in-list token (matched-info-s-candidate-list (car g-matched-info-stack)))))
    (when c
      (if company-ofc-debug
          (format " -> freq: [%d], edis: [%d], file: [%s]"
                  (candidate-s-freq c) (candidate-s-edis c) (candidate-s-filename c))
        (concat "[" (candidate-s-filename c) "]")))))

(defun record-matched-region (text-idx matched-regions)
  "updates the matched regions in the form of `((start1 . end1) (start2 . end2))` and returns it."
  (let ((last-region (car (last matched-regions))))
    ;; merge current matched position to previous if they are adjacent
    (if (and last-region
             (= text-idx (cdr last-region)))
        (progn
          (cl-incf (cdr last-region))
          matched-regions)
      (append matched-regions (list (cons text-idx (+ 1 text-idx)))))))

(defun find-candidate-from-scratch (downcased-input input-length)
  (let ((candidate-result '()))
    (maphash (lambda (file-path file-hash)
               (maphash (lambda (token candidate-list)
                          (let ((token-length (length token))
                                (matched-regions '()))
                            (when (do-fuzzy-compare downcased-input input-length token token-length
                                                    (lambda (text-idx)
                                                      (setq matched-regions (record-matched-region text-idx matched-regions))))
                              (cl-dolist (element candidate-list)
                                (setf (candidate-s-matched-regions element) matched-regions))
                              (setq candidate-result (append candidate-list candidate-result)))))
                        file-hash))
             g-filename2hash)
    candidate-result))

(defun find-candidate-from-list (downcased-input input-length candidate-list)
  (let ((candidate-result '()))
    (dolist (candidate candidate-list)
      (let ((token (candidate-s-token candidate))
            (matched-regions '()))
        (when (do-fuzzy-compare downcased-input input-length (downcase token) (length token)
                                (lambda (text-idx)
                                  (setq matched-regions (record-matched-region text-idx matched-regions))))
          (setf (candidate-s-matched-regions candidate) matched-regions)
          (setq candidate-result (append candidate-result (list candidate))))))
    candidate-result))

(defun find-matched-candidate-in-stack (downcased-input input-length)
  (generic-list-find g-matched-info-stack
                     (lambda (matched-candidate)
                       (let ((prefix (matched-info-s-downcased-input matched-candidate)))
                         ;; better to use this predicate, but a len-based one is ok here
                         ;; (do-fuzzy-compare prefix (length prefix) downcased-input input-length)
                         (< (length prefix) input-length)))))

(defun do-calc-edit-distance (a a-len b b-len)
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

(defun calc-edit-distance (a a-len b b-len)
  (if (< a-len b-len)
      (do-calc-edit-distance b b-len a a-len)
    (do-calc-edit-distance a a-len b b-len)))

(defun sort-candidates (input input-length candidate-result)
  "sort candidates by their edit-distances and used-frequencies"
  ;; calc edit distance for each candidate
  (cl-dolist (candidate candidate-result)
    (let ((token (candidate-s-token candidate)))
      (setf (candidate-s-edis candidate)
            (calc-edit-distance input input-length
                                token (length token)))))
  (cl-stable-sort candidate-result
                  (lambda (a b)
                    (let ((freq-a (candidate-s-freq a))
                          (freq-b (candidate-s-freq b)))
                      (if (> freq-a freq-b)
                          t
                        (when (= freq-a freq-b)
                          (< (candidate-s-edis a) (candidate-s-edis b))))))))

(defun company-ofc-find-candidate (input)
  (let ((input-length (length input)))
    (if (< input-length company-ofc-min-token-len)
        (setq g-matched-info-stack '()) ;; clear matched candidates
      (let ((downcased-input (downcase input)))
        (let ((matched-candidate (find-matched-candidate-in-stack downcased-input input-length))
              (candidate-result '()))
          (if matched-candidate
              (setq candidate-result
                    (find-candidate-from-list downcased-input input-length
                                              (matched-info-s-candidate-list matched-candidate)))
            (setq candidate-result
                  (find-candidate-from-scratch downcased-input input-length)))
          (when candidate-result
            (setq candidate-result (sort-candidates input input-length candidate-result))
            (push (make-matched-info-s :downcased-input downcased-input
                                       :candidate-list candidate-result)
                  g-matched-info-stack)
            (delete-dups (mapcar 'candidate-s-token candidate-result))))))))

(defun company-ofc-grab-suffix (pattern)
  (when (looking-at pattern)
    (match-string 0)))

(defun company-ofc-grab-prefix (pattern)
  (when (looking-back pattern (line-beginning-position) t)
    (match-string 0)))

(defun company-ofc-post-completion (token)
  ;; update matched candidates of each file
  (dolist (candidate (matched-info-s-candidate-list (car g-matched-info-stack)))
    (when (string= token (candidate-s-token candidate))
      (cl-incf (candidate-s-freq candidate))))
  ;; remove overlapping suffix
  (let ((suffix (company-ofc-grab-suffix company-ofc-token-pattern)))
    (when suffix
      (let ((suffix-length (length suffix)))
        (when (do-fuzzy-compare (downcase suffix) suffix-length
                                (downcase token) (length token))
          (delete-char suffix-length)))))
  ;; clear matched candidates
  (setq g-matched-info-stack '()))

(defun company-ofc-get-matched-info (token)
  (when g-matched-info-stack
    (let ((candidate (generic-list-find (matched-info-s-candidate-list (car g-matched-info-stack))
                                        (lambda (candidate)
                                          (string= token (candidate-s-token candidate))))))
      (when candidate
        (candidate-s-matched-regions candidate)))))

;; -----------------------------------------------------------------------------

(defun company-ofc (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (init (company-ofc-init))
    (prefix (company-ofc-grab-prefix company-ofc-token-pattern))
    (candidates (company-ofc-find-candidate arg))
    (match (company-ofc-get-matched-info arg))
    (annotation (company-ofc-get-annotation arg))
    (post-completion (company-ofc-post-completion arg))
    (sorted t) ;; tell company not to sort the result again
    (no-cache t)))

(provide 'company-ofc)
