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

;; default token charset. users can change `company-ofc-token-charset` before loading this plugin.
;; these 3 variables are buffer-local.
(defvar company-ofc-token-charset  "0-9a-zA-Z_")
(defvar company-ofc-token-pattern (concat "[" company-ofc-token-charset "]+"))
(defvar company-ofc-token-delim (concat "[^" company-ofc-token-charset "]+"))

;; -----------------------------------------------------------------------------
;; struct definitions

;; `freq` is the used frequency of `token` in its life time
;; `loc-list` is a list of filepath(s) where this token occurs. `loc` is short for `location`
(cl-defstruct company-ofc--candidate-s token freq loc-list)

;; `candidate` is a `company-ofc--candidate-s` instance
;; `edis` is the edit distance between the token of `candidate` and the current input
;; `matched-region-list` is a list: `((start1 . end1) (start2 . end2) ...)`
(cl-defstruct company-ofc--matched-item-info-s candidate edis matched-region-list)

;; `info-list` is a list of `company-ofc--matched-item-info-s` instances
(cl-defstruct company-ofc--matched-item-s downcased-input info-list)

;; -----------------------------------------------------------------------------
;; global variables

(define-hash-table-test 'company-ofc--hash-strcmp 'string= 'sxhash-equal)

;; a hash table containing all tokens: token => `company-ofc--candidate-s`
(defvar company-ofc--token-hash (make-hash-table :test 'company-ofc--hash-strcmp))

;; a hash table containing candidates' of a file: filepath => candidate-list
(defvar company-ofc--filepath2candidates (make-hash-table :test 'company-ofc--hash-strcmp))

;; each element of this stack is a `company-ofc--matched-item-s` instance
(defvar company-ofc--matched-item-stack '())

;; -----------------------------------------------------------------------------

(defun company-ofc--find-candidate-in-list (token candidate-list)
  (cl-dolist (candidate candidate-list)
    (when (string= token (company-ofc--candidate-s-token candidate))
      (cl-return candidate))))

(defun company-ofc--buffer2string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun company-ofc--for-each-token-in-buffer (buffer callback-func)
  (cl-dolist (token (split-string (company-ofc--buffer2string buffer) company-ofc-token-delim t))
    (funcall callback-func token)))

(defun company-ofc--find-or-insert-token-hash (token file-path)
  "finds or inserts a `company-ofc--candidate-s` instance of the specified token in `company-ofc--token-hash`,
 and returns that instance."
  (let ((candidate (gethash token company-ofc--token-hash)))
    (when (not candidate)
      (setq candidate (make-company-ofc--candidate-s :token token :freq 0 :loc-list '()))
      (puthash token candidate company-ofc--token-hash))
    (let ((loc-list (company-ofc--candidate-s-loc-list candidate)))
      (setf (company-ofc--candidate-s-loc-list candidate) (append loc-list (list file-path))))
    candidate))

(defun company-ofc--add-buffer-tokens (buffer)
  (let ((file-path (buffer-file-name buffer)))
    (when file-path
      (let ((dedup (make-hash-table :test 'company-ofc--hash-strcmp))
            (file-candidate-list '()))
        (company-ofc--for-each-token-in-buffer buffer
                                               (lambda (token)
                                                 (when (>= (length token) company-ofc-min-token-len)
                                                   (let ((found (gethash token dedup)))
                                                     (when (not found)
                                                       (let ((candidate (company-ofc--find-or-insert-token-hash token file-path)))
                                                         (setq file-candidate-list (append file-candidate-list (list candidate))))
                                                       (puthash token t dedup))))))
        (puthash file-path file-candidate-list company-ofc--filepath2candidates)))))

(defun company-ofc--init ()
  (setq-local company-ofc-token-pattern (concat "[" company-ofc-token-charset "]+"))
  (setq-local company-ofc-token-delim (concat "[^" company-ofc-token-charset "]+"))
  (company-ofc--add-buffer-tokens (current-buffer)))

(defun company-ofc--remove-buffer-tokens (buffer)
  (let* ((file-path (buffer-file-name buffer))
         (file-candidate-list (gethash file-path company-ofc--filepath2candidates)))
    (when file-candidate-list
      (mapc (lambda (candidate)
              (let ((loc-list (company-ofc--candidate-s-loc-list candidate)))
                (setf (company-ofc--candidate-s-loc-list candidate) (delete file-path loc-list))))
            file-candidate-list)
      (remhash file-path company-ofc--filepath2candidates))))

(add-hook 'after-save-hook
          (lambda ()
            (setq company-ofc--matched-item-stack '()) ;; clear matched stack
            (let ((buffer (current-buffer)))
              (company-ofc--remove-buffer-tokens buffer)
              (company-ofc--add-buffer-tokens buffer))))

(add-hook 'kill-buffer-hook (lambda ()
                              (company-ofc--remove-buffer-tokens (current-buffer))))

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
  (let ((item (car company-ofc--matched-item-stack)))
    (cl-dolist (info (company-ofc--matched-item-s-info-list item))
      (let* ((candidate (company-ofc--matched-item-info-s-candidate info))
             (ctoken (company-ofc--candidate-s-token candidate)))
        (when (string= token ctoken)
          (let ((file-path (car (company-ofc--candidate-s-loc-list candidate))))
            (cl-return (concat "[" (file-name-nondirectory file-path) "]"))))))))

(defun company-ofc--record-matched-region (text-idx matched-region-list)
  "updates the matched regions in the form of `((start1 . end1) (start2 . end2))` and returns it."
  (let ((last-region (car (last matched-region-list))))
    ;; merge current matched position to previous if they are adjacent
    (if (and last-region
             (= text-idx (cdr last-region)))
        (progn
          (cl-incf (cdr last-region))
          matched-region-list)
      (append matched-region-list (list (cons text-idx (+ 1 text-idx)))))))

(defun company-ofc--generate-matched-item-info-list-from-scratch (downcased-input input-length)
  "creates a list of `company-ofc--matched-item-info-s` instances."
  (let ((info-list '()))
    (maphash (lambda (token candidate)
               (let ((loc-list (company-ofc--candidate-s-loc-list candidate)))
                 (if (= 0 (length loc-list))
                     (remhash token company-ofc--token-hash) ;; remhash inside maphash is ok
                   (let ((downcased-token (downcase token))
                         (token-length (length token))
                         (matched-region-list '()))
                     (when (company-ofc--do-fuzzy-compare
                            downcased-input input-length downcased-token token-length
                            (lambda (text-idx)
                              (setq matched-region-list (company-ofc--record-matched-region text-idx matched-region-list))))
                       (setq info-list (append info-list (list (make-company-ofc--matched-item-info-s
                                                                :candidate candidate
                                                                :edis 0
                                                                :matched-region-list matched-region-list)))))))))
             company-ofc--token-hash)
    info-list))

(defun company-ofc--generate-matched-item-info-list-from-another (downcased-input input-length another-info-list)
  "creates a new `company-ofc--matched-item-info-s` list from another."
  (let ((info-list '()))
    (mapc (lambda (info)
            (let* ((candidate (company-ofc--matched-item-info-s-candidate info))
                   (downcased-token (downcase (company-ofc--candidate-s-token candidate)))
                   (token-length (length downcased-token))
                   (matched-region-list '()))
              (when (company-ofc--do-fuzzy-compare
                     downcased-input input-length downcased-token token-length
                     (lambda (text-idx)
                       (setq matched-region-list (company-ofc--record-matched-region text-idx matched-region-list))))
                (setq info-list (append info-list
                                        (list (make-company-ofc--matched-item-info-s
                                               :candidate candidate
                                               :edis 0
                                               :matched-region-list matched-region-list)))))))
          another-info-list)
    info-list))

(defun company-ofc--find-matched-item-in-stack (input-length)
  (cl-dolist (item company-ofc--matched-item-stack)
    (let ((pre-substr (company-ofc--matched-item-s-downcased-input item)))
      ;; better to use this predicate, but a len-based one is also ok here
      ;; (company-ofc--do-fuzzy-compare pre-substr (length pre-substr) downcased-input input-length)
      (when (< (length pre-substr) input-length)
        (cl-return item)))))

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

(defun company-ofc--sort-matched-item-info-list (input input-length matched-info-list)
  "sort matched infos by their edit-distances and used-frequencies."
  ;; calc edit distance for each candidate
  (cl-dolist (info matched-info-list)
    (let* ((candidate (company-ofc--matched-item-info-s-candidate info))
           (token (company-ofc--candidate-s-token candidate)))
      (setf (company-ofc--matched-item-info-s-edis info)
            (company-ofc--calc-edit-distance input input-length
                                             token (length token)))))
  (cl-stable-sort matched-info-list
                  (lambda (a b)
                    (let ((freq-a (company-ofc--candidate-s-freq (company-ofc--matched-item-info-s-candidate a)))
                          (freq-b (company-ofc--candidate-s-freq (company-ofc--matched-item-info-s-candidate b))))
                      (if (> freq-a freq-b)
                          t
                        (when (= freq-a freq-b)
                          (< (company-ofc--matched-item-info-s-edis a) (company-ofc--matched-item-info-s-edis b))))))))

(defun company-ofc--find-candidates (input)
  "returns a list of matched strings."
  (let ((input-length (length input)))
    (if (< input-length company-ofc-min-token-len)
        (setq company-ofc--matched-item-stack '()) ;; clear matched candidates
      (let ((downcased-input (downcase input))
            (matched-item (company-ofc--find-matched-item-in-stack input-length))
            (info-list '()))
        (if matched-item
            (setq info-list (company-ofc--generate-matched-item-info-list-from-another downcased-input input-length
                                                                                       (company-ofc--matched-item-s-info-list matched-item)))
          (setq info-list (company-ofc--generate-matched-item-info-list-from-scratch downcased-input input-length)))
        (when info-list
          (setq info-list (company-ofc--sort-matched-item-info-list input input-length info-list))
          (push (make-company-ofc--matched-item-s :downcased-input downcased-input :info-list info-list)
                company-ofc--matched-item-stack)
          (mapcar (lambda (info)
                    (let ((candidate (company-ofc--matched-item-info-s-candidate info)))
                      (company-ofc--candidate-s-token candidate)))
                  info-list))))))

(defun company-ofc--grab-suffix (pattern)
  (when (looking-at pattern)
    (match-string 0)))

(defun company-ofc--grab-prefix (pattern)
  (when (looking-back pattern (line-beginning-position) t)
    (match-string 0)))

(defun company-ofc--generic-list-filter (list predicate)
  (cl-dolist (element list)
    (when (funcall predicate element)
      (cl-return element))))

(defun company-ofc--post-completion (token)
  ;; update frequency of the matched candidate
  (let ((item (car company-ofc--matched-item-stack)))
    (company-ofc--generic-list-filter (company-ofc--matched-item-s-info-list item)
                                      (lambda (info)
                                        (let ((candidate (company-ofc--matched-item-info-s-candidate info)))
                                          (when (string= (company-ofc--candidate-s-token candidate) token)
                                            (cl-incf (company-ofc--candidate-s-freq candidate))
                                            t)))))
  ;; remove overlapping suffix
  (let ((suffix (company-ofc--grab-suffix company-ofc-token-pattern)))
    (when suffix
      (let ((suffix-length (length suffix)))
        (when (company-ofc--do-fuzzy-compare (downcase suffix) suffix-length
                                             (downcase token) (length token))
          (delete-char suffix-length)))))
  ;; clear matched candidates
  (setq company-ofc--matched-item-stack '()))

(defun company-ofc--get-matched-info (token)
  (let ((item (car company-ofc--matched-item-stack)))
    (cl-dolist (info (company-ofc--matched-item-s-info-list item))
      (let* ((candidate (company-ofc--matched-item-info-s-candidate info))
             (ctoken (company-ofc--candidate-s-token candidate)))
        (when (string= ctoken token)
          (cl-return (company-ofc--matched-item-info-s-matched-region-list info)))))))

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
