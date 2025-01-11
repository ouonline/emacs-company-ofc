;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'company-ofc-common "./company-ofc-common.el")

;; -----------------------------------------------------------------------------
;; settings

(defgroup company-ofc nil
  "fuzzy completion backends for company-mode of emacs"
  :prefix "company-ofc-"
  :link '(info-link "(emacs)company-ofc")
  :group 'matching)

(defcustom company-ofc-token-min-trigger-len 3
  "minimum length to trigger completion"
  :type 'integer)

(defcustom company-ofc-token-min-len 4
  "minimum length of tokens that are indexed"
  :type 'integer)

;; the following variables are buffer-local.

;; default token charset. users can change `company-ofc-token-charset` after loading this plugin.
(defcustom company-ofc-token-charset  "0-9a-zA-Z_"
  "valid path characters in regexp"
  :type 'string)

(defvar company-ofc-token-pattern (concat "[" company-ofc-token-charset "]+"))
(defvar company-ofc-token-delim (concat "[^" company-ofc-token-charset "]+"))
(defvar company-ofc-token-enabled nil)

;; -----------------------------------------------------------------------------
;; struct definitions

;; `freq` is the used frequency of `token` in its life time
;; `loc-list` is a list of buffer(s) where this token occurs. `loc` is short for `location`
(cl-defstruct company-ofc--token-candidate-s token freq loc-list)

;; `candidate` is a `company-ofc--token-candidate-s` instance
;; `edis` is the edit distance between the token of `candidate` and the current input
;; `matched-region-list` is a list: `((start1 . end1) (start2 . end2) ...)`
(cl-defstruct company-ofc--token-matched-item-info-s candidate edis matched-region-list)

;; `info-list` is a list of `company-ofc--token-matched-item-info-s` instances
(cl-defstruct company-ofc--token-matched-item-s downcased-input info-list)

;; -----------------------------------------------------------------------------
;; global variables

(define-hash-table-test 'company-ofc--token-hash-strcmp 'string= 'sxhash-equal)

;; a hash table containing all tokens: token => `company-ofc--token-candidate-s`
(defvar company-ofc--token-hash (make-hash-table :test 'company-ofc--token-hash-strcmp))

;; a hash table containing candidates' of a buffer: buffer => candidate-list
(defvar company-ofc--token-buffer2candidates (make-hash-table :test 'company-ofc--token-hash-strcmp))

;; each element of this stack is a `company-ofc--token-matched-item-s` instance
(defvar company-ofc--token-matched-item-stack '())

;; -----------------------------------------------------------------------------

(defun company-ofc--token-find-candidate-in-list (token candidate-list)
  (cl-dolist (candidate candidate-list)
    (when (string= token (company-ofc--token-candidate-s-token candidate))
      (cl-return candidate))))

(defun company-ofc--token-buffer2string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun company-ofc--token-for-each-token-in-buffer (buffer callback-func)
  (cl-dolist (token (split-string (company-ofc--token-buffer2string buffer) company-ofc-token-delim t))
    (funcall callback-func token)))

(defun company-ofc--token-find-or-insert-token-hash (token buffer)
  "finds or inserts a `company-ofc--token-candidate-s` instance of the specified token in
 `company-ofc--token-hash` and returns that instance."
  (let ((candidate (gethash token company-ofc--token-hash)))
    (when (not candidate)
      (setq candidate (make-company-ofc--token-candidate-s :token token :freq 0 :loc-list '()))
      (puthash token candidate company-ofc--token-hash))
    (let ((loc-list (company-ofc--token-candidate-s-loc-list candidate)))
      ;; `buffer` is guranteed unique in `loc-list` by the caller
      (setf (company-ofc--token-candidate-s-loc-list candidate) (append loc-list (list buffer))))
    candidate))

(defun company-ofc--token-remove-token-from-token-hash (token buffer)
  (let ((candidate (gethash token company-ofc--token-hash)))
    (when candidate
      (let ((loc-list (company-ofc--token-candidate-s-loc-list candidate)))
        (setq loc-list (delete buffer loc-list))
        (if (eq nil loc-list)
            (remhash token company-ofc--token-hash)
          (setf (company-ofc--token-candidate-s-loc-list candidate) loc-list))))))

(defun company-ofc--token-update-buffer-tokens (buffer)
  (when (> (length (buffer-name buffer)) 0)
    (let ((new-token-set (make-hash-table :test 'company-ofc--token-hash-strcmp)))
      ;; construct a new-token-set for the modified buffer
      (company-ofc--token-for-each-token-in-buffer buffer
                                                   (lambda (token)
                                                     (puthash token t new-token-set)))
      (let ((old-candidate-list (gethash buffer company-ofc--token-buffer2candidates '()))
            (new-candidate-list '()))
        (cl-dolist (candidate old-candidate-list)
          (let* ((token (company-ofc--token-candidate-s-token candidate))
                 (found (gethash token new-token-set)))
            (if found
                ;; if a token in the new-token-set exists in the old one, delete it from new-token-set
                ;; so that it remains unchanged in `company-ofc--token-hash`
                (progn
                  (remhash token new-token-set)
                  (setq new-candidate-list (append new-candidate-list (list candidate))))
              ;; otherwise remove it from `company-ofc--token-hash`
              (company-ofc--token-remove-token-from-token-hash token buffer))))
        (maphash (lambda (token unused)
                   (let ((candidate (company-ofc--token-find-or-insert-token-hash token buffer)))
                     (setq new-candidate-list (append new-candidate-list (list candidate)))))
                 new-token-set)
        (puthash buffer new-candidate-list company-ofc--token-buffer2candidates)))))

(defun company-ofc--token-init ()
  (setq-local company-ofc-token-pattern (concat "[" company-ofc-token-charset "]+"))
  (setq-local company-ofc-token-delim (concat "[^" company-ofc-token-charset "]+"))
  (setq-local company-ofc-token-enabled t)
  (company-ofc--token-update-buffer-tokens (current-buffer)))

(defun company-ofc--token-destroy-buffer-tokens (buffer)
  (let ((candidate-list (gethash buffer company-ofc--token-buffer2candidates)))
    (when candidate-list
      (mapc (lambda (candidate)
              (let ((loc-list (company-ofc--token-candidate-s-loc-list candidate)))
                (setq loc-list (delete buffer loc-list))
                (if (eq nil loc-list)
                    (remhash (company-ofc--token-candidate-s-token candidate) company-ofc--token-hash)
                  (setf (company-ofc--token-candidate-s-loc-list candidate) loc-list))))
            candidate-list)
      (remhash buffer company-ofc--token-buffer2candidates))))

(add-hook 'after-save-hook
          (lambda ()
            (when company-ofc-token-enabled
              (setq company-ofc--token-matched-item-stack '()) ;; clear matched stack
              (company-ofc--token-update-buffer-tokens (current-buffer)))))

(add-hook 'kill-buffer-hook (lambda ()
                              (when company-ofc-token-enabled
                                (company-ofc--token-destroy-buffer-tokens (current-buffer)))))

(defun company-ofc--token-get-annotation (token)
  (let ((item (car company-ofc--token-matched-item-stack)))
    (cl-dolist (info (company-ofc--token-matched-item-s-info-list item))
      (let* ((candidate (company-ofc--token-matched-item-info-s-candidate info))
             (ctoken (company-ofc--token-candidate-s-token candidate)))
        (when (string= token ctoken)
          (let ((buffer (car (company-ofc--token-candidate-s-loc-list candidate))))
            (cl-return (concat "[" (buffer-name buffer) "]"))))))))

(defun company-ofc--token-record-matched-region (text-idx matched-region-list)
  "updates the matched regions in the form of `((start1 . end1) (start2 . end2))` and returns it."
  (let ((last-region (car (last matched-region-list))))
    ;; merge current matched position to previous if they are adjacent
    (if (and last-region
             (= text-idx (cdr last-region)))
        (progn
          (cl-incf (cdr last-region))
          matched-region-list)
      (append matched-region-list (list (cons text-idx (+ 1 text-idx)))))))

(defun company-ofc--token-generate-matched-item-info-list-from-scratch (downcased-input input-length)
  "creates a list of `company-ofc--token-matched-item-info-s` instances."
  (let ((info-list '()))
    (maphash (lambda (token candidate)
               (let ((downcased-token (downcase token))
                     (token-length (length token))
                     (matched-region-list '()))
                 (when (company-ofc--fuzzy-compare
                        downcased-input input-length downcased-token token-length
                        (lambda (text-idx)
                          (setq matched-region-list (company-ofc--token-record-matched-region text-idx matched-region-list))))
                   (setq info-list (append info-list (list (make-company-ofc--token-matched-item-info-s
                                                            :candidate candidate
                                                            :edis 0
                                                            :matched-region-list matched-region-list)))))))
             company-ofc--token-hash)
    info-list))

(defun company-ofc--token-generate-matched-item-info-list-from-another (downcased-input input-length another-info-list)
  "creates a new `company-ofc--token-matched-item-info-s` list from another."
  (let ((info-list '()))
    (mapc (lambda (info)
            (let* ((candidate (company-ofc--token-matched-item-info-s-candidate info))
                   (downcased-token (downcase (company-ofc--token-candidate-s-token candidate)))
                   (token-length (length downcased-token))
                   (matched-region-list '()))
              (when (company-ofc--fuzzy-compare
                     downcased-input input-length downcased-token token-length
                     (lambda (text-idx)
                       (setq matched-region-list (company-ofc--token-record-matched-region text-idx matched-region-list))))
                (setq info-list (append info-list
                                        (list (make-company-ofc--token-matched-item-info-s
                                               :candidate candidate
                                               :edis 0
                                               :matched-region-list matched-region-list)))))))
          another-info-list)
    info-list))

(defun company-ofc--token-find-matched-item-in-stack (input-length)
  (cl-dolist (item company-ofc--token-matched-item-stack)
    (let ((pre-substr (company-ofc--token-matched-item-s-downcased-input item)))
      ;; better to use this predicate, but a len-based one is also ok here
      ;; (company-ofc--fuzzy-compare pre-substr (length pre-substr) downcased-input input-length)
      (when (< (length pre-substr) input-length)
        (cl-return item)))))

(defun company-ofc--token-do-calc-edit-distance (a a-len b b-len)
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

(defun company-ofc--token-calc-edit-distance (a a-len b b-len)
  (if (< a-len b-len)
      (company-ofc--token-do-calc-edit-distance b b-len a a-len)
    (company-ofc--token-do-calc-edit-distance a a-len b b-len)))

(defun company-ofc--token-sort-matched-item-info-list (input input-length matched-info-list)
  "sort matched infos by their edit-distances and used-frequencies."
  ;; calc edit distance for each candidate
  (cl-dolist (info matched-info-list)
    (let* ((candidate (company-ofc--token-matched-item-info-s-candidate info))
           (token (company-ofc--token-candidate-s-token candidate)))
      (setf (company-ofc--token-matched-item-info-s-edis info)
            (company-ofc--token-calc-edit-distance input input-length
                                                   token (length token)))))
  (cl-stable-sort matched-info-list
                  (lambda (a b)
                    (let ((freq-a (company-ofc--token-candidate-s-freq (company-ofc--token-matched-item-info-s-candidate a)))
                          (freq-b (company-ofc--token-candidate-s-freq (company-ofc--token-matched-item-info-s-candidate b))))
                      (if (> freq-a freq-b)
                          t
                        (when (= freq-a freq-b)
                          (< (company-ofc--token-matched-item-info-s-edis a) (company-ofc--token-matched-item-info-s-edis b))))))))

(defun company-ofc--token-find-candidates (input)
  "returns a list of matched strings."
  (let ((input-length (length input)))
    (if (< input-length company-ofc-token-min-trigger-len)
        (setq company-ofc--token-matched-item-stack '()) ;; clear matched candidates
      (let ((downcased-input (downcase input))
            (matched-item (company-ofc--token-find-matched-item-in-stack input-length))
            (info-list '()))
        (if matched-item
            (setq info-list (company-ofc--token-generate-matched-item-info-list-from-another downcased-input input-length
                                                                                             (company-ofc--token-matched-item-s-info-list matched-item)))
          (setq info-list (company-ofc--token-generate-matched-item-info-list-from-scratch downcased-input input-length)))
        (when info-list
          (setq info-list (company-ofc--token-sort-matched-item-info-list input input-length info-list))
          (push (make-company-ofc--token-matched-item-s :downcased-input downcased-input :info-list info-list)
                company-ofc--token-matched-item-stack)
          (mapcar (lambda (info)
                    (let ((candidate (company-ofc--token-matched-item-info-s-candidate info)))
                      (company-ofc--token-candidate-s-token candidate)))
                  info-list))))))

(defun company-ofc--token-grab-suffix (pattern)
  (when (looking-at pattern)
    (match-string 0)))

(defun company-ofc--token-grab-prefix (pattern)
  (when (looking-back pattern (line-beginning-position) t)
    (match-string 0)))

(defun company-ofc--token-generic-list-filter (list predicate)
  (cl-dolist (element list)
    (when (funcall predicate element)
      (cl-return element))))

(defun company-ofc--token-post-completion (token)
  ;; update frequency of the matched candidate
  (let ((item (car company-ofc--token-matched-item-stack)))
    (company-ofc--token-generic-list-filter (company-ofc--token-matched-item-s-info-list item)
                                            (lambda (info)
                                              (let ((candidate (company-ofc--token-matched-item-info-s-candidate info)))
                                                (when (string= (company-ofc--token-candidate-s-token candidate) token)
                                                  (cl-incf (company-ofc--token-candidate-s-freq candidate))
                                                  t)))))
  ;; remove overlapping suffix
  (let ((suffix (company-ofc--token-grab-suffix company-ofc-token-pattern)))
    (when suffix
      (let ((suffix-length (length suffix)))
        (when (company-ofc--fuzzy-compare (downcase suffix) suffix-length
                                          (downcase token) (length token))
          (delete-char suffix-length)))))
  ;; clear matched candidates
  (setq company-ofc--token-matched-item-stack '()))

(defun company-ofc--token-get-matched-info (token)
  (let ((item (car company-ofc--token-matched-item-stack)))
    (cl-dolist (info (company-ofc--token-matched-item-s-info-list item))
      (let* ((candidate (company-ofc--token-matched-item-info-s-candidate info))
             (ctoken (company-ofc--token-candidate-s-token candidate)))
        (when (string= ctoken token)
          (cl-return (company-ofc--token-matched-item-info-s-matched-region-list info)))))))

;; -----------------------------------------------------------------------------

(defun company-ofc-token (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (init (company-ofc--token-init))
    (prefix (company-ofc--token-grab-prefix company-ofc-token-pattern))
    (candidates (company-ofc--token-find-candidates arg))
    (match (company-ofc--token-get-matched-info arg))
    (annotation (company-ofc--token-get-annotation arg))
    (post-completion (company-ofc--token-post-completion arg))
    (sorted t) ;; tell company not to sort the result again
    (no-cache t)))

(provide 'company-ofc-token)
