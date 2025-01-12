(require 'cl-lib)
(require 'ofc-common "./ofc-common.el")

;; ------------------------------------------------------------------------- ;;
;; settings

;; minimum length to trigger completion
(defvar ofc-token-min-trigger-len 3)

;; minimum length of tokens that are indexed
(defvar ofc-token-min-len 4)

;; default token charset. users can change `ofc-token-charset' after loading this plugin.
(defvar ofc-token-charset  "0-9a-zA-Z_")

(defvar ofc-token-pattern (concat "[" ofc-token-charset "]+"))
(defvar ofc-token-delim (concat "[^" ofc-token-charset "]+"))

;; ------------------------------------------------------------------------- ;;
;; struct definitions

;; `used-freq' is the used frequency of `token' in its life time.
;; `buffer-list' is a list of buffer(s) where this token occurs.
(cl-defstruct ofc-token--token-info-s token used-freq buffer-list)

;; `token-info' is an instance of `ofc-token--token-info-s'.
;; `edit-distance' is the edit distance between this token and the current input.
;; `matched-region-list' is a list with the form `((begin1 . end1) (begin2 . end2) ...)`
(cl-defstruct ofc-token--matched-item-info-s token-info edit-distance matched-region-list)

;; `info-list' is a list of `ofc-token--matched-item-info-s' instances.
(cl-defstruct ofc-token--matched-item-s downcased-input info-list)

;; ------------------------------------------------------------------------- ;;
;; global variables

(define-hash-table-test 'ofc-token--token-hash-strcmp 'string= 'sxhash-equal)

;; a hash table containing all tokens: token => `ofc-token--token-info-s'
(defvar ofc-token--token-hash (make-hash-table :test 'ofc-token--token-hash-strcmp))

(define-hash-table-test 'ofc--buffer-cmp 'string= 'sxhash-equal)

;; a hash table containing all `ofc-token--token-info-s' of a buffer: buffer => (list `ofc-token--token-info-s' ...)
(defvar ofc-token--buffer2tokens (make-hash-table :test 'ofc--buffer-cmp))

;; each element of this stack is an instance of `ofc-token--matched-item-s'.
(defvar ofc-token--matched-stack '())

;; ------------------------------------------------------------------------- ;;

(defun ofc-token--buffer2string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun ofc-token--for-each-token-in-buffer (buffer callback-func)
  (cl-dolist (token (split-string (ofc-token--buffer2string buffer) ofc-token-delim t))
    (funcall callback-func token)))

(defun ofc-token--find-or-insert-token-hash (token buffer)
  "finds or inserts an instance of `ofc-token--token-info-s' for the specified token in
 `ofc-token--token-hash' and returns that instance."
  (let ((token-info (gethash token ofc-token--token-hash)))
    (when (not token-info)
      (setq token-info (make-ofc-token--token-info-s :token token :used-freq 0 :buffer-list '()))
      (puthash token token-info ofc-token--token-hash))
    (let ((buffer-list (ofc-token--token-info-s-buffer-list token-info)))
      ;; `buffer` is guranteed unique in `buffer-list` by the caller
      (setf (ofc-token--token-info-s-buffer-list token-info) (append buffer-list (list buffer))))
    token-info))

(defun ofc-token--remove-token-from-token-hash (token buffer)
  (let ((token-info (gethash token ofc-token--token-hash)))
    (when token-info
      (let ((buffer-list (ofc-token--token-info-s-buffer-list token-info)))
        (setq buffer-list (delete buffer buffer-list))
        (if (eq nil buffer-list)
            (remhash token ofc-token--token-hash)
          (setf (ofc-token--token-info-s-buffer-list token-info) buffer-list))))))

(defun ofc-token--update-buffer-tokens (buffer)
  (when (> (length (buffer-name buffer)) 0)
    (let ((new-token-set (make-hash-table :test 'ofc-token--token-hash-strcmp)))
      ;; construct a new-token-set for the modified buffer
      (ofc-token--for-each-token-in-buffer buffer
                                           (lambda (token)
                                             (when (>= (length token) ofc-token-min-len)
                                               (puthash token t new-token-set))))
      (let ((old-token-info-list (gethash buffer ofc-token--buffer2tokens '()))
            (new-token-info-list '()))
        (cl-dolist (token-info old-token-info-list)
          (let* ((token (ofc-token--token-info-s-token token-info))
                 (found (gethash token new-token-set)))
            (if found
                ;; if a token in the new-token-set exists in the old one, delete it from new-token-set
                ;; so that it remains unchanged in `ofc-token--token-hash`
                (progn
                  (remhash token new-token-set)
                  (setq new-token-info-list (append new-token-info-list (list token-info))))
              ;; otherwise remove it from `ofc-token--token-hash`
              (ofc-token--remove-token-from-token-hash token buffer))))
        (maphash (lambda (token _)
                   (let ((token-info (ofc-token--find-or-insert-token-hash token buffer)))
                     (setq new-token-info-list (append new-token-info-list (list token-info)))))
                 new-token-set)
        (puthash buffer new-token-info-list ofc-token--buffer2tokens)))))

(defun ofc-token--destroy-buffer-tokens (buffer)
  (let ((token-info-list (gethash buffer ofc-token--buffer2tokens)))
    (when token-info-list
      (mapc (lambda (token-info)
              (let ((buffer-list (ofc-token--token-info-s-buffer-list token-info)))
                (setq buffer-list (delete buffer buffer-list))
                (if (eq nil buffer-list)
                    (remhash (ofc-token--token-info-s-token token-info) ofc-token--token-hash)
                  (setf (ofc-token--token-info-s-buffer-list token-info) buffer-list))))
            token-info-list)
      (remhash buffer ofc-token--buffer2tokens))))

(defun ofc-token--after-buffer-saved ()
  (setq ofc-token--matched-stack '()) ;; clear matched stack
  (ofc-token--update-buffer-tokens (current-buffer)))

(add-hook 'after-save-hook #'ofc-token--after-buffer-saved 0 t)

(defun ofc-token--before-buffer-killed ()
  (ofc-token--destroy-buffer-tokens (current-buffer)))

(add-hook 'kill-buffer-hook #'ofc-token--before-buffer-killed 0 t)

(defun ofc-token--record-matched-region (text-idx matched-region-list)
  "updates the matched regions in the form of `((begin1 . end1) (begin2 . end2))`
 and returns it."
  (let ((last-region (car (last matched-region-list))))
    ;; merge current matched position to previous if they are adjacent
    (if (and last-region
             (= text-idx (cdr last-region)))
        (progn
          (cl-incf (cdr last-region))
          matched-region-list)
      (append matched-region-list (list (cons text-idx (+ 1 text-idx)))))))

(defun ofc-token--generate-matched-item-info-list-from-scratch (downcased-input input-length)
  "creates a list of `ofc-token--matched-item-info-s` instances."
  (let ((info-list '()))
    (maphash (lambda (token token-info)
               (let ((downcased-token (downcase token))
                     (token-length (length token))
                     (matched-region-list '()))
                 (when (ofc--fuzzy-compare
                        downcased-input input-length downcased-token token-length
                        (lambda (text-idx)
                          (setq matched-region-list (ofc-token--record-matched-region text-idx matched-region-list))))
                   (setq info-list (append info-list (list (make-ofc-token--matched-item-info-s
                                                            :token-info token-info
                                                            :edit-distance 0
                                                            :matched-region-list matched-region-list)))))))
             ofc-token--token-hash)
    info-list))

(defun ofc-token--generate-matched-item-info-list-from-another (downcased-input input-length another-info-list)
  "creates a new `ofc-token--matched-item-info-s` list from another."
  (let ((info-list '()))
    (mapc (lambda (info)
            (let* ((token-info (ofc-token--matched-item-info-s-token-info info))
                   (downcased-token (downcase (ofc-token--token-info-s-token token-info)))
                   (token-length (length downcased-token))
                   (matched-region-list '()))
              (when (ofc--fuzzy-compare
                     downcased-input input-length downcased-token token-length
                     (lambda (text-idx)
                       (setq matched-region-list (ofc-token--record-matched-region text-idx matched-region-list))))
                (setq info-list (append info-list
                                        (list (make-ofc-token--matched-item-info-s
                                               :token-info token-info
                                               :edit-distance 0
                                               :matched-region-list matched-region-list)))))))
          another-info-list)
    info-list))

(defun ofc-token--find-matched-item-in-stack (input-length)
  (cl-dolist (item ofc-token--matched-stack)
    (let ((pre-substr (ofc-token--matched-item-s-downcased-input item)))
      ;; better to use this predicate, but a len-based one is also ok here
      ;; (ofc--fuzzy-compare pre-substr (length pre-substr) downcased-input input-length)
      (when (< (length pre-substr) input-length)
        (cl-return item)))))

(defun ofc-token--do-calc-edit-distance (a a-len b b-len)
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

(defun ofc-token--calc-edit-distance (a a-len b b-len)
  (if (< a-len b-len)
      (ofc-token--do-calc-edit-distance b b-len a a-len)
    (ofc-token--do-calc-edit-distance a a-len b b-len)))

(defun ofc-token--sort-matched-item-info-list (input input-length matched-item-info-list)
  "sort matched infos by their edit-distances and used-frequencies."
  (cl-dolist (info matched-item-info-list)
    (let* ((token-info (ofc-token--matched-item-info-s-token-info info))
           (token (ofc-token--token-info-s-token token-info)))
      (setf (ofc-token--matched-item-info-s-edit-distance info)
            (ofc-token--calc-edit-distance input input-length
                                           token (length token)))))
  (cl-stable-sort matched-item-info-list
                  (lambda (a b)
                    (let ((freq-a (ofc-token--token-info-s-used-freq (ofc-token--matched-item-info-s-token-info a)))
                          (freq-b (ofc-token--token-info-s-used-freq (ofc-token--matched-item-info-s-token-info b))))
                      (if (> freq-a freq-b)
                          t
                        (when (= freq-a freq-b)
                          (< (ofc-token--matched-item-info-s-edit-distance a)
                             (ofc-token--matched-item-info-s-edit-distance b))))))))

(defun ofc-token--grab-suffix (pattern)
  (when (looking-at pattern)
    (match-string 0)))

(defun ofc-token--do-post-completion (token-info)
  ;; update frequency of the matched token info
  (cl-incf (ofc-token--token-info-s-used-freq token-info))
  ;; remove overlapping suffix
  (let ((suffix (ofc-token--grab-suffix ofc-token-pattern)))
    (when suffix
      (let ((token (ofc-token--token-info-s-token token-info))
            (suffix-length (length suffix)))
        (when (ofc--fuzzy-compare (downcase suffix) suffix-length
                                  (downcase token) (length token))
          (delete-char suffix-length)))))
  ;; clear matched token info
  (setq ofc-token--matched-stack '()))

(defun ofc-token--grab-prefix (pattern)
  (when (looking-back pattern (line-beginning-position) t)
    (match-string 0)))

(defun ofc-token--do-find-candidates (input create-candidate-func)
  (let ((input-length (length input)))
    (if (< input-length ofc-token-min-trigger-len)
        (setq ofc-token--matched-stack '()) ;; clear matched items
      (let ((downcased-input (downcase input))
            (matched-item (ofc-token--find-matched-item-in-stack input-length))
            (info-list '()))
        (if matched-item
            (setq info-list (ofc-token--generate-matched-item-info-list-from-another downcased-input input-length
                                                                                     (ofc-token--matched-item-s-info-list matched-item)))
          (setq info-list (ofc-token--generate-matched-item-info-list-from-scratch downcased-input input-length)))
        (when info-list
          (setq info-list (ofc-token--sort-matched-item-info-list input input-length info-list))
          (push (make-ofc-token--matched-item-s :downcased-input downcased-input :info-list info-list)
                ofc-token--matched-stack)
          (mapcar (lambda (info)
                    (funcall create-candidate-func info))
                  info-list))))))

(defun ofc-token--after-buffer-created ()
  (when (local-variable-p 'ofc-token-charset)
    (setq-local ofc-token-pattern (concat "[" ofc-token-charset "]+"))
    (setq-local ofc-token-delim (concat "[^" ofc-token-charset "]+")))
  (ofc-token--update-buffer-tokens (current-buffer)))

(provide 'ofc-token)
