;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ofc-common "./ofc-common.el")

;; ------------------------------------------------------------------------- ;;
;; settings

;; minimum length to trigger completion
(defvar ofc-token-min-trigger-len 3)

;; minimum length of tokens that are indexed
(defvar ofc-token-min-len 4)

;; ------------------------------------------------------------------------- ;;
;; struct definitions

;; `used-freq' is the used frequency of `token' in its life time.
;; `buffer-list' is a list of buffer(s) where this token occurs.
(cl-defstruct ofc-token--token-info-s token used-freq buffer-list)

;; `candidate-list' is a list of tokens, each of which has the following text properties:
;;   * `token-info': an instance of `ofc-token--token-info-s'.
;;   * `edit-distance': the edit distance between this token and the current input.
;;   * `matched-region-list': a list with the form `((begin1 . end1) (begin2 . end2) ...)`
(cl-defstruct ofc-token--matched-item-s downcased-input candidate-list)

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

(defun ofc-token--for-each-token-in-buffer (buffer callback-func)
  (with-current-buffer buffer
    (let ((end (point-max)))
      (save-excursion
        (goto-char (point-min))
        (while (< (point) end)
          (skip-syntax-forward " ") ;; skip over whitespace and other non-symbol characters
          (let ((symbol-start (point)))
            (skip-syntax-forward "w_")
            (let ((symbol-end (point)))
              (if (> symbol-end symbol-start)
                  (funcall callback-func (buffer-substring-no-properties symbol-start symbol-end))
                (when (< symbol-end end)
                  (forward-char))))))))))

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

(defun ofc-token--do-gen-candidate (downcased-input input-length token token-info)
  "returns a matched candidate-token or nil."
  (let ((downcased-token (downcase token))
        (token-length (length token))
        (matched-region-list '()))
    (when (ofc--fuzzy-compare
           downcased-input input-length downcased-token token-length
           (lambda (text-idx)
             (setq matched-region-list (ofc-token--record-matched-region text-idx matched-region-list))))
      (let ((new-candidate-token (substring-no-properties token)))
        (add-text-properties 0 token-length (list :token-info token-info
                                                  :edit-distance 0
                                                  :matched-region-list matched-region-list)
                             new-candidate-token)
        new-candidate-token))))

(defun ofc-token--generate-candidate-list-from-scratch (downcased-input input-length)
  "creates a list of token candidates."
  (let ((candidate-list '()))
    (maphash (lambda (token token-info)
               (let ((new-candidate-token (ofc-token--do-gen-candidate downcased-input input-length
                                                                       token token-info)))
                 (when new-candidate-token
                   (setq candidate-list (append candidate-list (list new-candidate-token))))))
             ofc-token--token-hash)
    candidate-list))

(defun ofc-token--generate-candidate-list-from-another (downcased-input input-length another-candidate-list)
  "creates a new candidate list from another."
  (let ((candidate-list '()))
    (mapc (lambda (candidate-token)
            (let* ((token-info (get-text-property 0 :token-info candidate-token))
                   (new-candidate-token (ofc-token--do-gen-candidate downcased-input input-length
                                                                     candidate-token token-info)))
              (when new-candidate-token
                (setq candidate-list (append candidate-list (list new-candidate-token))))))
          another-candidate-list)
    candidate-list))

(defun ofc-token--find-matched-item-in-stack (input-length)
  (cl-dolist (item ofc-token--matched-stack)
    (let ((pre-substr (ofc-token--matched-item-s-downcased-input item)))
      ;; better to use this predicate, but a len-based one is also ok here
      ;; (ofc--fuzzy-compare pre-substr (length pre-substr) downcased-input input-length)
      (when (< (length pre-substr) input-length)
        (cl-return item)))))

(defun ofc-token--sort-candidate-list (input input-length candidate-list)
  "sort matched infos by their edit-distances and used-frequencies."
  (cl-dolist (candidate-token candidate-list)
    (let ((token-length (length candidate-token)))
      (put-text-property 0 token-length
                         :edit-distance (ofc--calc-edit-distance input input-length
                                                                 candidate-token token-length)
                         candidate-token)))
  (cl-stable-sort candidate-list
                  (lambda (a b)
                    (let ((freq-a (ofc-token--token-info-s-used-freq (get-text-property 0 :token-info a)))
                          (freq-b (ofc-token--token-info-s-used-freq (get-text-property 0 :token-info b))))
                      (if (> freq-a freq-b)
                          t
                        (when (= freq-a freq-b)
                          (< (get-text-property 0 :edit-distance a)
                             (get-text-property 0 :edit-distance b))))))))

(defun ofc-token--find-candidates (input)
  (let ((input-length (length input)))
    (if (< input-length ofc-token-min-trigger-len)
        (setq ofc-token--matched-stack '()) ;; clear matched items
      (let ((downcased-input (downcase input))
            (matched-item (ofc-token--find-matched-item-in-stack input-length))
            (candidate-list '()))
        (if matched-item
            (setq candidate-list (ofc-token--generate-candidate-list-from-another downcased-input input-length
                                                                                  (ofc-token--matched-item-s-candidate-list matched-item)))
          (setq candidate-list (ofc-token--generate-candidate-list-from-scratch downcased-input input-length)))
        (when candidate-list
          (when (> (length candidate-list) 1)
            (setq candidate-list (ofc-token--sort-candidate-list input input-length candidate-list)))
          (push (make-ofc-token--matched-item-s :downcased-input downcased-input :candidate-list candidate-list)
                ofc-token--matched-stack)
          candidate-list)))))

(defun ofc-token--grab-prefix ()
  (buffer-substring-no-properties (point)
                                  (save-excursion (skip-syntax-backward "w_")
                                                  (point))))

(defun ofc-token--grab-suffix ()
  (buffer-substring-no-properties (point)
                                  (save-excursion (skip-syntax-forward "w_")
                                                  (point))))

(defun ofc-token--post-completion (candidate-token)
  ;; update frequency of the matched token info
  (cl-incf (ofc-token--token-info-s-used-freq (get-text-property 0 :token-info candidate-token)))
  ;; remove overlapping suffix
  (let ((suffix (ofc-token--grab-suffix)))
    (when suffix
      (let ((suffix-length (length suffix)))
        (when (ofc--fuzzy-compare (downcase suffix) suffix-length
                                  (downcase candidate-token) (length candidate-token))
          (delete-char suffix-length)))))
  ;; clear matched token info
  (setq ofc-token--matched-stack '()))

(defun ofc-token--after-buffer-saved ()
  (setq ofc-token--matched-stack '()) ;; clear matched stack
  (ofc-token--update-buffer-tokens (current-buffer)))

(defun ofc-token--before-buffer-killed ()
  (ofc-token--destroy-buffer-tokens (current-buffer)))

(defun ofc-token--after-buffer-created ()
  (add-hook 'after-save-hook #'ofc-token--after-buffer-saved 0 t)
  (add-hook 'kill-buffer-hook #'ofc-token--before-buffer-killed 0 t)
  (ofc-token--update-buffer-tokens (current-buffer)))

(provide 'ofc-token)
