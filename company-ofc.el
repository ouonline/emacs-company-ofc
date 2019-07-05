(require 'cl-lib)

;; -----------------------------------------------------------------------------
;; constants

(setq company-ofc-token-char-set "0-9a-zA-Z_")
(setq company-ofc-token-pattern (concat "[" company-ofc-token-char-set "]+"))
(setq company-ofc-token-delim (concat "[^" company-ofc-token-char-set "]+"))
(setq company-ofc-min-token-len 4)

;; -----------------------------------------------------------------------------
;; variables used across functions

(define-hash-table-test 'ofc-kv-map-cmp-func (lambda (a b) (string= a b)) 'sxhash)
(setq g-filename2hash (make-hash-table :test 'ofc-kv-map-cmp-func))

(setq g-candidate-list '())

;; -----------------------------------------------------------------------------
;; typedef

(cl-defstruct candidate-s token freq desc)

;; -----------------------------------------------------------------------------

(defun find-candidate-in-list (token candidate-list)
  (if (null candidate-list)
      nil
    (let ((element (car candidate-list)))
      (if (string= token (candidate-s-token element))
          element
        (find-candidate-in-list token (cdr candidate-list))))))

;; make sure that candidate-list is not null
(defun find-or-insert-candidate-list (candidate-list token freq desc)
  (let ((candidate (car candidate-list)))
    (if (string= token (candidate-s-token candidate))
        candidate
      (let ((rest (cdr candidate-list)))
        (if (null rest)
            (setcdr candidate-list (list (make-candidate-s :token token :freq freq :desc desc)))
          (find-or-insert-candidate-list rest token freq desc))))))

(defun buffer2string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun for-each-token-in-buffer (buffer separator token-callback-func)
  (mapcar token-callback-func
          (split-string (buffer2string buffer) separator t)))

(defun add-token-to-hash (token freq desc file-hash)
  (let ((key (downcase token)))
    (let ((candidate-list (gethash key file-hash nil)))
      (if (not candidate-list)
          (puthash key (list (make-candidate-s :token token :freq freq :desc desc)) file-hash)
        (find-or-insert-candidate-list candidate-list token freq desc)))))

(defun make-hash-for-buffer (file-path file-buffer file-hash)
  (let ((desc (concat "[" (file-name-nondirectory file-path) "]")))
    (for-each-token-in-buffer file-buffer
                              company-ofc-token-delim
                              (lambda (token)
                                (when (>= (length token) company-ofc-min-token-len)
                                  (add-token-to-hash token 0 desc file-hash))))))

(defun company-ofc-init ()
  (let ((file-hash (make-hash-table :test 'ofc-kv-map-cmp-func))
        (file-path (buffer-file-name)))
    (when file-path
      (make-hash-for-buffer file-path (current-buffer) file-hash)
      (puthash file-path file-hash g-filename2hash))))

(defun update-buffer-hash (buffer file-path old-file-hash new-file-hash)
  (let ((token-desc (concat "[" (file-name-nondirectory file-path) "]")))
    (for-each-token-in-buffer buffer
                              company-ofc-token-delim
                              (lambda (token)
                                (let ((old-candidate-list (gethash (downcase token) old-file-hash nil)))
                                  (if old-candidate-list
                                      (let ((old-candidate (find-candidate-in-list token old-candidate-list)))
                                        (if old-candidate
                                            (add-token-to-hash (candidate-s-token old-candidate)
                                                               (candidate-s-freq old-candidate)
                                                               token-desc
                                                               new-file-hash)
                                          (add-token-to-hash token 0 token-desc new-file-hash)))
                                    (add-token-to-hash token 0 token-desc new-file-hash)))))))

(add-hook 'after-save-hook
          (lambda ()
            (let ((file-path (buffer-file-name)))
              (let ((old-file-hash (gethash file-path g-filename2hash nil)))
                (if (not old-file-hash)
                    (company-ofc-init)
                  (let ((new-file-hash (make-hash-table :test 'ofc-kv-map-cmp-func)))
                    (update-buffer-hash (current-buffer) file-path old-file-hash new-file-hash)
                    (puthash file-path new-file-hash g-filename2hash)))))))

(add-hook 'kill-buffer-hook (lambda ()
                              (remhash (buffer-file-name) g-filename2hash)))

(defun do-fuzzy-compare (pattern pattern-length text text-length)
  (if (> pattern-length text-length)
      nil
    (let ((text-idx 0)
          (pattern-idx 0))
      (while (and (< text-idx text-length)
                  (< pattern-idx pattern-length))
        (when (eq (elt text text-idx) (elt pattern pattern-idx))
          (cl-incf pattern-idx))
        (cl-incf text-idx))
      (= pattern-idx pattern-length))))

(defun company-ofc-get-annotation (token)
  (let ((c (find-candidate-in-list token g-candidate-list)))
    (when c
      (candidate-s-desc c))))

(defun company-ofc-find-candidate (input)
  (setq g-candidate-list '())
  (let ((input-length (length input)))
    (when (>= input-length company-ofc-min-token-len)
      (let ((downcased-input (downcase input)))
        (maphash (lambda (file-path file-hash)
                   (maphash (lambda (token candidate-list)
                              (let ((token-length (length token)))
                                (when (do-fuzzy-compare downcased-input input-length
                                                        token token-length)
                                  (setq g-candidate-list (append candidate-list g-candidate-list)))))
                            file-hash))
                 g-filename2hash)
        ;; sort candidates according to freq
        (setq g-candidate-list (sort g-candidate-list
                                     (lambda (a b)
                                       (> (candidate-s-freq a) (candidate-s-freq b)))))
        (delete-dups (mapcar 'candidate-s-token g-candidate-list))))))

(defun company-ofc-grab-suffix (pattern)
  (when (looking-at pattern)
    (match-string 0)))

(defun company-ofc-grab-prefix (pattern)
  (when (looking-back pattern (line-beginning-position) t)
    (match-string 0)))

(defun company-ofc-post-completion (token)
  ;; update matched candidates in each file
  (mapcar (lambda (candidate)
            (when (string= token (candidate-s-token candidate))
              (cl-incf (candidate-s-freq candidate))))
          g-candidate-list)
  ;; remove overlapping suffix
  (let ((suffix (company-ofc-grab-suffix company-ofc-token-pattern)))
    (when (and suffix
               (do-fuzzy-compare suffix (length suffix) token (length token)))
      (delete-char (length suffix)))))

;; -----------------------------------------------------------------------------

(defun company-ofc (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (init (company-ofc-init))
    ;;(interactive (company-begin-backend 'company-ofc))
    (prefix (company-ofc-grab-prefix company-ofc-token-pattern))
    (candidates (company-ofc-find-candidate arg))
    (post-completion (company-ofc-post-completion arg))
    (annotation (company-ofc-get-annotation arg))
    (sorted t) ;; tell company not to sort the result again
    (no-cache t)))

(provide 'company-ofc)
