(require 'cl-lib)

(defvar company-ofc-char-set "0-9a-zA-Z_")
(defvar company-ofc-word-separator (concat "[^" company-ofc-char-set "]+"))
(defvar company-ofc-min-word-len 3)

(define-hash-table-test 'ofc-kv-map-cmp-func (lambda (a b)
                                               (string= a b)) 'sxhash)
(defvar g-filename2hash (make-hash-table :test 'ofc-kv-map-cmp-func))

(defun buffer2string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun for-each-word-in-buffer (buffer separator word-callback-func)
  (mapcar word-callback-func
          (split-string (buffer2string buffer) separator t)))

(defun company-ofc-buffer-p ()
  (member major-mode '(c-mode c++-mode emacs-lisp-mode)))

(defun company-ofc-make-cache (buffer buffer-hash)
  (for-each-word-in-buffer buffer
                           company-ofc-word-separator
                           (lambda (word)
                             (if (>= (length word) company-ofc-min-word-len)
                                 (let ((key (downcase word)))
                                   (let ((word-list (gethash key buffer-hash nil)))
                                     (if (not word-list)
                                         (puthash key (list word) buffer-hash)
                                       (when (not (member word word-list))
                                         (add-to-list 'word-list word)))))))))

(defun company-ofc-init ()
  (if (company-ofc-buffer-p)
      (let ((file-hash (make-hash-table :test 'ofc-kv-map-cmp-func))
            (file-name (buffer-file-name)))
        (company-ofc-make-cache (current-buffer) file-hash)
        (puthash file-name file-hash g-filename2hash))))

(add-hook 'after-save-hook 'company-ofc-init) ;; rewrite the file hash after saving

(add-hook 'kill-buffer-hook (lambda ()
                              (remhash (buffer-file-name) g-filename2hash)))

(defun company-ofc-fuzzy-match-word (input input-length word word-length)
  (if (> input-length word-length)
      nil
    (let ((ith 0)
          (state 0))
      (while (and (< ith word-length)
                  (< state input-length))
        (if (eq (elt word ith) (elt input state))
            (cl-incf state))
        (cl-incf ith))
      (= state input-length))))

(defun company-ofc-find-candidate (input)
  (let ((input-length (length input)))
    (if (and (>= input-length company-ofc-min-word-len)
             (company-ofc-buffer-p))
        (let ((result '())
              (downcased-input (downcase input)))
          (maphash (lambda (file-name file-hash)
                     (maphash (lambda (word word-list)
                                (let ((word-length (length word)))
                                  (if (company-ofc-fuzzy-match-word downcased-input input-length
                                                                    word word-length)
                                      (mapcar (lambda (candidate)
                                                (if (not (member candidate result))
                                                    (add-to-list 'result candidate)))
                                              word-list))))
                              file-hash))
                   g-filename2hash)
          result))))

(defun company-ofc (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (init (company-ofc-init))
    ;;(interactive (company-begin-backend 'company-ofc))
    (prefix (company-grab-symbol))
    (candidates (company-ofc-find-candidate arg))))

(provide 'company-ofc)
