(require 'cl-lib)

(defvar company-ofc-char-set "0-9a-zA-Z_")
(defvar company-ofc-word-separator (concat "[^" company-ofc-char-set "]+"))
(defvar company-ofc-min-word-len 3)

(define-hash-table-test 'ofc-kv-map-cmp-func (lambda (a b)
                                               (string= a b)) 'sxhash)
(defvar g-filename2hash (make-hash-table :test 'ofc-kv-map-cmp-func))

(defun buffer-whole-string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun for-each-word-in-buffer (buffer separator word-callback-func)
  (mapcar word-callback-func
          (split-string (buffer-whole-string buffer) separator t)))

(defun company-ofc-buffer-ok ()
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
                                         (add-to-list 'word-list word)
                                         (puthash key word-list buffer-hash)))))))))

(defun company-ofc-init ()
  (if (company-ofc-buffer-ok)
      (let ((file-hash (make-hash-table :test 'ofc-kv-map-cmp-func))
            (file-name (buffer-file-name)))
        (company-ofc-make-cache (current-buffer) file-hash)
        (puthash file-name file-hash g-filename2hash))))

(add-hook 'after-save-hook 'company-ofc-init) ;; rewrite the whole file after saving

(add-hook 'kill-buffer-hook (lambda ()
                              (remhash (buffer-file-name) g-filename2hash)))

(defun company-ofc-fuzzy-match-word (input word)
  (let ((input-length (length input))
        (word-length (length word)))
    (let ((ith 0)
          (state 0))
      (while (and (< ith word-length)
                  (< state input-length))
        (if (eq (elt word ith) (elt input state))
            (setq state (+ state 1)))
        (setq ith (+ ith 1)))
      (= state input-length))))

(defun company-ofc-find-candidate (input)
  (if (and (>= (length input) company-ofc-min-word-len)
           (company-ofc-buffer-ok))
      (let ((result '()))
        (maphash (lambda (file-name file-hash)
                   (maphash (lambda (word word-list)
                              (when (company-ofc-fuzzy-match-word (downcase input) word)
                                (mapcar (lambda (candidate)
                                          (if (not (member candidate result))
                                              (add-to-list 'result candidate)))
                                        word-list)))
                            file-hash))
                 g-filename2hash)
        result)))

(defun company-ofc (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (init (company-ofc-init))
    ;;(interactive (company-begin-backend 'company-ofc))
    (prefix (company-grab-symbol))
    (candidates (company-ofc-find-candidate arg))))

(provide 'company-ofc)
