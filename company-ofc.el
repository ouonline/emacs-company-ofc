(require 'cl-lib)

(defvar company-ofc-char-set "0-9a-zA-Z_")
(defvar company-ofc-word-separator (concat "[^" company-ofc-char-set "]+"))
(defvar company-ofc-min-word-len 4)

(define-hash-table-test 'ofc-kv-map-cmp-func (lambda (a b) (string= a b)) 'sxhash)

(defvar g-filename2hash (make-hash-table :test 'ofc-kv-map-cmp-func))

(cl-defstruct candidate-s word freq desc)

(defun company-ofc-buffer-p ()
  (member major-mode '(c-mode c++-mode)))

(defun find-candidate-in-list (word candidate-list)
  (if (null candidate-list)
      nil
    (let ((element (car candidate-list)))
      (if (string= word (candidate-s-word element))
          element
        (find-candidate-in-list word (cdr candidate-list))))))

;; make sure that candidate-list is not null
(defun find-or-insert-candidate-list (candidate-list word freq desc)
  (let ((candidate (car candidate-list)))
    (if (string= word (candidate-s-word candidate))
        candidate
      (let ((rest (cdr candidate-list)))
        (if (null rest)
            (setcdr candidate-list (list (make-candidate-s :word word :freq freq :desc desc)))
          (find-or-insert-candidate-list rest word freq desc))))))

(defun buffer2string (buffer)
  (with-current-buffer buffer
    (save-restriction
      (widen)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun for-each-word-in-buffer (buffer separator word-callback-func)
  (mapcar word-callback-func
          (split-string (buffer2string buffer) separator t)))

(defun add-word-to-hash (word freq desc file-hash)
  (let ((key (downcase word)))
    (let ((candidate-list (gethash key file-hash nil)))
      (if (not candidate-list)
          (puthash key (list (make-candidate-s :word word :freq freq :desc desc)) file-hash)
        (find-or-insert-candidate-list candidate-list word freq desc)))))

(defun make-hash-for-buffer (file-path file-buffer file-hash)
  (let ((desc (concat "[" (file-name-nondirectory file-path) "]")))
    (for-each-word-in-buffer file-buffer
                             company-ofc-word-separator
                             (lambda (word)
                               (if (>= (length word) company-ofc-min-word-len)
                                   (add-word-to-hash word 0 desc file-hash))))))

(defun company-ofc-init ()
  (if (company-ofc-buffer-p)
      (let ((file-hash (make-hash-table :test 'ofc-kv-map-cmp-func))
            (file-path (buffer-file-name)))
        (make-hash-for-buffer file-path (current-buffer) file-hash)
        (puthash file-path file-hash g-filename2hash))))

(defun update-buffer-hash (buffer file-path old-file-hash new-file-hash)
  (let ((word-desc (concat "[" (file-name-nondirectory file-path) "]")))
    (for-each-word-in-buffer buffer
                             company-ofc-word-separator
                             (lambda (word)
                               (let ((old-candidate-list (gethash (downcase word) old-file-hash nil)))
                                 (if old-candidate-list
                                     (let ((old-candidate (find-candidate-in-list word old-candidate-list)))
                                       (if old-candidate
                                           (add-word-to-hash (candidate-s-word old-candidate)
                                                             (candidate-s-freq old-candidate)
                                                             word-desc
                                                             new-file-hash)
                                         (add-word-to-hash word 0 word-desc new-file-hash)))
                                   (add-word-to-hash word 0 word-desc new-file-hash)))))))

(add-hook 'after-save-hook
          (lambda ()
            (if (company-ofc-buffer-p)
                (let ((file-path (buffer-file-name)))
                  (let ((old-file-hash (gethash file-path g-filename2hash nil)))
                    (if (not old-file-hash)
                        (company-ofc-init)
                      (let ((new-file-hash (make-hash-table :test 'ofc-kv-map-cmp-func)))
                        (update-buffer-hash (current-buffer) file-path old-file-hash new-file-hash)
                        (puthash file-path new-file-hash g-filename2hash))))))))

(add-hook 'kill-buffer-hook (lambda ()
                              (if (company-ofc-buffer-p)
                                  (remhash (buffer-file-name) g-filename2hash))))

(defun do-fuzzy-compare (input input-length word word-length)
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

(setq g-candidate-list '())

(defun company-ofc-get-annotation (word)
  (let ((c (find-candidate-in-list word g-candidate-list)))
    (if c
        (candidate-s-desc c))))

(defun company-ofc-find-candidate (input)
  (setq g-candidate-list '())
  (let ((input-length (length input)))
    (if (and (>= input-length company-ofc-min-word-len)
             (company-ofc-buffer-p))
        (let ((downcased-input (downcase input)))
          (maphash (lambda (file-path file-hash)
                     (maphash (lambda (word candidate-list)
                                (let ((word-length (length word)))
                                  (if (do-fuzzy-compare downcased-input input-length
                                                        word word-length)
                                      (setq g-candidate-list (append candidate-list g-candidate-list)))))
                              file-hash))
                   g-filename2hash)
          ;; sort candidates according to freq
          (setq g-candidate-list (sort g-candidate-list
                                       (lambda (a b)
                                         (> (candidate-s-freq a) (candidate-s-freq b)))))
          (delete-dups (mapcar 'candidate-s-word g-candidate-list))))))

(defun company-ofc-update-word-freq (word)
  (mapcar (lambda (candidate)
            (if (string= word (candidate-s-word candidate))
                (cl-incf (candidate-s-freq candidate))))
          g-candidate-list))

(defun company-ofc (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (init (company-ofc-init))
    ;;(interactive (company-begin-backend 'company-ofc))
    (prefix (company-grab-symbol))
    (candidates (company-ofc-find-candidate arg))
    (post-completion (company-ofc-update-word-freq arg))
    (annotation (company-ofc-get-annotation arg))
    (sorted t) ;; tell company not to sort the result again
    (no-cache t)))

(provide 'company-ofc)
