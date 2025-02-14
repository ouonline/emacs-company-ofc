;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ofc-common "./ofc-common.el")

;; -----------------------------------------------------------------------------
;; settings

(defconst ofc-path-token-charset "0-9a-zA-Z_/\.~-")
(defconst ofc-path-token-pattern (concat "[" ofc-path-token-charset "]+"))

;; -----------------------------------------------------------------------------
;; global variales

;; each element is `(list dir entry-list)'
(defvar ofc-path--item-cache '())

(defvar ofc-path--abs-path "") ;; updated when `prefix' is called

;; -----------------------------------------------------------------------------

(defun ofc-path--grab-prefix (pattern)
  (when (looking-back pattern (line-beginning-position) t)
    (let ((prefix (match-string-no-properties 0)))
      (when (string-match-p "/" prefix)
        ;; returns a pseudo prefix to make tooltip shown at the proper position and
        ;; stores the real prefix in `ofc-path--abs-path'
        (if (string-prefix-p "~/" prefix)
            (setq ofc-path--abs-path (concat (substitute-in-file-name "$HOME/")
                                             (substring-no-properties prefix 2)))
          (if (eq (aref prefix 0) ?/)
              (setq ofc-path--abs-path prefix)
            (setq ofc-path--abs-path (concat default-directory prefix))))
        (file-name-nondirectory prefix)))))

(defun ofc-path--find-entry-list-in-cache (dir)
  (cl-dolist (item ofc-path--item-cache)
    (when (string= (car item) dir)
      (cl-return (cadr item)))))

(defun ofc-path--get-entry-list (parent-dir)
  (let ((entry-list (ofc-path--find-entry-list-in-cache parent-dir)))
    (unless entry-list
      (let ((current-entry-list (directory-files parent-dir)))
        (when current-entry-list
          (setq entry-list (mapcar (lambda (entry)
                                     (let ((full-path (concat parent-dir "/" entry)))
                                       (if (file-directory-p full-path)
                                           (concat entry "/")
                                         entry)))
                                   current-entry-list))
          (when entry-list
            (push (list parent-dir entry-list) ofc-path--item-cache)))))
    entry-list))

(defun ofc-path--find-candidates (not-used)
  (let ((parent-dir (file-name-directory ofc-path--abs-path))
        (downcased-input (downcase (file-name-nondirectory ofc-path--abs-path))))
    (when (file-directory-p parent-dir)
      (let ((entry-list (ofc-path--get-entry-list parent-dir)))
        (when entry-list
          (let ((downcased-input-length (length downcased-input))
                (entry-result '()))
            (dolist (entry entry-list)
              (when (ofc--fuzzy-compare downcased-input downcased-input-length
                                        (downcase entry) (length entry))
                (push entry entry-result)))
            entry-result))))))

(defun ofc-path--post-completion (token)
  (when (string-suffix-p "/" token)
    (delete-char (- 1)))
  (setq ofc-path--item-cache '()))

(defun company-ofc-path (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (prefix (ofc-path--grab-prefix ofc-path-token-pattern))
    (candidates (ofc-path--find-candidates arg))
    (post-completion (ofc-path--post-completion arg))
    (sorted t)
    (no-cache t)))

(provide 'company-ofc-path)
