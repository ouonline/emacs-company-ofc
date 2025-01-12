;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ofc-common "./ofc-common.el")

;; -----------------------------------------------------------------------------
;; settings

(defgroup company-ofc nil
  "fuzzy completion backends for company-mode of emacs"
  :prefix "company-ofc-"
  :link '(info-link "(emacs)company-ofc")
  :group 'matching)

(defcustom company-ofc-path-token-charset "0-9a-zA-Z_/\.~-"
  "valid path characters in regexp"
  :type 'string)

(defconst company-ofc-path-token-pattern (concat "[" company-ofc-path-token-charset "]+"))

;; -----------------------------------------------------------------------------
;; struct definitions

(cl-defstruct company-ofc--path-candidate-s parent-dir entry-list)

;; -----------------------------------------------------------------------------
;; global variales

(defvar company-ofc--path-candidate-stack '())
(defvar company-ofc--path-real-prefix "") ;; updated when `prefix` is called

;; -----------------------------------------------------------------------------

(defun company-ofc--path-grab-prefix (pattern)
  (when (looking-back pattern (line-beginning-position) t)
    (let ((prefix (match-string 0)))
      (when (string-match-p "/" prefix)
        ;; returns a pseudo prefix to make tooltip shown at the proper position and
        ;; stores the real prefix in `company-ofc--path-real-prefix`
        (if (string-prefix-p "~/" prefix)
            (setq company-ofc--path-real-prefix (concat (substitute-in-file-name "$HOME/")
                                                        (substring-no-properties prefix 2)))
          (setq company-ofc--path-real-prefix prefix))
        (file-name-nondirectory prefix)))))

(defun company-ofc--path-find-entry-list-in-stack (parent-dir)
  (cl-dolist (candidate company-ofc--path-candidate-stack)
    (let ((dir (company-ofc--path-candidate-s-parent-dir candidate)))
      (when (string= dir parent-dir)
        (cl-return candidate)))))

(defun company-ofc--path-get-entry-list (parent-dir)
  (let ((candidate (company-ofc--path-find-entry-list-in-stack parent-dir)))
    (if candidate
        (company-ofc--path-candidate-s-entry-list candidate)
      (let ((current-entry-list (directory-files parent-dir)))
        (if current-entry-list
            (let ((new-entry-list (mapcar (lambda (entry)
                                            (let ((full-path (concat parent-dir "/" entry)))
                                              (if (file-directory-p full-path)
                                                  (concat entry "/")
                                                entry)))
                                          current-entry-list)))
              (push (make-company-ofc--path-candidate-s :parent-dir parent-dir
                                                        :entry-list new-entry-list)
                    company-ofc--path-candidate-stack)
              new-entry-list)
          '())))))

(defun company-ofc--path-find-candidates (not-used)
  (let ((parent-dir (file-name-directory company-ofc--path-real-prefix))
        (last-component (downcase (file-name-nondirectory company-ofc--path-real-prefix))))
    (when (file-directory-p parent-dir)
      (let ((entry-list (company-ofc--path-get-entry-list parent-dir)))
        (when (not (null entry-list))
          (let ((last-component-length (length last-component))
                (entry-result '()))
            (dolist (entry entry-list)
              (when (ofc--fuzzy-compare last-component last-component-length
                                        (downcase entry) (length entry))
                (push entry entry-result)))
            entry-result))))))

(defun company-ofc--path-post-completion (token)
  (when (string-suffix-p "/" token)
    (delete-char (- 1)))
  (setq company-ofc--path-candidate-stack '()))

(defun company-ofc-path (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (prefix (company-ofc--path-grab-prefix company-ofc-path-token-pattern))
    (candidates (company-ofc--path-find-candidates arg))
    (post-completion (company-ofc--path-post-completion arg))
    (sorted t)
    (no-cache t)))

(provide 'company-ofc-path)
