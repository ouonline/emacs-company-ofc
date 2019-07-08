(require 'cl-lib)

;; -----------------------------------------------------------------------------
;; constants

(defconst company-ofc-path-token-char-set "0-9a-zA-Z_/\.-")
(defconst company-ofc-path-token-pattern (concat "[" company-ofc-path-token-char-set "]+"))

;; -----------------------------------------------------------------------------
;; struct definitions

(cl-defstruct candidate-s parent-dir entry-list)

;; -----------------------------------------------------------------------------
;; global variales

(defvar g-candidate-stack '())
(defvar g-real-prefix "") ;; updated when `prefix` is called

;; -----------------------------------------------------------------------------

(defun generic-list-find (predicate list)
  (cl-dolist (element list)
    (if (funcall predicate element)
        (cl-return element))))

(defun do-fuzzy-compare (pattern pattern-length text text-length)
  ;; tells if `pattern` is part of `text`
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

(defun company-ofc-path-grab-prefix (pattern)
  (when (looking-back pattern (line-beginning-position) t)
    (let ((prefix (match-string 0)))
      (when (string-match-p "/" prefix)
        ;; returns a pseudo prefix to make tooltip shown in the proper position and
        ;; keeps the real prefix in `g-real-prefix`
        (setq g-real-prefix prefix)
        (file-name-nondirectory prefix)))))

(defun find-entry-list-in-stack (parent-dir)
  (generic-list-find (lambda (candidate)
                       (let ((dir (candidate-s-parent-dir candidate)))
                         (string= dir parent-dir)))
                     g-candidate-stack))

(defun get-entry-list (parent-dir)
  (let ((candidate (find-entry-list-in-stack parent-dir)))
    (if candidate
        (candidate-s-entry-list candidate)
      (let ((current-entry-list (directory-files parent-dir)))
        (if current-entry-list
            (let ((new-entry-list (mapcar (lambda (entry)
                                            (let ((full-path (concat parent-dir "/" entry)))
                                              (if (file-directory-p full-path)
                                                  (concat entry "/")
                                                entry)))
                                          current-entry-list)))
              (push (make-candidate-s :parent-dir parent-dir
                                      :entry-list new-entry-list)
                    g-candidate-stack)
              new-entry-list)
          '())))))

(defun company-ofc-path-find-candidate (not-used)
  (let ((parent-dir (file-name-directory g-real-prefix))
        (last-component (file-name-nondirectory g-real-prefix)))
    (when (file-directory-p parent-dir)
      (let ((entry-list (get-entry-list parent-dir)))
        (when (not (null entry-list))
          (let ((last-component-length (length last-component))
                (entry-result '()))
            (dolist (entry entry-list)
              (when (do-fuzzy-compare last-component last-component-length
                                      entry (length entry))
                (push entry entry-result)))
            entry-result))))))

(defun company-ofc-path-post-completion (token)
  (when (string-suffix-p "/" token)
    (delete-char (- 1)))
  (setq g-candidate-stack '()))

(defun company-ofc-path (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (prefix (company-ofc-path-grab-prefix company-ofc-path-token-pattern))
    (candidates (company-ofc-path-find-candidate arg))
    (post-completion (company-ofc-path-post-completion arg))
    (sorted t)
    (no-cache t)))

(provide 'company-ofc-path)
