;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ofc-token "./ofc-token.el")

(defun company-ofc-token (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (init (ofc-token--after-buffer-created))
    (prefix (ofc-token--grab-prefix))
    (candidates (ofc-token--find-candidates arg))
    (match (get-text-property 0 :matched-region-list arg))
    (annotation (let* ((token-info (get-text-property 0 :token-info arg))
                       (buffer (car (ofc-token--token-info-s-buffer-list token-info))))
                  (concat "[" (buffer-name buffer) "]")))
    (post-completion (ofc-token--post-completion arg))
    (sorted t) ;; tell company not to sort the result again
    (no-cache t)))

(provide 'company-ofc-token)
