;; -*- lexical-binding: t -*-

(require 'cl-lib)
(require 'ofc-token "./ofc-token.el")

;; ------------------------------------------------------------------------- ;;

(defun company-ofc-token--find-candidates (prefix)
  (ofc-token--do-find-candidates
   prefix
   (lambda (matched-item-info)
     (let* ((token-info (ofc-token--matched-item-info-s-token-info matched-item-info))
            (token (ofc-token--token-info-s-token token-info))
            (new-token (substring-no-properties token)))
       (add-text-properties 0 (length new-token)
                            (list :ofc-matched-region-list (ofc-token--matched-item-info-s-matched-region-list matched-item-info)
                                  :ofc-annotation (let ((buffer (car (ofc-token--token-info-s-buffer-list token-info))))
                                                    (concat "[" (buffer-name buffer) "]"))
                                  :ofc-post-completion (lambda ()
                                                         (ofc-token--do-post-completion token-info)))
                            new-token)
       new-token))))

;; ------------------------------------------------------------------------- ;;

(defun company-ofc-token (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (init (ofc-token--after-buffer-created))
    (prefix (ofc-token--grab-prefix ofc-token-pattern))
    (candidates (company-ofc-token--find-candidates arg))
    (match (get-text-property 0 :ofc-matched-region-list arg))
    (annotation (get-text-property 0 :ofc-annotation arg))
    (post-completion (funcall (get-text-property 0 :ofc-post-completion arg)))
    (sorted t) ;; tell company not to sort the result again
    (no-cache t)))

(provide 'company-ofc-token)
