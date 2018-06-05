# Intruduction

This is a fuzzy completion backend for [company-mode](https://github.com/company-mode/company-mode) of [Emacs](https://www.gnu.org/software/emacs/).

# Installation

```lisp
;; company mode settings here, for example:
;; (add-hook 'prog-mode-hook (lambda () (company-mode)))

(add-to-list 'load-path "/path/to/emacs-company-ofc/")
(add-to-list 'company-backends 'company-ofc)
```
