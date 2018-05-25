# Intruduction

This is a fuzzy completion backend for [company-mode](https://github.com/company-mode/company-mode) of [Emacs](https://www.gnu.org/software/emacs/).

# Installation

```lisp
;; other company mode settings here
(add-to-list 'load-path "~/.emacs.d/company-ofc/")
(require 'company-ofc)
(add-to-list 'company-backends 'company-ofc)
```
