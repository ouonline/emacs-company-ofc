# Intruduction

This is a fuzzy completion backend for [company-mode](https://github.com/company-mode/company-mode) of [Emacs](https://www.gnu.org/software/emacs/).

# Installation

```lisp
;; company mode settings example
(add-hook 'prog-mode-hook (lambda () (company-mode)))
(add-to-list 'load-path "/path/to/emacs-company-ofc/")
(add-to-list 'company-backends 'company-ofc)
```

You can use the following command

```bash
emacs -batch -f batch-byte-compile *.el
```

to create a byte-compiled version to speed up completion. **DO NOT** forget to re-compile when you update the source .el file.
