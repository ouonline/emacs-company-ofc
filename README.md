# Intruduction

These are fuzzy completion backends for [company-mode](https://github.com/company-mode/company-mode) of [Emacs](https://www.gnu.org/software/emacs/).

`company-ofc` is for general purpose completion and `company-ofc-path` for path completion.

# Installation

```lisp
;; company mode settings example

(add-to-list 'load-path "/path/to/emacs-company-ofc")

(make-local-variable 'company-backends)

(add-hook 'prog-mode-hook (lambda ()
                            (setq-local company-backends '(company-ofc-path company-ofc))
                            (company-mode)))
(add-hook 'shell-mode-hook (lambda ()
                            (setq-local company-backends '(company-ofc-path))
                            (company-mode)))
```

You can use the following command

```bash
emacs -batch -f batch-byte-compile *.el
```

to create a byte-compiled version to speed up completion. **DO NOT** forget to re-compile when you update the source .el file.
