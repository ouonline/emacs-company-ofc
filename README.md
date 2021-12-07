# Overview

These are fuzzy completion backends for [company-mode](https://github.com/company-mode/company-mode) of [Emacs](https://www.gnu.org/software/emacs/).

`company-ofc` is for general purpose completions and `company-ofc-path` for path completions.

# Installations

```lisp
;; company mode settings example

(add-to-list 'load-path "/path/to/emacs-company-ofc")

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

to create a byte-compiled version to speed up completions. **DO NOT** forget to re-compile when you update these `.el` files.
