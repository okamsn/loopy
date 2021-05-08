---
name: Bug report
about: Create a report to help us improve
title: ''
labels: ''
assignees: ''

---

Suggested content:

- Problem
- Code used to produce problem
- Emacs version

If you have problems running the macro, please include the expanded code, such as with the command `pp-macroexpand-last-sexp` or the below command

``` elisp
(defun my-pp-macroexpand-all-last-sexp (arg)
  "Like `pp-macroexpand-last-sexp', but use `macroexpand-all'.
With ARG, print into current buffer."
  (interactive "P")
  (if arg
      (insert (pp-to-string (macroexpand-all (pp-last-sexp))))
    (pp-display-expression (macroexpand-all (pp-last-sexp))
                           "*Pp Macroexpand Output*")))
```

though the first is usually good enough.
