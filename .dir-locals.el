;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil      . ((sentence-end-double-space . t)))
 (emacs-lisp-mode . ((eval . (dolist
                                 (regexp
                                  '("^\\s-*(loopy--def\\(?:\\(?:accumul\\|iter\\)ation\\)\\s-+\\(?1:\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)"
                                    "^\\s-*(defun loopy--parse-\\(?1:\\(?:\\sw\\|\\s_\\|\\\\.\\)+\\)-command"))
                               (push
                                (list "Loop Commands" regexp 1)
                                imenu-generic-expression))))))
