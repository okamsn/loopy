;; Run these tests from project dir using:
;; emacs -Q --batch -l ert -l tests.el -f ert-run-tests-batch-and-exit


(push (expand-file-name ".")
      load-path)

(require 'cl-lib)
(require 'ert)
(require 'loopy "./loopy.el")
(require 'loopy-dash "./loopy-dash.el")

(defvar loopy-default-destructuring-function)
(setq loopy-default-destructuring-function
      #'loopy--create-destructured-assignment-dash)
(load-file "tests/tests.el")
