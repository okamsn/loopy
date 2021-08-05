;; Run these tests from project dir using:
;; emacs -Q --batch -l ert -l tests.el -f ert-run-tests-batch-and-exit


(push (expand-file-name ".")
      load-path)

(require 'cl-lib)
(require 'ert)
(require 'dash "./dependecy-links/dash.el")
(require 'loopy "./loopy.el")
(require 'loopy-dash "./loopy-dash.el")

(ert-deftest dash-flag-default ()
  (should (equal '(5 6)
                 (let ((loopy-default-flags '(dash)))
                   (eval (quote (loopy (list (&plist :a a  :b b)
                                             '((:a 3  :b 4) (:a 5 :b 6)))
                                       (finally-return a b))))))))

(ert-deftest dash-flag-list-destructuring ()
  (should (equal '(1 2)
                 (eval (quote (loopy (flag dash)
                                     (list (i) '((1) (2)))
                                     (collect i))))))

  (should (equal '(((1) 1) ((2) 2))
                 (eval (quote (loopy (flag dash)
                                     (list (whole &as i) '((1) (2)))
                                     (collect (list whole i)))))))

  (should (equal '((1 2) (3 4))
                 (eval (quote (loopy (flag dash)
                                     (list (i j) '((1 2) (3 4)))
                                     (collect (list i j)))))))

  (should (equal '((1 2) (3 4))
                 (eval (quote (loopy (flag dash)
                                     (list (i . j) '((1 . 2) (3 . 4)))
                                     (collect (list i j)))))))

  (should (equal '((1 2) (4 3))
                 (eval (quote (loopy (flag dash)
                                     (list (&plist :k1 i :k2 j) '((:k1 1 :k2 2)
                                                                  (:k2 3 :k1 4)))
                                     (collect (list i j)))))))

  (should (equal '((1 2) (4 3))
                 (eval (quote (loopy (flag dash)
                                     (array (&alist :k1 i :k2 j) [((:k1 . 1)
                                                                   (:k2 . 2))
                                                                  ((:k2 . 3)
                                                                   (:k1 . 4))])
                                     (collect (list i j))))))))

(ert-deftest dash-flag-array-destructuring ()
  (should (equal '((1 2 3))
                 (eval (quote (loopy (flag dash)
                                     (list [i j k] '([1 2 3]))
                                     (collect (list i j k)))))))

  (should (equal '((1 [2 3]))
                 (eval (quote (loopy (flag dash)
                                     (list [i &rest j] '([1 2 3]))
                                     (collect (list i j))))))))

(ert-deftest dash-flag-default-disable ()
  :expected-result :failed
  (should (equal '(5 6)
                 (let ((loopy-default-flags '(dash)))
                   (eval (quote (loopy (flag -dash)
                                       (list (&plist :a a  :b b)
                                             '((:a 3  :b 4) (:a 5 :b 6)))
                                       (finally-return a b))))))))

(ert-deftest dash-flag-enable-disable ()
  :expected-result :failed
  (should (equal '(5 6)
                 (eval (quote (loopy (flag dash -dash)
                                     (list (&plist :a a  :b b)
                                           '((:a 3  :b 4) (:a 5 :b 6)))
                                     (finally-return a b)))))))


(ert-deftest dash-with-destructuring ()
  (should (= 7 (eval (quote (loopy (flag dash)
                                   (with ((&plist :a a  :b b) '(:a 3 :b 4)))
                                   (repeat 1)
                                   (return (+ a b))))))))

;; Make sure all variables for the needed settings are properly bound.
(ert-deftest destructuring-settings-not-escape ()
  (eval (quote (loopy (flag dash) (repeat 0))))
  (should-not loopy--destructuring-for-with-vars-function)
  (should-not loopy--destructuring-for-iteration-function)
  (should-not loopy--destructuring-accumulation-parser))
