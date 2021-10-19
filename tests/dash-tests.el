;; Run these tests from project dir using:
;; emacs -Q --batch -l ert -l tests.el -f ert-run-tests-batch-and-exit


(push (expand-file-name ".")
      load-path)

(require 'cl-lib)
(require 'ert)
(require 'dash "./dependecy-links/dash.el" 'no-error)
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

(ert-deftest dash-alist-iteration-destructuring ()
  (should (equal '(1 2 3 4)
                 (eval (quote (loopy (flag dash)
                                     (list (&alist 'a avar 'b bvar)
                                           '(((a . 1) (b . 2))
                                             ((a . 3) (b . 4))))
                                     (collect avar)
                                     (collect bvar))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy (flag dash)
                                     (list (&alist 'a 'b)
                                           '(((a . 1) (b . 2))
                                             ((a . 3) (b . 4))))
                                     (collect a)
                                     (collect b))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy (flag dash)
                                     (list (&alist :a :b)
                                           '(((:a . 1) (:b . 2))
                                             ((:a . 3) (:b . 4))))
                                     (collect a)
                                     (collect b))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy (flag dash)
                                     (list (&alist "a" "b")
                                           '((("a" . 1) ("b" . 2))
                                             (("a" . 3) ("b" . 4))))
                                     (collect a)
                                     (collect b)))))))

(ert-deftest dash-alist-accumulation-destructuring ()
  (should (equal '((1 3) (2 4))
                 (eval (quote (loopy (flag dash)
                                     (list elem
                                           '(((a . 1) (b . 2))
                                             ((a . 3) (b . 4))))
                                     (collect (&alist 'a avar 'b bvar) elem)
                                     (finally-return avar bvar))))))

  (should (equal '((1 3) (2 4))
                 (eval (quote (loopy (flag dash)
                                     (list elem
                                           '(((a . 1) (b . 2))
                                             ((a . 3) (b . 4))))
                                     (collect (&alist 'a 'b) elem)
                                     (finally-return a b))))))

  (should (equal '((1 3) (2 4))
                 (eval (quote (loopy (flag dash)
                                     (list elem
                                           '(((:a . 1) (:b . 2))
                                             ((:a . 3) (:b . 4))))
                                     (collect (&alist :a :b) elem)
                                     (finally-return a b))))))

  (should (equal '((1 3) (2 4))
                 (eval (quote (loopy (flag dash)
                                     (list elem
                                           '((("a" . 1) ("b" . 2))
                                             (("a" . 3) ("b" . 4))))
                                     (collect (&alist "a" "b") elem)
                                     (finally-return a b)))))))

(ert-deftest dash-plist-iteration-destructuring ()
  (should (equal '(1 2 3 4)
                 (eval (quote (loopy (flag dash)
                                     (list (&plist 'a avar 'b bvar)
                                           '((a 1 b 2) (a 3 b 4)))
                                     (collect avar)
                                     (collect bvar))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy (flag dash)
                                     (list (&plist 'a 'b)
                                           '((a 1 b 2) (a 3 b 4)))
                                     (collect a)
                                     (collect b))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy (flag dash)
                                     (list (&plist :a :b)
                                           '((:a 1 :b 2) (:a 3 :b 4)))
                                     (collect a)
                                     (collect b))))))

  ;; NOTE: This test won't work, since `plist-get' uses `eq', which fails for
  ;;       lists.
  ;;
  ;; (should (equal '((1 3) (2 4))
  ;;                (eval (quote (loopy (flag dash)
  ;;                                    (accum-opt a b)
  ;;                                    (list elem '(("a" 1 "b" 2) ("a" 3 "b" 4)))
  ;;                                    (collect (&plist "a" "b") elem)
  ;;                                    (finally-return a b))))))
  )

(ert-deftest dash-plist-accumulation-destructuring ()
  (should (equal '((1 3) (2 4))
                 (eval (quote (loopy (flag dash)
                                     (list elem '((a 1 b 2) (a 3 b 4)))
                                     (collect (&plist 'a avar 'b bvar) elem)
                                     (finally-return avar bvar))))))

  (should (equal '((1 3) (2 4))
                 (eval (quote (loopy (flag dash)
                                     (list elem '((a 1 b 2) (a 3 b 4)))
                                     (collect (&plist 'a 'b) elem)
                                     (finally-return a b))))))

  (should (equal '((1 3) (2 4))
                 (eval (quote (loopy (flag dash)
                                     (list elem '((:a 1 :b 2) (:a 3 :b 4)))
                                     (collect (&plist :a :b) elem)
                                     (finally-return a b))))))

  ;; NOTE: This test won't work, since `plist-get' uses `eq', which fails for
  ;;       lists.
  ;;
  ;; (should (equal '((1 3) (2 4))
  ;;                (eval (quote (loopy (flag dash)
  ;;                                    (accum-opt a b)
  ;;                                    (list elem '(("a" 1 "b" 2) ("a" 3 "b" 4)))
  ;;                                    (collect (&plist "a" "b") elem)
  ;;                                    (finally-return a b))))))
  )

;; Just trying things.
(ert-deftest dash-random-destructurings ()
  (should (equal '((1 6) (2 7) (3 8) (4 10) (5 9))
                 (loopy (flag dash)
                        (array elem [(1 2 3 :k1 4 :k2 5)
                                     (6 7 8 :k2 9 :k1 10)])
                        (collect (a b c &plist :k1 :k2) elem)
                        (finally-return a b c k1 k2))))

  (should (equal '((1 6) (3 8) (4 10) (5 9))
                 (loopy (flag dash)
                        (array elem [(1 2 3 (:k1 . 4) (:k2 . 5))
                                     (6 7 8 (:k2 . 9) (:k1 . 10))])
                        (collect (a _ c &alist :k1 :k2) elem)
                        (finally-return a c k1 k2)))))
