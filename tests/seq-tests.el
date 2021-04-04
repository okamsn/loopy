;; -*- lexical-binding: t; -*-
;; Run these tests from project dir using:
;; emacs -Q --batch -l ert -l tests.el -f ert-run-tests-batch-and-exit

(push (expand-file-name ".")
      load-path)

(require 'cl-lib)
(require 'ert)
(require 'seq)
(require 'loopy "./loopy.el")
(require 'loopy-seq "./loopy-seq.el")

;; We can't just run the normal tests like we can with Dash, as `seq' expects a
;; different form of input.  In particular, `seq-let' uses `&rest' for lists and
;; sequences but doesn't allow dotted pairs (you must do "(a &rest b)" instead).
;; For now, just test the important cases.

(ert-deftest seq-with-destructuring ()
  (should (= 5 (eval (quote (loopy (flag seq)
                                   (with ((a b) '(1 2))
                                         ([c d] `[,(1+ a) ,(1+ b)])
                                         (e 7))
                                   (return (+ (- a b)
                                              (- c d)
                                              e))))))))

(ert-deftest seq-array-recursive-destructuring ()
  (should
   (equal '(4 5 6)
          (eval (quote
                 (loopy (flag seq)
                        (array [i [j k]] [[1 [2 3]] [4 [5 6]]])
                        (finally-return i j k)))))))

(ert-deftest seq-list-destructuring ()
  (should (and (equal '(5 6)
                       (eval (quote (loopy (flag seq)
                                           (list (a &rest b)
                                                 '((1 . 2) (3 . 4) (5 . 6)))
                                           (finally-return a b)))))
               (equal '(5 (6))
                       (eval (quote (loopy (flag seq)
                                           (list (a &rest b)
                                                 '((1 2) (3 4) (5 6)))
                                           (finally-return a b)))))
               (equal '(4 5 6 7)
                       (eval (quote (loopy (flag seq)
                                           (list (a b c d)
                                                 '((1 2 3 4) (4 5 6 7)))
                                           (finally-return a b c d)))))
               (equal '(4 5 6)
                       (eval (quote (loopy (flag seq)
                                           (list [i j k] '([1 2 3] [4 5 6]))
                                           (finally-return i j k))))))))

(ert-deftest seq-collect-destructuring ()
  (should (and (equal '((1 4) ((2 3) (5 6)))
                       (eval (quote (loopy (flag seq)
                                           (list j '((1 2 3) (4 5 6)))
                                           (collect (coll1 &rest coll2) j)
                                           (finally-return coll1 coll2)))))

               (equal '((1 4) (2 5) (3 6))
                       (eval (quote (loopy (flag seq)
                                           (list j '((1 2 3) (4 5 6)))
                                           (collect (coll1 coll2 coll3) j)
                                           (finally-return coll1 coll2 coll3)))))

               (equal '((1 4) (2 5) (3 6))
                       (eval (quote (loopy (flag seq)
                                           (list j '([1 2 3] [4 5 6]))
                                           (collect [coll1 coll2 coll3] j)
                                           (finally-return coll1 coll2 coll3))))))))

(ert-deftest seq-collect-implicit ()
  (should
   (equal '((1 4) (3 6))
           (eval (quote (loopy (flag seq)
                               (list elem '((1 (2 3)) (4 (5 6))))
                               (collect (a (_ b)) elem)))))))

(ert-deftest seq-flag-default ()
  (should (equal '(5 6)
                  (let ((loopy-default-flags '(seq)))
                    (eval (quote (loopy (list (a &rest b)
                                              '((1 . 2) (3 . 4) (5 . 6)))
                                        (finally-return a b))))))))

(ert-deftest seq-flag-default-disable ()
  (should (equal '(5 6)
                  (let ((loopy-default-flags '(seq)))
                    (eval (quote (loopy (flag -seq)
                                        (list (a . b)
                                              '((1 . 2) (3 . 4) (5 . 6)))
                                        (finally-return a b))))))))

(ert-deftest seq-flag-enable-disable ()
  (should (equal '(5 6)
                  (eval (quote (loopy (flag seq -seq)
                                      (list (a . b)
                                            '((1 . 2) (3 . 4) (5 . 6)))
                                      (finally-return a b)))))))
