;; -*- lexical-binding: t; -*-
;; Run these tests from project dir using:
;; make seq-tests

(require 'cl-lib)

(require 'ert)
(require 'seq)
(require 'loopy)
(require 'loopy-seq)

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

;; Need to test accumulation commands of more than 2 arguments.
(ert-deftest seq-accum-keywords ()
  (should (equal '(((1 . 2)) ((1 . 1) (2 . 3)))
                 (loopy (flag seq)
                        (list i '([(1 . 2) (1 . 1)]
                                  [(1 . 2) (2 . 3)]))
                        (adjoin [a1 a2] i :test #'equal)
                        (finally-return a1 a2))))

  (should (equal '((3 1) (4 2))
                 (loopy (flag seq)
                        (list j '([1 2] [3 4]))
                        (collect [c1 c2] j :at start)
                        (finally-return c1 c2)))))

;; Make sure all variables for the needed settings are properly bound.
(ert-deftest destructuring-settings-not-escape ()
  (eval (quote (loopy (flag seq) (repeat 0))))
  (should-not loopy--destructuring-for-with-vars-function)
  (should-not loopy--destructuring-for-iteration-function)
  (should-not loopy--destructuring-accumulation-parser))
