;; -*- lexical-binding: t; -*-
;; Run these tests from project dir using:
;; make pcase-tests

;; NOTE: Tests the `pcase' flag, not the `pcase' implementation of destructuring.

(require 'cl-lib)

(require 'ert)
(require 'pcase)
(require 'loopy)
(require 'loopy-pcase)

;; We can't just run the normal tests like we can with Dash, as `pcase' expects
;; a different form of input.  For now, just test the important cases.

(ert-deftest pcase-with-destructuring ()
  (should (= -2
             (eval (quote (loopy (flag pcase)
                                 (with (`(,a ,b) '(1 2))
                                       (`[,c ,d] `[,(1+ a) ,(1+ b)]))
                                 (return (+ (- a b)
                                            (- c d)))))))))

(ert-deftest pcase-array-recursive-destructuring ()
  (should
   (equal '(4 5 6)
          (eval (quote
                 (loopy (flag pcase)
                        (array `[,i [,j ,k]] [[1 [2 3]] [4 [5 6]]])
                        (finally-return i j k)))))))

(ert-deftest pcase-list-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy (flag pcase)
                                          (list `(,a . ,b)
                                                '((1 . 2) (3 . 4) (5 . 6)))
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy (flag pcase)
                                          (list `(,a . ,b)
                                                '((1 2) (3 4) (5 6)))
                                          (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy (flag pcase)
                                          (list `(,a ,b ,c ,d)
                                                '((1 2 3 4) (4 5 6 7)))
                                          (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy (flag pcase)
                                          (list `[,i ,j ,k] '([1 2 3] [4 5 6]))
                                          (finally-return i j k))))))))

(ert-deftest pcase-collect-destructuring ()
  (should (and (equal '((1 4) ((2 3) (5 6)))
                      (eval (quote (loopy (flag pcase)
                                          (list j '((1 2 3) (4 5 6)))
                                          (collect `(,coll1 . ,coll2) j)
                                          (finally-return coll1 coll2)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy (flag pcase)
                                          (list j '((1 2 3) (4 5 6)))
                                          (collect `(,coll1 ,coll2 ,coll3) j)
                                          (finally-return coll1 coll2 coll3)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy (flag pcase)
                                          (list j '([1 2 3] [4 5 6]))
                                          (collect `[,coll1 ,coll2 ,coll3] j)
                                          (finally-return coll1 coll2 coll3))))))))

(ert-deftest pcase-flag-default ()
  (should (equal '(5 6)
                 (let ((loopy-default-flags '(pcase)))
                   (eval (quote (loopy (list `(,a . ,b)
                                             '((1 . 2) (3 . 4) (5 . 6)))
                                       (finally-return a b))))))))

(ert-deftest pcase-flag-default-disable ()
  (should (equal '(5 6)
                 (let ((loopy-default-flags '(pcase)))
                   (eval (quote (loopy (flag -pcase)
                                       (list (a . b)
                                             '((1 . 2) (3 . 4) (5 . 6)))
                                       (finally-return a b))))))))

(ert-deftest pcase-flag-enable-disable ()
  (should (equal '(5 6)
                 (eval (quote (loopy (flag pcase -pcase)
                                     (list (a . b)
                                           '((1 . 2) (3 . 4) (5 . 6)))
                                     (finally-return a b)))))))

(ert-deftest pcase-accum-keywords ()
  (should (equal '(((1 . 2)) ((1 . 1) (2 . 3)))
                 (loopy (flag pcase)
                        (list i '([(1 . 2) (1 . 1)]
                                  [(1 . 2) (2 . 3)]))
                        (adjoin `[,a1 ,a2] i :test #'equal)
                        (finally-return a1 a2))))

  (should (equal '((3 1) (4 2))
                 (loopy (flag pcase)
                        (list j '([1 2] [3 4]))
                        (collect `[,c1 ,c2] j :at start)
                        (finally-return c1 c2)))))

;; Make sure all variables for the needed settings are properly bound.
(ert-deftest destructuring-settings-not-escape ()
  (eval (quote (loopy (flag pcase) (repeat 0))))
  (should-not loopy--destructuring-for-with-vars-function)
  (should-not loopy--destructuring-for-iteration-function)
  (should-not loopy--destructuring-accumulation-parser))
