;; -*- lexical-binding: t; -*-
;; Run these tests from project dir using:
;; emacs -Q --batch -l ert -l tests.el -f ert-run-tests-batch-and-exit


(push (expand-file-name ".")
      load-path)

(require 'cl-lib)
(require 'ert)
(require 'pcase)
(require 'loopy "./loopy.el")
(require 'loopy-pcase "./loopy-pcase.el")

;; We can't just run the normal tests like we can with Dash, as `pcase' expects
;; a different form of input.  For now, just test the important cases.

(ert-deftest pcase-with-destructuring ()
  (should (= -2
             (eval (quote (loopy (flag pcase)
                                 (with (`(,a ,b) '(1 2))
                                       (`[,c ,d] `[,(1+ a) ,(1+ b)]))
                                 (loop (return (+ (- a b)
                                                  (- c d))))))))))

(ert-deftest pcase-array-recursive-destructuring ()
  (should
   (equal '(4 5 6)
          (eval (quote
                 (loopy (flag pcase)
                        ((array `[,i [,j ,k]] [[1 [2 3]] [4 [5 6]]]))
                        (return i j k)))))))

(ert-deftest list-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy (flag pcase)
                                          ((list `(,a . ,b)
                                                 '((1 . 2) (3 . 4) (5 . 6))))
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy (flag pcase)
                                          ((list `(,a . ,b)
                                                 '((1 2) (3 4) (5 6))))
                                          (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy (flag pcase)
                                          ((list `(,a ,b ,c ,d)
                                                 '((1 2 3 4) (4 5 6 7))))
                                          (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy (flag pcase)
                                          ((list `[,i ,j ,k] '([1 2 3] [4 5 6])))
                                          (return i j k))))))))

(ert-deftest pcase-collect-destructuring ()
  (should (and (equal '((1 4) ((2 3) (5 6)))
                      (eval (quote (loopy (flag pcase)
                                          ((list j '((1 2 3) (4 5 6)))
                                           (collect `(,coll1 . ,coll2) j))
                                          (return coll1 coll2)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy (flag pcase)
                                          ((list j '((1 2 3) (4 5 6)))
                                           (collect `(,coll1 ,coll2 ,coll3) j))
                                          (return coll1 coll2 coll3)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy (flag pcase)
                                          ((list j '([1 2 3] [4 5 6]))
                                           (collect `[,coll1 ,coll2 ,coll3] j))
                                          (return coll1 coll2 coll3))))))))
