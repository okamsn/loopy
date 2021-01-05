;; Run these tests using:
;; emacs -Q --batch -l ert -l tests.el -f ert-run-tests-batch-and-exit

(push (expand-file-name ".")
      load-path)

(require 'cl-lib)
(require 'ert)
(require 'loopy "./loopy.el")

;;; Outside the Loop
;;;; Before Do
;; `before-do' always runs, and occurs before the loop.
(ert-deftest basic-before-do ()
  (should (= 4
             (eval (quote (loopy (with (i 3))
                                 (before-do (setq i (1+ i)))
                                 (loop (return i))))))))

;;;; After Do - runs after loop is loop completed successfully
(ert-deftest basic-after-do ()
  (should (and (eq t (eval (quote (loopy (with (my-ret nil))
                                         (loop (list i '(1 2 3 4)))
                                         (after-do (setq my-ret t))
                                         (finally-return my-ret)))))
               (eq nil (eval (quote (loopy (with (my-ret nil))
                                           (loop (list i '(1 2 3 4))
                                                 (return nil))
                                           (after-do (setq my-ret t))
                                           (finally-return my-ret))))))))

;;;; Before and After
(ert-deftest basic-before-and-after-test ()
  (should (= 3 (eval (quote (loopy (with (i 1))
                                   (before-do (cl-incf i))
                                   (loop (repeat 1))
                                   (after-do (cl-incf i))
                                   (finally-return i)))))))

;;;; Final Instructions
(ert-deftest finally-do ()
  (should (= 10
             (let (my-var)
               (eval (quote (loopy ((list i (number-sequence 1 10)))
                                   (finally-do (setq my-var i)))))
               my-var))))

(ert-deftest finally-do-not-affect-return ()
  (should (eq nil
              (eval (quote (loopy ((list i (number-sequence 1 10)))
                                  (finally-do 3)))))))

(ert-deftest finally-return-single-value ()
  (should (= 10
             (eval (quote (loopy ((list i (number-sequence 1 10)))
                                 (finally-return i)))))))

(ert-deftest finally-return-list-of-values ()
  (should (equal '(10 7)
                 (eval (quote (loopy ((list i (number-sequence 1 10)))
                                     (finally-return i 7)))))))

;;;; Changing the order of macro arguments.
(ert-deftest change-order-of-commands ()
  (should (= 7
             (eval (quote (loopy ((list i '(1 2 3)))
                                 (finally-return (+ i a))
                                 (with (a 4))))))))

;;;; Default return values.
(ert-deftest default-return-nil ()
  (should (not (or (eval (quote (loopy ((list i '(1 2 3))))))
                   (eval (quote (loopy ((repeat 1))
                                       (finally-do (1+ 1)))))))))

;;; Loop Commands
;;;; Generic Evaluation
;;;;; Do
(ert-deftest do ()
  (should
   (equal '(t nil)
          (eval (quote (loopy (with (my-val nil)
                                    (this-nil? t))
                              (loop (do (setq my-val t)
                                        (setq this-nil? (not my-val)))
                                    (return nil))
                              (finally-return my-val this-nil?)))))))

;;;;; Expr
(ert-deftest expr-one-value ()
  (should
   (and (eval (quote (loopy (with (my-val nil))
                            (loop (expr my-val t)
                                  (return nil))
                            (finally-return my-val))))
        (equal '(t t) (eval (quote (loopy ((expr (i j) '(t t))
                                           (return nil))
                                          (return i j))))))))

(ert-deftest expr-two-values ()
  (should
   (and
    (equal '(1 2 2)
            (eval (quote (loopy (loop (repeat 3)
                                      (expr my-val 1 2)
                                      (collect my-coll my-val))
                                (finally-return my-coll)))))
    (equal '((1 1) (2 2) (2 2))
            (eval (quote (loopy (loop (repeat 3)
                                      (expr (i j) '(1 1) '(2 2))
                                      (collect my-coll (list i j)))
                                (finally-return my-coll))))))))

;; Implementation is different for more than 2 values.
(ert-deftest expr-five-values ()
  (should
   (and (equal '(1 2 3 4 5 5 5 5 5 5)
                (eval (quote (loopy (loop (repeat 10)
                                          (expr my-val 1 2 3 4 5)
                                          (collect my-coll my-val))
                                    (finally-return my-coll)))))
        (equal '((1 1) (2 2) (3 3) (4 4) (5 5) (5 5) (5 5) (5 5) (5 5) (5 5))
                (eval (quote (loopy (loop (repeat 10)
                                          (expr (i j) '(1 1) '(2 2)
                                                '(3 3) '(4 4) '(5 5))
                                          (collect my-coll (list i j)))
                                    (finally-return my-coll))))))))

(ert-deftest expr-dont-repeat ()
  "Make sure commands don't repeatedly create/declare the same variable."
  (should
   (= 1 (with-temp-buffer
          (prin1 (macroexpand '(loopy (loop (expr j 3)
                                            (expr j 4)
                                            (return j))))
                 (current-buffer))
          (goto-char (point-min))
          (how-many "(j nil)")))))

;;;;; Group
(ert-deftest group ()
  (should
   (equal '((2 4 6) (2 4 6))
          (eval (quote (loopy ((list i '(1 2 3 4 5 6))
                               (if (cl-evenp i)
                                   (group (collect c1 i)
                                          (collect c2 i))))
                              (return c1 c2)))))))

;;;; Iteration
;;;;; Array
(ert-deftest array ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy (loop (array i [1 2 3])
                                           (collect coll i))
                                     (return coll)))))))

(ert-deftest array-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy ((array (a . b)
                                                  [(1 . 2) (3 . 4) (5 . 6)]))
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy ((array (a . b)
                                                  [(1 2) (3 4) (5 6)]))
                                          (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy ((array (a b c d)
                                                  [(1 2 3 4) (4 5 6 7)]))
                                          (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy ((array [i j k] [[1 2 3] [4 5 6]]))
                                          (return i j k))))))))


(ert-deftest array-recursive-destructuring ()
  (should
   (and
    (equal '(5 5 6)
           (eval (quote (loopy ((array (a [b c]) [(1 [1 2]) (5 [5 6])]))
                               (finally-return (list a b c))))))
    (equal '(4 5 6)
           (eval
            (quote
             (loopy ((array [a [b c]] [[1 [2 3]] [4 [5 6]]]))
                    (return a b c)))))
    (equal '(4 5 6)
           (eval
            (quote
             (loopy ((array [a [b [c]]] [[1 [2 [3]]] [4 [5 [6]]]]))
                    (return a b c)))))
    (equal '(4 5 6)
           (eval
            (quote
             (loopy ((array [a (b c)] [[1 (2 3)] [4 (5 6)]]))
                    (return a b c))))))))

;;;;; Array Ref
(ert-deftest array-ref ()
  (should (equal "aaa"
                 (eval (quote (loopy (with (my-str "cat"))
                                     (loop (array-ref i my-str)
                                           (do (setf i ?a)))
                                     (return my-str)))))))


(ert-deftest array-ref-destructuring ()
  (should (and (equal [(7 8 9) (7 8 9)]
                      (eval (quote (loopy (with (my-array [(1 2 3) (4 5 6)]))
                                          ((array-ref (i j k) my-array)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k 9)))
                                          (return my-array)))))
               (equal [(7 8 9 10) (7 8 9 10)]
                      (eval (quote (loopy (with (my-array [(1 2 3 4) (4 5 6 8)]))
                                          ((array-ref (i j . k) my-array)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k '(9 10))))
                                          (return my-array)))))
               (equal [[7 8 9 4] [7 8 9 8]]
                      (eval (quote (loopy (with (my-array [[1 2 3 4] [4 5 6 8]]))
                                          ((array-ref [i j k] my-array)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k 9)))
                                          (return my-array))))))))

(ert-deftest array-ref-recursive-destructuring ()
  (should (and (equal [(7 [8 9]) (7 [8 9])]
                      (eval (quote (loopy (with (my-array [(1 [2 3]) (4 [5 6])]))
                                          ((array-ref (i [j k]) my-array)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k 9)))
                                          (return my-array)))))
               (equal [[7 [8 9] 4] [7 [8 9] 8]]
                      (eval (quote (loopy (with (my-array [[1 [2 3] 4] [4 [5 6] 8]]))
                                          ((array-ref [i [j k]] my-array)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k 9)))
                                          (return my-array))))))))

;;;; Cons
(ert-deftest cons ()
  (should
   (and (cl-every (lambda (list)
                    (equal list '((1 2 3 4) (2 3 4) (3 4) (4))))
                  (list (eval (quote (loopy ((cons x '(1 2 3 4))
                                             (collect coll x))
                                            (return coll))))
                        (eval (quote (loopy ((cons x '(1 2 3 4) cdr)
                                             (collect coll x))
                                            (return coll))))))
        (cl-every (lambda (list)
                    (equal list '((1 2 3 4) (3 4))))
                  (list (eval (quote (loopy ((cons x '(1 2 3 4) #'cddr)
                                             (collect coll x))
                                            (return coll))))
                        (eval (quote (loopy ((cons x '(1 2 3 4)
                                                   (lambda (x) (cddr x)))
                                             (collect coll x))
                                            (return coll))))))
        (equal '((1 (2 3 4)) (2 (3 4)) (3 (4)) (4 nil))
               (eval (quote (loopy ((cons (i . j) '(1 2 3 4))
                                    (collect coll (list i j)))
                                   (return coll)))))
        (equal '((1 (2 3 4)) (3 (4)))
               (eval (quote (loopy ((cons (i . j) '(1 2 3 4) #'cddr)
                                    (collect coll (list i j)))
                                   (return coll))))))))


;;;; List
(ert-deftest list ()
  (should (= 3 (eval (quote (loopy (loop (list i '(1 2 3)))
                                   ;; Same thing:
                                   ;; (after-do (cl-return i))
                                   (finally-return i)))))))

(ert-deftest list-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy ((list (a . b)
                                                 '((1 . 2) (3 . 4) (5 . 6))))
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy ((list (a . b)
                                                 '((1 2) (3 4) (5 6))))
                                          (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy ((list (a b c d)
                                                 '((1 2 3 4) (4 5 6 7))))
                                          (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy ((list [i j k] '([1 2 3] [4 5 6])))
                                          (return i j k))))))))

(ert-deftest list-recursive-destructuring ()
  (should
   (and
    (equal '(5 5 6)
           (eval (quote (loopy ((list (a (b c)) '((1 (1 2)) (5 (5 6)))))
                               (finally-return (list a b c))))))
    (equal '(5 5 6)
           ;; This is more of an evaluation-time test.
           (eval (quote (loopy ((list (a . (b c)) '((1 . (1 2)) (5 . (5 6)))))
                               (finally-return (list a b c))))))
    (equal '(4 5 6)
           (loopy ((list (a . [b c]) '((1 . [2 3]) (4 . [5 6]))))
                  (return a b c)))
    (equal '(5 5 6)
           (eval (quote (loopy ((list (a (b (c))) '((1 (1 (2))) (5 (5 (6))))))
                               (finally-return (list a b c)))))))))

;;;; List Ref
(ert-deftest list-ref ()
  (should (equal  '(7 7 7)
                  (eval (quote (loopy (with (my-list '(1 2 3)))
                                      (loop (list-ref i my-list)
                                            (do (setf i 7)))
                                      (return my-list)))))))

(ert-deftest list-ref-destructuring ()
  (should (and (equal '((7 8 9) (7 8 9))
                      (eval (quote (loopy (with (my-list '((1 2 3) (4 5 6))))
                                          ((list-ref (i j k) my-list)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k 9)))
                                          (return my-list)))))
               (equal '((7 8 9 10) (7 8 9 10))
                      (eval (quote (loopy (with (my-list '((1 2 3 4) (4 5 6 8))))
                                          ((list-ref (i j . k) my-list)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k '(9 10))))
                                          (return my-list)))))
               (equal '([7 8 9 4] [7 8 9 8])
                      (eval (quote (loopy (with (my-list '([1 2 3 4] [4 5 6 8])))
                                          ((list-ref [i j k] my-list)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k 9)))
                                          (return my-list))))))))

(ert-deftest list-ref-recursive-destructuring ()
  (should (and (equal '((7 (8 9)) (7 (8 9)))
                      (eval (quote (loopy (with (my-list '((1 (2 3)) (4 (5 6)))))
                                          ((list-ref (i (j k)) my-list)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k 9)))
                                          (return my-list)))))
               (equal '([7 (8 9) 4] [7 (8 9) 8])
                      (eval (quote (loopy (with (my-list '([1 (2 3) 4] [4 (5 6) 8])))
                                          ((list-ref [i (j k) l] my-list)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k 9)))
                                          (return my-list))))))))

;;;; Repeat
(ert-deftest repeat-no-var ()
  (should (= 3 (length (eval (quote (loopy (loop (repeat 3)
                                                 (list i (number-sequence 1 10))
                                                 (collect coll i))
                                           (finally-return coll))))))))

(ert-deftest repeat-var ()
  "Need to test order of execution and functionality."
  (should (equal '(0 1 2)
                 (eval (quote (loopy ((collect coll i)
                                      (repeat i 3))
                                     (finally-return coll)))))))

;;;; Seq
(ert-deftest seq ()
  (should (eval (quote (loopy ((seq l '(1 2 3 4 5))
                               (seq a [1 2 3 4 5])
                               (if (/= l a)
                                   (return nil)))
                              (finally-return t))))))

(ert-deftest seq-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy ((seq (a . b)
                                                [(1 . 2) (3 . 4) (5 . 6)]))
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy ((seq (a . b)
                                                [(1 2) (3 4) (5 6)]))
                                          (finally-return a b)))))
               (equal '(4 5 6)
                      (eval (quote (loopy ((seq (a b c)
                                                [(1 2 3) (4 5 6)]))
                                          (finally-return a b c)))))
               (equal '(5 6)
                      (eval (quote (loopy ((seq (a . b)
                                                '((1 . 2) (3 . 4) (5 . 6))))
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy ((seq (a . b)
                                                '((1 2) (3 4) (5 6))))
                                          (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy ((seq (a b c d)
                                                '((1 2 3 4) (4 5 6 7))))
                                          (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy ((seq [i j k] '([1 2 3] [4 5 6])))
                                          (return i j k)))))
               (equal '(4 5 6)
                      (eval (quote (loopy ((seq [i j k] [[1 2 3] [4 5 6]]))
                                          (return i j k))))))))

(ert-deftest seq-ref-destructuring ()
  (should (and (equal [(7 8 9) (7 8 9)]
                      (eval (quote (loopy (with (my-seq [(1 2 3) (4 5 6)]))
                                          ((seq-ref (i j k) my-seq)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k 9)))
                                          (return my-seq)))))
               (equal [(7 8 9 10) (7 8 9 10)]
                      (eval (quote (loopy (with (my-seq [(1 2 3 4) (4 5 6 8)]))
                                          ((seq-ref (i j . k) my-seq)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k '(9 10))))
                                          (return my-seq)))))
               (equal '((7 8 9) (7 8 9))
                       (eval (quote (loopy (with (my-seq '((1 2 3) (4 5 6))))
                                           ((seq-ref (i j k) my-seq)
                                            (do (setf i 7)
                                                (setf j 8)
                                                (setf k 9)))
                                           (return my-seq)))))
               (equal '((7 8 9 10) (7 8 9 10))
                       (eval (quote (loopy (with (my-seq '((1 2 3 4) (4 5 6 8))))
                                           ((seq-ref (i j . k) my-seq)
                                            (do (setf i 7)
                                                (setf j 8)
                                                (setf k '(9 10))))
                                           (return my-seq)))))
               (equal '([7 8 9 4] [7 8 9 8])
                       (eval (quote (loopy (with (my-seq '([1 2 3 4] [4 5 6 8])))
                                           ((seq-ref [i j k] my-seq)
                                            (do (setf i 7)
                                                (setf j 8)
                                                (setf k 9)))
                                           (return my-seq)))))
               (equal [[7 8 9 4] [7 8 9 8]]
                      (eval (quote (loopy (with (my-seq [[1 2 3 4] [4 5 6 8]]))
                                          ((seq-ref [i j k] my-seq)
                                           (do (setf i 7)
                                               (setf j 8)
                                               (setf k 9)))
                                          (return my-seq))))))))

;;;; Seq Ref
(ert-deftest seq-ref ()
  (should
   (equal '(7 7 7 7)
           (eval (quote (loopy (with (my-seq '(1 2 3 4)))
                               (loop (seq-ref i my-seq)
                                     (do (setf i 7)))
                               (return my-seq)))))))

;;; Accumulation
(ert-deftest append ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy ((list i '((1 2 3) (4 5 6)))
                                      (append coll i))
                                     (return coll)))))))

(ert-deftest append-implicit ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy ((list i '((1 2 3) (4 5 6)))
                                      (append i))))))))

(ert-deftest collect ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy ((list j '(1 2 3))
                                      (collect coll j))
                                     (finally-return coll)))))))

(ert-deftest collect-destructuring ()
  (should (and (equal '((1 4) ((2 3) (5 6)))
                      (eval (quote (loopy ((list j '((1 2 3) (4 5 6)))
                                           (collect (coll1 . coll2) j))
                                          (return coll1 coll2)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy ((list j '((1 2 3) (4 5 6)))
                                           (collect (coll1 coll2 coll3) j))
                                          (return coll1 coll2 coll3)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy ((list j '([1 2 3] [4 5 6]))
                                           (collect [coll1 coll2 coll3] j))
                                          (return coll1 coll2 coll3))))))))
(ert-deftest collect-implicit ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy ((list j '(1 2 3))
                                      (collect j))))))))

(ert-deftest concat ()
  (should (equal "catdog"
                 (eval (quote (loopy ((list j '("cat" "dog"))
                                      (concat coll j))
                                     (finally-return coll)))))))

(ert-deftest concat-destructuring ()
  (should (and (equal '("ad" "be" "cf")
                      (eval (quote (loopy ((list j '(("a" "b" "c") ("d" "e" "f")))
                                           (concat (coll1 coll2 coll3) j))
                                          (return coll1 coll2 coll3)))))

               (equal '("ad" "be" "cf")
                      (eval (quote (loopy ((list j '(["a" "b" "c"] ["d" "e" "f"]))
                                           (concat [coll1 coll2 coll3] j))
                                          (return coll1 coll2 coll3))))))))

(ert-deftest concat-implict ()
  (should (equal "catdog"
                 (eval (quote (loopy ((list j '("cat" "dog"))
                                      (concat j))))))))

(ert-deftest count ()
  (should (= 2
             (eval (quote (loopy ((list i '(t nil t nil))
                                  (count c i))
                                 (return c)))))))

(ert-deftest count-destructuring ()
  (should
   (equal '(2 1)
          (eval (quote (loopy ((list elem '((t nil) (t t)))
                               (count (c1 c2) elem))
                              (return c1 c2)))))))

(ert-deftest count-implict ()
  (should (= 2
             (eval (quote (loopy ((list i '(t nil t nil))
                                  (count i))))))))

(ert-deftest max ()
  (should (= 11
             (eval (quote (loopy ((list i '(1 11 2 10 3 9 4 8 5 7 6))
                                  (max my-max i))
                                 (return my-max)))))))

(ert-deftest max-destructuring ()
  (should
   (equal '(9 11)
          (eval (quote (loopy ((list elem '((1 11) (9 4)))
                               (max (m1 m2) elem))
                              (return m1 m2)))))))

(ert-deftest max-implict ()
  (should (= 11
             (eval (quote (loopy ((list i '(1 11 2 10 3 9 4 8 5 7 6))
                                  (max i))))))))

(ert-deftest min ()
  (should
   (= 0
      (eval (quote (loopy ((list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                           (min my-min i))
                          (return my-min)))))))

(ert-deftest min-destructuring ()
  (should
   (equal '(1 4)
          (eval (quote (loopy ((list elem '((1 11) (9 4)))
                               (min (m1 m2) elem))
                              (return m1 m2)))))))

(ert-deftest min-implict ()
  (should
   (= 0
      (eval (quote (loopy ((list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                           (min i))))))))

(ert-deftest nconc ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy ((list i '((1 2 3) (4 5 6)))
                                      (nconc l i))
                                     (return l)))))))

(ert-deftest nconc-destructuring ()
  (should
   (equal '((1 4) ((2 3) (5 6)))
          (eval (quote (loopy ((list elem '(((1) (2 3)) ((4) (5 6))))
                               (nconc (n1 . n2) elem))
                              (return n1 n2)))))))

(ert-deftest nconc-implict ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy ((list i '((1 2 3) (4 5 6)))
                                      (nconc l i))))))))

(ert-deftest push-into ()
  (should (equal '(3 2 1)
                 (eval (quote (loopy ((list j '(1 2 3))
                                      (push-into coll j))
                                     (finally-return coll)))))))

(ert-deftest push-into-destructuring ()
  (should (equal '((5 3 1) (6 4 2))
                 (eval (quote (loopy ((list elem '((1 2) (3 4) (5 6)))
                                      (push-into (p1 p2) elem))
                                     (return p1 p2)))))))

(ert-deftest push-into-implict ()
  (should (equal '(3 2 1)
                 (eval (quote (loopy ((list j '(1 2 3))
                                      (push-into j))))))))

(ert-deftest sum ()
  (should (= 6
             (eval (quote (loopy ((list i '(1 2 3))
                                  (sum s i))
                                 (return s)))))))

(ert-deftest sum-destructuring ()
  (should (equal '(5 7 9)
                 (loopy ((list elem '((1 2 3) (4 5 6)))
                         (sum (sum1 sum2 sum3) elem))
                        (return sum1 sum2 sum3)))))

(ert-deftest sum-implict ()
  (should (= 6
             (eval (quote (loopy ((list i '(1 2 3))
                                  (sum i))))))))

(ert-deftest vconcat ()
  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy ((list elem '([1 2 3 4 5 6]
                                                   [7 8 9 10 11 12]))
                                      (vconcat v elem))
                                     (return v)))))))

(ert-deftest vconcat-destructuring ()
  (should (equal '([1 2 3 7 8 9] [4 5 6 10 11 12])
                 (eval (quote (loopy ((list elem '(([1 2 3] [4 5 6])
                                                   ([7 8 9] [10 11 12])))
                                      (vconcat (v1 v2) elem))
                                     (return v1 v2)))))))

(ert-deftest vconcat-implict ()
  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy ((list elem '([1 2 3 4 5 6]
                                                   [7 8 9 10 11 12]))
                                      (vconcat elem))))))))

(ert-deftest accumulation-recursive-destructuring ()
  (should
   (and
    (equal '(4 6 8)
           (eval (quote (loopy ((list i '((1 (2 3)) (3 (4 5))))
                                (sum (s1 (s2 s3)) i))
                               (return s1 s2 s3)))))
    (equal '(4 6 8)
           (eval (quote (loopy ((list i '((1 (2 . 3)) (3 (4 . 5))))
                                (sum (s1 (s2 . s3)) i))
                               (return s1 s2 s3)))))
    (equal '(4 6 8)
           (eval (quote (loopy ((array i [[1 [2 3]] [3 [4 5]]])
                                (sum [s1 [s2 s3]] i))
                               (return s1 s2 s3)))))
    (equal '(4 6 8 10)
           (eval (quote (loopy ((list i '((1 (2 . [3 4])) (3 (4 . [5 6]))))
                                (sum (s1 (s2 . [s3 s4])) i))
                               (return s1 s2 s3 s4)))))
    (equal '((1 3) (2 4) (3 5) (4 6))
           (eval (quote (loopy ((list i '((1 (2 . [3 4])) (3 (4 . [5 6]))))
                                (collect (c1 (c2 . [c3 c4])) i))
                               (return c1 c2 c3 c4))))))))

;;; Control Flow
;;;; Conditionals
;;;;; If
(ert-deftest if ()
  (should (equal '((2 4) (1 3))
                 (loopy ((list i '(1 2 3 4))
                         (if (cl-evenp i)
                             (collect evens i)
                           (collect odds i)))
                        (return evens odds)))))

;;;;; When
;; (ert-deftest basic-when-parse ()
;;   (should (equal (loopy--parse-conditional-forms 'when 't '((do (+ 1 1))))
;;                  '((loopy--main-body when t (progn (+ 1 1)))))))

(ert-deftest recursive-when-test ()
  (should (equal
           (eval (quote (loopy ((list i (number-sequence 1 10))
                                (list j '(1 2 3 6 7 8))
                                (when (cl-evenp i)
                                  (when (> j i)
                                    (return (cons j i))))))))
           '(6 . 4))))

(ert-deftest when-multiple-subcommands ()
  (should (equal '(2 (1 3))
                  (loopy (with (counter 0))
                         ((list i '(1 2 3))
                          (when (cl-oddp i)
                            (collect odds i)
                            (do (cl-incf counter))))
                         (return counter odds)))))

(ert-deftest multi-when-prepend-test ()
  (should
   (string=
    (eval (quote (loopy (with (first-var 2)
                              (second-var 3))
                        ((seq el [1 2 3 4 5 6 7])
                         ;; Could also use (do (cond ...)).
                         (when (zerop (mod el first-var))
                           (push-into msg-coll (format "Multiple of 2: %d" el)))
                         (when (zerop (mod el second-var))
                           (push-into msg-coll (format "Multiple of 3: %d" el))))
                        (finally-return (string-join (nreverse msg-coll) "\n")))))
    "Multiple of 2: 2
Multiple of 3: 3
Multiple of 2: 4
Multiple of 2: 6
Multiple of 3: 6")))

;;;;; Unless
(ert-deftest multi-unless-prepend-test ()
  (should
   (string=
    (eval (quote (loopy (with (first-var 2)
                              (second-var 3))
                        ((seq el [1 2 3 4 5 6 7])
                         ;; Could also use (do (cond ...)).
                         (unless (zerop (mod el first-var))
                           (push-into msg-coll (format "Not multiple of 2: %d" el)))
                         (unless (zerop (mod el second-var))
                           (push-into msg-coll (format "Not multiple of 3: %d" el))))
                        (finally-return (string-join (nreverse msg-coll) "\n")))))
    "Not multiple of 2: 1
Not multiple of 3: 1
Not multiple of 3: 2
Not multiple of 2: 3
Not multiple of 3: 4
Not multiple of 2: 5
Not multiple of 3: 5
Not multiple of 2: 7
Not multiple of 3: 7")))

;;;;; Cond FORMS
;; (ert-deftest parse-cond-form ()
;;   (should (equal (loopy--parse-cond-form '(((= a 1)
;;                                             (do (message "hi")))
;;                                            ((= b 2)
;;                                             (return 5))))
;;                  '((loopy--main-body cond
;;                                      ((= a 1) (progn (message "hi")))
;;                                      ((= b 2) (cl-return-from nil 5)))))))

(ert-deftest cond ()
  (should (equal (eval
                  (quote
                   (loopy ((list i (number-sequence 0 5))
                           (cond ((cl-evenp i)
                                  (push-into evens i)
                                  (push-into holding-list evens))
                                 (t (push-into odds i))))
                          (finally-return (list evens odds holding-list)))))
                 '((4 2 0) (5 3 1) ((4 2 0) (2 0) (0))))))

;; ;; Exiting the Loop Early
;;;;; Return
(ert-deftest return ()
  (should (= 6 (eval (quote (loopy (with  (j 0))
                                   ((do (cl-incf j))
                                    (when (> j 5)
                                      (return j)))))))))

;;;;; Return From
(ert-deftest return-from-single-loop ()
  (should (= 6
             (eval (quote (loopy my-loop
                                 ((list i (number-sequence 1 10))
                                  (when (> i 5)
                                    (return-from my-loop i)))))))))

(ert-deftest return-from-outer-loop ()
  (should
   (= 6
      (eval (quote (loopy outer
                          ;; Could use ‘sum’ command, but don’t want dependencies.
                          (with (sum 0))
                          (loop (list sublist '((1 2 3 4 5) (6 7 8 9) (10 11)))
                                (do (loopy (loop (list i sublist)
                                                 (do (setq sum (+ sum i)))
                                                 (when (> sum 15)
                                                   (return-from outer i))))))))))))

(ert-deftest return-commands-multiple-values ()
  (should
   (and
    (equal '(1 2 3 4)
           (eval (quote (loopy ((return 1 2 3 4))))))
    (equal '(1 2 3 4)
           (eval (quote (loopy my-loop ((return-from my-loop 1 2 3 4)))))))))

;;;;; Skip
(ert-deftest skip ()
  (should (cl-every #'cl-oddp
                    (eval (quote (loopy ((seq i (number-sequence 1 10))
                                         (when (cl-evenp i)
                                           (skip))
                                         (push-into my-collection i))
                                        (finally-return (nreverse my-collection))))))))


;;; Custom Commands
(ert-deftest custom-command-sum ()
  (cl-defun my-loopy-sum-command ((_ target &rest items))
    "Set TARGET to the sum of ITEMS."
    `((loopy--explicit-vars . (,target nil))
      (loopy--main-body . (setq ,target (apply #'+ (list ,@items))))))
  (setq-local loopy-custom-command-parsers
              (list (cons 'target-sum #'my-loopy-sum-command)))
  (should (= 6
             (eval (quote (loopy (loop (target-sum my-target 1 2 3)
                                       (return nil))
                                 (finally-return my-target)))))))

;; NOTE: Also tests that post-conditions work as expected.
(ert-deftest custom-command-always ()
  (cl-defun my--loopy-always-command-parser ((_ &rest conditions))
    "Parse a command of the form `(always [CONDITIONS])'.
     If any condition is `nil', `loopy' should immediately return nil.
     Otherwise, `loopy' should return t."
    (let (instructions)
      ;; Return t if loop completes successfully.
      (push `(loopy--after-do . (cl-return t)) instructions)
      ;; Check all conditions at the end of the loop body, forcing an exit if any
      ;; evaluate to nil.  Since the default return value of the macro is nil, we
      ;; don’t need to do anything else.
      ;;
      ;; NOTE: We must not add anything to `loopy--final-return', since that
      ;;       would override the value of any early returns.
      (dolist (condition conditions)
        (push `(loopy--post-conditions . ,condition) instructions))
      instructions))

  (add-to-list 'loopy-custom-command-parsers
               (cons 'always #'my--loopy-always-command-parser))

  ;; One condition: => t
  (should (and
           (eval (quote
                  (loopy ((list i (number-sequence 1 9)) (always (< i 10))))))

           ;; Two conditions: => nil
           (not (eval (quote
                       (loopy ((list i (number-sequence 1 9))
                               (list j '(2 4 6 8 9))
                               (always (< i 10) (cl-evenp j))))))))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; flycheck-emacs-lisp-load-path: ("./.")
;; End:
