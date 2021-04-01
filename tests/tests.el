;; Run these tests using:
;; emacs -Q --batch -l ert -l tests.el -f ert-run-tests-batch-and-exit

(push (expand-file-name ".")
      load-path)

(require 'cl-lib)
(require 'ert)
(require 'loopy "./loopy.el")

;;; Macro arguments
;;;; With
(ert-deftest with-arg-order ()
  (should (and (= 4
                  (eval (quote (loopy (with (a 2)
                                            (b (+ a 2)))
                                      (return b)))))
               (= 4
                  (eval (quote (loopy (let* (a 2)
                                        (b (+ a 2)))
                                      (return b))))))))

(ert-deftest with-destructuring ()
  (should (= -2
             (eval (quote (loopy (with ((a b) '(1 2))
                                       ([c d] `[,(1+ a) ,(1+ b)]))
                                 (return (+ (- a b)
                                            (- c d)))))))))

;;;; Without
(ert-deftest without ()
  (should (and (equal '(4 5)
                      (let ((a 1) (b 2))
                        (eval (quote (loopy (with (c 3))
                                            (without a b)
                                            (expr a (+ a c))
                                            (expr b (+ b c))
                                            (return a b))))))
               (equal '(4 5)
                      (let ((a 1) (b 2))
                        (eval (quote (loopy (with (c 3))
                                            (no-init a b)
                                            (expr a (+ a c))
                                            (expr b (+ b c))
                                            (return a b)))))))))

;;;; Before Do
;; `before-do' always runs, and occurs before the loop.
(ert-deftest basic-before-do ()
  (should (and (= 4
                  (eval (quote (loopy (with (i 3))
                                      (before-do (setq i (1+ i)))
                                      (return i)))))
               (= 4
                  (eval (quote (loopy (with (i 3))
                                      (before (setq i (1+ i)))
                                      (return i)))))
               (= 4
                  (eval (quote (loopy (with (i 3))
                                      (initially-do (setq i (1+ i)))
                                      (return i)))))
               (= 4
                  (eval (quote (loopy (with (i 3))
                                      (initially (setq i (1+ i)))
                                      (return i))))))))

;;;; After Do - runs after loop is loop completed successfully
(ert-deftest basic-after-do ()
  (should (and (eq t (eval (quote (loopy (with (my-ret nil))
                                         (list i '(1 2 3 4))
                                         (after-do (setq my-ret t))
                                         (finally-return my-ret)))))
               (eq nil (eval (quote (loopy (with (my-ret nil))
                                           (list i '(1 2 3 4))
                                           (return nil)
                                           (after-do (setq my-ret t))
                                           (finally-return my-ret)))))
               (eq nil (eval (quote (loopy (with (my-ret nil))
                                           (list i '(1 2 3 4))
                                           (return nil)
                                           (after (setq my-ret t))
                                           (finally-return my-ret)))))
               (eq nil (eval (quote (loopy (with (my-ret nil))
                                           (list i '(1 2 3 4))
                                           (return nil)
                                           (else-do (setq my-ret t))
                                           (finally-return my-ret)))))
               (eq nil (eval (quote (loopy (with (my-ret nil))
                                           (list i '(1 2 3 4))
                                           (return nil)
                                           (else (setq my-ret t))
                                           (finally-return my-ret))))))))

;;;; Before and After
(ert-deftest basic-before-and-after-test ()
  (should (= 3 (eval (quote (loopy (with (i 1))
                                   (before-do (cl-incf i))
                                   (repeat 1)
                                   (after-do (cl-incf i))
                                   (finally-return i)))))))

;;;; Final Instructions
(ert-deftest finally-do ()
  (should (and (= 10
                  (let (my-var)
                    (eval (quote (loopy (list i (number-sequence 1 10))
                                        (finally-do (setq my-var i)))))
                    my-var))
               (= 10
                  (let (my-var)
                    (eval (quote (loopy (list i (number-sequence 1 10))
                                        (finally (setq my-var i)))))
                    my-var)))))

(ert-deftest finally-do-not-affect-return ()
  (should (eq nil
              (eval (quote (loopy (list i (number-sequence 1 10))
                                  (finally-do 3)))))))

(ert-deftest finally-return-single-value ()
  (should (= 10
             (eval (quote (loopy (list i (number-sequence 1 10))
                                 (finally-return i)))))))

(ert-deftest finally-return-list-of-values ()
  (should (equal '(10 7)
                 (eval (quote (loopy (list i (number-sequence 1 10))
                                     (finally-return i 7)))))))

;;;; Changing the order of macro arguments.
(ert-deftest change-order-of-commands ()
  (should (= 7
             (eval (quote (loopy (list i '(1 2 3))
                                 (finally-return (+ i a))
                                 (with (a 4))))))))

;;;; Default return values.
(ert-deftest default-return-nil ()
  (should (not (or (eval (quote (loopy (list i '(1 2 3)))))
                   (eval (quote (loopy (repeat 1)
                                       (finally-do (1+ 1)))))))))

;;; Loop Commands
;;;; Miscellaneous
;;;;; Sub Loop
(ert-deftest sub-loop-implicit-accum-in-loop ()
  (should (equal '((1 . 4) (1 . 5) (2 . 4) (2 . 5))
                 (eval (quote (loopy (list i '(1 2))
                                     (loop (list j '(4 5))
                                           (collect (cons i j))))))))
  (should (equal "14152425"
                 (eval (quote (loopy (list i '("1" "2"))
                                     (loop (list j '("4" "5"))
                                           (concat (concat i j))))))))

  (should (equal '(0 (1 . 4) (1 . 5) (2 . 4) (2 . 5))
                 (eval (quote (loopy (list i '(1 2))
                                     (loop (list j '(4 5))
                                           (collect (cons i j)))
                                     (finally-return (cons 0 loopy-result))))))))

(ert-deftest sub-loop-explicit-accum-in-loop ()
  (should (equal '(0 (1 . 4) (1 . 5) (2 . 4) (2 . 5))
                 (eval (quote (loopy (list i '(1 2))
                                     (loop (list j '(4 5))
                                           (collect my-coll (cons i j)))
                                     (finally-return (cons 0 my-coll)))))))
  (should (equal "014152425"
                 (eval (quote (loopy (list i '("1" "2"))
                                     (loop (list j '("4" "5"))
                                           (concat my-str (concat i j)))
                                     (finally-return (concat "0" my-str))))))))

(ert-deftest sub-loop-implicit-accum-in-named-loop ()
  "The sub-loop should be able to accumulate into the main loop's
implicit variable without knowing it's name, even for named loops."
  (should (equal '((1 . 3) (1 . 4) (2 . 3) (2 . 4))
                 (eval (quote (loopy outer
                                     (list i '(1 2))
                                     (loop inner
                                           (list j '(3 4))
                                           (collect (cons i j)))))))))

(ert-deftest sub-loop-leave-early ()
  "A `leave' in a sub-loop should not affect the outer loop."
  (should (equal '(1 2 3)
                 (eval (quote (loopy (list i '(1 2 3))
                                     (loop (list j '(4 5 6))
                                           (leave)
                                           (collect j))
                                     (collect i)))))))

(ert-deftest sub-loop-skip ()
  "A `skip' in a sub-loop should not affect the outer loop."
  (should (equal '(5 7 1 5 7 2 5 7 3)
                 (eval (quote (loopy  (list i '(1 2 3))
                                      (loop (list j '(4 5 6 7 8))
                                            (when (cl-evenp j)
                                              (continue))
                                            (collect j))
                                      (collect i)))))))

(ert-deftest sub-loop-return-from-outer ()
  (should (= 3 (loopy outer
                      (list i '(1 2 3))
                      (loop (list j '(4 5 6 3))
                            (when (= j i)
                              (return-from outer j)))))))

(ert-deftest sub-loop-named ()
  (should
   (equal
    '((3 5) (3 5))
    (eval (quote
           (loopy (repeat 2)
                  (loop inner1
                        (list j '(3 4))
                        (loop (list k '(5 6 7))
                              (if (= k 6)
                                  ;; Return from inner1 so never reach 4.
                                  (return-from inner1)
                                (collect (list j k)))))))))))

;;;; Generic Evaluation
;;;;; Do
(ert-deftest do ()
  (should
   (equal '(t nil)
          (eval (quote (loopy (with (my-val nil)
                                    (this-nil? t))
                              (do (setq my-val t)
                                  (setq this-nil? (not my-val)))
                              (return nil)
                              (finally-return my-val this-nil?)))))))

;;;;; Expr
(ert-deftest expr-one-value ()
  (should
   (and (eval (quote (loopy (with (my-val nil))
                            (expr my-val t)
                            (return nil)
                            (finally-return my-val))))
        (equal '(t t) (eval (quote (loopy (expr (i j) '(t t))
                                          (return nil) ; TODO: Change to leave.
                                          (finally-return i j))))))))

(ert-deftest expr-two-values ()
  (should
   (and
    (equal '(1 2 2)
           (eval (quote (loopy  (repeat 3)
                                (expr my-val 1 2)
                                (collect my-coll my-val)
                                (finally-return my-coll)))))
    (equal '((1 1) (2 2) (2 2))
           (eval (quote (loopy  (repeat 3)
                                (expr (i j) '(1 1) '(2 2))
                                (collect my-coll (list i j))
                                (finally-return my-coll))))))))

(ert-deftest expr-two-values-when ()
  (should (equal '(nil 0 0 1 1 2 2 3)
                 (loopy (list i '(1 2 3 4 5 6 7 8))
                        (when (cl-evenp i)
                          (expr j 0 (1+ j)))
                        (collect j)))))

(ert-deftest expr-three-values-when ()
  (should (equal '(nil a a 0 0 1 1 2)
                 (loopy (list i '(1 2 3 4 5 6 7 8))
                        (when (cl-evenp i)
                          (expr j 'a 0 (1+ j)))
                        (collect j)))))

;; Implementation is different for more than 2 values.
(ert-deftest expr-five-values ()
  (should
   (and (equal '(1 2 3 4 5 5 5 5 5 5)
               (eval (quote (loopy  (repeat 10)
                                    (expr my-val 1 2 3 4 5)
                                    (collect my-coll my-val)
                                    (finally-return my-coll)))))
        (equal '((1 1) (2 2) (3 3) (4 4) (5 5) (5 5) (5 5) (5 5) (5 5) (5 5))
               (eval (quote (loopy  (repeat 10)
                                    (expr (i j) '(1 1) '(2 2)
                                          '(3 3) '(4 4) '(5 5))
                                    (collect my-coll (list i j))
                                    (finally-return my-coll))))))))

(ert-deftest expr-dont-repeat ()
  "Make sure commands don't repeatedly create/declare the same variable."
  (should
   (= 1 (with-temp-buffer
          (prin1 (macroexpand '(loopy  (expr j 3)
                                       (expr j 4)
                                       (return j)))
                 (current-buffer))
          (goto-char (point-min))
          (how-many "(j nil)")))))

;;;;; Group
(ert-deftest group ()
  (should
   (equal '((2 4 6) (2 4 6))
          (eval (quote (loopy (list i '(1 2 3 4 5 6))
                              (if (cl-evenp i)
                                  (group (collect c1 i)
                                         (collect c2 i)))
                              (finally-return c1 c2)))))))

;;;; Iteration
;; Making sure iteration fails in sub-level
(ert-deftest iteration-sub-level ()
  (should-error
   (progn
     (loopy (if t (list i '(1))) (finally-return t))
     (loopy (if t (list-ref i '(1))) (finally-return t))
     (loopy (if t (array i '(1))) (finally-return t))
     (loopy (if t (array-ref i '(1))) (finally-return t))
     (loopy (if t (seq i '(1))) (finally-return t))
     (loopy (if t (seq-ref i '(1))) (finally-return t))
     (loopy (if t (repeat 1)) (finally-return t))
     (loopy (when t (list i '(1))) (finally-return t))
     (loopy (when t (list-ref i '(1))) (finally-return t))
     (loopy (when t (array i '(1))) (finally-return t))
     (loopy (when t (array-ref i '(1))) (finally-return t))
     (loopy (when t (seq i '(1))) (finally-return t))
     (loopy (when t (seq-ref i '(1))) (finally-return t))
     (loopy (when t (repeat 1)) (finally-return t))
     (loopy (unless t (list i '(1))) (finally-return t))
     (loopy (unless t (list-ref i '(1))) (finally-return t))
     (loopy (unless t (array i '(1))) (finally-return t))
     (loopy (unless t (array-ref i '(1))) (finally-return t))
     (loopy (unless t (seq i '(1))) (finally-return t))
     (loopy (unless t (seq-ref i '(1))) (finally-return t))
     (loopy (unless t (repeat 1)) (finally-return t))
     (loopy (cond (t (list i '(1)))) (finally-return t))
     (loopy (cond (t (list-ref i '(1)))) (finally-return t))
     (loopy (cond (t (array i '(1)))) (finally-return t))
     (loopy (cond (t (array-ref i '(1)))) (finally-return t))
     (loopy (cond (t (seq i '(1)))) (finally-return t))
     (loopy (cond (t (seq-ref i '(1)))) (finally-return t))
     (loopy (cond (t (repeat 1))) (finally-return t))
     (loopy (group (list i '(1))) (finally-return t))
     (loopy (group (list-ref i '(1))) (finally-return t))
     (loopy (group (array i '(1))) (finally-return t))
     (loopy (group (array-ref i '(1))) (finally-return t))
     (loopy (group (seq i '(1))) (finally-return t))
     (loopy (group (seq-ref i '(1))) (finally-return t))
     (loopy (group (repeat 1))) (finally-return t))
   :type 'user-error))

;;;;; Array
(ert-deftest array ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy  (array i [1 2 3])
                                      (collect coll i)
                                      (finally-return coll)))))))

(ert-deftest array-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy (array (a . b)
                                                 [(1 . 2) (3 . 4) (5 . 6)])
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy (array (a . b)
                                                 [(1 2) (3 4) (5 6)])
                                          (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy (array (a b c d)
                                                 [(1 2 3 4) (4 5 6 7)])
                                          (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy (array [i j k] [[1 2 3] [4 5 6]])
                                          (finally-return i j k))))))))


(ert-deftest array-recursive-destructuring ()
  (should
   (and
    (equal '(5 5 6)
           (eval (quote (loopy (array (a [b c]) [(1 [1 2]) (5 [5 6])])
                               (finally-return (list a b c))))))
    (equal '(4 5 6)
           (eval
            (quote
             (loopy (array [a [b c]] [[1 [2 3]] [4 [5 6]]])
                    (finally-return a b c)))))
    (equal '(4 5 6)
           (eval
            (quote
             (loopy (array [a [b [c]]] [[1 [2 [3]]] [4 [5 [6]]]])
                    (finally-return a b c)))))
    (equal '(4 5 6)
           (eval
            (quote
             (loopy (array [a (b c)] [[1 (2 3)] [4 (5 6)]])
                    (finally-return a b c))))))))

;;;;; Array Ref
(ert-deftest array-ref ()
  (should (equal "aaa"
                 (eval (quote (loopy (with (my-str "cat"))
                                     (array-ref i my-str)
                                     (do (setf i ?a))
                                     (finally-return my-str)))))))


(ert-deftest array-ref-destructuring ()
  (should (and (equal [(7 8 9) (7 8 9)]
                      (eval (quote (loopy (with (my-array [(1 2 3) (4 5 6)]))
                                          (array-ref (i j k) my-array)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-array)))))
               (equal [(7 8 9 10) (7 8 9 10)]
                      (eval (quote (loopy (with (my-array [(1 2 3 4) (4 5 6 8)]))
                                          (array-ref (i j . k) my-array)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k '(9 10)))
                                          (finally-return my-array)))))
               (equal [[7 8 9 4] [7 8 9 8]]
                      (eval (quote (loopy (with (my-array [[1 2 3 4] [4 5 6 8]]))
                                          (array-ref [i j k] my-array)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-array))))))))

(ert-deftest array-ref-recursive-destructuring ()
  (should (and (equal [(7 [8 9]) (7 [8 9])]
                      (eval (quote (loopy (with (my-array [(1 [2 3]) (4 [5 6])]))
                                          (array-ref (i [j k]) my-array)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-array)))))
               (equal [[7 [8 9] 4] [7 [8 9] 8]]
                      (eval (quote (loopy (with (my-array [[1 [2 3] 4] [4 [5 6] 8]]))
                                          (array-ref [i [j k]] my-array)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-array))))))))

;;;;; Cons
(ert-deftest cons ()
  (should
   (and (cl-every (lambda (list)
                    (equal list '((1 2 3 4) (2 3 4) (3 4) (4))))
                  (list (eval (quote (loopy (cons x '(1 2 3 4))
                                            (collect coll x)
                                            (finally-return coll))))
                        (eval (quote (loopy (cons x '(1 2 3 4) cdr)
                                            (collect coll x)
                                            (finally-return coll))))))
        (cl-every (lambda (list)
                    (equal list '((1 2 3 4) (3 4))))
                  (list (eval (quote (loopy (cons x '(1 2 3 4) #'cddr)
                                            (collect coll x)
                                            (finally-return coll))))
                        (eval (quote (loopy (cons x '(1 2 3 4)
                                                  (lambda (x) (cddr x)))
                                            (collect coll x)
                                            (finally-return coll))))))
        (equal '((1 (2 3 4)) (2 (3 4)) (3 (4)) (4 nil))
               (eval (quote (loopy (cons (i . j) '(1 2 3 4))
                                   (collect coll (list i j))
                                   (finally-return coll)))))
        (equal '((1 (2 3 4)) (3 (4)))
               (eval (quote (loopy (cons (i . j) '(1 2 3 4) #'cddr)
                                   (collect coll (list i j))
                                   (finally-return coll))))))))


;;;;; List
(ert-deftest list ()
  (should (= 3 (eval (quote (loopy  (list i '(1 2 3))
                                    ;; Same thing:
                                    ;; (after-do (cl-return i))
                                    (finally-return i)))))))

(ert-deftest list-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy (list (a . b)
                                                '((1 . 2) (3 . 4) (5 . 6)))
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy (list (a . b)
                                                '((1 2) (3 4) (5 6)))
                                          (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy (list (a b c d)
                                                '((1 2 3 4) (4 5 6 7)))
                                          (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy (list [i j k] '([1 2 3] [4 5 6]))
                                          (finally-return i j k))))))))

(ert-deftest list-recursive-destructuring ()
  (should
   (and
    (equal '(5 5 6)
           (eval (quote (loopy (list (a (b c)) '((1 (1 2)) (5 (5 6))))
                               (finally-return (list a b c))))))
    (equal '(5 5 6)
           ;; This is more of an evaluation-time test.
           (eval (quote (loopy (list (a . (b c)) '((1 . (1 2)) (5 . (5 6))))
                               (finally-return (list a b c))))))
    (equal '(4 5 6)
           (loopy (list (a . [b c]) '((1 . [2 3]) (4 . [5 6])))
                  (finally-return a b c)))
    (equal '(5 5 6)
           (eval (quote (loopy (list (a (b (c))) '((1 (1 (2))) (5 (5 (6)))))
                               (finally-return (list a b c)))))))))

;;;;; List Ref
(ert-deftest list-ref ()
  (should (equal  '(7 7 7)
                  (eval (quote (loopy (with (my-list '(1 2 3)))
                                      (list-ref i my-list)
                                      (do (setf i 7))
                                      (finally-return my-list)))))))

(ert-deftest list-ref-destructuring ()
  (should (and (equal '((7 8 9) (7 8 9))
                      (eval (quote (loopy (with (my-list '((1 2 3) (4 5 6))))
                                          (list-ref (i j k) my-list)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-list)))))
               (equal '((7 8 9 10) (7 8 9 10))
                      (eval (quote (loopy (with (my-list '((1 2 3 4) (4 5 6 8))))
                                          (list-ref (i j . k) my-list)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k '(9 10)))
                                          (finally-return my-list)))))
               (equal '([7 8 9 4] [7 8 9 8])
                      (eval (quote (loopy (with (my-list '([1 2 3 4] [4 5 6 8])))
                                          (list-ref [i j k] my-list)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-list))))))))

(ert-deftest list-ref-recursive-destructuring ()
  (should (and (equal '((7 (8 9)) (7 (8 9)))
                      (eval (quote (loopy (with (my-list '((1 (2 3)) (4 (5 6)))))
                                          (list-ref (i (j k)) my-list)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-list)))))
               (equal '([7 (8 9) 4] [7 (8 9) 8])
                      (eval (quote (loopy (with (my-list '([1 (2 3) 4] [4 (5 6) 8])))
                                          (list-ref [i (j k) l] my-list)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-list))))))))

;;;;; Repeat
(ert-deftest repeat-no-var ()
  (should (= 3 (length (eval (quote (loopy  (repeat 3)
                                            (list i (number-sequence 1 10))
                                            (collect coll i)
                                            (finally-return coll))))))))

(ert-deftest repeat-var ()
  "Need to test order of execution and functionality."
  (should (equal '(0 1 2)
                 (eval (quote (loopy (collect coll i)
                                     (repeat i 3)
                                     (finally-return coll)))))))

;;;;; Seq
(ert-deftest seq ()
  (should (eval (quote (loopy (seq l '(1 2 3 4 5))
                              (seq a [1 2 3 4 5])
                              (if (/= l a)
                                  (return nil))
                              (finally-return t))))))

(ert-deftest seq-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy (seq (a . b)
                                               [(1 . 2) (3 . 4) (5 . 6)])
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy (seq (a . b)
                                               [(1 2) (3 4) (5 6)])
                                          (finally-return a b)))))
               (equal '(4 5 6)
                      (eval (quote (loopy (seq (a b c)
                                               [(1 2 3) (4 5 6)])
                                          (finally-return a b c)))))
               (equal '(5 6)
                      (eval (quote (loopy (seq (a . b)
                                               '((1 . 2) (3 . 4) (5 . 6)))
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy (seq (a . b)
                                               '((1 2) (3 4) (5 6)))
                                          (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy (seq (a b c d)
                                               '((1 2 3 4) (4 5 6 7)))
                                          (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy (seq [i j k] '([1 2 3] [4 5 6]))
                                          (finally-return i j k)))))
               (equal '(4 5 6)
                      (eval (quote (loopy (seq [i j k] [[1 2 3] [4 5 6]])
                                          (finally-return i j k))))))))

(ert-deftest seq-ref-destructuring ()
  (should (and (equal [(7 8 9) (7 8 9)]
                      (eval (quote (loopy (with (my-seq [(1 2 3) (4 5 6)]))
                                          (seq-ref (i j k) my-seq)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-seq)))))
               (equal [(7 8 9 10) (7 8 9 10)]
                      (eval (quote (loopy (with (my-seq [(1 2 3 4) (4 5 6 8)]))
                                          (seq-ref (i j . k) my-seq)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k '(9 10)))
                                          (finally-return my-seq)))))
               (equal '((7 8 9) (7 8 9))
                      (eval (quote (loopy (with (my-seq '((1 2 3) (4 5 6))))
                                          (seq-ref (i j k) my-seq)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-seq)))))
               (equal '((7 8 9 10) (7 8 9 10))
                      (eval (quote (loopy (with (my-seq '((1 2 3 4) (4 5 6 8))))
                                          (seq-ref (i j . k) my-seq)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k '(9 10)))
                                          (finally-return my-seq)))))
               (equal '([7 8 9 4] [7 8 9 8])
                      (eval (quote (loopy (with (my-seq '([1 2 3 4] [4 5 6 8])))
                                          (seq-ref [i j k] my-seq)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-seq)))))
               (equal [[7 8 9 4] [7 8 9 8]]
                      (eval (quote (loopy (with (my-seq [[1 2 3 4] [4 5 6 8]]))
                                          (seq-ref [i j k] my-seq)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-seq))))))))

;;;;; Seq Ref
(ert-deftest seq-ref ()
  (should
   (equal '(7 7 7 7)
          (eval (quote (loopy (with (my-seq '(1 2 3 4)))
                              (seq-ref i my-seq)
                              (do (setf i 7))
                              (finally-return my-seq)))))))

;;;; Accumulation Commands
;;;;; Order of implicit returns.
(ert-deftest implicit-collect-order ()
  (should (equal '((2) (1 3))
                 (eval (quote (loopy (list i '(1 2 3))
                                     (if (cl-evenp i)
                                         (collect evens i)
                                       (collect odds i))))))))

;;;;; Name of implicit accumulations
(ert-deftest implicit-accumulation-name ()
  (should
   (and (equal '(1 2 3)
               (eval (quote (loopy (list i '(1 2 3))
                                   (collect i)
                                   (else-do (cl-return loopy-result))))))
        (equal '(0 1 2 3)
               (eval (quote (loopy (list i '(1 2 3))
                                   (collect i)
                                   (else-do
                                    (push 0 loopy-result)
                                    (cl-return loopy-result))))))
        (equal '(0 1 2 3)
               (eval (quote (loopy (list i '(1 2 3))
                                   (collect i)
                                   (finally-do
                                    (push 0 loopy-result))
                                   (finally-return loopy-result)))))
        (equal '(1 2 3)
               (eval (quote (loopy (list i '(1 2 3))
                                   (collect i)
                                   (finally-return loopy-result))))))))

;;;;; Split flag
(ert-deftest split-flag ()
  (should (equal '((1 3) (2))
                 (eval (quote (loopy (flag split)
                                     (list i '(1 2 3))
                                     (if (cl-oddp i)
                                         (collect i)
                                       (collect i))))))))
(ert-deftest split-flag-default ()
  (should (equal '((1 3) (2))
                 (let ((loopy-default-flags '(split)))
                   (eval (quote (loopy (list i '(1 2 3))
                                       (if (cl-oddp i)
                                           (collect i)
                                         (collect i)))))))))

(ert-deftest split-flag-default-disable ()
  (should (equal '(1 2 3)
                 (let ((loopy-default-flags '(split)))
                   (eval (quote (loopy (flag -split)
                                       (list i '(1 2 3))
                                       (if (cl-oddp i)
                                           (collect i)
                                         (collect i)))))))))

(ert-deftest split-flag-enable-disable ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy (flag +split -split)
                                     (list i '(1 2 3))
                                     (if (cl-oddp i)
                                         (collect i)
                                       (collect i))))))))

;;;;; Default flag
(ert-deftest default-flag-disable-split ()
  (should (equal '(1 2 3)
                 (let ((loopy-default-flags '(split)))
                   (eval (quote (loopy (flag default)
                                       (list i '(1 2 3))
                                       (if (cl-oddp i)
                                           (collect i)
                                         (collect i))))))))
  (should (equal '(1 2 3)
                 (eval (quote (loopy (flags split default)
                                     (list i '(1 2 3))
                                     (if (cl-oddp i)
                                         (collect i)
                                       (collect i))))))))

(ert-deftest default-flag-then-split ()
  (should (equal '((1 3) (2))
                 (eval (quote (loopy (flags default split)
                                     (list i '(1 2 3))
                                     (if (cl-oddp i)
                                         (collect i)
                                       (collect i))))))))

;;;;; Append
(ert-deftest append ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (append coll i)
                                     (finally-return coll))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (appending coll i)
                                     (finally-return coll)))))))

(ert-deftest append-destructuring ()
  (should (equal '((1 2 5 6) (3 4 7 8))
                 (eval (quote (loopy (array i [((1 2) (3 4)) ((5 6) (7 8))])
                                     (append (j k) i))))))
  (should (equal '((1 2 5 6) (3 4 7 8))
                 (eval (quote (loopy (array i [((1 2) (3 4)) ((5 6) (7 8))])
                                     (appending (j k) i)))))))

(ert-deftest append-implicit ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (append i))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (appending i)))))))

(ert-deftest collect ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (collect coll j)
                                     (finally-return coll))))))
  (should (equal '(1 2 3)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (collecting coll j)
                                     (finally-return coll)))))))

(ert-deftest collect-destructuring ()
  (should (and (equal '((1 4) ((2 3) (5 6)))
                      (eval (quote (loopy (list j '((1 2 3) (4 5 6)))
                                          (collect (coll1 . coll2) j)
                                          (finally-return coll1 coll2)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy (list j '((1 2 3) (4 5 6)))
                                          (collect (coll1 coll2 coll3) j)
                                          (finally-return coll1 coll2 coll3)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy (list j '([1 2 3] [4 5 6]))
                                          (collect [coll1 coll2 coll3] j)
                                          (finally-return coll1 coll2 coll3)))))))
  (should (and (equal '((1 4) ((2 3) (5 6)))
                      (eval (quote (loopy (list j '((1 2 3) (4 5 6)))
                                          (collecting (coll1 . coll2) j)
                                          (finally-return coll1 coll2)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy (list j '((1 2 3) (4 5 6)))
                                          (collecting (coll1 coll2 coll3) j)
                                          (finally-return coll1 coll2 coll3)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy (list j '([1 2 3] [4 5 6]))
                                          (collecting [coll1 coll2 coll3] j)
                                          (finally-return coll1 coll2 coll3))))))))
(ert-deftest collect-implicit ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (collect j))))))
  (should (equal '(1 2 3)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (collecting j)))))))

(ert-deftest concat ()
  (should (equal "catdog"
                 (eval (quote (loopy (list j '("cat" "dog"))
                                     (concat coll j)
                                     (finally-return coll))))))
  (should (equal "catdog"
                 (eval (quote (loopy (list j '("cat" "dog"))
                                     (concating coll j)
                                     (finally-return coll)))))))

(ert-deftest concat-destructuring ()
  (should (and (equal '("ad" "be" "cf")
                      (eval (quote (loopy (list j '(("a" "b" "c") ("d" "e" "f")))
                                          (concat (coll1 coll2 coll3) j)
                                          (finally-return coll1 coll2 coll3)))))

               (equal '("ad" "be" "cf")
                      (eval (quote (loopy (list j '(["a" "b" "c"] ["d" "e" "f"]))
                                          (concat [coll1 coll2 coll3] j)
                                          (finally-return coll1 coll2 coll3)))))))
  (should (and (equal '("ad" "be" "cf")
                      (eval (quote (loopy (list j '(("a" "b" "c") ("d" "e" "f")))
                                          (concating (coll1 coll2 coll3) j)
                                          (finally-return coll1 coll2 coll3)))))

               (equal '("ad" "be" "cf")
                      (eval (quote (loopy (list j '(["a" "b" "c"] ["d" "e" "f"]))
                                          (concating [coll1 coll2 coll3] j)
                                          (finally-return coll1 coll2 coll3))))))))

(ert-deftest concat-implict ()
  (should (equal "catdog"
                 (eval (quote (loopy (list j '("cat" "dog"))
                                     (concat j))))))
  (should (equal "catdog"
                 (eval (quote (loopy (list j '("cat" "dog"))
                                     (concating j)))))))

(ert-deftest count ()
  (should (= 2
             (eval (quote (loopy (list i '(t nil t nil))
                                 (count c i)
                                 (finally-return c))))))
  (should (= 2
             (eval (quote (loopy (list i '(t nil t nil))
                                 (counting c i)
                                 (finally-return c)))))))

(ert-deftest count-destructuring ()
  (should
   (equal '(2 1)
          (eval (quote (loopy (list elem '((t nil) (t t)))
                              (count (c1 c2) elem)
                              (finally-return c1 c2))))))
  (should
   (equal '(2 1)
          (eval (quote (loopy (list elem '((t nil) (t t)))
                              (counting (c1 c2) elem)
                              (finally-return c1 c2)))))))

(ert-deftest count-implict ()
  (should (= 2
             (eval (quote (loopy (list i '(t nil t nil))
                                 (count i))))))
  (should (= 2
             (eval (quote (loopy (list i '(t nil t nil))
                                 (counting i)))))))

(ert-deftest max ()
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (max my-max i)
                                 (finally-return my-max))))))
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (maxing my-max i)
                                 (finally-return my-max))))))
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (maximize my-max i)
                                 (finally-return my-max))))))
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (maximizing my-max i)
                                 (finally-return my-max)))))))

(ert-deftest max-destructuring ()
  (should
   (equal '(9 11)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (max (m1 m2) elem)
                              (finally-return m1 m2))))))
  (should
   (equal '(9 11)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (maxing (m1 m2) elem)
                              (finally-return m1 m2))))))
  (should
   (equal '(9 11)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (maximize (m1 m2) elem)
                              (finally-return m1 m2))))))
  (should
   (equal '(9 11)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (maximizing (m1 m2) elem)
                              (finally-return m1 m2)))))))

(ert-deftest max-implict ()
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (max i))))))
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (maxing i))))))
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (maximize i))))))
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (maximizing i)))))))

(ert-deftest min ()
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (min my-min i)
                          (finally-return my-min))))))
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (minimize my-min i)
                          (finally-return my-min))))))
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (minimizing my-min i)
                          (finally-return my-min))))))
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (minning my-min i)
                          (finally-return my-min)))))))

(ert-deftest min-destructuring ()
  (should
   (equal '(1 4)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (min (m1 m2) elem)
                              (finally-return m1 m2))))))
  (should
   (equal '(1 4)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (minimize (m1 m2) elem)
                              (finally-return m1 m2))))))
  (should
   (equal '(1 4)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (minning (m1 m2) elem)
                              (finally-return m1 m2))))))
  (should
   (equal '(1 4)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (minimizing (m1 m2) elem)
                              (finally-return m1 m2)))))))

(ert-deftest min-implict ()
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (min i))))))
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (minning i))))))
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (minimize i))))))
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (minimizing i)))))))

(ert-deftest nconc ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconc l i)
                                     (finally-return l))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconcing l i)
                                     (finally-return l)))))))

(ert-deftest nconc-destructuring ()
  (should
   (equal '((1 4) ((2 3) (5 6)))
          (eval (quote (loopy (list elem '(((1) (2 3)) ((4) (5 6))))
                              (nconc (n1 . n2) elem)
                              (finally-return n1 n2))))))
  (should
   (equal '((1 4) ((2 3) (5 6)))
          (eval (quote (loopy (list elem '(((1) (2 3)) ((4) (5 6))))
                              (nconcing (n1 . n2) elem)
                              (finally-return n1 n2)))))))

(ert-deftest nconc-implict ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconc l i))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconcing l i)))))))

(ert-deftest prepend ()
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepend my-list i)
                                     (finally-return my-list))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepending my-list i)
                                     (finally-return my-list))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepend my-list i))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepending my-list i)))))))

(ert-deftest prepend-destructuring ()
  (should (equal '((5 6 1 2) (7 8 3 4))
                 (eval (quote (loopy (list i '([(1 2) (3 4)] [(5 6) (7 8)]))
                                     (prepend [my-list1 my-list2] i))))))
  (should (equal '((5 6 1 2) (7 8 3 4))
                 (eval (quote (loopy (list i '([(1 2) (3 4)] [(5 6) (7 8)]))
                                     (prepending [my-list1 my-list2] i)))))))

(ert-deftest prepend-implicit ()
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepend i)
                                     (finally-return loopy-result))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepending i)
                                     (finally-return loopy-result))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepend i))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepending i)))))))


(ert-deftest push-into ()
  (should (equal '(3 2 1)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (push-into coll j)
                                     (finally-return coll))))))
  (should (equal '(3 2 1)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (pushing-into coll j)
                                     (finally-return coll))))))
  (should (equal '(3 2 1)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (push coll j)
                                     (finally-return coll))))))
  (should (equal '(3 2 1)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (pushing coll j)
                                     (finally-return coll)))))))

(ert-deftest push-into-destructuring ()
  (should (equal '((5 3 1) (6 4 2))
                 (eval (quote (loopy (list elem '((1 2) (3 4) (5 6)))
                                     (push-into (p1 p2) elem)
                                     (finally-return p1 p2))))))
  (should (equal '((5 3 1) (6 4 2))
                 (eval (quote (loopy (list elem '((1 2) (3 4) (5 6)))
                                     (pushing-into (p1 p2) elem)
                                     (finally-return p1 p2))))))
  (should (equal '((5 3 1) (6 4 2))
                 (eval (quote (loopy (list elem '((1 2) (3 4) (5 6)))
                                     (push (p1 p2) elem)
                                     (finally-return p1 p2))))))
  (should (equal '((5 3 1) (6 4 2))
                 (eval (quote (loopy (list elem '((1 2) (3 4) (5 6)))
                                     (pushing (p1 p2) elem)
                                     (finally-return p1 p2)))))))

(ert-deftest push-into-implict ()
  (should (equal '(3 2 1)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (push-into coll j))))))
  (should (equal '(3 2 1)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (pushing-into coll j))))))
  (should (equal '(3 2 1)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (push coll j))))))
  (should (equal '(3 2 1)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (pushing coll j)))))))

(ert-deftest sum ()
  (should (= 6
             (eval (quote (loopy (list i '(1 2 3))
                                 (sum s i)
                                 (finally-return s))))))
  (should (= 6
             (eval (quote (loopy (list i '(1 2 3))
                                 (summing s i)
                                 (finally-return s)))))))

(ert-deftest sum-destructuring ()
  (should (equal '(5 7 9)
                 (loopy (list elem '((1 2 3) (4 5 6)))
                        (sum (sum1 sum2 sum3) elem)
                        (finally-return sum1 sum2 sum3))))
  (should (equal '(5 7 9)
                 (loopy (list elem '((1 2 3) (4 5 6)))
                        (summing (sum1 sum2 sum3) elem)
                        (finally-return sum1 sum2 sum3)))))

(ert-deftest sum-implict ()
  (should (= 6
             (eval (quote (loopy (list i '(1 2 3))
                                 (sum i))))))
  (should (= 6
             (eval (quote (loopy (list i '(1 2 3))
                                 (summing i)))))))

(ert-deftest vconcat ()
  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy (list elem '([1 2 3 4 5 6]
                                                  [7 8 9 10 11 12]))
                                     (vconcat v elem)
                                     (finally-return v))))))
  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy (list elem '([1 2 3 4 5 6]
                                                  [7 8 9 10 11 12]))
                                     (vconcating v elem)
                                     (finally-return v)))))))

(ert-deftest vconcat-destructuring ()
  (should (equal '([1 2 3 7 8 9] [4 5 6 10 11 12])
                 (eval (quote (loopy (list elem '(([1 2 3] [4 5 6])
                                                  ([7 8 9] [10 11 12])))
                                     (vconcat (v1 v2) elem)
                                     (finally-return v1 v2))))))
  (should (equal '([1 2 3 7 8 9] [4 5 6 10 11 12])
                 (eval (quote (loopy (list elem '(([1 2 3] [4 5 6])
                                                  ([7 8 9] [10 11 12])))
                                     (vconcating (v1 v2) elem)
                                     (finally-return v1 v2)))))))

(ert-deftest vconcat-implict ()
  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy (list elem '([1 2 3 4 5 6]
                                                  [7 8 9 10 11 12]))
                                     (vconcat elem))))))
  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy (list elem '([1 2 3 4 5 6]
                                                  [7 8 9 10 11 12]))
                                     (vconcating elem)))))))

(ert-deftest accumulation-recursive-destructuring ()
  (should
   (and
    (equal '(4 6 8)
           (eval (quote (loopy (list i '((1 (2 3)) (3 (4 5))))
                               (sum (s1 (s2 s3)) i)
                               (finally-return s1 s2 s3)))))
    (equal '(4 6 8)
           (eval (quote (loopy (list i '((1 (2 . 3)) (3 (4 . 5))))
                               (sum (s1 (s2 . s3)) i)
                               (finally-return s1 s2 s3)))))
    (equal '(4 6 8)
           (eval (quote (loopy (array i [[1 [2 3]] [3 [4 5]]])
                               (sum [s1 [s2 s3]] i)
                               (finally-return s1 s2 s3)))))
    (equal '(4 6 8 10)
           (eval (quote (loopy (list i '((1 (2 . [3 4])) (3 (4 . [5 6]))))
                               (sum (s1 (s2 . [s3 s4])) i)
                               (finally-return s1 s2 s3 s4)))))
    (equal '((1 3) (2 4) (3 5) (4 6))
           (eval (quote (loopy (list i '((1 (2 . [3 4])) (3 (4 . [5 6]))))
                               (collect (c1 (c2 . [c3 c4])) i)
                               (finally-return c1 c2 c3 c4))))))))

;;; Control Flow
;;;; Conditionals
;;;;; If
(ert-deftest if ()
  (should (equal '((2 4) (1 3))
                 (loopy (list i '(1 2 3 4))
                        (if (cl-evenp i)
                            (collect evens i)
                          (collect odds i))
                        (finally-return evens odds)))))

;;;;; When
;; (ert-deftest basic-when-parse ()
;;   (should (equal (loopy--parse-conditional-forms 'when 't '((do (+ 1 1))))
;;                  '((loopy--main-body when t (progn (+ 1 1)))))))

(ert-deftest recursive-when-test ()
  (should (equal
           (eval (quote (loopy (list i (number-sequence 1 10))
                               (list j '(1 2 3 6 7 8))
                               (when (cl-evenp i)
                                 (when (> j i)
                                   (return (cons j i)))))))
           '(6 . 4))))

(ert-deftest when-multiple-subcommands ()
  (should (equal '(2 (1 3))
                 (loopy (with (counter 0))
                        (list i '(1 2 3))
                        (when (cl-oddp i)
                          (collect odds i)
                          (do (cl-incf counter)))
                        (finally-return counter odds)))))

(ert-deftest multi-when-prepend-test ()
  (should
   (string=
    (eval (quote (loopy (with (first-var 2)
                              (second-var 3))
                        (seq el [1 2 3 4 5 6 7])
                        ;; Could also use (do (cond ...)).
                        (when (zerop (mod el first-var))
                          (push-into msg-coll (format "Multiple of 2: %d" el)))
                        (when (zerop (mod el second-var))
                          (push-into msg-coll (format "Multiple of 3: %d" el)))
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
                        (seq el [1 2 3 4 5 6 7])
                        ;; Could also use (do (cond ...)).
                        (unless (zerop (mod el first-var))
                          (push-into msg-coll (format "Not multiple of 2: %d" el)))
                        (unless (zerop (mod el second-var))
                          (push-into msg-coll (format "Not multiple of 3: %d" el)))
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
                   (loopy (list i (number-sequence 0 5))
                          (cond ((cl-evenp i)
                                 (push-into evens i)
                                 (push-into holding-list evens))
                                (t (push-into odds i)))
                          (finally-return (list evens odds holding-list)))))
                 '((4 2 0) (5 3 1) ((4 2 0) (2 0) (0))))))

;;;; Exiting the Loop Early
;;;;; Leave
(ert-deftest leave ()
  (should (equal '(1)
                 (eval (quote (loopy (list i '(1))
                                     (collect i)
                                     (leave)))))))

;;;;; Return
(ert-deftest return ()
  (should (= 6 (eval (quote (loopy (with  (j 0))
                                   (do (cl-incf j))
                                   (when (> j 5)
                                     (return j))))))))

;;;;; Return From
(ert-deftest return-from-single-loop ()
  (should (= 6
             (eval (quote (loopy my-loop
                                 (list i (number-sequence 1 10))
                                 (when (> i 5)
                                   (return-from my-loop i))))))))

(ert-deftest return-from-outer-loop ()
  (should
   (= 6
      (eval (quote (loopy outer
                          ;; Could use ‘sum’ command, but don’t want dependencies.
                          (with (sum 0))
                          (list sublist '((1 2 3 4 5) (6 7 8 9) (10 11)))
                          (do (loopy  (list i sublist)
                                      (do (setq sum (+ sum i)))
                                      (when (> sum 15)
                                        (return-from outer i))))))))))

(ert-deftest return-commands-multiple-values ()
  (should
   (and
    (equal '(1 2 3 4)
           (eval (quote (loopy (return 1 2 3 4)))))
    (equal '(1 2 3 4)
           (eval (quote (loopy my-loop (return-from my-loop 1 2 3 4))))))))

;;;;; Skip
(ert-deftest skip ()
  (should (cl-every #'cl-oddp
                    (eval (quote (loopy (seq i (number-sequence 1 10))
                                        (when (cl-evenp i)
                                          (skip))
                                        (push-into my-collection i)
                                        (finally-return (nreverse my-collection))))))))

;;;;; While
(ert-deftest while ()
  (should (equal '(1 2)
                 (eval (quote (loopy (list i '(1 2 3 4 5 6))
                                     (while (< i 3))
                                     (collect i)))))))

;;;;; Until
(ert-deftest until ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy (list i '(1 2 3 4 5 6))
                               (until (> i 3))
                               (collect i)))))))

;;;;; Always
(ert-deftest always ()
  (should (equal t (eval (quote (loopy (list i '(1 2 3 4 5 6))
				 (always (< i 7)))))))
  (should (null
	   (eval (quote (loopy (list i '(1 2 3 4 5 6))
			 (always (> i 7))))))))

;;;;; Never
(ert-deftest never ()
  (should (equal nil
		 (eval (quote (loopy (list i '(1 2 3 4 5 6))
			       (never (> i 0)))))))
  (should (equal t
		 (eval (quote (loopy (list i '(1 2 3 4 5 6))
			       (never (< i 0))))))))

;;;;; Thereis
(ert-deftest thereis ()
  (should (= 6 (eval (quote (loopy (list i '(1 2 3 4 5 6))
			     (thereis (> i 5)))))))

  (should (= 9 (eval (quote (loopy (list i (number-sequence 1 9))
			     (thereis (and (> i 8) i)))))))
  (should (equal t
		 (eval (quote (loopy (list i '(1 2 3 4 5 6))
			       (thereis (> i 7))))))))

;;; Custom Commands
(ert-deftest custom-command-sum ()
  (cl-defun my-loopy-sum-command ((_ target &rest items))
    "Set TARGET to the sum of ITEMS."
    `((loopy--iteration-vars . (,target nil))
      (loopy--main-body . (setq ,target (apply #'+ (list ,@items))))))
  (setq-local loopy-custom-command-parsers
              (list (cons 'target-sum #'my-loopy-sum-command)))
  (should (= 6
             (eval (quote (loopy  (target-sum my-target 1 2 3)
                           (return nil)
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
                  (loopy (list i (number-sequence 1 9)) (always (< i 10)))))

           ;; Two conditions: => nil
           (not (eval (quote
                       (loopy (list i (number-sequence 1 9))
                              (list j '(2 4 6 8 9))
                              (always (< i 10) (cl-evenp j)))))))))

;;; Repeated evaluation of macro

;; This was an odd case reported by a user. See:
;; https://github.com/okamsn/loopy/issues/17
(ert-deftest evaluate-function-twice ()
  (should
   (progn
     (defun mu4e:other-path ()
       "Return load-path for mu4e.
This assumes that you're on guix."
       (loopy (with (regexp "Documents")
	            (base-dir (expand-file-name "~/")))
	      (list file (directory-files base-dir))
	      (expr full-path (expand-file-name file base-dir))))
     (mu4e:other-path)
     ;; If an `nreverse' goes bad, then the function value of `mu4e:other-path'
     ;; might be changed (somehow), which causes an error.
     (eq nil (mu4e:other-path)))))

;;; Custom Aliases
(setq loopy-custom-command-aliases
      '((f . flags)
        (as . with)
        (ignore . without)
        (precode . before-do)
        (postcode . after-do)
        (fd . finally-do)
        (fr . finally-return)))

(ert-deftest custom-alias-flag ()
  (should (equal '((1) (2))
                 (eval (quote (loopy (f split)
                                     (list i '(1))
                                     (collect i)
                                     (collect (1+ i))))))))

(ert-deftest custom-aliases-with ()
  (should (= 1
             (eval (quote (loopy (as (a 1))
                                 (return a)))))))

(ert-deftest custom-aliases-without ()
  (should (= 5 (let ((a 1)
                     (b 2))
                 (eval (quote (loopy (ignore a b)
                                     (repeat 1)
                                     (expr a 2)
                                     (expr b 3))))
                 (+ a b)))))

(ert-deftest custom-aliases-before-do ()
  (should (= 7 (eval (quote (loopy (with (i 2))
                                   (precode (setq i 7))
                                   (return i)))))))

(ert-deftest custom-aliases-after-do ()
  (should (eval (quote (loopy (with (my-ret nil))
                              (list i '(1 2 3 4))
                              (postcode (setq my-ret t))
                              (finally-return my-ret))))))

(ert-deftest custom-aliases-finally-do ()
  (should
   (= 10
      (let (my-var)
        (eval (quote (loopy (list i (number-sequence 1 10))
                            (fd (setq my-var i)))))
        my-var))))

(ert-deftest custom-aliases-finally-return ()
  (should (= 10
             (eval (quote (loopy (list i (number-sequence 1 10))
                                 (fr i)))))))

(ert-deftest custom-aliases-list ()
  (should (loopy-defalias l list))
  (should (loopy-defalias a 'array))
  (should (equal '((1 . 4) (2 . 5) (3 . 6))
                 (eval (quote (loopy (l i '(1 2 3))
                                     (a j [4 5 6])
                                     (collect (cons i j))))))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; flycheck-emacs-lisp-load-path: ("./.")
;; End:
