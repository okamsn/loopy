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
                                                 (leave))
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

;;; Loop Commands
;;;; Generic Evaluation
;;;;; Do
(ert-deftest do ()
  (should
   (eval (quote (loopy (with (my-val nil))
                       (loop (do (setq my-val t))
                             (leave))
                       (finally-return my-val))))))

;;;;; Expr
(ert-deftest expr-one-value ()
  (should
   (eval (quote (loopy (with (my-val nil))
                       (loop (expr my-val t)
                             (leave))
                       (finally-return my-val))))))

(ert-deftest expr-two-values ()
  (should
   (equal '(1 2 2)
          (eval (quote (loopy (loop (repeat 3)
                                    (expr my-val 1 2)
                                    (collect my-coll my-val))
                              (finally-return my-coll)))))))

;; Implementation is different for more than 2 values.
(ert-deftest expr-five-values ()
  (should
   (equal '(1 2 3 4 5 5 5 5 5 5)
          (eval (quote (loopy (loop (repeat 10)
                                    (expr my-val 1 2 3 4 5)
                                    (collect my-coll my-val))
                              (finally-return my-coll)))))))

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

;;;; Iteration
;;;;; Array
(ert-deftest array ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy (loop (array i [1 2 3])
                                           (collect coll i))
                                     (return coll)))))))

;;;;; Array Ref
(ert-deftest array-ref ()
  (should (equal "aaa"
                 (eval (quote (loopy (with (my-str "cat"))
                                     (loop (array-ref i my-str)
                                           (do (setf i ?a)))
                                     (return my-str)))))))

;;;; Cons
(ert-deftest cons ()
  (should (and (equal (eval (quote (loopy ((cons x '(1 2 3 4))
                                           (collect coll x))
                                          (return coll))))
                      '((1 2 3 4) (2 3 4) (3 4) (4)))
               (equal (eval (quote (loopy ((cons x '(1 2 3 4) #'cddr)
                                           (collect coll x))
                                          (return coll))))
                      '((1 2 3 4) (3 4))))))

;;;; List
(ert-deftest list ()
  (should (= 3 (eval (quote (loopy (loop (list i '(1 2 3)))
                                   ;; Same thing:
                                   ;; (after-do (cl-return i))
                                   (finally-return i)))))))

;;;; List Ref
(ert-deftest list-ref ()
  (should (equal  '(7 7 7)
                  (eval (quote (loopy (with (my-list '(1 2 3)))
                                      (loop (list-ref i my-list)
                                            (do (setf i 7)))
                                      (return my-list)))))))

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

(ert-deftest collect ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy ((list j '(1 2 3))
                                      (collect coll j))
                                     (finally-return coll)))))))

(ert-deftest concat ()
  (should (equal "catdog"
                 (eval (quote (loopy ((list j '("cat" "dog"))
                                      (concat coll j))
                                     (finally-return coll)))))))

(ert-deftest count ()
  (should (= 2
             (eval (quote (loopy ((list i '(t nil t nil))
                                  (count c i))
                                 (return c)))))))

(ert-deftest max ()
  (should (= 11
             (eval (quote (loopy ((list i '(1 11 2 10 3 9 4 8 5 7 6))
                                  (max my-max i))
                                 (return my-max)))))))

(ert-deftest min ()
  (should
   (= 0
      (eval (quote (loopy ((list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                           (min my-min i))
                          (return my-min)))))))

(ert-deftest nconc ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy ((list i '((1 2 3) (4 5 6)))
                                      (nconc l i))
                                     (return l)))))))

(ert-deftest push-into ()
  (should (equal '(3 2 1)
                 (eval (quote (loopy ((list j '(1 2 3))
                                      (push-into coll j))
                                     (finally-return coll)))))))

(ert-deftest sum ()
  (should (= 6
             (eval (quote (loopy ((list i '(1 2 3))
                                  (sum s i))
                                 (return s)))))))

(ert-deftest vconcat ()
  (should (equal [1 2 3 4 5 6]
                 (eval (quote (loopy ((list i '([1 2 3] [4 5 6]))
                                      (vconcat vector i))
                                     (return vector)))))))

;;; Control Flow
;;;; Conditionals
;;;;; When
(ert-deftest basic-when-parse ()
  (should (equal (loopy--parse-conditional-forms 'when 't '((do (+ 1 1))))
                 '((loopy--main-body when t (progn (+ 1 1)))))))

(ert-deftest recursive-when-test ()
  (should (equal
           (eval (quote (loopy ((list i (number-sequence 1 10))
                                (list j '(1 2 3 6 7 8))
                                (when (cl-evenp i)
                                  (when (> j i)
                                    (return (cons j i))))))))
           '(6 . 4))))

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
(ert-deftest parse-cond-form ()
  (should (equal (loopy--parse-cond-form '(((= a 1)
                                            (do (message "hi")))
                                           ((= b 2)
                                            (return 5))))
                 '((loopy--main-body cond
                                     ((= a 1) (progn (message "hi")))
                                     ((= b 2) (cl-return-from nil 5)))))))

(ert-deftest parse-cond-loop ()
  (should (equal (eval (quote (loopy ((list i (number-sequence 1 10))
                                      (cond
                                       ((cl-evenp i)
                                        (push-into evens i))
                                       (t (push-into odds i))))
                                     (finally-return (list evens odds)))))
                 '((10 8 6 4 2) (9 7 5 3 1)))))

;;;; Exiting the Loop Early
;;;;; Leave
(ert-deftest leave ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy ((list i '(1 2 3 "cat" 4 5 6))
                                      (if (numberp i)
                                          (collect coll i)
                                        (leave)))
                                     (return coll)))))))

;;;;; Leave From
(ert-deftest leave-from-single-loop ()
  (should (= 6
             (eval (quote (loopy my-loop
                                 ((list i (number-sequence 1 10))
                                  (when (> i 5)
                                    (leave-from my-loop)))
                                 (return i)))))))

(ert-deftest leave-from-outer-loop ()
  (should
   (= 21
      (eval (quote (loopy outer
                          ;; Could use ‘sum’ command, but don’t want dependencies.
                          (with (sum 0))
                          (loop (list sublist '((1 2 3 4 5) (6 7 8 9) (10 11)))
                                (do (loopy (loop (list i sublist)
                                                 (do (setq sum (+ sum i)))
                                                 (when (> sum 15)
                                                   (leave-from outer))))))
                          (finally-return sum)))))))

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

;;;;; Skip
(ert-deftest skip ()
  (should (cl-every #'cl-oddp
                    (eval (quote (loopy ((seq i (number-sequence 1 10))
                                         (when (cl-evenp i)
                                           (skip))
                                         (push-into my-collection i))
                                        (finally-return (nreverse my-collection))))))))


;;; Custom Commands
(ert-deftest custom-command ()
  (cl-defun my-loopy-sum-command ((_ target &rest items))
    "Set TARGET to the sum of ITEMS."
    `((loopy--explicit-vars . (,target nil))
      (loopy--main-body . (setq ,target (apply #'+ (list ,@items))))))
  (setq-local loopy-custom-command-parsers
              (list (cons 'target-sum #'my-loopy-sum-command)))
  (should (= 6
             (eval (quote (loopy (loop (target-sum my-target 1 2 3)
                                       (leave))
                                 (finally-return my-target)))))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; flycheck-emacs-lisp-load-path: ("./.")
;; End:
