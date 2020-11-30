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
             (loopy (with (i 3))
                    (before-do (setq i (1+ i)))
                    (loop (return i))))))

;;;; After Do - runs after loop is loop completed successfully
(ert-deftest basic-after-do ()
  (should (and (eq t (loopy (with (my-ret nil))
                            (loop (list i '(1 2 3 4)))
                            (after-do (setq my-ret t))
                            (finally-return my-ret)))
               (eq nil (loopy (with (my-ret nil))
                              (loop (list i '(1 2 3 4))
                                    (leave))
                              (after-do (setq my-ret t))
                              (finally-return my-ret))))))

;;;; Before and After
(ert-deftest loopy-basic-before-and-after-test ()
  (should (= 3 (loopy (with (i 1))
                      (before-do (cl-incf i))
                      (loop (repeat 1))
                      (after-do (cl-incf i))
                      (finally-return i)))))

;;; Iteration
;;;; Array
(ert-deftest loopy-basic-array-test ()
  (should (equal '(1 2 3)
                 (loopy (loop (array i [1 2 3])
                              (collect coll i))
                        (return coll)))))

;;;; Array Ref
(ert-deftest loopy-basic-array-ref-test ()
  (should (equal "aaa"
                 (loopy (with (my-str "cat"))
                        (loop (array-ref i my-str)
                              (do (setf i ?a)))
                        (return my-str)))))

;;;; Cons
(ert-deftest loopy-basic-cons-test ()
  (should (and (equal (loopy ((cons x '(1 2 3 4))
                              (collect coll x))
                             (return coll))
                      '((1 2 3 4) (2 3 4) (3 4) (4)))
               (equal (loopy ((cons x '(1 2 3 4) #'cddr)
                              (collect coll x))
                             (return coll))
                      '((1 2 3 4) (3 4))))))

;;;; Expr
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

(ert-deftest expr-multi-arg-test ()
  (should (equal (loopy ((repeat 10)
                         (expr i 1 2 3)
                         (collect coll i))
                        (return coll))
                 '(1 2 3 3 3 3 3 3 3 3))))

;;;; List
(ert-deftest loopy-basic-list-test ()
  (should (= 3 (loopy (loop (list i '(1 2 3)))
                      ;; Same thing:
                      ;; (after-do (cl-return i))
                      (finally-return i)))))

;;;; List Ref
(ert-deftest loopy-basic-list-ref-test ()
  (should (equal  '(7 7 7)
                  (loopy (with (my-list '(1 2 3)))
                         (loop (list-ref i my-list)
                               (do (setf i 7)))
                         (return my-list)))))

;;;; Repeat
(ert-deftest loopy-basic-repeat-test ()
  (should (= 3 (length (loopy (loop (repeat 3)
                                    (list i (number-sequence 1 10))
                                    (collect coll i))
                              (finally-return coll))))))

(ert-deftest loopy-basic-repeat-var-test ()
  "Need to test order of execution and functionality."
  (should (equal '(0 1 2)
                 (loopy ((collect coll i)
                         (repeat i 3))
                        (finally-return coll)))))

;;;; Seq
(ert-deftest loopy-basic-seq-test ()
  (should (loopy ((seq l '(1 2 3 4 5))
                  (seq a [1 2 3 4 5])
                  (if (/= l a)
                      (return nil)))
                 (return t))))

;;;; Seq Ref
(ert-deftest loopy-basic-seq-ref-test ()
  (should
   (equal '(7 7 7 7)
          (loopy (with (my-seq '(1 2 3 4)))
                 (loop (seq-ref i my-seq)
                       (do (setf i 7)))
                 (return my-seq)))))

(cl-loop for var being the elements of-ref '(1 2 3)
         collect i)

;;; Leaving, Returning, Skipping
(ert-deftest mod-when-test ()
  "Check WHEN processing."
  (should (equal (loopy ((seq i (number-sequence 1 20))
                         (when (zerop (mod i 10))
                           (skip))
                         (when (cl-evenp i)
                           (prepend my-collection i)))
                        (finally-return (nreverse my-collection)))
                 '(2 4 6 8 12 14 16 18))))

(ert-deftest leave-named ()
  (should (= 6
             (loopy outer
                    ((list i (number-sequence 1 10))
                     (when (> i 5)
                       (leave-named-loop outer i)))))))

(ert-deftest leave-outer-named ()
  (should (eq 6
              (loopy
               outer
               ((list outer-i (number-sequence 1 10))
                (expr ret-loop
                      (loopy inner
                             ((expr inner-sum (+ outer-i 10))
                              (when (> inner-sum 15)
                                (leave-named-loop outer outer-i))
                              ;; Note: Without explicit return, inner loop is
                              ;; infinite.
                              (return)))))))))

(loopy ((seq i (number-sequence 1 20))
        (when (zerop (mod i 10))
          (skip))
        (when (cl-evenp i)
          (prepend my-collection i)))
       (finally-return (nreverse my-collection)))

;;; Conditionals
;;;; When

(ert-deftest basic-when-parse ()
  (should (equal (loopy--parse-conditional-forms 'when 't '((do (+ 1 1))))
                 '((loopy--loop-body when t (progn (+ 1 1)))))))

(ert-deftest multi-when-prepend-test ()
  (should
   (string=
    (loopy (with (first-var 2)
                 (second-var 3))
           ((seq el [1 2 3 4 5 6 7])
            ;; Could also use (do (cond ...)).
            (when (zerop (mod el first-var))
              (prepend msg-coll (message "Multiple of 2: %d" el)))
            (when (zerop (mod el second-var))
              (prepend msg-coll (message "Multiple of 3: %d" el))))
           (finally-return (string-join (nreverse msg-coll) "\n")))
    "Multiple of 2: 2
Multiple of 3: 3
Multiple of 2: 4
Multiple of 2: 6
Multiple of 3: 6")))

(ert-deftest recursive-when-test ()
  (should (equal
           (loopy ((list i (number-sequence 1 10))
                   (list j '(1 2 3 6 7 8))
                   (when (cl-evenp i)
                     (when (> j i)
                       (do (message "J > I"))
                       (return (cons j i))))))
           '(6 . 4))))

(ert-deftest multi-when-prepend-test ()
  (should
   (string=
    (loopy (with (first-var 2)
                 (second-var 3))
           ((seq el [1 2 3 4 5 6 7])
            ;; Could also use (do (cond ...)).
            ()
            (when (zerop (mod el first-var))
              (prepend msg-coll (message "Multiple of 2: %d" el)))
            (when (zerop (mod el second-var))
              (prepend msg-coll (message "Multiple of 3: %d" el))))
           (finally-return (string-join (nreverse msg-coll) "\n")))
    "Multiple of 2: 2
Multiple of 3: 3
Multiple of 2: 4
Multiple of 2: 6
Multiple of 3: 6")))

;;;; Unless
(ert-deftest multi-unless-prepend-test ()
  (should
   (string=
    (loopy (with (first-var 2)
                 (second-var 3))
           ((seq el [1 2 3 4 5 6 7])
            ;; Could also use (do (cond ...)).
            (unless (zerop (mod el first-var))
              (prepend msg-coll (message "Not multiple of 2: %d" el)))
            (unless (zerop (mod el second-var))
              (prepend msg-coll (message "Not multiple of 3: %d" el))))
           (finally-return (string-join (nreverse msg-coll) "\n")))
    "Not multiple of 2: 1
Not multiple of 3: 1
Not multiple of 3: 2
Not multiple of 2: 3
Not multiple of 3: 4
Not multiple of 2: 5
Not multiple of 3: 5
Not multiple of 2: 7
Not multiple of 3: 7")))

;;;; Cond FORMS
(ert-deftest parse-cond-form ()
  (should (equal (loopy--parse-cond-form '(((= a 1)
                                             (do (message "hi")))
                                            ((= b 2)
                                             (return 5))))
                 '((loop-body cond
                              ((= a 1) (progn (message "hi")))
                              ((= b 2) (cl-return-from nil 5)))))))

(ert-deftest parse-cond-loop ()
  (should (equal (loopy ((list i (number-sequence 1 10))
                         (cond
                          ((cl-evenp i)
                           (prepend evens i))
                          (t (prepend odds i))))
                        (finally-return (list evens odds)))
                 '((10 8 6 4 2) (9 7 5 3 1)))))

;;;; Final Instructions
(loopy ((list i (number-sequence 1 10)))
       (finally-do (message "Last i: %d" i)
                   (message "Less 1: %d" (1- i))))

(loopy ((list i (number-sequence 1 10)))
       (finally-do 3))

(loopy ((list i (number-sequence 1 10)))
       (finally-return i))

(loopy ((list i (number-sequence 1 10)))
       (finally-return i 7))

;;;; Changing the order of commands.
(loopy ((list i '(1 2 3)))
       (finally-return (+ i a))
       (with (a 4)))

;;;; Accumulation
(should (equal '(3 2 1)
               (loopy ((list j '(1 2 3))
                       (prepend coll j))
                      (finally-return coll))))

(should (equal '(1 2 3)
               (loopy ((list j '(1 2 3))
                       (collect coll j))
                      (finally-return coll))))

(should (equal "catdog"
               (loopy ((list j '("cat" "dog"))
                       (concat coll j))
                      (finally-return coll))))

(loopy ((list i '(t nil t nil))
        (count c i))
       (return c))

(loopy ((list i '(1 2 3))
        (sum s i))
       (return s))

(loopy ((list i '((1 2 3) (4 5 6)))
        (nconc l i))
       (return l))

;;; Extensions
;;TODO: How to test this?
;; (cl-defun my-loopy-greet-command ((_ first &optional last))
;;   "Greet one with first name FIRST and optional last name LAST."
;;   `((loopy--main-body . (if ,last
;;                             (message "Hello, %s %s" ,first ,last)
;;                           (message "Hello, %s" ,first)))))
;;
;; (push (cons 'greet #'my-loopy-greet-command)
;;       loopy-custom-command-parsers)
;;
;; (loopy ((list name '(("John" "Deer") ("Jane" "Doe") ("Jimmy")))
;;         (greet (car name) (cadr name))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; flycheck-emacs-lisp-load-path: ("./.")
;; End:
