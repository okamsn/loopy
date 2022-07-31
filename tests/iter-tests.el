;;; Tests for `loopy-iter'  -*- lexical-binding: t; -*-

(push (expand-file-name ".")
      load-path)

(eval-when-compile (require 'loopy "./loopy.el")
                   (require 'loopy-iter "./loopy-iter.el"))
(require 'loopy "./loopy.el")
(require 'loopy-iter "./loopy-iter.el")
(require 'ert)
(require 'cl-lib)

(defmacro liq (&rest body)
  "`loopy' quote: Quote a use of `loopy'."
  `(eval (quote (loopy-iter ,@body))))

;; A list of special-form code walkers in Iterate. In Emacs Lisp, many of these
;; are macros, and so we should not need to test them, as they expand to simpler
;; constructs.  We now use `macroexpand-all' to find loop commands, so we
;; shouldn't have a problem with any of these.

;; | CL Special Form          | Elisp Macro              | Tested |
;; |--------------------------+--------------------------+--------|
;; | and                      | n                        | n      |
;; | block                    | y                        | n      |
;; | catch                    | n                        | y      |
;; | cond                     | n                        | n      |
;; | declare                  | y cl-declare, n declare  | n      |
;; | eval-when                | y                        | n      |
;; | flet                     | y                        | n      |
;; | function                 | n                        | y      |
;; | go                       | n, only exist in tagbody | n      |
;; | handler-bind             | n, doesn't exist         | n      |
;; | if                       | n                        | y      |
;; | ignore-errors            | y                        | y      |
;; | labels                   | y                        | n      |
;; | let                      | n                        | y      |
;; | let*                     | n                        | y      |
;; | load-time-value          | y                        | n      |
;; | locally                  | y                        | n      |
;; | multiple-value-bind      | y                        | n      |
;; | multiple-value-call      | n, alias of apply        | n      |
;; | multiple-value-list      | y                        | n      |
;; | multiple-value-prog1     | n, doesn't exist         | n      |
;; | multiple-value-setq      | y                        | n      |
;; | nth-value                | n, alias of nth          | n      |
;; | or                       | n                        | n      |
;; | prog1                    | n                        | n      |
;; | prog2                    | n                        | n      |
;; | progn                    | n                        | n      |
;; | progv                    | y                        | n      |
;; | psetq                    | y                        | n      |
;; | quote                    | n                        | y      |
;; | return-from              | y                        | n      |
;; | setq                     | n                        | y      |
;; | symbol-macrolet          | y                        | y      |
;; | tagbody                  | y                        | n      |
;; | the                      | y                        | n      |
;; | throw                    | n                        | y      |
;; | unwind-protect           | n                        | y      |
;; | with-hash-table-iterator | n, same                  | n      |
;; | with-package-iterator    | n, same                  | n      |

(ert-deftest wrap-catch ()
  (should (= 5 (eval (quote (loopy-iter (for list i '(1 2 3 4 5 6 7))
                                        (when (catch (progn
                                                       (for expr tag 'my-tag)
                                                       tag)
                                                (for set j (1+ i))
                                                (if (> j 5)
                                                    (throw tag t)))
                                          (exit return i)))))))

  (should (= 5 (eval (quote (loopy-iter (listing i '(1 2 3 4 5 6 7))
                                        (when (catch (progn
                                                       (setting tag 'my-tag)
                                                       tag)
                                                (setting j (1+ i))
                                                (if (> j 5)
                                                    (throw tag t)))
                                          (returning i))))))))

(ert-deftest wrap-throw ()
  (should (equal '(7 3 7 5 7)
                 (loopy-iter (for list i '(1 2 3 4 5))
                             (accum collect (catch 'my-tag
                                              (when (cl-evenp i)
                                                (throw (progn
                                                         (for set tag 'my-tag)
                                                         tag)
                                                       (progn
                                                         (for set val (1+ i))
                                                         val)))
                                              7)))))

  (should (equal '(7 3 7 5 7)
                 (loopy-iter (listing i '(1 2 3 4 5))
                             (collecting (catch 'my-tag
                                           (when (cl-evenp i)
                                             (throw (progn
                                                      (setting tag 'my-tag)
                                                      tag)
                                                    (progn
                                                      (setting val (1+ i))
                                                      val)))
                                           7))))))

(ert-deftest wrap-function ()
  (should
   (equal '(2 3 4)
          (eval (quote
                 (loopy-iter (for list i '(1 2 3))
                             (accum collect (funcall (function (lambda (x)
                                                                 (1+ x)))
                                                     i)))))))

  (should
   (equal '(2 3 4)
          (eval (quote
                 (loopy-iter (for list i '(1 2 3))
                             (accum collect (funcall (cl-function (lambda (x)
                                                                    (1+ x)))
                                                     i)))))))
  (should
   (equal '(3 7)
          (eval (quote
                 (loopy-iter (listing elem '((1 2) (3 4)))
                             (collecting (funcall (cl-function (lambda ((x y))
                                                                 (+ x y)))
                                                  elem)))))))

  (should
   (equal '(2 3 4)
          (eval (quote
                 (loopy-iter (listing i '(1 2 3))
                             (collecting (funcall (function (lambda (x)
                                                              (1+ x)))
                                                  i))))))))


(ert-deftest just-commands ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (for list i '(1 2 3))
                                          (accum collect i))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (collecting i)))))))


(ert-deftest wrap-in-let ()
  (should
   (equal '(2 4 6)
          (eval (quote
                 (loopy-iter (for list i '(1 2 3))
                             (let ((a i)
                                   ;; A single symbol shouldn't cause an error,
                                   ;; and should be ignored.
                                   b)
                               (accum collect (+ a i))))))))

  (should
   (equal '(2 4 6)
          (eval (quote
                 (loopy-iter (listing i '(1 2 3))
                             (let ((a i)
                                   ;; A single symbol shouldn't cause an error,
                                   ;; and should be ignored.
                                   b)
                               (collecting (+ a i)))))))))


(ert-deftest wrap-if ()
  (should (equal '((2 4) (1 3 5))
                 (eval (quote (loopy-iter (for list i '(1 2 3 4 5))
                                          (if (progn
                                                (for set test (cl-evenp i))
                                                test)
                                              (accum collect evens i)
                                            (accum collect odds i))
                                          (finally-return evens odds))))))

  (should (equal '((2 4) (1 3 5))
                 (eval (quote (loopy-iter (listing i '(1 2 3 4 5))
                                          (if (progn
                                                (setting test (cl-evenp i))
                                                test)
                                              (collecting evens i)
                                            (collecting odds i))
                                          (finally-return evens odds))))))
  (should
   (equal '(2 4)
          (eval (quote
                 (loopy-iter (listing i '(1 2 3 4 5))
                             (collecting (if (progn
                                               (setting test (cl-evenp i))
                                               test)
                                             i))
                             (finally-return
                              (remq nil loopy-result)))))))

  (should
   (equal '(2 4)
          (eval (quote
                 (loopy-iter (for list i '(1 2 3 4 5))
                             (accum collect (if (progn
                                                  (for set test (cl-evenp i))
                                                  test)
                                                i))
                             (finally-return
                              (remq nil loopy-result))))))))


(ert-deftest wrap-ignore-errors ()
  (should (equal '(1 nil 3 nil 5)
                 (loopy-iter (for list i '(1 2 3 4 5))
                             (accum collect (ignore-errors
                                              (if (cl-evenp i)
                                                  (error ""))
                                              i)))))

  (should (equal '(1 nil 3 nil 5)
                 (loopy-iter (listing i '(1 2 3 4 5))
                             (collecting (ignore-errors
                                           (if (cl-evenp i)
                                               (error ""))
                                           i))))))


(ert-deftest dont-swap-let-var-args ()
  "Parse `let'-like forms correctly.
E.g., \"(let ((for list)) ...)\" should not try to operate on the
\"(for list)\", only on the \"list\"."
  (should (= 1 (eval (quote (loopy-iter (for list list '(1 2 3))
                                        (let ((for list))
                                          (exit return list)))))))
  (should (equal '(3 6 9)
                 (eval
                  (quote
                   (loopy-iter (for list elem '(1 2 3))
                               (let ((a (progn
                                          (for set i (* 2 elem))
                                          (+ elem i))))
                                 (accum collect a)))))))

  (should (= 1 (eval (quote (loopy-iter (listing list '(1 2 3))
                                        (let ((listing))
                                          (returning list)))))))

  (should (equal '(3 6 9)
                 (eval
                  (quote
                   (loopy-iter (listing elem '(1 2 3))
                               (let ((a (progn
                                          (setting i (* 2 elem))
                                          (+ elem i))))
                                 (collecting a))))))))


(ert-deftest dont-expand-quoted ()
  (should (equal '((for expr i 2))
                 (eval
                  (quote
                   (loopy-iter (for cycle 1)
                               (let ((j '(for expr i 2)))
                                 (accum collect j)))))))

  (should (equal '((for expr i 2))
                 (eval
                  (quote
                   (loopy-iter (cycling 1)
                               (let ((j '(for expr i 2)))
                                 (collecting j)))))))

  (should (equal '((for expr i 2))
                 (eval
                  (quote
                   (loopy-iter (for cycle 1)
                               (let ((j (quote (for expr i 2))))
                                 (accum collect j)))))))

  (should (equal '((for expr i 2))
                 (eval
                  (quote
                   (loopy-iter (cycling 1)
                               (let ((j (quote (for expr i 2))))
                                 (collecting j))))))))


(ert-deftest wrap-macro ()
  (should (equal '(3 6 9)
                 (eval
                  (quote
                   (loopy-iter (for list elem '(1 2 3))
                               (cl-destructuring-bind (a b c) (progn
                                                                (for set i elem)
                                                                (list i i i))
                                 (accum collect (+ a b c))))))))

  (should (equal '(3 6 9)
                 (eval
                  (quote
                   (loopy-iter (listing elem '(1 2 3))
                               (cl-destructuring-bind (a b c) (progn
                                                                (setting i elem)
                                                                (list i i i))
                                 (collecting (+ a b c)))))))))


(ert-deftest wrap-setq ()
  (should
   (equal '((1 . 2) (2 . 4) (3 . 6))
          (eval (quote (loopy-iter (listing elem '(1 2 3))
                                   (setq a (progn
                                             (setting i elem)
                                             i)
                                         b (progn
                                             (setting j (* 2 elem))
                                             j))
                                   (collecting (cons a b)))))))

  (should (equal '((for expr i 2) (for expr i 2) (for expr i 2))
                 (eval (quote (loopy-iter (listing elem '(1 2 3))
                                          (setq a '(for expr i 2))
                                          (collecting a))))))

  (should
   (equal '((1 . 2) (2 . 4) (3 . 6))
          (eval (quote (loopy-iter (for list elem '(1 2 3))
                                   (setq a (progn
                                             (for set i elem)
                                             i)
                                         b (progn
                                             (for set j (* 2 elem))
                                             j))
                                   (accum collect (cons a b)))))))

  (should (equal '((for expr i 2) (for expr i 2) (for expr i 2))
                 (eval (quote (loopy-iter (for list elem '(1 2 3))
                                          (setq a '(for expr i 2))
                                          (accum collect a)))))))


(ert-deftest wrap-symbol-macrolet ()
  (should
   (equal '(2 3 4)
          (eval (quote
                 (loopy-iter (for list i '(1 2 3))
                             (cl-symbol-macrolet ((mac (for set val (1+ i))))
                               mac
                               (accum collect val)))))))

  (should
   (equal '(2 3 4)
          (eval (quote
                 (loopy-iter (listing i '(1 2 3))
                             (cl-symbol-macrolet ((mac (setting val (1+ i))))
                               mac
                               (collecting val))))))))


(ert-deftest wrap-lambda ()
  "Both quoted an unquoted lambda's should be wrapped."
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (for list elem '(1 2 3))
                                          (let ((a #'(lambda (x)
                                                       (for set j x))))
                                            (accum collect (progn
                                                             (funcall a elem)
                                                             j))))))))
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (for list elem '(1 2 3))
                                          (let ((a (lambda (x)
                                                     (for set j x))))
                                            (accum collect (progn
                                                             (funcall a elem)
                                                             j))))))))

  ;; NOTE: `defun' expands to a use of `lambda'.
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (for list elem '(1 2 3))
                                          (let ((a (defun loopy-defun-test (x)
                                                     (for set j x))))
                                            (accum collect (progn
                                                             (funcall a elem)
                                                             j))))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing elem '(1 2 3))
                                          (let ((a #'(lambda (x)
                                                       (setting j x))))
                                            (collecting (progn
                                                          (funcall a elem)
                                                          j))))))))
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing elem '(1 2 3))
                                          (let ((a (lambda (x)
                                                     (setting j x))))
                                            (collecting (progn
                                                          (funcall a elem)
                                                          j))))))))

  ;; NOTE: `defun' expands to a use of `lambda'.
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing elem '(1 2 3))
                                          (let ((a (defun loopy-defun-test (x)
                                                     (setting j x))))
                                            (collecting (progn
                                                          (funcall a elem)
                                                          j)))))))))


(ert-deftest wrap-while ()
  ;; `dolist' expands to `while'
  (should (equal '(1 7 2 8 3 9)
                 (eval (quote (loopy-iter (listing elem '((1 7) (2 8) (3 9)))
                                          (dolist (i elem)
                                            (collecting i)))))))

  (should (equal '(1 7 2 8 3 9)
                 (eval (quote (loopy-iter (for list elem '((1 7) (2 8) (3 9)))
                                          (dolist (i elem)
                                            (accum collect i))))))))


(ert-deftest wrap-backquote ()
  (should (equal '((1 2) (2 4) (3 6))
                 (eval (quote
                        (loopy-iter (listing elem '(1 2 3))
                                    (let ((a `(,elem ,(progn
                                                        (setting i (* elem 2))
                                                        i))))
                                      (collecting a)))))))

  (should (equal '((1 2) (2 4) (3 6))
                 (eval (quote
                        (loopy-iter (for list elem '(1 2 3))
                                    (let ((a `(,elem ,(progn
                                                        (for set i (* elem 2))
                                                        i))))
                                      (accum collect a))))))))


(ert-deftest leave-in-let ()
  (should
   (equal '(8 9 10 11 12)
          (eval
           (quote
            (loopy-iter (let ((a (progn
                                   ;; NOTE: No restriction on placement of `expr'.
                                   (setting j 8 (1+ j))
                                   (when (> j 12)
                                     ;; Leave loop but don't force return value,
                                     ;; allowing implicit result to be returned.
                                     (leaving))
                                   j)))
                          (collecting a)))))))

  (should
   (equal '(8 9 10 11 12)
          (eval
           (quote
            (loopy-iter (let ((a (progn
                                   ;; NOTE: No restriction on placement of `expr'.
                                   (for set j 8 (1+ j))
                                   (when (> j 12)
                                     ;; Leave loop but don't force return value,
                                     ;; allowing implicit result to be returned.
                                     (exit leave))
                                   j)))
                          (accum collect a))))))))

;;; lax naming
;; TODO: HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;; (ert-deftest deprecate-flag-lax-naming ()
;;   (should-error ))

;; Wrap in accum
(ert-deftest wrap-expressions-in-loop-commands ()
  (should
   (equal '(2 4 6)
          (eval (quote (loopy-iter (for list i '(1 2 3))
                                   (accum collect (progn (for set a (* 2 i))
                                                         a)))))))
  (should
   (equal '(2 4 6)
          (eval (quote (loopy-iter (listing elem '(1 2 3))
                                   (setting i (progn
                                                (setting j (* 2 elem))
                                                j))
                                   (collecting i)))))))




(ert-deftest wrap-pcase-let* ()
  (should
   (equal '(1 2 3 4 5 6)
          (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                   (pcase-let* ((`(,a ,b) i))
                                     (collecting a)
                                     (collecting b)))))))

  (should
   (equal '(1 2 3 4 5 6)
          (eval (quote (loopy-iter (for list i '((1 2) (3 4) (5 6)))
                                   (pcase-let* ((`(,a ,b) i))
                                     (accum collect a)
                                     (accum collect b))))))))

(ert-deftest wrap-pcase ()
  (should
   (equal '((2 4) (1 3))
          (eval
           (quote
            (loopy-iter (listing i '(1 2 3 4))
                        (let ((j i))
                          (pcase j
                            ((pred cl-evenp) ; <- `pcase' needs predicates unquoted.
                             (collecting evens j))
                            ((pred cl-oddp)
                             (collecting odds j))))
                        (finally-return evens odds))))))
  (should
   (equal '((2 4) (1 3))
          (eval
           (quote
            (loopy-iter (for list i '(1 2 3 4))
                        (let ((j i))
                          (pcase j
                            ((pred cl-evenp) ; <- `pcase' needs predicates unquoted.
                             (accum collect evens j))
                            ((pred cl-oddp)
                             (accum collect odds j))))
                        (finally-return evens odds)))))))


(ert-deftest wrap-pcase-let ()
  (should
   (equal '(1 2 3 4 5 6)
          (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                   (pcase-let ((`(,a ,b) i))
                                     (collecting a)
                                     (collecting b)))))))

  (should
   (equal '(1 2 3 4 5 6)
          (eval (quote (loopy-iter (for list i '((1 2) (3 4) (5 6)))
                                   (pcase-let ((`(,a ,b) i))
                                     (accum collect a)
                                     (accum collect b))))))))


(ert-deftest wrap-seq-let ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (seq-let (a b) i
                                            (collecting a)
                                            (collecting b)))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (for list i '((1 2) (3 4) (5 6)))
                                          (seq-let (a b) i
                                            (accum collect a)
                                            (accum collect b))))))))


(ert-deftest wrap-destructuring-bind ()
  (should
   (equal '((1 3 5) (2 4 6))
          (eval (quote (loopy-iter (for list i '((1 2) (3 4) (5 6)))
                                   (cl-destructuring-bind (a b) i
                                     (accum collect firsts a)
                                     (accum collect seconds b))
                                   (finally-return firsts seconds))))))

  (should
   (equal '((1 3 5) (2 4 6))
          (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                   (cl-destructuring-bind (a b) i
                                     (collecting firsts a)
                                     (collecting seconds b))
                                   (finally-return firsts seconds)))))))


(ert-deftest wrap-unwind-protect ()
  (should (= 6
             (eval (quote
                    (loopy-iter (with (important-val 0))
                                (for list i '(1 2 3 4 5))
                                ;; While the clean-up forms in `unwind-protect'
                                ;; are still run when there is an error, we
                                ;; still need to guard against the error so that
                                ;; the loop actually completes.
                                (ignore-errors
                                  (unwind-protect
                                      (progn
                                        ;; Set to 0, then update if no error.
                                        (for set val 0)
                                        (for set val (if (cl-evenp i)
                                                         i
                                                       (error ""))))
                                    (accum sum important-val val)))
                                (finally-return important-val))))))

  (should (= 6
             (eval (quote
                    (loopy-iter (with (important-val 0))
                                (listing i '(1 2 3 4 5))
                                ;; While the clean-up forms in `unwind-protect'
                                ;; are still run when there is an error, we
                                ;; still need to guard against the error so that
                                ;; the loop actually completes.
                                (ignore-errors
                                  (unwind-protect
                                      (progn
                                        ;; Set to 0, then update if no error.
                                        (setting val 0)
                                        (setting val (if (cl-evenp i)
                                                         i
                                                       (error ""))))
                                    (summing important-val val)))
                                (finally-return important-val)))))))


(ert-deftest sub-loop ()
  (should (equal '(2 3 4 5 6)
                 (loopy-iter outer
                             (for list i '(1 2 3 4 5))
                             (for loop
                                  (for repeat 1)
                                  (for set j (1+ i))
                                  (for at outer
                                       (accum collect j))))))

  (should (equal '(2 3 4 5 6)
                 (loopy-iter outer
                             (listing i '(1 2 3 4 5))
                             (looping
                              (repeating 1)
                              (setting j (1+ i))
                              (at outer
                                  (collecting j))))))

  (should (equal '(2 3 4 5 6)
                 (loopy-iter outer
                             (for list i '(1 2 3 4 5))
                             (loopy-iter
                              (for repeat 1)
                              (for set j (1+ i))
                              (for at outer
                                   (accum collect j))))))

  (should (equal '(2 3 4 5 6)
                 (loopy-iter outer
                             (for list i '(1 2 3 4 5))
                             (loopy-iter
                              (repeating 1)
                              (setting j (1+ i))
                              (at outer
                                  (collecting j)))))))


(ert-deftest collect-coercion ()
  (should (equal [1 2 3]
                 (loopy-iter (for list j '(1 2 3))
                             (accum collect v j :result-type 'vector)
                             (finally-return v))))

  (should (equal [1 2 3]
                 (loopy-iter (listing j '(1 2 3))
                             (collecting v j :result-type 'vector)
                             (finally-return v))))

  (should (equal [1 2 3]
                 (loopy-iter (listing j '(1 2 3))
                             (collecting j :result-type 'vector))))

  (should (equal [1 2 3]
                 (loopy-iter (for list j '(1 2 3))
                             (accum collect j :result-type 'vector)))))


(ert-deftest loopy-iter-command ()
  (should (equal '(11 12 13 14 15 16)
                 (loopy outer
                        (list i '((1 2) (3 4) (5 6)))
                        (loopy-iter (for list j i)
                                    (for at outer
                                         (let ((val 10))
                                           (accum collect (+ val j))))))))

  (should (equal '(11 12 13 14 15 16)
                 (loopy outer
                        (list i '((1 2) (3 4) (5 6)))
                        (loopy-iter (listing j i)
                                    (for at outer
                                         (let ((val 10))
                                           (collecting (+ val j)))))))))


(ert-deftest loopy-command-in-iter ()
  (should (equal '(1 2 3 4)
                 (loopy-iter outer
                             (for array i [(1 2) (3 4)])
                             (for loopy
                                  (list j i)
                                  (at outer (collect j))))))

  (should (equal '(1 2 3 4)
                 (loopy-iter outer
                             (arraying i [(1 2) (3 4)])
                             (loopy
                              (list j i)
                              (at outer (collect j)))))))


(ert-deftest loopy-iter-sub-loop-named ()
  (should
   (equal
    '((3 5) (3 5))
    (loopy-iter
     outer
     (for repeat 2)
     (for loop inner1
          (for list j '(3 4))
          (for loop
               (for list k '(5 6 7))
               (if (= k 6)
                   ;; Return from inner1 so never reach 4.
                   (exit return-from inner1)
                 (for at outer (accum collect (list j k))))))))))


(ert-deftest loopy-iter-ignore-let* ()
  (should
   (equal '(("git" "show" "diff" "log"))
          (eval (quote
                 (let ((test-eshell-visual-subcommands))
                   (loopy-iter
                    (listing (cmd &rest subcmds) '(("git" "log" "diff" "show")))
                    (looping
                     (listing subcmd subcmds)
                     (push subcmd (alist-get cmd test-eshell-visual-subcommands
                                             nil nil #'equal))))
                   test-eshell-visual-subcommands)))))

  (should
   (equal '(("git" "show" "diff" "log"))
          (eval (quote
                 (let ((test-eshell-visual-subcommands))
                   (loopy-iter
                    (for list (cmd &rest subcmds) '(("git" "log" "diff" "show")))
                    (for loop
                         (for list subcmd subcmds)
                         (push subcmd (alist-get cmd test-eshell-visual-subcommands
                                                 nil nil #'equal))))
                   test-eshell-visual-subcommands)))))

  (should (equal '(13 12 11)
                 (let ((target))
                   (loopy-iter (listing i '(1 2 3))
                               (let* ((j (+ i 10)))
                                 (push j target)))
                   target)))

  (should (equal '(13 12 11)
                 (let ((target))
                   (loopy-iter (for list i '(1 2 3))
                               (let* ((j (+ i 10)))
                                 (push j target)))
                   target))))



(ert-deftest loopy-iter-clean-stack-variables ()
  (let ((loopy--known-loop-names)
        (loopy--accumulation-places)
        (loopy--at-instructions)
        (loopy--accumulation-list-end-vars)
        (loopy--accumulation-variable-info))
    (should (equal '((3 4) (1 2) 1 2 3 4)
                   (eval (quote (loopy-iter my-loop
                                            (for array i [(1 2) (3 4)])
                                            (accum collect i :at start)
                                            (for loop inner
                                                 (for list j i)
                                                 (for at my-loop
                                                      (accum collect j :at
                                                             end))))))))
    (should-not (or loopy--known-loop-names
                    loopy--accumulation-places
                    loopy--at-instructions
                    loopy--accumulation-list-end-vars
                    loopy--accumulation-variable-info)))

  (let ((loopy--known-loop-names)
        (loopy--accumulation-places)
        (loopy--at-instructions)
        (loopy--accumulation-list-end-vars)
        (loopy--accumulation-variable-info))
    (should (equal '((3 4) (1 2) 1 2 3 4)
                   (eval (quote (loopy-iter
                                 my-loop
                                 (arraying i [(1 2) (3 4)])
                                 (collecting i :at start)
                                 (looping inner
                                          (listing j i)
                                          (at my-loop
                                              (collecting j :at end))))))))
    (should-not (or loopy--known-loop-names
                    loopy--accumulation-places
                    loopy--at-instructions
                    loopy--accumulation-list-end-vars
                    loopy--accumulation-variable-info)))

  (let ((loopy--known-loop-names)
        (loopy--accumulation-places)
        (loopy--at-instructions)
        (loopy--accumulation-list-end-vars)
        (loopy--accumulation-variable-info))
    (macroexpand '(loopy-iter
                   main
                   (flag lax-naming)
                   (with (a 1) (b 2) (c 3))
                   (while clause)
                   (loopy-let* ((key (pop clause))
                                ((key-fn take-fn transform-fn) (--first (funcall (car it) key)
                                                                        oo-bind-processers))
                                (taken (loop (while (and clause
                                                         (not (keywordp it))
                                                         (funcall take-fn it)))
                                             (collecting (pop clause)))))
                     (collecting (funcall transform-fn (list (cons key taken) rest))
                                 :at start))))
    (should-not (or loopy--known-loop-names
                    loopy--accumulation-places
                    loopy--at-instructions
                    loopy--accumulation-list-end-vars
                    loopy--accumulation-variable-info)))

  (let ((loopy--known-loop-names)
        (loopy--accumulation-places)
        (loopy--at-instructions)
        (loopy--accumulation-list-end-vars)
        (loopy--accumulation-variable-info))
    (macroexpand '(loopy-iter
                   main
                   (flag lax-naming)
                   (with (a 1) (b 2) (c 3))
                   (while clause)
                   (loopy-let* ((key (pop clause))
                                ((key-fn take-fn transform-fn) (--first (funcall (car it) key)
                                                                        oo-bind-processers))
                                (taken (loop (while (and clause
                                                         (not (keywordp it))
                                                         (funcall take-fn it)))
                                             (collecting (pop clause)))))
                     (for collect (funcall transform-fn (list (cons key taken) rest))
                          :at start))))
    (should-not (or loopy--known-loop-names
                    loopy--accumulation-places
                    loopy--at-instructions
                    loopy--accumulation-list-end-vars
                    loopy--accumulation-variable-info))))


;;; Macro arguments
;;;; Keywords for special macro arguments.
(ert-deftest arg-keyword ()
  (should (loopy-iter (arg with (a 3) ((b c) '(4 5)))
                      (repeating 3)
                      (collecting (list a b c))))

  (should (loopy-iter (accum let* (a 3) ((b c) '(4 5)))
                      (repeating 3)
                      (let* ((d 6))
                        (collecting (list a b c d)))))

  (should (loopy-iter (arg let* (a 3) ((b c) '(4 5)))
                      (repeating 3)
                      (collecting (list a b c)))))
;;;; With
(ert-deftest with-arg-order ()
  (should (= 4
             (eval (quote (loopy-iter (with (a 2)
                                            (b (+ a 2)))
                                      (returning b))))))

  (should (= 4
             (eval (quote (loopy-iter (init (a 2)
                                            (b (+ a 2)))
                                      (returning b)))))))

(ert-deftest with-destructuring ()
  (should (= -2
             (eval (quote (loopy-iter (with ((a b) '(1 2))
                                            ([c d] `[,(1+ a) ,(1+ b)]))
                                      (returning (+ (- a b)
                                                    (- c d)))))))))

;;;; Without
(ert-deftest without ()
  (should (equal '(4 5)
                 (eval (quote (let ((a 1) (b 2))
                                (loopy-iter (with (c 3))
                                            (without a b)
                                            (setting a (+ a c))
                                            (setting b (+ b c))
                                            (returning a b)))))))

  (should (equal '(4 5)
                 (eval (quote (let ((a 1) (b 2))
                                (loopy-iter (with (c 3))
                                            (no-init a b)
                                            (setting a (+ a c))
                                            (setting b (+ b c))
                                            (returning a b)))))))

  (should (equal '(4 5)
                 (eval (quote (let ((a 1) (b 2))
                                (loopy-iter (with (c 3))
                                            (no-with a b)
                                            (setting a (+ a c))
                                            (setting b (+ b c))
                                            (returning a b))))))))

;;;; Before Do
;; `before-do' always runs, and occurs before the loop.
(ert-deftest basic-before-do ()
  (should (and (= 4
                  (eval (quote (loopy-iter (with (i 3))
                                           (before-do (setq i (1+ i)))
                                           (returning i)))))
               (= 4
                  (eval (quote (loopy-iter (with (i 3))
                                           (before (setq i (1+ i)))
                                           (returning i)))))
               (= 4
                  (eval (quote (loopy-iter (with (i 3))
                                           (initially-do (setq i (1+ i)))
                                           (returning i)))))
               (= 4
                  (eval (quote (loopy-iter (with (i 3))
                                           (initially (setq i (1+ i)))
                                           (returning i))))))))

;;;; After Do - runs after loop is loop completed successfully
(ert-deftest basic-after-do ()
  (should (eq t (eval (quote (loopy-iter (with (my-ret nil))
                                         (listing i '(1 2 3 4))
                                         (after-do (setq my-ret t))
                                         (finally-return my-ret))))))

  (should (eq nil (eval (quote (loopy-iter (with (my-ret nil))
                                           (listing i '(1 2 3 4))
                                           (returning nil)
                                           (after-do (setq my-ret t))
                                           (finally-return my-ret))))))

  (should (eq nil (eval (quote (loopy-iter (with (my-ret nil))
                                           (listing i '(1 2 3 4))
                                           (returning nil)
                                           (after (setq my-ret t))
                                           (finally-return my-ret))))))

  (should (eq nil (eval (quote (loopy-iter (with (my-ret nil))
                                           (listing i '(1 2 3 4))
                                           (returning nil)
                                           (else-do (setq my-ret t))
                                           (finally-return my-ret))))))

  (should (eq nil (eval (quote (loopy-iter (with (my-ret nil))
                                           (listing i '(1 2 3 4))
                                           (returning nil)
                                           (else (setq my-ret t))
                                           (finally-return my-ret)))))))

;;;; Before and After
(ert-deftest basic-before-and-after-test ()
  (should (= 3 (eval (quote (loopy-iter (with (i 1))
                                        (before-do (cl-incf i))
                                        (repeating 1)
                                        (after-do (cl-incf i))
                                        (finally-return i)))))))

;;;; Wrap

(ert-deftest wrap ()
  ;; Test saving match data
  (should
   (save-match-data
     (let ((original-data (set-match-data nil)))
       (equal original-data
              (eval (quote (loopy-iter (wrap save-match-data)
                                       (cycling 1)
                                       (string-match (make-string 100 ?a)
                                                     (make-string 100 ?a))
                                       (finally-return (match-data)))))))))

  ;; Test order things wrapped in.
  (should (= 3 (eval (quote (loopy-iter (wrap (let ((a 1)))
                                              (let ((b (1+ a)))))
                                        (returning (+ a b)))))))

  ;; Ensure wrapping effects don't linger.
  (should-not
   (save-match-data
     (let ((original-data (set-match-data nil)))
       (equal original-data
              (eval (quote (loopy-iter (cycling 1)
                                       (string-match (make-string 100 ?a)
                                                     (make-string 100 ?a))
                                       (finally-return (match-data))))))))))

;;;; Final Instructions
(ert-deftest finally-do ()
  (should (= 10 (let ((my-var))
                  (loopy-iter (listing i (number-sequence 1 10))
                              (finally-do (setq my-var i)))
                  my-var)))
  (should (= 10 (let ((my-var))
                  (loopy-iter (listing i (number-sequence 1 10))
                              (finally (setq my-var i)))
                  my-var))))

(ert-deftest finally-do-not-affect-return ()
  (should (eq nil (eval (quote (loopy-iter (listing i (number-sequence 1 10))
                                           (finally-do 3)))))))

(ert-deftest finally-return-single-value ()
  (should (= 10 (eval (quote (loopy-iter (listing i (number-sequence 1 10))
                                         (finally-return i)))))))

(ert-deftest finally-return-list-of-values ()
  (should (equal '(10 7)
                 (eval (quote (loopy-iter (listing i (number-sequence 1 10))
                                          (finally-return i 7)))))))

;;;; Finally Protect
(ert-deftest finally-protect ()
  (should (equal (list 1 4 '(1 2 3 4))
                 (let ((test-result))
                   (should-error
                    (loopy-iter (with (example-var 1))
                                (listing i '(1 2 3 4 5))
                                (collecting my-collection i)
                                (when (> i 3)
                                  (error "%s" (list i)))
                                (finally-protect
                                 (setq test-result (list example-var i my-collection))))
                    :type '(error))
                   test-result)))

  (should (equal (list 1 4 '(1 2 3 4))
                 (let ((test-result))
                   (should-error
                    (loopy-iter (with (example-var 1))
                                (listing i '(1 2 3 4 5))
                                (collecting my-collection i)
                                (when (> i 3)
                                  (error "%s" (list i)))
                                (finally-protected
                                 (setq test-result (list example-var i my-collection))))
                    :type '(error))
                   test-result))))

;;;; Changing the order of macro arguments.
(ert-deftest change-order-of-commands ()
  (should (= 7
             (eval (quote (loopy-iter (listing i '(1 2 3))
                                      (finally-return (+ i a))
                                      (with (a 4))))))))

;;;; Default return values.
(ert-deftest default-return-nil ()
  (should (not (or (eval (quote (loopy-iter (listing i '(1 2 3)))))
                   (eval (quote (loopy-iter (repeating 1)
                                            (finally-do (1+ 1)))))))))

;;;; Optimized Named  Accumulations
(ert-deftest optimized-named-vars ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (arraying i [1 2 3])
                                          (adjoining coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 2 3 3 4)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (arraying i [(1 2) (2 3) (3 4)])
                                          (appending coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal "abcdef"
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (arraying i ["ab" "cd" "ef"])
                                          (concating coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nconcing coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nunioning coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (unioning coll i)
                                          (finally-return coll))))))

  (should (equal [1 2 3 4 5 6]
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (arraying i [(1 2) (3 4) (5  6)])
                                          (vconcating coll i)
                                          (finally-return coll)))))))

(ert-deftest optimized-named-vars-with-pos-end ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (arraying i [1 2 3])
                                          (adjoining coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 2 3 3 4)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (arraying i [(1 2) (2 3) (3 4)])
                                          (appending coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal "abcdef"
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (arraying i ["ab" "cd" "ef"])
                                          (concating coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nconcing coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nunioning coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (unioning coll i)
                                          (finally-return coll))))))

  (should (equal [1 2 3 4 5 6]
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (arraying i [(1 2) (3 4) (5  6)])
                                          (vconcating coll i)
                                          (finally-return coll)))))))

(ert-deftest optimized-named-vars-with-pos-beginning ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (arraying i [1 2 3])
                                          (adjoining coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 2 3 3 4)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (arraying i [(1 2) (2 3) (3 4)])
                                          (appending coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal "abcdef"
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (arraying i ["ab" "cd" "ef"])
                                          (concating coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nconcing coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nunioning coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (unioning coll i)
                                          (finally-return coll))))))

  (should (equal [1 2 3 4 5 6]
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (arraying i [(1 2) (3 4) (5  6)])
                                          (vconcating coll i)
                                          (finally-return coll)))))))

(ert-deftest optimized-named-vars-with-pos-start ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (arraying i [1 2 3])
                                          (adjoining coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 2 3 3 4)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (arraying i [(1 2) (2 3) (3 4)])
                                          (appending coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal "abcdef"
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (arraying i ["ab" "cd" "ef"])
                                          (concating coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nconcing coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nunioning coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (unioning coll i)
                                          (finally-return coll))))))

  (should (equal [1 2 3 4 5 6]
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (arraying i [(1 2) (3 4) (5  6)])
                                          (vconcating coll i)
                                          (finally-return coll)))))))

;;; Tests from `loopy'.
;;; Macro arguments
;;;; With
(ert-deftest with-arg-order ()
  (should (= 4
             (eval (quote (loopy-iter (with (a 2)
                                            (b (+ a 2)))
                                      (returning b))))))

  ;; (should (= 4
  ;;            (eval (quote (loopy-iter (let* (a 2)
  ;;                                       (b (+ a 2)))
  ;;                                     (returning b))))))

  (should (= 4
             (eval (quote (loopy-iter (init (a 2)
                                            (b (+ a 2)))
                                      (returning b)))))))

(ert-deftest with-destructuring ()
  (should (= -2
             (eval (quote (loopy-iter (with ((a b) '(1 2))
                                            ([c d] `[,(1+ a) ,(1+ b)]))
                                      (returning (+ (- a b)
                                                    (- c d)))))))))

;;;; Without
(ert-deftest without ()
  (should (equal '(4 5)
                 (eval (quote (let ((a 1) (b 2))
                                (loopy-iter (with (c 3))
                                            (without a b)
                                            (setting a (+ a c))
                                            (setting b (+ b c))
                                            (returning a b)))))))

  (should (equal '(4 5)
                 (eval (quote (let ((a 1) (b 2))
                                (loopy-iter (with (c 3))
                                            (no-init a b)
                                            (setting a (+ a c))
                                            (setting b (+ b c))
                                            (returning a b)))))))

  (should (equal '(4 5)
                 (eval (quote (let ((a 1) (b 2))
                                (loopy-iter (with (c 3))
                                            (no-with a b)
                                            (setting a (+ a c))
                                            (setting b (+ b c))
                                            (returning a b))))))))

;;;; Before Do
;; `before-do' always runs, and occurs before the loop.
(ert-deftest basic-before-do ()
  (should (and (= 4
                  (eval (quote (loopy-iter (with (i 3))
                                           (before-do (setq i (1+ i)))
                                           (returning i)))))
               (= 4
                  (eval (quote (loopy-iter (with (i 3))
                                           (before (setq i (1+ i)))
                                           (returning i)))))
               (= 4
                  (eval (quote (loopy-iter (with (i 3))
                                           (initially-do (setq i (1+ i)))
                                           (returning i)))))
               (= 4
                  (eval (quote (loopy-iter (with (i 3))
                                           (initially (setq i (1+ i)))
                                           (returning i))))))))

;;;; After Do - runs after loop is loop completed successfully
(ert-deftest basic-after-do ()
  (should (and (eq t (eval (quote (loopy-iter (with (my-ret nil))
                                              (listing i '(1 2 3 4))
                                              (after-do (setq my-ret t))
                                              (finally-return my-ret)))))
               (eq nil (eval (quote (loopy-iter (with (my-ret nil))
                                                (listing i '(1 2 3 4))
                                                (returning nil)
                                                (after-do (setq my-ret t))
                                                (finally-return my-ret)))))
               (eq nil (eval (quote (loopy-iter (with (my-ret nil))
                                                (listing i '(1 2 3 4))
                                                (returning nil)
                                                (after (setq my-ret t))
                                                (finally-return my-ret)))))
               (eq nil (eval (quote (loopy-iter (with (my-ret nil))
                                                (listing i '(1 2 3 4))
                                                (returning nil)
                                                (else-do (setq my-ret t))
                                                (finally-return my-ret)))))
               (eq nil (eval (quote (loopy-iter (with (my-ret nil))
                                                (listing i '(1 2 3 4))
                                                (returning nil)
                                                (else (setq my-ret t))
                                                (finally-return my-ret))))))))

;;;; Before and After
(ert-deftest basic-before-and-after-test ()
  (should (= 3 (eval (quote (loopy-iter (with (i 1))
                                        (before-do (cl-incf i))
                                        (cycling 1)
                                        (after-do (cl-incf i))
                                        (finally-return i)))))))

;;;; Wrap

(ert-deftest wrap ()
  ;; Test saving match data
  (should
   (save-match-data
     (let ((original-data (set-match-data nil)))
       (equal original-data
              (eval (quote (loopy-iter (wrap save-match-data)
                                       (cycling 1)
                                       (string-match (make-string 100 ?a)
                                                     (make-string 100 ?a))
                                       (finally-return (match-data)))))))))

  ;; Test order things wrapped in.
  (should (= 3 (eval (quote (loopy-iter (wrap (let ((a 1)))
                                              (let ((b (1+ a)))))
                                        (returning (+ a b)))))))

  ;; Ensure wrapping effects don't linger.
  (should-not
   (save-match-data
     (let ((original-data (set-match-data nil)))
       (equal original-data
              (eval (quote (loopy-iter (cycling 1)
                                       (string-match (make-string 100 ?a)
                                                     (make-string 100 ?a))
                                       (finally-return (match-data))))))))))

;;;; Final Instructions
(ert-deftest finally-do ()
  (should (and (= 10
                  (let ((my-var))
                    (loopy-iter (listing i (number-sequence 1 10))
                                (finally-do (setq my-var i)))
                    my-var))
               (= 10
                  (let ((my-var))
                    (loopy-iter (listing i (number-sequence 1 10))
                                (finally (setq my-var i)))
                    my-var)))))

(ert-deftest finally-do-not-affect-return ()
  (should (eq nil
              (eval (quote (loopy-iter (listing i (number-sequence 1 10))
                                       (finally-do 3)))))))

(ert-deftest finally-return-single-value ()
  (should (= 10
             (eval (quote (loopy-iter (listing i (number-sequence 1 10))
                                      (finally-return i)))))))

(ert-deftest finally-return-list-of-values ()
  (should (equal '(10 7)
                 (eval (quote (loopy-iter (listing i (number-sequence 1 10))
                                          (finally-return i 7)))))))

;;;; Finally Protect
(ert-deftest finally-protect ()
  (should (equal (list 1 4 '(1 2 3 4))
                 (let ((test-result))
                   (should-error
                    (loopy-iter (with (example-var 1))
                                (listing i '(1 2 3 4 5))
                                (collecting my-collection i)
                                (when (> i 3)
                                  (error "%s" (list i)))
                                (finally-protect
                                 (setq test-result (list example-var i my-collection))))
                    :type '(error))
                   test-result)))

  (should (equal (list 1 4 '(1 2 3 4))
                 (let ((test-result))
                   (should-error
                    (loopy-iter (with (example-var 1))
                                (listing i '(1 2 3 4 5))
                                (collecting my-collection i)
                                (when (> i 3)
                                  (error "%s" (list i)))
                                (finally-protected
                                 (setq test-result (list example-var i my-collection))))
                    :type '(error))
                   test-result))))

;;;; Changing the order of macro arguments.
(ert-deftest change-order-of-commands ()
  (should (= 7
             (eval (quote (loopy-iter (listing i '(1 2 3))
                                      (finally-return (+ i a))
                                      (with (a 4))))))))

;;;; Default return values.
(ert-deftest default-return-nil ()
  (should (not (or (eval (quote (loopy-iter (listing i '(1 2 3)))))
                   (eval (quote (loopy-iter (cycling 1)
                                            (finally-do (1+ 1)))))))))

;;;; Optimized Named  Accumulations
(ert-deftest optimized-named-vars ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (arraying i [1 2 3])
                                          (adjoining coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 2 3 3 4)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (arraying i [(1 2) (2 3) (3 4)])
                                          (appending coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal "abcdef"
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (arraying i ["ab" "cd" "ef"])
                                          (concating coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nconcing coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nunioning coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (unioning coll i)
                                          (finally-return coll))))))

  (should (equal [1 2 3 4 5 6]
                 (eval (quote (loopy-iter (accum-opt coll)
                                          (arraying i [(1 2) (3 4) (5  6)])
                                          (vconcating coll i)
                                          (finally-return coll)))))))

(ert-deftest optimized-named-vars-with-pos-end ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (arraying i [1 2 3])
                                          (adjoining coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 2 3 3 4)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (arraying i [(1 2) (2 3) (3 4)])
                                          (appending coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal "abcdef"
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (arraying i ["ab" "cd" "ef"])
                                          (concating coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nconcing coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nunioning coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (unioning coll i)
                                          (finally-return coll))))))

  (should (equal [1 2 3 4 5 6]
                 (eval (quote (loopy-iter (accum-opt (coll end))
                                          (arraying i [(1 2) (3 4) (5  6)])
                                          (vconcating coll i)
                                          (finally-return coll)))))))

(ert-deftest optimized-named-vars-with-pos-beginning ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (arraying i [1 2 3])
                                          (adjoining coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 2 3 3 4)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (arraying i [(1 2) (2 3) (3 4)])
                                          (appending coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal "abcdef"
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (arraying i ["ab" "cd" "ef"])
                                          (concating coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nconcing coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nunioning coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (unioning coll i)
                                          (finally-return coll))))))

  (should (equal [1 2 3 4 5 6]
                 (eval (quote (loopy-iter (accum-opt (coll beginning))
                                          (arraying i [(1 2) (3 4) (5  6)])
                                          (vconcating coll i)
                                          (finally-return coll)))))))

(ert-deftest optimized-named-vars-with-pos-start ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (arraying i [1 2 3])
                                          (adjoining coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 2 3 3 4)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (arraying i [(1 2) (2 3) (3 4)])
                                          (appending coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal "abcdef"
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (arraying i ["ab" "cd" "ef"])
                                          (concating coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nconcing coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (nunioning coll i)
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (listing i (list (list 1 2)
                                                           (list 3 4)
                                                           (list 5 6)))
                                          (unioning coll i)
                                          (finally-return coll))))))

  (should (equal [1 2 3 4 5 6]
                 (eval (quote (loopy-iter (accum-opt (coll start))
                                          (arraying i [(1 2) (3 4) (5  6)])
                                          (vconcating coll i)
                                          (finally-return coll)))))))



;;; Loop Commands
;;;; Miscellaneous
;;;;; At and sub-loop
;; TODO: Test error checking for accumulations of different types in separate
;; loops.

(ert-deftest at-accum ()
  (should (equal '(1 2 3 4 5 6)
                 (loopy-iter outer
                             (listing i '((1 2) (3 4) (5 6)))
                             (sub-looping (listing j i)
                                          (at outer
                                              (collecting j)))))))

(ert-deftest at-leave ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter outer
                                          (flags split)
                                          (listing i '((1 2) (3 4) (5 6)))
                                          (sub-looping (listing j i)
                                                       (at outer
                                                           (if (> j 3)
                                                               (leaving)
                                                             (collecting j))))))))))

(ert-deftest at-disagreeing-accum-types ()
  (should-error (loopy-iter outer
                            (listing i '([1 2] [3]))
                            (collecting i)
                            (looping (arraying j i)
                                     (at outer (maximizing j)))))

  (should-error (loopy-iter outer
                            (listing i '([1 2] [3]))
                            (collecting i)
                            (at outer (maximizing j)))))

(ert-deftest sub-loop-implicit-accum-in-loop ()
  (should (equal '((1 . 4) (1 . 5) (2 . 4) (2 . 5))
                 (eval (quote (loopy-iter outer
                                          (listing i '(1 2))
                                          (looping (listing j '(4 5))
                                                   (at outer (collecting (cons i j)))))))))

  (should (equal "14152425"
                 (eval (quote (loopy-iter outer
                                          (listing i '("1" "2"))
                                          (looping (listing j '("4" "5"))
                                                   (at outer (concating (concat i j)))))))))

  (should (equal '(0 (1 . 4) (1 . 5) (2 . 4) (2 . 5))
                 (eval (quote (loopy-iter outer
                                          (listing i '(1 2))
                                          (looping (listing j '(4 5))
                                                   (at outer (collecting (cons i j))))
                                          (finally-return (cons 0 loopy-result))))))))

(ert-deftest sub-loop-explicit-accum-in-loop ()
  (should (equal '(0 (1 . 4) (1 . 5) (2 . 4) (2 . 5))
                 (eval
                  (quote
                   (loopy-iter outer
                               (listing i '(1 2))
                               (looping (listing j '(4 5))
                                        (at outer (collecting my-coll (cons i j))))
                               (finally-return (cons 0 my-coll)))))))

  (should (equal "014152425"
                 (eval
                  (quote
                   (loopy-iter outer
                               (listing i '("1" "2"))
                               (looping (listing j '("4" "5"))
                                        (at outer (concating my-str (concat i j))))
                               (finally-return (concat "0" my-str))))))))
;;
(ert-deftest sub-loop-leave-early ()
  "A `leave' in a sub-loop should not affect the outer loop."
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter outer
                                          (listing i '(1 2 3))
                                          (looping (listing j '(4 5 6))
                                                   (leaving)
                                                   (at outer (collecting j)))
                                          (collecting i)))))))

(ert-deftest sub-loop-skip ()
  "A `skip' in a sub-loop should not affect the outer loop."
  (should (equal '(5 7 1 5 7 2 5 7 3)
                 (eval (quote (loopy-iter  outer
                                           (listing i '(1 2 3))
                                           (looping (listing j '(4 5 6 7 8))
                                                    (when (cl-evenp j)
                                                      (continuing))
                                                    (at outer (collecting j)))
                                           (collecting i)))))))

(ert-deftest sub-loop-return-from-outer ()
  (should (= 3 (eval (quote (loopy-iter outer
                                        (listing i '(1 2 3))
                                        (looping (listing j '(4 5 6 3))
                                                 (when (= j i)
                                                   (returning-from outer j)))))))))

(ert-deftest sub-loop-named ()
  (should
   (equal
    '((3 5) (3 5))
    (eval (quote (loopy-iter outer
                             (cycling 2)
                             (looping inner1
                                      (listing j '(3 4))
                                      (looping (listing k '(5 6 7))
                                               (if (= k 6)
                                                   ;; Return from inner1 so never reach 4.
                                                   (returning-from inner1)
                                                 (at outer (collecting (list j k))))))))))))

;;;; Generic Evaluation

;;;;; Expr
(ert-deftest expr-init ()
  (should (= 1 (eval (quote (loopy-iter (cycling 3)
                                        (setting var 1 :init 'cat)
                                        (finally-return var))))))

  (should (= 1 (eval (quote (loopy-iter (cycling 3)
                                        (setting var 1 :init nil)
                                        (finally-return var))))))

  (should (= 3 (eval (quote (loopy-iter (cycling 3)
                                        (setting var (1+ var) :init 0)
                                        (finally-return var)))))))

(ert-deftest expr-one-value ()
  (should
   (and (eval (quote (loopy-iter (with (my-val nil))
                                 (setting my-val t)
                                 (returning nil)
                                 (finally-return my-val))))
        (equal '(t t) (eval (quote (loopy-iter (setting (i j) '(t t))
                                               (returning nil) ; TODO: Change to leave.
                                               (finally-return i j)))))

        (equal '(0 1 1 1)
               (eval (quote (loopy-iter (cycling 4)
                                        (collecting i)
                                        (setting i 1 :init 0)))))

        (equal '(0 1 1 1)
               (eval (quote (loopy-iter (cycling 4)
                                        (collecting i)
                                        (setting i 1 :init 0))))))))

(ert-deftest expr-two-values ()
  (should
   (and
    (equal '(1 2 2)
           (eval (quote (loopy-iter (cycling 3)
                                    (setting my-val 1 2)
                                    (collecting my-coll my-val)
                                    (finally-return my-coll)))))
    (equal '((1 1) (2 2) (2 2))
           (eval (quote (loopy-iter (cycling 3)
                                    (setting (i j) '(1 1) '(2 2))
                                    (collecting my-coll (list i j))
                                    (finally-return my-coll)))))

    (equal '(0 1 2 2)
           (eval (quote (loopy-iter (cycling 4)
                                    (collecting i)
                                    (setting i 1 2 :init 0))))))))

(ert-deftest expr-two-values-when ()
  (should (equal '(nil 0 0 1 1 2 2 3)
                 (loopy-iter (listing i '(1 2 3 4 5 6 7 8))
                             (when (cl-evenp i)
                               (setting j 0 (1+ j)))
                             (collecting j)))))

(ert-deftest expr-three-values-when ()
  (should (equal '(nil a a 0 0 1 1 2)
                 (loopy-iter (listing i '(1 2 3 4 5 6 7 8))
                             (when (cl-evenp i)
                               (setting j 'a 0 (1+ j)))
                             (collecting j)))))

;; Implementation is different for more than 2 values.
(ert-deftest expr-five-values ()
  (should
   (and (equal '(1 2 3 4 5 5 5 5 5 5)
               (eval (quote (loopy-iter (cycling 10)
                                        (setting my-val 1 2 3 4 5)
                                        (collecting my-coll my-val)
                                        (finally-return my-coll)))))

        (equal '((1 1) (2 2) (3 3) (4 4) (5 5) (5 5) (5 5) (5 5) (5 5) (5 5))
               (eval (quote (loopy-iter (cycling 10)
                                        (setting (i j) '(1 1) '(2 2)
                                                 '(3 3) '(4 4) '(5 5))
                                        (collecting my-coll (list i j))
                                        (finally-return my-coll)))))

        (equal '(0 1 2 3 4 5 5 5 5 5)
               (eval (quote (loopy-iter (cycling 10)
                                        (collecting i)
                                        (setting i 1 2 3 4 5 :init 0))))))))

(ert-deftest expr-dont-repeat ()
  "Make sure commands don't repeatedly create/declare the same variable."
  (should
   (= 1 (with-temp-buffer
          (prin1 (macroexpand '(loopy-iter (setting j 3)
                                           (setting j 4)
                                           (returning j)))
                 (current-buffer))
          (goto-char (point-min))
          (how-many "(j nil)")))))

;;;;; Prev-Expr
(ert-deftest prev-expr ()
  (should (equal '(nil 1 2 3 4)
                 (eval (quote (loopy-iter (listing i '(1 2 3 4 5))
                                          (setting-prev j i)
                                          (collecting j))))))

  (should (equal '(nil nil nil 1 2)
                 (eval (quote (loopy-iter (listing i '(1 2 3 4 5))
                                          (setting-prev j i :back 3)
                                          (collecting j))))))

  (should (equal '(first-val first-val 2 2 4 4 6 6 8 8)
                 (eval (quote (loopy-iter (numbering i 1 10)
                                          (when (cl-oddp i)
                                            (setting-prev j i :init 'first-val))
                                          (collecting j)))))))

(ert-deftest prev-expr-destructuring ()
  (should (equal '((7 7 1 3) (7 7 2 4))
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6) (7 8)))
                                          (setting-prev (a b) i :back 2 :init 7)
                                          (collecting c1 a)
                                          (collecting c2 b)
                                          (finally-return c1 c2)))))))

;;;; Iteration
;; Making sure iteration fails in sub-level
(ert-deftest iteration-sub-level ()
  (should-error
   (progn
     (loopy-iter (if t (listing i '(1))) (finally-return t))
     (loopy-iter (if t (listing-ref i '(1))) (finally-return t))
     (loopy-iter (if t (arraying i '(1))) (finally-return t))
     (loopy-iter (if t (arraying-ref i '(1))) (finally-return t))
     (loopy-iter (if t (sequencing i '(1))) (finally-return t))
     (loopy-iter (if t (sequencing-ref i '(1))) (finally-return t))
     (loopy-iter (if t (cycling 1)) (finally-return t))
     (loopy-iter (when t (listing i '(1))) (finally-return t))
     (loopy-iter (when t (listing-ref i '(1))) (finally-return t))
     (loopy-iter (when t (arraying i '(1))) (finally-return t))
     (loopy-iter (when t (arraying-ref i '(1))) (finally-return t))
     (loopy-iter (when t (sequencing i '(1))) (finally-return t))
     (loopy-iter (when t (sequencing-ref i '(1))) (finally-return t))
     (loopy-iter (when t (cycling 1)) (finally-return t))
     (loopy-iter (unless t (listing i '(1))) (finally-return t))
     (loopy-iter (unless t (listing-ref i '(1))) (finally-return t))
     (loopy-iter (unless t (arraying i '(1))) (finally-return t))
     (loopy-iter (unless t (arraying-ref i '(1))) (finally-return t))
     (loopy-iter (unless t (sequencing i '(1))) (finally-return t))
     (loopy-iter (unless t (sequencing-ref i '(1))) (finally-return t))
     (loopy-iter (unless t (cycling 1)) (finally-return t))
     (loopy-iter (cond (t (listing i '(1)))) (finally-return t))
     (loopy-iter (cond (t (listing-ref i '(1)))) (finally-return t))
     (loopy-iter (cond (t (arraying i '(1)))) (finally-return t))
     (loopy-iter (cond (t (arraying-ref i '(1)))) (finally-return t))
     (loopy-iter (cond (t (sequencing i '(1)))) (finally-return t))
     (loopy-iter (cond (t (sequencing-ref i '(1)))) (finally-return t))
     (loopy-iter (cond (t (cycling 1))) (finally-return t))
     (loopy-iter (progn (listing i '(1))) (finally-return t))
     (loopy-iter (progn (listing-ref i '(1))) (finally-return t))
     (loopy-iter (progn (arraying i '(1))) (finally-return t))
     (loopy-iter (progn (arraying-ref i '(1))) (finally-return t))
     (loopy-iter (progn (sequencing i '(1))) (finally-return t))
     (loopy-iter (progn (sequencing-ref i '(1))) (finally-return t))
     (loopy-iter (progn (cycling 1))) (finally-return t))
   :type 'user-error))

;;;;; Array
(ert-deftest array ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (arraying i [1 2 3])
                                          (collecting coll i)
                                          (finally-return coll))))))

  (should (equal '(97 98 99)
                 (eval (quote (loopy-iter (string i "abc")
                                          (collecting coll i)
                                          (finally-return coll)))))))

(ert-deftest array-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy-iter (arraying (a . b)
                                                         [(1 . 2) (3 . 4) (5 . 6)])
                                               (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy-iter (arraying (a . b)
                                                         [(1 2) (3 4) (5 6)])
                                               (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy-iter (arraying (a b c d)
                                                         [(1 2 3 4) (4 5 6 7)])
                                               (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy-iter (arraying [i j k] [[1 2 3] [4 5 6]])
                                               (finally-return i j k))))))))


(ert-deftest array-recursive-destructuring ()
  (should
   (and
    (equal '(5 5 6)
           (eval (quote (loopy-iter (arraying (a [b c]) [(1 [1 2]) (5 [5 6])])
                                    (finally-return (list a b c))))))
    (equal '(4 5 6)
           (eval
            (quote
             (loopy-iter (arraying [a [b c]] [[1 [2 3]] [4 [5 6]]])
                         (finally-return a b c)))))
    (equal '(4 5 6)
           (eval
            (quote
             (loopy-iter (arraying [a [b [c]]] [[1 [2 [3]]] [4 [5 [6]]]])
                         (finally-return a b c)))))
    (equal '(4 5 6)
           (eval
            (quote
             (loopy-iter (arraying [a (b c)] [[1 (2 3)] [4 (5 6)]])
                         (finally-return a b c))))))))

(ert-deftest array-multi-array ()
  (should (equal '((1 3) (1 4) (2 3) (2 4))
                 (loopy-iter (arraying i [1 2] [3 4])
                             (collecting i))))

  (should (equal '((1 3) (2 3))
                 (loopy-iter (arraying i [1 2] [3 4] :by 2)
                             (collecting i))))

  ;; Just to check how quoting is handled.
  (should (equal '((1 3) (1 4) (2 3) (2 4))
                 (loopy-iter (arraying i `[1 ,(1+ 1)] [3 4])
                             (collecting i)))))

(ert-deftest array-multi-array-destructuring ()
  (should (equal '((1 1 2 2) (3 4 3 4))
                 (eval (quote (loopy-iter (arraying (i j) [1 2] [3 4])
                                          (collecting c1 i)
                                          (collecting c2 j)
                                          (finally-return c1 c2)))))))

(ert-deftest array-keywords ()
  (should (equal '((0 . 4) (1 . 3) (2 . 2) (3 . 1) (4 . 0))
                 (eval (quote (loopy-iter (arraying i [4 3 2 1 0] :index cat)
                                          (collecting (cons cat i)))))))

  (should (equal '(0 2 4 6 8 10)
                 (eval (quote (loopy-iter (arraying i [0 1 2 3 4 5 6 7 8 9 10] :by 2)
                                          (collecting i))))))

  (should (equal '(8 6 4 2)
                 (eval (quote (loopy-iter (arraying i [0 1 2 3 4 5 6 7 8 9 10]
                                                    :from 8 :downto 1 :by 2)
                                          (collecting i))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (loopy-iter (arraying i [0 1 2 3 4 5 6 7 8 9 10] :upto 7)
                                          (collecting i))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (loopy-iter (arraying i [0 1 2 3 4 5 6 7 8 9 10] :to 7)
                                          (collecting i))))))

  (should (equal '(10 9 8 7 6 5 4 3)
                 (eval (quote (loopy-iter (arraying i [0 1 2 3 4 5 6 7 8 9 10] :downto 3)
                                          (collecting i))))))

  (should (equal '(10 9 8)
                 (eval (quote (loopy-iter (arraying i [0 1 2 3 4 5 6 7 8 9 10] :above 7)
                                          (collecting i))))))

  (should (equal '(0 1 2)
                 (eval (quote (loopy-iter (arraying i [0 1 2 3 4 5 6 7 8 9 10] :below 3)
                                          (collecting i)))))))

;;;;; Array Ref
(ert-deftest arraying-ref ()
  (should (equal "aaa"
                 (eval (quote (loopy-iter (with (my-str "cat"))
                                          (arraying-ref i my-str)
                                          (setf i ?a)
                                          (finally-return my-str))))))
  (should (equal "aaa"
                 (eval (quote (loopy-iter (with (my-str "cat"))
                                          (stringing-ref i my-str)
                                          (setf i ?a)
                                          (finally-return my-str)))))))


(ert-deftest arraying-ref-destructuring ()
  (should (and (equal [(7 8 9) (7 8 9)]
                      (eval (quote (loopy-iter (with (my-array [(1 2 3) (4 5 6)]))
                                               (arraying-ref (i j k) my-array)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k 9)
                                               (finally-return my-array)))))
               (equal [(7 8 9 10) (7 8 9 10)]
                      (eval (quote (loopy-iter (with (my-array [(1 2 3 4) (4 5 6 8)]))
                                               (arraying-ref (i j . k) my-array)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k '(9 10))
                                               (finally-return my-array)))))
               (equal [[7 8 9 4] [7 8 9 8]]
                      (eval (quote (loopy-iter (with (my-array [[1 2 3 4] [4 5 6 8]]))
                                               (arraying-ref [i j k] my-array)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k 9)
                                               (finally-return my-array))))))))

(ert-deftest arraying-ref-recursive-destructuring ()
  (should (and (equal [(7 [8 9]) (7 [8 9])]
                      (eval (quote (loopy-iter (with (my-array [(1 [2 3]) (4 [5 6])]))
                                               (arraying-ref (i [j k]) my-array)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k 9)
                                               (finally-return my-array)))))
               (equal [[7 [8 9] 4] [7 [8 9] 8]]
                      (eval (quote (loopy-iter (with (my-array [[1 [2 3] 4] [4 [5 6] 8]]))
                                               (arraying-ref [i [j k]] my-array)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k 9)
                                               (finally-return my-array))))))))

(ert-deftest arraying-ref-keywords ()
  (should (equal "a1a3a5a7a9"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (arraying-ref i my-str :by 2)
                                          (setf i ?a)
                                          (finally-return my-str))))))

  (should (equal "a1a3a5a7a9"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (arraying-ref i my-str :by 2 :index cat)
                                          (setf (aref my-str cat) ?a)
                                          (finally-return my-str))))))

  (should (equal "0a2a4a6a8a"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (arraying-ref i my-str :from 1 :by 2 )
                                          (setf i ?a)
                                          (finally-return my-str))))))

  (should (equal "0123456a8a"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (arraying-ref i my-str :downto 6 :by 2 )
                                          (setf i ?a)
                                          (finally-return my-str))))))

  (should (equal "aaaaa56789"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (arraying-ref i my-str :below 5)
                                          (setf i ?a)
                                          (finally-return my-str))))))

  (should (equal "012345aaaa"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (arraying-ref i my-str :above 5)
                                          (setf i ?a)
                                          (finally-return my-str))))))

  (should (equal "aaaaaa6789"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (arraying-ref i my-str :upto 5)
                                          (setf i ?a)
                                          (finally-return my-str))))))

  (should (equal "0a2a4a6a8a"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (arraying-ref i my-str :upfrom 1 :by 2 )
                                          (setf i ?a)
                                          (finally-return my-str)))))))

;;;;; Cons
(ert-deftest cons ()
  (should (equal '((1 2 3 4) (2 3 4) (3 4) (4))
                 (eval (quote (loopy-iter (consing x '(1 2 3 4))
                                          (collecting coll x)
                                          (finally-return coll))))))

  (should (equal '((1 2 3 4) (2 3 4) (3 4) (4))
                 (eval (quote (loopy-iter (consing x '(1 2 3 4) :by #'cdr)
                                          (collecting coll x)
                                          (finally-return coll))))))

  (should (equal '((1 2 3 4) (3 4))
                 (eval (quote (loopy-iter (consing x '(1 2 3 4) :by #'cddr)
                                          (collecting coll x)
                                          (finally-return coll))))))

  (should (equal '((1 2 3 4) (3 4))
                 (eval (quote (loopy-iter (consing x '(1 2 3 4)
                                                   :by (lambda (x) (cddr x)))
                                          (collecting coll x)
                                          (finally-return coll))))))

  (should (equal '((1 2 3 4) (3 4))
                 (eval (quote (let ((f (lambda (x) (cddr x))))
                                (loopy-iter (consing x '(1 2 3 4) :by f)
                                            (collecting coll x)
                                            (finally-return coll)))))))

  (should (equal '((1 (2 3 4)) (2 (3 4)) (3 (4)) (4 nil))
                 (eval (quote (loopy-iter (consing (i . j) '(1 2 3 4))
                                          (collecting coll (list i j))
                                          (finally-return coll))))))

  (should (equal '((1 (2 3 4)) (3 (4)))
                 (eval (quote (loopy-iter (consing (i . j) '(1 2 3 4) :by #'cddr)
                                          (collecting coll (list i j))
                                          (finally-return coll)))))))


;;;;; List
(ert-deftest list ()
  (should (= 3 (eval (quote (loopy-iter (listing i '(1 2 3))
                                        ;; Same thing:
                                        ;; (after-do (cl-return i))
                                        (finally-return i))))))
  (should (equal '(1 3)
                 (let ((my-cddr (lambda (x)  (cddr x))))
                   (loopy-iter (listing i '(1 2 3 4) :by my-cddr)
                               (collecting i)))))

  (should (equal '(1 3)
                 (loopy-iter (listing i '(1 2 3 4) :by (lambda (x) (cddr x)))
                             (collecting i))))

  (should (equal '(1 3)
                 (loopy-iter (listing i '(1 2 3 4) :by #'cddr)
                             (collecting i)))))

(ert-deftest list-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy-iter (listing (a . b)
                                                        '((1 . 2) (3 . 4) (5 . 6)))
                                               (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy-iter (listing (a . b)
                                                        '((1 2) (3 4) (5 6)))
                                               (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy-iter (listing (a b c d)
                                                        '((1 2 3 4) (4 5 6 7)))
                                               (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy-iter (listing [i j k] '([1 2 3] [4 5 6]))
                                               (finally-return i j k))))))))

(ert-deftest list-recursive-destructuring ()
  (should (equal '(4 5 6)
                 (eval (quote (loopy-iter (listing (a (b c)) '((1 (2 3)) (4 (5 6))))
                                          (finally-return (list a b c)))))))
  (should (equal '(5 5 6)
                 ;; This is more of an evaluation-time test.
                 (eval (quote (loopy-iter (listing (a . (b c)) '((1 . (1 2)) (5 . (5 6))))
                                          (finally-return (list a b c)))))))
  (should (equal '(4 5 6)
                 (loopy-iter (listing (a . [b c]) '((1 . [2 3]) (4 . [5 6])))
                             (finally-return a b c))))
  (should (equal '(5 5 6)
                 (eval (quote (loopy-iter (listing (a (b (c))) '((1 (1 (2))) (5 (5 (6)))))
                                          (finally-return (list a b c))))))))

(ert-deftest list-multi-list ()
  (should (equal '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))
                 (eval (quote (loopy-iter (listing i '(1 2 3) '(4 5 6))
                                          (collecting i))))))

  (should (equal '((1 7) (1 8) (1 9) (2 7) (2 8) (2 9))
                 (eval (quote (cl-labels ((fx () '(7 8 9)))
                                (loopy-iter (listing i '(1 2) (fx))
                                            (collecting i)))))))

  (should (equal '((10 13) (10 15) (11 14) (12 13) (12 15))
                 (eval (quote (loopy-iter (listing i '(10 11 12) '(13 14 15) :by #'cddr)
                                          (collecting i)))))))

(ert-deftest list-multi-list-destructuring ()
  (should (equal '((1 1 2 2) (4 5 4 5))
                 (eval (quote (loopy-iter (listing (i j) '(1 2) '(4 5))
                                          (collecting c1 i)
                                          (collecting c2 j)
                                          (finally-return c1 c2)))))))

;;;;; List Ref
(ert-deftest listing-ref ()
  (should (equal  '(7 7 7)
                  (eval (quote (loopy-iter (with (my-list '(1 2 3)))
                                           (listing-ref i my-list)
                                           (setf i 7)
                                           (finally-return my-list))))))

  (should (equal  '(7 2 7)
                  (eval (quote (loopy-iter (with (my-list '(1 2 3)))
                                           (listing-ref i my-list :by #'cddr)
                                           (setf i 7)
                                           (finally-return my-list))))))

  (should (equal  '(7 2 7)
                  (eval (quote (loopy-iter (with (my-list '(1 2 3)))
                                           (listing-ref i my-list
                                                        :by (lambda (x) (cddr x)))
                                           (setf i 7)
                                           (finally-return my-list))))))

  (should (equal  '(7 2 7)
                  (eval (quote (let ((f (lambda (x) (cddr x))))
                                 (loopy-iter (with (my-list '(1 2 3)))
                                             (listing-ref i my-list :by f)
                                             (setf i 7)
                                             (finally-return my-list))))))))

(ert-deftest listing-ref-destructuring ()
  (should (and (equal '((7 8 9) (7 8 9))
                      (eval (quote (loopy-iter (with (my-list '((1 2 3) (4 5 6))))
                                               (listing-ref (i j k) my-list)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k 9)
                                               (finally-return my-list)))))
               (equal '((7 8 9 10) (7 8 9 10))
                      (eval (quote (loopy-iter (with (my-list '((1 2 3 4) (4 5 6 8))))
                                               (listing-ref (i j . k) my-list)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k '(9 10))
                                               (finally-return my-list)))))
               (equal '([7 8 9 4] [7 8 9 8])
                      (eval (quote (loopy-iter (with (my-list '([1 2 3 4] [4 5 6 8])))
                                               (listing-ref [i j k] my-list)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k 9)
                                               (finally-return my-list))))))))

(ert-deftest listing-ref-recursive-destructuring ()
  (should (equal '((7 (8 9)) (7 (8 9)))
                 (eval (quote (loopy-iter (with (my-list '((1 (2 3)) (4 (5 6)))))
                                          (listing-ref (i (j k)) my-list)
                                          (setf i 7)
                                          (setf j 8)
                                          (setf k 9)
                                          (finally-return my-list))))))
  (should (equal '([7 (8 9) 4] [7 (8 9) 8])
                 (eval (quote (loopy-iter (with (my-list '([1 (2 3) 4] [4 (5 6) 8])))
                                          (listing-ref [i (j k) l] my-list)
                                          (setf i 7)
                                          (setf j 8)
                                          (setf k 9)
                                          (finally-return my-list)))))))

;;;;; Map
(ert-deftest map ()
  (should (equal '((a . 1) (b . 2))
                 (eval (quote (loopy-iter (mapping pair '((a . 1) (b . 2)))
                                          (collecting coll pair)
                                          (finally-return coll))))))

  (should (equal '((a . 1) (b . 2))
                 (eval (quote (loopy-iter (mapping-pairs pair '((a . 1) (b . 2)))
                                          (collecting coll pair)
                                          (finally-return coll))))))

  (should (equal '((0 . a) (1 . b) (2 . c) (3 . d))
                 (eval (quote (loopy-iter (mapping pair [a b c d])
                                          (collecting coll pair)
                                          (finally-return coll))))))

  (should (equal '((0 . a) (1 . b) (2 . c) (3 . d))
                 (eval (quote (loopy-iter (mapping-pairs pair [a b c d])
                                          (collecting coll pair)
                                          (finally-return coll))))))

  (let ((my-hash (make-hash-table)))
    (puthash 'a 1 my-hash)
    (puthash 'b 2 my-hash)
    (should (equal '((a . 1) (b . 2))
                   (loopy-iter (mapping pair my-hash)
                               (collecting coll pair)
                               (finally-return coll))))))

(ert-deftest map-unique ()
  (should (equal '((a . 1) (b . 2) (c . 3))
                 (eval (quote (loopy-iter (mapping-pairs pair '((a . 1)
                                                                (a . 27)
                                                                (b . 2)
                                                                (c . 3)))
                                          (collecting coll pair)
                                          (finally-return coll))))))

  (should (equal '((a . 1) (b . 2) (c . 3))
                 (eval (quote (loopy-iter (mapping-pairs pair '((a . 1)
                                                                (a . 27)
                                                                (b . 2)
                                                                (c . 3))
                                                         :unique t)
                                          (collecting coll pair)
                                          (finally-return coll))))))

  (should (equal '((a . 1) (a . 27) (b . 2) (c . 3))
                 (eval (quote (loopy-iter (mapping-pairs pair '((a . 1)
                                                                (a . 27)
                                                                (b . 2)
                                                                (c . 3))
                                                         :unique nil)
                                          (collecting coll pair)
                                          (finally-return coll)))))))

(ert-deftest map-destructuring ()
  (should (equal '((a b) (1 2))
                 (eval (quote (loopy-iter (mapping (key . val) '((a . 1) (b . 2)))
                                          (collecting keys key)
                                          (collecting vals val)
                                          (finally-return keys vals))))))

  (should (equal '((0 1 2 3) (a b c d))
                 (eval (quote (loopy-iter (mapping (key . val) [a b c d])
                                          (collecting keys key)
                                          (collecting vals val)
                                          (finally-return keys vals))))))
  (let ((my-hash (make-hash-table)))
    (puthash 'a 1 my-hash)
    (puthash 'b 2 my-hash)
    (should (equal '((a b) (1 2))
                   (loopy-iter (mapping (key . val) my-hash)
                               (collecting keys key)
                               (collecting vals val)
                               (finally-return keys vals))))))

;;;;; Map Ref

(ert-deftest map-ref ()
  (should (equal [17 18 19 20 21]
                 (eval (quote (loopy-iter (with (map (vector 10 11 12 13 14)))
                                          (mapping-ref i map)
                                          (cl-incf i 7)
                                          (finally-return map))))))

  (should (equal '([17 18 19 20 21] (0 1 2 3 4))
                 (eval (quote (loopy-iter (with (map (vector 10 11 12 13 14)))
                                          (mapping-ref i map :key my-key)
                                          (cl-incf i 7)
                                          (collecting my-key)
                                          (finally-return map loopy-result)))))))

(ert-deftest map-ref-unique ()
  (should (equal '(:a 8 :a ignored :b 10)
                 (let ((map (list :a 1 :a 'ignored :b 3)))
                   (loopy-iter (mapping-ref i map)
                               (cl-incf i 7)
                               (finally-return map)))))

  (should (equal '(:a 8 :a ignored :b 10)
                 (let ((map (list :a 1 :a 'ignored :b 3)))
                   (loopy-iter (mapping-ref i map :unique t)
                               (cl-incf i 7)
                               (finally-return map)))))

  (should (equal '(:a 15 :a ignored :b 10)
                 (let ((map (list :a 1 :a 'ignored :b 3)))
                   (loopy-iter (mapping-ref i map :unique nil)
                               (cl-incf i 7)
                               (finally-return map))))))

(ert-deftest map-ref-destructuring ()
  (should (equal [[7 8] [7 8]]
                 (eval (quote (loopy-iter (with (map (vector (vector 10 11)
                                                             (vector 12 13))))
                                          (mapping-ref [i j] map)
                                          (setf i 7)
                                          (setf j 8)
                                          (finally-return map))))))

  (should (equal '((a 7 8) (b 7 8))
                 (eval (quote (loopy-iter (with (map (list (cons 'a (list 1 2))
                                                           (cons 'b (list 3 4)))))
                                          (mapping-ref (i j) map)
                                          (setf i 7)
                                          (setf j 8)
                                          (finally-return map)))))))

;;;;; Nums
(ert-deftest nums ()
  (should (equal '(1 3 5)
                 (eval (quote (loopy-iter (numbering i 1 5 2)
                                          (collecting i))))))

  (should (equal '(5 3 1)
                 (eval (quote (loopy-iter (numbering i 5 1 -2)
                                          (collecting i)))))))

(ert-deftest nums-keywords ()
  (should (equal '(1 3 5)
                 (eval (quote (loopy-iter (numbering i 1 5 :by 2)
                                          (collecting i))))))

  (should (equal '(5 3 1)
                 (eval (quote (loopy-iter (numbering i 5 :downto 1 :by 2)
                                          (collecting i))))))

  (should (equal '(0 7 14)
                 (eval (quote (loopy-iter (cycling 3)
                                          (numbering i 0 :by 7)
                                          (collecting i))))))

  (should (equal '(0 -7 -14 -21 -28 -35 -42)
                 (eval (quote (loopy-iter (cycling 7)
                                          (numbering i :downfrom 0 :by 7)
                                          (collecting i))))))
  (should (equal '(7 8 9)
                 (eval (quote (loopy-iter (cycling 3)
                                          (numbering i :upfrom 7)
                                          (collecting i))))))

  (should (equal '(7 8 9)
                 (eval (quote (loopy-iter (cycling 3)
                                          (numbering i :from 7)
                                          (collecting i))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (loopy-iter (numbering i :upto 7)
                                          (collecting i))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (loopy-iter (numbering i :to 7)
                                          (collecting i))))))

  (should (equal '(0 -1 -2 -3 -4 -5 -6 -7)
                 (eval (quote (loopy-iter (numbering i :downto -7)
                                          (collecting i))))))

  (should (equal '(0 -1 -2 -3 -4 -5 -6)
                 (eval (quote (loopy-iter (numbering i :above -7)
                                          (collecting i))))))

  (should (equal '(0 1 2)
                 (eval (quote (loopy-iter (numbering i :below 3)
                                          (collecting i))))))

  (should (equal nil
                 (eval (quote (loopy-iter (numbering i :above 3)
                                          (collecting i))))))
  (should (equal '(0 1.5 3.0)
                 (loopy-iter (numbering i 0 3 :by 1.5)
                             (collecting i))))

  (should (equal '(0 1.5 3.0 4.5)
                 (eval (quote (loopy-iter (numbering i 0 5 :by 1.5)
                                          (collecting i))))))

  ;; NOTE: It remains to be seen how well this test works.
  (progn
    (cl-float-limits)
    (should (cl-every (lambda (x y) (> cl-float-epsilon (- x y)))
                      '(0.5 0.3 0.1 -0.1 -0.3 -0.5)
                      (eval (quote (loopy-iter (numbering i
                                                          :downfrom 0.5
                                                          :above -0.7
                                                          :by 0.2)
                                               (collecting i))))))))

;;;;; Nums-Down
(ert-deftest nums-down ()
  (should (equal '(10 8 6 4 2)
                 (eval (quote (loopy-iter (numbering-down i 10 1 :by 2)
                                          (collecting i)))))))

;;;;;; Nums-Up
(ert-deftest nums-up ()
  (should (equal '(1 3 5 7 9)
                 (eval (quote (loopy-iter (numbering-up i 1 10 :by 2)
                                          (collecting i))))))

  (should (equal '(1 3 5 7 9)
                 (eval (quote (loopy-iter (numbering-up i 1 10 2)
                                          (collecting i))))))

  (should (equal '(1 3 5 7 9)
                 (eval (quote (loopy-iter (numbering-up i 1 10 :by 2)
                                          (collecting i))))))

  (should (equal '(1 3 5 7 9)
                 (eval (quote (loopy-iter (numbering-up i 1 10 :by 2)
                                          (collecting i)))))))

;;;;; Repeat
(ert-deftest repeat-cycle-no-var ()
  (should (= 3 (length (eval (quote (loopy-iter (cycling 3)
                                                (listing i (number-sequence 1 10))
                                                (collecting coll i)
                                                (finally-return coll))))))))

(ert-deftest repeat-cycle-var ()
  "Need to test order of execution and functionality."
  (should (equal '(0 1 2)
                 (eval (quote (loopy-iter (collecting coll i)
                                          (cycling i 3)
                                          (finally-return coll)))))))

;;;;; Seq
(ert-deftest seq ()
  (should (eval (quote (loopy-iter (sequencing l '(1 2 3 4 5))
                                   (sequencing a [1 2 3 4 5])
                                   (if (/= l a)
                                       (returning nil))
                                   (finally-return t))))))

(ert-deftest seq-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy-iter (sequencing (a . b)
                                                           [(1 . 2) (3 . 4) (5 . 6)])
                                               (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy-iter (sequencing (a . b)
                                                           [(1 2) (3 4) (5 6)])
                                               (finally-return a b)))))
               (equal '(4 5 6)
                      (eval (quote (loopy-iter (sequencing (a b c)
                                                           [(1 2 3) (4 5 6)])
                                               (finally-return a b c)))))
               (equal '(5 6)
                      (eval (quote (loopy-iter (sequencing (a . b)
                                                           '((1 . 2) (3 . 4) (5 . 6)))
                                               (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy-iter (sequencing (a . b)
                                                           '((1 2) (3 4) (5 6)))
                                               (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy-iter (sequencing (a b c d)
                                                           '((1 2 3 4) (4 5 6 7)))
                                               (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy-iter (sequencing [i j k] '([1 2 3] [4 5 6]))
                                               (finally-return i j k)))))
               (equal '(4 5 6)
                      (eval (quote (loopy-iter (sequencing [i j k] [[1 2 3] [4 5 6]])
                                               (finally-return i j k))))))))

(ert-deftest seqing-ref-destructuring ()
  (should (and (equal [(7 8 9) (7 8 9)]
                      (eval (quote (loopy-iter (with (my-seq [(1 2 3) (4 5 6)]))
                                               (sequencing-ref (i j k) my-seq)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k 9)
                                               (finally-return my-seq)))))
               (equal [(7 8 9 10) (7 8 9 10)]
                      (eval (quote (loopy-iter (with (my-seq [(1 2 3 4) (4 5 6 8)]))
                                               (sequencing-ref (i j . k) my-seq)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k '(9 10))
                                               (finally-return my-seq)))))
               (equal '((7 8 9) (7 8 9))
                      (eval (quote (loopy-iter (with (my-seq '((1 2 3) (4 5 6))))
                                               (sequencing-ref (i j k) my-seq)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k 9)
                                               (finally-return my-seq)))))
               (equal '((7 8 9 10) (7 8 9 10))
                      (eval (quote (loopy-iter (with (my-seq '((1 2 3 4) (4 5 6 8))))
                                               (sequencing-ref (i j . k) my-seq)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k '(9 10))
                                               (finally-return my-seq)))))
               (equal '([7 8 9 4] [7 8 9 8])
                      (eval (quote (loopy-iter (with (my-seq '([1 2 3 4] [4 5 6 8])))
                                               (sequencing-ref [i j k] my-seq)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k 9)
                                               (finally-return my-seq)))))
               (equal [[7 8 9 4] [7 8 9 8]]
                      (eval (quote (loopy-iter (with (my-seq [[1 2 3 4] [4 5 6 8]]))
                                               (sequencing-ref [i j k] my-seq)
                                               (setf i 7)
                                               (setf j 8)
                                               (setf k 9)
                                               (finally-return my-seq))))))))

(ert-deftest seq-keywords ()
  (should (equal '((0 . 4) (1 . 3) (2 . 2) (3 . 1) (4 . 0))
                 (eval (quote (loopy-iter (sequencing i [4 3 2 1 0] :index cat)
                                          (collecting (cons cat i)))))))

  (should (equal '(0 2 4 6 8 10)
                 (eval (quote (loopy-iter (sequencing i [0 1 2 3 4 5 6 7 8 9 10] :by 2)
                                          (collecting i))))))

  (should (equal '(8 6 4 2)
                 (eval (quote (loopy-iter (sequencing i [0 1 2 3 4 5 6 7 8 9 10]
                                                      :from 8 :downto 1 :by 2)
                                          (collecting i))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (loopy-iter (sequencing i [0 1 2 3 4 5 6 7 8 9 10] :upto 7)
                                          (collecting i))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (loopy-iter (sequencing i [0 1 2 3 4 5 6 7 8 9 10] :to 7)
                                          (collecting i))))))

  (should (equal '(10 9 8 7 6 5 4 3)
                 (eval (quote (loopy-iter (sequencing i [0 1 2 3 4 5 6 7 8 9 10] :downto 3)
                                          (collecting i))))))

  (should (equal '(10 9 8)
                 (eval (quote (loopy-iter (sequencing i [0 1 2 3 4 5 6 7 8 9 10] :above 7)
                                          (collecting i))))))

  (should (equal '(0 1 2)
                 (eval (quote (loopy-iter (sequencing i [0 1 2 3 4 5 6 7 8 9 10] :below 3)
                                          (collecting i)))))))

(ert-deftest seq-multi-seq ()
  (should (equal '((1 3) (1 4) (2 3) (2 4))
                 (eval (quote (loopy-iter (sequencing i [1 2] '(3 4))
                                          (collecting i))))))

  (should (equal '((1 3) (2 3))
                 (eval (quote (loopy-iter (sequencing i [1 2] '(3 4) :by 2)
                                          (collecting i)))))))

;;;;; Seq Index
(ert-deftest seq-index ()
  (should (equal '(0 1 2 3)
                 (eval (quote (loopy-iter (seqing-index i [1 2 3 4])
                                          (collecting i))))))

  (should (equal '(0 1 2 3 4 5 6)
                 (eval (quote (loopy-iter (arraying-index i "abcdefg")
                                          (collecting i))))))

  (should (equal '(0 1 2 3 4 5 6)
                 (eval (quote (loopy-iter (stringing-index i "abcdefg")
                                          (collecting i))))))

  (should (equal '(0 1 2 3 4)
                 (eval (quote (loopy-iter (listing-index i '(1 2 3 4 5))
                                          (collecting i)))))))

(ert-deftest seq-index-keywords ()
  (should (equal '(0 2 4 6 8 10)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy-iter (seqing-index i my-seq :by 2)
                                            (collecting (elt my-seq i))))))))

  (should (equal '(8 6 4 2)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy-iter (seqing-index i my-seq
                                                          :from 8 :downto 1 :by 2)
                                            (collecting (elt my-seq i))))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy-iter (seqing-index i my-seq :upto 7)
                                            (collecting (elt my-seq i))))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy-iter (seqing-index i my-seq :to 7)
                                            (collecting (elt my-seq i))))))))

  (should (equal '(10 9 8 7 6 5 4 3)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy-iter (seqing-index i my-seq :downto 3)
                                            (collecting (elt my-seq i))))))))

  (should (equal '(10 9 8)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy-iter (seqing-index i my-seq :above 7)
                                            (collecting (elt my-seq i))))))))

  (should (equal '(0 1 2)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy-iter (seqing-index i my-seq :below 3)
                                            (collecting (elt my-seq i)))))))))

;;;;; Seq Ref
(ert-deftest seq-ref ()
  (should
   (equal '(7 7 7 7)
          (eval (quote (loopy-iter (with (my-seq '(1 2 3 4)))
                                   (sequencing-ref i my-seq)
                                   (setf i 7)
                                   (finally-return my-seq)))))))

(ert-deftest seq-ref-keywords ()
  (should (equal "a1a3a5a7a9"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (sequencing-ref i my-str :by 2)
                                          (setf i ?a)
                                          (finally-return my-str))))))

  (should (equal "a1a3a5a7a9"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (sequencing-ref i my-str :by 2 :index cat)
                                          (setf (aref my-str cat) ?a)
                                          (finally-return my-str))))))

  (should (equal '(0 cat 2 cat 4 cat 6 cat 8 cat)
                 (eval (quote (loopy-iter (with (my-list '(0 1 2 3 4 5 6 7 8 9)))
                                          (sequencing-ref i my-list :from 1 :by 2 )
                                          (setf i 'cat)
                                          (finally-return my-list))))))

  (should (equal "0123456a8a"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (sequencing-ref i my-str :downto 6 :by 2 )
                                          (setf i ?a)
                                          (finally-return my-str))))))

  (should (equal "aaaaa56789"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (sequencing-ref i my-str :below 5)
                                          (setf i ?a)
                                          (finally-return my-str))))))

  (should (equal "012345aaaa"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (sequencing-ref i my-str :above 5)
                                          (setf i ?a)
                                          (finally-return my-str))))))

  (should (equal '(0 1 2 3 4 5 cat cat cat cat)
                 (eval (quote (loopy-iter (with (my-list '(0 1 2 3 4 5 6 7 8 9)))
                                          (sequencing-ref i my-list :above 5)
                                          (setf i 'cat)
                                          (finally-return my-list))))))

  (should (equal "aaaaaa6789"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (sequencing-ref i my-str :upto 5)
                                          (setf i ?a)
                                          (finally-return my-str))))))

  (should (equal "0a2a4a6a8a"
                 (eval (quote (loopy-iter (with (my-str "0123456789"))
                                          (sequencing-ref i my-str :upfrom 1 :by 2 )
                                          (setf i ?a)
                                          (finally-return my-str))))))

  (should (equal '(0 cat 2 cat 4 cat 6 cat 8 cat)
                 (eval (quote (loopy-iter (with (my-list '(0 1 2 3 4 5 6 7 8 9)))
                                          (sequencing-ref i my-list :upfrom 1 :by 2)
                                          (setf i 'cat)
                                          (finally-return my-list)))))))

;;;; Accumulation Commands
;;;;; Final updates
(ert-deftest accumulation-conflicting-final-updates ()
  (should-error (eval (quote (loopy-iter (listing i '((1) (2) (3)))
                                         (appending i)
                                         (vconcating i)))))

  (should-error (eval (quote (loopy-iter (listing i '((1) (2) (3)))
                                         (collecting i)
                                         (collecting (1+ i) :result-type vector))))))

;;;;; Into Argument
(ert-deftest accumulation-into-argument ()
  (should (equal '((2 3 4) (2 2 2))
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (collecting (1+ i) :into coll1)
                                          ;; This should be the same value repeated.
                                          ;; If not, it means `coll1'  is constructed
                                          ;; in reverse, instead of being treated as
                                          ;; explicitly named.
                                          (collecting coll2 (cl-first coll1))
                                          (finally-return coll1 coll2))))))

  (should (= 9 (eval (quote (loopy-iter (listing i '(1 2 3))
                                        (summing (1+ i) :into j)
                                        (finally-return j)))))))

(ert-deftest accumulation-raise-error-bad-arg ()
  :expected-result :failed
  (eval (quote (loopy-iter (listing i '(1 2 3))
                           (collecting i :casdfasdf x)))))

;;;;; Command Compatibility
(ert-deftest accumulation-compatibility ()
  (should (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                   (collecting i)
                                   (appending i)
                                   (adjoining i)
                                   (unioning i)
                                   (nunioning (copy-sequence i))
                                   (nconcing (copy-sequence i))))))

  (should-error (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                         (collecting i)
                                         (concating i)))))

  (should-error (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                         (appending i)
                                         (concating i)))))

  (should-error (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                         (adjoining i)
                                         (concating i)))))

  (should-error (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                         (unioning i)
                                         (concating i)))))

  (should-error (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                         (nunioning i)
                                         (concating i)))))

  (should-error (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                         (nconcing i)
                                         (concating i)))))

  (should-error (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                         (vconcating i)
                                         (concating i)))))

  ;; Also check that we don't throw errors for commands of the same type.
  (should (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                   (vconcating i)
                                   (vconcating i)))))

  (should (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                   (collecting i)
                                   (collecting i)))))

  (should (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                   (concating i)
                                   (concating i))))))

;;;;; Order of implicit returns.
(ert-deftest implicit-collect-order ()
  (should (equal '((2) (1 3))
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (if (cl-evenp i)
                                              (collecting evens i)
                                            (collecting odds i))
                                          (finally-return evens odds)))))))

;;;;; Name of implicit accumulations
(ert-deftest implicit-accumulation-name ()
  (should
   (and (equal '(1 2 3)
               (eval (quote (loopy-iter (listing i '(1 2 3))
                                        (collecting i)
                                        (else-do (cl-return loopy-result))))))
        (equal '(0 1 2 3)
               (eval (quote (loopy-iter (listing i '(1 2 3))
                                        (collecting i)
                                        (else-do
                                         (push 0 loopy-result)
                                         (cl-return loopy-result))))))
        (equal '(0 1 2 3)
               (eval (quote (loopy-iter (listing i '(1 2 3))
                                        (collecting i)
                                        (finally-do
                                         (push 0 loopy-result))
                                        (finally-return loopy-result)))))
        (equal '(1 2 3)
               (eval (quote (loopy-iter (listing i '(1 2 3))
                                        (collecting i)
                                        (finally-return loopy-result))))))))

;;;;; Accumulate
(ert-deftest accumulate ()
  (should (equal '(2 1)
                 (eval (quote (loopy-iter (listing i '(1 2))
                                          (accumulating my-accum i #'cons
                                                        :init nil)
                                          (finally-return my-accum))))))

  (should (equal '((3 1) (4 2))
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4)))
                                          (accumulating (accum1 accum2) i #'cons
                                                        :init nil)
                                          (finally-return accum1 accum2))))))

  (should (equal '((3 1) (4 2))
                 (eval (quote (let ((f #'cons))
                                (loopy-iter (listing i '((1 2) (3 4)))
                                            (accumulating (accum1 accum2) i f
                                                          :init nil)
                                            (finally-return accum1 accum2))))))))

;;;;; Adjoin
(ert-deftest adjoin ()
  (should (equal '((1 . 1) (1 . 2) (2 . 3))
                 (eval (quote (loopy-iter (listing i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                                          (adjoining a i)
                                          (finally-return a))))))

  (should (equal '((1 . 1) (1 . 2) (2 . 3))
                 (eval (quote (loopy-iter (listing i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                                          (adjoining a i :test #'equal)
                                          (finally-return a))))))

  (should (equal '((1 . 1) (1 . 2) (1 . 2) (2 . 3))
                 (eval (quote (loopy-iter (listing i '((1 . 1) (1 . 2)
                                                       (1 . 2) (2 . 3)))
                                          (adjoining a i :test #'eql)
                                          (finally-return a))))))

  (should (equal '((1 . 1) (2 . 3))
                 (eval (quote (loopy-iter (listing i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                                          (adjoining a i :test #'= :key #'car)
                                          (finally-return a))))))

  (should (equal '((1 . 1) (2 . 3))
                 (let ((my-test #'=)
                       (my-key #'car))
                   (loopy-iter (listing i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                               (adjoining a i :test my-test :key my-key)
                               (finally-return a)))))

  (should (equal '((1 . 1) (2 . 3))
                 (let ((my-key #'car))
                   (loopy-iter (listing i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                               (adjoining a i :key my-key)
                               (finally-return a)))))

  (should (equal '((1 . 1) (1 . 2) (2 . 3))
                 (let ((my-test #'equal))
                   (loopy-iter (listing i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                               (adjoining a i :test my-test)
                               (finally-return a))))))

(ert-deftest adjoin-destructuring ()
  (should (equal '(((1 . 1) (1 . 2)) ((1 . 2) (2 . 3)))
                 (eval (quote (loopy-iter (listing i '(((1 . 1) (1 . 2))
                                                       ((1 . 2) (2 . 3))))
                                          (adjoining (a1 a2) i)
                                          (finally-return a1 a2))))))

  (should (equal '(((1 . 2)) ((1 . 1) (2 . 3)))
                 (eval (quote (loopy-iter (listing i '(((1 . 2) (1 . 1))
                                                       ((1 . 2) (2 . 3))))
                                          (adjoining (a1 a2) i :test #'equal)
                                          (finally-return a1 a2))))))

  (should (equal '(((1 . 2)) ((1 . 1) (2 . 3)))
                 (eval (quote (loopy-iter (with (test #'equal))
                                          (listing i '(((1 . 2) (1 . 1))
                                                       ((1 . 2) (2 . 3))))
                                          (adjoining (a1 a2) i :test test)
                                          (finally-return a1 a2))))))

  (should (equal '(((1 . 1)) ((1 . 2) (2 . 3)))
                 (eval (quote (loopy-iter (listing i '(((1 . 1) (1 . 2))
                                                       ((1 . 2) (2 . 3))))
                                          (adjoining (a1 a2) i :key #'car)
                                          (finally-return a1 a2))))))

  (should (equal '(((1 . 1)) ((1 . 2) (2 . 3)))
                 (eval (quote (loopy-iter (with (key #'car))
                                          (listing i '(((1 . 1) (1 . 2))
                                                       ((1 . 2) (2 . 3))))
                                          (adjoining (a1 a2) i :key key)
                                          (finally-return a1 a2)))))))

(ert-deftest adjoin-implicit ()
  (should (equal '((1 . 1) (1 . 2) (2 . 3))
                 (eval (quote (loopy-iter (listing i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                                          (adjoining i))))))

  (should (equal '((1 . 1) (1 . 2) (2 . 3))
                 (eval (quote (loopy-iter (listing i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                                          (adjoining i :test #'equal))))))

  (should (equal '((1 . 1) (1 . 2) (1 . 2) (2 . 3))
                 (eval (quote (loopy-iter (listing i '((1 . 1) (1 . 2)
                                                       (1 . 2) (2 . 3)))
                                          (adjoining i :test #'eql))))))

  (should (equal '((1 . 1) (2 . 3))
                 (eval (quote (loopy-iter (listing i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                                          (adjoining i :test #'= :key #'car)))))))

(ert-deftest adjoin-coercion ()
  (should (equal [1 2 3 4 5]
                 (eval (quote (loopy-iter (listing i '(1 2 2 3 4 4 5))
                                          (adjoining i :result-type 'array))))))

  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy-iter (listing i '(1 2 2 3 4 4 5))
                                          (adjoining i :result-type 'list))))))

  (should (equal "abcd"
                 (eval (quote (loopy-iter (listing i '(?a ?b ?c ?d))
                                          (adjoining i :result-type 'string))))))

  (should (equal [1 2 3]
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (adjoining my-var i :result-type 'vector)
                                          (finally-return my-var))))))

  (should (equal [1 2 3 4 5]
                 (eval (quote (loopy-iter (listing i '(1 2 2 3 4 4 5))
                                          (adjoining i :result-type array))))))

  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy-iter (listing i '(1 2 2 3 4 4 5))
                                          (adjoining i :result-type list))))))

  (should (equal "abcd"
                 (eval (quote (loopy-iter (listing i '(?a ?b ?c ?d))
                                          (adjoining i :result-type string))))))

  (should (equal [1 2 3]
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (adjoining my-var i :result-type vector)
                                          (finally-return my-var)))))))

(ert-deftest adjoin-at ()
  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy-iter (listing i '(1 2 3 4 4 5))
                                          (adjoining i :at end))))))

  (should (equal '(5 4 3 2 1)
                 (eval (quote (loopy-iter (listing i '(1 2 3 4 4 5))
                                          (adjoining i :at start))))))

  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy-iter (flag split)
                                          (listing i '(1 2 3 4 4 5))
                                          (adjoining i :at end))))))

  (should (equal [5 4 3 2 1]
                 (eval (quote (loopy-iter (listing i '(1 2 3 4 4 5))
                                          (adjoining i :at start :result-type 'array))))))

  (should
   (= 1
      (cl-count-if (lambda (x) (= (cl-second x) 4))
                   (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 4)))
                                            (adjoining i
                                                       :at start
                                                       :key #'cl-second)))))))

  (should
   (= 1
      (cl-count-if (lambda (x) (equal (cl-second x) '(4 0)))
                   (eval (quote (loopy-iter (listing i '((1 2)
                                                         (3 (4 0))
                                                         (5 (4 0))))
                                            (adjoining i :at start
                                                       :key #'cl-second
                                                       :test #'equal))))))))

(ert-deftest adjoin-end-tracking ()
  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy-iter (listing i '(1 2 2 3 3 4))
                                          (adjoining i :at end)
                                          (adjoining (1+ i) :at end))))))

  (should (equal '(3 2 1 11 12 13)
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (adjoining i :at start)
                                          (adjoining (+ i 10) :at end))))))

  (should (equal '(3 2 1 11 12 13)
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (adjoining (+ i 10) :at end)
                                          (adjoining i :at start)))))))

(ert-deftest adjoin-not-destructive ()
  (let ((l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10)))
    (loopy-iter (listing i l1) (adjoining coll i :at start))
    (should (equal l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10))))

  (let ((l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10)))
    (loopy-iter (listing i l1) (adjoining coll i :at end))
    (should (equal l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10))))

  (let ((l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10)))
    (loopy-iter (listing i l1) (adjoining i :at start))
    (should (equal l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10))))

  (let ((l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10)))
    (loopy-iter (listing i l1) (adjoining i :at end))
    (should (equal l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10))))

  (let ((l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10)))
    (loopy-iter (flag split) (listing i l1) (adjoining i :at end))
    (should (equal l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10)))))

;;;;; Append
(ert-deftest append ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (appending coll i)
                                          (finally-return coll))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (appending coll i)
                                          (finally-return coll)))))))

(ert-deftest append-destructuring ()
  (should (equal '((1 2 5 6) (3 4 7 8))
                 (eval (quote (loopy-iter (arraying i [((1 2) (3 4)) ((5 6) (7 8))])
                                          (appending (j k) i)
                                          (finally-return j k))))))

  (should (equal '((1 2 5 6) (3 4 7 8))
                 (eval (quote (loopy-iter (arraying i [((1 2) (3 4)) ((5 6) (7 8))])
                                          (appending (j k) i)
                                          (finally-return j k)))))))

(ert-deftest append-implicit ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (appending i))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (appending i)))))))

(ert-deftest append-at ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (appending i :at end))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (flag split)
                                          (listing i '((1 2 3) (4 5 6)))
                                          (appending i :at end))))))

  (should (equal '(4 5 6 1 2 3)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (appending i :at start)))))))

(ert-deftest append-end-tracking ()
  (should (equal '(1 2 8 9 3 4 10 11 6 7 13 14)
                 (loopy-iter (listing i '((1 2) (3 4) (6 7)))
                             (appending i :at end)
                             (appending (mapcar (lambda (x) (+ x 7)) i)
                                        :at end))))

  (should (equal '(6 7 3 4 1 2)
                 (loopy-iter (accum-opt (coll end))
                             (listing i '((1 2) (3 4) (6 7)))
                             (appending coll i :at start)
                             (finally-return coll))))

  (should (equal '(1 2 3 4 6 7)
                 (loopy-iter (accum-opt (coll start))
                             (listing i '((1 2) (3 4) (6 7)))
                             (appending coll i :at end)
                             (finally-return coll))))

  (should (equal '(1 2 3 4 5 6)
                 (loopy-iter (flag split)
                             (listing i '((1 2) (3 4) (5 6)))
                             (appending i :at end))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (appending i :at start)
                                          (appending (mapcar (lambda (x) (+ x 10)) i)
                                                     :at end))))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (appending (mapcar (lambda (x) (+ x 10)) i)
                                                     :at end)
                                          (appending i :at start)))))))

(ert-deftest append-not-destructive ()
  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (listing i l1) (appending coll i :at start))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (listing i l1) (appending coll i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (listing i l1) (appending i :at start))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (listing i l1) (appending i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (flag split) (listing i l1) (appending i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8))))))

;;;;; Collect
(ert-deftest collect ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing j '(1 2 3))
                                          (collecting coll j)
                                          (finally-return coll))))))
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing j '(1 2 3))
                                          (collecting coll j)
                                          (finally-return coll)))))))

;; Make sure that adding to end works correctly.
(ert-deftest collect-end-tracking ()
  (should (equal '(1 8 2 9 3 10 4 11)
                 (loopy-iter (listing i '(1 2 3 4))
                             (collecting coll i :at end)
                             (collecting coll (+ i 7) :at end)
                             (finally-return coll))))

  (should (equal '(1 8 2 9 3 10 4 11)
                 (loopy-iter (listing i '(1 2 3 4))
                             (collecting i :at end)
                             (collecting (+ i 7) :at end))))

  (should (equal '(1 2 3 4)
                 (loopy-iter (flag split)
                             (listing i '(1 2 3 4))
                             (collecting i :at end))))

  (should (equal '(3 2 1 1 2 3)
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (collecting i :at end)
                                          (collecting i :at start))))))

  (should (equal '(3 2 1 1 2 3)
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (collecting i :at start)
                                          (collecting i :at end)))))))



(ert-deftest collect-destructuring ()
  (should (and (equal '((1 4) ((2 3) (5 6)))
                      (eval (quote (loopy-iter (listing j '((1 2 3) (4 5 6)))
                                               (collecting (coll1 . coll2) j)
                                               (finally-return coll1 coll2)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy-iter (listing j '((1 2 3) (4 5 6)))
                                               (collecting (coll1 coll2 coll3) j)
                                               (finally-return coll1 coll2 coll3)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy-iter (listing j '([1 2 3] [4 5 6]))
                                               (collecting [coll1 coll2 coll3] j)
                                               (finally-return coll1 coll2 coll3)))))))
  (should (and (equal '((1 4) ((2 3) (5 6)))
                      (eval (quote (loopy-iter (listing j '((1 2 3) (4 5 6)))
                                               (collecting (coll1 . coll2) j)
                                               (finally-return coll1 coll2)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy-iter (listing j '((1 2 3) (4 5 6)))
                                               (collecting (coll1 coll2 coll3) j)
                                               (finally-return coll1 coll2 coll3)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy-iter (listing j '([1 2 3] [4 5 6]))
                                               (collecting [coll1 coll2 coll3] j)
                                               (finally-return coll1 coll2 coll3))))))))
(ert-deftest collect-implicit ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing j '(1 2 3))
                                          (collecting j))))))
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing j '(1 2 3))
                                          (collecting j)))))))

(ert-deftest collect-coercion ()
  (should (equal [1 2 3]
                 (eval (quote (loopy-iter (listing j '(1 2 3))
                                          (collecting v j :result-type 'vector)
                                          (finally-return v))))))

  (should (equal "abc"
                 (eval (quote (loopy-iter (listing j '(?a ?b ?c))
                                          (collecting j :result-type 'string))))))

  (should (equal [1 2 3]
                 (eval (quote (loopy-iter (listing j '(1 2 3))
                                          (collecting v j :result-type vector)
                                          (finally-return v))))))

  (should (equal "abc"
                 (eval (quote (loopy-iter (listing j '(?a ?b ?c))
                                          (collecting j :result-type string)))))))

(ert-deftest collect-at ()
  (should (equal (list '(3 2 1) '(1 2 3))
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (collecting coll1 i :at 'beginning)
                                          (collecting coll2 (cl-first coll1))
                                          (finally-return coll1 coll2))))))

  (should (equal '(3 2 1)
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (collecting i :at beginning))))))

  (should (equal [3 2 1]
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (collecting coll1 i :at 'start :result-type 'array)
                                          (finally-return coll1))))))

  (should (equal '(3 2 1)
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (collecting i :at start))))))

  (should (equal '(3 2 1)
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (collecting i :at 'start))))))

  (should (equal '((1 2 3) (1 1 1))
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (collecting coll1 i :at 'end)
                                          (collecting coll2 (cl-first coll1))
                                          (finally-return coll1 coll2)))))))

;; This shouldn't ever happen, but it's still worth checking.
(ert-deftest collect-not-destructive ()
  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (listing i l1) (collecting coll i :at start))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (listing i l1) (collecting coll i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (listing i l1) (collecting i :at start))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (listing i l1) (collecting i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (flag split) (listing i l1) (collecting i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8))))))

;;;;; Concat
(ert-deftest concat ()
  (should (equal "catdog"
                 (eval (quote (loopy-iter (listing j '("cat" "dog"))
                                          (concating coll j)
                                          (finally-return coll))))))
  (should (equal "catdog"
                 (eval (quote (loopy-iter (listing j '("cat" "dog"))
                                          (concating coll j)
                                          (finally-return coll)))))))

(ert-deftest concat-destructuring ()
  (should (and (equal '("ad" "be" "cf")
                      (eval (quote (loopy-iter (listing j '(("a" "b" "c") ("d" "e" "f")))
                                               (concating (coll1 coll2 coll3) j)
                                               (finally-return coll1 coll2 coll3)))))

               (equal '("ad" "be" "cf")
                      (eval (quote (loopy-iter (listing j '(["a" "b" "c"] ["d" "e" "f"]))
                                               (concating [coll1 coll2 coll3] j)
                                               (finally-return coll1 coll2 coll3)))))))
  (should (and (equal '("ad" "be" "cf")
                      (eval (quote (loopy-iter (listing j '(("a" "b" "c") ("d" "e" "f")))
                                               (concating (coll1 coll2 coll3) j)
                                               (finally-return coll1 coll2 coll3)))))

               (equal '("ad" "be" "cf")
                      (eval (quote (loopy-iter (listing j '(["a" "b" "c"] ["d" "e" "f"]))
                                               (concating [coll1 coll2 coll3] j)
                                               (finally-return coll1 coll2 coll3))))))))

(ert-deftest concat-implict ()
  (should (equal "catdog"
                 (eval (quote (loopy-iter (listing j '("cat" "dog"))
                                          (concating j))))))
  (should (equal "catdog"
                 (eval (quote (loopy-iter (listing j '("cat" "dog"))
                                          (concating j)))))))

(ert-deftest concat-at ()
  (should (equal "catdog"
                 (eval (quote (loopy-iter (listing j '("cat" "dog"))
                                          (concating j :at end))))))

  (should (equal "dogcat"
                 (eval (quote (loopy-iter (listing j '("cat" "dog"))
                                          (concating j :at start))))))

  (should (equal "catdog"
                 (eval (quote (loopy-iter (listing j '("cat" "dog"))
                                          (concating str j :at end)
                                          (finally-return str))))))

  (should (equal "dogcat"
                 (eval (quote (loopy-iter (listing j '("cat" "dog"))
                                          (concating str j :at start)
                                          (finally-return str))))))

  (should (equal '("ad" "be" "cf")
                 (eval (quote (loopy-iter (listing j '(("a" "b" "c") ("d" "e" "f")))
                                          (concating (coll1 coll2 coll3) j :at end)
                                          (finally-return coll1 coll2 coll3))))))

  (should (equal '("da" "eb" "fc")
                 (eval (quote (loopy-iter (listing j '(("a" "b" "c") ("d" "e" "f")))
                                          (concating (coll1 coll2 coll3) j :at start)
                                          (finally-return coll1 coll2 coll3)))))))

;;;;; Count
(ert-deftest count ()
  (should (= 2
             (eval (quote (loopy-iter (listing i '(t nil t nil))
                                      (counting c i)
                                      (finally-return c))))))
  (should (= 2
             (eval (quote (loopy-iter (listing i '(t nil t nil))
                                      (counting c i)
                                      (finally-return c)))))))

(ert-deftest count-destructuring ()
  (should
   (equal '(2 1)
          (eval (quote (loopy-iter (listing elem '((t nil) (t t)))
                                   (counting (c1 c2) elem)
                                   (finally-return c1 c2))))))
  (should
   (equal '(2 1)
          (eval (quote (loopy-iter (listing elem '((t nil) (t t)))
                                   (counting (c1 c2) elem)
                                   (finally-return c1 c2)))))))

(ert-deftest count-implict ()
  (should (= 2
             (eval (quote (loopy-iter (listing i '(t nil t nil))
                                      (counting i))))))
  (should (= 2
             (eval (quote (loopy-iter (listing i '(t nil t nil))
                                      (counting i)))))))

;;;;; Max
(ert-deftest max ()
  (should (= 11
             (eval (quote (loopy-iter (listing i '(1 11 2 10 3 9 4 8 5 7 6))
                                      (maximizing my-max i)
                                      (finally-return my-max))))))
  )

(ert-deftest max-destructuring ()
  (should
   (equal '(9 11)
          (eval (quote (loopy-iter (listing elem '((1 11) (9 4)))
                                   (maximizing (m1 m2) elem)
                                   (finally-return m1 m2)))))))

(ert-deftest max-implict ()
  (should (= 11
             (eval (quote (loopy-iter (listing i '(1 11 2 10 3 9 4 8 5 7 6))
                                      (maximizing i)))))))

;;;;; Min
(ert-deftest min ()
  (should
   (= 0
      (eval (quote (loopy-iter (listing i '(1 11 2 10 3 0 9 4 8 5 7 6))
                               (minimizing my-min i)
                               (finally-return my-min)))))))

(ert-deftest min-destructuring ()

  (should
   (equal '(1 4)
          (eval (quote (loopy-iter (listing elem '((1 11) (9 4)))
                                   (minimizing (m1 m2) elem)
                                   (finally-return m1 m2)))))))

(ert-deftest min-implict ()

  (should
   (= 0
      (eval (quote (loopy-iter (listing i '(1 11 2 10 3 0 9 4 8 5 7 6))
                               (minimizing i)))))))

;;;;; Multiply
(ert-deftest multiply ()
  (should (= 120 (eval (quote (loopy-iter (listing i '(1 2 3 4 5))
                                          (multiplying product i)
                                          (finally-return product)))))))

(ert-deftest multiply-destructuring ()
  (should (equal '(3 8) (eval (quote (loopy-iter (listing i '((1 2) (3 4)))
                                                 (multiplying (x y) i)
                                                 (finally-return x y)))))))

(ert-deftest multiply-implicit ()
  (should (= 120 (eval (quote (loopy-iter (listing i '(1 2 3 4 5))
                                          (multiplying i)))))))

;;;;; Nconc
(ert-deftest nconc ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i (list (list 1 2 3)
                                                           (list 4 5 6)))
                                          (nconcing l i)
                                          (finally-return l)))))))

(ert-deftest nconc-destructuring ()
  (should
   (equal '((1 4) ((2 3) (5 6)))
          (eval (quote (loopy-iter (listing elem (list (list (list 1) (list 2 3))
                                                       (list (list 4) (list 5 6))))
                                   (nconcing (n1 . n2) elem)
                                   (finally-return n1 n2)))))))

(ert-deftest nconc-implict ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (nconcing i))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (nconcing i)))))))

(ert-deftest nconc-at-literal-lists ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (nconcing i))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (nconcing i :at end))))))

  (should (equal '(4 5 6 1 2 3)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (nconcing i :at start))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (nconcing c i)
                                          (finally-return c))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (nconcing c i :at end)
                                          (finally-return c))))))

  (should (equal '(4 5 6 1 2 3)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (4 5 6)))
                                          (nconcing c i :at start)
                                          (finally-return c)))))))

(ert-deftest nconc-at-new-lists ()

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (let ((l1 (list 1 2 3))
                                    (l2 (list 4 5 6)))
                                (loopy-iter (listing i (list l1 l2))
                                            (nconcing i)))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (let ((l1 (list 1 2 3))
                                    (l2 (list 4 5 6)))
                                (loopy-iter (listing i (list l1 l2))
                                            (nconcing i :at end)))))))

  (should (equal '(4 5 6 1 2 3)
                 (eval (quote (let ((l1 (list 1 2 3))
                                    (l2 (list 4 5 6)))
                                (loopy-iter (listing i (list l1 l2))
                                            (nconcing i :at start)))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (let ((l1 (list 1 2 3))
                                    (l2 (list 4 5 6)))
                                (loopy-iter (listing i (list l1 l2))
                                            (nconcing c i)
                                            (finally-return c)))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (let ((l1 (list 1 2 3))
                                    (l2 (list 4 5 6)))
                                (loopy-iter (listing i (list l1 l2))
                                            (nconcing c i :at end)
                                            (finally-return c)))))))

  (should (equal '(4 5 6 1 2 3)
                 (eval (quote (let ((l1 (list 1 2 3))
                                    (l2 (list 4 5 6)))
                                (loopy-iter (listing i (list l1 l2))
                                            (nconcing c i :at start)
                                            (finally-return c))))))))

(ert-deftest nconc-end-tracking ()
  (should (equal '(1 2 11 12 3 4 13 14 5 6 15 16)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (listing j '((11 12) (13 14) (15 16)))
                                          (nconcing coll i :at end)
                                          (nconcing coll j :at end)
                                          (finally-return coll))))))

  (should (equal '(1 2 11 12 3 4 13 14 5 6 15 16)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (listing j '((11 12) (13 14) (15 16)))
                                          (nconcing i :at end)
                                          (nconcing j :at end))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy-iter (flag split)
                                          (listing i '((1 2) (3 4) (5 6)))
                                          (nconcing i :at end))))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (nconcing (copy-sequence i) :at start)
                                          (nconcing (mapcar (lambda (x) (+ x 10))
                                                            (copy-sequence i))
                                                    :at end))))))

  (should (equal '(5 6 3 4 1 2)
                 (loopy-iter (accum-opt (coll end))
                             (listing i (list (list 1 2) (list 3 4) (list 5 6)))
                             (appending coll i :at start)
                             (finally-return coll))))

  (should (equal '(1 2 3 4 5 6)
                 (loopy-iter (accum-opt (coll start))
                             (listing i (list (list 1 2) (list 3 4) (list 5 6)))
                             (appending coll i :at end)
                             (finally-return coll))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (nconcing (mapcar (lambda (x) (+ x 10))
                                                            (copy-sequence i))
                                                    :at end)
                                          (nconcing (copy-sequence i) :at start)))))))

;;;;; Nunion
(ert-deftest nunion ()
  ;; (should (null (cl-set-difference
  ;;                '(4 1 2 3)
  ;;                (eval (quote (loopy-iter (listing i '((1 2) (2 3) (3 4)))
  ;;                                    (nunioning var i)))))))
  ;; (should (null (cl-set-difference
  ;;                '(4 1 2 3)
  ;;                (eval (quote (loopy-iter (listing i '((1 2) (2 3) (3 4)))
  ;;                                    (nunioning var i)
  ;;                                    (finally-return var)))))))
  ;;
  ;; (should (null (cl-set-difference
  ;;                '(4 2 (1 1) 3)
  ;;                (eval (quote (loopy-iter (listing i '(((1 1) 2) ((1 1) 3) (3 4)))
  ;;                                    (nunioning var i :test #'equal)
  ;;                                    (finally-return var))))
  ;;                :test #'equal)))
  ;;
  ;; ;; The resulting list should only have one element whose `car' is `a'.
  ;; (should (= 1 (cl-count-if (lambda (x) (eq (car x) 'a))
  ;;                           (eval (quote (loopy-iter (arraying i [((a . 1)) ((a . 2))])
  ;;                                               (nunioning var i :key #'car)
  ;;                                               (finally-return var)))))))

  (should (equal
           '(1 2 3 4)
           (eval (quote (loopy-iter (listing i '((1 2) (2 3) (3 4)))
                                    (nunioning var i)
                                    (finally-return var))))))
  (should (equal
           '(1 2 3 4)
           (eval (quote (loopy-iter (listing i '((1 2) (2 3) (3 4)))
                                    (nunioning var i)
                                    (finally-return var))))))

  (should (equal '((1 1) 2 3 4)
                 (eval (quote (loopy-iter (listing i '(((1 1) 2) ((1 1) 3) (3 4)))
                                          (nunioning var i :test #'equal)
                                          (finally-return var))))))

  (should (equal '((1 1) 2 3 4)
                 (eval (quote (let ((test #'equal))
                                (loopy-iter (listing i '(((1 1) 2) ((1 1) 3) (3 4)))
                                            (nunioning var i :test test)
                                            (finally-return var)))))))

  ;; The resulting list should only have one element whose `car' is `a'.
  (should (equal '((a . 1)) (eval (quote (loopy-iter (arraying i [((a . 1)) ((a . 2))])
                                                     (nunioning var i :key #'car)
                                                     (finally-return var))))))

  (should (equal '((a . 1)) (eval (quote (let ((key #'car))
                                           (loopy-iter (arraying i [((a . 1)) ((a . 2))])
                                                       (nunioning var i :key key)
                                                       (finally-return var))))))))

(ert-deftest nunion-destructuring ()
  (should (equal '((1 2 3) (2 3 4))
                 (eval (quote (loopy-iter (arraying i [((1 2) (2 3))
                                                       ((1 2 3) (3 4))])
                                          (nunioning (var1 var2) i :test #'equal)
                                          (finally-return var1 var2)))))))

(ert-deftest nunion-at ()
  (should (equal '((1 2) (3 2) (1 1))
                 (eval (quote (loopy-iter (listing i '(((1 2) (3 2)) ((1 1) (4 2))))
                                          (nunioning i :at end :key #'cl-second))))))

  (should (equal '((1 2) (3 2) (4 2))
                 (eval (quote (loopy-iter (listing i '(((1 2) (3 2)) ((1 1) (4 2))))
                                          (nunioning i :at end :key #'car))))))

  (should (equal '((4 2) (1 2) (3 2))
                 (eval (quote (loopy-iter (listing i '(((1 2) (3 2)) ((1 1) (4 2))))
                                          (nunioning i :at start :key #'car))))))

  (should (equal '((4 2) (1 2) (3 2))
                 (eval (quote (loopy-iter (listing i '(((1 2) (3 2)) ((1 1) (4 2))))
                                          (nunioning c i :at start :key #'car)
                                          (finally-return c))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (1 2 3)))
                                          (nunioning i :test #'equal :at start))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (1 2 3)))
                                          (nunioning c i :test #'equal :at start)
                                          (finally-return c)))))))

(ert-deftest nunion-end-tracking ()
  (should (equal '(1 2 3 4 5 6 7 8 9 10)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (1 2 3) (4 5 6) (7 8 9)))
                                          (nunioning coll i)
                                          (nunioning coll (mapcar #'1+ i))
                                          (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6 7 8 9)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (1 2 3) (4 5 6) (7 8 9)))
                                          (nunioning i :at end))))))

  (should (equal '(1 2 3 4 5 6 7 8 9)
                 (eval (quote (loopy-iter (flag split)
                                          (listing i '((1 2 3) (1 2 3) (4 5 6) (7 8 9)))
                                          (nunioning i :at end))))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                             (nunioning (copy-sequence i) :at start)
                             (nunioning (mapcar (lambda (x) (+ x 10)) (copy-sequence i))
                                        :at end))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                             (nunioning (mapcar (lambda (x) (+ x 10)) (copy-sequence i))
                                        :at end)
                             (nunioning (copy-sequence i) :at start)))))

;;;;; Prepend
(ert-deftest prepend ()
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (prepending my-list i)
                                          (finally-return my-list))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (prepending my-list i)
                                          (finally-return my-list)))))))

(ert-deftest prepend-destructuring ()
  (should (equal '((5 6 1 2) (7 8 3 4))
                 (eval (quote (loopy-iter (listing i '([(1 2) (3 4)] [(5 6) (7 8)]))
                                          (prepending [my-list1 my-list2] i)
                                          (finally-return my-list1 my-list2))))))
  (should (equal '((5 6 1 2) (7 8 3 4))
                 (eval (quote (loopy-iter (listing i '([(1 2) (3 4)] [(5 6) (7 8)]))
                                          (prepending [my-list1 my-list2] i)
                                          (finally-return my-list1 my-list2)))))))

(ert-deftest prepend-implicit ()
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (prepending i)
                                          (finally-return loopy-result))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (prepending i)
                                          (finally-return loopy-result))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (prepending i))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                                          (prepending i)))))))


;;;;; Push Into
(ert-deftest push-into ()

  (should (equal '(3 2 1)
                 (eval (quote (loopy-iter (listing j '(1 2 3))
                                          (pushing-into coll j)
                                          (finally-return coll))))))

  (should (equal '(3 2 1)
                 (eval (quote (loopy-iter (listing j '(1 2 3))
                                          (pushing coll j)
                                          (finally-return coll)))))))

(ert-deftest push-into-destructuring ()

  (should (equal '((5 3 1) (6 4 2))
                 (eval (quote (loopy-iter (listing elem '((1 2) (3 4) (5 6)))
                                          (pushing-into (p1 p2) elem)
                                          (finally-return p1 p2))))))
  (should (equal '((5 3 1) (6 4 2))
                 (eval (quote (loopy-iter (listing elem '((1 2) (3 4) (5 6)))
                                          (pushing (p1 p2) elem)
                                          (finally-return p1 p2)))))))

;;;;; Reduce
(ert-deftest reduce ()
  (should (= 6
             (eval (quote (loopy-iter (listing i '(1 2 3))
                                      (reducing r i #'+ :init 0)
                                      (finally-return r))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing i '((1) (2) (3)))
                                          (reducing r i #'append)
                                          (finally-return r))))))

  (should (equal '(1 2 3)
                 (eval (quote (let ((func #'append))
                                (loopy-iter (listing i '((1) (2) (3)))
                                            (reducing r i func)
                                            (finally-return r)))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing i '((1) (2) (3)))
                                          (reducing r i #'append)
                                          (finally-return r)))))))

(ert-deftest reduce-destructuring ()
  (should (equal '(4 6)
                 (eval (quote (loopy-iter (listing i '((1 2) (3 4)))
                                          (reducing (r1 r2) i #'+ :init 0)
                                          (finally-return r1 r2))))))

  (should (equal '((1 3) (2 4))
                 (eval (quote (loopy-iter (listing i '([(1) (2)] [(3) (4)]))
                                          (reducing [r1 r2] i #'append)
                                          (finally-return r1 r2))))))

  (should (equal '((1 3) (2 4))
                 (eval (quote (loopy-iter (listing i '([(1) (2)] [(3) (4)]))
                                          (reducing [r1 r2] i #'append)
                                          (finally-return r1 r2)))))))

(ert-deftest reduce-implicit ()
  (should (= 6
             (eval (quote (loopy-iter (listing i '(1 2 3))
                                      (reducing i #'+ :init 0))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing i '((1) (2) (3)))
                                          (reducing i #'append))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing i '((1) (2) (3)))
                                          (reducing i #'append)))))))
;;;;; Sum
(ert-deftest sum ()
  (should (= 6
             (eval (quote (loopy-iter (listing i '(1 2 3))
                                      (summing s i)
                                      (finally-return s))))))
  (should (= 6
             (eval (quote (loopy-iter (listing i '(1 2 3))
                                      (summing s i)
                                      (finally-return s)))))))

(ert-deftest sum-destructuring ()
  (should (equal '(5 7 9)
                 (loopy-iter (listing elem '((1 2 3) (4 5 6)))
                             (summing (sum1 sum2 sum3) elem)
                             (finally-return sum1 sum2 sum3))))
  (should (equal '(5 7 9)
                 (loopy-iter (listing elem '((1 2 3) (4 5 6)))
                             (summing (sum1 sum2 sum3) elem)
                             (finally-return sum1 sum2 sum3)))))

(ert-deftest sum-implict ()
  (should (= 6
             (eval (quote (loopy-iter (listing i '(1 2 3))
                                      (summing i))))))
  (should (= 6
             (eval (quote (loopy-iter (listing i '(1 2 3))
                                      (summing i)))))))

;;;;; Union
(ert-deftest union ()
  ;; TODO: `union' currently has predictable behavior due to the `:at' position,
  ;;       but it might be worthwhile to remove that predictability for speed in
  ;;       the future.
  ;;
  ;; (should (null (cl-set-difference
  ;;                '(4 1 2 3)
  ;;                (eval (quote (loopy-iter (listing i '((1 2) (2 3) (3 4)))
  ;;                                    (unioning var i)))))))
  ;; (should (null (cl-set-difference
  ;;                '(4 1 2 3)
  ;;                (eval (quote (loopy-iter (listing i '((1 2) (2 3) (3 4)))
  ;;                                    (unioning var i)
  ;;                                    (finally-return var)))))))
  ;;
  ;; (should (null (cl-set-difference
  ;;                '(4 2 (1 1) 3)
  ;;                (eval (quote (loopy-iter (listing i '(((1 1) 2) ((1 1) 3) (3 4)))
  ;;                                    (unioning var i :test #'equal)
  ;;                                    (finally-return var))))
  ;;                :test #'equal)))
  ;;
  ;; ;; The resulting list should only have one element whose `car' is `a'.
  ;; (should (= 1 (cl-count-if (lambda (x) (eq (car x) 'a))
  ;;                           (eval (quote (loopy-iter (arraying i [((a . 1)) ((a . 2))])
  ;;                                               (unioning var i :key #'car)
  ;;                                               (finally-return var)))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy-iter (listing i '((1 2) (2 3) (3 4)))
                                          (unioning var i)
                                          (finally-return var))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy-iter (listing i '((1 2) (2 3) (3 4)))
                                          (unioning var i)
                                          (finally-return var))))))

  (should (equal '((1 1) 2 3 4)
                 (eval (quote (loopy-iter (listing i '(((1 1) 2) ((1 1) 3) (3 4)))
                                          (unioning var i :test #'equal)
                                          (finally-return var))))))

  (should (equal '((1 1) 2 3 4)
                 (eval (quote (let ((func #'equal ))
                                (loopy-iter (listing i '(((1 1) 2) ((1 1) 3) (3 4)))
                                            (unioning var i :test func)
                                            (finally-return var)))))))

  (should (equal '((a . 1))
                 (eval (quote (loopy-iter (arraying i [((a . 1)) ((a . 2))])
                                          (unioning var i :key #'car)
                                          (finally-return var))))))

  (should (equal '((a . 1))
                 (eval (quote (let ((func #'car))
                                (loopy-iter (arraying i [((a . 1)) ((a . 2))])
                                            (unioning var i :key func)
                                            (finally-return var))))))))

(ert-deftest union-destructuring ()
  ;; TODO: `union' currently has predictable behavior due to the `:at' position,
  ;;       but it might be worthwhile to remove that predictability for speed in
  ;;       the future.
  ;;
  ;; (should (null (cl-destructuring-bind (first second)
  ;;                   (eval (quote (loopy-iter (arraying i [((1 2) (2 3))
  ;;                                                 ((1 2 3) (3 4))])
  ;;                                       (unioning (var1 var2) i :test #'equal))))
  ;;                 (or (clsetdifference first '(1 2 3))
  ;;                     (clsetdifference second '(2 3 4))))))
  (should (equal '((1 2 3) (2 3 4))
                 (eval (quote (loopy-iter (arraying i [((1 2) (2 3))
                                                       ((1 2 3) (3 4))])
                                          (unioning (var1 var2) i :test #'=)
                                          (finally-return var1 var2)))))))

(ert-deftest union-at ()
  (should (equal '((1 2) (3 2) (1 1))
                 (eval (quote (loopy-iter (listing i '(((1 2) (3 2)) ((1 1) (4 2))))
                                          (unioning i :at end :key #'cl-second))))))

  (should (equal '((1 2) (3 2) (4 2))
                 (eval (quote (loopy-iter (listing i '(((1 2) (3 2)) ((1 1) (4 2))))
                                          (unioning i :at end :key #'car))))))

  (should (equal '((4 2) (1 2) (3 2))
                 (eval (quote (loopy-iter (listing i '(((1 2) (3 2)) ((1 1) (4 2))))
                                          (unioning i :at start :key #'car))))))

  (should (equal '((4 2) (1 2) (3 2))
                 (eval (quote (loopy-iter (listing i '(((1 2) (3 2)) ((1 1) (4 2))))
                                          (unioning c i :at start :key #'car)
                                          (finally-return c))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (1 2 3)))
                                          (unioning i :test #'equal :at start))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (listing i '((1 2 3) (1 2 3)))
                                          (unioning c i :test #'equal :at start)
                                          (finally-return c)))))))

(ert-deftest union-end-tracking ()
  (should (equal '(1 2 3 4 5 6 7 8)
                 (eval (quote
                        (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
                          (loopy-iter (flag split)
                                      (listing i l1)
                                      (unioning i :at end)))))))

  (should (equal '(1 2 3 4 5 6 7 8)
                 (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
                   (loopy-iter (flag -split)
                               (listing i l1)
                               (unioning i :at end)))))

  (should (equal '(1 2 3 4 5 6)
                 (let ((l1 (list (list 1 2) (list 3 4) (list 4 3) (list 5 6))))
                   (loopy-iter (listing i l1)
                               (unioning coll i :at end)
                               (finally-return coll)))))

  (should (equal '(1 2 3 4 5 6 7)
                 (let ((l1 (list (list 1 2) (list 3 4) (list 4 3) (list 5 6))))
                   (loopy-iter (listing i l1)
                               (unioning coll i :at end)
                               (unioning coll (mapcar #'1+ i) :at end)
                               (finally-return coll)))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                             (unioning i :at start)
                             (unioning (mapcar (lambda (x) (+ x 10)) i)
                                       :at end))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (loopy-iter (listing i '((1 2) (3 4) (5 6)))
                             (unioning (mapcar (lambda (x) (+ x 10)) i)
                                       :at end)
                             (unioning i :at start)))))

(ert-deftest union-not-destructive ()
  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (listing i l1) (unioning coll i :at start))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (listing i l1) (unioning coll i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (listing i l1) (unioning i :at start))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (should (equal (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
                   (loopy-iter (listing i l1) (unioning i :at end))
                   l1)
                 '((1 2) (3 4) (5 6) (7 8))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy-iter (flag split) (listing i l1) (unioning i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8))))))

;;;;; Vconcat
(ert-deftest vconcat ()
  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy-iter (listing elem '([1 2 3 4 5 6]
                                                          [7 8 9 10 11 12]))
                                          (vconcating v elem)
                                          (finally-return v))))))

  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy-iter (listing elem '([1 2 3 4 5 6]
                                                          [7 8 9 10 11 12]))
                                          (vconcating v elem)
                                          (finally-return v)))))))

(ert-deftest vconcat-destructuring ()
  (should (equal '([1 2 3 7 8 9] [4 5 6 10 11 12])
                 (eval (quote (loopy-iter (listing elem '(([1 2 3] [4 5 6])
                                                          ([7 8 9] [10 11 12])))
                                          (vconcating (v1 v2) elem)
                                          (finally-return v1 v2))))))

  (should (equal '([1 2 3 7 8 9] [4 5 6 10 11 12])
                 (eval (quote (loopy-iter (listing elem '(([1 2 3] [4 5 6])
                                                          ([7 8 9] [10 11 12])))
                                          (vconcating (v1 v2) elem)
                                          (finally-return v1 v2)))))))

(ert-deftest vconcat-implict ()
  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy-iter (listing elem '([1 2 3 4 5 6]
                                                          [7 8 9 10 11 12]))
                                          (vconcating elem))))))
  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy-iter (listing elem '([1 2 3 4 5 6]
                                                          [7 8 9 10 11 12]))
                                          (vconcating elem)))))))

(ert-deftest vconcat-at ()
  (should (equal [7 8 9 10 11 12 1 2 3 4 5 6]
                 (eval (quote (loopy-iter (listing elem '([1 2 3 4 5 6]
                                                          [7 8 9 10 11 12]))
                                          (vconcating v elem :at start)
                                          (finally-return v))))))

  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy-iter (listing elem '([1 2 3 4 5 6]
                                                          [7 8 9 10 11 12]))
                                          (vconcating v elem :at end)
                                          (finally-return v))))))

  (should (equal [7 8 9 10 11 12 1 2 3 4 5 6]
                 (eval (quote (loopy-iter (listing elem '([1 2 3 4 5 6]
                                                          [7 8 9 10 11 12]))
                                          (vconcating elem :at start))))))

  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy-iter (listing elem '([1 2 3 4 5 6]
                                                          [7 8 9 10 11 12]))
                                          (vconcating elem :at end))))))

  (should (equal '([1 2 3 7 8 9] [4 5 6 10 11 12])
                 (eval (quote (loopy-iter (listing elem '(([1 2 3] [4 5 6])
                                                          ([7 8 9] [10 11 12])))
                                          (vconcating (v1 v2) elem :at end)
                                          (finally-return v1 v2))))))

  (should (equal '([7 8 9 1 2 3] [10 11 12 4 5 6])
                 (eval (quote (loopy-iter (listing elem '(([1 2 3] [4 5 6])
                                                          ([7 8 9] [10 11 12])))
                                          (vconcating (v1 v2) elem :at start)
                                          (finally-return v1 v2)))))))

;;;;; Miscellaneous
(ert-deftest accumulation-recursive-destructuring ()
  (should
   (and
    (equal '(4 6 8)
           (eval (quote (loopy-iter (listing i '((1 (2 3)) (3 (4 5))))
                                    (summing (s1 (s2 s3)) i)
                                    (finally-return s1 s2 s3)))))
    (equal '(4 6 8)
           (eval (quote (loopy-iter (listing i '((1 (2 . 3)) (3 (4 . 5))))
                                    (summing (s1 (s2 . s3)) i)
                                    (finally-return s1 s2 s3)))))
    (equal '(4 6 8)
           (eval (quote (loopy-iter (arraying i [[1 [2 3]] [3 [4 5]]])
                                    (summing [s1 [s2 s3]] i)
                                    (finally-return s1 s2 s3)))))
    (equal '(4 6 8 10)
           (eval (quote (loopy-iter (listing i '((1 (2 . [3 4])) (3 (4 . [5 6]))))
                                    (summing (s1 (s2 . [s3 s4])) i)
                                    (finally-return s1 s2 s3 s4)))))
    (equal '((1 3) (2 4) (3 5) (4 6))
           (eval (quote (loopy-iter (listing i '((1 (2 . [3 4])) (3 (4 . [5 6]))))
                                    (collecting (c1 (c2 . [c3 c4])) i)
                                    (finally-return c1 c2 c3 c4))))))))

(ert-deftest accumulation-array-destructuring ()
  (should (equal '((1 4) (2 5) (3 6))
                 (eval (quote (loopy-iter (listing elem '([1 2 3] [4 5 6]))
                                          (collecting [i j k] elem)
                                          (finally-return i j k))))))

  (should (equal '(([1 2 3] [4 5 6]) (1 4) (2 5) (3 6))
                 (eval (quote (loopy-iter (listing elem '([1 2 3] [4 5 6]))
                                          (collecting [&whole whole i j k] elem)
                                          (finally-return whole i j k))))))

  (should (equal '((1 4) (3 6))
                 (eval (quote (loopy-iter (listing elem '([1 2 3] [4 5 6]))
                                          (collecting [i _ k] elem)
                                          (finally-return i k))))))

  (should (equal '((1 4) ([2 3] [5 6]))
                 (eval (quote (loopy-iter (listing elem '([1 2 3] [4 5 6]))
                                          (collecting [i &rest k] elem)
                                          (finally-return i  k))))))

  (should (equal '((1 4) (2 5) (3 6))
                 (eval (quote (loopy-iter (listing elem '([1 2 3] [4 5 6]))
                                          (collecting [i &rest [j k]] elem)
                                          (finally-return i j k)))))))

(ert-deftest accumulation-list-destructuring ()
  (should (equal '((1 4) (2 5) (3 6))
                 (eval (quote (loopy-iter (listing elem '((1 2 3) (4 5 6)))
                                          (collecting (i j k) elem)
                                          (finally-return i j k))))))

  (should (equal '((1 4) (3 6))
                 (eval (quote (loopy-iter (listing elem '((1 2 3) (4 5 6)))
                                          (collecting (i _ k) elem)
                                          (finally-return i k))))))

  (should (equal '(((1 2 3) (4 5 6)) (1 4) (2 5) (3 6))
                 (eval (quote (loopy-iter (listing elem '((1 2 3) (4 5 6)))
                                          (collecting (&whole tot i j k) elem)
                                          (finally-return tot i j k))))))

  (should (equal '((1 4) (2 5) (3 6))
                 (eval (quote (loopy-iter (listing elem '((1 2 3) (4 5 6)))
                                          (collecting (i &rest (j k)) elem)
                                          (finally-return i j k))))))

  (should (equal '((1 4) ((2 3) (5 6)))
                 (eval (quote (loopy-iter (listing elem '((1 2 3) (4 5 6)))
                                          (collecting (i . j) elem)
                                          (finally-return i j))))))

  (should (equal '((1 4) (2 5) (3 6) (4 27) (5 8))
                 (eval (quote (loopy-iter (listing elem '((1 2 3 :k1 4 :k2 5) (4 5 6 :k2 8)))
                                          (collecting (i j k &key (k1 27) k2) elem)
                                          (finally-return i j k k1 k2))))))

  (should (equal '((1 4) (2 5) (3 6) ((:k1 4 :k2 5) (:k2 8)) (4 27) (5 8))
                 (eval (quote (loopy-iter (listing elem '((1 2 3 :k1 4 :k2 5) (4 5 6 :k2 8)))
                                          (collecting (i j k &rest rest &key (k1 27) k2) elem)
                                          (finally-return i j k rest k1 k2))))))

  (should (equal '((1 4) (2 5) (3 6) ((:k1 4 :k2 5) (:k2 8)) (4 27) (5 8))
                 (eval (quote (loopy-iter (listing elem '((1 2 3 :k1 4 :k2 5) (4 5 6 :k2 8)))
                                          (collecting (i j k &key (k1 27) k2 &rest rest) elem)
                                          (finally-return i j k rest k1 k2))))))

  (should (equal '((1 4) (2 5) (3 6) ((:k1 4 :k2 5) (:k2 8)) (4 27) (5 8))
                 (eval (quote (loopy-iter (listing elem '((1 2 3 :k1 4 :k2 5) (4 5 6 :k2 8)))
                                          (collecting (i j k &key (k1 27) k2 . rest) elem)
                                          (finally-return i j k rest k1 k2)))))))

;;; Control Flow
;;;; Conditionals
;;;;; If
(ert-deftest if ()
  (should (equal '((2 4) (1 3))
                 (loopy-iter (listing i '(1 2 3 4))
                             (if (cl-evenp i)
                                 (collecting evens i)
                               (collecting odds i))
                             (finally-return evens odds)))))

;;;;; When
;; (ert-deftest basic-when-parse ()
;;   (should (equal (loopy--parse-conditional-forms 'when 't '((+ 1 1)))
;;                  '((loopy--main-body when t (progn (+ 1 1)))))))

(ert-deftest recursive-when-test ()
  (should (equal
           (eval (quote (loopy-iter (listing i (number-sequence 1 10))
                                    (listing j '(1 2 3 6 7 8))
                                    (when (cl-evenp i)
                                      (when (> j i)
                                        (returning (cons j i)))))))
           '(6 . 4))))

(ert-deftest when-multiple-subcommands ()
  (should (equal '(2 (1 3))
                 (loopy-iter (with (counter 0))
                             (listing i '(1 2 3))
                             (when (cl-oddp i)
                               (collecting odds i)
                               (cl-incf counter))
                             (finally-return counter odds)))))

(ert-deftest multi-when-prepend-test ()
  (should
   (string=
    (eval (quote (loopy-iter (with (first-var 2)
                                   (second-var 3))
                             (sequencing el [1 2 3 4 5 6 7])
                             ;; Could also use (cond ...).
                             (when (zerop (mod el first-var))
                               (pushing-into msg-coll (format "Multiple of 2: %d" el)))
                             (when (zerop (mod el second-var))
                               (pushing-into msg-coll (format "Multiple of 3: %d" el)))
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
    (eval (quote (loopy-iter (with (first-var 2)
                                   (second-var 3))
                             (sequencing el [1 2 3 4 5 6 7])
                             ;; Could also use (cond ...).
                             (unless (zerop (mod el first-var))
                               (pushing  msg-coll (format "Not multiple of 2: %d" el)))
                             (unless (zerop (mod el second-var))
                               (pushing  msg-coll (format "Not multiple of 3: %d" el)))
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
;;                                             (message "hi"))
;;                                            ((= b 2)
;;                                             (returning 5))))
;;                  '((loopy--main-body cond
;;                                      ((= a 1) (progn (message "hi")))
;;                                      ((= b 2) (cl-return-from nil 5)))))))

(ert-deftest cond ()
  (should (equal (eval
                  (quote
                   (loopy-iter (listing i (number-sequence 0 5))
                               (cond ((cl-evenp i)
                                      (pushing  evens i)
                                      (pushing  holding-list evens))
                                     (t (pushing  odds i)))
                               (finally-return (list evens odds holding-list)))))
                 '((4 2 0) (5 3 1) ((4 2 0) (2 0) (0))))))

;;;; Exiting the Loop Early
;;;;; Leave
(ert-deftest leave ()
  (should (equal '(1)
                 (eval (quote (loopy-iter (listing i '(1 2))
                                          (collecting i)
                                          (leaving)))))))

;;;;; Leave From
(ert-deftest leave-from ()
  (should (equal '([1 2 3])
                 (eval (quote (loopy-iter outer
                                          (listing i '([1 2 3] [4 5 6]))
                                          (looping (arraying j i)
                                                   (when (= j 5)
                                                     (leaving-from outer)))
                                          (collecting i)))))))

;;;;; Return
(ert-deftest return ()
  (should (= 6 (eval (quote (loopy-iter (with  (j 0))
                                        (cl-incf j)
                                        (when (> j 5)
                                          (returning j))))))))

;;;;; Return From
(ert-deftest return-from-single-loop ()
  (should (= 6
             (eval (quote (loopy-iter my-loop
                                      (listing i (number-sequence 1 10))
                                      (when (> i 5)
                                        (returning-from my-loop i))))))))

(ert-deftest return-from-outer-loop ()
  (should
   (= 6
      (eval (quote (loopy-iter outer
                               ;; Could use ‘sum’ command, but don’t want dependencies.
                               (with (sum 0))
                               (listing sublist '((1 2 3 4 5) (6 7 8 9) (10 11)))
                               (loopy-iter (listing i sublist)
                                           (setq sum (+ sum i))
                                           (when (> sum 15)
                                             (returning-from outer i)))))))))

(ert-deftest return-commands-multiple-values ()
  (should
   (and
    (equal '(1 2 3 4)
           (eval (quote (loopy-iter (returning 1 2 3 4)))))
    (equal '(1 2 3 4)
           (eval (quote (loopy-iter my-loop (returning-from my-loop 1 2 3 4))))))))

;;;;; Skip
(ert-deftest skip ()
  (should (cl-every #'cl-oddp
                    (eval (quote (loopy-iter (sequencing i (number-sequence 1 10))
                                             (when (cl-evenp i)
                                               (skipping))
                                             (pushing  my-collection i)
                                             (finally-return (nreverse my-collection))))))))

(ert-deftest skip-from ()
  (should (equal '((1 2 3) (7 8 9))
                 (loopy-iter outer
                             (arraying i [(1 2 3) (4 5 6) (7 8 9)])
                             (looping (listing j i)
                                      (if (= 5 j)
                                          (skipping-from outer)))
                             (collecting i)))))

;;;;; Always
(ert-deftest always ()
  (should (equal t (eval (quote (loopy-iter (listing i '(1 2 3 4 5 6))
				            (always (< i 7)))))))

  (should (null
	   (eval (quote (loopy-iter (listing i '(1 2 3 4 5 6))
			            (always (> i 7))))))))

(ert-deftest multiple-always ()
  (should (equal t (eval (quote (loopy-iter (listing i '(1 3 5 7))
                                            (always (cl-oddp i))
                                            (always (< i 10))))))))

;;;;; Never
(ert-deftest never ()
  (should (equal nil
		 (eval (quote (loopy-iter (listing i '(1 2 3 4 5 6))
			                  (never (> i 0)))))))

  (should (equal t
		 (eval (quote (loopy-iter (listing i '(1 2 3 4 5 6))
			                  (never (< i 0))))))))

(ert-deftest multiple-never ()
  (should (equal t (eval (quote (loopy-iter (listing i '(1 3 5 7))
                                            (never (cl-evenp i))
                                            (never (> i 10))))))))

(ert-deftest always-and-never ()
  ;; A `never' command should not stop `always' from ultimately setting the
  ;; return value to 2.
  (should (= 2
             (eval (quote (loopy-iter (cycling 2)
                                      (always 2)
                                      (never nil)))))))



;;;;; Thereis
(ert-deftest thereis ()
  (should (= 6 (eval (quote (loopy-iter (listing i '(1 2 3 4 5 6))
			                (thereis (and (> i 5) i)))))))

  (should (= 9 (eval (quote (loopy-iter (listing i (number-sequence 1 9))
			                (thereis (and (> i 8) i)))))))

  (should (null (eval (quote (loopy-iter (listing i '(1 2 3 4 5 6))
			                 (thereis (> i 7))))))))

;; finding
(ert-deftest find ()
  (should (= 3 (eval (quote (loopy-iter (listing i '(1 2 3))
			                (finding i (> i 2)))))))

  (should-not (eval (quote (loopy-iter (listing i '(1 2 3))
			               (finding i (> i 4))))))

  (should (= 0 (eval (quote (loopy-iter (listing i '(1 2 3))
			                (finding i (> i 4) :on-failure 0))))))

  (should (= 3 (eval (quote (loopy-iter (listing i '(1 2 3))
			                (finding i (> i 2)))))))

  (should-not (eval (quote (loopy-iter (listing i '(1 2 3))
			               (finding i (> i 4))))))

  (should (= 0 (eval (quote (loopy-iter (listing i '(1 2 3))
			                (finding i (> i 4) :on-failure 0))))))

  (should (= 2 (eval (quote (loopy-iter (listing i '(1 2 3))
                                        (finding found i (= i 2))
                                        (finally-return found))))))

  (should (equal "not found"
                 (eval (quote (loopy-iter (listing i '(1 2 3))
                                          (finding whether-found i (> i 4)
                                                   :on-failure "not found")
                                          (finally-return whether-found)))))))

;;; Custom Commands
(ert-deftest custom-command-sum ()
  (let ((loopy-command-parsers
         (map-insert loopy-command-parsers 'target-sum #'my-loopy-sum-command))
        (loopy-iter-bare-commands
         (cons 'target-sum (copy-sequence loopy-iter-bare-commands))))

    (cl-defun my-loopy-sum-command ((_ target &rest items))
      "Set TARGET to the sum of ITEMS."
      `((loopy--iteration-vars (,target nil))
        (loopy--main-body (setq ,target (apply #'+ (list ,@items))))))

    (should (= 6
               (eval (quote (loopy-iter (target-sum my-target 1 2 3)
                                        (returning nil)
                                        (finally-return my-target))))))))

;; NOTE: Also tests that post-conditions work as expected.
(ert-deftest custom-command-always ()
  (let ((loopy-command-parsers
         (map-insert loopy-command-parsers
                     'always #'my--loopy-always-command-parser)))

    (cl-defun my--loopy-always-command-parser ((_ &rest conditions))
      "Parse a command of the form `(always [CONDITIONS])'.
     If any condition is `nil', `loopy' should immediately return nil.
     Otherwise, `loopy' should return t."
      (let (instructions)
        ;; Return t if loop completes successfully.
        (push `(loopy--after-do (cl-return t)) instructions)
        ;; Check all conditions at the end of the loop body, forcing an exit if any
        ;; evaluate to nil.  Since the default return value of the macro is nil, we
        ;; don’t need to do anything else.
        ;;
        ;; NOTE: We must not add anything to `loopy--final-return', since that
        ;;       would override the value of any early returns.
        (dolist (condition conditions)
          (push `(loopy--post-conditions ,condition) instructions))
        instructions))

    ;; One condition: => t
    (should (and
             (eval (quote
                    (loopy-iter (listing i (number-sequence 1 9)) (always (< i 10)))))

             ;; Two conditions: => nil
             (not (eval (quote
                         (loopy-iter (listing i (number-sequence 1 9))
                                     (listing j '(2 4 6 8 9))
                                     (always (< i 10) (cl-evenp j))))))))))

;;; Repeated evaluation of macro

;; This was an odd case reported by a user. See:
;; https://github.com/okamsn/loopy/issues/17
(ert-deftest evaluate-function-twice ()
  (should
   (progn
     (defun mu4e:other-path ()
       "Return load-path for mu4e.
This assumes that you're on guix."
       (loopy-iter (with (regexp "Documents")
	                 (base-dir (expand-file-name "~/")))
	           (listing file (directory-files base-dir))
	           (setting full-path (expand-file-name file base-dir))))
     (mu4e:other-path)
     ;; If an `nreverse' goes bad, then the function value of `mu4e:other-path'
     ;; might be changed (somehow), which causes an error.
     (eq nil (mu4e:other-path)))))

;;; Custom Aliases
(ert-deftest custom-alias-flag ()
  (let ((loopy-aliases (map-copy loopy-aliases))
        (loopy-iter-bare-special-macro-arguments
         (copy-sequence loopy-iter-bare-special-macro-arguments)))
    (loopy-defalias f flag)
    (cl-pushnew 'f loopy-iter-bare-special-macro-arguments)
    (should (equal '((1) (2))
                   (eval (quote (loopy-iter (f split)
                                            (listing i '(1))
                                            (collecting i)
                                            (collecting (1+ i)))))))))

(ert-deftest custom-aliases-with ()
  (let ((loopy-aliases (map-copy loopy-aliases))
        (loopy-iter-bare-special-macro-arguments
         (copy-sequence loopy-iter-bare-special-macro-arguments)))
    (loopy-defalias as with)
    (cl-pushnew 'as loopy-iter-bare-special-macro-arguments)
    (should (= 1
               (eval (quote (loopy-iter (as (a 1))
                                        (returning a))))))))

(ert-deftest custom-aliases-without ()
  (eval (quote (let ((loopy-aliases (map-copy loopy-aliases))
                     (loopy-iter-bare-special-macro-arguments
                      (copy-sequence loopy-iter-bare-special-macro-arguments)))
                 (loopy-defalias 'ignore 'without)
                 (cl-pushnew 'ignore loopy-iter-bare-special-macro-arguments)
                 (should (= 5 (let ((a 1)
                                    (b 2))
                                (loopy-iter (ignore a b)
                                            (cycling 1)
                                            (setting a 2)
                                            (setting b 3))
                                (+ a b))))))))

(ert-deftest custom-aliases-before-do ()
  (eval (quote (let ((loopy-aliases (map-copy loopy-aliases))
                     (loopy-iter-bare-special-macro-arguments
                      (copy-sequence loopy-iter-bare-special-macro-arguments)))
                 (loopy-defalias 'precode 'before-do)
                 (cl-pushnew 'precode loopy-iter-bare-special-macro-arguments)
                 (should (= 7 (loopy-iter (with (i 2))
                                          (precode (setq i 7))
                                          (returning i))))))))

(ert-deftest custom-aliases-after-do ()
  (eval (quote (let ((loopy-aliases (map-copy loopy-aliases))
                     (loopy-iter-bare-special-macro-arguments
                      (copy-sequence loopy-iter-bare-special-macro-arguments)))
                 (loopy-defalias postcode after-do)
                 (cl-pushnew 'postcode loopy-iter-bare-special-macro-arguments)
                 (should (loopy-iter (with (my-ret nil))
                                     (listing i '(1 2 3 4))
                                     (postcode (setq my-ret t))
                                     (finally-return my-ret)))))))

(ert-deftest custom-aliases-finally-do ()
  (eval (quote (let ((loopy-aliases (map-copy loopy-aliases))
                     (loopy-iter-bare-special-macro-arguments
                      (copy-sequence loopy-iter-bare-special-macro-arguments))                     )
                 (cl-pushnew 'fd loopy-iter-bare-special-macro-arguments)
                 (loopy-defalias 'fd finally-do)
                 (should
                  (= 10
                     (let (my-var)
                       (loopy-iter (listing i (number-sequence 1 10))
                                   (fd (setq my-var i)))
                       my-var)))))))

(ert-deftest custom-aliases-finally-return ()
  (eval (quote (let ((loopy-aliases  (map-copy loopy-aliases))
                     (loopy-iter-bare-special-macro-arguments
                      (copy-sequence loopy-iter-bare-special-macro-arguments)))
                 (loopy-defalias fr 'finally-return)
                 (cl-pushnew 'fr loopy-iter-bare-special-macro-arguments)
                 (should (= 10
                            (loopy-iter (listing i (number-sequence 1 10))
                                        (fr i))))))))

(ert-deftest custom-aliases-list ()
  (let ((loopy-aliases (map-copy loopy-aliases))
        (loopy-iter-bare-commands (copy-sequence loopy-iter-bare-commands)))
    (should (progn
              (loopy-defalias l list)
              t))
    (should (progn
              (loopy-defalias a 'array)
              t))
    (cl-pushnew 'a loopy-iter-bare-commands)
    (cl-pushnew 'l loopy-iter-bare-commands)
    (should (equal '((1 . 4) (2 . 5) (3 . 6))
                   (eval (quote (loopy-iter (l i '(1 2 3))
                                            (a j [4 5 6])
                                            (collecting (cons i j)))))))))

;;; Clean Stack Variables
(ert-deftest clean-stack-variables ()
  (let (loopy--known-loop-names
        loopy--accumulation-places
        loopy--at-instructions
        loopy--accumulation-list-end-vars
        loopy--accumulation-variable-info)
    (should (equal '((3 4) (1 2) 1 2 3 4)
                   (eval (quote (loopy-iter my-loop
                                            (arraying i [(1 2) (3 4)])
                                            (collecting i :at start)
                                            (looping inner
                                                     (listing j i)
                                                     (at my-loop (collecting j :at end))))))))
    (should-not (or loopy--known-loop-names
                    loopy--accumulation-places
                    loopy--at-instructions
                    loopy--accumulation-list-end-vars
                    loopy--accumulation-variable-info))))

;; end
