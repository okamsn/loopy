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
;; constructs.

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
                                        ; TODO: HERE !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
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
;; end
