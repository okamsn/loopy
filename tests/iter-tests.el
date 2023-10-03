;;; Tests for `loopy-iter'  -*- lexical-binding: t; -*-

(eval-when-compile (require 'loopy)
                   (require 'loopy-iter))
(require 'loopy)
(require 'loopy-iter)
(require 'ert)
(require 'cl-lib)
(require 'generator)

(defmacro liq (&rest body)
  "`loopy' quote: Quote a use of `loopy'."
  `(eval (quote (loopy-iter ,@body)) t))


;;; Macro arguments
;;;; Keywords for special macro arguments.
(ert-deftest arg-keyword ()
  "This test is unique to `loopy-iter' while using \"bare\" forms."
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

(ert-deftest loopy-iter-sub-loop ()
  (should (equal '(2 3 4 5 6)
                 (loopy-iter (named outer)
                             (for list i '(1 2 3 4 5))
                             (loopy-iter
                              (for repeat 1)
                              (for set j (1+ i))
                              (for at outer
                                   (accum collect j))))))

  (should (equal '(2 3 4 5 6)
                 (loopy-iter outer
                             (listing i '(1 2 3 4 5))
                             (loopy-iter
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

(ert-deftest loopy-iter-command ()
  (should (equal '(11 12 13 14 15 16)
                 (eval (quote (loopy (named outer)
                                     (list i '((1 2) (3 4) (5 6)))
                                     (loopy-iter (for list j i)
                                                 (for at outer
                                                      (let ((val 10))
                                                        (accum collect (+ val j)))))))
                       t)))

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
     (for loopy-iter inner1
          (for list j '(3 4))
          (for loopy-iter
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
                    (loopy-iter
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
                    (for loopy-iter
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
                                            (for loopy-iter inner
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
                                 (loopy-iter inner
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
                   (with (a 1) (b 2) (c 3))
                   (while clause)
                   (loopy-let* ((key (pop clause))
                                ((key-fn take-fn transform-fn) (--first (funcall (car it) key)
                                                                        oo-bind-processers))
                                (taken (loopy (while (and clause
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
                   (with (a 1) (b 2) (c 3))
                   (while clause)
                   (loopy-let* ((key (pop clause))
                                ((key-fn take-fn transform-fn) (--first (funcall (car it) key)
                                                                        oo-bind-processers))
                                (taken (loopy (while (and clause
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


;; end
