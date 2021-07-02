;;; Tests for `loopy-iter'

(push (expand-file-name ".")
      load-path)

(require 'loopy "./loopy.el")
(require 'loopy-iter "./loopy-iter.el")
(require 'ert)
(require 'cl-lib)

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
                                                (for expr j (1+ i))
                                                (if (> j 5)
                                                    (throw tag t)))
                                          (exit return i))))))))

(ert-deftest wrap-throw ()
  (should (equal '(7 3 7 5 7)
                 (loopy-iter (for list i '(1 2 3 4 5))
                             (accum collect (catch 'my-tag
                                              (when (cl-evenp i)
                                                (throw (progn
                                                         (for expr tag 'my-tag)
                                                         tag)
                                                       (progn
                                                         (for expr val (1+ i))
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
   (equal '(3 7)
          (eval (quote
                 (loopy-iter (for list elem '((1 2) (3 4)))
                             (accum collect (funcall (cl-function (lambda ((x y))
                                                                    (+ x y)))
                                                     elem))))))))

(ert-deftest just-commands ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (for list i '(1 2 3))
                                          (accum collect i)))))))

(ert-deftest wrap-in-let ()
  (should
   (equal '(2 4 6)
          (eval (quote
                 (loopy-iter (for list i '(1 2 3))
                             (let ((a i)
                                   ;; A single symbol shouldn't cause an error,
                                   ;; and should be ignored.
                                   b)
                               (accum collect (+ a i)))))))))

(ert-deftest wrap-if ()
  (should (equal '((2 4) (1 3 5))
                 (eval (quote (loopy-iter (for list i '(1 2 3 4 5))
                                          (if (progn
                                                (for expr test (cl-evenp i))
                                                test)
                                              (accum collect evens i)
                                            (accum collect odds i))
                                          (finally-return evens odds))))))
  (should
   (equal '(2 4)
          (eval (quote
                 (loopy-iter (for list i '(1 2 3 4 5))
                             (accum collect (if (progn
                                                  (for expr test (cl-evenp i))
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
                                          (for expr i (* 2 elem))
                                          (+ elem i))))
                                 (accum collect a))))))))

(ert-deftest dont-expand-quoted ()
  (should (equal '((for expr i 2))
                 (eval
                  (quote
                   (loopy-iter (for repeat 1)
                               (let ((j '(for expr i 2)))
                                 (accum collect j)))))))

  (should (equal '((for expr i 2))
                 (eval
                  (quote
                   (loopy-iter (for repeat 1)
                               (let ((j (quote (for expr i 2))))
                                 (accum collect j))))))))


(ert-deftest wrap-macro ()
  (should (equal '(3 6 9)
                 (eval
                  (quote
                   (loopy-iter (for list elem '(1 2 3))
                               (cl-destructuring-bind (a b c) (progn
                                                                (for expr i elem)
                                                                (list i i i))
                                 (accum collect (+ a b c)))))))))

(ert-deftest wrap-setq ()
  (should
   (equal '((1 . 2) (2 . 4) (3 . 6))
          (eval (quote (loopy-iter (for list elem '(1 2 3))
                                   (setq a (progn
                                             (for expr i elem)
                                             i)
                                         b (progn
                                             (for expr j (* 2 elem))
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
                             (cl-symbol-macrolet ((mac (for expr val (1+ i))))
                               mac
                               (accum collect val))))))))

(ert-deftest wrap-lambda ()
  "Both quoted an unquoted lambda's should be wrapped."
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (for list elem '(1 2 3))
                                          (let ((a #'(lambda (x)
                                                       (for expr j x))))
                                            (accum collect (progn
                                                             (funcall a elem)
                                                             j))))))))
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (for list elem '(1 2 3))
                                          (let ((a (lambda (x)
                                                     (for expr j x))))
                                            (accum collect (progn
                                                             (funcall a elem)
                                                             j))))))))

  ;; NOTE: `defun' expands to a use of `lambda'.
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (for list elem '(1 2 3))
                                          (let ((a (defun loopy-defun-test (x)
                                                     (for expr j x))))
                                            (accum collect (progn
                                                             (funcall a elem)
                                                             j)))))))))

(ert-deftest wrap-while ()
  ;; `dolist' expands to `while'
  (should (equal '(1 7 2 8 3 9)
                 (eval (quote (loopy-iter (for list elem '((1 7) (2 8) (3 9)))
                                          (dolist (i elem)
                                            (accum collect i))))))))

(ert-deftest wrap-backquote ()
  (should (equal '((1 2) (2 4) (3 6))
                 (eval (quote
                        (loopy-iter (for list elem '(1 2 3))
                                    (let ((a `(,elem ,(progn
                                                        (for expr i (* elem 2))
                                                        i))))
                                      (accum collect a))))))))

(ert-deftest leave-in-let ()
  (should
   (equal '(8 9 10 11 12)
          (eval
           (quote
            (loopy-iter (let ((a (progn
                                   ;; NOTE: No restriction on placement of `expr'.
                                   (for expr j 8 (1+ j))
                                   (when (> j 12)
                                     ;; Leave loop but don't force return value,
                                     ;; allowing implicit result to be returned.
                                     (exit leave))
                                   j)))
                          (accum collect a))))))))

;;; lax naming
(ert-deftest flag-lax-naming ()
  (should
   (equal '(2 3 4)
          (eval (quote (loopy-iter (flag lax-naming)
                                   (array elem [1 2 3])
                                   (let ((a (1+ elem)))
                                     (collect a)))))))

  (should
   (equal '(2 3 4)
          (let ((loopy-default-flags '(lax-naming)))
            (eval (quote (loopy-iter (array elem [1 2 3])
                                     (let ((a (1+ elem)))
                                       (collect a)))))))))

;; Wrap in accum
(ert-deftest wrap-expressions-in-loop-commands ()
  (should
   (equal '(2 4 6)
          (eval (quote (loopy-iter (for list i '(1 2 3))
                                   (accum collect (progn (for expr a (* 2 i))
                                                         a)))))))
  (should
   (equal '(2 4 6)
          (eval (quote (loopy-iter (for list elem '(1 2 3))
                                   (for expr i (progn
                                                 (for expr j (* 2 elem))
                                                 j))
                                   (accum collect i)))))))

(ert-deftest dont-interpret-ignored-commands ()
  (let ((loopy-iter-ignored-commands '(when)))
    (should
     (equal '(2 3 4)
            (eval (quote
                   (loopy-iter (flag lax-naming)
                               (array elem [1 2 3])
                               (when t ; Should not be treated as a command.
                                 (let ((a (1+ elem)))
                                   (collect a)))))))))

  (should
   (equal '((4) (3 5))
          (let ((loopy-iter-ignored-commands '(if)))
            (eval
             (quote
              (loopy-iter (flag lax-naming)
                          (array elem [1 2 3])
                          ;; Should not be treated as a command.
                          (if (cl-evenp elem)
                              (let ((a (+ 2 elem)))
                                (collect evens a))
                            (let ((a (+ 2 elem)))
                              (collect odds a)))
                          (finally-return evens odds)))))))

  (let ((loopy-iter-ignored-commands '(if)))
    (should
     (equal '((4) (3 5))
            (eval
             (quote
              (loopy-iter (flag lax-naming)
                          (array elem [1 2 3])
                          ;; Should not be treated as a command.

                          (if (cl-evenp elem)
                              (collect evens (progn
                                               (expr a (+ 2 elem))
                                               a))
                            (collect odds (progn
                                            (expr a (+ 2 elem))
                                            a)))
                          (finally-return evens odds))))))))

(ert-deftest wrap-pcase-let* ()
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
           (eval (quote (loopy-iter (for list i '((1 2) (3 4) (5 6)))
                                    (pcase-let ((`(,a ,b) i))
                                      (accum collect a)
                                      (accum collect b))))))))

(ert-deftest wrap-seq-let ()
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
                                        (for expr val 0)
                                        (for expr val (if (cl-evenp i)
                                                          i
                                                        (error ""))))
                                    (accum sum important-val val)))
                                (finally-return important-val)))))))


(ert-deftest sub-loop ()
  (should (equal '(2 3 4 5 6)
                 (eval (quote (loopy-iter (for list i '(1 2 3 4 5))
                                          (for loop
                                               (for repeat 1)
                                               (for expr j (1+ i))
                                               (accum collect j)))))))

  (should (equal '(2 3 4 5 6)
                 (eval (quote (loopy-iter (for list i '(1 2 3 4 5))
                                          (let ((j nil))
                                            (for loop
                                                 (for repeat 1)
                                                 (setq j (1+ i))
                                                 (accum collect j))))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy-iter (for list i '((1 2) (3 4)))
                                          (for loop
                                               (for list j i)
                                               (accum collect j))))))))

(ert-deftest collect-coercion ()
  (should (equal [1 2 3]
                 (loopy-iter (for list j '(1 2 3))
                             (accum collect v j :result-type 'vector)
                             (finally-return v))))
  (should (equal [1 2 3]
                 (loopy-iter
                  (flag lax-naming)
                  (each j '(1 2 3))
                  (collect j :result-type 'vector)))))
;; end
