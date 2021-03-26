;;; Tests for `loopy-iter'

(push (expand-file-name ".")
      load-path)

(require 'loopy "./loopy.el")
(require 'loopy-iter "./loopy-iter.el")
(require 'ert)
(require 'cl-lib)

(ert-deftest just-commands ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-iter (for list i '(1 2 3))
                                          (accum collect i)))))))

(ert-deftest wrap-in-let ()
  (should (equal '(2 4 6)
                 (eval (quote (loopy-iter (for list i '(1 2 3))
                                          (let ((a i))
                                            (accum collect (+ a i)))))))))

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



;; end
