;;; Tests for `loopy-iter'

(push (expand-file-name ".")
      load-path)

(require 'loopy "./loopy.el")
(require 'ert)
(require 'cl-lib)

(ert-deftest just-commands ()
  (should (equal '(1 2 3)
                 (loopy-iter (for list i '(1 2 3))
                             (accum collect i)))))

(ert-deftest wrap-in-let ()
  (should (equal '(2 4 6)
                 (loopy-iter (for list i '(1 2 3))
                             (let ((a i))
                               (accum collect (+ a i)))))))

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
                                 (accum collect a)))))))
  )

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
                                   (accum collect (cons a b))))))))
;; end
