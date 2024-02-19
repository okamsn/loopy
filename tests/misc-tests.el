;; Tests of secondary features and helper functions.

(push (expand-file-name ".")
      load-path)

(require 'cl-lib)

(require 'package)
(unless (featurep 'compat)
  (dolist (dir (cl-remove-if-not #'file-directory-p (directory-files (expand-file-name package-user-dir) t "compat")))
    (push dir load-path)))

(require 'map)
(require 'ert)
(require 'pcase)
(require 'map)
(require 'loopy)
;; (require 'loopy-destructure)

(ert-deftest pcase-pat-defined ()
  (should (get 'loopy 'pcase-macroexpander)))

(defmacro loopy-test-structure (input output-pattern)
  "Use `pcase' to check a destructurings bindings.
INPUT is the destructuring usage.  OUTPUT-PATTERN is what to match."
  (declare (indent 1))
  `(pcase ,input
     (,output-pattern
      t)
     (_ nil)))

;;; Minor Functions

(ert-deftest loopy--member-p ()
  (should (loopy--member-p '((a . 1) (b . 2))
                           '(2 . c)
                           :test
                           (lambda (seq-val test-val)
                             (= (cdr seq-val)
                                (car test-val)))))
  (should (loopy--member-p '((1) (2) (3)) '(2)))
  (should (loopy--member-p '((1) (2) (3)) '(2) :test #'equal))
  (should-error (loopy--member-p '((1) (2) (3)) '(2) :test #'=))
  (should (loopy--member-p '(1 2 3) 2 :test #'=))
  (should (loopy--member-p '((1) (2) (3)) '(2) :test #'= :key #'car)))

;;; Destructuring

(ert-deftest destructure-with ()
  (should-error (eval (quote (loopy (with ((a b) [1 2]))
                                    (cycle 1)
                                    (collect a)
                                    (collect b)))
                      t)
                :type 'loopy-bad-run-time-destructuring)

  (should (equal '(1 2 3 6)
                 (eval (quote (loopy (with ([a b] [1 2])
                                           (c (1+ b))
                                           (d (+ 3 c)))
                                     (cycle 1)
                                     (collect a)
                                     (collect b)
                                     (collect c)
                                     (collect d)))
                       t))))

(ert-deftest destructure-array-errors ()
  (should-error (loopy--destructure-for-iteration-default [a b &rest] 'val)
                :type 'loopy-&rest-missing)
  (should-error (loopy--destructure-for-iteration-default [a b &rest c d] 'val)
                :type 'loopy-&rest-multiple)
  (should-error (loopy--destructure-for-iteration-default [&rest] 'val)
                :type 'loopy-&rest-missing)
  (should-error (loopy--destructure-for-iteration-default [&whole &rest] 'val)
                :type 'loopy-&whole-missing)
  (should-error (loopy--destructure-for-iteration-default [&whole] 'val)
                :type 'loopy-&whole-missing)
  (should-error (loopy--destructure-for-iteration-default [&whole _] 'val)
                :type 'loopy-&whole-missing)
  (should-error (loopy--destructure-for-iteration-default [&rest _] 'val)
                :type 'loopy-&rest-missing)
  (should-error (loopy--destructure-for-iteration-default [_ _] 'val)
                :type 'loopy-destructure-vars-missing))

(ert-deftest loopy-let*-prev-val ()
  "Make sure we don't shadow values.
Later bindings can have access to the values of earlier bindings.
Later variables in the same destructuring should not use the
new values of the earlier variables."
  (should (equal '(2 3 13 107)
                 (eval (quote (let ((a 1)
                                    (b 2)
                                    (c 7)
                                    (d 33))
                                (loopy-let* (((a b) (list (1+ a) (1+ b)))
                                             (f (lambda (x) (+ 100 x)))
                                             ([c d] (vector (+ 10 b)
                                                            (funcall f c))))
                                  (list a b c d))))
                       t))))

(ert-deftest destructure-arrays ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-let* (([a b c] [1 2 3]))
                                (list a b c))))))

  (should (equal '([1 2 3] 1 2 3)
                 (eval (quote (loopy-let* (([&whole cat a b c] [1 2 3]))
                                (list cat a b c))))))

  (should (equal '(1 [2 3])
                 (eval (quote (loopy-let* (([a &rest b] [1 2 3]))
                                (list a b))))))

  (should (equal '([1 2 3] 1 [2 3])
                 (eval (quote (loopy-let* (([&whole cat a &rest b] [1 2 3]))
                                (list cat a b))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy-let* (([a &rest [b c]] [1 2 3]))
                                (list a b c))))))

  (should (equal '([1 2 3] 1 2 3)
                 (eval (quote (loopy-let* (([&whole cat a &rest [b c]] [1 2 3]))
                                (list cat a b c)))))))

(ert-deftest destructure-list-errors ()
  (should-error (loopy--get-var-groups '(a b &rest)) :type 'loopy-&rest-missing)
  (should-error (loopy--get-var-groups '(a b &rest c d)) :type 'loopy-&rest-multiple)
  (should-error (loopy--get-var-groups '(&rest)) :type 'loopy-&rest-missing)
  (should-error (loopy--get-var-groups '(&whole &rest)) :type 'loopy-&whole-missing)
  (should-error (loopy--get-var-groups '(&whole)) :type 'loopy-&whole-missing)
  (should-error (loopy--get-var-groups '(&whole _)) :type 'loopy-&whole-missing)
  (should-error (loopy--get-var-groups '(&rest _)) :type 'loopy-&rest-missing)
  (should-error (loopy--get-var-groups '(&rest rest &optional a)) :type 'loopy-&optional-bad-position)
  (should-error (loopy--get-var-groups '(&key a b &optional c)) :type 'loopy-&optional-bad-position)
  (should-error (loopy--get-var-groups '(&optional a (_ 27) c))   :type 'loopy-&optional-ignored-default-or-supplied)
  (should-error (loopy--get-var-groups '(&optional a (_ nil b-supplied) c))   :type 'loopy-&optional-ignored-default-or-supplied)
  (should-error (loopy--get-var-groups '(&key))         :type 'loopy-&key-missing)
  (should-error (loopy--get-var-groups '(&keys))        :type 'loopy-&key-missing)
  (should-error (loopy--get-var-groups '(&map))         :type 'loopy-&map-missing)
  ;; TODO: This test is expensive with `pcase.el'.
  ;; (should-error (loopy--get-var-groups '(_ _)) )
  )

(ert-deftest destructure-lists ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-let* (((a b c) '(1 2 3)))
                                (list a b c))))))

  (should (equal '(1 2 3 (4 5))
                 (eval (quote (loopy-let* (((a b c . d) '(1 2 3 4 5)))
                                (list a b c d))))))

  (should (equal '(1 2 3 (4 5))
                 (eval (quote (loopy-let* (((a b c &rest d) '(1 2 3 4 5)))
                                (list a b c d))))))


  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy-let* (((a b c &optional d e) '(1 2 3 4 5)))
                                (list a b c d e))))))

  (should (equal '(1 2 3 4 5 nil nil)
                 (eval (quote (loopy-let* (((a b c &optional d e (f nil f-supp)) '(1 2 3 4 5)))
                                (list a b c d e f f-supp))))))

  (should (equal '(1 2 3 4 5 27 nil)
                 (eval (quote (loopy-let* (((a b c &optional d e (f 27 f-supp)) '(1 2 3 4 5)))
                                (list a b c d e f f-supp))))))

  (should (equal '(1 2 3 4 5 6 t)
                 (eval (quote (loopy-let* (((a b c &optional d e (f 27 f-supp)) '(1 2 3 4 5 6)))
                                (list a b c d e f f-supp))))))

  (should (equal '(1 2 3 4 5 6 t (7 8))
                 (eval (quote (loopy-let* ((( a b c &optional d e (f 27 f-supp)
                                              &rest g)
                                            '(1 2 3 4 5 6 7 8)))
                                (list a b c d e f f-supp g))))))

  (should (equal '(1 2 3 t)
                 (eval (quote (loopy-let* ((( a &optional ((b c) nil bc-supp))
                                            '(1 (2 3))))
                                (list a b c bc-supp))))))

  (should (equal '(1 77 88 nil)
                 (eval (quote (loopy-let* ((( a &optional ((b c) (list 77 88) bc-supp))
                                            '(1)))
                                (list a b c bc-supp))))))

  (should (equal '(1 77 88 nil nil)
                 (eval (quote (loopy-let* ((( a &optional ((b &optional (c 88 c-supp))
                                                           (list 77)
                                                           bc-supp))
                                            '(1)))
                                (list a b c bc-supp c-supp))))))

  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy-let* (((a b c &key d e) '(1 2 3 :e 5 :d 4)))
                                (list a b c d e))))))

  (should (equal '(1 2 3 5 t 27 nil)
                 (eval (quote (loopy-let* (( (a b c &key (e nil e-supp)
                                                (f 27 f-supp)
                                                &allow-other-keys)
                                             '(1 2 3 :e 5 :d 4)))
                                (list a b c e e-supp f f-supp))))))

  (should (equal '(1 2 3 5 t nil nil)
                 (eval (quote (loopy-let* (((a b c &key
                                               ((:elephant e) nil e-supp)
                                               ((:fox f) nil f-supp)
                                               &allow-other-keys)
                                            '(1 2 3 :elephant 5 :d 4)))
                                (list a b c e e-supp f f-supp))))))

  (should (equal '(1 2 3 4 5 (:e 5 :d 4))
                 (eval (quote (loopy-let* (((a b c &key d e . f) '(1 2 3 :e 5 :d 4)))
                                (list a b c d e f))))))

  (should (equal '(1 2 3 7 6 (:e 6 :d 7))
                 (eval (quote (loopy-let* (((a b c _ _ &key d e . f) '(1 2 3 4 5 :e 6 :d 7)))
                                (list a b c d e f))))))

  (should (equal '(1 2 3 7 6 (4 5 :e 6 :d 7) 5)
                 (eval (quote (loopy-let* (((a b c &key ((4 key4)) d e &rest f)
                                            '(1 2 3 4 5 :e 6 :d 7)))
                                (list a b c d e f key4))))))

  (should (equal '(1 2 3 7 6 (4 5 :e 6 :d 7) 5)
                 (eval (quote (loopy-let* (((a b c &rest f &key ((4 key4)) d e)
                                            '(1 2 3 4 5 :e 6 :d 7)))
                                (list a b c d e f key4))))))

  (should-error (eval (quote (loopy-let* (((&key d e) '(:a 7 :e 5 :d 4)))
                               (list d e a))))
                :type 'loopy-bad-run-time-destructuring)

  (should (equal '(4 5)
                 (eval (quote (loopy-let* (((&key d e &allow-other-keys) '(:a 7 :e 5 :d 4)))
                                (list d e))))))

  (should (= 4 (eval (quote (loopy-let* (((_ _ _ a _ _ _) '(1 2 3 4 5 6 7)))
                              a))))))

;; We separate this since there's just way too many conditions in one test
;; otherwise.
(ert-deftest destructure-list-with-whole ()
  (should (equal '((1 2 3) 1 2 3)
                 (eval (quote (loopy-let* (((&whole cat a b c) '(1 2 3)))
                                (list cat a b c))))))

  (should (equal '((1 2 3 4 5) 1 2 3 (4 5))
                 (eval (quote (loopy-let* (((&whole cat a b c . d) '(1 2 3 4 5)))
                                (list cat a b c d))))))

  (should (equal '((1 2 3 4 5) 1 2 3 (4 5))
                 (eval (quote (loopy-let* (((&whole cat a b c &rest d)
                                            '(1 2 3 4 5)))
                                (list cat a b c d))))))

  (should (equal '((1 2 3 :e 5 :d 4) 1 2 3 4 5)
                 (eval (quote (loopy-let* (((&whole cat a b c &key d e)
                                            '(1 2 3 :e 5 :d 4)))
                                (list cat a b c d e))))))

  (should (equal '((1 2 3 :e 5 :d 4) 1 2 3 4 5 (:e 5 :d 4))
                 (eval (quote (loopy-let* (((&whole cat a b c &key d e . f)
                                            '(1 2 3 :e 5 :d 4)))
                                (list cat a b c d e f))))))

  (should (equal '((1 2 3 4 5 :e 6 :d 7) 1 2 3 7 6 (4 5 :e 6 :d 7) 5)
                 (eval (quote (loopy-let* (((&whole cat a b c &key d e ((4 key4)). f)
                                            '(1 2 3 4 5 :e 6 :d 7)))
                                (list cat a b c d e f key4))))))

  (should (equal '((1 2 3 4 5 e 6 d 7) 1 2 3 7 6 (4 5 e 6 d 7))
                 (eval (quote (loopy-let* (((&whole cat a b c &map d e . f)
                                            '(1 2 3 4 5 e 6 d 7)))
                                (list cat a b c d e f))))))

  (should (equal '((1 2 3 4 5 :e 6 :d 7) 1 2 3 7 6 (4 5 :e 6 :d 7))
                 (eval (quote (loopy-let* (((&whole cat a b c &map (:d d) (:e e) . f)
                                            '(1 2 3 4 5 :e 6 :d 7)))
                                (list cat a b c d e f))))))

  (should (equal '((1 2 3 4 5 :e 6 :d 7) 1 2 3 7 6 (4 5 :e 6 :d 7) 5)
                 (eval (quote (loopy-let* (((&whole cat a b c &key d e ((4 key4)) &rest f)
                                            '(1 2 3 4 5 :e 6 :d 7)))
                                (list cat a b c d e f key4))))))

  (should (equal '((1 2 3 4 5 e 6 d 7) 1 2 3 7 6 (4 5 e 6 d 7))
                 (eval (quote (loopy-let* (((&whole cat a b c &map d e &rest f)
                                            '(1 2 3 4 5 e 6 d 7)))
                                (list cat a b c d e f))))))

  (should (equal '((1 2 3 4 5 :e 6 :d 7) 1 2 3 7 6 (4 5 :e 6 :d 7) 5)
                 (eval (quote (loopy-let* (((&whole cat a b c &rest f &key d e ((4 key4)))
                                            '(1 2 3 4 5 :e 6 :d 7)))
                                (list cat a b c d e f key4))))))

  (should (equal '((1 2 3 4 5 :e 6 :d 7) 1 2 3 7 6 (4 5 :e 6 :d 7))
                 (eval (quote (loopy-let* (((&whole cat a b c &rest f
                                                    &map (:d d) (:e e))
                                            '(1 2 3 4 5 :e 6 :d 7)))
                                (list cat a b c d e f))))))

  (should (equal '((:a 7 :e 5 :d 4) 4 5)
                 (eval (quote (loopy-let* (((&whole cat &key d e &allow-other-keys)
                                            '(:a 7 :e 5 :d 4)))
                                (list cat d e))))))

  (should (equal '((:a 7 :e 5 :d 4 :allow-other-keys t) 4 5)
                 (eval (quote (loopy-let* (((&whole cat &key d e)
                                            '(:a 7 :e 5 :d 4 :allow-other-keys t)))
                                (list cat d e)))))))

;; This only tests the getting of values.
(ert-deftest destructure-lists-ref-values ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy-ref (((a b c) '(1 2 3)))
                                (list a b c))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy-ref (((a b (c d)) '(1 2 (3 4))))
                                (list a b c d))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy-ref (((a b &rest (c d)) '(1 2 3 4)))
                                (list a b c d))))))

  (should (equal '(1 2 (3 4))
                 (eval (quote (loopy-ref (((a b . c) '(1 2 3 4)))
                                (list a b c))))))

  (should (equal '(1 2 3 5 6)
                 (eval (quote (loopy-ref (((a b c &key d e) '(1 2 3 :e 6 :d 5)))
                                (list a b c d e))))))

  (should (equal '(1 2 3 5 6 (:e 6 :d 5))
                 (eval (quote (loopy-ref (((a b c &rest rest &key d e)
                                           '(1 2 3 :e 6 :d 5)))
                                (list a b c d e rest))))))

  (should (equal '(1 2 3 5 6 (:e 6 :d 5))
                 (eval (quote (loopy-ref (((a b c &key d e . rest)
                                           '(1 2 3 :e 6 :d 5)))
                                (list a b c d e rest))))))

  (should (equal '(1 2 3 5 6 (:e 6 :d 5))
                 (eval (quote (loopy-ref (((a b c &key d e &rest rest)
                                           '(1 2 3 :e 6 :d 5)))
                                (list a b c d e rest))))))

  (should (equal '(5 6)
                 (eval (quote (loopy-ref (((&key d e) '(1 2 :e 6 :d 5)))
                                (list d e)))))))

;; This tests the setting of values.
(ert-deftest destructure-list-ref-setf ()
  (should (equal '(1 2 3)
                 (let ((l (list 7 7 7)))
                   (loopy-ref (((a b c) l))
                     (setf a 1 b 2 c 3)
                     l))))

  (should (equal '(1 2 (3 4))
                 (let ((l (list 7 7 (list 7 7))))
                   (loopy-ref (((a b (c d)) l))
                     (setf a 1 b 2 c 3 d 4)
                     l))))

  (should (equal '(1 2 3 4)
                 (let ((l (list 7 7 7 7)))
                   (loopy-ref (((a b &rest (c d)) l))
                     (setf a 1 b 2 c 3 d 4)
                     l))))

  (should (equal '(1 2 . 3)
                 (let ((l (list 7 7 7 7)))
                   (loopy-ref (((a b . c) l))
                     (setf a 1 b 2 c 3))
                   l)))

  (should (equal '(1 2 3 :d 4 :e 5)
                 (let ((l (list 7 7 7 :d 7 :e 7)))
                   (loopy-ref (((a b c &key d e) l))
                     (setf a 1 b 2 c 3 d 4 e 5))
                   l)))

  (should (equal '(1 2 3 :e 10 :d 8)
                 (let ((l (list 7 7 7 :e 7 :d 7)))
                   (loopy-ref (((a b c &rest rest &key d e) l))
                     (setf a 1 b 2 c 3 d 4 e 5
                           rest (mapcar (lambda (x)
                                          (if (numberp x)
                                              (* 2 x)
                                            x))
                                        rest)))
                   l)))

  (should (equal '(1 2 3 :e 10 :d 8)
                 (let ((l (list 7 7 7 :e 7 :d 7)))
                   (loopy-ref (((a b c &key d e . rest) l))
                     (setf a 1 b 2 c 3 d 4 e 5
                           rest (mapcar (lambda (x)
                                          (if (numberp x)
                                              (* 2 x)
                                            x))
                                        rest)))
                   l)))

  (should (equal '(1 2 3 :e 10 :d 8)
                 (let ((l (list 7 7 7 :e 7 :d 7)))
                   (loopy-ref (((a b c &key d e &rest rest) l))
                     (setf a 1 b 2 c 3 d 4 e 5
                           rest (mapcar (lambda (x)
                                          (if (numberp x)
                                              (* 2 x)
                                            x))
                                        rest)))
                   l)))

  (should (equal '(7 7 :a 1 :b 2)
                 (let ((l (list 7 7 :a 7 :b 7)))
                   (loopy-ref (((&key a b) l))
                     (setf a 1 b 2))
                   l)))

  (should (equal '(2 3)
                 (eval (quote
                        (let ((l (list 7 7)))
                          (loopy-ref (((&whole whole a b) l))
                            (setf a 1 b 2
                                  whole (mapcar #'1+ whole)))
                          l))))))

(ert-deftest generalized-array-should-error ()
  ;; TODO: Having trouble with `should-error' here?
  ;; (should-error (loopy--destructure-generalized-array [a b &optional c] 'val)
  ;;               :type '(loopy-&optional-generalized-variable))
  (should (condition-case err
              (loopy--destructure-generalized-array [a b &optional c] 'val)
            (loopy-&optional-generalized-variable
             t)))

  (should (condition-case err
              (loopy--destructure-generalized-array [a b &map ('c c 27)] 'val)
            (loopy-generalized-default
             t)))

  (should (condition-case err
              (loopy--destructure-generalized-array [a b &map ('c c nil c-supp)] 'val)
            (loopy-generalized-supplied
             t))))

(ert-deftest destructure-array-refs ()
  (should (equal [1 2 3]
                 (let ((arr [7 7 7]))
                   (loopy-ref (([a b c] arr))
                     (setf a 1 b 2 c 3))
                   arr)))

  (should (equal [2 3 4]
                 (let ((arr [7 7 7]))
                   (loopy-ref (([&whole whole a b c] arr))
                     (setf a 1 b 2 c 3
                           whole (cl-map 'vector #'1+ whole)))
                   arr)))

  (should (equal [1 2 3 [4 5]]
                 (let ((arr [7 7 7 [7 7]]))
                   (loopy-ref (([a b c [d e]] arr))
                     (setf a 1 b 2 c 3 d 4 e 5))
                   arr)))

  ;; TODO: This test currently doesn't pass due to Elisp limitations.
  ;; (should (equal [1 2 3 4 5]
  ;;                (eval (quote
  ;;                       (let ((arr [7 7 7 7 7]))
  ;;                         (loopy-ref (([a b c &rest [d e]] arr))
  ;;                           (setf a 1 b 2 c 3 d 4 e 5))
  ;;                         arr)))))

  ;; NOTE: Setting a variable after `&rest' in an array will not truncate the array.
  (should (equal [1 2 3 4 7]
                 (let ((arr [7 7 7 7 7]))
                   (loopy-ref (([a b c &rest d] arr))
                     (setf a 1 b 2 c 3 d [4]))
                   arr)))

  (should (equal [1 2 3 4 7]
                 (let ((arr [7 7 7 7 7]))
                   (loopy-ref (([a b c &rest d] arr))
                     (setf a 1 b 2 c 3 d [4]))
                   arr)))

  ;; NOTE: This currently doesn't work due to upstream implementations.
  ;;       See issue #184.
  ;; (should (equal [1 2 3 4 0 0 16]
  ;;                (let ((arr (vector 7 7 7 7 0 0 6)))
  ;;                  (loopy-ref (([a b c &rest d &map (3 sub-idx-3)] arr))
  ;;                    (setf a 1 b 2 c 3 d [4])
  ;;                    (cl-incf sub-idx-3 10))
  ;;                  arr)))

  (should (equal [2 3]
                 (let ((arr [7 7]))
                   (loopy-ref (([&whole cat a b] arr))
                     (setf a 1 b 2
                           cat (cl-map 'vector #'1+ cat)))
                   arr))))

;;;;; Default flag
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

(ert-deftest accumulation-array-destructuring ()
  (should (equal '((1 4) (2 5) (3 6))
                 (eval (quote (loopy (list elem '([1 2 3] [4 5 6]))
                                     (collect [i j k] elem)
                                     (finally-return i j k))))))

  (should (equal '(([1 2 3] [4 5 6]) (1 4) (2 5) (3 6))
                 (eval (quote (loopy (list elem '([1 2 3] [4 5 6]))
                                     (collect [&whole whole i j k] elem)
                                     (finally-return whole i j k))))))

  (should (equal '((1 4) (3 6))
                 (eval (quote (loopy (list elem '([1 2 3] [4 5 6]))
                                     (collect [i _ k] elem)
                                     (finally-return i k))))))

  (should (equal '((1 4) ([2 3] [5 6]))
                 (eval (quote (loopy (list elem '([1 2 3] [4 5 6]))
                                     (collect [i &rest k] elem)
                                     (finally-return i  k))))))

  (should (equal '((1 4) (2 5) (3 6))
                 (eval (quote (loopy (list elem '([1 2 3] [4 5 6]))
                                     (collect [i &rest [j k]] elem)
                                     (finally-return i j k)))))))

(ert-deftest accumulation-list-destructuring ()
  (should (equal '((1 4) (2 5) (3 6))
                 (eval (quote (loopy (list elem '((1 2 3) (4 5 6)))
                                     (collect (i j k) elem)
                                     (finally-return i j k))))))

  (should (equal '((1 4) (3 6))
                 (eval (quote (loopy (list elem '((1 2 3) (4 5 6)))
                                     (collect (i _ k) elem)
                                     (finally-return i k))))))

  (should (equal '(((1 2 3) (4 5 6)) (1 4) (2 5) (3 6))
                 (eval (quote (loopy (list elem '((1 2 3) (4 5 6)))
                                     (collect (&whole tot i j k) elem)
                                     (finally-return tot i j k))))))

  (should (equal '((1 4) (2 5) (3 6))
                 (eval (quote (loopy (list elem '((1 2 3) (4 5 6)))
                                     (collect (i &rest (j k)) elem)
                                     (finally-return i j k))))))

  (should (equal '((1 4) ((2 3) (5 6)))
                 (eval (quote (loopy (list elem '((1 2 3) (4 5 6)))
                                     (collect (i . j) elem)
                                     (finally-return i j))))))

  (should (equal '((1 4) (2 5) (3 6) (4 27) (5 8))
                 (eval (quote (loopy (list elem '((1 2 3 :k1 4 :k2 5) (4 5 6 :k2 8)))
                                     (collect (i j k &key (k1 27) k2) elem)
                                     (finally-return i j k k1 k2))))))

  (should (equal '((1 4) (2 5) (3 6) ((:k1 4 :k2 5) (:k2 8)) (4 27) (5 8))
                 (eval (quote (loopy (list elem '((1 2 3 :k1 4 :k2 5) (4 5 6 :k2 8)))
                                     (collect (i j k &rest rest &key (k1 27) k2) elem)
                                     (finally-return i j k rest k1 k2))))))

  (should (equal '((1 4) (2 5) (3 6) ((:k1 4 :k2 5) (:k2 8)) (4 27) (5 8))
                 (eval (quote (loopy (list elem '((1 2 3 :k1 4 :k2 5) (4 5 6 :k2 8)))
                                     (collect (i j k &key (k1 27) k2 &rest rest) elem)
                                     (finally-return i j k rest k1 k2))))))

  (should (equal '((1 4) (2 5) (3 6) ((:k1 4 :k2 5) (:k2 8)) (4 27) (5 8))
                 (eval (quote (loopy (list elem '((1 2 3 :k1 4 :k2 5) (4 5 6 :k2 8)))
                                     (collect (i j k &key (k1 27) k2 . rest) elem)
                                     (finally-return i j k rest k1 k2)))))))

;;;;; Pcase Pattern
(defmacro loopy--pcase-exhaustive-wrapper (vars val &rest branches)
  "Wrap variables to make sure that they're bound on earlier versions of Emacs.

Prior to Emacs 28, `pcase' didn't guarantee binding all variables.

- VARS is the list of variables.
- VAL is the value to match against.
- BRANCHES are the `pcase' branches."
  (declare (indent 2))
  `(eval (quote (let ,(mapcar (lambda (v)
                                `(,v 'intentionally-bad-test-val))
                              vars)
                  (pcase-exhaustive ,val
                    ,@branches)))
         t))

(ert-deftest pcase-tests-loopy-&whole-should-error ()
  "`&whole' must come first if given, and must be followed by a patter."
  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&whole))
                   (list a b c)))
                :type 'loopy-&whole-missing)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&whole &rest))
                   (list a b c)))
                :type 'loopy-&whole-missing)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&whole _ &rest))
                   (list a b c)))
                :type 'loopy-&whole-missing)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (a b &whole c))
                   (list a b c)))
                :type 'loopy-&whole-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&rest a &whole c))
                   (list a b c)))
                :type 'loopy-&whole-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&key a &whole c))
                   (list a b c)))
                :type 'loopy-&whole-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&aux (a 1) &whole c))
                   (list a b c)))
                :type 'loopy-&whole-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&optional (a 1) &whole c))
                   (list a b c)))
                :type 'loopy-&whole-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (whole1 whole2)
                    (list 1 2 3)
                  ((loopy (&whole whole1 &whole whole2))
                   (list whole1 whole2)))
                :type 'loopy-&whole-bad-position))

(ert-deftest pcase-tests-loopy-&whole ()
  "`&whole' can be a `pcase' pattern."
  (should (equal (list (list 1 2 3) 1 2 3)
                 (loopy--pcase-exhaustive-wrapper (whole a b c)
                     (list 1 2 3)
                   ((loopy (&whole whole a b c))
                    (list whole a b c)))))

  (should (equal (list 1 2 3 1 2 3)
                 (loopy--pcase-exhaustive-wrapper (a0 b0 c0 a b c)
                     (list 1 2 3)
                   ((loopy (&whole `(,a0 ,b0 ,c0) a b c))
                    (list a0 b0 c0 a b c))))))

(ert-deftest pcase-tests-loopy-pos ()
  "Positional variables must match the length of EXPVAL."
  (should (equal (list 1 2 3)
                 (loopy--pcase-exhaustive-wrapper (a b c)
                     (list 1 2 3)
                   ((loopy (a b c))
                    (list a b c)))))

  (should (equal nil
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list (list 1))
                   ((loopy (a b)) (list a b))
                   (_ nil))))

  (should (equal nil
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list (list 1 2 3))
                   ((loopy (a b)) (list a b))
                   (_ nil)))))

(ert-deftest pcase-tests-loopy-pos-sub-seq ()
  (should (equal (list 1 2 3 4)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2 (list 3 4))
                   ((loopy (a b (c d)))
                    (list a b c d)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list (list 1 2))
                   ((loopy ((a b)))
                    (list a b))))))

(ert-deftest pcase-tests-loopy-&optional-should-error ()
  "`&optional' cannot be used after `&optional', `&rest', `&key', and `&aux'."
  (should-error (equal (list 1 2 3)
                       (loopy--pcase-exhaustive-wrapper (a b c)
                           (list 1 2 3)
                         ((loopy (&rest a &optional b c))
                          (list a b c))))
                :type 'loopy-&optional-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&body a &optional b c))
                   (list a b c)))
                :type 'loopy-&optional-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&key a &optional b c))
                   (list a b c)))
                :type 'loopy-&optional-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&aux (a 1) &optional b c))
                   (list a b c)))
                :type 'loopy-&optional-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&optional a &optional b c))
                   (list a b c)))
                :type 'loopy-&optional-bad-position))

(ert-deftest pcase-tests-loopy-&optional ()
  (should (equal (list 1 2 3)
                 (loopy--pcase-exhaustive-wrapper (a b c)
                     (list 1 2 3)
                   ((loopy (a b &optional c))
                    (list a b c)))))

  (should (equal (list 1 2 nil)
                 (loopy--pcase-exhaustive-wrapper (a b c)
                     (list 1 2)
                   ((loopy (a b &optional c))
                    (list a b c)))))

  (should (equal (list 1 2 13)
                 (loopy--pcase-exhaustive-wrapper (a b c)
                     (list 1 2)
                   ((loopy (a b &optional (c 13)))
                    (list a b c)))))

  (should (equal (list 1 2 13)
                 (loopy--pcase-exhaustive-wrapper (a b c)
                     (list 1 2)
                   ((loopy (a b &optional [c 13]))
                    (list a b c)))))

  (should (equal (list 1 2 13 nil)
                 (loopy--pcase-exhaustive-wrapper (a b c c-supplied)
                     (list 1 2)
                   ((loopy (a b &optional [c 13 c-supplied]))
                    (list a b c c-supplied)))))

  (should (equal (list 1 2 3 t)
                 (loopy--pcase-exhaustive-wrapper (a b c c-supplied)
                     (list 1 2 3)
                   ((loopy (a b &optional [c 13 c-supplied]))
                    (list a b c c-supplied))))))

(ert-deftest pcase-tests-loopy-&optional-ignored ()
  (should (equal (list 1 2 nil)
                 (loopy--pcase-exhaustive-wrapper (a b d)
                     (list 1 2)
                   ((loopy (a b &optional _ d))
                    (list a b d)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list 1 2)
                   ((loopy (a b &optional _ _))
                    (list a b)))))

  (should (equal (list 1 2 13 nil)
                 (loopy--pcase-exhaustive-wrapper (a b k1 k2)
                     (list 1 2)
                   ((loopy (a b &optional _ _ &key [k1 13] k2))
                    (list a b k1 k2)))))

  (should (equal (list 1 2 nil 14 nil)
                 (loopy--pcase-exhaustive-wrapper (a b e k1 k2)
                     (list 1 2)
                   ((loopy (a b &optional _ _ &rest e &key [k1 14] k2))
                    (list a b e k1 k2)))))

  (should (equal (list 1 2 nil 14 nil)
                 (loopy--pcase-exhaustive-wrapper (a b e k1 k2)
                     (list 1 2)
                   ((loopy (a b &optional _ _ &rest e &map [:k1 k1 14] (:k2 k2)))
                    (list a b e k1 k2)))))

  (should (equal (list 1 2 nil)
                 (loopy--pcase-exhaustive-wrapper (a b d)
                     (vector 1 2)
                   ((loopy [a b &optional _ d])
                    (list a b d)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (vector 1 2)
                   ((loopy [a b &optional _ _])
                    (list a b)))))

  ;; FIXME: This test fails on Emacs 27 because the tests don't install the
  ;;       correct version of Map.el.
  (when (> emacs-major-version 27)
    (should (equal (list 1 2 [] 14 nil)
                   (loopy--pcase-exhaustive-wrapper (a b e k1 k2)
                       (vector 1 2)
                     ((loopy [a b &optional _ _ &rest e &map [:k1 k1 14] (:k2 k2)])
                      (list a b e k1 k2)))))))

(ert-deftest pcase-tests-loopy-&optional-sub-seq ()
  "Test using sub-seq in `loopy' pattern.
sub-seq must be contained within a sub-list, since a sub-list
also provides a default value."
  (should (equal (list 1 2 3 4)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2 (list 3 4))
                   ((loopy (a b &optional ((c d))))
                    (list a b c d)))))

  (should (equal (list 1 2 3 4)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2 (list 3 4))
                   ((loopy (a b &optional [(c d)]))
                    (list a b c d)))))

  (should (equal (list 1 2 nil nil)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2)
                   ((loopy (a b &optional ((c d))))
                    (list a b c d)))))

  (should (equal (list 1 2 nil nil)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2)
                   ((loopy (a b &optional [(c d)]))
                    (list a b c d)))))

  (should (equal (list 1 2 13 14)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2)
                   ((loopy (a b &optional ((c d) (list 13 14))))
                    (list a b c d)))))

  (should (equal (list 1 2 13 14)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2)
                   ((loopy (a b &optional [(c d) (list 13 14)]))
                    (list a b c d)))))

  (should (equal (list 1 2 13 14)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2)
                   ((loopy ( a b
                             &optional ((c &optional (d 14))
                                        (list 13))))
                    (list a b c d)))))

  (should (equal (list 1 2 13 14)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2)
                   ((loopy ( a b
                             &optional ((c &optional [d 14])
                                        (list 13))))
                    (list a b c d)))))

  (should (equal (list 1 2 13 14)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2)
                   ((loopy ( a b
                             &optional [(c &optional (d 14))
                                        (list 13)]))
                    (list a b c d)))))

  (should (equal (list 1 2 13 14)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2)
                   ((loopy ( a b
                             &optional [(c &optional [d 14])
                                        (list 13)]))
                    (list a b c d)))))

  (should (equal (list 1 2 13 14 nil)
                 (loopy--pcase-exhaustive-wrapper (a b c d cd-supplied)
                     (list 1 2)
                   ((loopy (a b &optional ((c d) (list 13 14) cd-supplied)))
                    (list a b c d cd-supplied)))))

  (should (equal (list 1 2 13 14 nil)
                 (loopy--pcase-exhaustive-wrapper (a b c d cd-supplied)
                     (list 1 2)
                   ((loopy (a b &optional [(c d) (list 13 14) cd-supplied]))
                    (list a b c d cd-supplied)))))

  (should (equal (list 1 2 13 14 nil t nil)
                 (loopy--pcase-exhaustive-wrapper (a b c d cd-supplied c-sub-sup d-sub-sup)
                     (list 1 2)
                   ((loopy ( a b
                             &optional
                             ((&optional (c 27 c-sub-sup)
                                         (d 14 d-sub-sup))
                              (list 13)
                              cd-supplied)))
                    (list a b c d cd-supplied c-sub-sup d-sub-sup)))))

  (should (equal (list 1 2 13 14 nil t nil)
                 (loopy--pcase-exhaustive-wrapper (a b c d cd-supplied c-sub-sup d-sub-sup)
                     (list 1 2)
                   ((loopy ( a b
                             &optional
                             [(&optional (c 27 c-sub-sup)
                                         [d 14 d-sub-sup])
                              (list 13)
                              cd-supplied]))
                    (list a b c d cd-supplied c-sub-sup d-sub-sup))))))

(ert-deftest pcase-tests-loopy-&rest-should-error ()
  "`&rest' (`&body', `.') cannot be used after `&rest', `&body', `&key',and `&aux'."
  (should-error (equal (list 1 2 3)
                       (loopy--pcase-exhaustive-wrapper (a b c)
                           (list 1 2 3)
                         ((loopy (&rest a &rest b))
                          (list a b c))))
                :type 'loopy-&rest-bad-position)

  (should-error (equal (list 1 2 3)
                       (loopy--pcase-exhaustive-wrapper (a b c)
                           (list 1 2 3)
                         ((loopy (&body a &body b))
                          (list a b c))))
                :type 'loopy-&rest-bad-position)

  (should-error (equal (list 1 2 3)
                       (loopy--pcase-exhaustive-wrapper (a b c)
                           (list 1 2 3)
                         ((loopy (&body a . b))
                          (list a b c))))
                :type 'loopy-&rest-dotted)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&body a &rest b))
                   (list a b c)))
                :type 'loopy-&rest-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&rest a &body b))
                   (list a b c)))
                :type 'loopy-&rest-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&aux (a 1) &rest b))
                   (list a b c)))
                :type 'loopy-&rest-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&aux (a 1) &body b))
                   (list a b c)))
                :type 'loopy-&rest-bad-position))

(ert-deftest pcase-tests-loopy-&rest-ignored ()

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     [1 2 3]
                   ((loopy [a b &rest _])
                    (list a b)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     '(1 2 3)
                   ((loopy (a b &rest _))
                    (list a b)))))

  (should (equal (list 1 2 3 11 12)
                 (loopy--pcase-exhaustive-wrapper (a b c k1 k2)
                     '(1 2 3 :k1 11 :k2 12)
                   ((loopy (a b c &rest _ &key k1 k2))
                    (list a b c k1 k2)))))

  (should (equal (list 1 2 3 11 12)
                 (loopy--pcase-exhaustive-wrapper (a b c k1 k2)
                     '(1 2 3 :k1 11 :k2 12)
                   ((loopy (a b c &rest _ &map (:k1 k1) (:k2 k2)))
                    (list a b c k1 k2))))))

(ert-deftest pcase-tests-loopy-&rest-nonlist-cdr ()
  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (cons 1 2)
                   ((loopy (a &rest b))
                    (list a b)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (cons 1 2)
                   ((loopy (a &body b))
                    (list a b)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (cons 1 2)
                   ((loopy (a . b))
                    (list a b))))))

(ert-deftest pcase-tests-loopy-&rest-with-&whole ()
  (should (equal (list (cons 1 2) 1 2)
                 (loopy--pcase-exhaustive-wrapper (whole a b)
                     (cons 1 2)
                   ((loopy (&whole whole a &rest b))
                    (list whole a b)))))

  (should (equal (list (cons 1 2) 1 2)
                 (loopy--pcase-exhaustive-wrapper (whole a b)
                     (cons 1 2)
                   ((loopy (&whole whole a &body b))
                    (list whole a b)))))

  (should (equal (list (cons 1 2) 1 2)
                 (loopy--pcase-exhaustive-wrapper (whole a b)
                     (cons 1 2)
                   ((loopy (&whole whole a . b))
                    (list whole a b))))))

(ert-deftest pcase-tests-loopy-&rest-only ()
  "Using only `&rest' should work like `&whole'."
  (should (equal (list (list 1 2))
                 (loopy--pcase-exhaustive-wrapper (a)
                     (list 1 2)
                   ((loopy (&rest a))
                    (list a)))))

  (should (equal (list (cons 1 2))
                 (loopy--pcase-exhaustive-wrapper (a)
                     (cons 1 2)
                   ((loopy (&body a))
                    (list a))))))

(ert-deftest pcase-tests-loopy-&rest-after-&optional ()
  (should (equal (list 1 2 3 (list 4 5))
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2 3 4 5)
                   ((loopy (&optional a b c &rest d))
                    (list a b c d)))))

  (should (equal (list 1 2 3 (list 4 5))
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2 3 4 5)
                   ((loopy (&optional a b c &body d))
                    (list a b c d)))))

  (should (equal (list 1 2 3 (list 4 5))
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2 3 4 5)
                   ((loopy (&optional a b c . d))
                    (list a b c d))))))

(ert-deftest pcase-tests-loopy-&rest-sub-seq ()
  (should (equal (list 1 2 3)
                 (loopy--pcase-exhaustive-wrapper (a b c)
                     (list 1 2 3)
                   ((loopy (a &rest (b c)))
                    (list a b c)))))

  (should (equal (list 1 2 3)
                 (loopy--pcase-exhaustive-wrapper (a b c)
                     (list 1 2 3)
                   ((loopy (a . (b c)))
                    (list a b c)))))

  (should (equal (list 1 2 3)
                 (loopy--pcase-exhaustive-wrapper (a b c)
                     (list 1 2 3)
                   ((loopy (a &body (b c)))
                    (list a b c))))))

(ert-deftest pcase-tests-loopy-&key-should-error ()
  "`&key' cannot be used after `&key', `&allow-other-keys', and `&aux'."
  (should-error (loopy--pcase-exhaustive-wrapper (a b)
                    (list :a 1 :b 2)
                  ((loopy (&key a &key b))
                   (list a b)))
                :type 'loopy-&key-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b)
                    (list :a 1 :b 2)
                  ((loopy (&aux (a 1) &key b))
                   (list a b)))
                :type 'loopy-&key-bad-position))

(ert-deftest pcase-tests-loopy-&map-should-error ()
  "`&map' cannot be used after `&map' and `&aux'."
  (should-error (loopy--pcase-exhaustive-wrapper (a b)
                    (list :a 1 :b 2)
                  ((loopy (&map a &map b))
                   (list a b)))
                :type 'loopy-&map-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b)
                    (list :a 1 :b 2)
                  ((loopy (&aux (a 1) &map b))
                   (list a b)))
                :type 'loopy-&map-bad-position))

(ert-deftest pcase-tests-&allow-other-keys ()
  (should-error (loopy--pcase-exhaustive-wrapper (a b)
                    (list :a 1 :b 2)
                  ((loopy (&allow-other-keys &key b))
                   (list a b)))
                :type 'loopy-&allow-other-keys-without-&key))

(ert-deftest pcase-tests-loopy-&key-exact ()
  "`&key' doesn't match unspecified keys unless `&allow-other-keys' or `:allow-other-keys' is given."
  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list :a 1 :b 2)
                   ((loopy (&key a b))
                    (list a b)))))

  (should-error (loopy--pcase-exhaustive-wrapper (a b)
                    (list :a 1 :b 2 :c 3)
                  ((loopy (&key a b))
                   (list a b))))

  (should (equal (list 1 2 nil)
                 (loopy--pcase-exhaustive-wrapper (a b c)
                     (list :a 1 :b 2)
                   ((loopy (&key a b c))
                    (list a b c))))))

(ert-deftest pcase-tests-loopy-&key-permissive ()
  "`&key' doesn't match unspecified keys unless `&allow-other-keys' or `:allow-other-keys' is given."
  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list :a 1 :b 2 :c 3)
                   ((loopy (&key a b &allow-other-keys))
                    (list a b)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list :a 1 :b 2 :c 3 :allow-other-keys t)
                   ((loopy (&key a b))
                    (list a b))))))

(ert-deftest pcase-tests-loopy-&map-permissive ()
  "`&map' should not require a construct like `&allow-other-keys'."
  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list 'a 1 'b 2 'c 3)
                   ((loopy (&map a b))
                    (list a b)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list :a 1 :b 2 :c 3)
                   ((loopy (&map (:a a) (:b b)))
                    (list a b))))))

(ert-deftest pcase-tests-loopy-&key-not-first ()
  "The plist should be after positional values and equal to `&rest'."
  (should (equal (list 1 2 3 11 22)
                 (loopy--pcase-exhaustive-wrapper (a b c k1 k2)
                     (list 1 2 3 :k1 11 :k2 22)
                   ((loopy (a b c &key k1 k2))
                    (list a b c k1 k2)))))

  (should (equal (list 1 2 3 (list :k1 11 :k2 22) 11 22)
                 (loopy--pcase-exhaustive-wrapper (a b c r1 k1 k2)
                     (list 1 2 3 :k1 11 :k2 22)
                   ((loopy (a b c &rest r1 &key k1 k2))
                    (list a b c r1 k1 k2))))))

(ert-deftest pcase-tests-loopy-&map-not-first ()
  "The map should be after positional values and equal to `&rest'."
  (should (equal (list 1 2 3 11 22)
                 (loopy--pcase-exhaustive-wrapper (a b c k1 k2)
                     (list 1 2 3 'k1 11 'k2 22)
                   ((loopy (a b c &map k1 k2))
                    (list a b c k1 k2)))))

  (should (equal (list 1 2 3 (list :k1 11 :k2 22) 11 22)
                 (loopy--pcase-exhaustive-wrapper (a b c r1 k1 k2)
                     (list 1 2 3 :k1 11 :k2 22)
                   ((loopy (a b c &rest r1 &map (:k1 k1) (:k2 k2)))
                    (list a b c r1 k1 k2))))))

(ert-deftest pcase-tests-loopy-&key-full-form ()
  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list :a 1 :b 2)
                   ((loopy (&key a (b 13)))
                    (list a b)))))

  (should (equal (list 1 13)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list :a 1)
                   ((loopy (&key a (b 13)))
                    (list a b)))))

  (should (equal (list 1 13 nil)
                 (loopy--pcase-exhaustive-wrapper (a b b-supplied)
                     (list :a 1)
                   ((loopy (&key a (b 13 b-supplied)))
                    (list a b b-supplied)))))

  (should (equal (list 1 2 t)
                 (loopy--pcase-exhaustive-wrapper (a b b-supplied)
                     (list :a 1 :b 2)
                   ((loopy (&key a (b 13 b-supplied)))
                    (list a b b-supplied)))))

  (should (equal (list 1 2 t)
                 (loopy--pcase-exhaustive-wrapper (a b b-supplied)
                     (list :a 1 :bat 2)
                   ((loopy (&key a ((:bat b) 13 b-supplied)))
                    (list a b b-supplied)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list :a 1 :bat 2)
                   ((loopy (&key a ((:bat b) 13)))
                    (list a b)))))

  (should (equal (list 1 13)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list :a 1)
                   ((loopy (&key a ((:bat b) 13)))
                    (list a b)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list :a 1 :bat 2)
                   ((loopy (&key a ((:bat b))))
                    (list a b)))))

  (should (equal (list 1 nil)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list :a 1)
                   ((loopy (&key a ((:bat b))))
                    (list a b)))))

  (should (equal (list 1 2 t)
                 (let ((key :bat))
                   (loopy--pcase-exhaustive-wrapper (a b b-supplied)
                       (list :a 1 :bat 2)
                     ((loopy (&key a ((key b) 13 b-supplied)))
                      (list a b b-supplied)))))))

(ert-deftest pcase-tests-loopy-&map-full-form ()
  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list 'a 1 'b 2)
                   ((loopy (&map a ('b b 13)))
                    (list a b)))))

  (should (equal (list 1 13)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list 'a 1)
                   ((loopy (&map a ('b b 13)))
                    (list a b)))))

  (should (equal (list 1 13 nil)
                 (loopy--pcase-exhaustive-wrapper (a b b-supplied)
                     (list 'a 1)
                   ((loopy (&map a ('b b 13 b-supplied)))
                    (list a b b-supplied)))))

  (should (equal (list 1 2 t)
                 (loopy--pcase-exhaustive-wrapper (a b b-supplied)
                     (list 'a 1 'b 2)
                   ((loopy (&map a ('b b 13 b-supplied)))
                    (list a b b-supplied)))))

  (should (equal (list 1 2 t)
                 (loopy--pcase-exhaustive-wrapper (a b b-supplied)
                     (list :a 1 :bat 2)
                   ((loopy (&map (:a a) (:bat b 13 b-supplied)))
                    (list a b b-supplied)))))

  (should (equal (list 1 2 t)
                 (let ((key :bat))
                   (loopy--pcase-exhaustive-wrapper (a b b-supplied)
                       (list :a 1 :bat 2)
                     ((loopy (&map (:a a) (key b 13 b-supplied)))
                      (list a b b-supplied)))))))

(ert-deftest pcase-tests-loopy-&key-sub-seq ()
  (should (equal '(1 2 (:c 77 :e should-ignore) nil 77 t 99 nil)
                 (loopy--pcase-exhaustive-wrapper
                     (a b cd cd-supp c c-supp d d-supp)
                     '(:ab (1 2))
                   ((loopy (&key
                            ((:ab (a b)))
                            ((:cd ( &whole cd
                                    &key
                                    (c 88 c-supp)
                                    ((:d d) 99 d-supp)
                                    &allow-other-keys))
                             (list :c 77 :e 'should-ignore)
                             cd-supp)))
                    (list a b cd cd-supp c c-supp d d-supp)))))

  (should (equal '( 1 2 (:c 77 :e should-ignore :allow-other-keys t) nil
                    77 t 99 nil)
                 (loopy--pcase-exhaustive-wrapper
                     (a b cd cd-supp c c-supp d d-supp)
                     '(:ab (1 2))
                   ((loopy (&key
                            ((:ab (a b)))
                            ((:cd ( &whole cd
                                    &key
                                    (c 88 c-supp)
                                    ((:d d) 99 d-supp)))
                             (list :c 77 :e 'should-ignore
                                   :allow-other-keys t)
                             cd-supp)))
                    (list a b cd cd-supp c c-supp d d-supp)))))

  (should (equal nil
                 (loopy--pcase-exhaustive-wrapper
                     (a b cd cd-supp c c-supp d d-supp)
                     '(:ab (1 2))
                   ((loopy (&key
                            ((:ab (a b)))
                            ((:cd ( &whole cd
                                    &key
                                    (c 88 c-supp)
                                    ((:d d) 99 d-supp)))
                             (list :c 77 :e 'should-fail)
                             cd-supp)))
                    (list a b cd cd-supp c c-supp d d-supp))
                   (_ nil)))))

(ert-deftest pcase-tests-loopy-&map-sub-seq ()
  (should (equal '(1 2 (:c 77 :e should-ignore) nil 77 t 99 nil)
                 (loopy--pcase-exhaustive-wrapper
                     (a b cd cd-supp c c-supp d d-supp)
                     '(:ab (1 2))
                   ((loopy (&map
                            (:ab (a b))
                            (:cd ( &whole cd
                                   &map
                                   (:c c 88 c-supp)
                                   (:d d 99 d-supp))
                                 (list :c 77 :e 'should-ignore)
                                 cd-supp)))
                    (list a b cd cd-supp c c-supp d d-supp))))))

(ert-deftest pcase-tests-loopy-&aux-should-error ()
  "`&aux' cannot be used after `&aux'."
  (should-error (loopy--pcase-exhaustive-wrapper (a b)
                    nil
                  ((loopy (&aux a &aux b))
                   (list a b)))
                :type 'loopy-&aux-bad-position))

(ert-deftest pcase-tests-loopy-&aux ()
  (should (equal (list 1 2 nil nil)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     nil
                   ((loopy (&aux (a 1) (b 2) (c) d))
                    (list a b c d)))))

  (should (equal (list 0 1 2 nil nil)
                 (loopy--pcase-exhaustive-wrapper (z0 a b c d)
                     (list 0)
                   ((loopy (z0 &aux (a 1) (b 2) (c) d))
                    (list z0 a b c d))))))

(ert-deftest pcase-tests-loopy-&aux-sub-seq ()
  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     nil
                   ((loopy (&aux ((a b) (list 1 2))))
                    (list a b))))))

(ert-deftest pcase-tests-loopy-all ()
  (should (equal '(1 2 3 4 5 (:k1 111 :k2 222) 111 222 111 222 333 444)
                 (loopy--pcase-exhaustive-wrapper
                     (a b c d e r k1 k2 map1 map2 x1 x2)
                     (list 1 2 3 4 5 :k1 111 :k2 222)
                   ((loopy ( a b c
                             &optional d e
                             &rest r
                             &key k1 k2
                             &map (:k1 map1) (:k2 map2)
                             &aux (x1 333) (x2 444)))
                    (list a b c d e r k1 k2 map1 map2 x1 x2))))))
