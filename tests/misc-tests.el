;; Tests of secondary features and helper functions.  -*- lexical-binding: t; -*-

(require 'cl-lib)

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

(ert-deftest loopy--seq-length= ()
  (should (equal t (loopy--seq-length= '(1 2 3) 3)))
  (should (equal t (loopy--seq-length= [1 2 3 4] 4)))
  (should (equal t (loopy--seq-length= (stream [1 2 3 4 5]) 5))))

(ert-deftest loopy--seq-length> ()
  (should (equal t (loopy--seq-length> '(1 2 3) 2)))
  (should (equal t (loopy--seq-length> [1 2 3 4] 3)))
  (should (equal t (loopy--seq-length> (stream [1 2 3 4 5]) 4))))

(ert-deftest list-too-short ()
  (should-error (loopy-let* (((a b c) '(a b)))
                  (list a b c))
                :type '(loopy-bad-run-time-destructuring (loopy (a b c)) (a b))))

(ert-deftest stream-too-short ()
  (should-error (loopy-let* (((&seq a b c) (stream '(a b))))
                  (list a b c))
                :type '(loopy-bad-run-time-destructuring (loopy (a b c)) (a b))))

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

;;;; `loopy-let*'

;; `loopy-let*' is just a version of `pcase', so the individual
;; destructuring features are covered by `pcase'.  We only need to
;; test that the macro expands correctly.


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

;;;; Generalized variables

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

  (should (equal '(1 2 3 5 6)
                 (eval (quote (loopy-ref (((a b c &map (:d d) (:e e)) '(1 2 3 :e 6 :d 5)))
                                (list a b c d e))))))

  (should (equal '(1 2 3 5 6 (:e 6 :d 5))
                 (eval (quote (loopy-ref (((a b c &rest rest &key d e)
                                           '(1 2 3 :e 6 :d 5)))
                                (list a b c d e rest))))))

  (should (equal '(1 2 3 5 6 (:e 6 :d 5))
                 (eval (quote (loopy-ref (((a b c &rest rest &map (:d d) (:e e))
                                           '(1 2 3 :e 6 :d 5)))
                                (list a b c d e rest))))))

  (should (equal '(1 2 3 5 6 (:e 6 :d 5))
                 (eval (quote (loopy-ref (((a b c &map (:d d) (:e e) . rest)
                                           '(1 2 3 :e 6 :d 5)))
                                (list a b c d e rest))))))

  (should (equal '(1 2 3 5 6 (:e 6 :d 5))
                 (eval (quote (loopy-ref (((a b c &key d e &rest rest)
                                           '(1 2 3 :e 6 :d 5)))
                                (list a b c d e rest))))))

  (should (equal '(5 6)
                 (eval (quote (loopy-ref (((&key d e) '(1 2 :e 6 :d 5)))
                                (list d e))))))

  (should (equal '(5 6)
                 (eval (quote (loopy-ref (((&map d e) '(1 2 e 6 d 5)))
                                (list d e)))))))

(defmacro loopy-def-loopy-ref-test (base-name &rest args)
  "Create variants of test BASE-NAME.

The valid keys are:

- `:doc': Documentation of the test.
- `:name': Name of the variant.
- `:val': Value to be destructured.
- `:var': Variables used in destructuring.
- `:do': How the destructuring should output.
         By default, a list of the variables used in
         destructuring in the order given in VAR.
- `:result': What the value of DO should be equal to.
- `:tests': A sequence of property lists containing
           any of the above keys, which override
           any values for the keys given outside
          the property list.

\(fn BASE-NAME &key DOC NAME VAL BAR RESULT PAT DO TESTS)"
  (declare (indent 1))
  (cl-labels ((loopy--dpt-internal-expander (plist)
                (if-let ((tests (plist-get plist :tests)))
                    (cons 'progn
                          (mapcar (let ((new-plist `( :tests nil ,@plist)))
                                    (lambda (elt)
                                      (loopy--dpt-internal-expander
                                       (append elt new-plist))))
                                  tests))
                  (map-let ((:base base)
                            (:doc doc)
                            (:name name)
                            (:val val)
                            (:var var)
                            (:result result)
                            (:pat pat)
                            (:do do))
                      plist
                    `(ert-deftest ,(intern (concat (symbol-name base) "-" (symbol-name name)))
                         ,doc
                       ()
                       (should (equal ,result
                                      (eval (quote (let ,(cl-loop
                                                          for v in var
                                                          collect
                                                          `(,v 'loopy--intentionally-bad-val))
                                                     (loopy-ref ((,pat ,val))
                                                       ,(or do (cons 'list var)))))
                                            t))))))))
    (let ((output (loopy--dpt-internal-expander (append (list :base base-name)
                                                        args))))
      (if (memq (car output) '(ert-deftest progn))
          output
        (cons 'progn output)))))



(loopy-def-loopy-ref-test destructure-seq-ref-values
  :result (list 1 2 3)
  :var (a b c)
  :tests [( :val (list 1 2 3)
            :tests [(:name list-as-list :pat (&seq a b c))
                    (:name list-as-array :pat [&seq a b c])])
          ( :val (vector 1 2 3)
            :tests [(:name vector-as-list :pat (&seq a b c))
                    (:name vector-as-array :pat [&seq a b c])])])

(loopy-def-loopy-ref-test destructure-seq-ref-subseq-values
  :result (list 1 2 3 4)
  :var (a b c d)
  :tests [( :val (list 1 2 (list 3 4))
            :tests [(:name list-in-list-as-list-1 :pat (&seq a b (c d)))
                    (:name list-in-list-as-array-1 :pat [&seq a b (c d)])
                    (:name list-in-list-as-list-2 :pat (&seq a b (&seq c d)))
                    (:name list-in-list-as-array-2 :pat [&seq a b (&seq c d)])
                    (:name list-in-list-as-list-3 :pat (&seq a b [&seq c d]))
                    (:name list-in-list-as-array-4 :pat [&seq a b [&seq c d]])])
          ( :val (list 1 2 (vector 3 4))
            :tests [(:name vector-in-list-as-list-1 :pat (&seq a b [c d]))
                    (:name vector-in-list-as-array-1 :pat [&seq a b [c d]])
                    (:name vector-in-list-as-list-2 :pat (&seq a b (&seq c d)))
                    (:name vector-in-list-as-array-2 :pat [&seq a b (&seq c d)])
                    (:name vector-in-list-as-list-3 :pat (&seq a b [&seq c d]))
                    (:name vector-in-list-as-array-4 :pat [&seq a b [&seq c d]])])
          ( :val (vector 1 2 (list 3 4))
            :tests [(:name list-in-vector-as-list-1 :pat (&seq a b (c d)))
                    (:name list-in-vector-as-array-1 :pat [&seq a b (c d)])
                    (:name list-in-vector-as-list-2 :pat (&seq a b (&seq c d)))
                    (:name list-in-vector-as-array-2 :pat [&seq a b (&seq c d)])
                    (:name list-in-vector-as-list-3 :pat (&seq a b [&seq c d]))
                    (:name list-in-vector-as-array-4 :pat [&seq a b [&seq c d]])])
          ( :val (vector 1 2 (vector 3 4))
            :tests [(:name vector-in-vector-as-list-1 :pat (&seq a b [c d]))
                    (:name vector-in-vector-as-array-1 :pat [&seq a b [c d]])
                    (:name vector-in-vector-as-list-2 :pat (&seq a b (&seq c d)))
                    (:name vector-in-vector-as-array-2 :pat [&seq a b (&seq c d)])
                    (:name vector-in-vector-as-list-3 :pat (&seq a b [&seq c d]))
                    (:name vector-in-vector-as-array-4 :pat [&seq a b [&seq c d]])])])

(ert-deftest pcase-for-iteration-unique-values ()
  "This condition was found during documentation writing."
  (thread-last (loopy--pcase-destructure-for-iteration
                '(loopy [&seq i j &optional k &rest r])
                'val)
               (cl-second)
               (seq-group-by #'identity)
               (map-every-p (lambda (_ val) (= 1 (length val))))))

(ert-deftest destructure-lists-ref-&seq-values ()
  (should (equal '(1 2 3 4)
                 (eval (quote (loopy-ref (((&seq a b (c d)) '(1 2 (3 4))))
                                (list a b c d))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy-ref (((&seq a b (&seq c d)) '(1 2 (3 4))))
                                (list a b c d))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy-ref (((&seq a b &rest (c d)) '(1 2 3 4)))
                                (list a b c d))))))

  (should (equal '(1 2 (3 4))
                 (eval (quote (loopy-ref (((&seq a b . c) '(1 2 3 4)))
                                (list a b c))))))

  (should (equal '(1 2 (3 4))
                 (eval (quote (loopy-ref (((&seq a b &rest c) '(1 2 3 4)))
                                (list a b c))))))

  (should (equal '(1 2 (3 4))
                 (eval (quote (loopy-ref (([&seq a b &rest c] '(1 2 3 4)))
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

(ert-deftest destructure-&seq-list-ref-setf ()
  (should (equal '(1 2 3)
                 (let ((l (list 7 7 7)))
                   (loopy-ref (((&seq a b c) l))
                     (setf a 1 b 2 c 3)
                     l))))

  (should (equal '(1 2 (3 4))
                 (let ((l (list 7 7 (list 7 7))))
                   (loopy-ref (((&seq a b (c d)) l))
                     (setf a 1 b 2 c 3 d 4)
                     l))))

  (should (equal '(1 2 (3 4))
                 (let ((l (list 7 7 (list 7 7))))
                   (loopy-ref (((&seq a b (&seq c d)) l))
                     (setf a 1 b 2 c 3 d 4)
                     l))))

  (should (equal '(1 2 3 4)
                 (let ((l (list 7 7 7 7)))
                   (loopy-ref (((&seq a b &rest (c d)) l))
                     (setf a 1 b 2 c 3 d 4)
                     l))))

  (should (equal '(1 2 3 4)
                 (let ((l (list 7 7 7 7)))
                   (loopy-ref (((&seq a b &rest (&seq c d)) l))
                     (setf a 1 b 2 c 3 d 4)
                     l))))

  ;; NOTE: This tests is not generic enough for seq.el:
  ;; (should (equal '(1 2 . 3)
  ;;                (let ((l (list 7 7 7 7)))
  ;;                  (loopy-ref (((&seq a b . c) l))
  ;;                    (setf a 1 b 2 c 3))
  ;;                  l)))

  (should (equal '(1 2 3 :d 4 :e 5)
                 (let ((l (list 7 7 7 :d 7 :e 7)))
                   (loopy-ref (((&seq a b c &map (:d d) (:e e)) l))
                     (setf a 1 b 2 c 3 d 4 e 5))
                   l)))

  (should (equal '(1 2 3 :e 10 :d 8)
                 (let ((l (list 7 7 7 :e 7 :d 7)))
                   (loopy-ref (((&seq a b c &rest rest &map (:d d) (:e e)) l))
                     (setf a 1 b 2 c 3 d 4 e 5
                           rest (mapcar (lambda (x)
                                          (if (numberp x)
                                              (* 2 x)
                                            x))
                                        rest)))
                   l)))

  (should (equal '(1 2 3 :e 10 :d 8)
                 (let ((l (list 7 7 7 :e 7 :d 7)))
                   (loopy-ref (((&seq a b c &map (:d d) (:e e) . rest) l))
                     (setf a 1 b 2 c 3 d 4 e 5
                           rest (mapcar (lambda (x)
                                          (if (numberp x)
                                              (* 2 x)
                                            x))
                                        rest)))
                   l)))

  (should (equal '(1 2 3 :e 10 :d 8)
                 (let ((l (list 7 7 7 :e 7 :d 7)))
                   (loopy-ref (((&seq a b c &map (:d d) (:e e) &rest rest) l))
                     (setf a 1 b 2 c 3 d 4 e 5
                           rest (mapcar (lambda (x)
                                          (if (numberp x)
                                              (* 2 x)
                                            x))
                                        rest)))
                   l)))

  (should (equal '(7 7 :a 1 :b 2)
                 (let ((l (list 7 7 :a 7 :b 7)))
                   (loopy-ref (((&seq &map (:a a) (:b b)) l))
                     (setf a 1 b 2))
                   l)))

  (should (equal '(2 3)
                 (eval (quote
                        (let ((l (list 7 7)))
                          (loopy-ref (((&seq &whole whole a b) l))
                            (setf a 1 b 2
                                  whole (mapcar #'1+ whole)))
                          l))))))

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

(ert-deftest destructure-&seq-array-refs ()
  (should (equal [1 2 3]
                 (let ((arr (vector 7 7 7)))
                   (loopy-ref (([&seq a b c] arr))
                     (setf a 1 b 2 c 3))
                   arr)))

  (should (equal [1 2 3 99]
                 (let ((arr (vector 7 7 7 27)))
                   (loopy-ref (([&seq a b c &map [0 d]] arr))
                     (setf a 1 b 2 c 3 d 99))
                   arr)))

  (should (equal [2 3 4]
                 (let ((arr (vector 7 7 7)))
                   (loopy-ref (([&seq &whole whole a b c] arr))
                     (setf a 1 b 2 c 3
                           whole (cl-map 'vector #'1+ whole)))
                   arr)))

  (should (equal [1 2 3 [4 5]]
                 (let ((arr (vector 7 7 7 (vector 7 7))))
                   (loopy-ref (([&seq a b c [d e]] arr))
                     (setf a 1 b 2 c 3 d 4 e 5))
                   arr)))

  (should (equal [1 2 3 [4 5]]
                 (let ((arr (vector 7 7 7 (vector 7 7))))
                   (loopy-ref (([&seq a b c [&seq d e]] arr))
                     (setf a 1 b 2 c 3 d 4 e 5))
                   arr)))

  (should (equal [1 2 3 [4 5]]
                 (let ((arr (vector 7 7 7 (vector 7 7))))
                   (loopy-ref (([&seq a b c (&seq d e)] arr))
                     (setf a 1 b 2 c 3 d 4 e 5))
                   arr)))

  ;; NOTE: Setting a variable after `&rest' in an array will not truncate the array.
  (should (equal [1 2 3 4 7]
                 (let ((arr (vector 7 7 7 7 7)))
                   (loopy-ref (([&seq a b c &rest d] arr))
                     (setf a 1 b 2 c 3 d [4]))
                   arr)))

  (should (equal [1 2 3 4 7]
                 (let ((arr (vector 7 7 7 7 7)))
                   (loopy-ref (([&seq a b c &rest d] arr))
                     (setf a 1 b 2 c 3 d [4]))
                   arr)))

  (should (equal [2 3]
                 (let ((arr (vector 7 7)))
                   (loopy-ref (([&seq &whole cat a b] arr))
                     (setf a 1 b 2
                           cat (cl-map 'vector #'1+ cat)))
                   arr))))

(ert-deftest recursive-gv-setting-tests ()
  "These found in issue #184.

The current solution is to use custom GV setters via wrappers for the
builtin `aref', `seq-drop', and the custom
`loopy--destructure-seq-drop'."
  (should (equal '(1 2 3 4 5)
                 (eval (quote
                        (let ((arr (list 7 7 7 7 7)))
                          (loopy-ref (([&seq a b c &rest [&seq d e]] arr))
                            (setf a 1 b 2 c 3 d 4 e 5))
                          arr)))))

  (should (equal '(7 7 7 4 7)
                 (let ((arr (list 7 7 7 7 7)))
                   (setf (loopy--destructure-seq-elt
                          (nthcdr 3 arr)
                          0)
                         4)
                   arr)))

  (should (equal '(7 7 7 4 7)
                 (let ((arr (list 7 7 7 7 7)))
                   (setf (nth
                          0 (nthcdr 3 arr))
                         4)
                   arr)))

  (should (equal '(7 7 7 4 7)
                 (let ((arr (list 7 7 7 7 7)))
                   (setf (nth
                          0
                          (loopy--destructure-seq-drop arr 3))
                         4)
                   arr)))

  (should (equal [1 2 3 4 5]
                 (eval (quote
                        (let ((arr (vector 7 7 7 7 7)))
                          (loopy-ref (([&seq a b c &rest [&seq d e]] arr))
                            (setf a 1 b 2 c 3 d 4 e 5))
                          arr)))))

  (should (equal [7 7 7 4 7]
                 (let ((arr (vector 7 7 7 7 7)))
                   (setf (loopy--destructure-seq-elt
                          (loopy--destructure-seq-drop arr 3)
                          0)
                         4)
                   arr)))

  (should (equal [1 2 3 4 5]
                 (eval (quote
                        (let ((arr (vector 7 7 7 7 7)))
                          (loopy-ref (([&seq a b c &rest [d e]] arr))
                            (setf a 1 b 2 c 3 d 4 e 5))
                          arr)))))

  (should (equal [7 7 7 4 7]
                 (let ((arr (vector 7 7 7 7 7)))
                   (setf (loopy--destructure-seq-elt
                          (cl-subseq arr 3)
                          0)
                         4)
                   arr)))

  (should (equal [1 2 3 4 5]
                 (eval (quote
                        (let ((arr (vector 7 7 7 7 7)))
                          (loopy-ref (([a b c &rest [&seq d e]] arr))
                            (setf a 1 b 2 c 3 d 4 e 5))
                          arr)))))

  (should (equal [7 7 7 4 7]
                 (let ((arr (vector 7 7 7 7 7)))
                   (setf (loopy--destructure-seq-elt
                          (loopy--destructure-seq-drop arr 3)
                          0)
                         4)
                   arr)))

  (should (equal [7 7 7 4 7]
                 (let ((arr (vector 7 7 7 7 7)))
                   (setf (loopy--destructure-aref
                          (loopy--destructure-seq-drop arr 3)
                          0)
                         4)
                   arr)))

  (should (equal '(7 7 7 4 7)
                 (let ((lst (list 7 7 7 7 7)))
                   (setf (nth 0 (loopy--destructure-seq-drop lst 3))
                         4)
                   lst)))

  (should (equal [1 2 3 4 0 0 16]
                 (let ((arr (vector 7 7 7 7 0 0 6)))
                   (loopy-ref (([&seq a b c &rest d &map (3 sub-idx-3)] arr))
                     (setf a 1 b 2 c 3 d [4])
                     (cl-incf sub-idx-3 10))
                   arr)))

  (should (equal [7 7 7 7 0 0 16]
                 (let ((arr (vector 7 7 7 7 0 0 6)))
                   (cl-incf (loopy--destructure-map-elt
                             (loopy--destructure-seq-drop arr 3)
                             3)
                            10)
                   arr)))

  (should (equal [1 2 3 4 0 0 16]
                 (let ((arr (vector 7 7 7 7 0 0 6)))
                   (loopy-ref (([a b c &rest d &map (3 sub-idx-3)] arr))
                     (setf a 1 b 2 c 3 d [4])
                     (cl-incf sub-idx-3 10))
                   arr)))

  (should (equal [7 7 7 7 0 0 16]
                 (let ((arr (vector 7 7 7 7 0 0 6)))
                   (cl-incf (loopy--destructure-map-elt
                             (loopy--destructure-cl-subseq arr 3)
                             3)
                            10)
                   arr)))

  (should (equal [1 2 3 4 0 0 16]
                 (let ((arr (vector 7 7 7 7 0 0 6)))
                   (loopy-ref (([&seq a b c &rest d &map (3 sub-idx-3)] arr))
                     (setf a 1 b 2 c 3 d [4])
                     (cl-incf sub-idx-3 10))
                   arr)))

  (should (equal [7 7 7 7 0 0 16]
                 (let ((arr (vector 7 7 7 7 0 0 6)))
                   (cl-incf (loopy--destructure-map-elt
                             (loopy--destructure-seq-drop arr 3)
                             3)
                            10)
                   arr))))

(ert-deftest gv-destr-simplification-seq-elt-on-sub ()
  "`seq-elt' on sub-sequence."
  (should (equal '(seq-elt my-list 3)
                 (macroexpand-all '(loopy--destructure-gv-seq-elt-simplifier
                                    (loopy--destructure-gv-list-rest-simplifier
                                     my-list 3)
                                    0))))

  (should (equal '(seq-elt my-list 4)
                 (macroexpand-all '(loopy--destructure-gv-seq-elt-simplifier
                                    (loopy--destructure-gv-array-rest-simplifier
                                     my-list 3)
                                    1))))

  (should (equal '(seq-elt my-list 5)
                 (macroexpand-all '(loopy--destructure-gv-seq-elt-simplifier
                                    (loopy--destructure-gv-seq-rest-simplifier
                                     my-list 3)
                                    2)))))

(ert-deftest gv-destr-simplification-seq-rest-on-sub ()
  "`seq-drop' on sub-sequence."
  (should (equal '(seq-drop my-list 3)
                 (macroexpand-all '(loopy--destructure-gv-seq-rest-simplifier
                                    (loopy--destructure-gv-list-rest-simplifier
                                     my-list 3)
                                    0))))

  (should (equal '(seq-drop my-list 4)
                 (macroexpand-all '(loopy--destructure-gv-seq-rest-simplifier
                                    (loopy--destructure-gv-array-rest-simplifier
                                     my-list 3)
                                    1))))

  (should (equal '(seq-drop my-list 5)
                 (macroexpand-all '(loopy--destructure-gv-seq-rest-simplifier
                                    (loopy--destructure-gv-seq-rest-simplifier
                                     my-list 3)
                                    2)))))

(ert-deftest gv-destr-simplification-list-elt-on-sub ()
  "`nth' on sub-sequence."
  (should (equal '(nth 3 my-list)
                 (macroexpand-all '(loopy--destructure-gv-list-elt-simplifier
                                    (loopy--destructure-gv-list-rest-simplifier
                                     my-list 3)
                                    0))))

  (should (equal '(nth 5 my-list)
                 (macroexpand-all '(loopy--destructure-gv-list-elt-simplifier
                                    (loopy--destructure-gv-seq-rest-simplifier
                                     my-list 3)
                                    2)))))

(ert-deftest gv-destr-simplification-list-rest-on-sub ()
  "`nthcdr' on sub-sequence."
  (should (equal '(nthcdr 3 my-list)
                 (macroexpand-all '(loopy--destructure-gv-list-rest-simplifier
                                    (loopy--destructure-gv-seq-rest-simplifier
                                     my-list 3)
                                    0))))

  (should (equal '(nthcdr 5 my-list)
                 (macroexpand-all '(loopy--destructure-gv-list-rest-simplifier
                                    (loopy--destructure-gv-list-rest-simplifier
                                     my-list 3)
                                    2)))))

(ert-deftest gv-destr-simplification-array-elt-on-sub ()
  "`aref' on sub-sequence."
  (should (equal '(aref my-array 3)
                 (macroexpand-all '(loopy--destructure-gv-array-elt-simplifier
                                    (loopy--destructure-gv-array-rest-simplifier
                                     my-array 3)
                                    0))))

  (should (equal '(aref my-array 5)
                 (macroexpand-all '(loopy--destructure-gv-array-elt-simplifier
                                    (loopy--destructure-gv-seq-rest-simplifier
                                     my-array 3)
                                    2)))))

(ert-deftest gv-destr-simplification-array-rest-on-sub ()
  "`cl-subseq' on sub-sequence."
  (should (equal '(cl-subseq my-array 3)
                 (macroexpand-all '(loopy--destructure-gv-array-rest-simplifier
                                    (loopy--destructure-gv-seq-rest-simplifier
                                     my-array 3)
                                    0))))

  (should (equal '(cl-subseq my-array 5)
                 (macroexpand-all '(loopy--destructure-gv-array-rest-simplifier
                                    (loopy--destructure-gv-array-rest-simplifier
                                     my-array 3)
                                    2)))))

(ert-deftest gv-use-of-list-simplifiers ()
  (should (equal (loopy--destructure-generalized-list '(a . b) 'my-val)
                 '((a (loopy--destructure-gv-list-elt-simplifier my-val 0))
                   (b (loopy--destructure-gv-list-rest-simplifier my-val 1)))))

  (should (equal (loopy--destructure-generalized-list '(a &rest b) 'my-val)
                 '((a (loopy--destructure-gv-list-elt-simplifier my-val 0))
                   (b (loopy--destructure-gv-list-rest-simplifier my-val 1))))))

(ert-deftest gv-use-of-array-simplifiers ()
  (should (equal (loopy--destructure-generalized-array [a &rest b] 'my-val)
                 '((a (loopy--destructure-gv-array-elt-simplifier my-val 0))
                   (b (loopy--destructure-gv-array-rest-simplifier my-val 1))))))

(ert-deftest gv-use-of-seq-simplifiers ()
  (should (equal (loopy--destructure-generalized-sequence '(&seq a &rest b) 'my-val)
                 '((a (loopy--destructure-gv-seq-elt-simplifier my-val 0))
                   (b (loopy--destructure-gv-seq-rest-simplifier my-val 1)))))

  (should (equal (loopy--destructure-generalized-sequence [&seq a &rest b] 'my-val)
                 '((a (loopy--destructure-gv-seq-elt-simplifier my-val 0))
                   (b (loopy--destructure-gv-seq-rest-simplifier my-val 1))))))

(ert-deftest destructure-array-refs ()
  (should (equal [1 2 3]
                 (let ((arr (vector 7 7 7)))
                   (loopy-ref (([a b c] arr))
                     (setf a 1 b 2 c 3))
                   arr)))

  (should (equal [2 3 4]
                 (let ((arr (vector 7 7 7)))
                   (loopy-ref (([&whole whole a b c] arr))
                     (setf a 1 b 2 c 3
                           whole (cl-map 'vector #'1+ whole)))
                   arr)))

  (should (equal [1 2 3 [4 5]]
                 (let ((arr (vector 7 7 7 (vector 7 7))))
                   (loopy-ref (([a b c [d e]] arr))
                     (setf a 1 b 2 c 3 d 4 e 5))
                   arr)))

  ;; NOTE: These two test currently passes due to how we simplify array indexing
  ;;       to avoid creating new objects, but it doesn't work in the general
  ;;       case, such as with `&seq'.
  (should (equal [1 2 3 4 5]
                 (eval (quote
                        (let ((arr (vector 7 7 7 7 7)))
                          (loopy-ref (([a b c &rest [d e]] arr))
                            (setf a 1 b 2 c 3 d 4 e 5))
                          arr)))))

  (should (equal [1 2 3 4 5 6]
                 (eval (quote
                        (let ((arr (vector 7 7 7 7 7 7)))
                          (loopy-ref (([a b c &rest [d &rest [e f]]] arr))
                            (setf a 1 b 2 c 3 d 4 e 5 f 6))
                          arr)))))

  (should (equal [1 2 3 4 5]
                 (eval (quote
                        (let ((arr (vector 7 7 7 7 7 ))
                              (key1 0)
                              (key2 1))
                          (loopy-ref (([a b c &rest [&map (key1 d) (key2 e)]] arr))
                            (setf a 1 b 2 c 3 d 4 e 5))
                          arr)))))

  ;; NOTE: Setting a variable after `&rest' in an array will not truncate the array.
  (should (equal [1 2 3 4 7]
                 (let ((arr (vector 7 7 7 7 7)))
                   (loopy-ref (([a b c &rest d] arr))
                     (setf a 1 b 2 c 3 d [4]))
                   arr)))

  (should (equal [1 2 3 4 7]
                 (let ((arr (vector 7 7 7 7 7)))
                   (loopy-ref (([a b c &rest d] arr))
                     (setf a 1 b 2 c 3 d [4]))
                   arr)))

  (should (equal [2 3]
                 (let ((arr (vector 7 7)))
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
(define-error 'loopy-pcase-no-match "No match found.")
(defmacro loopy--pcase-exhaustive-wrapper (vars val &rest branches)
  "Wrap variables to make sure that they're bound on earlier versions of Emacs.

Prior to Emacs 28, `pcase' didn't guarantee binding all variables.

- VARS is the list of variables.
- VAL is the value to match against.
- BRANCHES are the `pcase' branches."
  (declare (indent 2))
  `(let ,(mapcar (lambda (v)
                   `(,v 'intentionally-bad-test-val))
                 vars)
     (or (pcase ,val
           ,@branches)
         (signal 'loopy-pcase-no-match
                 (quote (,val ,@branches)))))
  ;; `(eval (quote (let ,(mapcar (lambda (v)
  ;;                               `(,v 'intentionally-bad-test-val))
  ;;                             vars)
  ;;                 (pcase ,val
  ;;                   ,@branches
  ;;                   (_ (signal 'loopy-pcase-no-match
  ;;                              (quote (,val ,@branches)))))))
  ;;        t)
  )

(defun loopy--pcase-convert (seq type &optional recursive)
  "Convert SEQ into TYPE, optionally RECURSIVE."
  (seq-into (seq-map (lambda (x)
                       (if (and recursive (seqp x))
                           (loopy--pcase-convert x type recursive)
                         x))
                     seq)
            type))

(cl-defmacro loopy-def-pcase-test (name &key doc result pat do var val
                                        vector-result
                                        list-result
                                        (error nil)
                                        (convert t)
                                        (list t)
                                        (seq t)
                                        (seq-vector 1)
                                        (vector t)
                                        )
  "Create variant of test.

- ERROR is whether it should error.
- RESULT is the output for `equal.'
- PAT is the pattern for `(loopy PAT)'
- DO is the last expression in the `pcase' branch
- VAR is a list of variables to be set during the test.
- VAL is the matched value
- CONVERT means convert VAL to the tested type.  If `recursive'
  then also convert the subsequences in the pattern and the result.
- LIST, SEQ, and VECTOR are the kinds of sequences to convert into.
  SEQ-VECTOR means to ignore the vector version of the `&seq' pattern."
  (declare (indent 1))
  (let ((do (or do `(list ,@var)))
        (str-name (symbol-name name)))
    `(progn
       ,(when list
          `(ert-deftest ,(intern (concat str-name "-list")) ()
             ,doc
             ,(let ((body `(loopy--pcase-exhaustive-wrapper ,var
                               ,(if convert
                                    `(loopy--pcase-convert ,val 'list ,(eq convert 'recursive))
                                  val)
                             ((loopy ,(loopy--pcase-convert pat 'list (eq convert 'recursive)))
                              ,do))))
                (if error
                    `(should-error ,body :type 'loopy-pcase-no-match)
                  `(should (equal ,(or list-result result) ,body))))))

       ,(when vector
          `(ert-deftest ,(intern (concat str-name "-vector")) ()
             ,doc
             ,(let ((body `(loopy--pcase-exhaustive-wrapper ,var
                               ,(if convert
                                    `(loopy--pcase-convert ,val 'vector ,(eq convert 'recursive))
                                  val)
                             ((loopy ,(loopy--pcase-convert pat 'vector (eq convert 'recursive)))
                              ,do))))
                (if error
                    `(should-error ,body :type 'loopy-pcase-no-match)
                  `(should (equal ,(or vector-result result) ,body))))))

       ,(when seq
          (let ((new-pat (if (listp pat)
                             (cons '&seq pat)
                           (vconcat [&seq] pat))))
            `(progn
               (ert-deftest ,(intern (concat str-name "-&seq-list-as-list")) ()
                 ,doc
                 ,(let ((body `(loopy--pcase-exhaustive-wrapper ,var
                                   ,(if convert
                                        `(loopy--pcase-convert ,val 'list ,(eq convert 'recursive))
                                      val)
                                 ((loopy ,(loopy--pcase-convert new-pat 'list (eq convert 'recursive)))
                                  ,do))))
                    (if error
                        `(should-error ,body :type 'loopy-pcase-no-match)
                      `(should (equal ,(or list-result result) ,body)))))

               (ert-deftest ,(intern (concat str-name "-&seq-vector-as-list")) ()
                 ,doc
                 ,(let ((body `(loopy--pcase-exhaustive-wrapper ,var
                                   ,(if convert
                                        `(loopy--pcase-convert ,val 'vector ,(eq convert 'recursive))
                                      val)
                                 ((loopy ,(loopy--pcase-convert new-pat 'list (eq convert 'recursive)))
                                  ,do))))
                    (if error
                        `(should-error ,body :type 'loopy-pcase-no-match)
                      `(should (equal ,(or vector-result result) ,body)))))

               ,(when seq-vector
                  `(progn
                     (ert-deftest ,(intern (concat str-name "-&seq-list-as-vector")) ()
                       ,doc
                       ,(let ((body `(loopy--pcase-exhaustive-wrapper ,var
                                         ,(if convert
                                              `(loopy--pcase-convert ,val 'list ,(eq convert 'recursive))
                                            val)
                                       ((loopy ,(loopy--pcase-convert new-pat 'vector (eq convert 'recursive)))
                                        ,do))))
                          (if error
                              `(should-error ,body :type 'loopy-pcase-no-match)
                            `(should (equal ,(or list-result result) ,body)))))

                     (ert-deftest ,(intern (concat str-name "-&seq-vector-as-vector")) ()
                       ,doc
                       ,(let ((body `(loopy--pcase-exhaustive-wrapper ,var
                                         ,(if convert
                                              `(loopy--pcase-convert ,val 'vector ,(eq convert 'recursive))
                                            val)
                                       ((loopy ,(loopy--pcase-convert new-pat 'vector (eq convert 'recursive)))
                                        ,do))))
                          (if error
                              `(should-error ,body :type 'loopy-pcase-no-match)
                            `(should (equal ,(or vector-result result) ,body)))))))))))))

(push (list "Loopy Pcase Tests"
            (rx (0+ blank)
                "(loopy-def-pcase-test"
                (0+ (or (syntax symbol) (syntax word)))
                (1+ (syntax whitespace))
                (group-n 1 (1+ (or word (syntax symbol)))))
            1)
      imenu-generic-expression)

(defmacro loopy-def-pcase-test3 (base-name &rest args)
  "Create variants of test BASE-NAME.

The valid keys are:

- `:doc': Documentation of the test.
- `:name': Name of the variant.
- `:val': Value to be destructured.
- `:var': Variables used in destructuring.
- `:do': How the destructuring should output.
         By default, a list of the variables used in
         destructuring in the order given in VAR.
- `:result': What the value of DO should be equal to.
- `:tests': A sequence of property lists containing
           any of the above keys, which override
           any values for the keys given outside
          the property list.

\(fn BASE-NAME &key DOC NAME VAL BAR RESULT PAT DO TESTS)"
  (declare (indent 1))
  (cl-labels ((loopy--dpt-internal-expander (plist)
                (if-let ((tests (plist-get plist :tests)))
                    (cons 'progn
                          (mapcar (let ((new-plist `( :tests nil ,@plist)))
                                    (lambda (elt)
                                      (loopy--dpt-internal-expander
                                       (append elt new-plist))))
                                  tests))
                  (map-let ((:base base)
                            (:doc doc)
                            (:name name)
                            (:val val)
                            (:var var)
                            (:result result)
                            (:pat pat)
                            (:do do))
                      plist
                    `(ert-deftest ,(intern (concat (symbol-name base) "-" (symbol-name name)))
                         ,doc
                       ()
                       (should (equal ,result
                                      (loopy--pcase-exhaustive-wrapper
                                          ,var
                                          ,val
                                        ((loopy
                                          ,pat)
                                         ,(or do (cons 'list var)))))))))))
    (let ((output (loopy--dpt-internal-expander (append (list :base base-name)
                                                        args))))
      (if (memq (car output) '(ert-deftest progn))
          output
        (cons 'progn output)))))

(ert-deftest pcase-tests-loopy-&seq-should-error ()
  "`&seq' must come first if given, and must be followed by a patter."
  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&seq))
                   (list a b c)))
                :type 'loopy-bad-desctructuring)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&seq &rest))
                   (list a b c)))
                :type 'loopy-bad-desctructuring)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (a b &seq c))
                   (list a b c)))
                :type 'loopy-&seq-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&whole a &seq c))
                   (list a b c)))
                :type 'loopy-&seq-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&rest a &seq c))
                   (list a b c)))
                :type 'loopy-&seq-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&key a &seq c))
                   (list a b c)))
                :type 'loopy-&seq-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&aux (a 1) &seq c))
                   (list a b c)))
                :type 'loopy-&seq-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (a b c)
                    (list 1 2 3)
                  ((loopy (&optional (a 1) &seq c))
                   (list a b c)))
                :type 'loopy-&seq-bad-position)

  (should-error (loopy--pcase-exhaustive-wrapper (seq1 seq2)
                    (list 1 2 3)
                  ((loopy (&seq seq1 &seq seq2))
                   (list seq1 seq2)))
                :type 'loopy-&seq-bad-position))

(loopy-def-pcase-test pcase-tests-loopy-pos-1
  :doc "Positional variables must match the length or less of EXPVAL."
  :result (list 1 2 3)
  :val (list 1 2 3)
  :var (a b c)
  :pat (a b c)
  :do (list a b c))

(loopy-def-pcase-test pcase-tests-loopy-pos-2
  :doc "Positional variables must match the length or less of EXPVAL."
  :error t
  :val (list 1)
  :var (a b)
  :pat (a b))

(loopy-def-pcase-test pcase-tests-loopy-pos-3
  :doc "Positional variables must match the length or less of EXPVAL."
  :result (list 1 2 3)
  :val (list 1 2 3 4)
  :var (a b c)
  :pat (a b c))

(ert-deftest pcase-tests-loopy-pos-sub-seq ()
  (should (equal (list 1 2 3 4)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2 (vector 3 4))
                   ((loopy (&seq a b (&seq c d)))
                    (list a b c d)))))

  (should (equal (list 1 2 3 4)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (vector 1 2 (list 3 4))
                   ((loopy (&seq a b [&seq c d]))
                    (list a b c d)))))

  (should (equal (list 1 2 3 4)
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (vector 1 2 (vector 3 4))
                   ((loopy [&seq a b (&seq c d)])
                    (list a b c d)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list (vector 1 2))
                   ((loopy (&seq (&seq a b)))
                    (list a b)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (vector (list 1 2))
                   ((loopy (&seq [&seq a b]))
                    (list a b)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (vector (vector 1 2))
                   ((loopy [&seq (&seq a b)])
                    (list a b)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (list (list 1 2))
                   ((loopy [&seq [&seq a b]])
                    (list a b))))))

(loopy-def-pcase-test pcase-tests-loopy-&optional-1
  :val (list 1 2 3)
  :result (list 1 2 3)
  :var (a b c)
  :pat (a b &optional c)
  :do (list a b c))

(loopy-def-pcase-test pcase-tests-loopy-&optional-2
  :val (list 1 2)
  :result (list 1 2 nil)
  :var (a b c)
  :pat (a b &optional c)
  :do (list a b c))

(loopy-def-pcase-test pcase-tests-loopy-&optional-3
  :val (list 1 2)
  :result (list 1 2 13)
  :var (a b c)
  :pat (a b &optional (c 13))
  :do (list a b c))

(loopy-def-pcase-test pcase-tests-loopy-&optional-4
  :val (list 1 2)
  :result (list 1 2 13)
  :var (a b c)
  :pat (a b &optional [c 13])
  :do (list a b c))

(loopy-def-pcase-test pcase-tests-loopy-&optional-5
  :val (list 1 2)
  :result (list 1 2 13 nil)
  :var (a b c c-supplied)
  :pat (a b &optional (c 13 c-supplied)))

(loopy-def-pcase-test pcase-tests-loopy-&optional-6
  :val (list 1 2 3)
  :result (list 1 2 3 t)
  :var (a b c c-supplied)
  :pat (a b &optional [c 13 c-supplied]))

(loopy-def-pcase-test pcase-tests-loopy-&optional-ignored-1
  :result (list 1 2 nil)
  :val   (list 1 2)
  :var (a b d)
  :pat (a b &optional _ d))

(loopy-def-pcase-test pcase-tests-loopy-&optional-ignored-2
  :result (list 1 2)
  :val   (list 1 2)
  :var (a b)
  :pat (a b &optional _ _))

(loopy-def-pcase-test pcase-tests-loopy-&optional-ignored-3
  :result (list 1 2 13 nil)
  :val   (list 1 2)
  :var (a b k1 k2)
  :pat (a b &optional _ _ &key [k1 13] k2)
  :vector nil
  :seq nil)

(loopy-def-pcase-test pcase-tests-loopy-&optional-ignored-4
  :result (list 1 2 nil 14 nil)
  :val   (list 1 2)
  :var (a b e k1 k2)
  :pat (a b &optional _ _ &rest e &key [k1 14] k2)
  :vector nil
  :seq nil)

(loopy-def-pcase-test pcase-tests-loopy-&optional-ignored-5
  :result (list 1 2 nil 14 nil)
  :val   (list 1 2)
  :var (a b e k1 k2)
  :pat (a b &optional _ _ &rest e &map [:k1 k1 14] (:k2 k2))
  :vector nil
  :convert nil)

;; FIXME: This test fails on Emacs 27 because the tests don't install the
;;       correct version of Map.el.
(static-if (> emacs-major-version 27)
    (loopy-def-pcase-test pcase-tests-loopy-&optional-ignored-6
      :result (list 1 2 [] 14 nil)
      :val   (vector 1 2)
      :var (a b e k1 k2)
      :pat [a b &optional _ _ &rest e &map [:k1 k1 14] (:k2 k2)]
      :list nil
      :convert nil))

(loopy-def-pcase-test pcase-tests-loopy-&whole-1
  :result (list (list 1 2 3) 1 2 3)
  :val   (list 1 2 3)
  :var (whole a b c)
  :pat (&whole whole a b c)
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&whole-2
  :result (list (vector 1 2 3) 1 2 3)
  :val   (vector 1 2 3)
  :var (whole a b c)
  :pat (&whole whole a b c)
  :list nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&whole-3
  :result (list 1 2 3 1 2 3)
  :val   (vector 1 2 3)
  :var (a0 b0 c0 a b c)
  :pat (&whole `[,a0 ,b0 ,c0] a b c)
  :list nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&whole-4
  :result (list 1 2 3 1 2 3)
  :val   (list 1 2 3)
  :var (a0 b0 c0 a b c)
  :pat (&whole `(,a0 ,b0 ,c0) a b c)
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&whole-5
  :result (list 1 2 3 1 2 3)
  :val   (vector 1 2 3)
  :var (a0 b0 c0 a b c)
  :pat (&whole (loopy (&seq a0 b0 c0)) a b c)
  :vector nil)

(loopy-def-pcase-test3 pcase-tests-loopy-pos-sub-seq-1
  :var (a b c d)
  :result (list 1 2 3 4)
  :tests [( :val (list 1 2 (list 3 4))
            :tests [(:name list-1               :pat (a b (c d)))
                    (:name list-2               :pat (a b (&seq c d)))
                    (:name list-3               :pat (a b [&seq c d]))
                    (:name seq-list-as-list-1   :pat (&seq a b (c d)))
                    (:name seq-list-as-list-2   :pat (&seq a b (&seq c d)))
                    (:name seq-list-as-list-3   :pat (&seq a b [&seq c d]))
                    (:name seq-list-as-vector-1 :pat [&seq a b (c d)])
                    (:name seq-list-as-vector-2 :pat [&seq a b (&seq c d)])
                    (:name seq-list-as-vector-3 :pat [&seq a b [&seq c d]])])
          ( :val (vector 1 2 (vector 3 4))
            :tests [(:name vector-1               :pat [a b [c d]])
                    (:name vector-2               :pat [a b (&seq c d)])
                    (:name vector-3               :pat [a b [&seq c d]])
                    (:name seq-vector-as-list-1   :pat (&seq a b [c d]))
                    (:name seq-vector-as-list-2   :pat (&seq a b (&seq c d)))
                    (:name seq-vector-as-list-3   :pat (&seq a b [&seq c d]))
                    (:name seq-vector-as-vector-1 :pat [&seq a b [c d]])
                    (:name seq-vector-as-vector-2 :pat [&seq a b (&seq c d)])
                    (:name seq-vector-as-vector-3 :pat [&seq a b [&seq c d]])])])


(loopy-def-pcase-test3 pcase-tests-loopy-pos-sub-seq-2
  :var (a b)
  :result (list 1 2)
  :tests [( :val (list (list 1 2))
            :tests [(:name list-in-list-1               :pat ((a b)))
                    (:name seq-list-in-list-as-list-1   :pat (&seq (a b)))
                    (:name seq-list-in-list-as-list-2   :pat ((&seq a b)))
                    (:name seq-list-in-list-as-list-3   :pat (&seq (&seq a b)))
                    (:name seq-list-in-list-as-list-4   :pat (&seq [&seq a b]))
                    (:name seq-list-in-list-as-vector-1 :pat [&seq (a b)])
                    (:name seq-list-in-list-as-vector-2 :pat [&seq (&seq a b)])
                    (:name seq-list-in-list-as-vector-3 :pat [&seq [&seq a b]])])
          ( :val (list (vector 1 2))
            :tests [(:name vector-in-list-1               :pat ([a b]))
                    (:name seq-vector-in-list-as-list-1   :pat (&seq [a b]))
                    (:name seq-vector-in-list-as-list-2   :pat ((&seq a b)))
                    (:name seq-vector-in-list-as-list-3   :pat (&seq (&seq a b)))
                    (:name seq-vector-in-list-as-list-4   :pat (&seq [&seq a b]))
                    (:name seq-vector-in-list-as-vector-1 :pat [&seq [a b]])
                    (:name seq-vector-in-list-as-vector-2 :pat [&seq (&seq a b)])
                    (:name seq-vector-in-list-as-vector-3 :pat [&seq [&seq a b]])])
          ( :val (vector (list 1 2))
            :tests [(:name list-in-vector-1               :pat [(a b)])
                    (:name seq-list-in-vector-as-list-1   :pat (&seq (a b)))
                    (:name seq-list-in-vector-as-list-2   :pat [(&seq a b)])
                    (:name seq-list-in-vector-as-list-3   :pat (&seq (&seq a b)))
                    (:name seq-list-in-vector-as-list-4   :pat (&seq [&seq a b]))
                    (:name seq-list-in-vector-as-vector-1 :pat [&seq (a b)])
                    (:name seq-list-in-vector-as-vector-2 :pat [&seq (&seq a b)])
                    (:name seq-list-in-vector-as-vector-3 :pat [&seq [&seq a b]])])
          ( :val (vector (vector 1 2))
            :tests [(:name vector-in-vector-1               :pat [[a b]])
                    (:name seq-vector-in-vector-as-list-1   :pat (&seq [a b]))
                    (:name seq-vector-in-vector-as-list-2   :pat [(&seq a b)])
                    (:name seq-vector-in-vector-as-list-3   :pat (&seq (&seq a b)))
                    (:name seq-vector-in-vector-as-list-4   :pat (&seq [&seq a b]))
                    (:name seq-vector-in-vector-as-vector-1 :pat [&seq [a b]])
                    (:name seq-vector-in-vector-as-vector-2 :pat [&seq (&seq a b)])
                    (:name seq-vector-in-vector-as-vector-3 :pat [&seq [&seq a b]])])])

;; NOTE: These tests disabled while we figure out how we want this to
;;       behave.
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-1
;;   :result (list 1 2 3 4)
;;   :val (list 1 2 (list 3 4))
;;   :var (a b c d)
;;   :pat (a b &optional ((c d))))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-2
;;   :result (list 1 2 3 4)
;;   :val (list 1 2 (list 3 4))
;;   :var (a b c d)
;;   :pat (a b &optional [(c d)]))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-3
;;   :result (list 1 2 3 4)
;;   :val (list 1 2 (list 3 4))
;;   :var (a b c d)
;;   :pat (a b &optional ((&seq c d))))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-4
;;   :result (list 1 2 3 4)
;;   :val (list 1 2 (list 3 4))
;;   :var (a b c d)
;;   :pat (a b &optional [(&seq c d)]))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-5
;;   :result (list 1 2 nil nil)
;;   :val (list 1 2)
;;   :var (a b c d)
;;   :pat (a b &optional [(c d)]))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-6
;;   :result (list 1 2 nil nil)
;;   :val (list 1 2)
;;   :var (a b c d)
;;   :pat (a b &optional ((c d))))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-7
;;   :result (list 1 2 nil nil)
;;   :val (list 1 2)
;;   :var (a b c d)
;;   :pat (a b &optional ((&seq c d))))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-8
;;   :result (list 1 2 13 14)
;;   :val (list 1 2)
;;   :var (a b c d)
;;   :pat (a b &optional ((c d) (list 13 14))))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-9
;;   :result (list 1 2 13 14)
;;   :val (list 1 2)
;;   :var (a b c d)
;;   :pat (a b &optional ([c d] (vector 13 14))))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-10
;;   :result (list 1 2 13 14)
;;   :val (list 1 2)
;;   :var (a b c d)
;;   :pat (a b &optional ((&seq c d) (list 13 14))))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-11
;;   :result (list 1 2 13 14)
;;   :val (list 1 2)
;;   :var (a b c d)
;;   :pat (a b &optional ([&seq c d] (list 13 14))))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-12
;;   :result (list 1 2 13 14)
;;   :val (list 1 2)
;;   :var (a b c d)
;;   :pat (a b &optional ((c &optional (d 14)) (list 13))))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-13
;;   :result (list 1 2 13 14)
;;   :val (list 1 2)
;;   :var (a b c d)
;;   :pat (a b &optional ([c &optional (d 14)] (vector 13))))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-14
;;   :result (list 1 2 13 14 nil)
;;   :val (list 1 2)
;;   :var (a b c d cd-supplied)
;;   :pat (a b &optional ((&seq c d) (list 13 14) cd-supplied)))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-15
;;   :result (list 1 2 13 14 nil)
;;   :val (list 1 2)
;;   :var (a b c d cd-supplied)
;;   :pat (a b &optional ((c d) (list 13 14) cd-supplied)))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-16
;;   :result (list 1 2 13 14 nil)
;;   :val (list 1 2)
;;   :var (a b c d cd-supplied)
;;   :pat (a b &optional ([c d] (vector 13 14) cd-supplied)))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-17
;;   :result (list 1 2 13 14 nil t nil)
;;   :val (list 1 2)
;;   :var (a b c d cd-supplied c-sub-sup d-sub-sup)
;;   :pat  (a b &optional ((&optional (c 27 c-sub-sup) (d 14 d-sub-sup))
;;                         (list 13)
;;                         cd-supplied)))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-18
;;   :result (list 1 2 13 14 nil t nil)
;;   :val (list 1 2)
;;   :var (a b c d cd-supplied c-sub-sup d-sub-sup)
;;   :pat  (a b &optional ([&optional (c 27 c-sub-sup) (d 14 d-sub-sup)]
;;                         (vector 13)
;;                         cd-supplied)))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-19
;;   :result (list 1 2 13 14 nil t nil)
;;   :val (list 1 2)
;;   :var (a b c d cd-supplied c-sub-sup d-sub-sup)
;;   :pat  (a b &optional ([&seq &optional (c 27 c-sub-sup) (d 14 d-sub-sup)]
;;                         (vector 13)
;;                         cd-supplied)))
;;
;; (loopy-def-pcase-test pcase-tests-loopy-&optional-sub-seq-20
;;   :result (list 1 2 13 14 nil t nil)
;;   :val (list 1 2)
;;   :var (a b c d cd-supplied c-sub-sup d-sub-sup)
;;   :pat (a b &optional ((&seq &optional (c 27 c-sub-sup) [d 14 d-sub-sup])
;;                        (vector 13)
;;                        cd-supplied)))

(loopy-def-pcase-test pcase-tests-loopy-&rest-ignored-1
  :result (list 1 2)
  :val (list 1 2 3)
  :var (a b)
  :pat  (a b &rest _))

(loopy-def-pcase-test pcase-tests-loopy-&rest-ignored-2
  :result (list 1 2 3 11 12)
  :val '(1 2 3 :k1 11 :k2 12)
  :var (a b c k1 k2)
  :pat  (a b c &rest _ &map (:k1 k1) (:k2 k2))
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&rest-ignored-3
  :result (list 1 2 3 11 12)
  :val '(1 2 3 :k1 11 :k2 12)
  :var (a b c k1 k2)
  :pat  (a b c &rest _ &key (k1 :k1) (k2 :k2))
  :vector nil
  :seq nil)

(loopy-def-pcase-test pcase-tests-loopy-&rest-nonlist-cdr-1
  :result (list 1 2)
  :val (cons 1 2)
  :var (a b)
  :pat (a &rest b)
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&rest-nonlist-cdr-2
  :result (list 1 2)
  :val (cons 1 2)
  :var (a b)
  :pat (a &body b)
  :vector nil
  :convert nil)

(ert-deftest pcase-tests-loopy-&rest-nonlist-cdr-3 ()
  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (cons 1 2)
                   ((loopy (a . b))
                    (list a b)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (cons 1 2)
                   ;; This works for the list form of `&seq' because it is still
                   ;; an improper list for `loopy--get-var-groups'.
                   ((loopy (&seq a . b))
                    (list a b))))))

(loopy-def-pcase-test3 pcase-tests-loopy-&rest-with-&whole-1
  :var (whole a b)
  :tests [( :val (list 1 2)
            :result (list (list 1 2) 1 (list 2))
            :tests [(:name list                 :pat (&whole whole a &body b))
                    (:name seq-list-as-list-1   :pat (&seq &whole whole a &body b))
                    (:name seq-list-as-list-2   :pat (&seq &whole whole a &body b))
                    (:name seq-list-as-list-3   :pat (&seq &whole whole a &body b))
                    (:name seq-list-as-vector-1 :pat [&seq &whole whole a &body b])
                    (:name seq-list-as-vector-2 :pat [&seq &whole whole a &body b])
                    (:name seq-list-as-vector-3 :pat [&seq &whole whole a &body b])])
          ( :val (vector 1 2)
            :result (list (vector 1 2) 1 (vector 2))
            :tests [(:name vector                 :pat [&whole whole a &body b])
                    (:name seq-vector-as-vector-1 :pat [&seq &whole whole a &body b])
                    (:name seq-vector-as-vector-2 :pat [&seq &whole whole a &body b])
                    (:name seq-vector-as-vector-3 :pat [&seq &whole whole a &body b])
                    (:name seq-vector-as-list-1   :pat (&seq &whole whole a &body b))
                    (:name seq-vector-as-list-2   :pat (&seq &whole whole a &body b))
                    (:name seq-vector-as-list-3   :pat (&seq &whole whole a &body b))])])

(loopy-def-pcase-test3 pcase-tests-loopy-&rest-with-&whole-2
  :var (whole a b)
  :tests [( :val (list 1 2)
            :result (list (list 1 2) 1 (list 2))
            :tests [(:name list                 :pat (&whole whole a &rest b))
                    (:name seq-list-as-list-1   :pat (&seq &whole whole a &rest b))
                    (:name seq-list-as-list-2   :pat (&seq &whole whole a &rest b))
                    (:name seq-list-as-list-3   :pat (&seq &whole whole a &rest b))
                    (:name seq-list-as-vector-1 :pat [&seq &whole whole a &rest b])
                    (:name seq-list-as-vector-2 :pat [&seq &whole whole a &rest b])
                    (:name seq-list-as-vector-3 :pat [&seq &whole whole a &rest b])])
          ( :val (vector 1 2)
            :result (list (vector 1 2) 1 (vector 2))
            :tests [(:name vector                 :pat [&whole whole a &rest b])
                    (:name seq-vector-as-vector-1 :pat [&seq &whole whole a &rest b])
                    (:name seq-vector-as-vector-2 :pat [&seq &whole whole a &rest b])
                    (:name seq-vector-as-vector-3 :pat [&seq &whole whole a &rest b])
                    (:name seq-vector-as-list-1   :pat (&seq &whole whole a &rest b))
                    (:name seq-vector-as-list-2   :pat (&seq &whole whole a &rest b))
                    (:name seq-vector-as-list-3   :pat (&seq &whole whole a &rest b))])])

(ert-deftest pcase-tests-loopy-&rest-with-&whole-3 ()
  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (cons 1 2)
                   ((loopy (a . b))
                    (list a b)))))

  (should (equal (list 1 2)
                 (loopy--pcase-exhaustive-wrapper (a b)
                     (cons 1 2)
                   ;; This works for the list form of `&seq' because it is still
                   ;; an improper list for `loopy--get-var-groups'.
                   ((loopy (&seq a . b))
                    (list a b))))))

(loopy-def-pcase-test pcase-tests-loopy-&rest-only-1
  :doc "Using only `&rest' should work like `&whole'."
  :list-result (list (list 1 2))
  :vector-result (list (vector 1 2))
  :val (list 1 2)
  :var (a)
  :pat (&rest a))

(loopy-def-pcase-test pcase-tests-loopy-&rest-only-2
  :doc "Using only `&rest' should work like `&whole'."
  :list-result (list (list 1 2))
  :vector-result (list (vector 1 2))
  :val (list 1 2)
  :var (a)
  :pat (&body a))

(loopy-def-pcase-test pcase-tests-loopy-&rest-after-&optional-1
  :result (list 1 2 3 (list 4 5))
  :val (list 1 2 3 4 5)
  :var (a b c d)
  :pat (&optional a b c &rest d)
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&rest-after-&optional-2
  :list-result (list 1 2 3 (list 4 5))
  :vector-result (list 1 2 3 (vector 4 5))
  :val (list 1 2 3 4 5)
  :var (a b c d)
  :pat (&optional a b c &body d))

(ert-deftest pcase-tests-loopy-&rest-after-&optional-3 ()
  (should (equal (list 1 2 3 (list 4 5))
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2 3 4 5)
                   ((loopy (&optional a b c . d))
                    (list a b c d)))))

  (should (equal (list 1 2 3 (list 4 5))
                 (loopy--pcase-exhaustive-wrapper (a b c d)
                     (list 1 2 3 4 5)
                   ((loopy (&seq &optional a b c . d))
                    (list a b c d))))))

(loopy-def-pcase-test3 pcase-tests-loopy-&rest-sub-seq-1
  :var (a b c)
  :result (list 1 2 3)
  :tests [( :val (list 1 2 3)
            :tests [(:name list                 :pat (a &rest (b c)))
                    (:name seq-list-as-list-1   :pat (&seq a &rest (b c)))
                    (:name seq-list-as-list-2   :pat (&seq a &rest (&seq b c)))
                    (:name seq-list-as-list-3   :pat (&seq a &rest [&seq b c]))
                    (:name seq-list-as-vector-1 :pat [&seq a &rest (b c)])
                    (:name seq-list-as-vector-2 :pat [&seq a &rest (&seq b c)])
                    (:name seq-list-as-vector-3 :pat [&seq a &rest [&seq b c]])])
          ( :val (vector 1 2 3)
            :tests [(:name vector                 :pat [a &rest [b c]])
                    (:name seq-vector-as-vector-1 :pat [&seq a &rest [b c]])
                    (:name seq-vector-as-vector-2 :pat [&seq a &rest [&seq b c]])
                    (:name seq-vector-as-vector-3 :pat [&seq a &rest (&seq b c)])
                    (:name seq-vector-as-list-1   :pat (&seq a &rest [b c]))
                    (:name seq-vector-as-list-2   :pat (&seq a &rest [&seq b c]))
                    (:name seq-vector-as-list-3   :pat (&seq a &rest (&seq b c)))])])

(loopy-def-pcase-test3 pcase-tests-loopy-&rest-sub-seq-2
  :var (a b c)
  :result (list 1 2 3)
  :tests [( :val (list 1 2 3)
            :tests [(:name list                 :pat (a &body (b c)))
                    (:name seq-list-as-list-1   :pat (&seq a &body (b c)))
                    (:name seq-list-as-list-2   :pat (&seq a &body (&seq b c)))
                    (:name seq-list-as-list-3   :pat (&seq a &body [&seq b c]))
                    (:name seq-list-as-vector-1 :pat [&seq a &body (b c)])
                    (:name seq-list-as-vector-2 :pat [&seq a &body (&seq b c)])
                    (:name seq-list-as-vector-3 :pat [&seq a &body [&seq b c]])])
          ( :val (vector 1 2 3)
            :tests [(:name vector                 :pat [a &body [b c]])
                    (:name seq-vector-as-vector-1 :pat [&seq a &body [b c]])
                    (:name seq-vector-as-vector-2 :pat [&seq a &body [&seq b c]])
                    (:name seq-vector-as-vector-3 :pat [&seq a &body (&seq b c)])
                    (:name seq-vector-as-list-1   :pat (&seq a &body [b c]))
                    (:name seq-vector-as-list-2   :pat (&seq a &body [&seq b c]))
                    (:name seq-vector-as-list-3   :pat (&seq a &body (&seq b c)))])])

(loopy-def-pcase-test pcase-tests-loopy-&rest-sub-seq-3
  :result (list 1 2 3)
  :val (list 1 2 3)
  :var (a b c)
  :pat (a . (b c))
  :vector nil
  :seq-vector nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-permissive-1
  :doc "`&map' should not require a construct like `&allow-other-keys'."
  :result (list 1 2)
  :val (list 'a 1 'b 2 'c 3)
  :var (a b)
  :pat (&map a b)
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-permissive-2
  :doc "`&map' should not require a construct like `&allow-other-keys'."
  :result (list 1 2)
  :val (list :a 1 :b 2 :c 3)
  :var (a b)
  :pat (&map (:a a) (:b b))
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-not-first-1
  :doc "The map should be after positional values and equal to `&rest'."
  :result (list 1 2 3 11 22)
  :val (list 1 2 3 'k1 11 'k2 22)
  :var (a b c k1 k2)
  :pat (a b c &map k1 k2)
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-not-first-2
  :doc "The map should be after positional values and equal to `&rest'."
  :result (list 1 2 3 (list :k1 11 :k2 22) 11 22)
  :val (list 1 2 3 :k1 11 :k2 22)
  :var (a b c r1 k1 k2)
  :pat (a b c &rest r1 &map (:k1 k1) (:k2 k2))
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-not-first-3
  :doc "The map should be after positional values and equal to `&rest'."
  :result (list 0 1 2 [10 11 12] 10 11)
  :val [0 1 2 10 11 12]
  :var (a b c r1 k0 k1)
  :pat (a b c &rest r1 &map (0 k0) (1 k1))
  :list nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-full-form-1
  :result (list 1 2)
  :val (list 'a 1 'b 2)
  :var (a b)
  :pat (&map a ('b b 13))
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-full-form-2
  :result (list 1 2)
  :val (list 'a 1 'b 2)
  :var (a b)
  :pat (&map a ['b b 13])
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-full-form-3
  :result (list 1 13)
  :val (list 'a 1)
  :var (a b)
  :pat (&map a ('b b 13))
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-full-form-4
  :result (list 1 13)
  :val (list 'a 1)
  :var (a b)
  :pat (&map a ['b b 13])
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-full-form-5
  :result (list 1 13 nil)
  :val (list 'a 1)
  :var (a b b-supplied)
  :pat (&map a ('b b 13 b-supplied))
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-full-form-6
  :result (list 1 2 t)
  :val (list 'a 1 'b 2)
  :var (a b b-supplied)
  :pat (&map a ('b b 13 b-supplied))
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-full-form-7
  :result (list 1 2 t)
  :val (list :a 1 :b 2)
  :var (a b b-supplied)
  :pat (&map (:a a) (:b b 13 b-supplied))
  :vector nil
  :convert nil)

(let ((key :bat))
  (loopy-def-pcase-test pcase-tests-loopy-&map-full-form-8
    :result (list 1 2 t)
    :val (list :a 1 :bat 2)
    :var (a b b-supplied)
    :pat (&map (:a a) (key b 13 b-supplied))
    :vector nil
    :convert nil))

(loopy-def-pcase-test pcase-tests-loopy-&map-sub-seq-1
  :result '(1 2 (:c 77 :e should-ignore) nil 77 t 99 nil)
  :val '(:ab (1 2))
  :var (a b cd cd-supp c c-supp d d-supp)
  :pat (&map
        (:ab (a b))
        (:cd ( &whole cd
               &map
               (:c c 88 c-supp)
               (:d d 99 d-supp))
             (list :c 77 :e 'should-ignore)
             cd-supp))
  :vector nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&map-sub-seq-2
  :result '(1 2 (:c 77 :e should-ignore) nil 77 t 99 nil)
  :val (vector (list 1 2))
  :var (a b cd cd-supp c c-supp d d-supp)
  :pat [&map
        (0 (a b))
        (1 ( &whole cd
             &map
             (:c c 88 c-supp)
             (:d d 99 d-supp))
           (list :c 77 :e 'should-ignore)
           cd-supp)]
  :list nil
  :convert nil)

(loopy-def-pcase-test pcase-tests-loopy-&aux-1
  :result (list 1 2 nil nil)
  :val nil
  :var (a b c d)
  :pat (&aux (a 1) (b 2) (c) d))

(loopy-def-pcase-test pcase-tests-loopy-&aux-2
  :result (list 0 1 2 nil nil)
  :val (list 0)
  :var (z0 a b c d)
  :pat (z0 &aux [a 1] [b 2] [c] d))

(loopy-def-pcase-test pcase-tests-loopy-&aux-sub-seq-1
  :result (list 1 2)
  :val nil
  :var (a b)
  :pat (&aux ((a b) (list 1 2))))

(loopy-def-pcase-test pcase-tests-loopy-&aux-sub-seq-2
  :result (list 1 2)
  :val nil
  :var (a b)
  :pat (&aux ([a b] (vector 1 2))))

(loopy-def-pcase-test pcase-tests-loopy-&aux-sub-seq-3
  :result (list 1 2)
  :val nil
  :var (a b)
  :pat (&aux ([&seq a b] (list 1 2))))

(loopy-def-pcase-test pcase-tests-loopy-&aux-sub-seq-4
  :result (list 1 2)
  :val nil
  :var (a b)
  :pat (&aux ((&seq a b) (vector 1 2))))

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

  (should-error
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
      (list a b cd cd-supp c c-supp d d-supp)))
   :type 'loopy-pcase-no-match))

(ert-deftest pcase-tests-loopy-&aux-should-error ()
  "`&aux' cannot be used after `&aux'."
  (should-error (loopy--pcase-exhaustive-wrapper (a b)
                    nil
                  ((loopy (&aux a &aux b))
                   (list a b)))
                :type 'loopy-&aux-bad-position))

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
