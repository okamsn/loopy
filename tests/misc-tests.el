;; Tests of secondary features and helper functions.

(require 'cl-lib)
(require 'map)
(require 'ert)
(require 'pcase)
(require 'map)
(require 'loopy)

(defmacro loopy-test-structure (input output-pattern)
  "Use `pcase' to check a destructurings bindings.
INPUT is the destructuring usage.  OUTPUT-PATTERN is what to match."
  `(pcase ,input
     (,output-pattern
      t)
     (_ nil)))

;;; Minor Functions
(ert-deftest split-off-last-var ()
  (should (equal '((a b c) d)
                 (loopy--split-off-last-var '(a b c d))))

  (should (equal '((a b c) d)
                 (loopy--split-off-last-var '(a b c . d)))))

;;; Destructuring

(ert-deftest destructure-array-errors ()
  (should-error (loopy--destructure-array [a b &rest] 'val))
  (should-error (loopy--destructure-array [a b &rest c d] 'val))
  (should-error (loopy--destructure-array [&rest] 'val))
  (should-error (loopy--destructure-array [&whole &rest] 'val))
  (should-error (loopy--destructure-array [&whole] 'val))
  (should-error (loopy--destructure-array [&whole _] 'val))
  (should-error (loopy--destructure-array [&rest _] 'val))
  (should-error (loopy--destructure-array [_ _] 'val)))

(ert-deftest destructure-arrays-steps-output ()
  "Test for ideal output."
  (should (loopy-test-structure (loopy--destructure-array [a] 'val)
                                `((a (aref val 0)))))

  (should (loopy-test-structure (loopy--destructure-array [a _] 'val)
                                `((a (aref val 0)))))

  (should (loopy-test-structure (loopy--destructure-array [_ b] 'val)
                                `((b (aref val 1)))))

  (should (loopy-test-structure (loopy--destructure-array [_ b _] 'val)
                                `((b (aref val 1)))))

  (should (loopy-test-structure (loopy--destructure-array [a b] 'val)
                                `((,_ val)
                                  (a (aref ,_ 0))
                                  (b (aref ,_ 1)))))

  (should (loopy-test-structure (loopy--destructure-array [_ b c] 'val)
                                `((,_ val)
                                  (b (aref ,_ 1))
                                  (c (aref ,_ 2)))))

  (should (loopy-test-structure (loopy--destructure-array [_ b c _ _] 'val)
                                `((,_ val)
                                  (b (aref ,_ 1))
                                  (c (aref ,_ 2)))))

  (should (loopy-test-structure (loopy--destructure-array [a b &rest c] 'val)
                                `((,_ val)
                                  (a (aref ,_ 0))
                                  (b (aref ,_ 1))
                                  (c (substring ,_ 2)))))

  (should (loopy-test-structure (loopy--destructure-array [a _ &rest c] 'val)
                                `((,_ val)
                                  (a (aref ,_ 0))
                                  (c (substring ,_ 2)))))

  (should (loopy-test-structure (loopy--destructure-array [a b &rest _] 'val)
                                `((,_ val)
                                  (a (aref ,_ 0))
                                  (b (aref ,_ 1)))))

  (should (loopy-test-structure (loopy--destructure-array [_ b _ &rest _] 'val)
                                `((b (aref val 1))))))

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
  (should-error (loopy--destructure-list '(a b &rest) 'val))
  (should-error (loopy--destructure-list '(a b &rest c d) 'val))
  (should-error (loopy--destructure-list '(&rest) 'val))
  (should-error (loopy--destructure-list '(&whole &rest) 'val))
  (should-error (loopy--destructure-list '(&whole) 'val))
  (should-error (loopy--destructure-list '(&whole _) 'val))
  (should-error (loopy--destructure-list '(&rest _) 'val))
  (should-error (loopy--destructure-list '(&key) 'val))
  (should-error (loopy--destructure-list '(&keys) 'val))
  (should-error (loopy--destructure-list '(_ _) 'val)))

(ert-deftest destructure-lists-steps-output-rest ()
  (should (loopy-test-structure (loopy--destructure-list '(a b &rest c)  'val)
                                `((c val)
                                  (a (pop c))
                                  (b (pop c)))))

  (should (loopy-test-structure (loopy--destructure-list '(&whole whole a b &rest c)  'val)
                                `((whole val)
                                  (c whole)
                                  (a (pop c))
                                  (b (pop c)))))

  (should (loopy-test-structure (loopy--destructure-list '(a _ &rest c)  'val)
                                `((c val)
                                  (a (pop c))
                                  (c (nthcdr 1 c)))))

  (should (loopy-test-structure (loopy--destructure-list '(&whole whole a _ &rest c)  'val)
                                `((whole val)
                                  (c whole)
                                  (a (pop c))
                                  (c (nthcdr 1 c)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(_ _ a _ _ &rest c)  'val)
           `((c (nthcdr 2 val))
             (a (pop c))
             (c (nthcdr 2 c)))))

  (should (loopy-test-structure (loopy--destructure-list '(a b &rest _)  'val)
                                `((b val)
                                  (a (pop b))
                                  (b (car b)))))

  (should (loopy-test-structure (loopy--destructure-list '(&whole whole a b &rest _)  'val)
                                `((whole val)
                                  (b whole)
                                  (a (pop b))
                                  (b (car b)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(_ _ &rest (a b))  'val)
           `((b (nthcdr 2 val))
             (a (pop b))
             (b (car b)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(&whole whole _ _ &rest (a b))  'val)
           `((whole val)
             (b (nthcdr 2 whole))
             (a (pop b))
             (b (car b)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(_ b _ &rest _)  'val)
           `((b (nth 1 val)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(&whole whole _ b _ &rest _)  'val)
           `((whole val)
             (b (nth 1 whole)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(_ b _ &rest (c d e))  'val)
           `((,_ (nthcdr 1 val))
             (b (pop ,_))
             (,_ (nthcdr 1 ,_))
             (e ,_)
             (c (pop ,_))
             (d (pop ,_))
             (e (car e)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(&whole whole _ b _ &rest (c d e))  'val)
           `((whole val)
             (,_ (nthcdr 1 whole))
             (b (pop ,_))
             (,_ (nthcdr 1 ,_))
             (e ,_)
             (c (pop ,_))
             (d (pop ,_))
             (e (car e)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(_ _ &rest b)  'val)
           `((b (nthcdr 2 val)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(&whole whole _ _ &rest b)  'val)
           `((whole val)
             (b (nthcdr 2 whole))))))

(ert-deftest destructure-lists-steps-output-key ()
  (should (loopy-test-structure
           (loopy--destructure-list '(&key k1 k2)  'val)
           `((,_ val)
             (k1 (plist-get ,_ :k1))
             (k2 (plist-get ,_ :k2)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(_ _ &rest b &key k1 k2)  'val)
           `((b (nthcdr 2 val))
             (k1 (plist-get b :k1))
             (k2 (plist-get b :k2)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(&whole whole _ _ &rest b &key k1 k2)  'val)
           `((whole val)
             (b (nthcdr 2 whole))
             (k1 (plist-get b :k1))
             (k2 (plist-get b :k2)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(_ _ &rest _ &key k1 k2)  'val)
           `((,_ (nthcdr 2 val))
             (k1 (plist-get ,_ :k1))
             (k2 (plist-get ,_ :k2)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(&whole whole _ _ &rest _ &key k1 k2)  'val)
           `((whole val)
             (,_ (nthcdr 2 whole))
             (k1 (plist-get ,_ :k1))
             (k2 (plist-get ,_ :k2)))))


  (should (loopy-test-structure
           (loopy--destructure-list '(_ _ &key k1 k2)  'val)
           `((,_ (nthcdr 2 val))
             (k1 (plist-get ,_ :k1))
             (k2 (plist-get ,_ :k2)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(&whole whole _ _ &key k1 k2)  'val)
           `((whole val)
             (,_ (nthcdr 2 whole))
             (k1 (plist-get ,_ :k1))
             (k2 (plist-get ,_ :k2)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(a _ &key k1 k2)  'val)
           `((,_ val)
             (a (pop ,_))
             (,_ (nthcdr 1 ,_))
             (k1 (plist-get ,_ :k1))
             (k2 (plist-get ,_ :k2)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(_ _ a _ &key k1 k2)  'val)
           `((,_ (nthcdr 2 val))
             (a (pop ,_))
             (,_ (nthcdr 1 ,_))
             (k1 (plist-get ,_ :k1))
             (k2 (plist-get ,_ :k2)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(&whole whole _ _ a _ &key k1 k2)  'val)
           `((whole val)
             (,_ (nthcdr 2 whole))
             (a (pop ,_))
             (,_ (nthcdr 1 ,_))
             (k1 (plist-get ,_ :k1))
             (k2 (plist-get ,_ :k2)))))

  (should (loopy-test-structure
           (loopy--destructure-list '(_ _ a _ &key k1 (k2 25) k3)  'val)
           `((,_ (nthcdr 2 val))
             (a (pop ,_))
             (,_ (nthcdr 1 ,_))
             (k1 (plist-get ,_ :k1))
             (k2 (if-let ((key-found (plist-member ,_ :k2)))
                     (cl-second key-found)
                   25))
             (k3 (plist-get ,_ :k3))))))

(ert-deftest destructure-lists-steps-output ()
  "Test for ideal output."
  (should (loopy-test-structure (loopy--destructure-list '(a)  'val)
                                `((a (nth 0 val)))))

  (should (loopy-test-structure (loopy--destructure-list '(&whole whole a)  'val)
                                `((whole val)
                                  (a (nth 0 whole)))))

  (should (loopy-test-structure (loopy--destructure-list '(a _)  'val)
                                `((a (nth 0 val)))))

  (should (loopy-test-structure (loopy--destructure-list '(_ b)  'val)
                                `((b (nth 1 val)))))

  (should (loopy-test-structure (loopy--destructure-list '(&whole whole _ b)  'val)
                                `((whole val)
                                  (b (nth 1 whole)))))

  (should (loopy-test-structure (loopy--destructure-list '(_ b _)  'val)
                                `((b (nth 1 val)))))

  (should (loopy-test-structure (loopy--destructure-list '(a b)  'val)
                                `((b val)
                                  (a (pop b))
                                  (b (car b)))))

  (should (loopy-test-structure (loopy--destructure-list '(_ b c)  'val)
                                `((c (nthcdr 1 val))
                                  (b (pop c))
                                  (c (car c)))))

  (should (loopy-test-structure (loopy--destructure-list '(_ b c _ _)  'val)
                                `((c (nthcdr 1 val))
                                  (b (pop c))
                                  (c (car c)))))

  (should (loopy-test-structure (loopy--destructure-list '(&whole whole _ b c _ _)  'val)
                                `((whole val)
                                  (c (nthcdr 1 whole))
                                  (b (pop c))
                                  (c (car c)))))

  (should (loopy-test-structure (loopy--destructure-list '(_ (a b) _)  'val)
                                `((b (nth 1 val))
                                  (a (pop b))
                                  (b (car b)))))

  (should (loopy-test-structure (loopy--destructure-list '(&whole whole _ (a b) _)  'val)
                                `((whole val)
                                  (b (nth 1 whole))
                                  (a (pop b))
                                  (b (car b))))))

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
                 (eval (quote (loopy-let* (((a b c &key d e) '(1 2 3 :e 5 :d 4)))
                                (list a b c d e))))))

  (should (equal '(1 2 3 4 5 (:e 5 :d 4))
                 (eval (quote (loopy-let* (((a b c &key d e . f) '(1 2 3 :e 5 :d 4)))
                                (list a b c d e f))))))

  (should (equal '(1 2 3 7 6 (4 5 :e 6 :d 7))
                 (eval (quote (loopy-let* (((a b c &key d e . f) '(1 2 3 4 5 :e 6 :d 7)))
                                (list a b c d e f))))))

  (should (equal '(1 2 3 7 6 (4 5 :e 6 :d 7))
                 (eval (quote (loopy-let* (((a b c &key d e &rest f)
                                            '(1 2 3 4 5 :e 6 :d 7)))
                                (list a b c d e f))))))

  (should (equal '(1 2 3 7 6 (4 5 :e 6 :d 7))
                 (eval (quote (loopy-let* (((a b c &rest f &key d e)
                                            '(1 2 3 4 5 :e 6 :d 7)))
                                (list a b c d e f))))))

  (should (equal '(4 5)
                 (eval (quote (loopy-let* (((&key d e) '(:a 7 :e 5 :d 4)))
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

  (should (equal '((1 2 3 4 5 :e 6 :d 7) 1 2 3 7 6 (4 5 :e 6 :d 7))
                 (eval (quote (loopy-let* (((&whole cat a b c &key d e . f)
                                            '(1 2 3 4 5 :e 6 :d 7)))
                                (list cat a b c d e f))))))

  (should (equal '((1 2 3 4 5 :e 6 :d 7) 1 2 3 7 6 (4 5 :e 6 :d 7))
                 (eval (quote (loopy-let* (((&whole cat a b c &key d e &rest f)
                                            '(1 2 3 4 5 :e 6 :d 7)))
                                (list cat a b c d e f))))))

  (should (equal '((1 2 3 4 5 :e 6 :d 7) 1 2 3 7 6 (4 5 :e 6 :d 7))
                 (eval (quote (loopy-let* (((&whole cat a b c &rest f &key d e)
                                            '(1 2 3 4 5 :e 6 :d 7)))
                                (list cat a b c d e f))))))

  (should (equal '((:a 7 :e 5 :d 4) 4 5)
                 (eval (quote (loopy-let* (((&whole cat &key d e)
                                            '(:a 7 :e 5 :d 4)))
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
