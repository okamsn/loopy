;; -*- lexical-binding: t; -*-

;; Run these tests using:
;; emacs -Q --batch -l ert -l tests.el -f ert-run-tests-batch-and-exit
;;
;; NOTE:
;; - Destructuring tests in `./misc-tests.el'.
;; - Alternative destructuring systems tested in their own files.

(push (expand-file-name ".")
      load-path)

(require 'cl-lib)

(require 'package)
(dolist (feature '(compat stream))
  (unless (featurep feature)
    (dolist (dir (seq-filter #'file-directory-p
                             (directory-files
                              (expand-file-name package-user-dir)
                              t
                              (symbol-name feature))))
      (push dir load-path))))

(require 'subr-x)
(require 'package)
(require 'compat)
(require 'map)
(require 'ert)
(require 'generator)
(require 'pcase)
(require 'loopy)
(require 'loopy-iter)

(push (list "Loopy Tests"
            (rx (0+ blank) "(loopy-deftest" (0+ blank)
                (group-n 1 (1+ (or word (syntax symbol)))))
            1)
      imenu-generic-expression)

;; "loopy quote"
(defmacro lq (&rest body)
  "`loopy' quote: Quote a use of `loopy'."
  `(eval (quote (loopy ,@body)) t))

;;; Check for ELC files, which can mess up testing.
(ert-deftest no-elc-in-cwd ()
  (should (cl-loop for f in (directory-files ".")
                   never (string-match-p "\\.elc\\'" f))))

(cl-defmacro loopy-deftest
    ( name
      &key
      args doc repeat body multi-body
      repeat-loopy repeat-iter-bare repeat-iter-keyword
      wrap
      macroexpand
      (loopy nil loopy-provided)
      (iter-bare nil iter-bare-provided)
      (iter-keyword nil iter-keyword-provided)
      (should nil should-provided)
      (result nil result-provided)
      (error nil error-provided))
  "Create test for `loopy' and `loopy-iter'.

- NAME is the name of the test.

- ARGS is the list of test arguments.

- DOC is the documentation string of the test.

- BODY is the test.

- MULTI-BODY means there are multiple bodies in BODY.

- LOOPY are the `loopy' names.

- ITER-BARE are the `loopy-iter' names.

- ITER-KEYWORD are the `loopy-iter' names after keywords.
  This can also be a simple list of command names.

- RESULT is compared using `equal' and `should'.

- SHOULD means just using `should'.  This is better when
  testing that macro expansion should succeed.

- ERROR is the list of signals for `should-error'.

- REPEAT is the temp name in LOOPY and ITER for which we
  test multiple names.

- REPEAT-LOOPY is the temp name in LOOPY for which we
  test multiple names.

- REPEAT-ITER-BARE is the temp name in ITER-BARE for which we
  test multiple names.

- REPEAT-ITER-KEYWORD is the temp name in ITER-KEYWORD for which we
  test multiple names.

- WRAP is an alist of (VAR . EXPANSION-TO-BE-QUOTE).
  E.g., (x . \\=`(let ((a 2)) ,x)).

- If MACROEXPAND is non-nil, then we test using `macroexpand'
  instead of `eval'.

LOOPY and ITER-BARE can be `t' instead of an alist, which will
run those tests without substitution.  If ITER-KEYWORD is `t', we
prefix the items in LOOPY or ITER-BARE."
  (declare (indent 1))

  (unless (or result-provided error-provided should-provided)
    (error "Must include `result' or `error'"))
  (unless (or loopy iter-bare iter-keyword)
    (error "Must include `loopy' or `iter-bare'"))
  (unless body
    (error "Must include `body'"))

  (pcase loopy
    ((pred (eq t)) (setq loopy nil))
    ((pred (eq nil)) (setq loopy-provided nil)))

  (pcase iter-bare
    ((pred (eq t)) (setq iter-bare nil))
    ((pred (eq nil)) (setq iter-bare-provided nil)))

  (pcase iter-keyword
    ((pred (eq t)) (setq iter-keyword (or loopy iter-bare)))
    ((pred (eq nil)) (setq iter-keyword-provided nil))
    (`(,(pred symbolp) . ,_)
     (setq iter-keyword (cl-loop for sym in iter-keyword
                                 collect (cons sym sym)))))

  (when (eq error t)
    (setq error 'error))

  (cl-labels
      (;; Wrap body into other forms.
       (surround-wrap (sexp wraps)
         (let ((result sexp))
           (pcase-dolist (`(,var . ,exp) wraps)
             (setq result (funcall `(lambda (,var) ,exp)
                                   result)))
           result))
       ;; Want to evaluate quoted form lexically.
       (quote-wrap (sexp) (if macroexpand
                              `(macroexpand (quote ,sexp))
                            `(eval (quote ,sexp) t)))
       ;; What output should be.
       (output-wrap (x) (cond (should-provided `(should ,x))
                              (result-provided `(should (equal ,result ,x)))
                              (error-provided  `(should-error ,x :type
                                                              (quote ,error)))))
       ;; Replace given placeholder command names with actual names,
       ;; maybe including the `for' keyword for `loopy-iter'.
       (translate (group-alist this-body &optional keyword)
         (mapcar (lambda (sexp)
                   (pcase sexp
                     (`(loopy-test-escape ,form) form)
                     (`(,first . ,rest)
                      ;; If not a proper list, then it was probably a
                      ;; dotted variable list.
                      (if (not (proper-list-p rest))
                          sexp
                        (let ((new-rest (translate group-alist rest keyword))
                              (new-first (map-elt group-alist first)))
                          (if new-first
                              (if keyword
                                  `(for ,new-first ,@new-rest)
                                `(,new-first ,@new-rest))
                            `(,first ,@new-rest)))))
                     (_ sexp)))
                 this-body))
       (make-bodies (alist group-repeat &optional keyword)
         ;; Use `mapcan' to turn list of lists of bodies into list of
         ;; bodies.
         (mapcan (lambda (x)
                   (if group-repeat
                       (let ((names (map-elt alist repeat)))
                         (mapcar (lambda (name)
                                   (translate `((,group-repeat . ,name)
                                                ,@alist)
                                              x keyword))
                                 names))
                     (list (translate alist x keyword))))
                 (if multi-body body (list body))))
       (build (&key macro prefix alist provided repeat keyword)
         (when provided
           `(ert-deftest ,(intern (format "%s/%s" prefix name)) ,args
              ,doc ; Nil if not given
              ,@(mapcar (lambda (x) (thread-first `(,macro ,@x)
                                                  (surround-wrap wrap)
                                                  quote-wrap
                                                  output-wrap))
                        (make-bodies alist repeat keyword))))))
    `(progn
       ,(build :macro 'loopy
               :prefix 'loopy
               :alist loopy
               :provided loopy-provided
               :repeat (or repeat repeat-loopy))
       ,(build :macro 'loopy-iter
               :prefix 'iter-bare
               :alist iter-bare
               :provided iter-bare-provided
               :repeat (or repeat repeat-iter-bare))
       ,(build :macro 'loopy-iter
               :prefix 'iter-keyword
               :alist iter-keyword
               :provided iter-keyword-provided
               :repeat (or repeat repeat-iter-keyword)
               :keyword t))))

(defun my-iter-insert (&rest syms-str)
  "Insert values for `:iter-keyword' and `:iter-bare'.
SYMS-STR are the string names of symbols from `loopy-iter-bare-commands'."
  (interactive (completing-read-multiple "Bare name: "
                                         loopy-iter-bare-commands))
  (let* ((true-names-str (mapcar (lambda (x)
                                   (thread-last x
                                                intern
                                                loopy--get-true-name
                                                symbol-name))
                                 syms-str)))
    (insert (format ":iter-keyword (%s)"
                    (string-join true-names-str " ")))
    (newline-and-indent)
    (insert (format ":iter-bare (%s)"
                    (string-join (cl-loop for true in true-names-str
                                          for iter in syms-str
                                          collect (format "(%s . %s)" true iter))
                                 "\n")))))

;;; Macro arguments
;;;; Named (loop Name)

(loopy-deftest named
  :result 4
  :multi-body t
  :body (((named my-loop)
          (return-from my-loop 4))
         (my-loop
          (collect 4)
          (leave-from my-loop)
          (finally-return (car loopy-result))))
  :loopy t
  :iter-bare ((collect . collecting)
              (return-from . returning-from)
              (leave-from . leaving-from))
  :iter-keyword t)

;;;; With
(loopy-deftest with-arg-order
  :result 4
  :body ((_with (a 2) (b (+ a 2)))
         (_return b))
  :loopy ((_with . (with let* init))
          (_return . return))
  :iter-bare ((_with . (with init))
              (_return . returning))
  :iter-keyword t
  :repeat _with)

(loopy-deftest with-destructuring
  :result -2
  :wrap ((x . `(let ((e 7)) ,x)))
  :body ((with ((a b) '(1 2))
               ([c d] `[,(1+ a) ,(1+ b)])
               ((e f) (list (1+ e) (1+ e))))
         (return (+ (- a b)
                    (- c d)
                    (- e f))))
  :loopy t
  :iter-bare ((return . returning)))

;;;; Without
(loopy-deftest without
  :result '(4 5)
  :wrap ((x . `(let ((a 1) (b 2)) ,x)))
  :body ((with (c 3))
         (_without a b)
         (set a (+ a c))
         (set b (+ b c))
         (return a b))
  :loopy ((_without . (without no-init no-with)))
  :iter-bare ((_without . (without no-init no-with))
              (set . setting)
              (return . returning))
  :repeat _without)

;;;; Before Do
;; `before-do' always runs, and occurs before the loop.
(loopy-deftest basic-before-do
  :result 4
  :body ((with (i 3))
         (_before (setq i (1+ i)))
         (return i))
  :loopy ((_before . (before-do before initially-do initially)))
  :loopy ((_before . (before-do before initially-do initially))
          (return . returning))
  :repeat _before)

;;;; After Do - runs after loop is loop completed successfully
(loopy-deftest basic-after-do-does-run
  :result t
  :body ((with (my-ret nil))
         (list i '(1 2 3 4))
         (after-do (setq my-ret t))
         (finally-return my-ret))
  :loopy t
  :iter-bare ((list . listing)))

(loopy-deftest basic-after-does-not-run
  :result nil
  :multi-body t
  :body (((with (my-ret nil))
          (_list i '(1 2 3 4))
          (_return nil)
          (_after (setq my-ret t))
          (finally-return my-ret))
         ((with (my-ret nil))
          (_list i '(1 2 3 4))
          (_leave)
          (_after (setq my-ret t))
          (finally-return my-ret)))
  :loopy ((_after . (after-do after else-do else))
          (_leave . leave)
          (_return . return)
          (_list . list))
  :iter-bare ((_after . (after-do after else-do else))
              (_list . listing)
              (_return . returning)
              (_leave . leaving))
  :iter-keyword t
  :repeat _after)

;;;; Before and After
(loopy-deftest basic-before-and-after-test
  :result 3
  :body ((with (i 1))
         (before-do (cl-incf i))
         (cycle 1)
         (after-do (cl-incf i))
         (finally-return i))
  :loopy t
  :iter-bare ((cycle . cycling))
  :iter-keyword ((cycle . cycle)))

;;;; Wrap
(loopy-deftest wrap
  ;; Test saving match data
  :result t
  :wrap ((x . `(let ((original-data (set-match-data nil)))
                 (equal original-data ,x))))
  :body ((wrap save-match-data)
         (_cycle 1)
         (_do (string-match (make-string 100 ?a)
                            (make-string 100 ?a)))
         (finally-return (match-data)))
  :loopy ((_cycle . cycle)
          (_do . do))
  :iter-bare ((_cycle . cycling)
              ;; Use `ignore' to eval arguments without doing anything.
              (_do . ignore))
  :iter-keyword ((_cycle . cycle)
                 (_do . do)))

(loopy-deftest wrap-order
  ;; Test order things wrapped in.
  :result 3
  :body ((wrap (let ((a 1)))
               (let ((b (1+ a)))))
         (return (+ a b)))
  :loopy t
  :iter-bare ((return . returning))
  :iter-keyword ((return . return)))

(loopy-deftest wrap-not-linger
  :result nil
  :wrap ((x . `(let ((original-data (set-match-data nil)))
                 (equal original-data ,x))))
  :body ((_cycle 1)
         (_do (string-match (make-string 100 ?a)
                            (make-string 100 ?a)))
         (finally-return (match-data)))
  :loopy ((_cycle . cycle)
          (_do . do))
  :iter-bare ((_cycle . cycling)
              ;; Use `ignore' to eval arguments without doing anything.
              (_do . ignore))
  :iter-keyword ((_cycle . cycle)
                 (_do . do)))

;;;; Final Instructions
(loopy-deftest finally-do
  :result 10
  :wrap ((x . `(let ((my-var)) ,x my-var)))
  :body ((_list i (number-sequence 1 10))
         (finally-do (setq my-var i)))
  :loopy ((_list . list))
  :iter-bare ((_list . listing))
  :iter-keyword ((_list . list)))

(loopy-deftest finally-do-not-affect-return
  :result nil
  :body ((_list i (number-sequence 1 10))
         (finally-do 3))
  :loopy ((_list . list))
  :iter-bare ((_list . listing))
  :iter-keyword ((_list . list)))

(loopy-deftest finally-return-single-value
  :result 10
  :body ((_list i (number-sequence 1 10))
         (finally-return i))
  :loopy ((_list . list))
  :iter-bare ((_list . listing))
  :iter-keyword ((_list . list)))

(loopy-deftest finally-return-list-of-values
  :result '(10 7)
  :body ((_list i (number-sequence 1 10))
         (finally-return i 7))
  :loopy ((_list . list))
  :iter-bare ((_list . listing))
  :iter-keyword ((_list . list)))

;;;; Finally Protect
(loopy-deftest finally-protect
  :result (list 1 4 '(1 2 3 4))
  :wrap ((x . `(let ((test-result))
                 (should-error ,x :type '(error))
                 test-result)))
  :body ((with (example-var 1))
         (_list i '(1 2 3 4 5))
         (_collect my-collection i)
         (when (> i 3) (_do (error "%s" (list i))))
         (finally-protect (setq test-result (list example-var i my-collection))))
  :loopy ((_list . list)
          (_collect . collect)
          (_do . do))
  :iter-bare ((_list . listing)
              (_collect . collecting)
              (_do . ignore))
  :iter-keyword ((_list . list)
                 (_collect . collect)
                 (_do . do)))

;;;; Changing the order of macro arguments.
(loopy-deftest change-order-of-commands
  :result 7
  :body ((list i '(1 2 3))
         (finally-return (+ i a))
         (with (a 4)))
  :loopy t
  :iter-bare ((list . listing))
  :iter-keyword (list))

;;;; Default return values.
(loopy-deftest default-return-nil
  :result nil
  :multi-body t
  :body (((list i '(1 2 3)))
         ((cycle 1)
          (finally-do (1+ 1))))
  :loopy t
  :iter-bare ((list . listing)
              (cycle . cycling))
  :iter-keyword (list cycle))

;;;; Optimized Named  Accumulations
(defmacro loopy--optimized-vars-tests ()
  `(progn
     ,@(cl-loop
        for var in '(coll (coll end) (coll start) (coll beginning))
        for suffix in '("" "-with-pos-end" "-with-pos-start" "-with-pos-beginning")
        collect `(progn
                   (loopy-deftest ,(intern (concat "optimized-named-vars-adjoin" suffix))
                     :result '(1 2 3)
                     :body ((accum-opt ,var)
                            (array i [1 2 3])
                            (adjoin coll i)
                            (finally-return coll))
                     :loopy t
                     :iter-bare ((array . arraying)
                                 (adjoin . adjoining))
                     :iter-keyword (array adjoin))

                   (loopy-deftest ,(intern (concat "optimized-named-vars-collect" suffix))
                     :result '(1 2 3)
                     :body ((accum-opt ,var)
                            (array i [1 2 3])
                            (collect coll i)
                            (finally-return coll))
                     :loopy t
                     :iter-bare ((array . arraying)
                                 (collect . collecting))
                     :iter-keyword (array collect))

                   (loopy-deftest ,(intern (concat "optimized-named-vars-append" suffix))
                     :result '(1 2 2 3 3 4)
                     :body ((accum-opt ,var)
                            (array i (vector (list 1 2)
                                             (list 2 3)
                                             (list 3 4)))
                            (append coll i)
                            (finally-return coll))
                     :loopy t
                     :iter-bare ((array . arraying)
                                 (append . appending))
                     :iter-keyword (append array))

                   (loopy-deftest ,(intern (concat "optimized-named-vars-nconc" suffix))
                     :result '(1 2 2 3 3 4)
                     :body ((accum-opt ,var)
                            (array i (vector (list 1 2)
                                             (list 2 3)
                                             (list 3 4)))
                            (nconc coll i)
                            (finally-return coll))
                     :loopy t
                     :iter-bare ((array . arraying)
                                 (nconc . nconcing))
                     :iter-keyword (nconc array))

                   (loopy-deftest ,(intern (concat "optimized-named-vars-union" suffix))
                     :result '(1 2 3 4)
                     :body ((accum-opt ,var)
                            (array i (vector (list 1 2)
                                             (list 2 3)
                                             (list 3 4)))
                            (union coll i)
                            (finally-return coll))
                     :loopy t
                     :iter-bare ((array . arraying)
                                 (union . unioning))
                     :iter-keyword (union array))

                   (loopy-deftest ,(intern (concat "optimized-named-vars-nunion" suffix))
                     :result '(1 2 3 4)
                     :body ((accum-opt ,var)
                            (array i (vector (list 1 2)
                                             (list 2 3)
                                             (list 3 4)))
                            (nunion coll i)
                            (finally-return coll))
                     :loopy t
                     :iter-bare ((array . arraying)
                                 (nunion . nunioning))
                     :iter-keyword (nunion array))

                   (loopy-deftest ,(intern (concat "optimized-named-vars-concat" suffix))
                     :result "abcdef"
                     :body ((accum-opt ,var)
                            (array i ["ab" "cd" "ef"])
                            (concat coll i)
                            (finally-return coll))
                     :loopy t
                     :iter-bare ((array . arraying)
                                 (concat . concating))
                     :iter-keyword (array concat))

                   (loopy-deftest ,(intern (concat "optimized-named-vars-vconcat" suffix))
                     :result [1 2 2 3 3 4]
                     :body ((accum-opt ,var)
                            (array i (vector (list 1 2)
                                             (list 2 3)
                                             (list 3 4)))
                            (vconcat coll i)
                            (finally-return coll))
                     :loopy t
                     :iter-bare ((array . arraying)
                                 (vconcat . vconcating))
                     :iter-keyword (array vconcat))))))

(loopy--optimized-vars-tests)

;;; Loop Commands
;;;; Sub-loop Commands
;;;;; loopy command

(loopy-deftest same-level-at-accum
  :result '(1 2 3 4)
  :body (outer
         (list i '(1 2 3 4))
         (at outer (collect i)))
  :loopy t
  :iter-bare ((list . listing)
              (collect . collecting))
  :iter-keyword (list collect at))

(loopy-deftest loopy-at-accum
  :result '(1 2 3 4)
  :multi-body t
  :body ((outer
          (array i [(1 2) (3 4)])
          (loopy (list j i)
                 (at outer (collect j))))
         ((named outer)
          (array i [(1 2) (3 4)])
          (loopy (list j i)
                 (at outer (collect j)))))
  :loopy t
  ;; `loopy' should work barely.
  :iter-bare ((array . arraying))
  ;; "for loopy"" should work, but is redundant and unneeded.
  :iter-keyword (array loopy))

(loopy-deftest loopy-at-leave
  :result '(1 2 3)
  :multi-body t
  :body ((outer
          (array i [(1 2) (3 4) (5 6)])
          (loopy (list j i)
                 (at outer (if (> j 3)
                               (leave)
                             (collect j)))))
         ((named outer)
          (array i [(1 2) (3 4) (5 6)])
          (loopy (list j i)
                 (at outer (if (> j 3)
                               (leave)
                             (collect j))))))
  :loopy t
  ;; `loopy' should work barely.
  :iter-bare ((array . arraying))
  ;; "for loopy"" should work, but is redundant and unneeded.
  :iter-keyword (array loopy))

(loopy-deftest at-disagreeing-accum-types
  :error loopy-incompatible-accumulation-types
  :macroexpand t
  :multi-body t
  :body ((outer
          (list i '([1 2] [3]))
          (collect i)
          (loopy (array j i)
                 (at outer (max j))))
         (outer
          (list i '([1 2] [3]))
          (collect i)
          (at outer (_max j))))
  :loopy ((_max . max))
  :iter-bare ((list . listing)
              (collect . collecting)
              (_max . maximizing)
              (max . maximizing))
  :iter-keyword ((list . list)
                 (collect . collect)
                 (_max . max)))

(loopy-deftest loopy-cmd-implicit-accum-in-loop
  :result '(0 (1 . 4) (1 . 5) (2 . 4) (2 . 5))
  :multi-body t
  :body ((outer
          (list i '(1 2))
          (loopy (array j [4 5])
                 (at outer (collect (cons i j))))
          (after-do (push 0 loopy-result)))
         ((named outer)
          (list i '(1 2))
          (loopy (array j [4 5])
                 (at outer (collect (cons i j))))
          (finally-return (cons 0 loopy-result))))
  :loopy t
  :iter-bare ((list . listing))
  :iter-keyword (list))

(loopy-deftest loopy-cmd-leave-early
  :result '(1 2 3)
  :body (outer
         (_list i '(1 2 3))
         (loopy (list j '(4 5 6))
                (leave)
                (at outer (collect j)))
         (_collect i))
  :loopy ((_list . list)
          (_collect . collect))
  :iter-bare ((_list . listing)
              (_collect . collecting))
  :iter-keyword ((_list . list)
                 (_collect . collect)))

(loopy-deftest loopy-cmd-skip
  :doc "A `skip' in a sub-loop should not affect the outer loop."
  :result '(5 7 1 5 7 2 5 7 3)
  :body (outer
         (_list i '(1 2 3))
         (loopy (list j '(4 5 6 7 8))
                (when (cl-evenp j)
                  (continue))
                (at outer (collect j)))
         (_collect i))
  :loopy ((_list . list)
          (_collect . collect))
  :iter-bare ((_list . listing)
              (_collect . collecting))
  :iter-keyword ((_list . list)
                 (_collect . collect)))

(loopy-deftest loopy-cmd-return-from-outer
  :result 3
  :body (outer
         (_list i '(1 2 3))
         (loopy (list j '(4 5 6 3))
                (when (= j i)
                  (return-from outer j))))
  :loopy ((_list . list))
  :iter-bare ((_list . listing))
  :iter-keyword ((_list . list)))

(loopy-deftest loopy-cmd-named
  :result '((3 5) (3 5))
  :body (outer
         (_cycle 2)
         (loopy inner1
                (list j '(3 4))
                (loopy (list k '(5 6 7))
                       (if (= k 6)
                           ;; Return from inner1 so never reach 4.
                           (return-from inner1)
                         (at outer (collect (list j k)))))))
  :loopy ((_cycle . cycle))
  :iter-bare ((_cycle . cycling))
  :iter-keyword ((_cycle . cycle)))

;;;; Generic Evaluation
;;;;; Do
(loopy-deftest do
  :result '(t nil)
  :body ((with (my-val nil)
               (this-nil? t))
         (do (setq my-val t)
             (setq this-nil? (not my-val)))
         (return nil)
         (finally-return my-val this-nil?))
  :loopy t
  :iter-keyword (do return))

;;;;; Set
(loopy-deftest set-with
  :doc "Test to make sure that we can replace `:init' with `with'."
  :result 3
  :body ((with (var 0))
         (cycle 3)
         (set var (1+ var))
         (finally-return var))
  :loopy t
  :iter-keyword (cycle set)
  :iter-bare ((cycle . cycling)
              (set . setting)))

(loopy-deftest set-with-destr
  :doc "Test to make sure that we can replace `:init' with `with'."
  :result '((0 0 0) (1 2 3))
  :body ((with (i 0)
               (j 0)
               (k 0))
         (collect (list i j k))
         (set (i j k) '(1 2 3))
         (collect (list i j k))
         (leave))
  :loopy t
  :iter-keyword (leave set collect)
  :iter-bare ((collect . collecting)
              (leave . leaving)
              (set . setting)))

(loopy-deftest set-when
  :result '(nil 0 0 1 1 2 2 3)
  :body ((list i '(1 2 3 4 5 6 7 8))
         (when (cl-evenp i)
           (set j 0 (1+ j)))
         (collect j))
  :loopy t
  :iter-keyword (list set collect)
  :iter-bare ((list . listing)
              (set . setting)
              (collect . collecting)))

(defmacro loopy--set-destr-tests ()
  "Implementation is different for more than 2 values."
  (macroexp-progn
   (cl-loop with list = '((1 2) (3 4) (5 6) (7 8))
            for num from 1 to (length list)
            for subseq = (cl-subseq list 0 num)
            collect `(loopy-deftest ,(intern (format "expr-destr-%d-value" num))
                       :result (quote ,subseq)
                       :repeat _set
                       :body ((cycle ,num)
                              (_set (i j) ,@(cl-loop for i in subseq
                                                     collect `(quote ,i)))
                              (collect coll (list i j))
                              (finally-return coll))
                       :loopy ((_set . (set expr)))
                       :iter-bare ((_set . (setting))
                                   (collect . collecting)
                                   (cycle . cycling))
                       :iter-keyword ((_set . (set expr))
                                      (collect . collect)
                                      (cycle . cycle))))))

(loopy--set-destr-tests)

(defmacro loopy--set-value-tests ()
  "Implementation is different for more than 2 values."
  (macroexp-progn
   (cl-loop with list = '(1 2 3 4 5)
            and len = 10
            for num in '(1 2 3 5)
            for subseq = (cl-subseq list 0 num)
            for result = (append subseq (make-list (- len num) num))
            collect `(loopy-deftest ,(intern (format "expr-%d-values" num))
                       :result (quote ,result)
                       :repeat _set
                       :body ((cycle ,len)
                              (_set i ,@subseq)
                              (collect coll i)
                              (finally-return coll))
                       :loopy ((_set . (set expr)))
                       :iter-bare ((_set . (setting))
                                   (collect . collecting)
                                   (cycle . cycling))
                       :iter-keyword ((_set . (set expr))
                                      (collect . collect)
                                      (cycle . cycle))))))

(loopy--set-value-tests)

(loopy-deftest set-dont-repeat
  :doc "Make sure commands don't repeatedly create/declare the same variable."
  :result 1
  :wrap ((x . `(with-temp-buffer
                 (prin1 (macroexpand (quote ,x))
                        (current-buffer))
                 (goto-char (point-min))
                 (how-many "(j nil)"))))
  :body ((set j 3)
         (set j 4)
         (return j))
  :loopy t
  :iter-keyword (set return)
  :iter-bare ((set . setting)
              (return . returning)))

;;;;; command-do
(loopy-deftest command-do
  :result '((2 4 6) (2 4 6))
  :body ((list i '(1 2 3 4 5 6))
         (if (cl-evenp i)
             (_command-do (collect c1 i)
                          (collect c2 i)))
         (finally-return c1 c2))
  :repeat _command-do
  :loopy ((_command-do . (command-do command-do)))
  ;; Technically don't need to test (and wouldn't work if we used `for' inside,
  ;; anyway).
  :iter-keyword ((list . list)
                 (_command-do . (command-do command-do))))

;;;;; Set-Prev
(loopy-deftest set-prev
  :result '(nil 1 2 3 4)
  :body ((list i '(1 2 3 4 5))
         (_set-prev j i)
         (collect j))
  :repeat _set-prev
  :loopy ((_set-prev . (set-prev prev-set prev-expr)))
  :iter-bare ((list . listing)
              (collect . collecting)
              (_set-prev . (setting-prev)))
  :iter-keyword ((list . list)
                 (collect . collect)
                 (_set-prev . (set-prev prev-set prev-expr))))

(loopy-deftest set-prev-keyword-back
  :result '(nil nil nil 1 2)
  :multi-body t
  :body [((list i '(1 2 3 4 5))
          (set-prev j i :back 3)
          (collect j))

         ((with (n 3)
                (first-time t))
          (list i '(1 2 3 4 5))
          (set-prev j i :back (if first-time
                                  (progn
                                    (setq first-time nil)
                                    n)
                                (error "Evaluated more than once.")))
          (collect j))]
  :loopy t
  :iter-bare ((list . listing)
              (collect . collecting)
              (set-prev . setting-prev))
  :iter-keyword (list set-prev collect))

(loopy-deftest set-prev-keyword-with
  :result '(first-val first-val 2 2 4 4 6 6 8 8)
  :body ((with (j 'first-val))
         (numbers i :from 1 :to 10)
         (when (cl-oddp i)
           (set-prev j i))
         (collect j))
  :loopy t
  :iter-bare ((numbers . numbering)
              (collect . collecting)
              (set-prev . setting-prev))
  :iter-keyword (numbers set-prev collect))

(loopy-deftest set-prev-:back-with-destructure
  :doc "Special behavior in command for with-bound variables."
  :result '((first-val 22) (first-val 22) (1 2) (3 4))
  :body ((with (j 'first-val)
               (k 22))
         (list i '((1 . 2) (3 . 4) (5 . 6) (7 . 8)))
         (set-prev (j . k) i :back 2)
         (collect (loopy-test-escape (list j k))))
  :loopy t
  :iter-bare ((list . listing)
              (collect . collecting)
              (set-prev . setting-prev))
  :iter-keyword (list set-prev collect))

(loopy-deftest set-prev-destructuring
  :result '((7 7 1 3) (7 7 2 4))
  :body ((with (a 7)
               (b 7))
         (list i '((1 2) (3 4) (5 6) (7 8)))
         (set-prev (a b) i :back 2)
         (collect c1 a)
         (collect c2 b)
         (finally-return c1 c2))
  :loopy t
  :iter-bare ((list . listing)
              (collect . collecting)
              (set-prev . setting-prev))
  :iter-keyword (list set-prev collect))

;;;; Iteration
;; Making sure iteration fails in sub-level
(defmacro test--iteration-sub-level ()
  (let ((plain-cmds '( list list-ref
                       cons
                       array array-ref
                       seq seq-ref seq-index
                       cycle
                       iter
                       map map-ref
                       numbers numbers-down numbers-up))
        (ing-cmds   '( listing listing-ref
                       consing
                       arraying arraying-ref
                       sequencing sequencing-ref sequencing-index
                       cycling
                       iterating
                       mapping mapping-ref
                       numbering numbering-down numbering-up)))
    `(progn
       ,@(cl-loop
          for body in '(((if t (_cmd i '(1))) (finally-return t))
                        ((when t (_cmd i '(1))) (finally-return t))
                        ((unless t (_cmd i '(1))) (finally-return t))
                        ((cond (t (_cmd i '(1)))) (finally-return t)))
          for sub-cmd = (car (car body))
          collect `(loopy-deftest ,(intern (format "iteration-sub-level-%s"
                                                   sub-cmd))
                     :error loopy-iteration-in-sub-level
                     :repeat _cmd
                     :loopy ((_cmd . ,plain-cmds))
                     :iter-bare ((_cmd . ,ing-cmds))
                     :iter-keyword ((_cmd . ,plain-cmds))
                     :body ,body))

       (loopy-deftest iteration-sub-level-group
         :doc "Don't test `group' for `iter-bare'."
         :error loopy-iteration-in-sub-level
         :repeat _cmd
         :loopy ((_cmd . ,plain-cmds))
         :iter-keyword ((_cmd . ,plain-cmds))
         :body  ((group (_cmd i '(1))) (finally-return t))))))

(test--iteration-sub-level)

(loopy-deftest iteration-same-var-multiple-cmd
  :doc "Can't bind the same iteration variable with multiple commands."
  :error loopy-reinitializing-iteration-variable
  :body ((list i '(1 2 3))
         (list i '(1 2 3)))
  :loopy t
  :iter-bare ((list . listing))
  :iter-keyword (list))

;;;;; Array
(loopy-deftest array
  :result '(97 98 99)
  :multi-body t
  :body [((array  i [97 98 99]) (collect coll i) (finally-return coll))
         ((string i "abc")      (collect coll i) (finally-return coll))]
  :loopy t
  :iter-keyword (array collect string)
  :iter-bare ((array . arraying)
              (string . stringing)
              (collect . collecting)))

(loopy-deftest array-vars
  :doc "Test behavior for using numbers stored in variable vs. using numbers directly.
Using numbers directly will use less variables and more efficient code."
  :result '(2 4 6 8)
  :multi-body t
  :body [((with (start 2) (end 8)
                (arr (cl-coerce (number-sequence 0 10) 'vector)))
          (array i arr :from start :to end :by 2)
          (collect i))
         ((with (start 2) (end 8) (step 2)
                (arr (cl-coerce (number-sequence 0 10) 'vector)))
          (array i arr :from start :to end :by step)
          (collect i))]
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))

(loopy-deftest array-destructuring
  :doc "Check that `array' implements destructuring, not destructuring itself."
  :result '(5 6 7 8)
  :body ((array (a b c . d) [(1 2 3 . 4) (5 6 7 . 8)])
         (finally-return a b c d))
  :loopy t
  :iter-keyword (array)
  :iter-bare ((array . arraying)))

(loopy-deftest array-multi-array
  :result '((1 3) (1 4) (2 3) (2 4))
  :body ((array i [1 2] [3 4])
         (collect i))
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))

(loopy-deftest array-multi-array-:by
  :result '((1 3) (2 3))
  :body ((array i [1 2] [3 4] :by 2)
         (collect i))
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))

(loopy-deftest array-multi-array-quote
  :doc "Just to check how quoting is handled."
  :result '((1 3) (1 4) (2 3) (2 4))
  :body ((array i  `[1 ,(1+ 1)] [3 4])
         (collect i))
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))

(loopy-deftest array-keywords-:index
  :result  '((0 . 4) (1 . 3) (2 . 2) (3 . 1) (4 . 0))
  :body ((array i [4 3 2 1 0] :index cat)
         (collect (cons cat i)))
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))

(loopy-deftest array-keywords-:by
  :result '(0 2 4 6 8 10)
  :body ((array i [0 1 2 3 4 5 6 7 8 9 10] :by 2)
         (collect i))
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))

(loopy-deftest array-keywords-:downfrom
  :result '(10 8 6 4 2 0)
  :body ((array i (vector 0 1 2 3 4 5 6 7 8 9 10)
                :downfrom 10 :by 2)
         (collect i))
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))


(loopy-deftest array-keywords-:from-:downto-:by
  :result  '(8 6 4 2)
  :body ((array i [0 1 2 3 4 5 6 7 8 9 10]
                :from 8 :downto 1 :by 2)
         (collect i))
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))

(loopy-deftest array-keywords-:upto
  :result  '(0 1 2 3 4 5 6 7)
  :body ((array i [0 1 2 3 4 5 6 7 8 9 10] :upto 7)
         (collect i))
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))


(loopy-deftest array-keywords-:to
  :result  '(0 1 2 3 4 5 6 7)
  :body ((array i [0 1 2 3 4 5 6 7 8 9 10] :to 7)
         (collect i))
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))

(loopy-deftest array-keywords-:downto
  :result  '(10 9 8 7 6 5 4 3)
  :body ((array i [0 1 2 3 4 5 6 7 8 9 10] :downto 3)
         (collect i))
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))


(loopy-deftest array-keywords-:above
  :result  '(10 9 8)
  :body ((array i [0 1 2 3 4 5 6 7 8 9 10] :above 7)
         (collect i))
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))

(loopy-deftest array-keywords-:below
  :result  '(0 1 2)
  :body ((array i [0 1 2 3 4 5 6 7 8 9 10] :below 3)
         (collect i))
  :loopy t
  :iter-keyword (collect array)
  :iter-bare ((collect . collecting)
              (array . arraying)))

(loopy-deftest array-:test
  :result '(8 6 4 2)
  :body ((with (start 8)
               (end 2)
               (step -2))
         (array i [0 1 2 3 4 5 6 7 8 9 10]
                :from start :to end :by step
                :test #'>=)
         (collect i))
  :loopy t
  :iter-keyword (array collect)
  :iter-bare ((array . arraying)
              (collect . collecting)))

(loopy-deftest array-:test-just-once
  :result '(2 4 6 8)
  :body ((with (times 0))
         (array i [0 1 2 3 4 5 6 7 8 9 10]
                :from 2 :to 8 :by 2
                :test (progn
                        (cl-assert (= times 0))
                        (cl-incf times)
                        #'<=))
         (collect i))
  :loopy t
  :iter-keyword (array collect)
  :iter-bare ((array . arraying)
              (collect . collecting)))

;;;;; Array Ref
(loopy-deftest array-ref
  :result "aaa"
  :body ((with (my-str "cat"))
         (_cmd i my-str)
         (do (setf i ?a))
         (finally-return my-str))
  :repeat _cmd
  :loopy ((_cmd . (array-ref string-ref arrayf stringf)))
  :iter-keyword ((_cmd . (array-ref string-ref arrayf stringf))
                 (do . do))
  :iter-bare ((_cmd . (arraying-ref stringing-ref))
              (do . ignore)))

(loopy-deftest array-ref-destructuring
  :doc  "Check that `array-ref' implements destructuring, not destructuring itself."
  :result [(7 8 9) (7 8 9)]
  :body ((with (my-array [(1 2 3) (4 5 6)]))
         (array-ref (i j k) my-array)
         (do (setf i 7)
             (setf j 8)
             (setf k 9))
         (finally-return my-array))
  :loopy t
  :iter-keyword (array-ref do)
  :iter-bare ((array-ref . arraying-ref)
              (do . ignore)))


(loopy-deftest array-ref-keywords-:by
  :result "a1a3a5a7a9"
  :body  ((with (my-str "0123456789"))
          (array-ref i my-str :by 2)
          (do (setf i ?a))
          (finally-return my-str))
  :loopy t
  :iter-keyword (array-ref do)
  :iter-bare ((array-ref . arraying-ref)
              (do . ignore)))

(loopy-deftest array-ref-keywords-:by-:index
  :result  "a1a3a5a7a9"
  :body  ((with (my-str "0123456789"))
          (array-ref i my-str :by 2 :index cat)
          (do (setf (aref my-str cat) ?a))
          (finally-return my-str))
  :loopy t
  :iter-keyword (array-ref do)
  :iter-bare ((array-ref . arraying-ref)
              (do . ignore)))

(loopy-deftest array-ref-keywords-:from-:by
  :result  "0a2a4a6a8a"
  :body  ((with (my-str "0123456789"))
          (array-ref i my-str :from 1 :by 2 )
          (do (setf i ?a))
          (finally-return my-str))
  :loopy t
  :iter-keyword (array-ref do)
  :iter-bare ((array-ref . arraying-ref)
              (do . ignore)))

(loopy-deftest array-ref-keywords-:downto-:by
  :result  "0123456a8a"
  :body  ((with (my-str "0123456789"))
          (array-ref i my-str :downto 6 :by 2 )
          (do (setf i ?a))
          (finally-return my-str))
  :loopy t
  :iter-keyword (array-ref do)
  :iter-bare ((array-ref . arraying-ref)
              (do . ignore)))

(loopy-deftest array-ref-keywords-:below
  :result  "aaaaa56789"
  :body  ((with (my-str "0123456789"))
          (array-ref i my-str :below 5)
          (do (setf i ?a))
          (finally-return my-str))
  :loopy t
  :iter-keyword (array-ref do)
  :iter-bare ((array-ref . arraying-ref)
              (do . ignore)))

(loopy-deftest array-ref-keywords-:above
  :result  "012345aaaa"
  :body  ((with (my-str "0123456789"))
          (array-ref i my-str :above 5)
          (do (setf i ?a))
          (finally-return my-str))
  :loopy t
  :iter-keyword (array-ref do)
  :iter-bare ((array-ref . arraying-ref)
              (do . ignore)))

(loopy-deftest array-ref-keywords-:upto
  :result  "aaaaaa6789"
  :body  ((with (my-str "0123456789"))
          (array-ref i my-str :upto 5)
          (do (setf i ?a))
          (finally-return my-str))
  :loopy t
  :iter-keyword (array-ref do)
  :iter-bare ((array-ref . arraying-ref)
              (do . ignore)))

(loopy-deftest array-ref-keywords-:upfrom-:by
  :result  "0a2a4a6a8a"
  :body  ((with (my-str "0123456789"))
          (array-ref i my-str :upfrom 1 :by 2 )
          (do (setf i ?a))
          (finally-return my-str))
  :loopy t
  :iter-keyword (array-ref do)
  :iter-bare ((array-ref . arraying-ref)
              (do . ignore)))

(loopy-deftest array-ref-vars
  :doc "Test behavior for using numbers stored in variable vs. using numbers directly.
Using numbers directly will use less variables and more efficient code."
  :result [0 1 22 3 22 5 22 7 22 9 10]
  :multi-body t
  :body [((with (start 2) (end 8) (arr (cl-coerce (number-sequence 0 10) 'vector)))
          (array-ref i arr :from start :to end :by 2)
          (do (setf i 22))
          (finally-return arr))
         ((with (start 2) (end 8) (step 2) (arr (cl-coerce (number-sequence 0 10) 'vector)))
          (array-ref i arr :from start :to end :by step)
          (do (setf i 22))
          (finally-return arr))]
  :loopy t
  :iter-keyword (array-ref do)
  :iter-bare ((array-ref . arraying-ref)
              (do . ignore)))

(loopy-deftest array-ref-:test
  :result [0 1 22 3 22 5 22 7 22 9 10]
  :body ((with (start 8)
               (end 2)
               (step -2)
               (arr (vector 0 1 2 3 4 5 6 7 8 9 10)))
         (array-ref i arr
                    :from start :to end :by step
                    :test #'>=)
         (do (setf i 22))
         (finally-return arr))
  :loopy t
  :iter-keyword (array-ref do)
  :iter-bare ((array-ref . arraying-ref)
              (do . ignore)))

(loopy-deftest array-ref-:test-just-once
  :result [0 1 22 3 22 5 22 7 22 9 10]
  :body ((with (times 0)
               (arr (vector 0 1 2 3 4 5 6 7 8 9 10)))
         (array-ref i arr
                    :from 2 :to 8 :by 2
                    :test (progn
                            (cl-assert (= times 0))
                            (cl-incf times)
                            #'<=))
         (do (setf i 22))
         (finally-return arr))
  :loopy t
  :iter-keyword (array-ref do)
  :iter-bare ((array-ref . arraying-ref)
              (do . ignore)))

;;;;; Cons
(loopy-deftest cons
  :result '((1 2 3 4) (2 3 4) (3 4) (4))
  :body ((_cmd x '(1 2 3 4))
         (collect coll x)
         (finally-return coll))
  :repeat _cmd
  :loopy ((_cmd . (cons conses)))
  :iter-keyword ((_cmd . (cons conses))
                 (collect . collect))
  :iter-bare ((_cmd . (consing))
              (collect . collecting)))

(loopy-deftest cons-:by
  :result '((1 2 3 4) (3 4))
  :multi-body t
  :body [((cons x '(1 2 3 4) :by #'cddr)
          (collect coll x)
          (finally-return coll))
         ((cons x '(1 2 3 4) :by (lambda (y) (cddr y)))
          (collect coll x)
          (finally-return coll))
         ((with (f (lambda (y) (cddr y))))
          (cons x '(1 2 3 4) :by f)
          (collect coll x)
          (finally-return coll))]
  :loopy t
  :iter-keyword (cons collect)
  :iter-bare ((cons . consing)
              (collect . collecting)))

(loopy-deftest cons-:by-once-only
  :doc "The function should be evaluated only once."
  :result '((1 2 3 4) (3 4))
  :body ((with (times 0))
         (cons x '(1 2 3 4) :by (progn
                                  (if (> times 0)
                                      (error "Evaluated more than once")
                                    (cl-incf times)
                                    #'cddr)))
         (collect coll x)
         (finally-return coll))
  :loopy t
  :iter-keyword (cons collect)
  :iter-bare ((cons . consing)
              (collect . collecting)))

(loopy-deftest cons-destr
  :doc  "Check that `cons' implements destructuring, not destructuring itself."
  :result '((1 (2 3 4)) (2 (3 4)) (3 (4)) (4 nil))
  :body ((cons (i . j) '(1 2 3 4))
         (collect coll (list i j))
         (finally-return coll))
  :loopy t
  :iter-keyword (cons collect)
  :iter-bare ((cons . consing)
              (collect . collecting)))

(loopy-deftest cons-init-direct
  :doc "Check that `cons' immediately binds the value when possible."
  :result '((1 2 3 4) (1 2 3 4) (2 3 4) (2 3 4) (3 4) (3 4) (4) (4))
  :body  ((collect l)
          (cons l '(1 2 3 4))
          (collect l))
  :loopy t
  :iter-keyword (cons collect)
  :iter-bare ((cons . consing)
              (collect . collecting)))

(loopy-deftest cons-init-indirect
  :doc "Check that `cons' doesn't overwrite a with-bound value."
  :result '(25 (1 2 3 4) (1 2 3 4) (2 3 4) (2 3 4) (3 4) (3 4) (4))
  :body  ((with (l 25))
          (collect l)
          (cons l '(1 2 3 4))
          (collect l))
  :loopy t
  :iter-keyword (cons collect)
  :iter-bare ((cons . consing)
              (collect . collecting)))

;;;;; Iter
(loopy-deftest iter-single-var
  :doc "When single var, `iter' can bind the value directly."
  :result '(1 1 2 2 3 3)
  :body ((with (iter-maker (iter-lambda ()
                             (iter-yield 1)
                             (iter-yield 2)
                             (iter-yield 3))))
         (collect i)
         (iter i (funcall iter-maker))
         (collect i))
  :loopy t
  :iter-keyword (iter collect)
  :iter-bare ((iter . iterating)
              (collect . collecting)))

(loopy-deftest iter-single-var-with-bound
  :doc "When single var is `with'-bound, `iter' must be indirect."
  :result '(27 1 1 2 2 3)
  :body ((with (iter-maker (iter-lambda ()
                             (iter-yield 1)
                             (iter-yield 2)
                             (iter-yield 3)))
               (i 27))
         (collect i)
         (iter i (funcall iter-maker))
         (collect i))
  :loopy t
  :iter-keyword (iter collect)
  :iter-bare ((iter . iterating)
              (collect . collecting)))

(loopy-deftest iter-no-var
  :doc "When no var, as in non-with-bound single var, `iter' can bind the value directly."
  :result  '(1 2 3)
  :body ((with (iter-maker (iter-lambda ()
                             (iter-yield 1)
                             (iter-yield 2)
                             (iter-yield 3))))
         (iter (funcall iter-maker))
         (set i 1 (1+ i))
         (collect i))
  :loopy t
  :iter-keyword (iter collect set)
  :iter-bare ((iter . iterating)
              (collect . collecting)
              (set . setting)))

(loopy-deftest iter-close-twice
  :doc "Check that trying to close the iterator object twice doesn't signal an error."
  :result '(1 2)
  :body ((with (iter-maker (iter-lambda ()
                             (iter-yield 1)
                             (iter-yield 2)
                             (iter-yield 3)))
               (gen (funcall iter-maker)))
         (iter i gen :close t)
         (iter j gen :close t)
         (leave)
         (finally-return i j))
  :loopy t
  :iter-keyword (iter collect leave)
  :iter-bare ((iter . iterating)
              (collect . collecting)
              (leave . leaving)))

(loopy-deftest iter-close-value
  :doc "Check that `close' is respected when it's a variable."
  :result t
  :body (loopy (with (some-val nil)
                     (iter-maker (iter-lambda ()
                                   (iter-yield 1)
                                   (iter-yield 2)
                                   (iter-yield 3)))
                     (gen (funcall iter-maker)))
               (iter i gen :close some-val)
               (leave) ; Should not preventing closing or final updates.
               (finally-return
                ;; Older versions of Emacs don't have the `:success' clause,
                ;; so we work around it.
                (let ((val (condition-case err
                               (iter-next gen)
                             (iter-end-of-sequence 'fail)
                             (error
                              (signal (car err) (cdr err))))))
                  (if (eq val 'fail)
                      nil
                    (iter-close gen)
                    t))))
  :loopy t
  :iter-keyword (iter leave)
  :iter-bare ((iter . iterating)
              (leave . leaving)))

(loopy-deftest iter-same-gen
  :doc "Check that `iter' doesn't reset iterator objects."
  :result  '((1 . 2) (3 . 4))
  :body ((with (iter-maker (iter-lambda (x)
                             (while x
                               (iter-yield (pop x)))))
               (gen (funcall iter-maker (list 1 2 3 4))))
         (iter i gen :close nil)
         (iter j gen :close nil)
         (collect (cons i j)))
  :loopy t
  :iter-keyword (iter collect)
  :iter-bare ((iter . iterating)
              (collect . collecting)))

(loopy-deftest iter-destr
  :doc "`iter' should initialize destructured values to `nil'."
  :result  '((nil nil) (1 2) (1 2) (3 4) (3 4) (5 6))
  :body ((with (iter-maker (iter-lambda ()
                             (iter-yield (list 1 2))
                             (iter-yield (list 3 4))
                             (iter-yield (list 5 6)))))
         (collect (list i j))
         (iter (i j) (funcall iter-maker))
         (collect (list i j)))
  :loopy t
  :iter-keyword (iter collect)
  :iter-bare ((iter . iterating)
              (collect . collecting)))

;;;;; List
(loopy-deftest list-final-val
  :result 3
  :body ((list i '(1 2 3))
         ;; Same thing:
         ;; (after-do (cl-return i))
         (finally-return i))
  :loopy t
  :iter-keyword (list)
  :iter-bare ((list . listing)))

(loopy-deftest list-:by
  :result '(1 3)
  :multi-body t
  :body [((with (my-cddr (lambda (x)  (cddr x))))
          (list i '(1 2 3 4) :by my-cddr)
          (collect i) )
         ((list i '(1 2 3 4) :by (lambda (x) (cddr x)))
          (collect i))
         ((list i '(1 2 3 4) :by #'cddr)
          (collect i))]
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest list-:by-once-only
  :doc "The function should be evaluated only once."
  :result '(1 3)
  :body ((with (times 0))
         (list x '(1 2 3 4) :by (progn
                                  (if (> times 0)
                                      (error "Evaluated more than once")
                                    (cl-incf times)
                                    #'cddr)))
         (collect coll x)
         (finally-return coll))
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest list-destructuring
  :doc  "Check that `list' implements destructuring, not destructuring itself."
  :result '(5 6)
  :body ((list (a . b)
               '((1 . 2) (3 . 4) (5 . 6)))
         (finally-return a b))
  :loopy t
  :iter-keyword (list)
  :iter-bare ((list . listing)))

(loopy-deftest list-distribution
  :result '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))
  :body ((list i '(1 2 3) '(4 5 6))
         (collect i))
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest list-distribution-:by
  :result '((1 4)  (1 6) (2 5) (3 4) (3 6))
  :body ((list i '(1 2 3) '(4 5 6) :by #'cddr)
         (collect i))
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest list-distribution-destr
  :result '((1 1 2 2) (4 5 4 5))
  :body ((list (i j) '(1 2) '(4 5))
         (collect c1 i)
         (collect c2 j)
         (finally-return c1 c2))
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

;;;;; List Ref
(loopy-deftest list-ref
  :result '(7 7 7)
  :body ((with (my-list (list 1 2 3)))
         (list-ref i my-list)
         (do (setf i 7))
         (finally-return my-list))
  :loopy t
  :iter-keyword (list-ref do)
  :iter-bare ((list-ref . listing-ref)
              (do . ignore)))

(loopy-deftest list-ref-:by
  :result '(7 2 7)
  :multi-body t
  :body [((with (my-list (list 1 2 3)))
          (list-ref i my-list :by #'cddr)
          (do (setf i 7))
          (finally-return my-list))
         ((with (my-list (list 1 2 3)))
          (list-ref i my-list :by (lambda (x) (cddr x)))
          (do (setf i 7))
          (finally-return my-list))
         ((with (my-list (list 1 2 3)) (f (lambda (x) (cddr x))))
          (list-ref i my-list :by f)
          (do (setf i 7))
          (finally-return my-list))]
  :loopy t
  :iter-keyword (list-ref do)
  :iter-bare ((list-ref . listing-ref)
              (do . ignore)))

(loopy-deftest list-ref-:by-once-only
  :doc "The function should be evaluated only once."
  :result '(7 2 7)
  :body ((with (my-list (list 1 2 3))
               (f (lambda (x) (cddr x)))
               (times 0))
         (list-ref i my-list :by (progn
                                   (if (> times 0)
                                       (error "Evaluated more than once")
                                     (cl-incf times)
                                     #'cddr)))
         (do (setf i 7))
         (finally-return my-list))
  :loopy t
  :iter-keyword (list-ref do)
  :iter-bare ((list-ref . listing-ref)
              (do . ignore)))

(loopy-deftest list-ref-destructuring
  :doc  "Check that `list-ref' implements destructuring, not destructuring itself."
  :result '((7 8 9) (7 8 9))
  :body ((with (my-list (list (list 1 2 3) (list 4 5 6))))
         (list-ref (i j k) my-list)
         (do (setf i 7)
             (setf j 8)
             (setf k 9))
         (finally-return my-list))
  :loopy t
  :iter-keyword (list-ref do)
  :iter-bare ((list-ref . listing-ref)
              (do . ignore)))

;;;;; Map
(loopy-deftest map-alist
  :result '((a . 1) (b . 2))
  :repeat _map
  :body ((_map pair '((a . 1) (b . 2)))
         (collect coll pair)
         (finally-return coll))
  :loopy ((_map . (map map-pairs)))
  :iter-keyword ((_map . (map map-pairs))
                 (collect . collect))
  :iter-bare ((_map . (mapping mapping-pairs))
              (collect . collecting)))

(loopy-deftest map-plist
  :result '((a . 1) (b . 2))
  :repeat _map
  :body ((_map pair '(a 1 b 2))
         (collect coll pair)
         (finally-return coll))
  :loopy ((_map . (map map-pairs)))
  :iter-keyword ((_map . (map map-pairs))
                 (collect . collect))
  :iter-bare ((_map . (mapping mapping-pairs))
              (collect . collecting)))

(loopy-deftest map-vector
  :result '((0 . a) (1 . b) (2 . c) (3 . d))
  :repeat _map
  :body ((_map pair [a b c d])
         (collect coll pair)
         (finally-return coll))
  :loopy ((_map . (map map-pairs)))
  :iter-keyword ((_map . (map map-pairs))
                 (collect . collect))
  :iter-bare ((_map . (mapping mapping-pairs))
              (collect . collecting)))

(loopy-deftest map-hash-table
  :result '((a . 1) (b . 2))
  :repeat _map
  :body ((with (my-hash (let ((h (make-hash-table)))
                          (puthash 'a 1 h)
                          (puthash 'b 2 h)
                          h)))
         (_map pair my-hash)
         (collect coll pair)
         (finally-return coll))
  :loopy ((_map . (map map-pairs)))
  :iter-keyword ((_map . (map map-pairs))
                 (collect . collect))
  :iter-bare ((_map . (mapping mapping-pairs))
              (collect . collecting)))

(loopy-deftest map-:unique-t-1
  :doc "`:unique' it `t' by default."
  :result '((a . 1) (b . 2) (c . 3))
  :multi-body t
  :body [((map-pairs pair '((a . 1) (a . 27) (b . 2) (c . 3)))
          (collect coll pair)
          (finally-return coll))
         ((map-pairs pair '((a . 1) (a . 27) (b . 2) (c . 3)) :unique t)
          (collect coll pair)
          (finally-return coll))]
  :loopy t
  :iter-keyword (map-pairs collect)
  :iter-bare ((map-pairs . mapping-pairs)
              (collect . collecting)))

(loopy-deftest map-:unique-t-2
  :doc "Check that optimization to avoid `funcall' works."
  :result t
  :wrap ((x . `(not (string-match-p
                     "funcall"
                     (format "%S" (macroexpand-all (quote ,x) nil))))))
  :body ((map-pairs i map :unique t))
  :loopy t
  :iter-keyword (map-pairs)
  :iter-bare ((map-pairs . mapping-pairs)))

(loopy-deftest map-:unique-nil-1
  :doc "`:unique' it `t' by default.  Test when `nil'."
  :result '((a . 1) (a . 27) (b . 2) (c . 3))
  :body ((map-pairs pair '((a . 1) (a . 27) (b . 2) (c . 3)) :unique nil)
         (collect coll pair)
         (finally-return coll))
  :loopy t
  :iter-keyword (map-pairs collect)
  :iter-bare ((map-pairs . mapping-pairs)
              (collect . collecting)))

(loopy-deftest map-:unique-nil-2
  :doc "Check that optimization to avoid `funcall' works."
  :result t
  :wrap ((x . `(not (string-match-p
                     "funcall"
                     (format "%S" (macroexpand-all (quote ,x) nil))))))
  :body ((map-pairs i map :unique nil))
  :loopy t
  :iter-keyword (map-pairs)
  :iter-bare ((map-pairs . mapping-pairs)))

(loopy-deftest map-:unique-var
  :doc "`:unique' it `t' by default.  Test when `nil'."
  :result '((a . 1) (a . 27) (b . 2) (c . 3))
  :body ((with (cat nil))
         (map-pairs pair '((a . 1) (a . 27) (b . 2) (c . 3)) :unique cat)
         (collect coll pair)
         (finally-return coll))
  :loopy t
  :iter-keyword (map-pairs collect)
  :iter-bare ((map-pairs . mapping-pairs)
              (collect . collecting)))

(loopy-deftest map-destructuring
  :doc  "Check that `map' implements destructuring, not destructuring itself."
  :result '((a b) (1 2))
  :body ((map (key . val) '((a . 1) (b . 2)))
         (collect keys key)
         (collect vals val)
         (finally-return keys vals))
  :loopy t
  :iter-keyword (map collect)
  :iter-bare ((map . mapping)
              (collect . collecting)))

;;;;; Map Ref
(loopy-deftest map-ref
  :result [17 18 19 20 21]
  :body ((with (map (vector 10 11 12 13 14)))
         (_cmd i map)
         (do (cl-incf i 7))
         (finally-return map))
  :repeat _cmd
  :loopy ((_cmd . (map-ref mapf mapping-ref)))
  :iter-keyword ((_cmd . (map-ref mapf mapping-ref))
                 (do . do))
  :iter-bare ((_cmd . (mapping-ref))
              (do . ignore)))

(loopy-deftest map-ref-:key
  :result '([17 18 19 20 21] (0 1 2 3 4))
  :body ((with (map (vector 10 11 12 13 14)))
         (mapf i map :key my-key)
         (do (cl-incf i 7))
         (collect my-key)
         (finally-return map loopy-result))
  :loopy t
  :iter-keyword (mapf do collect)
  :iter-bare ((mapf . mapping-ref)
              (do . ignore)
              (collect . collecting)))

(loopy-deftest map-ref-:unique-t-1
  :doc "`:unique' is `t' by default."
  :result '(:a 8 :a 2 :b 10)
  :multi-body t
  :body [((with (map (list :a 1 :a 2 :b 3)))
          (map-ref i map)
          (do (cl-incf i 7))
          (finally-return map))
         ((with (map (list :a 1 :a 2 :b 3)))
          (map-ref i map :unique t)
          (do (cl-incf i 7))
          (finally-return map))]
  :loopy t
  :iter-keyword (map-ref do collect)
  :iter-bare ((map-ref . mapping-ref)
              (do . ignore)
              (collect . collecting)))

(loopy-deftest map-ref-:unique-t-2
  :doc "Check that optimization to avoid `funcall' works."
  :result t
  :wrap ((x . `(not (string-match-p
                     "funcall"
                     (format "%S" (macroexpand-all (quote ,x) nil))))))
  :body ((map-ref i map :unique t))
  :loopy t
  :iter-keyword (map-ref)
  :iter-bare ((map-ref . mapping-ref)))

(loopy-deftest map-ref-:unique-nil-1
  :doc "Fist `:a' becomes 15 because it gets found twice by `setf'."
  :result '(:a 15 :a 2 :b 10)
  :body ((with (map (list :a 1 :a 2 :b 3)))
         (map-ref i map :unique nil)
         (do (cl-incf i 7))
         (finally-return map))
  :loopy t
  :iter-keyword (map-ref do collect)
  :iter-bare ((map-ref . mapping-ref)
              (do . ignore)
              (collect . collecting)))

(loopy-deftest map-ref-:unique-nil-2
  :doc "Check that optimization to avoid `funcall' works."
  :result t
  :wrap ((x . `(not (string-match-p
                     "funcall"
                     (format "%S" (macroexpand-all (quote ,x) nil))))))
  :body ((map-ref i map :unique nil))
  :loopy t
  :iter-keyword (map-ref)
  :iter-bare ((map-ref . mapping-ref)))

(loopy-deftest map-ref-:unique-var
  :doc "Fist `:a' becomes 15 because it gets found twice by `setf'."
  :result '(:a 15 :a 2 :b 10)
  :body ((with (map (list :a 1 :a 2 :b 3))
               (cat nil))
         (map-ref i map :unique cat)
         (do (cl-incf i 7))
         (finally-return map))
  :loopy t
  :iter-keyword (map-ref do collect)
  :iter-bare ((map-ref . mapping-ref)
              (do . ignore)
              (collect . collecting)))

(loopy-deftest map-ref-destr
  :doc "Check that `map-ref' implements destructuring, not the destructuring itself."
  :result [[7 8] [7 8]]
  :body ((with (map (vector (vector 10 11)
                            (vector 12 13))))
         (map-ref [i j] map)
         (do (setf i 7)
             (setf j 8))
         (finally-return map))
  :loopy t
  :iter-keyword (map-ref do)
  :iter-bare ((map-ref . mapping-ref)
              (do . ignore)))

;;;;; Nums
;; TODO: Names `num' and `number' aren't listed in the Org doc.
;;       They should be removed.
(loopy-deftest numbers
  :result '(1 2 3 4 5)
  :repeat _cmd
  :body ((_cmd i :from 1 :to 5)
         (collect i))
  :loopy ((_cmd . (nums numbers num number)))
  :iter-keyword ((_cmd . (nums numbers num number))
                 (collect . collect))
  :iter-bare ((_cmd . (numbering))
              (collect . collecting)))

(loopy-deftest numbers-keywords-pos-args-should-error
  :doc "Make sure an error is signaled when using the now removed positional arguments."
  :error loopy-wrong-number-of-command-arguments-or-bad-keywords
  :body ((numbers i 1 5 2)
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

;;;;; Nums Keywords

(loopy-deftest numbers-keywords-:from-:to-:by
  :result '(0 2 4 6 8 10)
  :body ((numbers i :from 0 :to 10 :by 2)
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

(loopy-deftest numbers-keywords-:from-:downto-:by
  :result  '(8 6 4 2)
  :body ((numbers i :from 8 :downto 1 :by 2)
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

(loopy-deftest numbers-keywords-:upto
  :result  '(0 1 2 3 4 5 6 7)
  :body ((numbers i :upto 7)
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

(loopy-deftest numbers-keywords-:to
  :result  '(0 1 2 3 4 5 6 7)
  :body ((numbers i :to 7)
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

(loopy-deftest numbers-keywords-:from-:downto
  :result  '(10 9 8 7 6 5 4 3)
  :body ((numbers i :from 10 :downto 3)
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

(loopy-deftest numbers-keywords-:from-:above
  :result  '(10 9 8)
  :body ((numbers i :from 10 :above 7)
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

(loopy-deftest numbers-keywords-:below
  :result  '(0 1 2)
  :body ((numbers i :below 3)
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

(loopy-deftest numbers-keywords-:test
  :result '(1 2 3 4 5)
  :body ((numbers i :from 1 :to 5 :test #'<=)
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

(loopy-deftest numbers-keywords-:test-lambda
  :result '(1 2 3 4 5)
  :body ((numbers i :from 1 :to 5 :test (lambda (var end) (<= var end)))
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

(loopy-deftest numbers-keywords-:test-neg-step
  :result '(5 4 3 2 1)
  :body ((numbers i :from 5 :to 1 :by -1 :test #'>=)
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

(loopy-deftest numbers-keywords-test-err
  :error loopy-conflicting-command-arguments
  :multi-body t
  :body [((numbers i :test 'blah :downfrom 10))
         ((numbers i :test 'blah :upfrom 10))
         ((numbers i :test 'blah :downto 10))
         ((numbers i :test 'blah :upto 10))
         ((numbers i :test 'blah :above 10))
         ((numbers i :test 'blah :below 10))]
  :loopy t
  :iter-keyword (numbers)
  :iter-bare ((numbers . numbering)))

(loopy-deftest numbers-error-more-than-one
  :error loopy-conflicting-command-arguments
  :multi-body t
  :body [((numbers i :from 1 :upfrom 1))
         ((numbers i :from 1 :downfrom 1))
         ((numbers i :upfrom 1 :downfrom 1))

         ((numbers i :to 1 :upto 1))
         ((numbers i :to 1 :downto 1))
         ((numbers i :to 1 :above 1))
         ((numbers i :to 1 :below 1))
         ((numbers i :upto 1 :downto 1))
         ((numbers i :upto 1 :above 1))
         ((numbers i :upto 1 :below 1))
         ((numbers i :downto 1 :above 1))
         ((numbers i :downto 1 :below 1))
         ((numbers i :above 1 :below 1))]
  :loopy t
  :iter-keyword (numbers)
  :iter-bare ((numbers . numbering)))

(loopy-deftest numbers-error-conflicting-direction
  :error loopy-conflicting-command-arguments
  :multi-body t
  :body [((numbers i :upfrom 1 :downto 1))
         ((numbers i :upfrom 1 :above 1))
         ((numbers i :downfrom 1 :upto 1))
         ((numbers i :downfrom 1 :below 1))]
  :loopy t
  :iter-keyword (numbers)
  :iter-bare ((numbers . numbering)))

;;;;; Numbers With Vars
(loopy-deftest numbers-literal-by-and-literal-end
  :doc "Check the optimizing for non-variable `:by' and `:to' doesn't fail."
  :result '(2 4 6 8)
  :body ((with (start 2))
         (numbers i :from start :to 8 :by 2)
         (collect i))
  :loopy t
  :iter-keyword (numbers collect)
  :iter-bare ((numbers . numbering)
              (collect . collecting)))

(loopy-deftest numbers-literal-by-with-var-end
  :doc "Check the optimizing for non-variable `:by' and variable `:to' doesn't fail."
  :result '(2 4 6 8)
  :body ((with (start 2) (end 8))
         (numbers i :from start :to end :by 2)
         (collect i))
  :loopy t
  :iter-keyword (numbers collect)
  :iter-bare ((numbers . numbering)
              (collect . collecting)))

(loopy-deftest numbers-var-by-with-var-end
  :doc "Check the optimizing for variable `:by' and variable `:to' doesn't fail."
  :result '(2 4 6 8)
  :body ((with (start 2) (by 2) (end 8))
         (numbers i :from start :to end :by by)
         (collect i))
  :loopy t
  :iter-keyword (numbers collect)
  :iter-bare ((numbers . numbering)
              (collect . collecting)))

(loopy-deftest numbers-literal-by-and-no-end
  :doc "Check the optimizing for literal `:by' and no `:to'."
  :result '(2 4 6 8)
  :body ((with (start 2))
         (cycle 4)
         (numbers i :from start :by 2)
         (collect i))
  :loopy t
  :iter-keyword (numbers collect cycle)
  :iter-bare ((numbers . numbering)
              (collect . collecting)
              (cycle . cycling)))

(loopy-deftest numbers-var-by-and-no-end
  :doc "Check the optimizing for variable `:by' and no `:to'."
  :result '(2 4 6 8)
  :body ((with (start 2) (by 2))
         (cycle 4)
         (numbers i :from start :by by)
         (collect i))
  :loopy t
  :iter-keyword (numbers collect cycle)
  :iter-bare ((numbers . numbering)
              (collect . collecting)
              (cycle . cycling)))

(loopy-deftest numbers-no-with
  :doc "Var should be initialized to the first value."
  :result '(1 1 2 2)
  :body ((collect n)
         (numbers n :from 1 :to 2)
         (collect n))
  :loopy t
  :iter-keyword (numbers collect)
  :iter-bare ((numbers . numbering)
              (collect . collecting)))

(loopy-deftest numbers-yes-with
  :doc "Var should be initialized to the first value, unless with given."
  :result '(24 1 1 2)
  :body ((with (n 24))
         (collect n)
         (numbers n :from 1 :to 2)
         (collect n))
  :loopy t
  :iter-keyword (numbers collect)
  :iter-bare ((numbers . numbering)
              (collect . collecting)))

(loopy-deftest numbers-var-:test
  :result '(1 2 3 4 5)
  :body ((with (func #'<=))
         (numbers i :from 1 :to 5 :test func)
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

(loopy-deftest numbers-var-:test-neg-step
  :result '(5 4 3 2 1)
  :body ((with (func #'>=))
         (numbers i :from 5 :to 1 :by -1 :test func)
         (collect i))
  :loopy t
  :iter-keyword (collect numbers)
  :iter-bare ((collect . collecting)
              (numbers . numbering)))

;;;;; Nums-Down
(loopy-deftest numbers-down
  :result '(10 8 6 4 2)
  :multi-body t
  :body [((_cmd i 10 1 :by 2) (collect i))
         ((_cmd i 10 1 2) (collect i))]
  :repeat _cmd
  :loopy ((_cmd . (nums-down numsdown numbers-down)))
  :iter-keyword ((_cmd . (nums-down numsdown numbers-down))
                 (collect . collect))
  :iter-keyword ((_cmd . (numbering-down))
                 (collect . collecting)))

;;;;; Nums-Up
(loopy-deftest numbers-up
  :result '(1 3 5 7 9)
  :multi-body t
  :body [((_cmd i 1 10 :by 2) (collect i))
         ((_cmd i 1 10 2) (collect i))]
  :repeat _cmd
  :loopy ((_cmd . (nums-up numsup numbers-up)))
  :iter-keyword ((_cmd . (nums-up numsup numbers-up))
                 (collect . collect))
  :iter-keyword ((_cmd . (numbering-up))
                 (collect . collecting)))

;;;;; Repeat
(loopy-deftest cycle-no-var
  :result '(1 2 3)
  :body ((_cmd 3)
         (list i (number-sequence 1 10))
         (collect coll i)
         (finally-return coll))
  :repeat _cmd
  :loopy ((_cmd . (cycle repeat)))
  :iter-keyword ((_cmd . (cycle repeat))
                 (list . list)
                 (collect . collect))
  :iter-bare ((_cmd . (cycling repeating))
              (list . listing)
              (collect . collecting)))

(loopy-deftest cycle-yes-var
  :doc "Need to test order of execution and functionality."
  :result '(0 1 2)
  :body ((collect coll i)
         (_cmd i 3)
         (finally-return coll))
  :repeat _cmd
  :loopy ((_cmd . (cycle repeat)))
  :iter-keyword ((_cmd . (cycle repeat))
                 (list . list)
                 (collect . collect))
  :iter-bare ((_cmd . (cycling repeating))
              (list . listing)
              (collect . collecting)))

(loopy-deftest cycle-init-no-with
  :doc "Variable is initialized to 0."
  :result '(0 0 1 1 2 2)
  :body ((collect my-var)
         (cycle my-var 3)
         (collect my-var))
  :loopy t
  :iter-keyword (collect cycle)
  :iter-bare ((collect . collecting)
              (cycle . cycling)))

(loopy-deftest cycle-init-yes-with
  :doc "Variable is initialized to 0, except from `with'."
  :result '(cat 0 0 1 1 2)
  :body ((with (my-var 'cat))
         (collect my-var)
         (cycle my-var 3)
         (collect my-var))
  :loopy t
  :iter-keyword (collect cycle)
  :iter-bare ((collect . collecting)
              (cycle . cycling)))

;;;;; Seq
(loopy-deftest sequence
  :result t
  :body ((_cmd l '(97 98 99 100 101))
         (_cmd a [97 98 99 100 101])
         (_cmd s "abcde")
         (if (not (and (/= l a)
                       (/= a s)))
             (return nil))
         (finally-return t))
  :repeat _cmd
  :loopy ((_cmd . (seq sequence seqing sequencing elements)))
  :iter-keyword ((_cmd . (seq sequence seqing sequencing elements))
                 (return . return))
  :iter-bare ((seq . (seqing sequencing))
              (return . returning)))

(loopy-deftest seq-destructuring
  :doc "Check that `seq' implements destructuring, not the destructuring itself."
  :result '(97 98)
  :multi-body t
  :body [((seq (a . b) [(97 . 98)]) (finally-return a b))
         ((seq [a b] '([97 98])) (finally-return a b))
         ((seq [a b] ["ab"]) (finally-return a b))]
  :loopy t
  :iter-keyword (seq)
  :iter-bare ((seq . sequencing)))

(loopy-deftest seq-:by
  :result '(0 2 4 6 8 10)
  :multi-body t
  :body [((sequence i (list 0 1 2 3 4 5 6 7 8 9 10) :by 2)
          (collect i))
         ((sequence i [0 1 2 3 4 5 6 7 8 9 10] :by 2)
          (collect i))]
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-:by-just-once
  :doc "`:by' should only be evaluated once."
  :result '(1 3 5)
  :multi-body t
  :body [((with (times 0))
          (sequence i (vector 1 2 3 4 5 6) :by (progn
                                                 (cl-assert (= times 0))
                                                 (cl-incf times)
                                                 2))
          (collect i))

         ((with (times 0))
          (sequence i (list 1 2 3 4 5 6) :by (progn
                                               (cl-assert (= times 0))
                                               (cl-incf times)
                                               2))
          (collect i))]
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-end-just-once
  :doc "`:by' should only be evaluated once."
  :result '(0 1 2)
  :multi-body t
  :body [((with (times 0))
          (sequence i (vector 0 1 2 3 4 5 6) :to (progn
                                                   (cl-assert (= times 0))
                                                   (cl-incf times)
                                                   2))
          (collect i))

         ((with (times 0))
          (sequence i (vector 0 1 2 3 4 5 6) :upto (progn
                                                     (cl-assert (= times 0))
                                                     (cl-incf times)
                                                     2))
          (collect i))

         ((with (times 0))
          (sequence i (vector 2 1 0) :downto (progn
                                               (cl-assert (= times 0))
                                               (cl-incf times)
                                               0))
          (collect i))

         ((with (times 0))
          (sequence i (vector 0 1 2 3 4 5 6) :below (progn
                                                      (cl-assert (= times 0))
                                                      (cl-incf times)
                                                      3))
          (collect i))

         ((with (times 0))
          (sequence i (vector 2 1 0) :above (progn
                                              (cl-assert (= times 0))
                                              (cl-incf times)
                                              -1))
          (collect i))]
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-:index
  :result '((0 . 4) (1 . 3) (2 . 2) (3 . 1) (4 . 0))
  :multi-body t
  :body [((sequence i [4 3 2 1 0] :index cat)
          (collect (cons cat i)))
         ((sequence i (list 4 3 2 1 0) :index cat)
          (collect (cons cat i)))]
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-:from-:downto-:by
  :result '(8 6 4 2)
  :body ((sequence i [0 1 2 3 4 5 6 7 8 9 10]
                   :from 8 :downto 1 :by 2)
         (collect i))
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-:upto
  :result '(0 1 2 3 4 5 6 7)
  :body  ((sequence i [0 1 2 3 4 5 6 7 8 9 10] :upto 7)
          (collect i))
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-:to
  :result '(0 1 2 3 4 5 6 7)
  :body  ((sequence i [0 1 2 3 4 5 6 7 8 9 10] :to 7)
          (collect i))
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-:downto
  :result '(10 9 8 7 6 5 4 3)
  :body  ((sequence i [0 1 2 3 4 5 6 7 8 9 10] :downto 3)
          (collect i))
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-:above
  :result '(10 9 8)
  :body  ((sequence i [0 1 2 3 4 5 6 7 8 9 10] :above 7)
          (collect i))
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-:below
  :result '(0 1 2)
  :body  ((sequence i [0 1 2 3 4 5 6 7 8 9 10] :below 3)
          (collect i))
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-:downfrom
  :result '(5 4 3 2 1 0)
  :body ((sequence i [0 1 2 3 4 5] :downfrom 5)
         (collect i))
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-:upfrom
  :result '(2 3 4 5)
  :body ((sequence i [0 1 2 3 4 5] :upfrom 2)
         (collect i))
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-multi-seq
  :result '((1 3) (1 4) (2 3) (2 4))
  :body ((sequence i [1 2] '(3 4))
         (collect i))
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest seq-multi-seq-:by
  :result '((1 3)  (2 3))
  :body ((sequence i [1 2] '(3 4) :by 2)
         (collect i))
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest sequence-:test
  :result '(8 6 4 2)
  :multi-body t
  :body [((with (start 8)
                (end 2)
                (step -2))
          (sequence i [0 1 2 3 4 5 6 7 8 9 10]
                    :from start :to end :by step
                    :test #'>=)
          (collect i))
         ((with (start 8)
                (end 2)
                (step -2))
          (sequence i '(0 1 2 3 4 5 6 7 8 9 10)
                    :from start :to end :by step
                    :test #'>=)
          (collect i))]
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

(loopy-deftest sequence-:test-just-once
  :result '(2 4 6 8)
  :multi-body t
  :body [((with (times 0))
          (sequence i [0 1 2 3 4 5 6 7 8 9 10]
                    :from 2 :to 8 :by 2
                    :test (progn
                            (cl-assert (= times 0))
                            (cl-incf times)
                            #'<=))
          (collect i))

         ((with (times 0))
          (sequence i '(0 1 2 3 4 5 6 7 8 9 10)
                    :from 2 :to 8 :by 2
                    :test (progn
                            (cl-assert (= times 0))
                            (cl-incf times)
                            #'<=))
          (collect i))]
  :loopy t
  :iter-keyword (sequence collect)
  :iter-bare ((sequence . sequencing)
              (collect . collecting)))

;;;;; Seq Index
(loopy-deftest seq-index
  :result '(97 98 99)
  :multi-body t
  :body [((with (my-seq [97 98 99]))  (_cmd i my-seq) (collect (elt my-seq i)))
         ((with (my-seq "abc"))       (_cmd i my-seq) (collect (elt my-seq i)))
         ((with (my-seq '(97 98 99))) (_cmd i my-seq) (collect (elt my-seq i)))]
  :repeat _cmd
  :loopy ((_cmd . ( array-index arraying-index arrayi
                    list-index listing-index listi
                    string-index stringing-index stringi
                    sequence-index sequencing-index
                    sequencei seqi seqing-index))
          (collect . collect))
  :iter-keyword ((_cmd . ( array-index arraying-index arrayi
                           list-index listing-index listi
                           string-index stringing-index stringi
                           sequence-index sequencing-index
                           sequencei seqi seqing-index))
                 (collect . collect))
  :iter-bare ((_cmd . ( arraying-index listing-index stringing-index
                        sequencing-index seqing-index))
              (collect . collecting)))

(loopy-deftest seq-index-:by
  :result '(0 2 4 6 8 10)
  :body ((with (my-seq [0 1 2 3 4 5 6 7 8 9 10]))
         (seq-index i my-seq :by 2)
         (collect (elt my-seq i)))
  :loopy t
  :iter-keyword (seq-index collect)
  :iter-bare ((seq-index . sequencing-index)
              (collect . collecting)))

(loopy-deftest seq-index-:by-only-once
  :result '(0 2 4 6 8 10)
  :body ((with (my-seq [0 1 2 3 4 5 6 7 8 9 10])
               (times 0))
         (seq-index i my-seq :by (progn
                                   (cl-assert (= times 0))
                                   (cl-incf times)
                                   2))
         (collect (elt my-seq i)))
  :loopy t
  :iter-keyword (seq-index collect)
  :iter-bare ((seq-index . sequencing-index)
              (collect . collecting)))

(loopy-deftest seq-index-end-just-once-up
  :result '(0 1 2)
  :multi-body t
  :body [((with (times 0))
          (sequence-index i (vector 0 1 2 3 4 5 6) :to (progn
                                                         (cl-assert (= times 0))
                                                         (cl-incf times)
                                                         2))
          (collect i))

         ((with (times 0))
          (sequence-index i (vector 0 1 2 3 4 5 6) :upto (progn
                                                           (cl-assert (= times 0))
                                                           (cl-incf times)
                                                           2))
          (collect i))

         ((with (times 0))
          (sequence-index i (vector 0 1 2 3 4 5 6) :below (progn
                                                            (cl-assert (= times 0))
                                                            (cl-incf times)
                                                            3))
          (collect i))]
  :loopy t
  :iter-keyword (sequence-index collect)
  :iter-bare ((sequence-index . sequencing)
              (collect . collecting)))

(loopy-deftest seq-index-end-just-once-down
  :result '(2 1 0)
  :multi-body t
  :body [((with (times 0))
          (sequence-index i (vector 0 1 2) :downto (progn
                                                     (cl-assert (= times 0))
                                                     (cl-incf times)
                                                     0))
          (collect i))

         ((with (times 0))
          (sequence-index i (vector 0 1 2) :above (progn
                                                    (cl-assert (= times 0))
                                                    (cl-incf times)
                                                    -1))
          (collect i))]
  :loopy t
  :iter-keyword (sequence-index collect)
  :iter-bare ((sequence-index . sequencing)
              (collect . collecting)))

(loopy-deftest seq-index-:from-:downto-:by
  :result '(8 6 4 2)
  :body ((with (my-seq [0 1 2 3 4 5 6 7 8 9 10]))
         (seq-index i my-seq
                    :from 8 :downto 1 :by 2)
         (collect (elt my-seq i)))
  :loopy t
  :iter-keyword (seq-index collect)
  :iter-bare ((seq-index . sequencing-index)
              (collect . collecting)))

(loopy-deftest seq-index-:upto
  :result '(0 1 2 3 4 5 6 7)
  :body ((with (my-seq [0 1 2 3 4 5 6 7 8 9 10]))
         (seq-index i my-seq :upto 7)
         (collect (elt my-seq i)))
  :loopy t
  :iter-keyword (seq-index collect)
  :iter-bare ((seq-index . sequencing-index)
              (collect . collecting)))

(loopy-deftest seq-index-:to
  :result '(0 1 2 3 4 5 6 7)
  :body ((with (my-seq [0 1 2 3 4 5 6 7 8 9 10]))
         (seq-index i my-seq :to 7)
         (collect (elt my-seq i)))
  :loopy t
  :iter-keyword (seq-index collect)
  :iter-bare ((seq-index . sequencing-index)
              (collect . collecting)))

(loopy-deftest seq-index-:downto
  :result '(10 9 8 7 6 5 4 3)
  :body ((with (my-seq [0 1 2 3 4 5 6 7 8 9 10]))
         (seq-index i my-seq :downto 3)
         (collect (elt my-seq i)))
  :loopy t
  :iter-keyword (seq-index collect)
  :iter-bare ((seq-index . sequencing-index)
              (collect . collecting)))

(loopy-deftest seq-index-:above
  :result '(10 9 8)
  :body ((with (my-seq [0 1 2 3 4 5 6 7 8 9 10]))
         (seq-index i my-seq :above 7)
         (collect (elt my-seq i)))
  :loopy t
  :iter-keyword (seq-index collect)
  :iter-bare ((seq-index . sequencing-index)
              (collect . collecting)))

(loopy-deftest seq-index-:below
  :result '(0 1 2)
  :body ((with (my-seq [0 1 2 3 4 5 6 7 8 9 10]))
         (seq-index i my-seq :below 3)
         (collect (elt my-seq i)))
  :loopy t
  :iter-keyword (seq-index collect)
  :iter-bare ((seq-index . sequencing-index)
              (collect . collecting)))

(loopy-deftest seq-index-:downfrom
  :result '(5 4 3 2 1 0)
  :body ((sequence-index i [0 1 2 3 4 5 6 7 8 9] :downfrom 5)
         (collect i))
  :loopy t
  :iter-keyword (sequence-index collect)
  :iter-bare ((sequence-index . sequencing-index)
              (collect . collecting)))

(loopy-deftest seq-index-step-var
  :doc "If `:by' is a numeric literal, `seq-index' can use it directly."
  :result '(2 4 6 8)
  :multi-body t
  :body [((with (start 2) (end 8)
                (arr (cl-coerce (number-sequence 0 10) 'vector)))
          (seq-index i arr :from start :to end :by 2)
          (collect i))
         ((with (start 2) (end 8) (step 2)
                (arr (cl-coerce (number-sequence 0 10) 'vector)))
          (seq-index i arr :from start :to end :by step)
          (collect i))]
  :loopy t
  :iter-keyword (seq-index collect)
  :iter-bare ((seq-index . sequencing-index)
              (collect . collecting)))

(loopy-deftest seq-index-init-with
  :doc "`seq-index' can default to the starting index if not with-bound."
  :result '(27 0 0 1 1 2 2 3)
  :body ((with (i 27))
         (collect i)
         (seq-index i [1 2 3 4])
         (collect i))
  :loopy t
  :iter-keyword (seq-index collect)
  :iter-bare ((seq-index . sequencing-index)
              (collect . collecting)))

(loopy-deftest seq-index-init-no-with
  :doc "`seq-index' can default to the starting index if not with-bound."
  :result '(0 0 1 1 2 2 3 3)
  :body ((collect i)
         (seq-index i [1 2 3 4])
         (collect i))
  :loopy t
  :iter-keyword (seq-index collect)
  :iter-bare ((seq-index . sequencing-index)
              (collect . collecting)))

;;;;; Seq Ref
(loopy-deftest seq-ref
  :result '(7 7 7 7)
  :body ((with (my-seq '(1 2 3 4)))
         (_cmd i my-seq)
         (do (setf i 7))
         (finally-return my-seq))
  :repeat _cmd
  :loopy ((_cmd . (sequence-ref sequencef seq-ref seqf)))
  :iter-keyword ((_cmd . (sequence-ref sequencef seq-ref seqf))
                 (do . do))
  :iter-bare ((_cmd . (sequencing-ref))
              (do . ignore)))

(loopy-deftest seq-ref-:by-array
  :result "a1a3a5a7a9"
  :body ((with (my-str "0123456789"))
         (seq-ref i my-str :by 2)
         (do (setf i ?a))
         (finally-return my-str))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:by-list
  :result '(99 1 99 3 99 5 99 7 99 9 99)
  :body ((with (my-list (list 0 1 2 3 4 5 6 7 8 9 10) ))
         (seq-ref i my-list :by 2)
         (do (setf i 99))
         (finally-return my-list))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:by-just-once
  :result "a1a3a5a7a9"
  :body ((with (my-str "0123456789")
               (times 0))
         (seq-ref i my-str :by (progn
                                 (cl-assert (= times 0))
                                 (cl-incf times)
                                 2))
         (do (setf i ?a))
         (finally-return my-str))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:by-:index
  :result  "a1a3a5a7a9"
  :body ((with (my-str "0123456789"))
         (seq-ref i my-str :by 2 :index cat)
         (do (setf (aref my-str cat) ?a))
         (finally-return my-str))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:from-:by
  :result  '(0 cat 2 cat 4 cat 6 cat 8 cat)
  :body ((with (my-list (list 0 1 2 3 4 5 6 7 8 9)))
         (seq-ref i my-list :from 1 :by 2 )
         (do (setf i 'cat))
         (finally-return my-list))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:downto-:by
  :result  "0123456a8a"
  :body ((with (my-str "0123456789"))
         (seq-ref i my-str :downto 6 :by 2 )
         (do (setf i ?a))
         (finally-return my-str))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:below
  :result  "aaaaa56789"
  :body ((with (my-str "0123456789"))
         (seq-ref i my-str :below 5)
         (do (setf i ?a))
         (finally-return my-str))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:above
  :result  "012345aaaa"
  :body ((with (my-str "0123456789"))
         (seq-ref i my-str :above 5)
         (do (setf i ?a))
         (finally-return my-str))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:above-list
  :result  '(0 1 2 3 4 5 cat cat cat cat)
  :body ((with (my-list (list 0 1 2 3 4 5 6 7 8 9)))
         (seq-ref i my-list :above 5)
         (do (setf i 'cat))
         (finally-return my-list))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:upto
  :result  "aaaaaa6789"
  :body ((with (my-str "0123456789"))
         (seq-ref i my-str :upto 5)
         (do (setf i ?a))
         (finally-return my-str))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:upfrom-:by
  :result  "0a2a4a6a8a"
  :body ((with (my-str "0123456789"))
         (seq-ref i my-str :upfrom 1 :by 2 )
         (do (setf i ?a))
         (finally-return my-str))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:upfrom-:by-string
  :result  '(0 cat 2 cat 4 cat 6 cat 8 cat)
  :body ((with (my-list (list 0 1 2 3 4 5 6 7 8 9)))
         (seq-ref i my-list :upfrom 1 :by 2)
         (do (setf i 'cat))
         (finally-return my-list))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest sequence-ref-:downfrom
  :result '(0 cat 2 cat 4 cat 6 7 8 9)
  :body ((with (my-list (list 0 1 2 3 4 5 6 7 8 9)))
         (sequence-ref i my-list :downfrom 5 :by 2)
         (do (setf i 'cat))
         (finally-return my-list))
  :loopy t
  :iter-keyword (sequence-ref do)
  :iter-bare ((sequence-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:by-literal
  :doc "`seq-ref' can use literal `:by' directly."
  :result [0 1 22 3 22 5 22 7 22 9 10]
  :body ((with (start 2) (end 8)
               (arr (cl-coerce (number-sequence 0 10) 'vector)))
         (seq-ref i arr :from start :to end :by 2)
         (do (setf i 22))
         (finally-return arr))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-:by-variable
  :doc "`seq-ref' can use literal `:by' directly."
  :result [0 1 22 3 22 5 22 7 22 9 10]
  :body ((with (start 2) (end 8) (step 2)
               (arr (cl-coerce (number-sequence 0 10) 'vector)))
         (seq-ref i arr :from start :to end :by step)
         (do (setf i 22))
         (finally-return arr))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

(loopy-deftest seq-ref-destructuring
  :doc "Check that `seq-ref' implements destructuring. Not destructuring itself."
  :result [(7 8 9) (7 8 9)]
  :body ((with (my-seq [(1 2 3) (4 5 6)]))
         (seq-ref (i j k) my-seq)
         (do (setf i 7)
             (setf j 8)
             (setf k 9))
         (finally-return my-seq))
  :loopy t
  :iter-keyword (seq-ref do)
  :iter-bare ((seq-ref . sequencing-ref)
              (do . ignore)))

;;;;; Stream
(loopy-deftest stream-names
  :result '(0 1 2)
  :body ((_cmd i (stream [0 1 2]))
         (collect i))
  :repeat _cmd
  :loopy ((_cmd . (stream streaming))
          (collect . collect))
  :iter-keyword ((_cmd . (stream streaming))
                 (collect . collect))
  :iter-bare ((_cmd . (streaming))
              (collect . collecting)))

(loopy-deftest stream-destr
  :result '((0 1 2)
            (3 4 5))
  :body ((stream (i j k) (loopy-test-escape (stream [(0 1 2) (3 4 5)])))
         (collect (list i j k)))
  :loopy t
  :iter-keyword (stream collect)
  :iter-bare ((stream . streaming)
              (collect . collecting)))

(loopy-deftest stream-:by-const
  :result '(0 2 4 6)
  :body ((stream i (loopy-test-escape (stream [0 1 2 3 4 5 6])) :by 2)
         (collect i))
  :loopy t
  :iter-keyword (stream collect)
  :iter-bare ((stream . streaming)
              (collect . collecting)))

(loopy-deftest stream-:by-only-once
  :doc "Keywords like `length' should only be evaluated once."
  :result '(0 2 4 6)
  :body ((with (times 0))
         (stream i (loopy-test-escape (stream [0 1 2 3 4 5 6]))
                 :by (progn
                       (cl-assert (= times 0))
                       (cl-incf times)
                       2))
         (collect i))
  :loopy t
  :iter-keyword (stream collect set)
  :iter-bare ((set . setting)
              (stream . streaming)
              (collect . collecting)))

;;;;; Substream

(loopy-deftest substream-no-&seq-error
  :doc "Although implemented as lists, substream can only be destructured by `&seq'."
  :error loopy-substream-not-&seq
  :body ((substream (i) (loopy-test-escape (stream (vector 0 1 2))))
         (collect i))
  :loopy t
  :iter-keyword (substream collect)
  :iter-bare ((substream . substreaming)
              (collect . collecting)))

(loopy-deftest substream-no-&seq-no-error
  :doc "We shouldn't signal an error if we're not using the default system."
  :result '(0 1 2)
  :wrap ((x . `(progn (require 'loopy-seq) ,x)))
  :body ((flag seq)
         (substream (i) (loopy-test-escape (stream (vector 0 1 2))))
         (collect i))
  :loopy t
  :iter-keyword (substream collect)
  :iter-bare ((substream . substreaming)
              (collect . collecting)))

(loopy-deftest substream-names
  :result '(0 1 2)
  :body ((_cmd i (loopy-test-escape (stream [0 1 2])))
         (collect (stream-first i)))
  :repeat _cmd
  :loopy ((_cmd . (substream substreaming))
          (collect . collect))
  :iter-keyword ((_cmd . (substream substreaming))
                 (collect . collect))
  :iter-bare ((_cmd . (substreaming))
              (collect . collecting)))

(loopy-deftest substream-destr
  :result '((0 1 2)
            (1 2 3)
            (2 3 4))
  :body ((cycle 3)
         (substream (&seq i j k) (loopy-test-escape (stream [0 1 2 3 4 5 6])))
         (collect (list i j k)))
  :loopy t
  :iter-keyword (substream collect cycle)
  :iter-bare ((substream . substreaming)
              (collect . collecting)
              (cycle . cycling)))

(loopy-deftest substream-destr-too-short
  :error loopy-bad-run-time-destructuring
  :body ((substream (&seq i j k) (loopy-test-escape (stream [0 1 2])))
         (collect (list i j k)))
  :loopy t
  :iter-keyword (substream collect )
  :iter-bare ((substream . substreaming)
              (collect . collecting)))

(loopy-deftest substream-:length-const
  :result '((0 1 2) (1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6) (6))
  :body ((substream i (loopy-test-escape (stream [0 1 2 3 4 5 6])) :length 3)
         (set res nil)
         (do (seq-do (lambda (x) (push x res))
                     i))
         (collect (reverse res)))
  :loopy t
  :iter-keyword (substream collect set do)
  :iter-bare ((set . setting)
              (substream . substreaming)
              (collect . collecting)
              (do      . progn)))

(loopy-deftest substream-:length-only-once
  :doc "Keywords like `length' should only be evaluated once."
  :result '((0 1 2) (1 2 3) (2 3 4) (3 4 5) (4 5 6) (5 6) (6))
  :body ((with (times 0))
         (substream i (loopy-test-escape (stream [0 1 2 3 4 5 6])) :length (progn
                                                                             (cl-assert (= times 0))
                                                                             (cl-incf times)
                                                                             3))
         (set res nil)
         (do (seq-do (lambda (x) (push x res))
                     i))
         (collect (reverse res)))
  :loopy t
  :iter-keyword (substream collect set do)
  :iter-bare ((set . setting)
              (substream . substreaming)
              (collect . collecting)
              (do      . progn)))

(loopy-deftest substream-:by-const
  :result '((0 1 2 3 4 5 6) (2 3 4 5 6) (4 5 6) (6))
  :body ((substream i (loopy-test-escape (stream [0 1 2 3 4 5 6])) :by 2)
         (set res nil)
         (do (seq-do (lambda (x) (push x res))
                     i))
         (collect (reverse res)))
  :loopy t
  :iter-keyword (substream collect set do)
  :iter-bare ((set . setting)
              (substream . substreaming)
              (collect . collecting)
              (do      . progn)))

(loopy-deftest substream-:by-only-once
  :doc "Keywords like `length' should only be evaluated once."
  :result '((0 1 2 3 4 5 6) (2 3 4 5 6) (4 5 6) (6))
  :body ((with (times 0))
         (substream i (loopy-test-escape (stream [0 1 2 3 4 5 6])) :by (progn
                                                                         (cl-assert (= times 0))
                                                                         (cl-incf times)
                                                                         2))
         (set res nil)
         (do (seq-do (lambda (x) (push x res))
                     i))
         (collect (reverse res)))
  :loopy t
  :iter-keyword (substream collect set do)
  :iter-bare ((set . setting)
              (substream . substreaming)
              (collect . collecting)
              (do      . progn)))

;;;; Accumulation Commands
;;;;; Final updates

(loopy-deftest accumulation-conflicting-final-updates
  :doc "Check that commands of the same category but different updates error.

Previously, this was mostly concerned with using a different
`:result-type' but in the same command type category.

Wrapping with another eval to make sure variables are set by
expansion time."
  :error loopy-incompatible-accumulation-final-updates
  :wrap (
         (x . `(cl-labels ((my-loopy-sum-command1 ((&whole cmd _
                                                           var-or-val
                                                           &optional
                                                           maybe-val))
                             "Set TARGET to the sum of ITEMS."
                             (let ((var)
                                   (val))
                               (if maybe-val
                                   (setq var var-or-val
                                         val maybe-val)
                                 (setq var 'loopy-result
                                       val var-or-val))
                               (loopy--check-accumulation-compatibility
                                loopy--loop-name
                                var 'number cmd)
                               `((loopy--accumulation-vars (,var nil))
                                 (loopy--main-body (setq ,var (+ ,var ,val)))
                                 (loopy--vars-final-updates
                                  (,var . (setq ,var (1- ,var)))))))
                           (my-loopy-sum-command2 ((&whole cmd _
                                                           var-or-val
                                                           &optional
                                                           maybe-val))
                             "Set TARGET to the sum of ITEMS."
                             (let ((var)
                                   (val))
                               (if maybe-val
                                   (setq var var-or-val
                                         val maybe-val)
                                 (setq var 'loopy-result
                                       val var-or-val))
                               (loopy--check-accumulation-compatibility
                                loopy--loop-name
                                var 'number cmd)
                               `((loopy--accumulation-vars (,var nil))
                                 (loopy--main-body (setq ,var (+ ,var ,val)))
                                 (loopy--vars-final-updates
                                  (,var . (setq ,var (- ,var 100))))))))
                 (let ((loopy-command-parsers
                        (thread-first loopy-command-parsers
                                      (map-insert 'sum1
                                                  #'my-loopy-sum-command1)
                                      (map-insert 'sum2
                                                  #'my-loopy-sum-command2)))
                       (loopy-iter-bare-commands (append '(sum1 sum2)
                                                         loopy-iter-bare-commands)))
                   (eval (quote ,x) t)))))
  :multi-body t
  :body [((list i '(1 2 3 4 5))
          (sum1 my-target i)
          (sum2 my-target i)
          (finally-return my-target))

         ((list i '(1 2 3 4 5))
          (sum1 i)
          (sum2 i))]
  :loopy t
  :iter-keyword (sum1 sum2)
  :iter-bare t)

;;;;; Into Argument
(loopy-deftest accumulation-into-argument
  :doc "Check `:into' works and variable is treated as explicit."
  :result '((2 3 4) (2 2 2))
  :body ((list i '(1 2 3))
         (collect (1+ i) :into coll1)
         ;; This should be the same value repeated.
         ;; If not, it means `coll1'  is constructed
         ;; in reverse, instead of being treated as
         ;; explicitly named.
         (collect coll2 (cl-first coll1))
         (finally-return coll1 coll2))
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest accumulation-raise-error-bad-arg
  :error loopy-wrong-number-of-command-arguments-or-bad-keywords
  :body ((list i '(1 2 3))
         (collect i :casdfasdf x))
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

;;;;; Command Compatibility
(loopy-deftest accumulation-compatibility-lists
  :doc "Check that commands with different accumulation types should raise error."
  :should t
  :macroexpand t
  :body ((list i '((1 2 3) (4 5 6)))
         (collect i)
         (append i)
         (adjoin i)
         (union i)
         (nunion (copy-sequence i))
         (nconc (copy-sequence i))
         (finally-return t))
  :loopy t
  :iter-keyword (list collect append adjoin union nunion nconc)
  :iter-bare ((list . listing)
              (collect . collecting)
              (append . appending)
              (adjoin . adjoining)
              (union . unioning)
              (nunion . nunioning)
              (nconc . nconcing)))

;; TODO: Enable these tests in a future version.
;;
;; (loopy-deftest accumulation-compatibility-different-inits-1
;;   :doc "Check that accumulation commands with different initial values raise an error.
;; This should apply even when they're compatible types."
;;   :error loopy-incompatible-accumulation-initializations
;;   :macroexpand t
;;   :body ((list i '(1 2 3 4 5))
;;          (sum i)
;;          (multiply i))
;;   :loopy t
;;   :iter-keyword (list sum multiply)
;;   :iter-bare ((list . listing)
;;               (sum . summing)
;;               (multiply . multiplying)))
;;
;; (loopy-deftest accumulation-compatibility-different-inits-2
;;   :doc "Check that `with' on the variable (see test 1) avoids the error."
;;   :result 27
;;   :body ((with (loopy-result 0))
;;          (list i '(1 2 3))
;;          (sum i)
;;          (multiply i))
;;   :loopy t
;;   :iter-keyword (list sum multiply)
;;   :iter-bare ((list . listing)
;;               (sum . summing)
;;               (multiply . multiplying)))

(loopy-deftest accumulation-compatibility-different-types
  :doc "Check that commands with different accumulation types should raise error."
  :error loopy-incompatible-accumulation-types
  :macroexpand t
  :multi-body t
  :body [((list i '((1 2) (3 4) (5 6)))
          (collect i)
          (concat i))

         ((list i '((1 2) (3 4) (5 6)))
          (append i)
          (concat i))

         ((list i '((1 2) (3 4) (5 6)))
          (adjoin i)
          (concat i))

         ((list i '((1 2) (3 4) (5 6)))
          (union i)
          (concat i))

         ((list i '((1 2) (3 4) (5 6)))
          (nunion i)
          (concat i))

         ((list i '((1 2) (3 4) (5 6)))
          (nconc i)
          (concat i))

         ((list i '((1 2) (3 4) (5 6)))
          (vconcat i)
          (concat i))]
  :loopy t
  :iter-keyword (collect append concat adjoin union nunion nconc vconcat)
  :iter-bare ((collect . collecting)
              (append . appending)
              (concat . concating)
              (adjoin . adjoining)
              (union . unioning)
              (nunion . nunioning)
              (nconc . nconcing)
              (vconcat . vconcating)))

(loopy-deftest accumulation-compatibility-same-types
  :doc "Also check that we don't throw errors for commands of the same type."
  :should t
  :macroexpand t
  :multi-body t
  :body [((list i '((1 2) (3 4) (5 6)))
          (vconcat i)
          (vconcat i))

         ((list i '((1 2) (3 4) (5 6)))
          (collect i)
          (collect i))

         ((list i '((1 2) (3 4) (5 6)))
          (concat i)
          (concat i))]
  :loopy t
  :iter-keyword (vconcat collect concat)
  :iter-bare ((vconcat . vconcating)
              (collect . collecting)
              (concat . concating)))

;;;;; Name of implicit accumulations

(loopy-deftest implicit-accumulation-name
  :doc "Make sure that commands use `loopy-result' and that it's accessible in SMAs."
  :result '(1 2 3)
  :multi-body t
  :body [((list i '(1 2 3))
          (collect i)
          (after-do (cl-return loopy-result)))
         ((list i '(1 2 3))
          (collect i)
          (finally-return loopy-result))]
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

;;;;; Accumulate
(loopy-deftest accumulate
  :result '(2 1)
  :body ((list i '(1 2))
         (_cmd my-accum i #'cons)
         (finally-return my-accum))
  :repeat _cmd
  :loopy ((_cmd . (accumulate accumulating callf2)))
  :iter-keyword ((list . list)
                 (_cmd . (accumulate accumulating callf2)))
  :iter-bare ((list . listing)
              (_cmd . (accumulating))))

(loopy-deftest accumulate-with
  :doc "Test to make sure that we can replace `:inti' with `with'."
  :result 10
  :body ((with (my-accum 1))
         (list i '(2 3 4))
         (accumulate my-accum i #'+)
         (finally-return my-accum))
  :loopy t
  :iter-keyword (accumulate list)
  :iter-bare ((list . listing)
              (accumulate . accumulating)))

(loopy-deftest accumulate-destr
  :doc "Test that `accumulate' implements destructuring, not destructuring itself."
  :result  '((3 1) (4 2))
  :body ((with (f #'cons))
         (list i '((1 2) (3 4)))
         (accumulate (accum1 accum2) i f)
         (finally-return accum1 accum2))
  :loopy t
  :iter-keyword (accumulate list)
  :iter-bare ((list . listing)
              (accumulate . accumulating)))

;;;;; Adjoin
(loopy-deftest adjoin
  :result '((1 . 1) (1 . 2) (2 . 3))
  :multi-body t
  :body [((list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (_cmd a i)
          (finally-return a))
         ((list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (_cmd i))]
  :repeat _cmd
  :loopy ((_cmd . (adjoin adjoining)))
  :iter-keyword ((list . listing)
                 (_cmd . (adjoin adjoining)))
  :iter-bare ((list . listing)
              (_cmd . (adjoining))))

(loopy-deftest adjoin-:key
  :doc "If `key' is a quoted function, it should expand to a direct application."
  :result '((1 . 1) (2 . 3))
  :multi-body t
  :body [((with (my-key #'car))
          (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (adjoin a i :key my-key)
          (finally-return a))
         ((list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (adjoin a i :key #'car)
          (finally-return a))
         ;; Implicit
         ((with (my-key #'car))
          (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (adjoin i :key my-key))
         ((list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (adjoin i :key #'car))]
  :loopy t
  :iter-keyword (list adjoin)
  :iter-bare ((list . listing)
              (adjoin . adjoining)))

(loopy-deftest adjoin-:test-default
  :doc "Default value of `test' is `equal'."
  :result t
  :multi-body t
  :body [((list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (adjoin a1 i :test #'equal)
          (adjoin a2 i)
          (finally-return (equal a1 a2)))
         ((list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (adjoin i)
          (finally-return (equal loopy-result '((1 . 1) (1 . 2) (2 . 3)))))]
  :loopy t
  :iter-keyword (list adjoin)
  :iter-bare ((list . listing)
              (adjoin . adjoining)))

(loopy-deftest adjoin-:test
  :doc "If `key' is a quoted function, it should expand to a direct application."
  :result '((1 . 1) (1 . 2) (1 . 2) (2 . 3))
  :multi-body t
  :body [((list i '((1 . 1) (1 . 2)
                    (1 . 2) (2 . 3)))
          (adjoin a i :test #'eql)
          (finally-return a))
         ((with (test #'eql))
          (list i '((1 . 1) (1 . 2)
                    (1 . 2) (2 . 3)))
          (adjoin a i :test test)
          (finally-return a))
         ;; Implicit
         ((list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (adjoin i :test #'eql))
         ((with (test #'eql))
          (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (adjoin i :test test))]
  :loopy t
  :iter-keyword (list adjoin)
  :iter-bare ((list . listing)
              (adjoin . adjoining)))

(loopy-deftest adjoin-:test-:key
  :doc "`test' and `key' applied directly when quoted functions."
  :result '((1 . 1) (2 . 3))
  :multi-body t
  :body [((list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (adjoin a i :test #'= :key #'car)
          (finally-return a))
         ((with (test #'=) (key #'car))
          (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (adjoin a i :test test :key key)
          (finally-return a))
         ;; Implicit
         ((list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (adjoin i :test #'= :key #'car))
         ((with (test #'=) (key #'car))
          (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
          (adjoin i :test test :key key))]
  :loopy t
  :iter-keyword (list adjoin)
  :iter-bare ((list . listing)
              (adjoin . adjoining)))

(loopy-deftest adjoin-destructuring
  :doc "Test that `adjoin' implements destructuring, not destructuring itself."
  :result '(((1 . 1) (1 . 2)) ((1 . 2) (2 . 3)))
  :body ((list i '(((1 . 1) (1 . 2)) ((1 . 2) (2 . 3))))
         (adjoin (a1 a2) i)
         (finally-return a1 a2))
  :loopy t
  :iter-keyword (list adjoin)
  :iter-bare ((list . listing)
              (adjoin . adjoining)))

(loopy-deftest adjoin-:at-end
  :result '(1 2 3 4 5)
  :multi-body t
  :body  [((list i '(1 2 3 4 4 5))
           (adjoin i :at end))
          ((list i '(1 2 3 4 4 5))
           (adjoin i :at 'end))]
  :loopy t
  :iter-keyword (list adjoin)
  :iter-bare ((list . listing)
              (adjoin . adjoining)))

(loopy-deftest adjoin-:at-start/beginning
  :result '(5 4 3 2 1)
  :multi-body t
  :body  [((list i '(1 2 3 4 4 5))
           (adjoin i :at start))
          ((list i '(1 2 3 4 4 5))
           (adjoin i :at 'start))
          ((list i '(1 2 3 4 4 5))
           (adjoin i :at beginning))
          ((list i '(1 2 3 4 4 5))
           (adjoin i :at 'beginning))]
  :loopy t
  :iter-keyword (list adjoin)
  :iter-bare ((list . listing)
              (adjoin . adjoining)))

(loopy-deftest adjoin-end-tracking-start-and-end
  :doc "Make sure multiple uses doesn't overwrite any data.
Using `start' and `end' in either order should give the same result."
  :result '(3 2 1 11 12 13)
  :multi-body t
  :body [((list i '(1 2 3)) (adjoin i :at start)      (adjoin (+ i 10) :at end))
         ((list i '(1 2 3)) (adjoin (+ i 10) :at end) (adjoin i :at start))]
  :loopy t
  :iter-keyword (list adjoin)
  :iter-bare ((list . listing)
              (adjoin . adjoining)))

(loopy-deftest adjoin-end-tracking-end-twice
  :doc "Make sure multiple uses doesn't overwrite any data."
  :result '(1 2 3 4 5)
  :body ((list i '(1 2 2 3 3 4))
         (adjoin i :at end)
         (adjoin (1+ i) :at end))
  :loopy t
  :iter-keyword (list adjoin)
  :iter-bare ((list . listing)
              (adjoin . adjoining)))

(loopy-deftest adjoin-not-destructive
  :doc "Check that `adjoin' doesn't modify item being adjoined."
  :result t
  :multi-body t
  ;; Can't use function `list', since that would naively be replaced by
  ;; `listing' command.
  :body [((with (l1 (cl-loop for i from 1 to 9 by 2
                             collect i
                             collect (1+ i)
                             collect (1+ i))))
          (list i l1) (adjoin coll i :at start)
          (finally-return
           (equal l1 (cl-loop for i from 1 to 9 by 2
                              collect i
                              collect (1+ i)
                              collect (1+ i)))))

         ((with (l1 (cl-loop for i from 1 to 9 by 2
                             collect i
                             collect (1+ i)
                             collect (1+ i))))
          (list i l1) (adjoin coll i :at end)
          (finally-return
           (equal l1 (cl-loop for i from 1 to 9 by 2
                              collect i
                              collect (1+ i)
                              collect (1+ i)))))

         ((with (l1 (cl-loop for i from 1 to 9 by 2
                             collect i
                             collect (1+ i)
                             collect (1+ i))))
          (list i l1) (adjoin i :at start)
          (finally-return
           (equal l1 (cl-loop for i from 1 to 9 by 2
                              collect i
                              collect (1+ i)
                              collect (1+ i)))))

         ((with (l1 (cl-loop for i from 1 to 9 by 2
                             collect i
                             collect (1+ i)
                             collect (1+ i))))
          (list i l1) (adjoin i :at end)
          (finally-return
           (equal l1 (cl-loop for i from 1 to 9 by 2
                              collect i
                              collect (1+ i)
                              collect (1+ i)))))]
  :loopy t
  :iter-keyword (list adjoin)
  :iter-bare ((list . listing)
              (adjoin . adjoining)))

;;;;; Append
(loopy-deftest append
  :result '(1 2 3 4 5 6)
  :multi-body t
  :body [((list i '((1 2 3) (4 5 6)))
          (_cmd coll i)
          (finally-return coll))
         ((list i '((1 2 3) (4 5 6)))
          (_cmd i))]
  :repeat _cmd
  :loopy ((_cmd . (append appending)))
  :iter-keyword ((_cmd . (append appending))
                 (list . listing))
  :iter-bare ((list . listing)
              (_cmd . (appending))))

(loopy-deftest append-destructuring
  :doc "Check that `append' implements destructuring, not destructing itself."
  :result '((1 2 5 6) (3 4 7 8))
  :body ((array i [((1 2) (3 4)) ((5 6) (7 8))])
         (append (j k) i)
         (finally-return j k))
  :loopy t
  :iter-keyword (array append)
  :iter-bare ((array . arraying)
              (append . appending)))

(loopy-deftest append-:at-end
  :result '(1 2 3 4 5 6)
  :multi-body t
  :body [((list i '((1 2 3) (4 5 6)))
          (append i :at end))
         ((list i '((1 2 3) (4 5 6)))
          (append coll i :at end)
          (finally-return coll))]
  :loopy t
  :iter-keyword (list append)
  :iter-bare ((list . listing)
              (append . appending)))

(loopy-deftest append-:at-start/beginning
  :result '(4 5 6 1 2 3)
  :multi-body t
  :body [((list i '((1 2 3) (4 5 6)))
          (append i :at start))
         ((list i '((1 2 3) (4 5 6)))
          (append coll i :at start)
          (finally-return coll))
         ((list i '((1 2 3) (4 5 6)))
          (append i :at beginning))
         ((list i '((1 2 3) (4 5 6)))
          (append coll i :at beginning)
          (finally-return coll))]
  :loopy t
  :iter-keyword (list append)
  :iter-bare ((list . listing)
              (append . appending)))

(loopy-deftest append-end-tracking-accum-opt-end
  :result '(6 7 3 4 1 2)
  :body ((accum-opt (coll end))
         (list i '((1 2) (3 4) (6 7)))
         (append coll i :at start)
         (finally-return coll))
  :loopy t
  :iter-keyword (list append)
  :iter-bare ((list . listing)
              (append . appending)))

(loopy-deftest append-end-tracking-accum-opt-start
  :result '(1 2 3 4 6 7)
  :body ((accum-opt (coll start))
         (list i '((1 2) (3 4) (6 7)))
         (append coll i :at end)
         (finally-return coll))
  :loopy t
  :iter-keyword (list append)
  :iter-bare ((list . listing)
              (append . appending)))

(loopy-deftest append-end-tracking-start-end
  :result '(5 6 3 4 1 2 11 12 13 14 15 16)
  :multi-body t
  :body [((list i '((1 2) (3 4) (5 6)))
          (append coll i :at start)
          (append coll (mapcar (lambda (x) (+ x 10)) i) :at end)
          (finally-return coll))
         ((list i '((1 2) (3 4) (5 6)))
          (append coll (mapcar (lambda (x) (+ x 10)) i) :at end)
          (append coll i :at start)
          (finally-return coll))

         ((list i '((1 2) (3 4) (5 6)))
          (append i :at start)
          (append (mapcar (lambda (x) (+ x 10)) i) :at end))
         ((list i '((1 2) (3 4) (5 6)))
          (append (mapcar (lambda (x) (+ x 10)) i) :at end)
          (append i :at start))]
  :loopy t
  :iter-keyword (list append)
  :iter-bare ((list . listing)
              (append . appending)))

(loopy-deftest append-end-tracking-end-twice
  :result '(1 2 8 9 3 4 10 11 6 7 13 14)
  :multi-body t
  :body [((list i '((1 2) (3 4) (6 7)))
          (append coll i :at end)
          (append coll (mapcar (lambda (x) (+ x 7)) i) :at end)
          (finally-return coll))
         ((list i '((1 2) (3 4) (6 7)))
          (append i :at end)
          (append (mapcar (lambda (x) (+ x 7)) i) :at end))]
  :loopy t
  :iter-keyword (list append)
  :iter-bare ((list . listing)
              (append . appending)))

(loopy-deftest append-not-destructive
  :doc "Check that `append' doesn't modify the list being appended."
  :result t
  :multi-body t
  :body [((with (l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
          (listing i l1)
          (append coll i :at start)
          (finally-return (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

         ((with (l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
          (listing i l1)
          (append coll i :at end)
          (finally-return (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

         ((with (l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
          (listing i l1)
          (append i :at start)
          (finally-return (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

         ((with (l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
          (listing i l1)
          (append i :at end)
          (finally-return (equal l1 '((1 2) (3 4) (5 6) (7 8)))))]
  :loopy t
  :iter-keyword (listing append)
  :iter-bare ((append . appending)))

;;;;; Collect
(loopy-deftest collect
  :result '(1 2 3)
  :multi-body t
  :body [((list j '(1 2 3))
          (_cmd coll j)
          (finally-return coll))
         ((list j '(1 2 3))
          (_cmd j))]
  :repeat _cmd
  :loopy ((_cmd . (collect collecting)))
  :iter-keyword ((list . list)
                 (_cmd . (collect collecting)))
  :iter-bare ((list . listing)
              (_cmd . (collecting))))

(loopy-deftest collect-end-tracking-end-twice
  :result '(1 8 2 9 3 10 4 11)
  :multi-body t
  :body [((list i '(1 2 3 4))
          (collect coll i :at end)
          (collect coll (+ i 7) :at end)
          (finally-return coll))

         ((list i '(1 2 3 4))
          (collect i :at end)
          (collect (+ i 7) :at end))]
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest collect-end-tracking-start-end
  :doc "Should be same result in either order."
  :result '(3 2 1 1 2 3)
  :multi-body t
  :body [((list i '(1 2 3))
          (collect coll i :at end)
          (collect coll i :at start)
          (finally-return coll))

         ((list i '(1 2 3))
          (collect i :at end)
          (collect i :at start))

         ((list i '(1 2 3))
          (collect coll i :at start)
          (collect coll i :at end)
          (finally-return coll))

         ((list i '(1 2 3))
          (collect i :at start)
          (collect i :at end))]
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest collect-destructuring
  :doc "Check that `collect' implements destructuring, not destructuring itself."
  :result '((1 4) ((2 3) (5 6)))
  :body ((list j '((1 2 3) (4 5 6)))
         (collect (coll1 . coll2) j)
         (finally-return coll1 coll2))
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest collect-:at-start/beginning-explicit
  :result (list (list 3 2 1) (list 1 2 3))
  :multi-body t
  :body [((list i '(1 2 3))
          (collect coll1 i :at 'beginning)
          (collect coll2 (cl-first coll1))
          (finally-return coll1 coll2))

         ((list i '(1 2 3))
          (collect coll1 i :at 'start)
          (collect coll2 (cl-first coll1))
          (finally-return coll1 coll2))

         ((list i '(1 2 3))
          (collect coll1 i :at beginning)
          (collect coll2 (cl-first coll1))
          (finally-return coll1 coll2))

         ((list i '(1 2 3))
          (collect coll1 i :at start)
          (collect coll2 (cl-first coll1))
          (finally-return coll1 coll2))]
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest collect-:at-start/beginning-implicit
  :result (list 3 2 1)
  :multi-body t
  :body [((list i '(1 2 3))
          (collect i :at 'beginning))

         ((list i '(1 2 3))
          (collect i :at 'start))

         ((list i '(1 2 3))
          (collect i :at beginning))

         ((list i '(1 2 3))
          (collect i :at start))]
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest collect-:at-end-implicit
  :result (list 1 2 3)
  :multi-body t
  :body [((list i '(1 2 3)) (collect i :at 'end))
         ((list i '(1 2 3)) (collect i :at end))]
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest collect-:at-end-explicit
  :result (list (list 1 2 3) (list 1 1 1))
  :multi-body t
  :body [((list i '(1 2 3))
          (collect coll1 i :at 'end)
          (collect coll2 (cl-first coll1))
          (finally-return coll1 coll2))

         ((list i '(1 2 3))
          (collect coll1 i :at end)
          (collect coll2 (cl-first coll1))
          (finally-return coll1 coll2))]
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest collect-not-destructive
  :doc "This shouldn't ever happen, but it's still worth checking."
  :result t
  :multi-body t
  :body [((with (l1 (list (list 1 2) (list 3 4)
                          (list 5 6) (list 7 8))))
          (listing i l1)
          (collect coll i :at start)
          (finally-return (equal l1 (list (list 1 2) (list 3 4)
                                          (list 5 6) (list 7 8)))))

         ((with (l1 (list (list 1 2) (list 3 4)
                          (list 5 6) (list 7 8))))
          (listing i l1)
          (collect coll i :at end)
          (finally-return (equal l1 (list (list 1 2) (list 3 4)
                                          (list 5 6) (list 7 8)))))

         ((with (l1 (list (list 1 2) (list 3 4)
                          (list 5 6) (list 7 8))))
          (listing i l1)
          (collect i :at start)
          (finally-return (equal l1 (list (list 1 2) (list 3 4)
                                          (list 5 6) (list 7 8)))))

         ((with (l1 (list (list 1 2) (list 3 4)
                          (list 5 6) (list 7 8))))
          (listing i l1)
          (collect i :at end)
          (finally-return (equal l1 (list (list 1 2) (list 3 4)
                                          (list 5 6) (list 7 8)))))]
  :loopy t
  :iter-keyword (listing collect)
  :iter-bare ((listing . listing)
              (collect . collecting)))

;;;;; Concat
(loopy-deftest concat
  :result "catdog"
  :multi-body t
  :body [((list j '("cat" "dog"))
          (_cmd coll j)
          (finally-return coll))

         ((list j '("cat" "dog"))
          (_cmd j))]
  :repeat _cmd
  :loopy ((_cmd . (concat concating)))
  :iter-keyword ((list . list)
                 (_cmd . (concat concating)))
  :iter-bare ((list . listing)
              (_cmd . (concating))))

(loopy-deftest concat-destructuring
  :doc "Check that `concat'  implements destructuring, not the destrucutring itself."
  :result '("ad" "be" "cf")
  :body ((list j '(["a" "b" "c"] ["d" "e" "f"]))
         (concat [coll1 coll2 coll3] j)
         (finally-return coll1 coll2 coll3))
  :loopy t
  :iter-keyword (list concat)
  :iter-bare ((list . listing)
              (concat . concating)))

(loopy-deftest concat-:at-start/beginning
  :result "duckdogcat"
  :multi-body t
  :body [((list j '("cat" "dog" "duck")) (concat j :at start))
         ((list j '("cat" "dog" "duck")) (concat j :at 'start))
         ((list j '("cat" "dog" "duck")) (concat j :at beginning))
         ((list j '("cat" "dog" "duck")) (concat j :at 'beginning))

         ((list j '("cat" "dog" "duck")) (concat coll j :at start)  (finally-return coll))
         ((list j '("cat" "dog" "duck")) (concat coll j :at 'start) (finally-return coll))
         ((list j '("cat" "dog" "duck")) (concat coll j :at beginning) (finally-return coll))
         ((list j '("cat" "dog" "duck")) (concat coll j :at 'beginning) (finally-return coll))]
  :loopy t
  :iter-keyword (list concat)
  :iter-bare ((list . listing)
              (concat . concating)))

(loopy-deftest concat-:at-start
  :result "catdogduck"
  :multi-body t
  :body [((list j '("cat" "dog" "duck")) (concat j :at end))
         ((list j '("cat" "dog" "duck")) (concat j :at 'end))
         ((list j '("cat" "dog" "duck")) (concat coll j :at end)  (finally-return coll))
         ((list j '("cat" "dog" "duck")) (concat coll j :at 'end) (finally-return coll))]
  :loopy t
  :iter-keyword (list concat)
  :iter-bare ((list . listing)
              (concat . concating)))

(loopy-deftest concat-:at-end-destr
  :result '("ad" "be" "cf")
  :body ((list j '(("a" "b" "c") ("d" "e" "f")))
         (concat (coll1 coll2 coll3) j :at end)
         (finally-return coll1 coll2 coll3))
  :loopy t
  :iter-keyword (list concat)
  :iter-bare ((list . listing)
              (concat . concating)))

(loopy-deftest concat-:at-start-destr
  :result '("da" "eb" "fc")
  :body ((list j '(("a" "b" "c") ("d" "e" "f")))
         (concat (coll1 coll2 coll3) j :at start)
         (finally-return coll1 coll2 coll3))
  :loopy t
  :iter-keyword (list concat)
  :iter-bare ((list . listing)
              (concat . concating)))

(loopy-deftest concat-end-tracking-end-twice
  :doc "This only applies to the optimized form."
  :result "abcdef"
  :multi-body t
  :body [((accum-opt (coll end))
          (list (i j) '(("a" "b") ("c" "d") ("e" "f")))
          (concat coll i :at end)
          (concat coll j :at end)
          (finally-return coll))

         ((list (i j) '(("a" "b") ("c" "d") ("e" "f")))
          (concat i :at end)
          (concat j :at end))]
  :loopy t
  :iter-keyword (list concat)
  :iter-bare ((list . listing)
              (concat . concating)))

(loopy-deftest concat-end-tracking-start-end
  :doc "Should be same result in either order."
  :result "cbaabc"
  :multi-body t
  :body [((list i '("a" "b" "c"))
          (concat coll i :at end)
          (concat coll i :at start)
          (finally-return coll))

         ((list i '("a" "b" "c"))
          (concat i :at end)
          (concat i :at start))

         ((list i '("a" "b" "c"))
          (concat coll i :at start)
          (concat coll i :at end)
          (finally-return coll))

         ((list i '("a" "b" "c"))
          (concat i :at start)
          (concat i :at end))]
  :loopy t
  :iter-keyword (list concat)
  :iter-bare ((list . listing)
              (concat . concating)))
;;;;; Count
(loopy-deftest count
  :result 2
  :multi-body t
  :body [((list i '(t nil t nil))
          (_cmd c i)
          (finally-return c))

         ((list i '(t nil t nil))
          (_cmd i))]
  :repeat _cmd
  :loopy ((_cmd . (count counting)))
  :iter-keyword ((list . list)
                 (_cmd . (count counting)))
  :iter-keyword ((list . listing)
                 (_cmd . (count counting))))

(loopy-deftest count-destructuring
  :doc "Check that `count' implements destructuring, not destructuring itself."
  :result '(2 1)
  :body ((list elem '((t nil) (t t)))
         (count (c1 c2) elem)
         (finally-return c1 c2))
  :loopy t
  :iter-keyword (list count)
  :iter-keyword ((list . listing)
                 (count . counting)))

;;;;; Max
(loopy-deftest max
  :result 11
  :multi-body t
  :body [((list i '(1 11 2 10 3 9 4 8 5 7 6))
          (_cmd my-max i)
          (finally-return my-max))

         ((list i '(1 11 2 10 3 9 4 8 5 7 6))
          (_cmd i))]
  :repeat _cmd
  :loopy ((_cmd . (max maxing maximize maximizing)))
  :iter-keyword ((list . list)
                 (_cmd . (max maxing maximize maximizing)))
  :iter-bare ((list . listing)
              (_cmd . (maximizing))))

(loopy-deftest max-destructuring
  :doc "Check that `max' implements destructuring, not destructuring itself."
  :result '(9 11)
  :body  ((list elem '((1 11) (9 4)))
          (max (m1 m2) elem)
          (finally-return m1 m2))
  :loopy t
  :iter-keyword (list max)
  :iter-bare ((list . listing)
              (max . maximizing)))

;;;;; Min
(loopy-deftest min
  :result 0
  :multi-body t
  :body [((list i '(1 11 2 10 3 0 9 4 8 5 7 6))
          (_cmd my-min i)
          (finally-return my-min))

         ((list i '(1 11 2 10 3 0 9 4 8 5 7 6))
          (_cmd i))]
  :repeat _cmd
  :loopy ((_cmd . (min minning minimize minimizing)))
  :iter-keyword ((list . list)
                 (_cmd . (min minning minimize minimizing)))
  :iter-bare ((list . listing)
              (_cmd . (minimizing))))

(loopy-deftest min-destructuring
  :doc "Check that `min' implements destructuring, not destructuring itself."
  :result '(1 4)
  :body  ((list elem '((1 11) (9 4)))
          (min (m1 m2) elem)
          (finally-return m1 m2))
  :loopy t
  :iter-keyword (list min)
  :iter-bare ((list . listing)
              (min . minimizing)))

;;;;; Multiply
(loopy-deftest multiply
  :result 120
  :multi-body t
  :body [((list i '(1 2 3 4 5))
          (_cmd product i)
          (finally-return product))
         ((list i '(1 2 3 4 5))
          (_cmd i))]
  :repeat _cmd
  :loopy ((_cmd . (multiply multiplying)))
  :iter-keyword ((list . listing)
                 (_cmd . (multiply multiplying)))
  :iter-bare ((_cmd . (multiplying))
              (list . listing)))

(loopy-deftest multiply-destructuring
  :doc "Check that `multiply' implements destructuring, not the destructuring itself."
  :result '(3 8)
  :body ((list i '((1 2) (3 4)))
         (multiply (x y) i)
         (finally-return x y))
  :loopy t
  :iter-keyword (list multiply)
  :iter-bare ((multiply . multiplying)
              (list . listing)))

;;;;; Nconc
(loopy-deftest nconc
  :result '(1 2 3 4 5 6)
  :multi-body t
  :body [((sequence i (list (list 1 2 3) (list 4 5 6)))
          (_cmd l i)
          (finally-return l))

         ((sequence i (list (list 1 2 3) (list 4 5 6)))
          (_cmd i))]
  :repeat _cmd
  :loopy ((_cmd . (nconc nconcing)))
  :iter-keyword ((sequence . sequence)
                 (_cmd . (nconc nconcing)))
  :iter-bare ((sequence . sequencing)
              (_cmd . (nconcing))))

(loopy-deftest nconc-destructuring
  :doc "Check that `nconc' implements destructuring, not destructuring itself."
  :result '((1 4) ((2 3) (5 6)))
  :body ((array elem (vector (list (list 1) (list 2 3))
                             (list (list 4) (list 5 6))))
         (nconc (n1 . n2) elem)
         (finally-return n1 n2))
  :loopy t
  :iter-keyword (array nconc)
  :iter-bare ((array . arraying)
              (nconc . nconcing)))

;; TODO: Not sure how this text is supposed to work when Emacs is allowed to
;; modify he literals.
;;
;; (ert-deftest nconc-at-literal-lists ()
;;   (should (equal '(1 2 3 4 5 6)
;;                  (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
;;                                      (nconc i))))))
;;
;;   (should (equal '(1 2 3 4 5 6)
;;                  (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
;;                                      (nconc i :at end))))))
;;
;;   (should (equal '(4 5 6 1 2 3)
;;                  (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
;;                                      (nconc i :at start))))))
;;   (should (equal '(1 2 3 4 5 6)
;;                  (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
;;                                      (nconc c i)
;;                                      (finally-return c))))))
;;
;;   (should (equal '(1 2 3 4 5 6)
;;                  (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
;;                                      (nconc c i :at end)
;;                                      (finally-return c))))))
;;
;;   (should (equal '(4 5 6 1 2 3)
;;                  (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
;;                                      (nconc c i :at start)
;;                                      (finally-return c)))))))

(loopy-deftest nconc-:at-end
  :result '(1 2 3 4 5 6)
  :multi-body t
  :body [((with (l1 (list 1 2 3))
                (l2 (list 4 5 6)))
          (seq i (list l1 l2))
          (nconc coll i :at end)
          (finally-return coll))

         ((with (l1 (list 1 2 3))
                (l2 (list 4 5 6)))
          (seq i (list l1 l2))
          (nconc i :at end))]
  :loopy t
  :iter-keyword (seq nconc)
  :iter-bare ((seq . seqing)
              (nconc . nconcing)))

(loopy-deftest nconc-:at-start/beginning
  :result '(4 5 6 1 2 3)
  :multi-body t
  :body [((with (l1 (list 1 2 3))
                (l2 (list 4 5 6)))
          (seq i (list l1 l2))
          (nconc coll i :at start)
          (finally-return coll))

         ((with (l1 (list 1 2 3))
                (l2 (list 4 5 6)))
          (seq i (list l1 l2))
          (nconc i :at start))

         ((with (l1 (list 1 2 3))
                (l2 (list 4 5 6)))
          (seq i (list l1 l2))
          (nconc coll i :at beginning)
          (finally-return coll))

         ((with (l1 (list 1 2 3))
                (l2 (list 4 5 6)))
          (seq i (list l1 l2))
          (nconc i :at beginning))]
  :loopy t
  :iter-keyword (seq nconc)
  :iter-bare ((seq . seqing)
              (nconc . nconcing)))

(loopy-deftest nconc-end-tracking-end-twice
  :result '(1 2 11 12 3 4 13 14 5 6 15 16)
  :multi-body t
  :body [((seq i (list (list 1 2) (list 3 4) (list 5 6)))
          (seq j (list (list 11 12) (list 13 14) (list 15 16)))
          (nconc coll i :at end)
          (nconc coll j :at end)
          (finally-return coll))

         ((seq i (list (list 1 2) (list 3 4) (list 5 6)))
          (seq j (list (list 11 12) (list 13 14) (list 15 16)))
          (nconc i :at end)
          (nconc j :at end))]
  :loopy t
  :iter-keyword (seq nconc)
  :iter-bare ((seq . seqing)
              (nconc . nconcing)))

(loopy-deftest nconc-end-tracking-start-and-end
  :doc "Should get same result in both orders."
  :result '(5 6 3 4 1 2 11 12 13 14 15 16)
  :multi-body t
  :body [((seq i '((1 2) (3 4) (5 6)))
          (nconc coll (copy-sequence i) :at start)
          (nconc coll (mapcar (lambda (x) (+ x 10))
                              (copy-sequence i))
                 :at end)
          (finally-return coll))

         ((seq i '((1 2) (3 4) (5 6)))
          (nconc (copy-sequence i) :at start)
          (nconc (mapcar (lambda (x) (+ x 10))
                         (copy-sequence i))
                 :at end))

         ((seq i '((1 2) (3 4) (5 6)))
          (nconc coll (mapcar (lambda (x) (+ x 10))
                              (copy-sequence i))
                 :at end)
          (nconc coll (copy-sequence i) :at start)
          (finally-return coll))

         ((seq i '((1 2) (3 4) (5 6)))
          (nconc (mapcar (lambda (x) (+ x 10))
                         (copy-sequence i))
                 :at end)
          (nconc (copy-sequence i) :at start))]
  :loopy t
  :iter-keyword (seq nconc)
  :iter-bare ((seq . seqing)
              (nconc . nconcing)))

(loopy-deftest nconc-end-tracking-opt-end-at-start
  :result '(5 6 3 4 1 2)
  :body ((accum-opt (coll end))
         (seq i (list (list 1 2) (list 3 4) (list 5 6)))
         (nconc coll i :at start)
         (finally-return coll))
  :loopy t
  :iter-keyword (seq nconc)
  :iter-bare ((seq . seqing)
              (nconc . nconcing)))

(loopy-deftest nconc-end-tracking-opt-start-at-end
  :result '(1 2 3 4 5 6)
  :body ((accum-opt (coll start))
         (seq i (list (list 1 2) (list 3 4) (list 5 6)))
         (nconc coll i :at end)
         (finally-return coll))
  :loopy t
  :iter-keyword (seq nconc)
  :iter-bare ((seq . seqing)
              (nconc . nconcing)))

;;;;; Nunion
(loopy-deftest nunion
  :result '(1 2 3 4)
  :multi-body t
  :body [((seq i (list (list 1 2) (list 2 3) (list 3 4)))
          (_cmd var i)
          (finally-return var))

         ((seq i (list (list 1 2) (list 2 3) (list 3 4)))
          (_cmd i))]
  :repeat _cmd
  :loopy ((_cmd . (nunion nunioning)))
  :iter-keyword ((seq . seq)
                 (_cmd . (nunion nunioning)))
  :iter-bare ((seq . seqing)
              (_cmd . (nunioning))))

(loopy-deftest nunion-:test
  :result '((1 1) 2 3 4)
  :multi-body t
  :body [((seq i (list (list (list 1 1) 2)
                       (list (list 1 1) 3)
                       (list 3 4)))
          (nunion var i :test #'equal)
          (finally-return var))

         ((with (test #'equal))
          (seq i (list (list (list 1 1) 2)
                       (list (list 1 1) 3)
                       (list 3 4)))
          (nunion var i :test test)
          (finally-return var))

         ((seq i (list (list (list 1 1) 2)
                       (list (list 1 1) 3)
                       (list 3 4)))
          (nunion i :test #'equal))

         ((with (test #'equal))
          (seq i (list (list (list 1 1) 2)
                       (list (list 1 1) 3)
                       (list 3 4)))
          (nunion i :test test))]
  :loopy t
  :iter-keyword (seq nunion)
  :iter-bare ((seq . seqing)
              (nunion . nunioning)))

(loopy-deftest nunion-:key
  :doc "The resulting list should only have one element whose `car' is `a'."
  :result '((a . 1))
  :multi-body t
  :body [((array i (vector (list '(a . 1)) (list '(a . 2))))
          (nunioning var i :key #'car)
          (finally-return var))

         ((with (key #'car))
          (array i (vector (list '(a . 1)) (list '(a . 2))))
          (nunioning var i :key key)
          (finally-return var))

         ((array i (vector (list '(a . 1)) (list '(a . 2))))
          (nunioning i :key #'car))

         ((with (key #'car))
          (array i (vector (list '(a . 1)) (list '(a . 2))))
          (nunioning i :key key))]
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

(loopy-deftest nunion-destructuring
  :doc "Check that `nunion' implements, not destructuring itself."
  :result '((1 2 3) (2 3 4))
  :body ((array i (vector (list (list 1 2) (list 2 3))
                          (list (list 1 2 3) (list 3 4))))
         (nunion (var1 var2) i :test #'equal)
         (finally-return var1 var2))
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

(loopy-deftest nunion-:at-end
  :result '(1 2 3 4 5 6 7 8)
  :multi-body t
  :body [((array i (vector (list 1 2 3) (list 3 3 4 5)
                           (list 5 5 6 7 8)))
          (nunion coll i :at end)
          (finally-return coll))
         ((array i (vector (list 1 2 3) (list 3 3 4 5)
                           (list 5 5 6 7 8)))
          (nunion i))]
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

(loopy-deftest nunion-:at-start/beginning
  :result '(8 7 6 5 4 3 2 1)
  :multi-body t
  :body [((array i (vector (list 3 2 1) (list 5 4 3 3)
                           (list 8 7 6 5 5)))
          (nunion coll i :at start)
          (finally-return coll))
         ((array i (vector (list 3 2 1) (list 5 4 3 3)
                           (list 8 7 6 5 5)))
          (nunion i :at start))

         ((array i (vector (list 3 2 1) (list 5 4 3 3)
                           (list 8 7 6 5 5)))
          (nunion coll i :at beginning)
          (finally-return coll))
         ((array i (vector (list 3 2 1) (list 5 4 3 3)
                           (list 8 7 6 5 5)))
          (nunion i :at beginning))]
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

(loopy-deftest nunion-:at-end-:key-cl-second
  :result '((1 2) (3 2) (1 1))
  :multi-body t
  :body [((array i (vector (list (list 1 2) (list 3 2))
                           (list (list 1 1) (list 4 2))))
          (nunion coll i :at end :key #'cl-second)
          (finally-return coll))

         ((array i (vector (list (list 1 2) (list 3 2))
                           (list (list 1 1) (list 4 2))))
          (nunion i :at end :key #'cl-second))]
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

(loopy-deftest nunion-:at-end-:key-car
  :result '((1 2) (3 2) (4 2))
  :multi-body t
  :body [((array i (vector (list (list 1 2) (list 3 2))
                           (list (list 1 1) (list 4 2))))
          (nunion coll i :at end :key #'car)
          (finally-return coll))

         ((array i (vector (list (list 1 2) (list 3 2))
                           (list (list 1 1) (list 4 2))))
          (nunion i :at end :key #'car))]
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

(loopy-deftest nunion-:at-start-:key-car
  :result '((4 2) (1 2) (3 2))
  :multi-body t
  :body [((array i (vector (list (list 1 2) (list 3 2))
                           (list (list 1 1) (list 4 2))))
          (nunion coll i :at start :key #'car)
          (finally-return coll))

         ((array i (vector (list (list 1 2) (list 3 2))
                           (list (list 1 1) (list 4 2))))
          (nunion i :at start :key #'car))]
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

(loopy-deftest nunion-:at-start-:test-equal
  :result '(1 2 3)
  :multi-body t
  :body [((array i (vector (list 1 2 3) (list 1 2 3)))
          (nunion coll i :at start :test #'equal)
          (finally-return coll))
         ((array i (vector (list 1 2 3) (list 1 2 3)))
          (nunion i :at start :test #'equal))]
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

(loopy-deftest nunion-end-tracking-:at-end-twice
  :result '(1 2 3 4 5 6 7 8 9 10)
  :multi-body t
  :body [((array i (vector (list 1 2 3) (list 1 2 3)
                           (list 4 5 6) (list 7 8 9)))
          (nunion coll (copy-sequence i) :at end)
          (nunion coll (mapcar #'1+ i) :at end)
          (finally-return coll))

         ((array i (vector (list 1 2 3) (list 1 2 3)
                           (list 4 5 6) (list 7 8 9)))
          (nunion (copy-sequence i) :at end)
          (nunion (mapcar #'1+ i) :at end))

         ((array i (vector (list 1 2 3) (list 1 2 3)
                           (list 4 5 6) (list 7 8 9)))
          (nunion coll (copy-sequence i) :at end)
          (nunion coll (mapcar #'1+ i) :at end)
          (finally-return coll))

         ((array i (vector (list 1 2 3) (list 1 2 3)
                           (list 4 5 6) (list 7 8 9)))
          (nunion (copy-sequence i) :at end)
          (nunion (mapcar #'1+ i) :at end))]
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

(loopy-deftest nunion-end-tracking-:at-start-twice
  :result '(10 8 9 7 5 6 4 1 2 3)
  :multi-body t
  :body [((array i (vector (list 1 2 3) (list 1 2 3)
                           (list 4 5 6) (list 7 8 9)))
          (nunion coll (copy-sequence i) :at start)
          (nunion coll (mapcar #'1+ i) :at start)
          (finally-return coll))

         ((array i (vector (list 1 2 3) (list 1 2 3)
                           (list 4 5 6) (list 7 8 9)))
          (nunion (copy-sequence i) :at start)
          (nunion (mapcar #'1+ i) :at start))]
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

(loopy-deftest nunion-end-tracking-accum-opt-start-:at-end
  :result '(1 2 3 4 5 6 7 8 9 10)
  :body ((accum-opt (coll start))
         (array i (vector (list 1 2 3) (list 1 2 3)
                          (list 4 5 6) (list 7 8 9)))
         (nunion coll (copy-sequence i) :at end)
         (nunion coll (mapcar #'1+ i) :at end)
         (finally-return coll))
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

;; TODO: Fail.  Fix in optimized constructor, same as others.
(loopy-deftest nunion-end-tracking-accum-opt-end-:at-start
  :result '(10 8 9 7 5 6 4 1 2 3)
  :body ((accum-opt (coll end))
         (array i (vector (list 1 2 3) (list 1 2 3)
                          (list 4 5 6) (list 7 8 9)))
         (nunion coll (copy-sequence i) :at start)
         (nunion coll (mapcar #'1+ i) :at start)
         (finally-return coll))
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

(loopy-deftest nunion-end-tracking-:at-start-and-end
  :result '(5 6 3 4 1 2 11 12 13 14 15 16)
  :multi-body t
  :body [((array i (vector (list 1 2) (list 3 4) (list 5 6)))
          (nunion (copy-sequence i) :at start)
          (nunion (mapcar (lambda (x) (+ x 10)) (copy-sequence i))
                  :at end))

         ((array i (vector (list 1 2) (list 3 4) (list 5 6)))
          (nunion coll (copy-sequence i) :at start)
          (nunion coll (mapcar (lambda (x) (+ x 10)) (copy-sequence i))
                  :at end)
          (finally-return coll))

         ((array i (vector (list 1 2) (list 3 4) (list 5 6)))
          (nunion (mapcar (lambda (x) (+ x 10)) (copy-sequence i))
                  :at end)
          (nunion (copy-sequence i) :at start))

         ((array i (vector (list 1 2) (list 3 4) (list 5 6)))
          (nunion coll (mapcar (lambda (x) (+ x 10)) (copy-sequence i))
                  :at end)
          (nunion coll (copy-sequence i) :at start)
          (finally-return coll))]
  :loopy t
  :iter-keyword (array nunion)
  :iter-bare ((array . arraying)
              (nunion . nunioning)))

;;;;; Prepend
(loopy-deftest prepend
  :result '(5 6 3 4 1 2)
  :multi-body t
  :body [((list i '((1 2) (3 4) (5 6)))
          (_cmd my-list i)
          (finally-return my-list))

         ((list i '((1 2) (3 4) (5 6)))
          (_cmd i))]
  :repeat _cmd
  :loopy ((_cmd . (prepend prepending)))
  :iter-keyword ((_cmd . (prepend prepending))
                 (list . list))
  :iter-bare ((_cmd . (prepending))
              (list . listing)))

(loopy-deftest prepend-destructuring
  :result '((5 6 1 2) (7 8 3 4))
  :body ((list i '([(1 2) (3 4)] [(5 6) (7 8)]))
         (prepend [my-list1 my-list2] i)
         (finally-return my-list1 my-list2))
  :loopy t
  :iter-keyword (list prepend)
  :iter-bare ((list . listing)
              (prepend . prepending)))


(loopy-deftest prepend-append-compat
  :result '(7 6 5 4 3 2 1 2 3 4 5 6)
  :multi-body t
  :body [((list i '((1) (2) (3) (4) (5) (6)))
          (accum-opt (var end))
          (append var i :at end)
          (prepend var (mapcar #'1+ i))
          (finally-return var))

         ((list i '((1) (2) (3) (4) (5) (6)))
          (accum-opt (var end))
          (append i :at end)
          (prepend (mapcar #'1+ i)))]
  :loopy t
  :iter-keyword (list prepend append)
  :iter-bare ((list . listing)
              (append . appending)
              (prepend . prepending)))

;;;;; Push Into
(loopy-deftest push-into
  :result  '(3 2 1)
  :multi-body t
  :body [((list j '(1 2 3))
          (_cmd coll j)
          (finally-return coll))

         ((list j '(1 2 3))
          (_cmd j))]
  :repeat _cmd
  :loopy ((_cmd . (push-into pushing-into push pushing)))
  :iter-keyword ((list . list)
                 (_cmd . (push-into pushing-into push pushing)))
  :iter-bare ((list . listing)
              (_cmd . (pushing-into pushing))))


(loopy-deftest push-into-destructuring
  :result '((5 3 1) (6 4 2))
  :body ((list elem '((1 2) (3 4) (5 6)))
         (push-into (p1 p2) elem)
         (finally-return p1 p2))
  :loopy t
  :iter-keyword (list push-into)
  :iter-bare ((list . listing)
              (push-into . pushing-into)))

(loopy-deftest push-into-collect-compat
  :result '(7 6 5 4 3 2 1 2 3 4 5 6)
  :body ((list i '(1 2 3 4 5 6))
         (accum-opt (var end))
         (collect var i :at end)
         (push-into var (1+ i))
         (finally-return var))
  :loopy t
  :iter-keyword (list push-into collect)
  :iter-bare ((list . listing)
              (collect . collecting)
              (push-into . pushing-into)))

;;;;; Reduce
(loopy-deftest reduce
  :result '(1 2 3 4 5 6 7 8)
  :multi-body t
  :body [((list i '((1 2 3) (4 5 6) (7 8)))
          (_cmd r i #'append)
          (finally-return r))

         ((list i '((1 2 3) (4 5 6) (7 8)))
          (_cmd r i #'append)
          (finally-return r))]
  :repeat _cmd
  :loopy ((_cmd . (reduce reducing callf)))
  :iter-keyword ((list . list)
                 (_cmd . (reduce reducing callf)))
  :iter-bare ((list . listing)
              (_cmd . (reducing))))

(loopy-deftest reduce-no-init
  :doc "When the accumulation variable isn't explicitly initialized,
`reduce' should store the first value without calling the function.

This is how `cl-reduce' and `seq-reduce' work."
  :result (cl-reduce #'+ '(1 2 3))
  :multi-body t
  :body [((list i '(1 2 3))
          (reduce r i #'+)
          (finally-return r))

         ((list i '(1 2 3))
          (reduce i #'+))]
  :loopy t
  :iter-keyword (list reduce)
  :iter-bare ((list . listing)
              (reduce . reducing)))

(loopy-deftest reduce-with
  :doc "Test that we can replace `:init' with `with'."
  :result 6
  :multi-body t
  :body [((with (r 0))
          (list i '(1 2 3))
          (_cmd r i #'+)
          (finally-return r))

         ((with (r 0))
          (list i '(1 2 3))
          (_cmd r i #'+)
          (finally-return r))]
  :repeat _cmd
  :loopy ((_cmd . (reduce reducing callf)))
  :iter-keyword ((list . list)
                 (_cmd . (reduce reducing callf)))
  :iter-bare ((list . listing)
              (_cmd . (reducing))))

(loopy-deftest reduce-append
  :result '(1 2 3)
  :multi-body t
  :body [((list i '((1) (2) (3)))
          (reduce r i #'append)
          (finally-return r))

         ((with (func #'append))
          (list i '((1) (2) (3)))
          (reduce r i func)
          (finally-return r))]
  :loopy t
  :iter-keyword (list reduce)
  :iter-bare ((list . listing)
              (reduce . reducing)))

(loopy-deftest reduce-destructuring-+
  :result '(4 6)
  :multi-body t
  :body [((with (r1 0)
                (r2 0))
          (list i '((1 2) (3 4)))
          (reduce (r1 r2) i #'+)
          (finally-return r1 r2))

         ((list i '((1 2) (3 4)))
          (reduce (r1 r2) i #'+)
          (finally-return r1 r2))]
  :loopy t
  :iter-keyword (list reduce)
  :iter-bare ((list . listing)
              (reduce . reducing)))

(loopy-deftest reduce-destructuring-append
  :result '((1 3) (2 4))
  :body ((list i '([(1) (2)] [(3) (4)]))
         (reduce [r1 r2] i #'append)
         (finally-return r1 r2))
  :loopy t
  :iter-keyword (list reduce)
  :iter-bare ((list . listing)
              (reduce . reducing)))

;;;;; Set Accum

(loopy-deftest set-accum-+
  :result 16
  :multi-body t
  :body [((with (my-sum 10))
          (list i '(1 2 3))
          (_cmd my-sum (+ my-sum i))
          (finally-return my-sum))

         ((with (loopy-result 10))
          (list i '(1 2 3))
          (_cmd (+ loopy-result i)))]
  :repeat _cmd
  :loopy ((_cmd . (set-accum setting-accum)))
  :iter-keyword ((list . list)
                 (_cmd . (set-accum setting-accum)))
  :iter-bare ((list . listing)
              (_cmd . (setting-accum))))

(loopy-deftest set-accum-+-2
  :doc "Test to make sure that we can replace `:init' with `with'."
  :result 16
  :multi-body t
  :body [((with (my-sum 10))
          (list i '(1 2 3))
          (set-accum my-sum (+ my-sum i))
          (finally-return my-sum))

         ((with (loopy-result 10))
          (list i '(1 2 3))
          (set-accum (+ loopy-result i)))]
  :loopy t
  :iter-keyword (list set-accum)
  :iter-bare ((list . listing)
              (set-accum . setting-accum)))

(loopy-deftest set-accum-cons
  :result '(3 2 1)
  :multi-body t
  :body [((list i '(1 2 3))
          (set-accum coll (cons i coll))
          (finally-return coll))

         ((list i '(1 2 3))
          (set-accum (cons i loopy-result)))]
  :loopy t
  :iter-keyword (list set-accum)
  :iter-bare ((list . listing)
              (set-accum . setting-accum)))

(loopy-deftest set-accum-destructuring
  :result '(9 12)
  :body ((with (car 0) (cdr 0))
         (array elem [(1 . 2) (3 . 4) (5 . 6)])
         (set-accum (car . cdr) (cons (+ car (car elem))
                                      (+ cdr (cdr elem))))
         (finally-return car cdr))
  :loopy t
  :iter-keyword (array set-accum)
  :iter-bare ((array . arraying)
              (set-accum . setting-accum)))

;;;;; Sum
(loopy-deftest sum
  :result 6
  :multi-body t
  :body [((list i '(1 2 3))
          (_cmd s i)
          (finally-return s))
         ((list i '(1 2 3))
          (_cmd i))]
  :repeat _cmd
  :loopy ((list . list)
          (_cmd . (sum summing)))
  :iter-keyword ((list . list)
                 (_cmd . (sum summing)))
  :iter-bare ((list . listing)
              (_cmd . (summing))))

(loopy-deftest sum-destructuring
  :result '(5 7 9)
  :body ((list elem '((1 2 3) (4 5 6)))
         (sum (sum1 sum2 sum3) elem)
         (finally-return sum1 sum2 sum3))
  :loopy t
  :iter-keyword (list sum)
  :iter-bare ((list . listing)
              (sum . summing)))

;;;;; Union

(loopy-deftest union
  :result '(1 2 3 4)
  :multi-body t
  :body [((list i '((1 2) (2 3) (3 4)))
          (_cmd var i)
          (finally-return var))

         ((list i '((1 2) (2 3) (3 4)))
          (_cmd i))]
  :repeat _cmd
  :loopy ((_cmd . (union unioning)))
  :iter-keyword ((list . list)
                 (_cmd . (union unioning)))
  :iter-bare ((list . listing)
              (_cmd . (unioning))))

(loopy-deftest union-:key
  :result '((a . 1))
  :multi-body t
  :body [((array i [((a . 1)) ((a . 2))])
          (union var i :key #'car)
          (finally-return var))

         ((with (func #'car))
          (array i [((a . 1)) ((a . 2))])
          (union i :key func))]
  :loopy t
  :iter-keyword (union array)
  :iter-bare ((union . unioning)
              (array . arraying)))

(loopy-deftest union-:test
  :result '((1 1) 2 3 4)
  :multi-body t
  :body [((list i '(((1 1) 2) ((1 1) 3) (3 4)))
          (union var i :test #'equal)
          (finally-return var))

         ((with (func #'equal))
          (list i '(((1 1) 2) ((1 1) 3) (3 4)))
          (union i :test func))]
  :loopy t
  :iter-keyword (union list)
  :iter-bare ((union . unioning)
              (list . listing)))

(loopy-deftest union-destructuring
  :result '((1 2 3) (2 3 4))
  :body ((array i [((1 2) (2 3))
                   ((1 2 3) (3 4))])
         (union (var1 var2) i :test #'=)
         (finally-return var1 var2))
  :loopy t
  :iter-keyword (array union)
  :iter-bare ((array . arraying)
              (union . unioning)))

(loopy-deftest union-:at-end
  :result '(1 2 3 4 5 6)
  :multi-body t
  :body [((list i '((1 2 3) (3 4 5 6)))
          (union i :at end))

         ((list i '((1 2 3) (4 5 3 6)))
          (union coll i :at end)
          (finally-return coll))]
  :loopy t
  :iter-keyword (list union)
  :iter-bare ((list . listing)
              (union . unioning)))

(loopy-deftest union-:at-start/beginning
  :result '(4 5 6 1 2 3)
  :multi-body t
  :body [((list i '((1 2 3) (3 4 5 6)))
          (union i :at start))
         ((list i '((1 2 3) (3 4 5 6)))
          (union coll i :at start)
          (finally-return coll))
         ((list i '((1 2 3) (3 4 5 6)))
          (union i :at beginning))
         ((list i '((1 2 3) (3 4 5 6)))
          (union coll i :at beginning)
          (finally-return coll))]
  :loopy t
  :iter-keyword (list union)
  :iter-bare ((list . listing)
              (union . unioning)))

(loopy-deftest union-end-tracking-accum-opt-end
  :result '(4 5 6 1 2 3)
  :body ((accum-opt (coll end))
         (list i '((1 2 3) (3 4 5 6)))
         (union coll i :at start)
         (finally-return coll))
  :loopy t
  :iter-keyword (list union)
  :iter-bare ((list . listing)
              (union . unioning)))

(loopy-deftest union-end-tracking-accum-opt-start
  :result '(1 2 3 4 6 7)
  :body ((accum-opt (coll start))
         (list i '((1 2 3 4) (3 6 4 7)))
         (union coll i :at end)
         (finally-return coll))
  :loopy t
  :iter-keyword (list union)
  :iter-bare ((list . listing)
              (union . unioning)))

(loopy-deftest union-end-tracking-start-end
  :doc "Don't overlap. Just check that it combines well."
  :result '(8 7 6 5 1 2 3 4 11 12 13 14 18 17 16 15)
  :multi-body t
  :body [((list i '((1 2 3 4) (8 7 6 5)))
          (union coll i :at start)
          (union coll (mapcar (lambda (x) (+ x 10)) i) :at end)
          (finally-return coll))

         ((list i '((1 2 3 4) (8 7 6 5)))
          (union coll (mapcar (lambda (x) (+ x 10)) i) :at end)
          (union coll i :at start)
          (finally-return coll))

         ((list i '((1 2 3 4) (8 7 6 5)))
          (union i :at start)
          (union (mapcar (lambda (x) (+ x 10)) i) :at end))

         ((list i '((1 2 3 4) (8 7 6 5)))
          (union (mapcar (lambda (x) (+ x 10)) i) :at end)
          (union i :at start))]
  :loopy t
  :iter-keyword (list union)
  :iter-bare ((list . listing)
              (union . unioning)))

(loopy-deftest union-end-tracking-end-twice
  :result '(1 2 3 4 5 7 6 8)
  :multi-body t
  :body [((list i '((1 2 3 4) (7 4 6 3)))
          (union coll i :at end)
          (union coll (mapcar (lambda (x) (+ x 1)) i) :at end)
          (finally-return coll))

         ((list i '((1 2 3 4) (7 4 6 3)))
          (union i :at end)
          (union (mapcar (lambda (x) (+ x 1)) i) :at end))]
  :loopy t
  :iter-keyword (list union)
  :iter-bare ((list . listing)
              (union . unioning)))

(loopy-deftest union-not-destructive
  :doc "Check that `union' doesn't modify the list being unioned."
  :result t
  :multi-body t
  :body [((with (l1 (list (list 1 2 3) (list 3 4 5) (list 5 6 7) (list 7 8 9))))
          (listing i l1)
          (union coll i :at start)
          (finally-return (equal l1 '((1 2 3) (3 4 5) (5 6 7) (7 8 9)))))

         ((with (l1 (list (list 1 2 3) (list 3 4 5) (list 5 6 7) (list 7 8 9))))
          (listing i l1)
          (union coll i :at end)
          (finally-return (equal l1 '((1 2 3) (3 4 5) (5 6 7) (7 8 9)))))

         ((with (l1 (list (list 1 2 3) (list 3 4 5) (list 5 6 7) (list 7 8 9))))
          (listing i l1)
          (union i :at start)
          (finally-return (equal l1 '((1 2 3) (3 4 5) (5 6 7) (7 8 9)))))

         ((with (l1 (list (list 1 2 3) (list 3 4 5) (list 5 6 7) (list 7 8 9))))
          (listing i l1)
          (union i :at end)
          (finally-return (equal l1 '((1 2 3) (3 4 5) (5 6 7) (7 8 9)))))]
  :loopy t
  :iter-keyword (listing union)
  :iter-bare ((union . unioning)))

(loopy-deftest union-:test-:key
  :result '((4 2) (1 2) (3 2))
  :multi-body t
  :body [((list i '(((1 2) (3 2)) ((1 1) (4 2))))
          (union var i :at 'start :key #'car)
          (finally-return var))

         ((list i '(((1 2) (3 2)) ((1 1) (4 2))))
          (union i :at 'start :key #'car))]
  :loopy t
  :iter-keyword (list union)
  :iter-bare ((list . listing)
              (union . unioning)))

;;;;; Vconcat
(loopy-deftest vconcat
  :multi-body t
  :result [1 2 3 4 5 6 7 8 9 10 11 12]
  :body [((list elem '([1 2 3 4 5 6]
                       [7 8 9 10 11 12]))
          (_cmd v elem)
          (finally-return v))

         ((list elem '([1 2 3 4 5 6]
                       [7 8 9 10 11 12]))
          (_cmd elem))]
  :repeat _cmd
  :loopy ((_cmd . (vconcat vconcating)))
  :iter-keyword ((list . listing)
                 (_cmd . (vconcat vconcating)))
  :iter-bare ((list . listing)
              (_cmd . (vconcating))))

(loopy-deftest vconcat-destructuring
  :result '([1 2 3 7 8 9] [4 5 6 10 11 12])
  :body ((list elem '(([1 2 3] [4 5 6])
                      ([7 8 9] [10 11 12])))
         (vconcat (v1 v2) elem)
         (finally-return v1 v2))
  :loopy t
  :iter-keyword (list vconcat)
  :iter-bare ((list . listing)
              (vconcat . vconcating)))

(loopy-deftest vconcat-:at-start/beginning
  :result [7 8 9 10 11 12 1 2 3 4 5 6]
  :multi-body t
  :body  [((list elem '([1 2 3 4 5 6]
                        [7 8 9 10 11 12]))
           (vconcat v elem :at start)
           (finally-return v))

          ((list elem '([1 2 3 4 5 6]
                        [7 8 9 10 11 12]))
           (vconcat v elem :at beginning)
           (finally-return v))

          ((list elem '([1 2 3 4 5 6]
                        [7 8 9 10 11 12]))
           (vconcat elem :at start))

          ((list elem '([1 2 3 4 5 6]
                        [7 8 9 10 11 12]))
           (vconcat elem :at beginning))]
  :loopy t
  :iter-keyword (list vconcat)
  :iter-bare ((list . listing)
              (vconcat . vconcating)))

(loopy-deftest vconcat-:at-end
  :result [1 2 3 4 5 6 7 8 9 10 11 12]
  :multi-body t
  :body  [((list elem '([1 2 3 4 5 6]
                        [7 8 9 10 11 12]))
           (vconcat v elem :at end)
           (finally-return v))

          ((list elem '([1 2 3 4 5 6]
                        [7 8 9 10 11 12]))
           (vconcat elem :at end))]
  :loopy t
  :iter-keyword (list vconcat)
  :iter-bare ((list . listing)
              (vconcat . vconcating)))

(loopy-deftest vconcat-destr-:at-start
  :result '([7 8 9 1 2 3] [10 11 12 4 5 6])
  :body  ((list elem '(([1 2 3] [4 5 6])
                       ([7 8 9] [10 11 12])))
          (vconcat (v1 v2) elem :at start)
          (finally-return v1 v2))
  :loopy t
  :iter-keyword (list vconcat)
  :iter-bare ((list . listing)
              (vconcat . vconcating)))

(loopy-deftest vconcat-destr-:at-end
  :result '([1 2 3 7 8 9] [4 5 6 10 11 12])
  :body  ((list elem '(([1 2 3] [4 5 6])
                       ([7 8 9] [10 11 12])))
          (vconcat (v1 v2) elem :at end)
          (finally-return v1 v2))
  :loopy t
  :iter-keyword (list vconcat)
  :iter-bare ((list . listing)
              (vconcat . vconcating)))

;;;;; Miscellaneous
;;; Control Flow
;;;; Conditionals
;;;;; If
(loopy-deftest if
  :result '((2 4) (1 3))
  :body ((list i '(1 2 3 4))
         (if (cl-evenp i)
             (collect evens i)
           (collect odds i))
         (finally-return evens odds))
  :loopy t)

;;;;; When
;; (ert-deftest basic-when-parse ()
;;   (should (equal (loopy--parse-conditional-forms 'when 't '((do (+ 1 1))))
;;                  '((loopy--main-body when t (progn (+ 1 1)))))))

(loopy-deftest recursive-when-test
  :result  '(6 . 4)
  :body ((list i (number-sequence 1 10))
         (list j '(1 2 3 6 7 8))
         (when (cl-evenp i)
           (when (> j i)
             (return (cons j i)))))
  :loopy t)

(loopy-deftest when-multiple-subcommands
  :result '(2 (1 3))
  :body ((with (counter 0))
         (list i '(1 2 3))
         (when (cl-oddp i)
           (collect odds i)
           (do (cl-incf counter)))
         (finally-return counter odds))
  :loopy t)

(loopy-deftest multi-when-prepend-test
  :result "Multiple of 2: 2
Multiple of 3: 3
Multiple of 2: 4
Multiple of 2: 6
Multiple of 3: 6"
  :body ((with (first-var 2)
               (second-var 3))
         (seq el [1 2 3 4 5 6 7])
         ;; Could also use (do (cond ...)).
         (when (zerop (mod el first-var))
           (push-into msg-coll (format "Multiple of 2: %d" el)))
         (when (zerop (mod el second-var))
           (push-into msg-coll (format "Multiple of 3: %d" el)))
         (finally-return (string-join (nreverse msg-coll) "\n")))
  :loopy t)

;;;;; Unless
(loopy-deftest multi-unless-prepend-test
  :result "Not multiple of 2: 1
Not multiple of 3: 1
Not multiple of 3: 2
Not multiple of 2: 3
Not multiple of 3: 4
Not multiple of 2: 5
Not multiple of 3: 5
Not multiple of 2: 7
Not multiple of 3: 7"
  :body ((with (first-var 2)
               (second-var 3))
         (seq el [1 2 3 4 5 6 7])
         ;; Could also use (do (cond ...)).
         (unless (zerop (mod el first-var))
           (push-into msg-coll (format "Not multiple of 2: %d" el)))
         (unless (zerop (mod el second-var))
           (push-into msg-coll (format "Not multiple of 3: %d" el)))
         (finally-return (string-join (nreverse msg-coll) "\n")))
  :loopy t)

;;;;; Cond FORMS

(loopy-deftest cond
  :result '((4 2 0) (5 3 1) ((4 2 0) (2 0) (0)))
  :body ((list i (number-sequence 0 5))
         (cond ((cl-evenp i)
                (push-into evens i)
                (push-into holding-list evens))
               (t (push-into odds i)))
         (finally-return (list evens odds holding-list)))
  :loopy t)

;;;; Exiting the Loop Early
;;;;; Leave
(loopy-deftest leave
  :result '(1)
  :body ((list i '(1 2))
         (collect i)
         (_cmd))
  :repeat _cmd
  :loopy ((list . list)
          (collect . collect)
          (_cmd . (leave leaving)))
  :iter-keyword ((list . list)
                 (collect . collect)
                 (_cmd . (leave leaving)))
  :iter-bare ((list . listing )
              (collect . collecting)
              (_cmd . (leaving))))

;;;;; Leave From
(loopy-deftest leave-from-same
  :result '([1 2 3])
  :body (outer
         (list i '([1 2 3] [4 5 6]))
         (when (= (aref i 1) 5)
           (_cmd outer))
         (collect i))
  :repeat _cmd
  :loopy ((_cmd . (leave-from leaving-from)))
  :iter-keyword ((list . list)
                 (_cmd . (leave-from leaving-from))
                 (collect . collect))
  :iter-bare ((list . listing)
              (_cmd . (leaving-from))
              (collect . collecting)))

(loopy-deftest leave-from-inner-loopy
  :result '([1 2 3])
  :body (outer
         (list i '([1 2 3] [4 5 6]))
         (loopy-test-escape
          (loopy (array j i)
                 (when (= j 5)
                   (leave-from outer))))
         (collect i))
  :loopy t
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest leave-from-inner-loopy-iter
  :result '([1 2 3])
  :body (outer
         (list i '([1 2 3] [4 5 6]))
         (loopy-test-escape
          (loopy-iter (arraying j i)
                      (when (= j 5)
                        (leaving-from outer))))
         (collect i))
  :loopy t
  :iter-bare ((list . listing)
              (collect . collecting)))

;;;;; Return
(loopy-deftest return
  :result 6
  :body ((with  (j 0))
         (do (cl-incf j))
         (when (> j 5)
           (_cmd j)))
  :repeat _cmd
  :loopy ((_cmd . (return returning)))
  :iter-keyword ((_cmd . (return returning))
                 (do . do))
  :iter-bare ((_cmd . (returning))
              (do . progn)))

;;;;; Return From
(loopy-deftest return-from-single-loop
  :result 6
  :body (my-loop
         (list i (number-sequence 1 10))
         (when (> i 5)
           (_cmd my-loop i)))
  :repeat _cmd
  :loopy ((_cmd . (return-from returning-from)))
  :iter-keyword ((_cmd . (return-from returning-from))
                 (list . list))
  :iter-bare ((_cmd . (returning-from))
              (list . listing)))

(loopy-deftest return-from-outer-loop
  :result 6
  :multi-body t
  :body [( outer
           ;; Could use ‘sum’ command, but don’t want dependencies.
           (with (sum 0))
           (list sublist '((1 2 3 4 5) (6 7 8 9) (10 11)))
           (do (loopy-test-escape
                (loopy (list i sublist)
                       (do (setq sum (+ sum i)))
                       (when (> sum 15)
                         (return-from outer i))))))

         ( outer
           ;; Could use ‘sum’ command, but don’t want dependencies.
           (with (sum 0))
           (list sublist '((1 2 3 4 5) (6 7 8 9) (10 11)))
           (do (loopy-test-escape
                (loopy-iter (listing i sublist)
                            (setq sum (+ sum i))
                            (when (> sum 15)
                              (returning-from outer i))))))]
  :loopy t
  :iter-keyword (list do)
  :iter-bare ((list . listing)
              (do . progn)))

(loopy-deftest return-commands-multiple-values
  :result  '(1 2 3 4)
  :multi-body t
  :body [((return 1 2 3 4))
         (my-loop (return-from my-loop 1 2 3 4))]
  :loopy t
  :iter-keyword (return return-from)
  :iter-bare ((return . returning)
              (return-from . returning-from)))

;;;;; Skip
(loopy-deftest skip
  :result t
  :body ((seq i (number-sequence 1 10))
         (when (cl-evenp i)
           (_cmd))
         (collect my-collection i)
         (finally-return
          (cl-every #'cl-oddp my-collection)))
  :repeat _cmd
  :loopy ((_cmd . (skip skipping)))
  :iter-keyword ((_cmd . (skip skipping))
                 (seq . seq)
                 (collect . collect))
  :iter-bare ((skip . skipping)
              (seq . seqing)
              (collect . collecting)))

(loopy-deftest skip-from
  :result '((1 2 3) (7 8 9))
  :body ((named outer)
         (array i [(1 2 3) (4 5 6) (7 8 9)])
         (if (= 5 (cl-second i)) (_cmd outer))
         (collect i))
  :repeat _cmd
  :loopy ((_cmd . (skip-from skipping-from)))
  :iter-keyword ((array . array)
                 (collect . collect)
                 (_cmd . (skip-from skipping-from)))
  :iter-bare ((array . arraying)
              (collect . collecting)
              (_cmd . (skipping-from))))

(loopy-deftest skip-from-inner-loopy
  :result '((1 2 3) (7 8 9))
  :body ((named outer)
         (array i [(1 2 3) (4 5 6) (7 8 9)])
         (loopy-test-escape
          (loopy (list j i)
                 (if (= 5 j)
                     (skip-from outer))))
         (collect i))
  :loopy t
  :iter-bare ((array . arraying)
              (collect . collecting)))

(loopy-deftest skip-from-inner-loopy-iter
  :result '((1 2 3) (7 8 9))
  :body ((named outer)
         (array i [(1 2 3) (4 5 6) (7 8 9)])
         (loopy-test-escape
          (loopy-iter (listing j i)
                      (if (= 5 j)
                          (skipping-from outer))))
         (collect i))
  :loopy t
  :iter-bare ((array . arraying)
              (collect . collecting)))

;;;;; While
(loopy-deftest while
  :result '(1 2)
  :body ((list i '(1 2 3 4 5 6))
         (while (< i 3))
         (collect i))
  :loopy t
  :iter-keyword (list while collect)
  :iter-bare nil)

;;;;; Until
(loopy-deftest until
  :result '(1 2 3)
  :body ((list i '(1 2 3 4 5 6))
         (until (> i 3))
         (collect i))
  :loopy t
  :iter-keyword (list until collect)
  :iter-bare nil)


;;;;; Always
(loopy-deftest always-pass
  :result t
  :body ((list i '(1 2 3 4 5 6))
	 (always (< i 7)))
  :loopy t
  :iter-keyword (list always)
  :iter-bare ((list . listing)
              (always . always)))

(loopy-deftest always-fail
  :result nil
  :body ((list i '(8 9 10 12 0 13))
	 (always (> i 7)))
  :loopy t
  :iter-keyword (list always)
  :iter-bare ((list . listing)
              (always . always)))

(loopy-deftest always-multiple-commands
  :result t
  :body ((list i '(1 3 5 7))
         (always (cl-oddp i))
         (always (< i 10)))
  :loopy t
  :iter-keyword (list always)
  :iter-bare ((list . listing)
              (always . always)))

(loopy-deftest always-var
  :result 4
  :multi-body t
  :body [((list i '(1 2 3))
          (always (and i (numberp i) (1+ i)) :into test-var)
          (finally-return test-var))

         ((list i '(1 2 3))
          (always test-var (and i (numberp i) (1+ i)))
          (finally-return test-var))

         ((list i '(1 2 3))
          (always (and i (numberp i) (1+ i)))
          (finally-return loopy-result))]
  :loopy t
  :iter-keyword (list always)
  :iter-bare ((list . listing)
              (always . always)))

;;;;; Never
(loopy-deftest never-nil
  :result nil
  :body ((list i '(1 2 3 4 5 6))
	 (never (> i 0)))
  :loopy t
  :iter-keyword (list never)
  :iter-bare ((list . listing)
              (never . never)))

(loopy-deftest never-t
  :result t
  :body ((list i '(1 2 3 4 5 6))
	 (never (< i 0)))
  :loopy t
  :iter-keyword (list never)
  :iter-bare ((list . listing)
              (never . never)))

(loopy-deftest multiple-never
  :result t
  :body ((list i '(1 3 5 7))
         (never (cl-evenp i))
         (never (> i 10)))
  :loopy t
  :iter-keyword (list never)
  :iter-bare ((list . listing)
              (never . never)))

(loopy-deftest always-and-never
  :doc "A `never' command should not stop `always' from ultimately setting the return value to 2."
  :result 2
  :body ((repeat 2)
         (always 2)
         (never nil))
  :loopy t
  :iter-keyword (list never repeat)
  :iter-bare ((list . listing)
              (never . never)
              (repeat . repeating)))

(loopy-deftest never-var
  :result t
  :multi-body t
  :body [((list i '(1 2 3))
          (never (not (numberp i)) :into test-var)
          (finally-return test-var))

         ((list i '(1 2 3))
          (never test-var (not (numberp i)))
          (finally-return test-var))

         ((list i '(1 2 3))
          (never (not (numberp i)))
          (finally-return loopy-result))]
  :loopy t
  :iter-keyword (list never)
  :iter-bare ((list . listing)
              (never . never)))

;;;;; Thereis
(loopy-deftest thereis-val
  :doc "Make sure `thereis' sets the value correctly."
  :result 6
  :multi-body t
  :body [((list i '(1 2 3 4 5 6 7 8 9))
	  (thereis (and (> i 5) i)))

         ((list i '(1 2 3 4 5 6 7 8 9))
	  (thereis var (and (> i 5) i))
          (finally-return var))]
  :loopy t
  :iter-keyword (list thereis)
  :iter-bare ((list . listing)
              (thereis . thereis)))

(loopy-deftest thereis-pass
  :doc "Make sure `thereis' ends the loop early when the conditions is non-nil."
  :result '(6 (1 2 3 4 5 6))
  :multi-body t
  :body [((list i '(1 2 3 4 5 6 7 8 9))
          (collect coll i)
	  (thereis (and (> i 5) i))
          (finally-return loopy-result coll))

         ((list i '(1 2 3 4 5 6 7 8 9))
          (collect i)
	  (thereis var (and (> i 5) i))
          (finally-return var loopy-result))]
  :loopy t
  :iter-keyword (list thereis collect)
  :iter-bare ((list . listing)
              (thereis . thereis)
              (collect . collecting)))

(loopy-deftest thereis-fail
  :doc "Make sure `thereis' does not end the loop early and does not set the value."
  :result nil
  :multi-body t
  :body [((list i '(1 2 3 4 5 6))
	  (thereis (> i 7)))

         ((list i '(1 2 3 4 5 6))
	  (thereis var (> i 7))
          (finally-return var))

         ((list i '(1 2 3 4 5 6))
          (collect i)
	  (thereis var (> i 7))
          (finally-return (not (and (eq var nil)
                                    (equal loopy-result '(1 2 3 4 5 6))))))]
  :loopy t
  :iter-keyword (list thereis collect)
  :iter-bare ((list . listing)
              (thereis . thereis)
              (collect . collecting)))

(loopy-deftest thereis-always-same-var
  :error loopy-incompatible-accumulation-types
  :multi-body t
  :body [((list i '(1 2 3))
          (always i)
          (thereis i))
         ((list i '(1 2 3))
          (always i :into test)
          (thereis i :into test))
         ((list i '(1 2 3))
          (always  test i)
          (thereis test i))]
  :loopy t
  :iter-keyword (list thereis always)
  :iter-bare ((list . listing)))

(loopy-deftest thereis-never-same-var
  :error loopy-incompatible-accumulation-types
  :multi-body t
  :body  [((list i '(1 2 3))
           (never i)
           (thereis i))
          ((list i '(1 2 3))
           (never i :into test)
           (thereis i :into test))
          ((list i '(1 2 3))
           (never   test i)
           (thereis test i))]
  :loopy t
  :iter-keyword (list thereis never)
  :iter-bare ((list . listing)))

(loopy-deftest thereis-always-diff-var
  :result '(1 11)
  :body ((list i '(1 2 3))
         (always i :into test1)
         (thereis (+ i 10) :into test2))
  :loopy t
  :iter-keyword (list thereis always)
  :iter-bare ((list . listing)))

(loopy-deftest thereis-never-diff-var
  :result '(t 11)
  :body ((list i '(1 2 3))
         (never (not (numberp i)) :into test1)
         (thereis (+ i 10) :into test2))
  :loopy t
  :iter-keyword (list thereis never)
  :iter-bare ((list . listing)))

;;;;; finding
(loopy-deftest find-pass-notest
  :result 3
  :multi-body t
  :body [((list i '(1 2 3))
	  (_cmd res i (> i 2))
          (finally-return res))

         ((list i '(1 2 3))
	  (_cmd i (> i 2)))]
  :repeat _cmd
  :loopy ((_cmd . (find finding)))
  :iter-keyword ((list . list)
                 (_cmd . (find finding)))
  :iter-bare ((list . listing)
              (_cmd . (finding))))

(loopy-deftest find-fail-notest
  :result nil
  :multi-body t
  :body [((list i '(1 2 3))
	  (find res i (> i 4))
          (finally-return res))

         ((list i '(1 2 3))
	  (find i (> i 4)))]
  :loopy t
  :iter-keyword (list find)
  :iter-bare ((list . listing)
              (find . finding)))

(loopy-deftest find-onfail-at-beginning
  :doc "Make sure `:on-failure' is evaluated at the beginning."
  :result 27
  :multi-body t
  :body [((with (on-fail 27))
          (list i '(1 2 3))
          (set on-fail 33)
	  (find res i (> i 4) :on-failure on-fail)
          (finally-return res))

         ((with (on-fail 27))
          (list i '(1 2 3))
          (set on-fail 33)
	  (find i (> i 4) :on-failure on-fail))]
  :loopy t
  :iter-keyword (list set find)
  :iter-bare ((list . listing)
              (set . setting)
              (find . finding)))

(loopy-deftest find-fail-onfail
  :result 0
  :multi-body t
  :body [((list i '(1 2 3))
	  (find res i (> i 4) :on-failure 0)
          (finally-return res))

         ((list i '(1 2 3))
	  (find i (> i 4) :on-failure 0))]
  :loopy t
  :iter-keyword (list find)
  :iter-bare ((list . listing)
              (find . finding)))

(loopy-deftest find-pass-onfail
  :result 2
  :multi-body t
  :body [((list i '(1 2 3))
	  (find res i (> i 1) :on-failure 0)
          (finally-return res))

         ((list i '(1 2 3))
	  (find i (> i 1) :on-failure 0))]
  :loopy t
  :iter-keyword (list find)
  :iter-bare ((list . listing)
              (find . finding)))

(loopy-deftest find-expr-onfail-explicit-nil
  :doc "Make sure a nil value will still be set on failure."
  :result nil
  :multi-body t
  :body [((with (val 27))
          (list i '(1 2 3))
          (find val nil (> i 10) :on-failure nil)
          (finally-return val))

         ((with (loopy-result 27))
          (list i '(1 2 3))
          (find nil (> i 10) :on-failure nil))]
  :loopy t
  :iter-keyword (list find)
  :iter-bare ((list . listing)
              (find . finding)))

(loopy-deftest find-pass-var
  :result 2
  :multi-body t
  :body [((list i '(1 2 3))
          (find found i (= i 2))
          (finally-return found))

         ((list i '(1 2 3))
          (find i (= i 2)))]
  :loopy t
  :iter-keyword (list find)
  :iter-bare ((list . listing)
              (find . finding)))

(loopy-deftest find-fail-var
  :result nil
  :multi-body t
  :body [((list i '(1 2 3))
          (find found i (> i 3))
          (finally-return found))

         ((list i '(1 2 3))
          (find i (> i 3)))]
  :loopy t
  :iter-keyword (list find)
  :iter-bare ((list . listing)
              (find . finding)))

(loopy-deftest find-fail-var-onfail
  :result "not found"
  :multi-body t
  :body  [((list i '(1 2 3))
           (find whether-found i (> i 4) :on-failure "not found")
           (finally-return whether-found))

          ((list i '(1 2 3))
           (find i (> i 4) :on-failure "not found"))]
  :loopy t
  :iter-keyword (list find)
  :iter-bare ((list . listing)
              (find . finding)))

(loopy-deftest find-pass-var-onfail
  :result 2
  :multi-body t
  :body  [((list i '(1 2 3))
           (find whether-found i (> i 1)
                 :on-failure "not found")
           (finally-return whether-found))

          ((list i '(1 2 3))
           (find i (> i 1) :on-failure "not found"))]
  :loopy t
  :iter-keyword (list find)
  :iter-bare ((list . listing)
              (find . finding)))

(loopy-deftest find-expr-is-nil-onfail
  :doc "Make sure a nil value not assumed to be a failure."
  :result nil
  :multi-body t
  :body [((list i '(1 2 3))
          (find whether-found nil (> i 1)
                :on-failure "not found")
          (finally-return whether-found))

         ((list i '(1 2 3))
          (find nil (> i 1) :on-failure "not found"))]
  :loopy t
  :iter-keyword (list find)
  :iter-bare ((list . listing)
              (find . finding)))

;;; Custom Commands
(loopy-deftest custom-command-sum
  :doc "Wrapping with another eval to make sure variables are set by expansion time."
  :wrap ((x . `(cl-labels ((my-loopy-sum-command ((_ target &rest items))
                             "Set TARGET to the sum of ITEMS."
                             `((loopy--iteration-vars (,target nil))
                               (loopy--main-body (setq ,target (apply #'+ (list ,@items)))))))
                 (let ((loopy-command-parsers
                        (map-insert loopy-command-parsers 'target-sum
                                    #'my-loopy-sum-command))
                       (loopy-iter-bare-commands (cons 'target-sum
                                                       loopy-iter-bare-commands)))
                   (eval (quote ,x) t)))))
  :result 6
  :body ((target-sum my-target 1 2 3)
         (return nil)
         (finally-return my-target))
  :loopy t
  :iter-keyword (target-sum return)
  :iter-bare ((return . returning)))

(loopy-deftest custom-command-always-pass
  :doc "Wrapping with another eval to make sure variables are set by expansion time.
Also tests that post-conditions work as expected."
  :wrap ((x . `(cl-labels ((my--loopy-always-command-parser ((_ &rest conditions))
                             "Parse a command of the form `(my-always [CONDITIONS])'.
If any condition is `nil', `loopy' should immediately return nil.
Otherwise, `loopy' should return t."
                             ;; Return t if loop completes successfully.
                             `((loopy--after-do (cl-return t))
                               ;; Check all conditions at the end of the loop
                               ;; body, forcing an exit if any evaluate to nil.
                               ;; Since the default return value of the macro is
                               ;; nil, we don’t need to do anything else.
                               ;;
                               ;; NOTE: We must not add anything to
                               ;;       `loopy--final-return', since that would
                               ;;       override the value of any early returns.
                               ,@(cl-loop
                                  for condition in conditions
                                  collect `(loopy--post-conditions ,condition)))))
                 (let ((loopy-command-parsers
                        (map-insert loopy-command-parsers 'my-always
                                    #'my--loopy-always-command-parser))
                       (loopy-iter-bare-commands (cons 'my-always
                                                       loopy-iter-bare-commands)))
                   (eval (quote ,x) t)))))
  :result t
  :body ((list i (number-sequence 1 9))
         (my-always (< i 10) (< i 20)))
  :loopy t
  :iter-keyword (list my-always)
  :iter-bare ((list . listing)
              (my-always . my-always)))

(loopy-deftest custom-command-always-fail
  :doc "Wrapping with another eval to make sure variables are set by expansion time.
Also tests that post-conditions work as expected."
  :wrap ((x . `(cl-labels ((my--loopy-always-command-parser ((_ &rest conditions))
                             "Parse a command of the form `(my-always [CONDITIONS])'.
If any condition is `nil', `loopy' should immediately return nil.
Otherwise, `loopy' should return t."
                             ;; Return t if loop completes successfully.
                             `((loopy--after-do (cl-return t))
                               ;; Check all conditions at the end of the loop
                               ;; body, forcing an exit if any evaluate to nil.
                               ;; Since the default return value of the macro is
                               ;; nil, we don’t need to do anything else.
                               ;;
                               ;; NOTE: We must not add anything to
                               ;;       `loopy--final-return', since that would
                               ;;       override the value of any early returns.
                               ,@(cl-loop
                                  for condition in conditions
                                  collect `(loopy--post-conditions ,condition)))))
                 (let ((loopy-command-parsers
                        (map-insert loopy-command-parsers 'my-always
                                    #'my--loopy-always-command-parser))
                       (loopy-iter-bare-commands (cons 'my-always
                                                       loopy-iter-bare-commands)))
                   (eval (quote ,x) t)))))
  :result nil
  :body ((list i (number-sequence 1 9))
         (list j '(2 4 6 8 9))
         (my-always (< i 10) (cl-evenp j)))
  :loopy t
  :iter-keyword (list my-always)
  :iter-bare ((list . listing)
              (my-always . my-always)))

;;; Repeated evaluation of macro

;; This was an odd case reported by a user. See:
;; https://github.com/okamsn/loopy/issues/17
(ert-deftest evaluate-function-twice ()
  ;; Emacs 27 had a byte-compilation error that was fixed in
  ;; commit a0f60293d79cda858c033db4ae074e5e5560aab2.
  ;; See: https://git.savannah.gnu.org/cgit/emacs.git/commit/?id=a0f60293d97cda858c033db4ae074e5e5560aab2.
  :expected-result (static-if (= emacs-major-version 27)
                       :failed
                     :passed)
  (should
   (progn
     (defun mu4e:other-path ()
       "Return load-path for mu4e.
This assumes that you're on guix."
       (with-suppressed-warnings ((unused . (regexp base-dir)))
         (loopy (with (regexp "Documents")
	              (base-dir (expand-file-name "~/")))
	        (list file (directory-files base-dir))
	        (set full-path (expand-file-name file base-dir)))))
     (mu4e:other-path)
     ;; If an `nreverse' goes bad, then the function value of `mu4e:other-path'
     ;; might be changed (somehow), which causes an error.
     (eq nil (mu4e:other-path)))))

(loopy-deftest evaluate-function-twice-2
  :doc "Not sure if using `cl-labels' woudl prevent the error, so keep original test."
  :result t
  :wrap ((x . `(cl-labels ((mu4e:other-path ()
                             (with-suppressed-warnings
                                 ((unused . (regexp base-dir)))
                               ,x)))
                 (mu4e:other-path)
                 ;; If an `nreverse' goes bad, then the function value of
                 ;; `mu4e:other-path' might be changed (somehow), which causes an
                 ;; error.
                 (eq nil (mu4e:other-path)))))
  :body ((with (regexp "Documents")
	       (base-dir (expand-file-name "~/")))
	 (list file (directory-files base-dir))
	 (set full-path (expand-file-name file base-dir)))
  :loopy t
  :iter-keyword (list set)
  :iter-bare ((list . listing)
              (set . setting)))

;;; Custom Aliases
(loopy-deftest custom-alias-flag
  :doc "Test with `default' flag, which is essentially a no-op."
  :result '(1)
  :wrap ((x . `(let ((loopy-aliases (map-copy loopy-aliases))
                     (loopy-iter-bare-special-macro-arguments
                      (cons 'f loopy-iter-bare-special-macro-arguments)))
                 (loopy-defalias f flag)
                 (eval (quote ,x) t))))
  :body ((f default)
         (list i '(1))
         (collect i))
  :loopy t
  :iter-keyword (list collect f)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest custom-alias-with
  :result 1
  :wrap ((x . `(let ((loopy-aliases (map-copy loopy-aliases))
                     (loopy-iter-bare-special-macro-arguments
                      (cons 'as loopy-iter-bare-special-macro-arguments)))
                 (loopy-defalias as with)
                 (eval (quote ,x) t))))
  :body ((as (a 1))
         (return a))
  :loopy t
  :iter-keyword (as return)
  :iter-bare ((as . as)
              (return . returning)))

(loopy-deftest custom-alias-without
  :result 5
  :wrap ((x . `(let ((loopy-aliases (map-copy loopy-aliases))
                     (loopy-iter-bare-special-macro-arguments
                      (cons 'ignore loopy-iter-bare-special-macro-arguments)))
                 (loopy-defalias ignore without)
                 (eval  (quote (let ((a 1)
                                     (b 2))
                                 ,x
                                 (+ a b)))
                        t))))
  :body ((ignore a b)
         (repeat 1)
         (set a 2)
         (set b 3))
  :loopy t
  :iter-keyword (ignore repeat set)
  :iter-bare ((ignore . ignore)
              (repeat . repeating)
              (set . setting)))

(loopy-deftest custom-alias-before-do
  :result 7
  :wrap ((x . `(let ((loopy-aliases (map-copy loopy-aliases))
                     (loopy-iter-bare-special-macro-arguments
                      (cons 'precode loopy-iter-bare-special-macro-arguments)))
                 (loopy-defalias precode before-do)
                 (eval (quote ,x) t))))
  :body ((with (i 2))
         (precode (setq i 7))
         (return i))
  :loopy t
  :iter-keyword (precode return)
  :iter-bare ((precode . precode)
              (return . returning)))

(loopy-deftest custom-alias-after-do
  :result t
  :wrap ((x . `(let ((loopy-aliases (map-copy loopy-aliases))
                     (loopy-iter-bare-special-macro-arguments
                      (cons 'postcode loopy-iter-bare-special-macro-arguments)))
                 (loopy-defalias postcode after-do)
                 (eval (quote ,x) t))))
  :body ((with (my-ret nil))
         (list i '(1 2 3 4))
         (postcode (setq my-ret t))
         (finally-return my-ret))
  :loopy t
  :iter-keyword (list postcode)
  :iter-bare ((postcode . postcode)
              (list . listing)))

(loopy-deftest custom-alias-finally-do
  :result 10
  :wrap ((x . `(let ((loopy-aliases (map-copy loopy-aliases))
                     (loopy-iter-bare-special-macro-arguments
                      (cons 'fd loopy-iter-bare-special-macro-arguments)))
                 (loopy-defalias fd finally-do)
                 (eval (quote (let (my-var)
                                ,x
                                my-var))
                       t))))
  :body ((list i (number-sequence 1 10))
         (fd (setq my-var i)))
  :loopy t
  :iter-keyword (list fd)
  :iter-bare ((fd . fd)
              (list . listing)))

(loopy-deftest custom-alias-finally-return
  :result 10
  :wrap ((x . `(let ((loopy-aliases (map-copy loopy-aliases))
                     (loopy-iter-bare-special-macro-arguments
                      (cons 'fr loopy-iter-bare-special-macro-arguments)))
                 (loopy-defalias fr finally-return)
                 (eval (quote ,x)
                       t))))
  :body ((list i (number-sequence 1 10))
         (fr i))
  :loopy t
  :iter-keyword (list fr)
  :iter-bare ((fr . fr)
              (list . listing)))

(loopy-deftest custom-alias-list-array
  :result '((1 . 4) (2 . 5) (3 . 6))
  :wrap ((x . `(let ((loopy-aliases (map-copy loopy-aliases))
                     (loopy-iter-bare-commands
                      (append (list 'l 'a) loopy-iter-bare-commands)))
                 (loopy-defalias l list)
                 (loopy-defalias a 'array)
                 (eval (quote ,x)
                       t))))
  :body ((l i '(1 2 3))
         (a j [4 5 6])
         (collect (cons i j)))
  :loopy t
  :iter-keyword (l a collect)
  :iter-bare ((l . l)
              (a . a)
              (collect . collecting)))


;;; Clean Up Variables
(loopy-deftest clean-stack-variables
  :result nil
  :wrap ((x . `(let (loopy--known-loop-names
                     loopy--accumulation-places
                     loopy--at-instructions
                     loopy--accumulation-list-end-vars
                     loopy--accumulation-variable-info)
                 (eval (quote ,x) t)
                 (or loopy--known-loop-names
                     loopy--accumulation-places
                     loopy--at-instructions
                     loopy--accumulation-list-end-vars
                     loopy--accumulation-variable-info))))
  :multi-body t
  :body [(my-loop (array i [(1 2) (3 4)])
                  (collect i :at start)
                  (loopy-test-escape
                   (loopy inner
                          (list j i)
                          (at my-loop (collect j :at end)))))

         (my-loop (array i [(1 2) (3 4)])
                  (collect i :at start)
                  (loopy-test-escape
                   (loopy-iter inner
                               (listing j i)
                               (at my-loop (collecting j :at end)))))]
  :loopy t
  :iter-keyword (array collect)
  :iter-bare ((array . arraying)
              (collect . collecting)))

(loopy-deftest clean-var-variables-1
  :result 'good
  :wrap ((x . `(let ((i 'good)) ,x i)))
  :body ((list i '(1 2 3)))
  :loopy t
  :iter-keyword (list)
  :iter-bare ((list . listing)))

(loopy-deftest clean-var-variables-2
  :result 'good
  :wrap ((x . `(let ((i 'good)) ,x i)))
  :body ((cycle 1)
         (set i 'bad))
  :loopy t
  :iter-keyword (list cycle)
  :iter-bare ((list . listing)
              (cycle . cycling)))

(loopy-deftest accum-flet-outside
  :doc "Make sure that macro expansion doesn't mess with `cl-flet' environment.
We don't want to rebind the environment to nil by failing to pass
the existing environment (`macroexpand-all-environment') to
`macroexpand-all'."
  :result '(11 12 13 14 15)
  :wrap ((x . `(cl-flet ((10+ (y) (+ 10 y))) ,x)))
  :body ((list i '(1 2 3 4 5))
         (collect (10+ i)))
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest accum-flet-outside-wrap-sma
  :doc "Make sure that macro expansion doesn't mess with `cl-flet' environment.
We don't want to rebind the environment to nil by failing to pass
the existing environment (`macroexpand-all-environment') to
`macroexpand-all'."
  :result '(11 12 13 14 15)
  :body ((wrap (cl-flet ((10+ (y) (+ 10 y)))))
         (list i '(1 2 3 4 5))
         (collect (10+ i)))
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest flet-iter-subloop
  :doc "Make sure that macro expansion doesn't mess with `cl-flet' environment.
We don't want to rebind the environment to nil by failing to pass
the existing environment (`macroexpand-all-environment') to
`macroexpand-all'."
  :result '(11 12 13 14 15)
  :multi-body t
  :body [((named outer)
          (list i '((1 2) (3 4) (5)))
          (loopy-test-escape
           (loopy-iter (listing j i)
                       (at outer
                           (cl-flet ((10+ (y) (+ 10 y)))
                             (collecting (10+ j)))))))

         ((named outer)
          (list i '((1 2) (3 4) (5)))
          (loopy-test-escape
           (loopy-iter (listing j i)
                       (at outer
                           (cl-flet ((10+ (y) (+ 10 y)))
                             (collecting (funcall #'10+ j)))))))

         ((named outer)
          (list i '((1 2) (3 4) (5)))
          (loopy-test-escape
           (loopy-iter (listing j i)
                       (cl-flet ((10+ (y) (+ 10 y)))
                         (at outer
                             (collecting (10+ j)))))))

         ((named outer)
          (list i '((1 2) (3 4) (5)))
          (loopy-test-escape
           (loopy-iter (listing j i)
                       (cl-flet ((10+ (y) (+ 10 y)))
                         (at outer
                             (collecting (10+ j)))))))

         ((named outer)
          (list i '((1 2) (3 4) (5)))
          (loopy-test-escape
           (loopy-iter (listing j i)
                       (at outer
                           (cl-flet ((10+ (y) (+ 10 y)))
                             (collecting (funcall #'10+ j)))))))]
  :loopy t
  :iter-keyword (list collect)
  :iter-bare ((list . listing)
              (collect . collecting)))

(loopy-deftest iter-list-in-top-level-expr
  :doc "Macros that are required to be at the top level should not consider
a sub-expression as the top level."
  :error loopy-iteration-in-sub-level
  :macroexpand t
  :body ((let ((var 1))
           (listing i '(1 2 3 4 5)))
         (collecting i))
  :iter-keyword (listing collecting)
  :iter-bare t)

;; Local Variables:
;; End:
;; LocalWords:  destructurings backquote
