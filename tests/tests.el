;; -*- lexical-binding: t; -*-

;; Run these tests using:
;; emacs -Q --batch -l ert -l tests.el -f ert-run-tests-batch-and-exit

(push (expand-file-name ".")
      load-path)

(require 'cl-lib)
(require 'map)
(require 'ert)
(require 'generator)
(require 'pcase)
(require 'map "./dependecy-links/map.el" 'no-error)
(eval-when-compile (require 'loopy "./loopy.el")
                   (require 'loopy-iter "./loopy-iter.el"))
(require 'loopy "./loopy.el")
(require 'loopy-vars "./loopy-vars.el")
(require 'loopy-commands "./loopy-commands.el")
(require 'loopy-iter "./loopy-iter.el")

(push (list "Loopy Tests"
            (rx (0+ blank) "(loopy-deftest" (0+ blank)
                (group-n 1 (1+ (or word (syntax symbol)))))
            1)
      imenu-generic-expression)

;; "loopy quote"
(defmacro lq (&rest body)
  "`loopy' quote: Quote a use of `loopy'."
  `(eval (quote (loopy ,@body)) t))

(defmacro loopy-test-structure (input output-pattern)
  "Use `pcase' to check a destructurings bindings.
INPUT is the destructuring usage.  OUTPUT-PATTERN is what to match."
  `(pcase ,input
     (,output-pattern
      t)
     (_ nil)))

;;; Check for ELC files, which can mess up testing.
(ert-deftest no-elc-in-cwd ()
  (should (cl-loop for f in (directory-files ".")
                   never (string-match-p "\\.elc\\'" f))))

(cl-defmacro loopy-deftest
    ( name args
      &key doc repeat body multi-body
      repeat-loopy repeat-iter-bare repeat-iter-keyword
      wrap
      macroexpand
      (loopy nil loopy-provided)
      (iter-bare nil iter-bare-provided)
      (iter-keyword nil iter-keyword-provided)
      (result nil result-provided)
      (error nil error-provided))
  "Create test for `loopy' and `loopy-iter'.

- NAME is the name of the test.

- ARGS is the list of test arguments.

- BODY is the test.

- MULTI-BODY means there are multiple bodies in BODY.

- LOOPY are the `loopy' names.

- ITER-BARE are the `loopy-iter' names.

- ITER-KEYWORD are the `loopy-iter' names after keywords.
  This can also be a simple list of command names.

- RESULT is compared using `equal' and `should'.

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
  E.g., (x . (backquote (let ((a 2)) ,x))).

- If MACROEXPAND is non-nil, then we test using `macroexpand'
  instead of `eval'.

LOOPY and ITER-BARE can be `t' instead of an alist, which will
run those tests without substitution.  If ITER-KEYWORD is `t', we
prefix the items in LOOPY or ITER-BARE."
  (declare (indent 2))

  (unless (or result-provided error-provided)
    (error "Must include `result' or `error'"))
  (unless (or loopy iter-bare iter-keyword)
    (error "Must include `loopy' or `iter-bare'"))
  (unless body
    (error "Must include `body'"))

  (when (eq loopy t) (setq loopy nil))

  (when (eq iter-bare t) (setq iter-bare nil))

  (cond ((eq iter-keyword t)
         (setq iter-keyword (or loopy iter-bare)))

        ((symbolp (car iter-keyword))
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
       (output-wrap (x) (cond (result-provided `(should (equal ,result ,x)))
                              (error-provided  `(should-error ,x :type
                                                              (quote ,error)))))
       ;; Replace given placeholder command names with actual names,
       ;; maybe including the `for' keyword for `loopy-iter'.
       (translate (group-alist this-body &optional keyword)
                  (mapcar (lambda (sexp)
                            (pcase sexp
                              (`(,first . ,rest)
                               (let ((new-rest (translate group-alist rest keyword))
                                     (new-first (map-elt group-alist first)))
                                 (if new-first
                                     (if keyword
                                         `(for ,new-first ,@new-rest)
                                       `(,new-first ,@new-rest))
                                   `(,first ,@new-rest))))
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

;;; Macro arguments
;;;; Named (loop Name)

(loopy-deftest named ()
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

;; (ert-deftest named ()
;;   (should (= 4 (loopy my-loop (return-from my-loop 4))))
;;   (should (= 4 (loopy (named my-loop) (return-from my-loop 4))))
;;   (should (equal '(4) (loopy my-loop (collect 4) (leave-from my-loop))))
;;   (should (equal '(4) (loopy (named my-loop) (collect 4) (leave-from my-loop)))))

;;;; With
(loopy-deftest with-arg-order ()
  :result 4
  :body ((_with (a 2) (b (+ a 2)))
         (_return b))
  :loopy ((_with . (with let* init))
          (_return . return))
  :iter-bare ((_with . (with init))
              (_return . returning))
  :iter-keyword t
  :repeat _with)

;; (ert-deftest with-arg-order ()
;;   (should (= 4
;;              (eval (quote (loopy (with (a 2)
;;                                        (b (+ a 2)))
;;                                  (return b))))))
;;
;;   (should (= 4
;;              (eval (quote (loopy (let* (a 2)
;;                                    (b (+ a 2)))
;;                                  (return b))))))
;;
;;   (should (= 4
;;              (eval (quote (loopy (init (a 2)
;;                                        (b (+ a 2)))
;;                                  (return b)))))))

(loopy-deftest with-destructuring ()
  :result -2
  :body ((with ((a b) '(1 2))
               ([c d] `[,(1+ a) ,(1+ b)]))
         (return (+ (- a b)
                    (- c d))))
  :loopy t
  :iter-bare ((return . returning)))

;; (ert-deftest with-destructuring ()
;;   (should (= -2
;;              (eval (quote (loopy (with ((a b) '(1 2))
;;                                        ([c d] `[,(1+ a) ,(1+ b)]))
;;                                  (return (+ (- a b)
;;                                             (- c d)))))))))

;;;; Without
(loopy-deftest without ()
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

;; (ert-deftest without ()
;;   (should (equal '(4 5)
;;                  (eval (quote (let ((a 1) (b 2))
;;                                 (loopy (with (c 3))
;;                                        (without a b)
;;                                        (expr a (+ a c))
;;                                        (expr b (+ b c))
;;                                        (return a b)))))))
;;
;;   (should (equal '(4 5)
;;                  (eval (quote (let ((a 1) (b 2))
;;                                 (loopy (with (c 3))
;;                                        (no-init a b)
;;                                        (expr a (+ a c))
;;                                        (expr b (+ b c))
;;                                        (return a b)))))))
;;
;;   (should (equal '(4 5)
;;                  (eval (quote (let ((a 1) (b 2))
;;                                 (loopy (with (c 3))
;;                                        (no-with a b)
;;                                        (expr a (+ a c))
;;                                        (expr b (+ b c))
;;                                        (return a b))))))))

;;;; Before Do
;; `before-do' always runs, and occurs before the loop.
(loopy-deftest basic-before-do ()
  :result 4
  :body ((with (i 3))
         (_before (setq i (1+ i)))
         (return i))
  :loopy ((_before . (before-do before initially-do initially)))
  :loopy ((_before . (before-do before initially-do initially))
          (return . returning))
  :repeat _before)

;; (ert-deftest basic-before-do ()
;;   (should (and (= 4
;;                   (eval (quote (loopy (with (i 3))
;;                                       (before-do (setq i (1+ i)))
;;                                       (return i)))))
;;                (= 4
;;                   (eval (quote (loopy (with (i 3))
;;                                       (before (setq i (1+ i)))
;;                                       (return i)))))
;;                (= 4
;;                   (eval (quote (loopy (with (i 3))
;;                                       (initially-do (setq i (1+ i)))
;;                                       (return i)))))
;;                (= 4
;;                   (eval (quote (loopy (with (i 3))
;;                                       (initially (setq i (1+ i)))
;;                                       (return i))))))))

;;;; After Do - runs after loop is loop completed successfully
(loopy-deftest basic-after-do-does-run ()
  :result t
  :body ((with (my-ret nil))
         (list i '(1 2 3 4))
         (after-do (setq my-ret t))
         (finally-return my-ret))
  :loopy t
  :iter-bare ((list . listing)))

(loopy-deftest basic-after-does-not-run ()
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

;; (ert-deftest basic-after-do ()
;;   (should (and (eq t (eval (quote (loopy (with (my-ret nil))
;;                                          (list i '(1 2 3 4))
;;                                          (after-do (setq my-ret t))
;;                                          (finally-return my-ret)))))
;;                (eq nil (eval (quote (loopy (with (my-ret nil))
;;                                            (list i '(1 2 3 4))
;;                                            (return nil)
;;                                            (after-do (setq my-ret t))
;;                                            (finally-return my-ret)))))
;;                (eq nil (eval (quote (loopy (with (my-ret nil))
;;                                            (list i '(1 2 3 4))
;;                                            (return nil)
;;                                            (after (setq my-ret t))
;;                                            (finally-return my-ret)))))
;;                (eq nil (eval (quote (loopy (with (my-ret nil))
;;                                            (list i '(1 2 3 4))
;;                                            (return nil)
;;                                            (else-do (setq my-ret t))
;;                                            (finally-return my-ret)))))
;;                (eq nil (eval (quote (loopy (with (my-ret nil))
;;                                            (list i '(1 2 3 4))
;;                                            (return nil)
;;                                            (else (setq my-ret t))
;;                                            (finally-return my-ret))))))))

;;;; Before and After
(loopy-deftest basic-before-and-after-test ()
  :result 3
  :body ((with (i 1))
         (before-do (cl-incf i))
         (cycle 1)
         (after-do (cl-incf i))
         (finally-return i))
  :loopy t
  :iter-bare ((cycle . cycling))
  :iter-keyword ((cycle . cycle)))

;; (ert-deftest basic-before-and-after-test ()
;;   (should (= 3 (eval (quote (loopy (with (i 1))
;;                                    (before-do (cl-incf i))
;;                                    (repeat 1)
;;                                    (after-do (cl-incf i))
;;                                    (finally-return i)))))))

;;;; Wrap
(loopy-deftest wrap ()
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

(loopy-deftest wrap-order ()
  ;; Test order things wrapped in.
  :result 3
  :body ((wrap (let ((a 1)))
               (let ((b (1+ a)))))
         (return (+ a b)))
  :loopy t
  :iter-bare ((return . returning))
  :iter-keyword ((return . return)))

(loopy-deftest wrap-not-linger ()
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

;; (ert-deftest wrap ()
;;   ;; Test saving match data
;;   (should
;;    (save-match-data
;;      (let ((original-data (set-match-data nil)))
;;        (equal original-data
;;               (eval (quote (loopy (wrap save-match-data)
;;                                   (repeat 1)
;;                                   (do (string-match (make-string 100 ?a)
;;                                                     (make-string 100 ?a)))
;;                                   (finally-return (match-data)))))))))
;;
;;   ;; Test order things wrapped in.
;;   (should (= 3 (eval (quote (loopy (wrap (let ((a 1)))
;;                                          (let ((b (1+ a)))))
;;                                    (return (+ a b)))))))
;;
;;   ;; Ensure wrapping effects don't linger.
;;   (should-not
;;    (save-match-data
;;      (let ((original-data (set-match-data nil)))
;;        (equal original-data
;;               (eval (quote (loopy (cycle 1)
;;                                   (do (string-match (make-string 100 ?a)
;;                                                     (make-string 100 ?a)))
;;                                   (finally-return (match-data))))))))))

;;;; Final Instructions
(loopy-deftest finally-do ()
  :result 10
  :wrap ((x . `(let ((my-var)) ,x my-var)))
  :body ((_list i (number-sequence 1 10))
         (finally-do (setq my-var i)))
  :loopy ((_list . list))
  :iter-bare ((_list . listing))
  :iter-keyword ((_list . list)))

;; (ert-deftest finally-do ()
;;   (should (and (= 10
;;                   (let ((my-var))
;;                     (loopy (list i (number-sequence 1 10))
;;                            (finally-do (setq my-var i)))
;;                     my-var))
;;                (= 10
;;                   (let ((my-var))
;;                     (loopy (list i (number-sequence 1 10))
;;                            (finally (setq my-var i)))
;;                     my-var)))))

(loopy-deftest finally-do-not-affect-return ()
  :result nil
  :body ((_list i (number-sequence 1 10))
         (finally-do 3))
  :loopy ((_list . list))
  :iter-bare ((_list . listing))
  :iter-keyword ((_list . list)))

(loopy-deftest finally-return-single-value ()
  :result 10
  :body ((_list i (number-sequence 1 10))
         (finally-return i))
  :loopy ((_list . list))
  :iter-bare ((_list . listing))
  :iter-keyword ((_list . list)))

(loopy-deftest finally-return-list-of-values ()
  :result '(10 7)
  :body ((_list i (number-sequence 1 10))
         (finally-return i 7))
  :loopy ((_list . list))
  :iter-bare ((_list . listing))
  :iter-keyword ((_list . list)))

;;;; Finally Protect
(loopy-deftest finally-protect ()
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
(loopy-deftest change-order-of-commands ()
  :result 7
  :body ((list i '(1 2 3))
         (finally-return (+ i a))
         (with (a 4)))
  :loopy t
  :iter-bare ((list . listing))
  :iter-keyword (list))

;;;; Default return values.
(loopy-deftest default-return-nil ()
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
                   (loopy-deftest ,(intern (concat "optimized-named-vars-adjoin" suffix)) ()
                     :result '(1 2 3)
                     :body ((accum-opt ,var)
                            (array i [1 2 3])
                            (adjoin coll i)
                            (finally-return coll))
                     :loopy t
                     :iter-bare ((array . arraying)
                                 (adjoin . adjoining))
                     :iter-keyword (array adjoin))

                   (loopy-deftest ,(intern (concat "optimized-named-vars-collect" suffix)) ()
                     :result '(1 2 3)
                     :body ((accum-opt ,var)
                            (array i [1 2 3])
                            (collect coll i)
                            (finally-return coll))
                     :loopy t
                     :iter-bare ((array . arraying)
                                 (collect . collecting))
                     :iter-keyword (array collect))

                   (loopy-deftest ,(intern (concat "optimized-named-vars-append" suffix)) ()
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

                   (loopy-deftest ,(intern (concat "optimized-named-vars-nconc" suffix)) ()
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

                   (loopy-deftest ,(intern (concat "optimized-named-vars-union" suffix)) ()
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

                   (loopy-deftest ,(intern (concat "optimized-named-vars-nunion" suffix)) ()
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

                   (loopy-deftest ,(intern (concat "optimized-named-vars-concat" suffix)) ()
                     :result "abcdef"
                     :body ((accum-opt ,var)
                            (array i ["ab" "cd" "ef"])
                            (concat coll i)
                            (finally-return coll))
                     :loopy t
                     :iter-bare ((array . arraying)
                                 (concat . concating))
                     :iter-keyword (array concat))

                   (loopy-deftest ,(intern (concat "optimized-named-vars-vconcat" suffix)) ()
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
;;;;; At and sub-loop
;; NOTE: `sub-loop' is deprecated.
(ert-deftest sub-loop-implicit-accum-in-loop ()
  (should (equal '((1 . 4) (1 . 5) (2 . 4) (2 . 5))
                 (lq outer
                     (list i '(1 2))
                     (loop (list j '(4 5))
                           (at outer (collect (cons i j)))))))

  (should (equal "14152425"
                 (lq (named outer)
                     (list i '("1" "2"))
                     (loop (list j '("4" "5"))
                           (at outer (concat (concat i j)))))))

  (should (equal '(0 (1 . 4) (1 . 5) (2 . 4) (2 . 5))
                 (lq outer
                     (list i '(1 2))
                     (loop (list j '(4 5))
                           (at outer (collect (cons i j))))
                     (finally-return (cons 0 loopy-result))))))

(ert-deftest sub-loop-explicit-accum-in-loop ()
  (should (equal '(0 (1 . 4) (1 . 5) (2 . 4) (2 . 5))
                 (lq outer
                     (list i '(1 2))
                     (loop (list j '(4 5))
                           (at outer (collect my-coll (cons i j))))
                     (finally-return (cons 0 my-coll)))))

  (should (equal "014152425"
                 (lq (named outer)
                     (list i '("1" "2"))
                     (loop (list j '("4" "5"))
                           (at outer (concat my-str (concat i j))))
                     (finally-return (concat "0" my-str))))))
;;
(ert-deftest sub-loop-leave-early ()
  "A `leave' in a sub-loop should not affect the outer loop."
  (should (equal '(1 2 3)
                 (lq outer
                     (list i '(1 2 3))
                     (loop (list j '(4 5 6))
                           (leave)
                           (at outer (collect j)))
                     (collect i)))))

(ert-deftest sub-loop-skip ()
  "A `skip' in a sub-loop should not affect the outer loop."
  (should (equal '(5 7 1 5 7 2 5 7 3)
                 (lq  outer
                      (list i '(1 2 3))
                      (loop (list j '(4 5 6 7 8))
                            (when (cl-evenp j)
                              (continue))
                            (at outer (collect j)))
                      (collect i)))))

(ert-deftest sub-loop-return-from-outer ()
  (should (= 3 (lq (named outer)
                   (list i '(1 2 3))
                   (loop (list j '(4 5 6 3))
                         (when (= j i)
                           (return-from outer j)))))))

(ert-deftest sub-loop-named ()
  (should
   (equal
    '((3 5) (3 5))
    (lq outer
        (repeat 2)
        (loop inner1
              (list j '(3 4))
              (loop (list k '(5 6 7))
                    (if (= k 6)
                        ;; Return from inner1 so never reach 4.
                        (return-from inner1)
                      (at outer (collect (list j k))))))))))

;;;;; loopy command
;; NOTE: This duplicates the tests from the `sub-loop' command, which will be
;;       removed.

(loopy-deftest same-level-at-accum ()
  :result '(1 2 3 4)
  :body (outer
         (list i '(1 2 3 4))
         (at outer (collect i)))
  :loopy t
  :iter-bare ((list . listing)
              (collect . collecting))
  :iter-keyword (list collect at))

(loopy-deftest loopy-at-accum ()
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

(loopy-deftest loopy-at-leave ()
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

(loopy-deftest at-disagreeing-accum-types ()
  :error loopy-incompatible-accumulations
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

(loopy-deftest loopy-cmd-implicit-accum-in-loop ()
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

(loopy-deftest loopy-cmd-leave-early ()
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

(loopy-deftest loopy-cmd-skip ()
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

(loopy-deftest loopy-cmd-return-from-outer ()
  :result 3
  :body (outer
         (_list i '(1 2 3))
         (loopy (list j '(4 5 6 3))
                (when (= j i)
                  (return-from outer j))))
  :loopy ((_list . list))
  :iter-bare ((_list . listing))
  :iter-keyword ((_list . list)))

(loopy-deftest loopy-cmd-named ()
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
(loopy-deftest do ()
  :result '(t nil)
  :body ((with (my-val nil)
               (this-nil? t))
         (do (setq my-val t)
             (setq this-nil? (not my-val)))
         (return nil)
         (finally-return my-val this-nil?))
  :loopy t
  :iter-keyword (do return))

;;;;; Expr
(loopy-deftest expr-init ()
  :result 3
  :body ((cycle 3)
         (set var (1+ var) :init 0)
         (finally-return var))
  :loopy t
  :iter-keyword (cycle set)
  :iter-bare ((cycle . cycling)
              (set . setting)))

(loopy-deftest expr-when ()
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
            collect `(loopy-deftest ,(intern (format "expr-destr-%d-value" num)) ()
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
            collect `(loopy-deftest ,(intern (format "expr-%d-values" num)) ()
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

(ert-deftest expr-dont-repeat ()
  "Make sure commands don't repeatedly create/declare the same variable."
  (should
   (= 1 (with-temp-buffer
          (prin1 (macroexpand '(loopy  (expr j 3)
                                       (expr j 4)
                                       (return j)))
                 (current-buffer))
          (goto-char (point-min))
          (how-many "(j nil)")))))

;;;;; Group
(loopy-deftest group ()
  :result '((2 4 6) (2 4 6))
  :body ((list i '(1 2 3 4 5 6))
         (if (cl-evenp i)
             (_group (collect c1 i)
                     (collect c2 i)))
         (finally-return c1 c2))
  :repeat _group
  :loopy ((_group . (group command-do)))
  ;; Technically don't need to test (and wouldn't work if we used `for' inside,
  ;; anyway).
  :iter-keyword ((list . list)
                 (_group . (group command-do))))

;;;;; Prev-Expr
(loopy-deftest prev-expr ()
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

(loopy-deftest prev-expr-keyword-back ()
  :result '(nil nil nil 1 2)
  :body ((list i '(1 2 3 4 5))
         (set-prev j i :back 3)
         (collect j))
  :loopy t
  :iter-bare ((list . listing)
              (collect . collecting)
              (set-prev . setting-prev))
  :iter-keyword (list set-prev collect))

(loopy-deftest prev-expr-keyword-init ()
  :result '(first-val first-val 2 2 4 4 6 6 8 8)
  :body ((numbers i 1 10)
         (when (cl-oddp i)
           (set-prev j i :init 'first-val))
         (collect j))
  :loopy t
  :iter-bare ((numbers . numbering)
              (collect . collecting)
              (set-prev . setting-prev))
  :iter-keyword (numbers set-prev collect))

(loopy-deftest prev-expr-destructuring ()
  :result '((7 7 1 3) (7 7 2 4))
  :body ((list i '((1 2) (3 4) (5 6) (7 8)))
         (set-prev (a b) i :back 2 :init 7)
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
(ert-deftest iteration-sub-level ()
  (should-error
   (progn
     (loopy (if t (list i '(1))) (finally-return t))
     (loopy (if t (list-ref i '(1))) (finally-return t))
     (loopy (if t (array i '(1))) (finally-return t))
     (loopy (if t (array-ref i '(1))) (finally-return t))
     (loopy (if t (seq i '(1))) (finally-return t))
     (loopy (if t (seq-ref i '(1))) (finally-return t))
     (loopy (if t (repeat 1)) (finally-return t))
     (loopy (when t (list i '(1))) (finally-return t))
     (loopy (when t (list-ref i '(1))) (finally-return t))
     (loopy (when t (array i '(1))) (finally-return t))
     (loopy (when t (array-ref i '(1))) (finally-return t))
     (loopy (when t (seq i '(1))) (finally-return t))
     (loopy (when t (seq-ref i '(1))) (finally-return t))
     (loopy (when t (repeat 1)) (finally-return t))
     (loopy (unless t (list i '(1))) (finally-return t))
     (loopy (unless t (list-ref i '(1))) (finally-return t))
     (loopy (unless t (array i '(1))) (finally-return t))
     (loopy (unless t (array-ref i '(1))) (finally-return t))
     (loopy (unless t (seq i '(1))) (finally-return t))
     (loopy (unless t (seq-ref i '(1))) (finally-return t))
     (loopy (unless t (repeat 1)) (finally-return t))
     (loopy (cond (t (list i '(1)))) (finally-return t))
     (loopy (cond (t (list-ref i '(1)))) (finally-return t))
     (loopy (cond (t (array i '(1)))) (finally-return t))
     (loopy (cond (t (array-ref i '(1)))) (finally-return t))
     (loopy (cond (t (seq i '(1)))) (finally-return t))
     (loopy (cond (t (seq-ref i '(1)))) (finally-return t))
     (loopy (cond (t (repeat 1))) (finally-return t))
     (loopy (group (list i '(1))) (finally-return t))
     (loopy (group (list-ref i '(1))) (finally-return t))
     (loopy (group (array i '(1))) (finally-return t))
     (loopy (group (array-ref i '(1))) (finally-return t))
     (loopy (group (seq i '(1))) (finally-return t))
     (loopy (group (seq-ref i '(1))) (finally-return t))
     (loopy (group (repeat 1))) (finally-return t))
   :type 'user-error))

;; Can't bind the same iteration variable with multiple commands.
(ert-deftest iteration-same-var-multiple-cmd ()
  (should-error (eval (quote (loopy (list i '(1 2 3))
                                    (list i '(1 2 3)))))))

;;;;; Array
(ert-deftest array ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy  (array i [1 2 3])
                                      (collect coll i)
                                      (finally-return coll))))))

  (should (equal '(97 98 99)
                 (eval (quote (loopy  (string i "abc")
                                      (collect coll i)
                                      (finally-return coll)))))))

(ert-deftest array-vars ()
  (should (equal '(2 4 6 8)
                 (lq (with (start 2) (end 8)
                           (arr (cl-coerce (number-sequence 0 10) 'vector)))
                     (array i arr :from start :to end :by 2)
                     (collect i))))

  (should (equal '(2 4 6 8)
                 (lq (with (start 2) (end 8) (step 2)
                           (arr (cl-coerce (number-sequence 0 10) 'vector)))
                     (array i arr :from start :to end :by step)
                     (collect i)))))

(ert-deftest array-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy (array (a . b)
                                                 [(1 . 2) (3 . 4) (5 . 6)])
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy (array (a . b)
                                                 [(1 2) (3 4) (5 6)])
                                          (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy (array (a b c d)
                                                 [(1 2 3 4) (4 5 6 7)])
                                          (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy (array [i j k] [[1 2 3] [4 5 6]])
                                          (finally-return i j k))))))))


(ert-deftest array-recursive-destructuring ()
  (should
   (and
    (equal '(5 5 6)
           (eval (quote (loopy (array (a [b c]) [(1 [1 2]) (5 [5 6])])
                               (finally-return (list a b c))))))
    (equal '(4 5 6)
           (eval
            (quote
             (loopy (array [a [b c]] [[1 [2 3]] [4 [5 6]]])
                    (finally-return a b c)))))
    (equal '(4 5 6)
           (eval
            (quote
             (loopy (array [a [b [c]]] [[1 [2 [3]]] [4 [5 [6]]]])
                    (finally-return a b c)))))
    (equal '(4 5 6)
           (eval
            (quote
             (loopy (array [a (b c)] [[1 (2 3)] [4 (5 6)]])
                    (finally-return a b c))))))))

(ert-deftest array-multi-array ()
  (should (equal '((1 3) (1 4) (2 3) (2 4))
                 (loopy (array i [1 2] [3 4])
                        (collect i))))

  (should (equal '((1 3) (2 3))
                 (loopy (array i [1 2] [3 4] :by 2)
                        (collect i))))

  ;; Just to check how quoting is handled.
  (should (equal '((1 3) (1 4) (2 3) (2 4))
                 (loopy (array i `[1 ,(1+ 1)] [3 4])
                        (collect i)))))

(ert-deftest array-multi-array-destructuring ()
  (should (equal '((1 1 2 2) (3 4 3 4))
                 (eval (quote (loopy (array (i j) [1 2] [3 4])
                                     (collect c1 i)
                                     (collect c2 j)
                                     (finally-return c1 c2)))))))

(ert-deftest array-keywords ()
  (should (equal '((0 . 4) (1 . 3) (2 . 2) (3 . 1) (4 . 0))
                 (eval (quote (loopy (array i [4 3 2 1 0] :index cat)
                                     (collect (cons cat i)))))))

  (should (equal '(0 2 4 6 8 10)
                 (eval (quote (loopy (array i [0 1 2 3 4 5 6 7 8 9 10] :by 2)
                                     (collect i))))))

  (should (equal '(8 6 4 2)
                 (eval (quote (loopy (array i [0 1 2 3 4 5 6 7 8 9 10]
                                            :from 8 :downto 1 :by 2)
                                     (collect i))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (loopy (array i [0 1 2 3 4 5 6 7 8 9 10] :upto 7)
                                     (collect i))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (loopy (array i [0 1 2 3 4 5 6 7 8 9 10] :to 7)
                                     (collect i))))))

  (should (equal '(10 9 8 7 6 5 4 3)
                 (eval (quote (loopy (array i [0 1 2 3 4 5 6 7 8 9 10] :downto 3)
                                     (collect i))))))

  (should (equal '(10 9 8)
                 (eval (quote (loopy (array i [0 1 2 3 4 5 6 7 8 9 10] :above 7)
                                     (collect i))))))

  (should (equal '(0 1 2)
                 (eval (quote (loopy (array i [0 1 2 3 4 5 6 7 8 9 10] :below 3)
                                     (collect i)))))))

;;;;; Array Ref
(ert-deftest array-ref ()
  (should (equal "aaa"
                 (eval (quote (loopy (with (my-str "cat"))
                                     (array-ref i my-str)
                                     (do (setf i ?a))
                                     (finally-return my-str))))))
  (should (equal "aaa"
                 (eval (quote (loopy (with (my-str "cat"))
                                     (stringf i my-str)
                                     (do (setf i ?a))
                                     (finally-return my-str)))))))


(ert-deftest array-ref-destructuring ()
  (should (and (equal [(7 8 9) (7 8 9)]
                      (eval (quote (loopy (with (my-array [(1 2 3) (4 5 6)]))
                                          (array-ref (i j k) my-array)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-array)))))
               (equal [(7 8 9 10) (7 8 9 10)]
                      (eval (quote (loopy (with (my-array [(1 2 3 4) (4 5 6 8)]))
                                          (array-ref (i j . k) my-array)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k '(9 10)))
                                          (finally-return my-array)))))
               (equal [[7 8 9 4] [7 8 9 8]]
                      (eval (quote (loopy (with (my-array [[1 2 3 4] [4 5 6 8]]))
                                          (array-ref [i j k] my-array)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-array))))))))

(ert-deftest array-ref-recursive-destructuring ()
  (should (and (equal [(7 [8 9]) (7 [8 9])]
                      (eval (quote (loopy (with (my-array [(1 [2 3]) (4 [5 6])]))
                                          (array-ref (i [j k]) my-array)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-array)))))
               (equal [[7 [8 9] 4] [7 [8 9] 8]]
                      (eval (quote (loopy (with (my-array [[1 [2 3] 4] [4 [5 6] 8]]))
                                          (array-ref [i [j k]] my-array)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-array))))))))

(ert-deftest array-ref-keywords ()
  (should (equal "a1a3a5a7a9"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (array-ref i my-str :by 2)
                                     (do (setf i ?a))
                                     (finally-return my-str))))))

  (should (equal "a1a3a5a7a9"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (array-ref i my-str :by 2 :index cat)
                                     (do (setf (aref my-str cat) ?a))
                                     (finally-return my-str))))))

  (should (equal "0a2a4a6a8a"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (array-ref i my-str :from 1 :by 2 )
                                     (do (setf i ?a))
                                     (finally-return my-str))))))

  (should (equal "0123456a8a"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (array-ref i my-str :downto 6 :by 2 )
                                     (do (setf i ?a))
                                     (finally-return my-str))))))

  (should (equal "aaaaa56789"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (array-ref i my-str :below 5)
                                     (do (setf i ?a))
                                     (finally-return my-str))))))

  (should (equal "012345aaaa"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (array-ref i my-str :above 5)
                                     (do (setf i ?a))
                                     (finally-return my-str))))))

  (should (equal "aaaaaa6789"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (array-ref i my-str :upto 5)
                                     (do (setf i ?a))
                                     (finally-return my-str))))))

  (should (equal "0a2a4a6a8a"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (array-ref i my-str :upfrom 1 :by 2 )
                                     (do (setf i ?a))
                                     (finally-return my-str)))))))

(ert-deftest array-ref-vars ()
  (should (equal [0 1 22 3 22 5 22 7 22 9 10]
                 (lq (with (start 2) (end 8)
                           (arr (cl-coerce (number-sequence 0 10) 'vector)))
                     (array-ref i arr :from start :to end :by 2)
                     (do (setf i 22))
                     (finally-return arr))))

  (should (equal [0 1 22 3 22 5 22 7 22 9 10]
                 (lq (with (start 2) (end 8) (step 2)
                           (arr (cl-coerce (number-sequence 0 10) 'vector)))
                     (array-ref i arr :from start :to end :by step)
                     (do (setf i 22))
                     (finally-return arr)))))

;;;;; Cons
(ert-deftest cons ()
  (should (equal '((1 2 3 4) (2 3 4) (3 4) (4))
                 (eval (quote (loopy (cons x '(1 2 3 4))
                                     (collect coll x)
                                     (finally-return coll))))))

  (should (equal '((1 2 3 4) (2 3 4) (3 4) (4))
                 (eval (quote (loopy (cons x '(1 2 3 4) :by #'cdr)
                                     (collect coll x)
                                     (finally-return coll))))))

  (should (equal '((1 2 3 4) (3 4))
                 (eval (quote (loopy (cons x '(1 2 3 4) :by #'cddr)
                                     (collect coll x)
                                     (finally-return coll))))))

  (should (equal '((1 2 3 4) (3 4))
                 (eval (quote (loopy (cons x '(1 2 3 4)
                                           :by (lambda (x) (cddr x)))
                                     (collect coll x)
                                     (finally-return coll))))))

  (should (equal '((1 2 3 4) (3 4))
                 (eval (quote (let ((f (lambda (x) (cddr x))))
                                (loopy (cons x '(1 2 3 4) :by f)
                                       (collect coll x)
                                       (finally-return coll)))))))

  (should (equal '((1 (2 3 4)) (2 (3 4)) (3 (4)) (4 nil))
                 (eval (quote (loopy (cons (i . j) '(1 2 3 4))
                                     (collect coll (list i j))
                                     (finally-return coll))))))

  (should (equal '((1 (2 3 4)) (3 (4)))
                 (eval (quote (loopy (cons (i . j) '(1 2 3 4) :by #'cddr)
                                     (collect coll (list i j))
                                     (finally-return coll)))))))

(ert-deftest cons-init ()
  (equal '((1 2 3 4) (2 3 4) (3 4) (4))
         (lq (cons l '(1 2 3 4))
             (collect l)))

  (equal '((1 (2 3 4)) (2 (3 4)) (3 (4)) (4 nil))
         (lq (cons (car . cdr) '(1 2 3 4))
             (collect (list car cdr))))

  (equal '(25 (1 2 3 4) (1 2 3 4) (2 3 4) (2 3 4) (3 4) (3 4) (4))
         (lq (with (l 25))
             (collect l)
             (cons l '(1 2 3 4))
             (collect l))))

;;;;; Iter
(ert-deftest iter-with-single-var ()
  (should (equal '(1 2 3)
                 (lq (with (iter-maker (iter-lambda ()
                                         (iter-yield 1)
                                         (iter-yield 2)
                                         (iter-yield 3))))
                     (iter i (funcall iter-maker))
                     (collect i)))))

(ert-deftest iter-with-no-var ()
  (should (equal '(1 2 3)
                 (lq (with (iter-maker (iter-lambda ()
                                         (iter-yield 1)
                                         (iter-yield 2)
                                         (iter-yield 3))))
                     (iter (funcall iter-maker))
                     (set i 1 (1+ i))
                     (collect i)))))

(ert-deftest iter-close-twice ()
  (should (equal '(1 2) (lq (with (iter-maker (iter-lambda ()
                                                (iter-yield 1)
                                                (iter-yield 2)
                                                (iter-yield 3)))
                                  (gen (funcall iter-maker)))
                            (iter i gen :close t)
                            (iter j gen :close t)
                            (leave)
                            (finally-return i j)))))

(ert-deftest iter-same-gen ()
  (should (equal '((1 . 2) (3 . 4))
                 (lq (with (iter-maker (iter-lambda (x)
                                         (while x
                                           (iter-yield (pop x)))))
                           (gen (funcall iter-maker (list 1 2 3 4))))
                     (iter i gen :close nil)
                     (iter j gen :close nil)
                     (collect (cons i j))))))

(ert-deftest iter-init ()
  (should (equal '(1 2 3)
                 (lq (with (iter-maker (iter-lambda ()
                                         (iter-yield 1)
                                         (iter-yield 2)
                                         (iter-yield 3))))
                     (iter i (funcall iter-maker))
                     (collect i))))

  (should (equal '(27 1 1 2 2 3)
                 (lq (with (iter-maker (iter-lambda ()
                                         (iter-yield 1)
                                         (iter-yield 2)
                                         (iter-yield 3)))
                           (i 27))
                     (collect i)
                     (iter i (funcall iter-maker))
                     (collect i))))

  (should (equal '((nil nil) (1 2) (1 2) (3 4) (3 4) (5 6))
                 (lq (with (iter-maker (iter-lambda ()
                                         (iter-yield (list 1 2))
                                         (iter-yield (list 3 4))
                                         (iter-yield (list 5 6)))))
                     (collect (list i j))
                     (iter (i j) (funcall iter-maker))
                     (collect (list i j))))))

;;;;; List
(ert-deftest list ()
  (should (= 3 (eval (quote (loopy  (list i '(1 2 3))
                                    ;; Same thing:
                                    ;; (after-do (cl-return i))
                                    (finally-return i))))))
  (should (equal '(1 3)
                 (let ((my-cddr (lambda (x)  (cddr x))))
                   (loopy (list i '(1 2 3 4) :by my-cddr)
                          (collect i)))))

  (should (equal '(1 3)
                 (loopy (list i '(1 2 3 4) :by (lambda (x) (cddr x)))
                        (collect i))))

  (should (equal '(1 3)
                 (loopy (list i '(1 2 3 4) :by #'cddr)
                        (collect i)))))

(ert-deftest list-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy (list (a . b)
                                                '((1 . 2) (3 . 4) (5 . 6)))
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy (list (a . b)
                                                '((1 2) (3 4) (5 6)))
                                          (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy (list (a b c d)
                                                '((1 2 3 4) (4 5 6 7)))
                                          (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy (list [i j k] '([1 2 3] [4 5 6]))
                                          (finally-return i j k))))))))

(ert-deftest list-recursive-destructuring ()
  (should (equal '(4 5 6)
                 (eval (quote (loopy (list (a (b c)) '((1 (2 3)) (4 (5 6))))
                                     (finally-return (list a b c)))))))
  (should (equal '(5 5 6)
                 ;; This is more of an evaluation-time test.
                 (eval (quote (loopy (list (a . (b c)) '((1 . (1 2)) (5 . (5 6))))
                                     (finally-return (list a b c)))))))
  (should (equal '(4 5 6)
                 (loopy (list (a . [b c]) '((1 . [2 3]) (4 . [5 6])))
                        (finally-return a b c))))
  (should (equal '(5 5 6)
                 (eval (quote (loopy (list (a (b (c))) '((1 (1 (2))) (5 (5 (6)))))
                                     (finally-return (list a b c))))))))

(ert-deftest list-multi-list ()
  (should (equal '((1 4) (1 5) (1 6) (2 4) (2 5) (2 6) (3 4) (3 5) (3 6))
                 (eval (quote (loopy (list i '(1 2 3) '(4 5 6))
                                     (collect i))))))

  (should (equal '((1 7) (1 8) (1 9) (2 7) (2 8) (2 9))
                 (eval (quote (cl-labels ((fx () '(7 8 9)))
                                (loopy (list i '(1 2) (fx))
                                       (collect i)))))))

  (should (equal '((10 13) (10 15) (11 14) (12 13) (12 15))
                 (eval (quote (loopy (list i '(10 11 12) '(13 14 15) :by #'cddr)
                                     (collect i)))))))

(ert-deftest list-multi-list-destructuring ()
  (should (equal '((1 1 2 2) (4 5 4 5))
                 (eval (quote (loopy (list (i j) '(1 2) '(4 5))
                                     (collect c1 i)
                                     (collect c2 j)
                                     (finally-return c1 c2)))))))

;;;;; List Ref
(ert-deftest list-ref ()
  (should (equal  '(7 7 7)
                  (eval (quote (loopy (with (my-list '(1 2 3)))
                                      (list-ref i my-list)
                                      (do (setf i 7))
                                      (finally-return my-list))))))

  (should (equal  '(7 2 7)
                  (eval (quote (loopy (with (my-list '(1 2 3)))
                                      (list-ref i my-list :by #'cddr)
                                      (do (setf i 7))
                                      (finally-return my-list))))))

  (should (equal  '(7 2 7)
                  (eval (quote (loopy (with (my-list '(1 2 3)))
                                      (list-ref i my-list
                                                :by (lambda (x) (cddr x)))
                                      (do (setf i 7))
                                      (finally-return my-list))))))

  (should (equal  '(7 2 7)
                  (eval (quote (let ((f (lambda (x) (cddr x))))
                                 (loopy (with (my-list '(1 2 3)))
                                        (list-ref i my-list :by f)
                                        (do (setf i 7))
                                        (finally-return my-list))))))))

(ert-deftest list-ref-destructuring ()
  (should (and (equal '((7 8 9) (7 8 9))
                      (eval (quote (loopy (with (my-list '((1 2 3) (4 5 6))))
                                          (list-ref (i j k) my-list)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-list)))))
               (equal '((7 8 9 10) (7 8 9 10))
                      (eval (quote (loopy (with (my-list '((1 2 3 4) (4 5 6 8))))
                                          (list-ref (i j . k) my-list)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k '(9 10)))
                                          (finally-return my-list)))))
               (equal '([7 8 9 4] [7 8 9 8])
                      (eval (quote (loopy (with (my-list '([1 2 3 4] [4 5 6 8])))
                                          (list-ref [i j k] my-list)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-list))))))))

(ert-deftest list-ref-recursive-destructuring ()
  (should (equal '((7 (8 9)) (7 (8 9)))
                 (eval (quote (loopy (with (my-list '((1 (2 3)) (4 (5 6)))))
                                     (list-ref (i (j k)) my-list)
                                     (do (setf i 7)
                                         (setf j 8)
                                         (setf k 9))
                                     (finally-return my-list))))))
  (should (equal '([7 (8 9) 4] [7 (8 9) 8])
                 (eval (quote (loopy (with (my-list '([1 (2 3) 4] [4 (5 6) 8])))
                                     (list-ref [i (j k) l] my-list)
                                     (do (setf i 7)
                                         (setf j 8)
                                         (setf k 9))
                                     (finally-return my-list)))))))

;;;;; Map
(ert-deftest map ()
  (should (equal '((a . 1) (b . 2))
                 (eval (quote (loopy (map pair '((a . 1) (b . 2)))
                                     (collect coll pair)
                                     (finally-return coll))))))

  (should (equal '((a . 1) (b . 2))
                 (eval (quote (loopy (map-pairs pair '((a . 1) (b . 2)))
                                     (collect coll pair)
                                     (finally-return coll))))))

  (should (equal '((0 . a) (1 . b) (2 . c) (3 . d))
                 (eval (quote (loopy (map pair [a b c d])
                                     (collect coll pair)
                                     (finally-return coll))))))

  (should (equal '((0 . a) (1 . b) (2 . c) (3 . d))
                 (eval (quote (loopy (map-pairs pair [a b c d])
                                     (collect coll pair)
                                     (finally-return coll))))))

  (let ((my-hash (make-hash-table)))
    (puthash 'a 1 my-hash)
    (puthash 'b 2 my-hash)
    (should (equal '((a . 1) (b . 2))
                   (loopy (map pair my-hash)
                          (collect coll pair)
                          (finally-return coll))))))

(ert-deftest map-unique ()
  (should (equal '((a . 1) (b . 2) (c . 3))
                 (eval (quote (loopy (map-pairs pair '((a . 1)
                                                       (a . 27)
                                                       (b . 2)
                                                       (c . 3)))
                                     (collect coll pair)
                                     (finally-return coll))))))

  (should (equal '((a . 1) (b . 2) (c . 3))
                 (eval (quote (loopy (map-pairs pair '((a . 1)
                                                       (a . 27)
                                                       (b . 2)
                                                       (c . 3))
                                                :unique t)
                                     (collect coll pair)
                                     (finally-return coll))))))

  (should (equal '((a . 1) (a . 27) (b . 2) (c . 3))
                 (eval (quote (loopy (map-pairs pair '((a . 1)
                                                       (a . 27)
                                                       (b . 2)
                                                       (c . 3))
                                                :unique nil)
                                     (collect coll pair)
                                     (finally-return coll)))))))

(ert-deftest map-destructuring ()
  (should (equal '((a b) (1 2))
                 (eval (quote (loopy (map (key . val) '((a . 1) (b . 2)))
                                     (collect keys key)
                                     (collect vals val)
                                     (finally-return keys vals))))))

  (should (equal '((0 1 2 3) (a b c d))
                 (eval (quote (loopy (map (key . val) [a b c d])
                                     (collect keys key)
                                     (collect vals val)
                                     (finally-return keys vals))))))
  (let ((my-hash (make-hash-table)))
    (puthash 'a 1 my-hash)
    (puthash 'b 2 my-hash)
    (should (equal '((a b) (1 2))
                   (loopy (map (key . val) my-hash)
                          (collect keys key)
                          (collect vals val)
                          (finally-return keys vals))))))

;;;;; Map Ref

(ert-deftest map-ref ()
  (should (equal [17 18 19 20 21]
                 (eval (quote (loopy (with (map (vector 10 11 12 13 14)))
                                     (mapf i map)
                                     (do (cl-incf i 7))
                                     (finally-return map))))))

  (should (equal '([17 18 19 20 21] (0 1 2 3 4))
                 (eval (quote (loopy (with (map (vector 10 11 12 13 14)))
                                     (mapf i map :key my-key)
                                     (do (cl-incf i 7))
                                     (collect my-key)
                                     (finally-return map loopy-result)))))))

(ert-deftest map-ref-unique ()
  (should (equal '(:a 8 :a ignored :b 10)
                 (let ((map (list :a 1 :a 'ignored :b 3)))
                   (loopy (map-ref i map)
                          (do (cl-incf i 7))
                          (finally-return map)))))

  (should (equal '(:a 8 :a ignored :b 10)
                 (let ((map (list :a 1 :a 'ignored :b 3)))
                   (loopy (map-ref i map :unique t)
                          (do (cl-incf i 7))
                          (finally-return map)))))

  (should (equal '(:a 15 :a ignored :b 10)
                 (let ((map (list :a 1 :a 'ignored :b 3)))
                   (loopy (map-ref i map :unique nil)
                          (do (cl-incf i 7))
                          (finally-return map))))))

(ert-deftest map-ref-destructuring ()
  (should (equal [[7 8] [7 8]]
                 (eval (quote (loopy (with (map (vector (vector 10 11)
                                                        (vector 12 13))))
                                     (mapf [i j] map)
                                     (do (setf i 7)
                                         (setf j 8))
                                     (finally-return map))))))

  (should (equal '((a 7 8) (b 7 8))
                 (eval (quote (loopy (with (map (list (cons 'a (list 1 2))
                                                      (cons 'b (list 3 4)))))
                                     (mapf (i j) map)
                                     (do (setf i 7)
                                         (setf j 8))
                                     (finally-return map)))))))

;;;;; Nums
(ert-deftest nums ()
  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy (nums i 1 5)
                                     (collect i))))))

  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy (numbers i 1 5)
                                     (collect i))))))

  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy (num i 1 5)
                                     (collect i))))))

  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy (number i 1 5)
                                     (collect i))))))

  (should (equal '(1 3 5)
                 (eval (quote (loopy (nums i 1 5 2)
                                     (collect i))))))

  (should (equal '(5 3 1)
                 (eval (quote (loopy (nums i 5 1 -2)
                                     (collect i)))))))

(ert-deftest nums-keywords ()
  (should (equal '(1 3 5)
                 (eval (quote (loopy (nums i 1 5 :by 2)
                                     (collect i))))))

  (should (equal '(5 3 1)
                 (eval (quote (loopy (nums i 5 :downto 1 :by 2)
                                     (collect i))))))

  (should (equal '(0 7 14)
                 (eval (quote (loopy (repeat 3)
                                     (nums i 0 :by 7)
                                     (collect i))))))

  (should (equal '(0 -7 -14 -21 -28 -35 -42)
                 (eval (quote (loopy (repeat 7)
                                     (nums i :downfrom 0 :by 7)
                                     (collect i))))))
  (should (equal '(7 8 9)
                 (eval (quote (loopy (repeat 3)
                                     (nums i :upfrom 7)
                                     (collect i))))))

  (should (equal '(7 8 9)
                 (eval (quote (loopy (repeat 3)
                                     (nums i :from 7)
                                     (collect i))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (loopy (nums i :upto 7)
                                     (collect i))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (loopy (nums i :to 7)
                                     (collect i))))))

  (should (equal '(0 -1 -2 -3 -4 -5 -6 -7)
                 (eval (quote (loopy (nums i :downto -7)
                                     (collect i))))))

  (should (equal '(0 -1 -2 -3 -4 -5 -6)
                 (eval (quote (loopy (nums i :above -7)
                                     (collect i))))))

  (should (equal '(0 1 2)
                 (eval (quote (loopy (nums i :below 3)
                                     (collect i))))))

  (should (equal nil
                 (eval (quote (loopy (nums i :above 3)
                                     (collect i))))))
  (should (equal '(0 1.5 3.0)
                 (loopy (nums i 0 3 :by 1.5)
                        (collect i))))

  (should (equal '(0 1.5 3.0 4.5)
                 (eval (quote (loopy (nums i 0 5 :by 1.5)
                                     (collect i))))))

  ;; NOTE: It remains to be seen how well this test works.
  (progn
    (cl-float-limits)
    (should (cl-every (lambda (x y) (> cl-float-epsilon (- x y)))
                      '(0.5 0.3 0.1 -0.1 -0.3 -0.5)
                      (eval (quote (loopy (nums i
                                                :downfrom 0.5
                                                :above -0.7
                                                :by 0.2)
                                          (collect i))))))))

;;;;; Nums With Vars
(ert-deftest nums-vars ()
  (should (equal '(2 4 6 8)
                 (lq (with (start 2) (end 8) (step 2))
                     (numbers i start end step)
                     (collect i))))

  (should (equal '(2 4 6 8)
                 (lq (with (start 2) (end 8))
                     (numbers i start end 2)
                     (collect i))))

  (should (equal '(8 6 4 2)
                 (lq (with (start 8) (end 2) (step -2))
                     (numbers i start end step)
                     (collect i))))

  (should (equal '(2 4 6 8)
                 (lq (with (start 2) (end 8))
                     (numbers i :from start :to end :by 2)
                     (collect i))))

  (should (equal '(2 4 6 8)
                 (lq (with (start 2) (end 8) (step 2))
                     (numbers i :from start :to end :by step)
                     (collect i)))))

(ert-deftest nums-with ()
  (should (equal '(24 1 1 2)
                 (loopy (with (n 24))
                        (collect n)
                        (numbers n :from 1 :to 2)
                        (collect n)))))

;;;;; Nums-Down
(ert-deftest nums-down ()
  (should (equal '(10 8 6 4 2)
                 (eval (quote (loopy (nums-down i 10 1 :by 2)
                                     (collect i))))))

  (should (equal '(10 8 6 4 2)
                 (eval (quote (loopy (nums-down i 10 1 2)
                                     (collect i))))))

  (should (equal '(10 8 6 4 2)
                 (eval (quote (loopy (numsdown i 10 1 :by 2)
                                     (collect i))))))

  (should (equal '(10 8 6 4 2)
                 (eval (quote (loopy (numbers-down i 10 1 :by 2)
                                     (collect i)))))))

;;;;; Nums-Up
(ert-deftest nums-up ()
  (should (equal '(1 3 5 7 9)
                 (eval (quote (loopy (nums-up i 1 10 :by 2)
                                     (collect i))))))

  (should (equal '(1 3 5 7 9)
                 (eval (quote (loopy (nums-up i 1 10 2)
                                     (collect i))))))

  (should (equal '(1 3 5 7 9)
                 (eval (quote (loopy (numsup i 1 10 :by 2)
                                     (collect i))))))

  (should (equal '(1 3 5 7 9)
                 (eval (quote (loopy (numbers-up i 1 10 :by 2)
                                     (collect i)))))))

;;;;; Repeat
(ert-deftest repeat-cycle-no-var ()
  (should (= 3 (length (eval (quote (loopy  (repeat 3)
                                            (list i (number-sequence 1 10))
                                            (collect coll i)
                                            (finally-return coll)))))))

  (should (= 3 (length (eval (quote (loopy  (cycle 3)
                                            (list i (number-sequence 1 10))
                                            (collect coll i)
                                            (finally-return coll))))))))

(ert-deftest repeat-cycle-var ()
  "Need to test order of execution and functionality."
  (should (equal '(0 1 2)
                 (eval (quote (loopy (collect coll i)
                                     (repeat i 3)
                                     (finally-return coll))))))

  (should (equal '(0 1 2)
                 (eval (quote (loopy (collect coll i)
                                     (cycle i 3)
                                     (finally-return coll)))))))

(ert-deftest cycle-init ()
  (should (equal '(cat 0 0 1 1 2)
                 (lq (with (my-var 'cat))
                     (collect my-var)
                     (cycle my-var 3)
                     (collect my-var)))))

;;;;; Seq
(ert-deftest seq ()
  (should (eval (quote (loopy (seq l '(1 2 3 4 5))
                              (seq a [1 2 3 4 5])
                              (if (/= l a)
                                  (return nil))
                              (finally-return t))))))

(ert-deftest seq-destructuring ()
  (should (and (equal '(5 6)
                      (eval (quote (loopy (seq (a . b)
                                               [(1 . 2) (3 . 4) (5 . 6)])
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy (seq (a . b)
                                               [(1 2) (3 4) (5 6)])
                                          (finally-return a b)))))
               (equal '(4 5 6)
                      (eval (quote (loopy (seq (a b c)
                                               [(1 2 3) (4 5 6)])
                                          (finally-return a b c)))))
               (equal '(5 6)
                      (eval (quote (loopy (seq (a . b)
                                               '((1 . 2) (3 . 4) (5 . 6)))
                                          (finally-return a b)))))
               (equal '(5 (6))
                      (eval (quote (loopy (seq (a . b)
                                               '((1 2) (3 4) (5 6)))
                                          (finally-return a b)))))
               (equal '(4 5 6 7)
                      (eval (quote (loopy (seq (a b c d)
                                               '((1 2 3 4) (4 5 6 7)))
                                          (finally-return a b c d)))))
               (equal '(4 5 6)
                      (eval (quote (loopy (seq [i j k] '([1 2 3] [4 5 6]))
                                          (finally-return i j k)))))
               (equal '(4 5 6)
                      (eval (quote (loopy (seq [i j k] [[1 2 3] [4 5 6]])
                                          (finally-return i j k))))))))

(ert-deftest seq-ref-destructuring ()
  (should (and (equal [(7 8 9) (7 8 9)]
                      (eval (quote (loopy (with (my-seq [(1 2 3) (4 5 6)]))
                                          (seq-ref (i j k) my-seq)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-seq)))))
               (equal [(7 8 9 10) (7 8 9 10)]
                      (eval (quote (loopy (with (my-seq [(1 2 3 4) (4 5 6 8)]))
                                          (seq-ref (i j . k) my-seq)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k '(9 10)))
                                          (finally-return my-seq)))))
               (equal '((7 8 9) (7 8 9))
                      (eval (quote (loopy (with (my-seq '((1 2 3) (4 5 6))))
                                          (seq-ref (i j k) my-seq)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-seq)))))
               (equal '((7 8 9 10) (7 8 9 10))
                      (eval (quote (loopy (with (my-seq '((1 2 3 4) (4 5 6 8))))
                                          (seq-ref (i j . k) my-seq)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k '(9 10)))
                                          (finally-return my-seq)))))
               (equal '([7 8 9 4] [7 8 9 8])
                      (eval (quote (loopy (with (my-seq '([1 2 3 4] [4 5 6 8])))
                                          (seq-ref [i j k] my-seq)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-seq)))))
               (equal [[7 8 9 4] [7 8 9 8]]
                      (eval (quote (loopy (with (my-seq [[1 2 3 4] [4 5 6 8]]))
                                          (seq-ref [i j k] my-seq)
                                          (do (setf i 7)
                                              (setf j 8)
                                              (setf k 9))
                                          (finally-return my-seq))))))))

(ert-deftest seq-vars ()
  (should (equal '(2 3 4 5 6 7 8 9 10)
                 (loopy (with (start 2) (end 8)
                              (arr (cl-coerce (number-sequence 0 10) 'vector)))
                        (sequence i arr :from start :by 1)
                        (collect i))))

  (should (equal '(2 4 6 8)
                 (loopy (with (start 2) (end 8)
                              (arr (cl-coerce (number-sequence 0 10) 'vector)))
                        (sequence i arr :from start :to end :by 2)
                        (collect i))))

  (should (equal '(2 4 6 8)
                 (loopy (with (start 2) (end 8) (step 2)
                              (arr (cl-coerce (number-sequence 0 10) 'vector)))
                        (sequence i arr :from start :to end :by step)
                        (collect i)))))

(ert-deftest seq-keywords ()
  (should (equal '((0 . 4) (1 . 3) (2 . 2) (3 . 1) (4 . 0))
                 (eval (quote (loopy (seq i [4 3 2 1 0] :index cat)
                                     (collect (cons cat i)))))))

  (should (equal '(0 2 4 6 8 10)
                 (eval (quote (loopy (seq i [0 1 2 3 4 5 6 7 8 9 10] :by 2)
                                     (collect i))))))

  (should (equal '(8 6 4 2)
                 (eval (quote (loopy (seq i [0 1 2 3 4 5 6 7 8 9 10]
                                          :from 8 :downto 1 :by 2)
                                     (collect i))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (loopy (seq i [0 1 2 3 4 5 6 7 8 9 10] :upto 7)
                                     (collect i))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (loopy (seq i [0 1 2 3 4 5 6 7 8 9 10] :to 7)
                                     (collect i))))))

  (should (equal '(10 9 8 7 6 5 4 3)
                 (eval (quote (loopy (seq i [0 1 2 3 4 5 6 7 8 9 10] :downto 3)
                                     (collect i))))))

  (should (equal '(10 9 8)
                 (eval (quote (loopy (seq i [0 1 2 3 4 5 6 7 8 9 10] :above 7)
                                     (collect i))))))

  (should (equal '(0 1 2)
                 (eval (quote (loopy (seq i [0 1 2 3 4 5 6 7 8 9 10] :below 3)
                                     (collect i)))))))

(ert-deftest seq-multi-seq ()
  (should (equal '((1 3) (1 4) (2 3) (2 4))
                 (eval (quote (loopy (seq i [1 2] '(3 4))
                                     (collect i))))))

  (should (equal '((1 3) (2 3))
                 (eval (quote (loopy (seq i [1 2] '(3 4) :by 2)
                                     (collect i)))))))

;;;;; Seq Index
(ert-deftest seq-index ()
  (should (equal '(0 1 2 3)
                 (eval (quote (loopy (seq-index i [1 2 3 4])
                                     (collect i))))))

  (should (equal '(0 1 2 3 4 5 6)
                 (eval (quote (loopy (array-index i "abcdefg")
                                     (collect i))))))

  (should (equal '(0 1 2 3 4 5 6)
                 (eval (quote (loopy (string-index i "abcdefg")
                                     (collect i))))))

  (should (equal '(0 1 2 3 4)
                 (eval (quote (loopy (list-index i '(1 2 3 4 5))
                                     (collect i)))))))

(ert-deftest seq-index-keywords ()
  (should (equal '(0 2 4 6 8 10)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy (seq-index i my-seq :by 2)
                                       (collect (elt my-seq i))))))))

  (should (equal '(8 6 4 2)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy (seq-index i my-seq
                                                  :from 8 :downto 1 :by 2)
                                       (collect (elt my-seq i))))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy (seq-index i my-seq :upto 7)
                                       (collect (elt my-seq i))))))))

  (should (equal '(0 1 2 3 4 5 6 7)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy (seq-index i my-seq :to 7)
                                       (collect (elt my-seq i))))))))

  (should (equal '(10 9 8 7 6 5 4 3)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy (seq-index i my-seq :downto 3)
                                       (collect (elt my-seq i))))))))

  (should (equal '(10 9 8)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy (seq-index i my-seq :above 7)
                                       (collect (elt my-seq i))))))))

  (should (equal '(0 1 2)
                 (eval (quote (let ((my-seq [0 1 2 3 4 5 6 7 8 9 10]))
                                (loopy (seq-index i my-seq :below 3)
                                       (collect (elt my-seq i)))))))))

(ert-deftest seq-index-vars ()
  (should (equal '(2 4 6 8)
                 (lq (with (start 2) (end 8)
                           (arr (cl-coerce (number-sequence 0 10) 'vector)))
                     (seq-index i arr :from start :to end :by 2)
                     (collect i))))

  (should (equal '(2 4 6 8)
                 (lq (with (start 2) (end 8) (step 2)
                           (arr (cl-coerce (number-sequence 0 10) 'vector)))
                     (seq-index i arr :from start :to end :by step)
                     (collect i)))))

(ert-deftest seq-index-init ()
  (should (equal '(27 0 0 1 1 2 2 3)
                 (lq (with (i 27))
                     (collect i)
                     (seq-index i [1 2 3 4])
                     (collect i)))))

;;;;; Seq Ref
(ert-deftest seq-ref ()
  (should
   (equal '(7 7 7 7)
          (eval (quote (loopy (with (my-seq '(1 2 3 4)))
                              (seq-ref i my-seq)
                              (do (setf i 7))
                              (finally-return my-seq)))))))

(ert-deftest seq-ref-keywords ()
  (should (equal "a1a3a5a7a9"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (seq-ref i my-str :by 2)
                                     (do (setf i ?a))
                                     (finally-return my-str))))))

  (should (equal "a1a3a5a7a9"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (seq-ref i my-str :by 2 :index cat)
                                     (do (setf (aref my-str cat) ?a))
                                     (finally-return my-str))))))

  (should (equal '(0 cat 2 cat 4 cat 6 cat 8 cat)
                 (eval (quote (loopy (with (my-list '(0 1 2 3 4 5 6 7 8 9)))
                                     (seq-ref i my-list :from 1 :by 2 )
                                     (do (setf i 'cat))
                                     (finally-return my-list))))))

  (should (equal "0123456a8a"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (seq-ref i my-str :downto 6 :by 2 )
                                     (do (setf i ?a))
                                     (finally-return my-str))))))

  (should (equal "aaaaa56789"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (seq-ref i my-str :below 5)
                                     (do (setf i ?a))
                                     (finally-return my-str))))))

  (should (equal "012345aaaa"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (seq-ref i my-str :above 5)
                                     (do (setf i ?a))
                                     (finally-return my-str))))))

  (should (equal '(0 1 2 3 4 5 cat cat cat cat)
                 (eval (quote (loopy (with (my-list '(0 1 2 3 4 5 6 7 8 9)))
                                     (seq-ref i my-list :above 5)
                                     (do (setf i 'cat))
                                     (finally-return my-list))))))

  (should (equal "aaaaaa6789"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (seq-ref i my-str :upto 5)
                                     (do (setf i ?a))
                                     (finally-return my-str))))))

  (should (equal "0a2a4a6a8a"
                 (eval (quote (loopy (with (my-str "0123456789"))
                                     (seq-ref i my-str :upfrom 1 :by 2 )
                                     (do (setf i ?a))
                                     (finally-return my-str))))))

  (should (equal '(0 cat 2 cat 4 cat 6 cat 8 cat)
                 (eval (quote (loopy (with (my-list '(0 1 2 3 4 5 6 7 8 9)))
                                     (seq-ref i my-list :upfrom 1 :by 2)
                                     (do (setf i 'cat))
                                     (finally-return my-list)))))))

(ert-deftest seq-ref-vars ()
  (should (equal [0 1 22 3 22 5 22 7 22 9 10]
                 (lq (with (start 2) (end 8)
                           (arr (cl-coerce (number-sequence 0 10) 'vector)))
                     (seq-ref i arr :from start :to end :by 2)
                     (do (setf i 22))
                     (finally-return arr))))

  (should (equal [0 1 22 3 22 5 22 7 22 9 10]
                 (lq (with (start 2) (end 8) (step 2)
                           (arr (cl-coerce (number-sequence 0 10) 'vector)))
                     (seq-ref i arr :from start :to end :by step)
                     (do (setf i 22))
                     (finally-return arr)))))

;;;; Accumulation Commands
;;;;; Final updates
(ert-deftest accumulation-conflicting-final-updates ()
  (should-error (eval (quote (loopy (list i '((1) (2) (3)))
                                    (append i)
                                    (vconcat i)))))

  (should-error (eval (quote (loopy (list i '((1) (2) (3)))
                                    (collect i)
                                    (collect (1+ i) :result-type vector))))))

;;;;; Into Argument
(ert-deftest accumulation-into-argument ()
  (should (equal '((2 3 4) (2 2 2))
                 (eval (quote (loopy (list i '(1 2 3))
                                     (collect (1+ i) :into coll1)
                                     ;; This should be the same value repeated.
                                     ;; If not, it means `coll1'  is constructed
                                     ;; in reverse, instead of being treated as
                                     ;; explicitly named.
                                     (collect coll2 (cl-first coll1))
                                     (finally-return coll1 coll2))))))

  (should (= 9 (eval (quote (loopy (list i '(1 2 3))
                                   (sum (1+ i) :into j)
                                   (finally-return j)))))))

(ert-deftest accumulation-raise-error-bad-arg ()
  :expected-result :failed
  (eval (quote (loopy (list i '(1 2 3))
                      (collect i :casdfasdf x)))))

;;;;; Command Compatibility
(ert-deftest accumulation-compatibility ()
  (should (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                              (collect i)
                              (append i)
                              (adjoin i)
                              (union i)
                              (nunion (copy-sequence i))
                              (nconc (copy-sequence i))))))

  (should-error (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                    (collect i)
                                    (concat i)))))

  (should-error (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                    (append i)
                                    (concat i)))))

  (should-error (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                    (adjoin i)
                                    (concat i)))))

  (should-error (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                    (union i)
                                    (concat i)))))

  (should-error (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                    (nunion i)
                                    (concat i)))))

  (should-error (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                    (nconc i)
                                    (concat i)))))

  (should-error (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                    (vconcat i)
                                    (concat i)))))

  ;; Also check that we don't throw errors for commands of the same type.
  (should (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                              (vconcat i)
                              (vconcat i)))))

  (should (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                              (collect i)
                              (collect i)))))

  (should (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                              (concat i)
                              (concat i))))))

;;;;; Order of implicit returns.
(ert-deftest implicit-collect-order ()
  (should (equal '((2) (1 3))
                 (eval (quote (loopy (list i '(1 2 3))
                                     (if (cl-evenp i)
                                         (collect evens i)
                                       (collect odds i))
                                     (finally-return evens odds)))))))

;;;;; Name of implicit accumulations
(ert-deftest implicit-accumulation-name ()
  (should
   (and (equal '(1 2 3)
               (eval (quote (loopy (list i '(1 2 3))
                                   (collect i)
                                   (else-do (cl-return loopy-result))))))
        (equal '(0 1 2 3)
               (eval (quote (loopy (list i '(1 2 3))
                                   (collect i)
                                   (else-do
                                    (push 0 loopy-result)
                                    (cl-return loopy-result))))))
        (equal '(0 1 2 3)
               (eval (quote (loopy (list i '(1 2 3))
                                   (collect i)
                                   (finally-do
                                    (push 0 loopy-result))
                                   (finally-return loopy-result)))))
        (equal '(1 2 3)
               (eval (quote (loopy (list i '(1 2 3))
                                   (collect i)
                                   (finally-return loopy-result))))))))


;;;;; Accumulate
(ert-deftest accumulate ()
  (should (equal '(2 1)
                 (eval (quote (loopy (list i '(1 2))
                                     (accumulate my-accum i #'cons
                                                 :init nil)
                                     (finally-return my-accum))))))

  (should (equal '((3 1) (4 2))
                 (eval (quote (loopy (list i '((1 2) (3 4)))
                                     (accumulate (accum1 accum2) i #'cons
                                                 :init nil)
                                     (finally-return accum1 accum2))))))

  (should (equal '((3 1) (4 2))
                 (eval (quote (let ((f #'cons))
                                (loopy (list i '((1 2) (3 4)))
                                       (accumulate (accum1 accum2) i f
                                                   :init nil)
                                       (finally-return accum1 accum2)))))))

  (should (equal '(2 1)
                 (eval (quote (loopy (list i '(1 2))
                                     (callf2 my-accum i #'cons
                                             :init nil)
                                     (finally-return my-accum)))))))

;;;;; Adjoin
(ert-deftest adjoin ()
  (should (equal '((1 . 1) (1 . 2) (2 . 3))
                 (eval (quote (loopy (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                                     (adjoin a i)
                                     (finally-return a))))))

  (should (equal '((1 . 1) (1 . 2) (2 . 3))
                 (eval (quote (loopy (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                                     (adjoin a i :test #'equal)
                                     (finally-return a))))))

  (should (equal '((1 . 1) (1 . 2) (1 . 2) (2 . 3))
                 (eval (quote (loopy (list i '((1 . 1) (1 . 2)
                                               (1 . 2) (2 . 3)))
                                     (adjoin a i :test #'eql)
                                     (finally-return a))))))

  (should (equal '((1 . 1) (2 . 3))
                 (eval (quote (loopy (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                                     (adjoin a i :test #'= :key #'car)
                                     (finally-return a))))))

  (should (equal '((1 . 1) (2 . 3))
                 (let ((my-test #'=)
                       (my-key #'car))
                   (loopy (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                          (adjoin a i :test my-test :key my-key)
                          (finally-return a)))))

  (should (equal '((1 . 1) (2 . 3))
                 (let ((my-key #'car))
                   (loopy (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                          (adjoin a i :key my-key)
                          (finally-return a)))))

  (should (equal '((1 . 1) (1 . 2) (2 . 3))
                 (let ((my-test #'equal))
                   (loopy (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                          (adjoin a i :test my-test)
                          (finally-return a))))))

(ert-deftest adjoin-destructuring ()
  (should (equal '(((1 . 1) (1 . 2)) ((1 . 2) (2 . 3)))
                 (eval (quote (loopy (list i '(((1 . 1) (1 . 2))
                                               ((1 . 2) (2 . 3))))
                                     (adjoin (a1 a2) i)
                                     (finally-return a1 a2))))))

  (should (equal '(((1 . 2)) ((1 . 1) (2 . 3)))
                 (eval (quote (loopy (list i '(((1 . 2) (1 . 1))
                                               ((1 . 2) (2 . 3))))
                                     (adjoin (a1 a2) i :test #'equal)
                                     (finally-return a1 a2))))))

  (should (equal '(((1 . 2)) ((1 . 1) (2 . 3)))
                 (eval (quote (loopy (with (test #'equal))
                                     (list i '(((1 . 2) (1 . 1))
                                               ((1 . 2) (2 . 3))))
                                     (adjoin (a1 a2) i :test test)
                                     (finally-return a1 a2))))))

  (should (equal '(((1 . 1)) ((1 . 2) (2 . 3)))
                 (eval (quote (loopy (list i '(((1 . 1) (1 . 2))
                                               ((1 . 2) (2 . 3))))
                                     (adjoin (a1 a2) i :key #'car)
                                     (finally-return a1 a2))))))

  (should (equal '(((1 . 1)) ((1 . 2) (2 . 3)))
                 (eval (quote (loopy (with (key #'car))
                                     (list i '(((1 . 1) (1 . 2))
                                               ((1 . 2) (2 . 3))))
                                     (adjoin (a1 a2) i :key key)
                                     (finally-return a1 a2)))))))

(ert-deftest adjoin-implicit ()
  (should (equal '((1 . 1) (1 . 2) (2 . 3))
                 (eval (quote (loopy (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                                     (adjoin i))))))

  (should (equal '((1 . 1) (1 . 2) (2 . 3))
                 (eval (quote (loopy (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                                     (adjoin i :test #'equal))))))

  (should (equal '((1 . 1) (1 . 2) (1 . 2) (2 . 3))
                 (eval (quote (loopy (list i '((1 . 1) (1 . 2)
                                               (1 . 2) (2 . 3)))
                                     (adjoin i :test #'eql))))))

  (should (equal '((1 . 1) (2 . 3))
                 (eval (quote (loopy (list i '((1 . 1) (1 . 2) (1 . 2) (2 . 3)))
                                     (adjoin i :test #'= :key #'car)))))))

(ert-deftest adjoin-coercion ()
  (should (equal [1 2 3 4 5]
                 (eval (quote (loopy (list i '(1 2 2 3 4 4 5))
                                     (adjoin i :result-type 'array))))))

  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy (list i '(1 2 2 3 4 4 5))
                                     (adjoin i :result-type 'list))))))

  (should (equal "abcd"
                 (eval (quote (loopy (list i '(?a ?b ?c ?d))
                                     (adjoin i :result-type 'string))))))

  (should (equal [1 2 3]
                 (eval (quote (loopy (list i '(1 2 3))
                                     (adjoin my-var i :result-type 'vector)
                                     (finally-return my-var))))))

  (should (equal [1 2 3 4 5]
                 (eval (quote (loopy (list i '(1 2 2 3 4 4 5))
                                     (adjoin i :result-type array))))))

  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy (list i '(1 2 2 3 4 4 5))
                                     (adjoin i :result-type list))))))

  (should (equal "abcd"
                 (eval (quote (loopy (list i '(?a ?b ?c ?d))
                                     (adjoin i :result-type string))))))

  (should (equal [1 2 3]
                 (eval (quote (loopy (list i '(1 2 3))
                                     (adjoin my-var i :result-type vector)
                                     (finally-return my-var)))))))

(ert-deftest adjoin-at ()
  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy (list i '(1 2 3 4 4 5))
                                     (adjoin i :at end))))))

  (should (equal '(5 4 3 2 1)
                 (eval (quote (loopy (list i '(1 2 3 4 4 5))
                                     (adjoin i :at start))))))

  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy (flag split)
                                     (list i '(1 2 3 4 4 5))
                                     (adjoin i :at end))))))

  (should (equal [5 4 3 2 1]
                 (eval (quote (loopy (list i '(1 2 3 4 4 5))
                                     (adjoin i :at start :result-type 'array))))))

  (should
   (= 1
      (cl-count-if (lambda (x) (= (cl-second x) 4))
                   (eval (quote (loopy (list i '((1 2) (3 4) (5 4)))
                                       (adjoin i
                                               :at start
                                               :key #'cl-second)))))))

  (should
   (= 1
      (cl-count-if (lambda (x) (equal (cl-second x) '(4 0)))
                   (eval (quote (loopy (list i '((1 2)
                                                 (3 (4 0))
                                                 (5 (4 0))))
                                       (adjoin i :at start
                                               :key #'cl-second
                                               :test #'equal))))))))

(ert-deftest adjoin-end-tracking ()
  (should (equal '(1 2 3 4 5)
                 (eval (quote (loopy (list i '(1 2 2 3 3 4))
                                     (adjoin i :at end)
                                     (adjoin (1+ i) :at end))))))

  (should (equal '(3 2 1 11 12 13)
                 (eval (quote (loopy (list i '(1 2 3))
                                     (adjoin i :at start)
                                     (adjoin (+ i 10) :at end))))))

  (should (equal '(3 2 1 11 12 13)
                 (eval (quote (loopy (list i '(1 2 3))
                                     (adjoin (+ i 10) :at end)
                                     (adjoin i :at start)))))))

(ert-deftest adjoin-not-destructive ()
  (let ((l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10)))
    (loopy (list i l1) (adjoin coll i :at start))
    (should (equal l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10))))

  (let ((l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10)))
    (loopy (list i l1) (adjoin coll i :at end))
    (should (equal l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10))))

  (let ((l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10)))
    (loopy (list i l1) (adjoin i :at start))
    (should (equal l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10))))

  (let ((l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10)))
    (loopy (list i l1) (adjoin i :at end))
    (should (equal l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10))))

  (let ((l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10)))
    (loopy (flag split) (list i l1) (adjoin i :at end))
    (should (equal l1 (list 1 2 2 3 4 4 5 6 6 7 8 8 9 10 10)))))

;;;;; Append
(ert-deftest append ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (append coll i)
                                     (finally-return coll))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (appending coll i)
                                     (finally-return coll)))))))

(ert-deftest append-destructuring ()
  (should (equal '((1 2 5 6) (3 4 7 8))
                 (eval (quote (loopy (array i [((1 2) (3 4)) ((5 6) (7 8))])
                                     (append (j k) i)
                                     (finally-return j k))))))

  (should (equal '((1 2 5 6) (3 4 7 8))
                 (eval (quote (loopy (array i [((1 2) (3 4)) ((5 6) (7 8))])
                                     (appending (j k) i)
                                     (finally-return j k)))))))

(ert-deftest append-implicit ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (append i))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (appending i)))))))

(ert-deftest append-at ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (append i :at end))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (flag split)
                                     (list i '((1 2 3) (4 5 6)))
                                     (append i :at end))))))

  (should (equal '(4 5 6 1 2 3)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (append i :at start)))))))

(ert-deftest append-end-tracking ()
  (should (equal '(1 2 8 9 3 4 10 11 6 7 13 14)
                 (loopy (list i '((1 2) (3 4) (6 7)))
                        (append i :at end)
                        (append (mapcar (lambda (x) (+ x 7)) i)
                                :at end))))

  (should (equal '(6 7 3 4 1 2)
                 (loopy (accum-opt (coll end))
                        (list i '((1 2) (3 4) (6 7)))
                        (append coll i :at start)
                        (finally-return coll))))

  (should (equal '(1 2 3 4 6 7)
                 (loopy (accum-opt (coll start))
                        (list i '((1 2) (3 4) (6 7)))
                        (append coll i :at end)
                        (finally-return coll))))

  (should (equal '(1 2 3 4 5 6)
                 (loopy (flag split)
                        (list i '((1 2) (3 4) (5 6)))
                        (append i :at end))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (append i :at start)
                                     (append (mapcar (lambda (x) (+ x 10)) i)
                                             :at end))))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (append (mapcar (lambda (x) (+ x 10)) i)
                                             :at end)
                                     (append i :at start)))))))

(ert-deftest append-not-destructive ()
  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (list i l1) (append coll i :at start))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (list i l1) (append coll i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (list i l1) (append i :at start))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (list i l1) (append i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (flag split) (list i l1) (append i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8))))))

;;;;; Collect
(ert-deftest collect ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (collect coll j)
                                     (finally-return coll))))))
  (should (equal '(1 2 3)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (collecting coll j)
                                     (finally-return coll)))))))

;; Make sure that adding to end works correctly.
(ert-deftest collect-end-tracking ()
  (should (equal '(1 8 2 9 3 10 4 11)
                 (loopy (list i '(1 2 3 4))
                        (collect coll i :at end)
                        (collect coll (+ i 7) :at end)
                        (finally-return coll))))

  (should (equal '(1 8 2 9 3 10 4 11)
                 (loopy (list i '(1 2 3 4))
                        (collect i :at end)
                        (collect (+ i 7) :at end))))

  (should (equal '(1 2 3 4)
                 (loopy (flag split)
                        (list i '(1 2 3 4))
                        (collect i :at end))))

  (should (equal '(3 2 1 1 2 3)
                 (eval (quote (loopy (list i '(1 2 3))
                                     (collect i :at end)
                                     (collect i :at start))))))

  (should (equal '(3 2 1 1 2 3)
                 (eval (quote (loopy (list i '(1 2 3))
                                     (collect i :at start)
                                     (collect i :at end)))))))



(ert-deftest collect-destructuring ()
  (should (and (equal '((1 4) ((2 3) (5 6)))
                      (eval (quote (loopy (list j '((1 2 3) (4 5 6)))
                                          (collect (coll1 . coll2) j)
                                          (finally-return coll1 coll2)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy (list j '((1 2 3) (4 5 6)))
                                          (collect (coll1 coll2 coll3) j)
                                          (finally-return coll1 coll2 coll3)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy (list j '([1 2 3] [4 5 6]))
                                          (collect [coll1 coll2 coll3] j)
                                          (finally-return coll1 coll2 coll3)))))))
  (should (and (equal '((1 4) ((2 3) (5 6)))
                      (eval (quote (loopy (list j '((1 2 3) (4 5 6)))
                                          (collecting (coll1 . coll2) j)
                                          (finally-return coll1 coll2)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy (list j '((1 2 3) (4 5 6)))
                                          (collecting (coll1 coll2 coll3) j)
                                          (finally-return coll1 coll2 coll3)))))

               (equal '((1 4) (2 5) (3 6))
                      (eval (quote (loopy (list j '([1 2 3] [4 5 6]))
                                          (collecting [coll1 coll2 coll3] j)
                                          (finally-return coll1 coll2 coll3))))))))
(ert-deftest collect-implicit ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (collect j))))))
  (should (equal '(1 2 3)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (collecting j)))))))

(ert-deftest collect-coercion ()
  (should (equal [1 2 3]
                 (eval (quote (loopy (list j '(1 2 3))
                                     (collect v j :result-type 'vector)
                                     (finally-return v))))))

  (should (equal "abc"
                 (eval (quote (loopy (list j '(?a ?b ?c))
                                     (collect j :result-type 'string))))))

  (should (equal [1 2 3]
                 (eval (quote (loopy (list j '(1 2 3))
                                     (collect v j :result-type vector)
                                     (finally-return v))))))

  (should (equal "abc"
                 (eval (quote (loopy (list j '(?a ?b ?c))
                                     (collect j :result-type string)))))))

(ert-deftest collect-at ()
  (should (equal (list '(3 2 1) '(1 2 3))
                 (eval (quote (loopy (list i '(1 2 3))
                                     (collect coll1 i :at 'beginning)
                                     (collect coll2 (cl-first coll1))
                                     (finally-return coll1 coll2))))))

  (should (equal '(3 2 1)
                 (eval (quote (loopy (list i '(1 2 3))
                                     (collect i :at beginning))))))

  (should (equal [3 2 1]
                 (eval (quote (loopy (list i '(1 2 3))
                                     (collect coll1 i :at 'start :result-type 'array)
                                     (finally-return coll1))))))

  (should (equal '(3 2 1)
                 (eval (quote (loopy (list i '(1 2 3))
                                     (collect i :at start))))))

  (should (equal '(3 2 1)
                 (eval (quote (loopy (list i '(1 2 3))
                                     (collect i :at 'start))))))

  (should (equal '((1 2 3) (1 1 1))
                 (eval (quote (loopy (list i '(1 2 3))
                                     (collect coll1 i :at 'end)
                                     (collect coll2 (cl-first coll1))
                                     (finally-return coll1 coll2)))))))

;; This shouldn't ever happen, but it's still worth checking.
(ert-deftest collect-not-destructive ()
  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (list i l1) (collect coll i :at start))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (list i l1) (collect coll i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (list i l1) (collect i :at start))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (list i l1) (collect i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (flag split) (list i l1) (collect i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8))))))

;;;;; Concat
(ert-deftest concat ()
  (should (equal "catdog"
                 (eval (quote (loopy (list j '("cat" "dog"))
                                     (concat coll j)
                                     (finally-return coll))))))
  (should (equal "catdog"
                 (eval (quote (loopy (list j '("cat" "dog"))
                                     (concating coll j)
                                     (finally-return coll)))))))

(ert-deftest concat-destructuring ()
  (should (and (equal '("ad" "be" "cf")
                      (eval (quote (loopy (list j '(("a" "b" "c") ("d" "e" "f")))
                                          (concat (coll1 coll2 coll3) j)
                                          (finally-return coll1 coll2 coll3)))))

               (equal '("ad" "be" "cf")
                      (eval (quote (loopy (list j '(["a" "b" "c"] ["d" "e" "f"]))
                                          (concat [coll1 coll2 coll3] j)
                                          (finally-return coll1 coll2 coll3)))))))
  (should (and (equal '("ad" "be" "cf")
                      (eval (quote (loopy (list j '(("a" "b" "c") ("d" "e" "f")))
                                          (concating (coll1 coll2 coll3) j)
                                          (finally-return coll1 coll2 coll3)))))

               (equal '("ad" "be" "cf")
                      (eval (quote (loopy (list j '(["a" "b" "c"] ["d" "e" "f"]))
                                          (concating [coll1 coll2 coll3] j)
                                          (finally-return coll1 coll2 coll3))))))))

(ert-deftest concat-implict ()
  (should (equal "catdog"
                 (eval (quote (loopy (list j '("cat" "dog"))
                                     (concat j))))))
  (should (equal "catdog"
                 (eval (quote (loopy (list j '("cat" "dog"))
                                     (concating j)))))))

(ert-deftest concat-at ()
  (should (equal "catdog"
                 (eval (quote (loopy (list j '("cat" "dog"))
                                     (concat j :at end))))))

  (should (equal "dogcat"
                 (eval (quote (loopy (list j '("cat" "dog"))
                                     (concat j :at start))))))

  (should (equal "catdog"
                 (eval (quote (loopy (list j '("cat" "dog"))
                                     (concat str j :at end)
                                     (finally-return str))))))

  (should (equal "dogcat"
                 (eval (quote (loopy (list j '("cat" "dog"))
                                     (concat str j :at start)
                                     (finally-return str))))))

  (should (equal '("ad" "be" "cf")
                 (eval (quote (loopy (list j '(("a" "b" "c") ("d" "e" "f")))
                                     (concat (coll1 coll2 coll3) j :at end)
                                     (finally-return coll1 coll2 coll3))))))

  (should (equal '("da" "eb" "fc")
                 (eval (quote (loopy (list j '(("a" "b" "c") ("d" "e" "f")))
                                     (concat (coll1 coll2 coll3) j :at start)
                                     (finally-return coll1 coll2 coll3)))))))

;;;;; Count
(ert-deftest count ()
  (should (= 2
             (eval (quote (loopy (list i '(t nil t nil))
                                 (count c i)
                                 (finally-return c))))))
  (should (= 2
             (eval (quote (loopy (list i '(t nil t nil))
                                 (counting c i)
                                 (finally-return c)))))))

(ert-deftest count-destructuring ()
  (should
   (equal '(2 1)
          (eval (quote (loopy (list elem '((t nil) (t t)))
                              (count (c1 c2) elem)
                              (finally-return c1 c2))))))
  (should
   (equal '(2 1)
          (eval (quote (loopy (list elem '((t nil) (t t)))
                              (counting (c1 c2) elem)
                              (finally-return c1 c2)))))))

(ert-deftest count-implict ()
  (should (= 2
             (eval (quote (loopy (list i '(t nil t nil))
                                 (count i))))))
  (should (= 2
             (eval (quote (loopy (list i '(t nil t nil))
                                 (counting i)))))))

;;;;; Max
(ert-deftest max ()
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (max my-max i)
                                 (finally-return my-max))))))
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (maxing my-max i)
                                 (finally-return my-max))))))
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (maximize my-max i)
                                 (finally-return my-max))))))
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (maximizing my-max i)
                                 (finally-return my-max)))))))

(ert-deftest max-destructuring ()
  (should
   (equal '(9 11)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (max (m1 m2) elem)
                              (finally-return m1 m2))))))
  (should
   (equal '(9 11)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (maxing (m1 m2) elem)
                              (finally-return m1 m2))))))
  (should
   (equal '(9 11)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (maximize (m1 m2) elem)
                              (finally-return m1 m2))))))
  (should
   (equal '(9 11)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (maximizing (m1 m2) elem)
                              (finally-return m1 m2)))))))

(ert-deftest max-implict ()
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (max i))))))
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (maxing i))))))
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (maximize i))))))
  (should (= 11
             (eval (quote (loopy (list i '(1 11 2 10 3 9 4 8 5 7 6))
                                 (maximizing i)))))))

;;;;; Min
(ert-deftest min ()
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (min my-min i)
                          (finally-return my-min))))))
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (minimize my-min i)
                          (finally-return my-min))))))
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (minimizing my-min i)
                          (finally-return my-min))))))
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (minning my-min i)
                          (finally-return my-min)))))))

(ert-deftest min-destructuring ()
  (should
   (equal '(1 4)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (min (m1 m2) elem)
                              (finally-return m1 m2))))))
  (should
   (equal '(1 4)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (minimize (m1 m2) elem)
                              (finally-return m1 m2))))))
  (should
   (equal '(1 4)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (minning (m1 m2) elem)
                              (finally-return m1 m2))))))
  (should
   (equal '(1 4)
          (eval (quote (loopy (list elem '((1 11) (9 4)))
                              (minimizing (m1 m2) elem)
                              (finally-return m1 m2)))))))

(ert-deftest min-implict ()
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (min i))))))
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (minning i))))))
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (minimize i))))))
  (should
   (= 0
      (eval (quote (loopy (list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                          (minimizing i)))))))

;;;;; Multiply
(ert-deftest multiply ()
  (should (= 120 (eval (quote (loopy (list i '(1 2 3 4 5))
                                     (multiply product i)
                                     (finally-return product))))))

  (should (= 120 (eval (quote (loopy (list i '(1 2 3 4 5))
                                     (multiplying product i)
                                     (finally-return product)))))))

(ert-deftest multiply-destructuring ()
  (should (equal '(3 8) (eval (quote (loopy (list i '((1 2) (3 4)))
                                            (multiply (x y) i)
                                            (finally-return x y))))))

  (should (equal '(3 8) (eval (quote (loopy (list i '((1 2) (3 4)))
                                            (multiplying (x y) i)
                                            (finally-return x y)))))))

(ert-deftest multiply-implicit ()
  (should (= 120 (eval (quote (loopy (list i '(1 2 3 4 5))
                                     (multiply i))))))

  (should (= 120 (eval (quote (loopy (list i '(1 2 3 4 5))
                                     (multiplying i)))))))

;;;;; Nconc
(ert-deftest nconc ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconc l i)
                                     (finally-return l))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconcing l i)
                                     (finally-return l)))))))

(ert-deftest nconc-destructuring ()
  (should
   (equal '((1 4) ((2 3) (5 6)))
          (eval (quote (loopy (list elem '(((1) (2 3)) ((4) (5 6))))
                              (nconc (n1 . n2) elem)
                              (finally-return n1 n2))))))
  (should
   (equal '((1 4) ((2 3) (5 6)))
          (eval (quote (loopy (list elem '(((1) (2 3)) ((4) (5 6))))
                              (nconcing (n1 . n2) elem)
                              (finally-return n1 n2)))))))

(ert-deftest nconc-implict ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconc i))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconcing i)))))))

(ert-deftest nconc-at-literal-lists ()
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconc i))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconc i :at end))))))

  (should (equal '(4 5 6 1 2 3)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconc i :at start))))))
  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconc c i)
                                     (finally-return c))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconc c i :at end)
                                     (finally-return c))))))

  (should (equal '(4 5 6 1 2 3)
                 (eval (quote (loopy (list i '((1 2 3) (4 5 6)))
                                     (nconc c i :at start)
                                     (finally-return c)))))))

(ert-deftest nconc-at-new-lists ()

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (let ((l1 (list 1 2 3))
                                    (l2 (list 4 5 6)))
                                (loopy (list i (list l1 l2))
                                       (nconc i)))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (let ((l1 (list 1 2 3))
                                    (l2 (list 4 5 6)))
                                (loopy (list i (list l1 l2))
                                       (nconc i :at end)))))))

  (should (equal '(4 5 6 1 2 3)
                 (eval (quote (let ((l1 (list 1 2 3))
                                    (l2 (list 4 5 6)))
                                (loopy (list i (list l1 l2))
                                       (nconc i :at start)))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (let ((l1 (list 1 2 3))
                                    (l2 (list 4 5 6)))
                                (loopy (list i (list l1 l2))
                                       (nconc c i)
                                       (finally-return c)))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (let ((l1 (list 1 2 3))
                                    (l2 (list 4 5 6)))
                                (loopy (list i (list l1 l2))
                                       (nconc c i :at end)
                                       (finally-return c)))))))

  (should (equal '(4 5 6 1 2 3)
                 (eval (quote (let ((l1 (list 1 2 3))
                                    (l2 (list 4 5 6)))
                                (loopy (list i (list l1 l2))
                                       (nconc c i :at start)
                                       (finally-return c))))))))

(ert-deftest nconc-end-tracking ()
  (should (equal '(1 2 11 12 3 4 13 14 5 6 15 16)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (list j '((11 12) (13 14) (15 16)))
                                     (nconc coll i :at end)
                                     (nconc coll j :at end)
                                     (finally-return coll))))))

  (should (equal '(1 2 11 12 3 4 13 14 5 6 15 16)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (list j '((11 12) (13 14) (15 16)))
                                     (nconc i :at end)
                                     (nconc j :at end))))))

  (should (equal '(1 2 3 4 5 6)
                 (eval (quote (loopy (flag split)
                                     (list i '((1 2) (3 4) (5 6)))
                                     (nconc i :at end))))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (nconc (copy-sequence i) :at start)
                                     (nconc (mapcar (lambda (x) (+ x 10))
                                                    (copy-sequence i))
                                            :at end))))))

  (should (equal '(5 6 3 4 1 2)
                 (loopy (accum-opt (coll end))
                        (list i (list (list 1 2) (list 3 4) (list 5 6)))
                        (append coll i :at start)
                        (finally-return coll))))

  (should (equal '(1 2 3 4 5 6)
                 (loopy (accum-opt (coll start))
                        (list i (list (list 1 2) (list 3 4) (list 5 6)))
                        (append coll i :at end)
                        (finally-return coll))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (nconc (mapcar (lambda (x) (+ x 10))
                                                    (copy-sequence i))
                                            :at end)
                                     (nconc (copy-sequence i) :at start)))))))

;;;;; Nunion
(ert-deftest nunion ()
  ;; (should (null (cl-set-difference
  ;;                '(4 1 2 3)
  ;;                (eval (quote (loopy (list i '((1 2) (2 3) (3 4)))
  ;;                                    (nunion var i)))))))
  ;; (should (null (cl-set-difference
  ;;                '(4 1 2 3)
  ;;                (eval (quote (loopy (list i '((1 2) (2 3) (3 4)))
  ;;                                    (nunioning var i)
  ;;                                    (finally-return var)))))))
  ;;
  ;; (should (null (cl-set-difference
  ;;                '(4 2 (1 1) 3)
  ;;                (eval (quote (loopy (list i '(((1 1) 2) ((1 1) 3) (3 4)))
  ;;                                    (nunioning var i :test #'equal)
  ;;                                    (finally-return var))))
  ;;                :test #'equal)))
  ;;
  ;; ;; The resulting list should only have one element whose `car' is `a'.
  ;; (should (= 1 (cl-count-if (lambda (x) (eq (car x) 'a))
  ;;                           (eval (quote (loopy (array i [((a . 1)) ((a . 2))])
  ;;                                               (nunioning var i :key #'car)
  ;;                                               (finally-return var)))))))

  (should (equal
           '(1 2 3 4)
           (eval (quote (loopy (list i '((1 2) (2 3) (3 4)))
                               (nunion var i)
                               (finally-return var))))))
  (should (equal
           '(1 2 3 4)
           (eval (quote (loopy (list i '((1 2) (2 3) (3 4)))
                               (nunioning var i)
                               (finally-return var))))))

  (should (equal '((1 1) 2 3 4)
                 (eval (quote (loopy (list i '(((1 1) 2) ((1 1) 3) (3 4)))
                                     (nunioning var i :test #'equal)
                                     (finally-return var))))))

  (should (equal '((1 1) 2 3 4)
                 (eval (quote (let ((test #'equal))
                                (loopy
                                 (list i '(((1 1) 2) ((1 1) 3) (3 4)))
                                 (nunioning var i :test test)
                                 (finally-return var)))))))

  ;; The resulting list should only have one element whose `car' is `a'.
  (should (equal '((a . 1)) (eval (quote (loopy (array i [((a . 1)) ((a . 2))])
                                                (nunioning var i :key #'car)
                                                (finally-return var))))))

  (should (equal '((a . 1)) (eval (quote (let ((key #'car))
                                           (loopy (array i [((a . 1)) ((a . 2))])
                                                  (nunioning var i :key key)
                                                  (finally-return var))))))))

(ert-deftest nunion-destructuring ()
  (should (equal '((1 2 3) (2 3 4))
                 (eval (quote (loopy (array i [((1 2) (2 3))
                                               ((1 2 3) (3 4))])
                                     (nunion (var1 var2) i :test #'equal)
                                     (finally-return var1 var2)))))))

(ert-deftest nunion-at ()
  (should (equal '((1 2) (3 2) (1 1))
                 (eval (quote (loopy (list i '(((1 2) (3 2)) ((1 1) (4 2))))
                                     (nunion i :at end :key #'cl-second))))))

  (should (equal '((1 2) (3 2) (4 2))
                 (eval (quote (loopy (list i '(((1 2) (3 2)) ((1 1) (4 2))))
                                     (nunion i :at end :key #'car))))))

  (should (equal '((4 2) (1 2) (3 2))
                 (eval (quote (loopy (list i '(((1 2) (3 2)) ((1 1) (4 2))))
                                     (nunion i :at start :key #'car))))))

  (should (equal '((4 2) (1 2) (3 2))
                 (eval (quote (loopy (list i '(((1 2) (3 2)) ((1 1) (4 2))))
                                     (nunion c i :at start :key #'car)
                                     (finally-return c))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy (list i '((1 2 3) (1 2 3)))
                                     (nunion i :test #'equal :at start))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy (list i '((1 2 3) (1 2 3)))
                                     (nunion c i :test #'equal :at start)
                                     (finally-return c)))))))

(ert-deftest nunion-end-tracking ()
  (should (equal '(1 2 3 4 5 6 7 8 9 10)
                 (eval (quote (loopy (list i '((1 2 3) (1 2 3) (4 5 6) (7 8 9)))
                                     (nunion coll i)
                                     (nunion coll (mapcar #'1+ i))
                                     (finally-return coll))))))

  (should (equal '(1 2 3 4 5 6 7 8 9)
                 (eval (quote (loopy (list i '((1 2 3) (1 2 3) (4 5 6) (7 8 9)))
                                     (nunion i :at end))))))

  (should (equal '(1 2 3 4 5 6 7 8 9)
                 (eval (quote (loopy (flag split)
                                     (list i '((1 2 3) (1 2 3) (4 5 6) (7 8 9)))
                                     (nunion i :at end))))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (loopy (list i '((1 2) (3 4) (5 6)))
                        (nunion (copy-sequence i) :at start)
                        (nunion (mapcar (lambda (x) (+ x 10)) (copy-sequence i))
                                :at end))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (loopy (list i '((1 2) (3 4) (5 6)))
                        (nunion (mapcar (lambda (x) (+ x 10)) (copy-sequence i))
                                :at end)
                        (nunion (copy-sequence i) :at start)))))

;;;;; Prepend
(ert-deftest prepend ()
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepend my-list i)
                                     (finally-return my-list))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepending my-list i)
                                     (finally-return my-list)))))))

(ert-deftest prepend-destructuring ()
  (should (equal '((5 6 1 2) (7 8 3 4))
                 (eval (quote (loopy (list i '([(1 2) (3 4)] [(5 6) (7 8)]))
                                     (prepend [my-list1 my-list2] i)
                                     (finally-return my-list1 my-list2))))))
  (should (equal '((5 6 1 2) (7 8 3 4))
                 (eval (quote (loopy (list i '([(1 2) (3 4)] [(5 6) (7 8)]))
                                     (prepending [my-list1 my-list2] i)
                                     (finally-return my-list1 my-list2)))))))

(ert-deftest prepend-implicit ()
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepend i)
                                     (finally-return loopy-result))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepending i)
                                     (finally-return loopy-result))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepend i))))))
  (should (equal '(5 6 3 4 1 2)
                 (eval (quote (loopy (list i '((1 2) (3 4) (5 6)))
                                     (prepending i)))))))


;;;;; Push Into
(ert-deftest push-into ()
  (should (equal '(3 2 1)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (push-into coll j)
                                     (finally-return coll))))))
  (should (equal '(3 2 1)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (pushing-into coll j)
                                     (finally-return coll))))))
  (should (equal '(3 2 1)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (push coll j)
                                     (finally-return coll))))))
  (should (equal '(3 2 1)
                 (eval (quote (loopy (list j '(1 2 3))
                                     (pushing coll j)
                                     (finally-return coll)))))))

(ert-deftest push-into-destructuring ()
  (should (equal '((5 3 1) (6 4 2))
                 (eval (quote (loopy (list elem '((1 2) (3 4) (5 6)))
                                     (push-into (p1 p2) elem)
                                     (finally-return p1 p2))))))
  (should (equal '((5 3 1) (6 4 2))
                 (eval (quote (loopy (list elem '((1 2) (3 4) (5 6)))
                                     (pushing-into (p1 p2) elem)
                                     (finally-return p1 p2))))))
  (should (equal '((5 3 1) (6 4 2))
                 (eval (quote (loopy (list elem '((1 2) (3 4) (5 6)))
                                     (push (p1 p2) elem)
                                     (finally-return p1 p2))))))
  (should (equal '((5 3 1) (6 4 2))
                 (eval (quote (loopy (list elem '((1 2) (3 4) (5 6)))
                                     (pushing (p1 p2) elem)
                                     (finally-return p1 p2)))))))

;;;;; Reduce
(ert-deftest reduce ()
  (should (= 6
             (eval (quote (loopy (list i '(1 2 3))
                                 (reduce r i #'+ :init 0)
                                 (finally-return r))))))

  (should (= 6
             (eval (quote (loopy (list i '(1 2 3))
                                 (callf r i #'+ :init 0)
                                 (finally-return r))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy (list i '((1) (2) (3)))
                                     (reduce r i #'append)
                                     (finally-return r))))))

  (should (equal '(1 2 3)
                 (eval (quote (let ((func #'append))
                                (loopy (list i '((1) (2) (3)))
                                       (reduce r i func)
                                       (finally-return r)))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy (list i '((1) (2) (3)))
                                     (reducing r i #'append)
                                     (finally-return r)))))))

(ert-deftest reduce-destructuring ()
  (should (equal '(4 6)
                 (eval (quote (loopy (list i '((1 2) (3 4)))
                                     (reduce (r1 r2) i #'+ :init 0)
                                     (finally-return r1 r2))))))

  (should (equal '((1 3) (2 4))
                 (eval (quote (loopy (list i '([(1) (2)] [(3) (4)]))
                                     (reduce [r1 r2] i #'append)
                                     (finally-return r1 r2))))))

  (should (equal '((1 3) (2 4))
                 (eval (quote (loopy (list i '([(1) (2)] [(3) (4)]))
                                     (reducing [r1 r2] i #'append)
                                     (finally-return r1 r2)))))))

(ert-deftest reduce-implicit ()
  (should (= 6
             (eval (quote (loopy (list i '(1 2 3))
                                 (reduce i #'+ :init 0))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy (list i '((1) (2) (3)))
                                     (reduce i #'append))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy (list i '((1) (2) (3)))
                                     (reducing i #'append)))))))

;;;;; Set Accum
(ert-deftest set-accum-setup ()
  (should (eq 'loopy--parse-set-accum-command
              (loopy--get-command-parser 'set-accum)))
  (should (eq 'set-accum (loopy--get-true-name 'setting-accum))))

(ert-deftest set-accum ()
  (should (= 16 (eval (quote (loopy (list i '(1 2 3))
                                    (set-accum my-sum (+ my-sum i) :init 10)
                                    (finally-return my-sum))))))

  (should (equal '(3 2 1)
                 (eval (quote (loopy (list i '(1 2 3))
                                     (set-accum coll (cons i coll))
                                     (finally-return coll)))))))

(ert-deftest set-accum-implict ()
  (should (= 16 (eval (quote (loopy (list i '(1 2 3))
                                    (set-accum (+ loopy-result i) :init 10))))))

  (should (equal '(3 2 1)
                 (eval (quote (loopy (list i '(1 2 3))
                                     (set-accum (cons i loopy-result))))))))

(ert-deftest set-accum-destructuring ()
  (should (equal '(5 6)
                 (eval (quote (loopy (array elem [(1 . 2) (3 . 4) (5 . 6)])
                                     (set-accum (car . cdr) elem)
                                     (finally-return car cdr)))))))

;;;;; Sum
(ert-deftest sum ()
  (should (= 6
             (eval (quote (loopy (list i '(1 2 3))
                                 (sum s i)
                                 (finally-return s))))))
  (should (= 6
             (eval (quote (loopy (list i '(1 2 3))
                                 (summing s i)
                                 (finally-return s)))))))

(ert-deftest sum-destructuring ()
  (should (equal '(5 7 9)
                 (loopy (list elem '((1 2 3) (4 5 6)))
                        (sum (sum1 sum2 sum3) elem)
                        (finally-return sum1 sum2 sum3))))
  (should (equal '(5 7 9)
                 (loopy (list elem '((1 2 3) (4 5 6)))
                        (summing (sum1 sum2 sum3) elem)
                        (finally-return sum1 sum2 sum3)))))

(ert-deftest sum-implict ()
  (should (= 6
             (eval (quote (loopy (list i '(1 2 3))
                                 (sum i))))))
  (should (= 6
             (eval (quote (loopy (list i '(1 2 3))
                                 (summing i)))))))

;;;;; Union
(ert-deftest union ()
  ;; TODO: `union' currently has predictable behavior due to the `:at' position,
  ;;       but it might be worthwhile to remove that predictability for speed in
  ;;       the future.
  ;;
  ;; (should (null (cl-set-difference
  ;;                '(4 1 2 3)
  ;;                (eval (quote (loopy (list i '((1 2) (2 3) (3 4)))
  ;;                                    (union var i)))))))
  ;; (should (null (cl-set-difference
  ;;                '(4 1 2 3)
  ;;                (eval (quote (loopy (list i '((1 2) (2 3) (3 4)))
  ;;                                    (unioning var i)
  ;;                                    (finally-return var)))))))
  ;;
  ;; (should (null (cl-set-difference
  ;;                '(4 2 (1 1) 3)
  ;;                (eval (quote (loopy (list i '(((1 1) 2) ((1 1) 3) (3 4)))
  ;;                                    (unioning var i :test #'equal)
  ;;                                    (finally-return var))))
  ;;                :test #'equal)))
  ;;
  ;; ;; The resulting list should only have one element whose `car' is `a'.
  ;; (should (= 1 (cl-count-if (lambda (x) (eq (car x) 'a))
  ;;                           (eval (quote (loopy (array i [((a . 1)) ((a . 2))])
  ;;                                               (unioning var i :key #'car)
  ;;                                               (finally-return var)))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy (list i '((1 2) (2 3) (3 4)))
                                     (union var i)
                                     (finally-return var))))))

  (should (equal '(1 2 3 4)
                 (eval (quote (loopy (list i '((1 2) (2 3) (3 4)))
                                     (unioning var i)
                                     (finally-return var))))))

  (should (equal '((1 1) 2 3 4)
                 (eval (quote (loopy (list i '(((1 1) 2) ((1 1) 3) (3 4)))
                                     (unioning var i :test #'equal)
                                     (finally-return var))))))

  (should (equal '((1 1) 2 3 4)
                 (eval (quote (let ((func #'equal ))
                                (loopy (list i '(((1 1) 2) ((1 1) 3) (3 4)))
                                       (unioning var i :test func)
                                       (finally-return var)))))))

  (should (equal '((a . 1))
                 (eval (quote (loopy (array i [((a . 1)) ((a . 2))])
                                     (unioning var i :key #'car)
                                     (finally-return var))))))

  (should (equal '((a . 1))
                 (eval (quote (let ((func #'car))
                                (loopy (array i [((a . 1)) ((a . 2))])
                                       (unioning var i :key func)
                                       (finally-return var))))))))

(ert-deftest union-destructuring ()
  ;; TODO: `union' currently has predictable behavior due to the `:at' position,
  ;;       but it might be worthwhile to remove that predictability for speed in
  ;;       the future.
  ;;
  ;; (should (null (cl-destructuring-bind (first second)
  ;;                   (eval (quote (loopy (array i [((1 2) (2 3))
  ;;                                                 ((1 2 3) (3 4))])
  ;;                                       (union (var1 var2) i :test #'equal))))
  ;;                 (or (clsetdifference first '(1 2 3))
  ;;                     (clsetdifference second '(2 3 4))))))
  (should (equal '((1 2 3) (2 3 4))
                 (eval (quote (loopy (array i [((1 2) (2 3))
                                               ((1 2 3) (3 4))])
                                     (union (var1 var2) i :test #'=)
                                     (finally-return var1 var2)))))))

(ert-deftest union-at ()
  (should (equal '((1 2) (3 2) (1 1))
                 (eval (quote (loopy (list i '(((1 2) (3 2)) ((1 1) (4 2))))
                                     (union i :at end :key #'cl-second))))))

  (should (equal '((1 2) (3 2) (4 2))
                 (eval (quote (loopy (list i '(((1 2) (3 2)) ((1 1) (4 2))))
                                     (union i :at end :key #'car))))))

  (should (equal '((4 2) (1 2) (3 2))
                 (eval (quote (loopy (list i '(((1 2) (3 2)) ((1 1) (4 2))))
                                     (union i :at start :key #'car))))))

  (should (equal '((4 2) (1 2) (3 2))
                 (eval (quote (loopy (list i '(((1 2) (3 2)) ((1 1) (4 2))))
                                     (union c i :at start :key #'car)
                                     (finally-return c))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy (list i '((1 2 3) (1 2 3)))
                                     (union i :test #'equal :at start))))))

  (should (equal '(1 2 3)
                 (eval (quote (loopy (list i '((1 2 3) (1 2 3)))
                                     (union c i :test #'equal :at start)
                                     (finally-return c)))))))

(ert-deftest union-end-tracking ()
  (should (equal '(1 2 3 4 5 6 7 8)
                 (eval (quote
                        (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
                          (loopy (flag split)
                                 (list i l1)
                                 (union i :at end)))))))

  (should (equal '(1 2 3 4 5 6 7 8)
                 (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
                   (loopy (flag -split)
                          (list i l1)
                          (union i :at end)))))

  (should (equal '(1 2 3 4 5 6)
                 (let ((l1 (list (list 1 2) (list 3 4) (list 4 3) (list 5 6))))
                   (loopy (list i l1)
                          (union coll i :at end)
                          (finally-return coll)))))

  (should (equal '(1 2 3 4 5 6 7)
                 (let ((l1 (list (list 1 2) (list 3 4) (list 4 3) (list 5 6))))
                   (loopy (list i l1)
                          (union coll i :at end)
                          (union coll (mapcar #'1+ i) :at end)
                          (finally-return coll)))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (loopy (list i '((1 2) (3 4) (5 6)))
                        (union i :at start)
                        (union (mapcar (lambda (x) (+ x 10)) i)
                               :at end))))

  (should (equal '(5 6 3 4 1 2 11 12 13 14 15 16)
                 (loopy (list i '((1 2) (3 4) (5 6)))
                        (union (mapcar (lambda (x) (+ x 10)) i)
                               :at end)
                        (union i :at start)))))

(ert-deftest union-not-destructive ()
  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (list i l1) (union coll i :at start))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (list i l1) (union coll i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (list i l1) (union i :at start))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8)))))

  (should (equal (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
                   (loopy (list i l1) (union i :at end))
                   l1)
                 '((1 2) (3 4) (5 6) (7 8))))

  (let ((l1 (list (list 1 2) (list 3 4) (list 5 6) (list 7 8))))
    (loopy (flag split) (list i l1) (union i :at end))
    (should (equal l1 '((1 2) (3 4) (5 6) (7 8))))))

;;;;; Vconcat
(ert-deftest vconcat ()
  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy (list elem '([1 2 3 4 5 6]
                                                  [7 8 9 10 11 12]))
                                     (vconcat v elem)
                                     (finally-return v))))))

  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy (list elem '([1 2 3 4 5 6]
                                                  [7 8 9 10 11 12]))
                                     (vconcating v elem)
                                     (finally-return v)))))))

(ert-deftest vconcat-destructuring ()
  (should (equal '([1 2 3 7 8 9] [4 5 6 10 11 12])
                 (eval (quote (loopy (list elem '(([1 2 3] [4 5 6])
                                                  ([7 8 9] [10 11 12])))
                                     (vconcat (v1 v2) elem)
                                     (finally-return v1 v2))))))

  (should (equal '([1 2 3 7 8 9] [4 5 6 10 11 12])
                 (eval (quote (loopy (list elem '(([1 2 3] [4 5 6])
                                                  ([7 8 9] [10 11 12])))
                                     (vconcating (v1 v2) elem)
                                     (finally-return v1 v2)))))))

(ert-deftest vconcat-implict ()
  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy (list elem '([1 2 3 4 5 6]
                                                  [7 8 9 10 11 12]))
                                     (vconcat elem))))))
  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy (list elem '([1 2 3 4 5 6]
                                                  [7 8 9 10 11 12]))
                                     (vconcating elem)))))))

(ert-deftest vconcat-at ()
  (should (equal [7 8 9 10 11 12 1 2 3 4 5 6]
                 (eval (quote (loopy (list elem '([1 2 3 4 5 6]
                                                  [7 8 9 10 11 12]))
                                     (vconcat v elem :at start)
                                     (finally-return v))))))

  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy (list elem '([1 2 3 4 5 6]
                                                  [7 8 9 10 11 12]))
                                     (vconcat v elem :at end)
                                     (finally-return v))))))

  (should (equal [7 8 9 10 11 12 1 2 3 4 5 6]
                 (eval (quote (loopy (list elem '([1 2 3 4 5 6]
                                                  [7 8 9 10 11 12]))
                                     (vconcat elem :at start))))))

  (should (equal [1 2 3 4 5 6 7 8 9 10 11 12]
                 (eval (quote (loopy (list elem '([1 2 3 4 5 6]
                                                  [7 8 9 10 11 12]))
                                     (vconcat elem :at end))))))

  (should (equal '([1 2 3 7 8 9] [4 5 6 10 11 12])
                 (eval (quote (loopy (list elem '(([1 2 3] [4 5 6])
                                                  ([7 8 9] [10 11 12])))
                                     (vconcat (v1 v2) elem :at end)
                                     (finally-return v1 v2))))))

  (should (equal '([7 8 9 1 2 3] [10 11 12 4 5 6])
                 (eval (quote (loopy (list elem '(([1 2 3] [4 5 6])
                                                  ([7 8 9] [10 11 12])))
                                     (vconcat (v1 v2) elem :at start)
                                     (finally-return v1 v2)))))))

;;;;; Miscellaneous
;;; Control Flow
;;;; Conditionals
;;;;; If
(ert-deftest if ()
  (should (equal '((2 4) (1 3))
                 (loopy (list i '(1 2 3 4))
                        (if (cl-evenp i)
                            (collect evens i)
                          (collect odds i))
                        (finally-return evens odds)))))

;;;;; When
;; (ert-deftest basic-when-parse ()
;;   (should (equal (loopy--parse-conditional-forms 'when 't '((do (+ 1 1))))
;;                  '((loopy--main-body when t (progn (+ 1 1)))))))

(ert-deftest recursive-when-test ()
  (should (equal
           (eval (quote (loopy (list i (number-sequence 1 10))
                               (list j '(1 2 3 6 7 8))
                               (when (cl-evenp i)
                                 (when (> j i)
                                   (return (cons j i)))))))
           '(6 . 4))))

(ert-deftest when-multiple-subcommands ()
  (should (equal '(2 (1 3))
                 (loopy (with (counter 0))
                        (list i '(1 2 3))
                        (when (cl-oddp i)
                          (collect odds i)
                          (do (cl-incf counter)))
                        (finally-return counter odds)))))

(ert-deftest multi-when-prepend-test ()
  (should
   (string=
    (eval (quote (loopy (with (first-var 2)
                              (second-var 3))
                        (seq el [1 2 3 4 5 6 7])
                        ;; Could also use (do (cond ...)).
                        (when (zerop (mod el first-var))
                          (push-into msg-coll (format "Multiple of 2: %d" el)))
                        (when (zerop (mod el second-var))
                          (push-into msg-coll (format "Multiple of 3: %d" el)))
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
    (eval (quote (loopy (with (first-var 2)
                              (second-var 3))
                        (seq el [1 2 3 4 5 6 7])
                        ;; Could also use (do (cond ...)).
                        (unless (zerop (mod el first-var))
                          (push-into msg-coll (format "Not multiple of 2: %d" el)))
                        (unless (zerop (mod el second-var))
                          (push-into msg-coll (format "Not multiple of 3: %d" el)))
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
;;                                             (do (message "hi")))
;;                                            ((= b 2)
;;                                             (return 5))))
;;                  '((loopy--main-body cond
;;                                      ((= a 1) (progn (message "hi")))
;;                                      ((= b 2) (cl-return-from nil 5)))))))

(ert-deftest cond ()
  (should (equal (eval
                  (quote
                   (loopy (list i (number-sequence 0 5))
                          (cond ((cl-evenp i)
                                 (push-into evens i)
                                 (push-into holding-list evens))
                                (t (push-into odds i)))
                          (finally-return (list evens odds holding-list)))))
                 '((4 2 0) (5 3 1) ((4 2 0) (2 0) (0))))))

;;;; Exiting the Loop Early
;;;;; Leave
(ert-deftest leave ()
  (should (equal '(1)
                 (eval (quote (loopy (list i '(1 2))
                                     (collect i)
                                     (leave)))))))

;;;;; Leave From
(ert-deftest leave-from ()
  (should (equal '([1 2 3])
                 (eval (quote (loopy outer
                                     (list i '([1 2 3] [4 5 6]))
                                     (loopy (array j i)
                                            (when (= j 5)
                                              (leave-from outer)))
                                     (collect i)))))))

;;;;; Return
(ert-deftest return ()
  (should (= 6 (eval (quote (loopy (with  (j 0))
                                   (do (cl-incf j))
                                   (when (> j 5)
                                     (return j))))))))

;;;;; Return From
(ert-deftest return-from-single-loop ()
  (should (= 6
             (eval (quote (loopy my-loop
                                 (list i (number-sequence 1 10))
                                 (when (> i 5)
                                   (return-from my-loop i))))))))

(ert-deftest return-from-outer-loop ()
  (should
   (= 6
      (eval (quote (loopy outer
                          ;; Could use ‘sum’ command, but don’t want dependencies.
                          (with (sum 0))
                          (list sublist '((1 2 3 4 5) (6 7 8 9) (10 11)))
                          (do (loopy (list i sublist)
                                     (do (setq sum (+ sum i)))
                                     (when (> sum 15)
                                       (return-from outer i))))))))))

(ert-deftest return-commands-multiple-values ()
  (should
   (and
    (equal '(1 2 3 4)
           (eval (quote (loopy (return 1 2 3 4)))))
    (equal '(1 2 3 4)
           (eval (quote (loopy my-loop (return-from my-loop 1 2 3 4))))))))

;;;;; Skip
(ert-deftest skip ()
  (should (cl-every #'cl-oddp
                    (eval (quote (loopy (seq i (number-sequence 1 10))
                                        (when (cl-evenp i)
                                          (skip))
                                        (push-into my-collection i)
                                        (finally-return (nreverse my-collection))))))))

(ert-deftest skip-from ()
  (should (equal '((1 2 3) (7 8 9))
                 (eval (quote (loopy (named outer)
                                     (array i [(1 2 3) (4 5 6) (7 8 9)])
                                     (loopy (list j i)
                                            (if (= 5 j)
                                                (skip-from outer)))
                                     (collect i)))
                       t))))

;;;;; While
(ert-deftest while ()
  (should (equal '(1 2)
                 (eval (quote (loopy (list i '(1 2 3 4 5 6))
                                     (while (< i 3))
                                     (collect i)))))))

;;;;; Until
(ert-deftest until ()
  (should (equal '(1 2 3)
                 (eval (quote (loopy (list i '(1 2 3 4 5 6))
                                     (until (> i 3))
                                     (collect i)))))))

;;;;; Always
(ert-deftest always ()
  (should (equal t (eval (quote (loopy (list i '(1 2 3 4 5 6))
				       (always (< i 7)))))))

  (should (null
	   (eval (quote (loopy (list i '(1 2 3 4 5 6))
			       (always (> i 7))))))))

(ert-deftest multiple-always ()
  (should (equal t (eval (quote (loopy (list i '(1 3 5 7))
                                       (always (cl-oddp i))
                                       (always (< i 10))))))))

(ert-deftest always-var ()
  (should (equal 4 (lq (list i '(1 2 3))
                       (always i (numberp i) (1+ i) :into test-var)
                       (finally-return test-var)))))

;;;;; Never
(ert-deftest never ()
  (should (equal nil
		 (eval (quote (loopy (list i '(1 2 3 4 5 6))
			             (never (> i 0)))))))

  (should (equal t
		 (eval (quote (loopy (list i '(1 2 3 4 5 6))
			             (never (< i 0))))))))

(ert-deftest multiple-never ()
  (should (equal t (eval (quote (loopy (list i '(1 3 5 7))
                                       (never (cl-evenp i))
                                       (never (> i 10))))))))

(ert-deftest always-and-never ()
  ;; A `never' command should not stop `always' from ultimately setting the
  ;; return value to 2.
  (should (= 2
             (eval (quote (loopy (repeat 2)
                                 (always 2)
                                 (never nil)))))))

(ert-deftest never-var ()
  (should (equal t (lq (list i '(1 2 3))
                       (never (not (numberp i)) nil :into test-var)
                       (finally-return test-var)))))

;;;;; Thereis
(ert-deftest thereis ()
  (should (= 6 (eval (quote (loopy (list i '(1 2 3 4 5 6))
			           (thereis (and (> i 5) i)))))))

  (should (= 9 (eval (quote (loopy (list i (number-sequence 1 9))
			           (thereis (and (> i 8) i)))))))

  (should (null (eval (quote (loopy (list i '(1 2 3 4 5 6))
			            (thereis (> i 7))))))))

(ert-deftest thereis-incompatiblility ()
  (should-error (lq (list i '(1 2 3))
                    (always i)
                    (thereis i)))

  (should-error (lq (list i '(1 2 3))
                    (never i)
                    (thereis i)))

  (should-error (lq (list i '(1 2 3))
                    (always i :into test)
                    (thereis i :into test)))

  (should-error (lq (list i '(1 2 3))
                    (never i :into test)
                    (thereis i :into test))))

(ert-deftest thereis-diff-var-compatibility ()
  (should (equal '(1 11)
                 (lq (list i '(1 2 3))
                     (always i :into test1)
                     (thereis (+ i 10) :into test2))))

  (should (equal '(t 11)
                 (lq (list i '(1 2 3))
                     (never (not (numberp i)) :into test1)
                     (thereis (+ i 10) :into test2)))))

;; finding
(ert-deftest find ()
  (should (= 3 (eval (quote (loopy (list i '(1 2 3))
			           (find i (> i 2)))))))

  (should-not (eval (quote (loopy (list i '(1 2 3))
			          (find i (> i 4))))))

  (should (= 0 (eval (quote (loopy (list i '(1 2 3))
			           (find i (> i 4) :on-failure 0))))))

  (should (= 3 (eval (quote (loopy (list i '(1 2 3))
			           (finding i (> i 2)))))))

  (should-not (eval (quote (loopy (list i '(1 2 3))
			          (finding i (> i 4))))))

  (should (= 0 (eval (quote (loopy (list i '(1 2 3))
			           (finding i (> i 4) :on-failure 0))))))

  (should (= 2 (eval (quote (loopy (list i '(1 2 3))
                                   (finding found i (= i 2))
                                   (finally-return found))))))

  (should (equal "not found"
                 (eval (quote (loopy (list i '(1 2 3))
                                     (finding whether-found i (> i 4)
                                              :on-failure "not found")
                                     (finally-return whether-found)))))))

;;; Custom Commands
(ert-deftest custom-command-sum ()
  (let ((loopy-command-parsers
         (map-insert loopy-command-parsers 'target-sum #'my-loopy-sum-command)))

    (cl-defun my-loopy-sum-command ((_ target &rest items))
      "Set TARGET to the sum of ITEMS."
      `((loopy--iteration-vars (,target nil))
        (loopy--main-body (setq ,target (apply #'+ (list ,@items))))))

    (should (= 6
               (eval (quote (loopy  (target-sum my-target 1 2 3)
                                    (return nil)
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
                    (loopy (list i (number-sequence 1 9)) (always (< i 10)))))

             ;; Two conditions: => nil
             (not (eval (quote
                         (loopy (list i (number-sequence 1 9))
                                (list j '(2 4 6 8 9))
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
       (loopy (with (regexp "Documents")
	            (base-dir (expand-file-name "~/")))
	      (list file (directory-files base-dir))
	      (expr full-path (expand-file-name file base-dir))))
     (mu4e:other-path)
     ;; If an `nreverse' goes bad, then the function value of `mu4e:other-path'
     ;; might be changed (somehow), which causes an error.
     (eq nil (mu4e:other-path)))))

;;; Custom Aliases
(ert-deftest custom-alias-flag ()
  (let ((loopy-aliases (map-copy loopy-aliases)))
    (loopy-defalias f flag)
    (should (equal '((1) (2))
                   (eval (quote (loopy (f split)
                                       (list i '(1))
                                       (collect i)
                                       (collect (1+ i)))))))))

(ert-deftest custom-aliases-with ()
  (let ((loopy-aliases ))
    (loopy-defalias as with)
    (should (= 1
               (eval (quote (loopy (as (a 1))
                                   (return a))))))))

(ert-deftest custom-aliases-without ()
  (eval (quote (let ((loopy-aliases (map-copy loopy-aliases)))
                 (loopy-defalias 'ignore 'without)
                 (should (= 5 (let ((a 1)
                                    (b 2))
                                (loopy (ignore a b)
                                       (repeat 1)
                                       (expr a 2)
                                       (expr b 3))
                                (+ a b))))))))

(ert-deftest custom-aliases-before-do ()
  (eval (quote (let ((loopy-aliases (map-copy loopy-aliases)))
                 (loopy-defalias 'precode 'before-do)
                 (should (= 7 (loopy (with (i 2))
                                     (precode (setq i 7))
                                     (return i))))))))

(ert-deftest custom-aliases-after-do ()
  (eval (quote (let ((loopy-aliases (map-copy loopy-aliases)))
                 (loopy-defalias postcode after-do)
                 (should (loopy (with (my-ret nil))
                                (list i '(1 2 3 4))
                                (postcode (setq my-ret t))
                                (finally-return my-ret)))))))

(ert-deftest custom-aliases-finally-do ()
  (eval (quote (let ((loopy-aliases (map-copy loopy-aliases)))
                 (loopy-defalias 'fd finally-do)
                 (should
                  (= 10
                     (let (my-var)
                       (loopy (list i (number-sequence 1 10))
                              (fd (setq my-var i)))
                       my-var)))))))

(ert-deftest custom-aliases-finally-return ()
  (eval (quote (let ((loopy-aliases  (map-copy loopy-aliases)))
                 (loopy-defalias fr 'finally-return)
                 (should (= 10
                            (loopy (list i (number-sequence 1 10))
                                   (fr i))))))))

(ert-deftest custom-aliases-list ()
  (let ((loopy-aliases nil))
    (should (progn
              (loopy-defalias l list)
              t))
    (should (progn
              (loopy-defalias a 'array)
              t))
    (should (equal '((1 . 4) (2 . 5) (3 . 6))
                   (eval (quote (loopy (l i '(1 2 3))
                                       (a j [4 5 6])
                                       (collect (cons i j)))))))))

;;; Clean Up Variables
(ert-deftest clean-stack-variables ()
  (let (loopy--known-loop-names
        loopy--accumulation-places
        loopy--at-instructions
        loopy--accumulation-list-end-vars
        loopy--accumulation-variable-info)
    (should (equal '((3 4) (1 2) 1 2 3 4)
                   (eval (quote (loopy my-loop
                                       (array i [(1 2) (3 4)])
                                       (collect i :at start)
                                       (loopy inner
                                              (list j i)
                                              (at my-loop (collect j :at end))))))))
    (should-not (or loopy--known-loop-names
                    loopy--accumulation-places
                    loopy--at-instructions
                    loopy--accumulation-list-end-vars
                    loopy--accumulation-variable-info))))

(ert-deftest clean-var-variables ()
  (eval (quote (let ((i  'good))
                 (loopy (list i '(1 2 3)))
                 (eq i 'good)))
        t)

  (eval (quote (let ((i  'good))
                 (loopy (cycle 1)
                        (set i 'bad))
                 (eq i 'good)))
        t))


;; Local Variables:
;; End:
;; LocalWords:  destructurings backquote
