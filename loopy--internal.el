;; -*- lexical-binding: t; -*-
(require 'loopy)
(require 'loopy-iter)
(require 'loopy-iter)

(defvar loopy--mode nil)

(defmacro loopy--internal--def-special-processor (name &rest body)
  "Create a processor for the special macro argument NAME and its aliases.

BODY is the arguments to the macro `loopy' or `loopy-iter'.
Each processor should set a special variable (such as those
in `loopy--variables') and return a new BODY with its
own argument removed.

Variables available:
- `all-names' is all of the names found
- `matching-args' are all arguments that match elements in
  `all-names'
- `arg-value' is the value of the arg if there is only one match
- `arg-name' the name of the arg found if there is only one match"
  (declare (indent defun))
  `(defun ,(intern (format "loopy--internal--process-special-arg-%s" name))
       (mode body)
     ,(format "Process the special macro argument `%s' and its aliases.

Returns BODY without the `%s' argument."
              name name)
     (ignore mode)
     (let* ((all-names (loopy--get-all-names (quote ,name) :from-true t))
            (bare-names (cl-intersection all-names
                                         loopy-iter-bare-special-macro-arguments))
            (matching-args)
            (new-body))
       (dolist (expr body)
         (if (and (consp expr)
                  (or (memq (cl-first expr) bare-names)
                      (and (memq (cl-first expr) loopy-iter-keywords)
                           (memq (cl-second expr) all-names))))
             (push expr matching-args)
           (push expr new-body)))
       (when matching-args
         (if (cdr matching-args)
             (error "Conflicting arguments: %s" matching-args)
           (let ((arg (car matching-args))
                 (arg-name)
                 (arg-value))
             ;; TODO: Probably a better way to do this that doesn't
             ;; involve checking twice.
             (if (memq (cl-first arg) bare-names)
                 (loopy-setq (arg-name . arg-value) arg)
               (loopy-setq (_ arg-name . arg-value) arg))
             (ignore arg-name)
             ,@body)))
       (nreverse new-body))))

(defun loopy--internal--process-special-arg-loop-name (mode body)
  "Process BODY and the loop name listed therein."
  (ignore mode)
  (let* ((names)
         (new-body)
         (all-sma-names (loopy--get-all-names 'named :from-true t))
         (all-sma-bare-names
          (cl-intersection all-sma-names
                           loopy-iter-bare-special-macro-arguments)))
    (dolist (arg body)
      (cond ((symbolp arg)
             (push arg names))
            ((memq (car-safe arg) all-sma-bare-names)
             (if (/= 2 (length arg))
                 (error "Wrong number of arguments for loop name: %s" arg)
               (push (cl-second arg) names)))
            ((and (memq (car-safe arg) loopy-iter-keywords)
                  (memq (cl-second arg) all-sma-names))
             (if (/= 3 (length arg))
                 (error "Wrong number of arguments for loop name: %s" arg)
               (push (cl-third arg) names)))
            (t (push arg new-body))))
    (if (> (length names) 1)
        (error "Conflicting loop names: %s" names)
      (let ((loop-name (cl-first names))) ; Symbol or `nil'.
        (setq loopy--loop-name loop-name
              loopy--skip-tag-name (loopy--produce-skip-tag-name loop-name)
              loopy--non-returning-exit-tag-name
              (loopy--produce-non-returning-exit-tag-name loop-name))
        ;; Set up the stack-maps.
        (push loopy--loop-name loopy--known-loop-names)
        (push (list loopy--loop-name) loopy--accumulation-places)
        ;; Return non-name args.
        (nreverse new-body)))))

(loopy--internal--def-special-processor with
  ;; Note: These values don't have to be used literally, due to
  ;;       destructuring.
  (loopy (list binding arg-value)
         (collect
          (cond ((symbolp binding)      (list binding nil))
                ((= 1 (length binding)) (list (cl-first binding) nil))
                (t                       binding)))
         (finally-do
          (setq loopy--with-vars loopy-result))))


(loopy--internal--def-special-processor finally-return
  (setq loopy--final-return (if (= 1 (length arg-value))
                                (cl-first arg-value)
                              (cons 'list arg-value))))

(loopy--internal--def-special-processor flag
  ;; Process any flags passed to the macro.  In case of conflicts, the
  ;; processing order is:
  ;;
  ;; 1. Flags in `loopy-default-flags'.
  ;; 2. Flags in the `flag' macro argument, which can undo the first group.
  ;; (mapc #'loopy--apply-flag loopy-default-flags)
  (mapc #'loopy--apply-flag arg-value))

(loopy--internal--def-special-processor without
  (setq loopy--without-vars arg-value))

(loopy--internal--def-special-processor accum-opt
  (pcase-dolist ((or `(,var ,pos) var) arg-value)
    (push var loopy--optimized-accum-vars)
    (when pos
      (loopy--update-accum-place-count loopy--loop-name var pos 1.0e+INF))))

(loopy--internal--def-special-processor wrap
  (setq loopy--wrapping-forms arg-value))

(loopy--internal--def-special-processor before-do
  (setq loopy--before-do arg-value))

(loopy--internal--def-special-processor after-do
  (setq loopy--after-do arg-value))

(loopy--internal--def-special-processor finally-do
  (setq loopy--final-do arg-value))

(loopy--internal--def-special-processor finally-protect
  (setq loopy--final-protect arg-value))

(defmacro loopy--mode-let* (branches &rest body)
  "Conditionally bind variables based on `loopy--mode'.
BRANCHES is a alist of mode-bindings pairs.  BODY is the code
to which bindings applies."
  (declare (indent 1))
  `(cl-ecase loopy--mode
     ,@(cl-loop for (sym . bindings) in branches
                collect `(,sym (let* ,bindings
                                 ,@body)))))

(defun loopy--internal--name-parser-conclusion-alist ()
  "Combine `loopy-aliases' and `loopy-command-parsers' into final alist of command name-parser pairs."
  (append (cl-loop for (orig . aliases) in loopy-aliases
                   for parser = (map-elt loopy-command-parsers orig)
                   when parser
                   append (cl-loop for alias in aliases
                                   collect (cons alias parser)))
          loopy-command-parsers))

(defun loopy--internal (mode body)
  "Allows embedding loop commands in arbitrary code within this macro's body.

This can be more flexible than using the `do' loop command in
`loopy'.

Loop commands are expanded like macros inside the body.  Hence,
it's possible for the names of loop commands to overshadow other
definitions.  To avoid this, see the user options
`loopy-iter-bare-commands', `loopy-iter-bare-special-macro-arguments', and
`loopy-iter-keywords'.

See the Info node `(loopy)The loopy-iter Macro' for information
on how to use `loopy-iter'.  See the Info node `(loopy)' for how
to use `loopy' in general."
  ;; We expand the code in BODY in two passes.  The macro works like this:
  ;;
  ;; 1) Like normal, process the special macro arguments.
  ;;
  ;; 2) Set up the environments used for macro expansion.  These are alists of
  ;;    macro names and functions that process the macro arguments.  The
  ;;    functions only receive the arguments, not the name.
  ;;
  ;; 3) Parse commands and process the initial resulting instructions.  The
  ;;    result is a new body with the commands expanded, but the optimized
  ;;    accumulations incomplete and `at' instructions are not finished.
  ;;
  ;; 4) Parse the optimized accumulations and process the `at' instructions the
  ;;    resulted from processing the commands and instructions in Step 3.
  ;;
  ;; 5) Set `loopy--main-body' to the now expanded expressions (as a list, no
  ;;    `macroexpand-progn').
  ;;
  ;; 6) Then we manipulate the variables and build the loop like normal, as we
  ;;    do in `loopy'.

  (loopy--wrap-variables-around-body
   (let ((loopy--mode mode))

     (when (null loopy--mode)
       (error "`loopy-mode' unexpectedly nil"))

     (mapc #'loopy--apply-flag loopy-default-flags)

     (setq body (thread-last body
                             (loopy--internal--process-special-arg-loop-name loopy--mode)
                             (loopy--internal--process-special-arg-flag loopy--mode)
                             (loopy--internal--process-special-arg-with loopy--mode)
                             (loopy--internal--process-special-arg-without loopy--mode)
                             (loopy--internal--process-special-arg-accum-opt loopy--mode)
                             (loopy--internal--process-special-arg-wrap loopy--mode)
                             (loopy--internal--process-special-arg-before-do loopy--mode)
                             (loopy--internal--process-special-arg-after-do loopy--mode)
                             (loopy--internal--process-special-arg-finally-do loopy--mode)
                             (loopy--internal--process-special-arg-finally-return loopy--mode)
                             (loopy--internal--process-special-arg-finally-protect loopy--mode)))

     (let ((name-parser-alist (loopy--internal--name-parser-conclusion-alist)))
       (loopy--with-protected-stack
        (loopy--mode-let* ((basic . ((suppressed-expanders (mapcar #'list loopy--suppressed-macros))
                                     (loopy-iter--command-parsers (or loopy-iter--command-parsers
                                                                      (append loopy-iter-overwritten-command-parsers
                                                                              loopy-command-parsers)))
                                     (loopy-iter--non-main-body-instructions)
                                     (loopy-iter--level 0)
                                     (command-env
                                      (cl-loop for (sym . func) in (loopy--internal--name-parser-conclusion-alist)
                                               ;; `cl-loop' modifies vars, so
                                               ;; need to capture value.
                                               collect (let ((sym2 sym)
                                                             (func2 func))
                                                         (cons sym2
                                                               (lambda (&rest args)
                                                                 (loopy--bind-main-body (main other)
                                                                     ;; Bind here in case a command required to be
                                                                     ;; in the top level is found in an expression
                                                                     ;; while parsing an actual top-level command.
                                                                     (let* ((loopy-iter--level (1+ loopy-iter--level))
                                                                            (loopy--in-sub-level (> loopy-iter--level 1)))
                                                                       (funcall func2 (cons sym2 args)))
                                                                   (push other loopy-iter--non-main-body-instructions)
                                                                   `(loopy--basic-quote (quote ,(macroexp-progn main)))))))))
                                     (first-pass-env `((loopy--basic-quote . nil)
                                                       ,@suppressed-expanders
                                                       ,@command-env))
                                     (second-pass-env `(;; Identify second
                                                        ;; version of optimized
                                                        ;; accumulation.
                                                        (loopy--optimized-accum . loopy--expand-optimized-accum)
                                                        (loopy--basic-quote . cadr)
                                                        ,@suppressed-expanders))))
                           (iter . ((suppressed-expanders (mapcar #'list loopy-iter-suppressed-macros))
                                    (loopy-iter--command-parsers (or loopy-iter--command-parsers
                                                                     (append loopy-iter-overwritten-command-parsers
                                                                             loopy-command-parsers)))
                                    (loopy-iter--non-main-body-instructions)
                                    (loopy-iter--level 0)
                                    (command-env
                                     (append (cl-loop for keyword in loopy-iter-keywords
                                                      collect (cons keyword
                                                                    (lambda (&rest args)
                                                                      (loopy--bind-main-body (main other)
                                                                          ;; Bind here in case a command required to be
                                                                          ;; in the top level is found in an expression
                                                                          ;; while parsing an actual top-level command.
                                                                          (let* ((loopy-iter--level (1+ loopy-iter--level))
                                                                                 (loopy--in-sub-level (> loopy-iter--level 1)))
                                                                            (loopy-iter--parse-command args))
                                                                        (push other loopy-iter--non-main-body-instructions)
                                                                        (macroexp-progn main)))))
                                             (cl-loop for command in loopy-iter-bare-commands
                                                      collect (cons command
                                                                    ;; Expanding functions do not receive the head
                                                                    ;; of the expression, only the arguments, so
                                                                    ;; we use a lexical lambda to include that
                                                                    ;; information.
                                                                    (let ((cmd command))
                                                                      (lambda (&rest args)
                                                                        (loopy--bind-main-body (main other)
                                                                            ;; Bind here in case a command required to
                                                                            ;; be in the top level is found in an
                                                                            ;; expression while parsing an actual
                                                                            ;; top-level command.
                                                                            (let* ((loopy-iter--level (1+ loopy-iter--level))
                                                                                   (loopy--in-sub-level (> loopy-iter--level 1)))
                                                                              (loopy-iter--parse-command (cons cmd args)))
                                                                          (push other loopy-iter--non-main-body-instructions)
                                                                          (macroexp-progn main))))))))
                                    (common-env `(,@suppressed-expanders
                                                  ,@command-env
                                                  ,@macroexpand-all-environment))
                                    (first-pass-env `((loopy--optimized-accum . loopy-iter--opt-accum-expand-val)
                                                      (loopy--optimized-accum-2 . nil)
                                                      ,@common-env))
                                    (second-pass-env `(;; Identify second version of optimized accumulation.
                                                       (loopy--optimized-accum-2 . loopy--expand-optimized-accum)
                                                       ,@common-env)))))

          (cl-labels (;; A wrapper to set `loopy--in-sub-level' correctly:
                      ;; If this is a known command, expand as normal.  The command
                      ;; parser will handle sub-level-ness.  Otherwise, while EXPR
                      ;; isn't a command itself, bind `loopy--in-sub-level' in case
                      ;; of any commands further down.
                      (iter-macroexpand-all (expr)
                        (if (map-elt command-env (car expr))
                            (macroexpand-all expr first-pass-env)
                          (let ((loopy-iter--level (1+ loopy-iter--level))
                                (loopy--in-sub-level t))
                            (macroexpand-all expr first-pass-env))))
                      ;; Process body, insert data for optimized accumulations,
                      ;; then process the other instructions:
                      (first-pass (body)
                        (prog1
                            (mapcar #'iter-macroexpand-all body)
                          (loopy--process-instructions
                           (thread-last loopy-iter--non-main-body-instructions
                                        nreverse
                                        (apply #'append)))))
                      ;; Expand the optimized accumulation variables,
                      ;; then process the `at' instructions for this loop:
                      (second-pass (body)
                        (prog1
                            (mapcar (lambda (expr) (macroexpand-all expr second-pass-env))
                                    body)
                          (loopy--process-instructions (map-elt loopy--at-instructions
                                                                loopy--loop-name)
                                                       :erroring-instructions
                                                       '(loopy--main-body)))))
            (setq loopy--main-body
                  (thread-first body
                                first-pass
                                second-pass)))

          ;; Make sure the order-dependent lists are in the correct order.
          (loopy--correct-var-structure :exclude-main-body t)

          ;; Produce the expanded code, based on the `let'-bound variables.
          (loopy--expand-to-loop)))))))

