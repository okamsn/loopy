;;; loopy.el --- A looping macro -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: November 2020
;; URL: https://github.com/okamsn/loopy
;; Version: 0.9.1
;; Package-Requires: ((emacs "27.1") (map "3.0") (seq "2.22"))
;; Keywords: extensions
;; LocalWords:  Loopy's emacs

;;; Disclaimer:
;; This file is not part of GNU Emacs.
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; The `loopy' macro is used to generate code for a loop, similar to `cl-loop'.
;; Unlike `cl-loop', `loopy' uses symbolic expressions instead of "clauses".
;;
;; A simple usage of `cl-loop':
;;
;;     (cl-loop for i from 1 to 10
;;              if (cl-evenp i) collect i into evens
;;              else collect i into odds
;;              end ; This `end' keyword is optional here.
;;              finally return (list odds evens))
;;
;; How it could be done using `loopy':
;;
;;     (loopy (numbers i 1 10)
;;            (if (cl-evenp i)
;;                (collect evens i)
;;              (collect odds i))
;;            (finally-return odds evens))
;;
;; `loopy' supports destructuring for iteration commands like `list' and
;; accumulation commands like `sum' or `collect'.
;;
;;     ;; Summing the nth elements of arrays:
;;     ;; => (8 10 12 14 16 18)
;;     (loopy (list (list-elem1 list-elem2)
;;                  '(([1 2 3] [4 5 6])
;;                    ([7 8 9] [10 11 12])))
;;            (sum [sum1 sum2 sum3] list-elem1)
;;            (sum [sum4 sum5 sum6] list-elem2)
;;            (finally-return sum1 sum2 sum3 sum4 sum5 sum6))
;;
;;     ;; Or, more simply:
;;     ;; => (8 10 12 14 16 18)
;;     (loopy (list list-elem '(([1 2 3] [4 5 6])
;;                              ([7 8 9] [10 11 12])))
;;            (sum ([sum1 sum2 sum3] [sum4 sum5 sum6])
;;                 list-elem)
;;            (finally-return sum1 sum2 sum3 sum4 sum5 sum6))
;;
;;     ;; Separate the elements of sub-list:
;;     ;; => ((1 3) (2 4))
;;     (loopy (list i '((1 2) (3 4)))
;;            (collect (elem1 elem2) i)
;;            (finally-return elem1 elem2))
;;
;; The `loopy' macro is configurable and extensible.  In addition to writing
;; one's own "loop commands" (such as `list' in the example below), by using
;; "flags", one can choose whether to instead use `pcase-let', `seq-let', or
;; even the Dash library for destructuring.
;;
;;     ;; Use `pcase' to destructure array elements:
;;     ;; => ((1 2 3 4) (10 12 14) (11 13 15))
;;     (loopy (flag pcase)
;;            (array (or `(,car . ,cdr) digit)
;;                   [1 (10 . 11) 2 (12 . 13) 3 4 (14 . 15)])
;;            (if digit
;;                (collect digits digit)
;;              (collect cars car)
;;              (collect cdrs cdr))
;;            (finally-return digits cars cdrs))
;;
;;     ;; Using the default destructuring:
;;     ;; => ((1 2 3 4) (10 12 14) (11 13 15))
;;     (loopy (array elem [1 (10 . 11) 2 (12 . 13) 3 4 (14 . 15)])
;;            (if (numberp elem)
;;                (collect digits elem)
;;              (collect (cars . cdrs) elem))
;;            (finally-return digits cars cdrs))
;;
;; Variables like `cars', `cdrs', and `digits' in the example above are
;; automatically `let'-bound so as to not affect code outside of the loop.
;;
;; `loopy' has arguments for binding (or not binding) variables, executing code
;; before/after the loop, executing code only if the loop completes, and for
;; setting the macro's return value (default `nil').  This is in addition to the
;; looping features themselves.
;;
;; All of this makes `loopy' a useful and convenient choice for looping and
;; iteration.
;;
;; That being said, Loopy is not yet feature complete.  Please request features
;; or report problems in this project’s issues tracker
;; (<https://github.com/okamsn/loopy/issues>).  While most cases are covered,
;; full feature parity with some of the more niche uses of `cl-loop' is still
;; being worked on.
;;
;; For more information, including the full list of loop commands and how to
;; extend the macro, see this package's comprehensive Info documentation under
;; the Info node `(loopy)'.

;;; Code:

(require 'cl-lib)
(require 'gv)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'loopy-misc)
(require 'loopy-commands)
(require 'loopy-vars)

(defvar loopy-iter--lax-naming) ; A flag defined in file "loopy-iter.el".

;;;; Built-in flags
;;;;; Split
(defun loopy--enable-flag-split ()
  "Set `loopy-split-implied-accumulation-results' to t inside the loop."
  (setq loopy--split-implied-accumulation-results t))

(defun loopy--disable-flag-split ()
  "Set `loopy-split-implied-accumulation-results' to t inside the loop."
  ;; Currently redundant, but leaves room for possibilities.
  (if loopy--split-implied-accumulation-results
      (setq loopy--split-implied-accumulation-results nil)))

(dolist (flag '(split +split))
  (cl-callf map-insert loopy--flag-settings flag #'loopy--enable-flag-split))

(cl-callf map-insert loopy--flag-settings '-split #'loopy--disable-flag-split)

;;;;;; Default
;; It doesn't make sense to allow the disabling of this one.
(defun loopy--enable-flag-default ()
  "Set `loopy' behavior back to its default state for the loop."
  (setq loopy--split-implied-accumulation-results nil
        loopy--destructuring-for-with-vars-function
        #'loopy--destructure-for-with-vars-default
        loopy--destructuring-accumulation-parser
        #'loopy--parse-destructuring-accumulation-command
        loopy-iter--lax-naming nil))

(cl-callf map-insert loopy--flag-settings 'default #'loopy--enable-flag-default)

;;;; Important Variables
(defvaralias 'loopy-first-iteration-p 'loopy-first-iteration)

(defvar loopy-first-iteration nil
  "Whether this is the first cycle of the current loop.")

;;;; Miscellaneous and Utility Functions
(defun loopy--validate-binding (binding)
  "Validate the form of BINDING.  Signal error if invalid.

BINDING should be a list of two elements.  To avoid mistakes,
this means that an explicit \"nil\" is always required."
  (unless (and (consp binding)
               (= 2 (length binding)))
    (error "Invalid binding in `loopy' expansion: %s" binding)))

(defun loopy--ensure-valid-bindings (bindings)
  "Ensure BINDINGS valid according to `loopy--validate-binding'."
  (mapc #'loopy--validate-binding bindings))


;;;###autoload
(defmacro loopy-setq (&rest args)
  "Use Loopy destructuring in a `setq' form.

This macro supports only the built-in style of destructuring, and
is unaffected by flags like `seq' or `pcase'.  For example, if
you wish to use `pcase' destructuring, you should use `pcase-let'
instead of this macro.

\(fn SYM VAL SYM VAL ...)"
  (declare (debug (&rest [sexp form])))
  `(setq ,@(apply #'append
                  (cl-loop for (var val . _) on args by #'cddr
                           append (loopy--destructure-sequence var val)))))
;;;###autoload
(defalias 'loopy-dsetq 'loopy-setq) ; Named for Iterate's `dsetq'.

;;;###autoload
(defmacro loopy-let* (bindings &rest body)
  "Use Loopy destructuring on BINDINGS in a `let*' form wrapping BODY.

This macro supports only the built-in style of destructuring, and
is unaffected by flags like `seq' or `pcase'.  For example, if
you wish to use `pcase' destructuring, you should use `pcase-let'
instead of this macro."
  (declare (debug ((&rest [sexp form]) body))
           (indent 1))
  `(let* ,(cl-loop for (var val) in bindings
                   append (loopy--destructure-sequence var val))
     ,@body))

;;;###autoload
(defmacro loopy-ref (bindings &rest body)
  "Destructure BINDINGS as `setf'-able places around BODY.

This macro only creates references to those places via
`cl-symbol-macrolet'.  It does /not/ create new variables or bind
values.  Its behavior should not be mistaken with that of
`cl-letf*', which temporarily binds values to those places.

As these places are not true variable, BINDINGS is not
order-sensitive.

This macro supports only the built-in style of destructuring,
and is unaffected by flags like `pcase' and `seq'."
  (declare (debug ((&rest [sexp form]) body))
           (indent 1))
  `(cl-symbol-macrolet
       ,(cl-loop for (var val) in bindings
                 append (loopy--destructure-generalized-sequence
                         var val))
     ,@body))

;;;###autoload
(defmacro loopy-lambda (args &rest body)
  "Create a `lambda' using `loopy' destructuring in the argument list.

ARGS are the arguments of the lambda, which can be `loopy'
destructuring patterns.  See the info node `(loopy)Loop Commands'
for more on this.

BODY is the `lambda' body."
  (declare (debug (lambda-list body))
           (indent 1))
  (let ((lambda-args)
        (destructurings))
    (dolist (arg args)
      (if (symbolp arg)
          (push arg lambda-args)
        (let ((arg-var (gensym)))
          (push arg-var lambda-args)
          (push (list arg arg-var) destructurings))))
    `(lambda ,(nreverse lambda-args)
       (loopy-let* ,(nreverse destructurings)
         ,@body))))

(defalias 'loopy--basic-builtin-destructuring #'loopy--destructure-sequence
  "Destructure VALUE-EXPRESSION according to VAR.

Return a list of variable-value pairs (not dotted), suitable for
substituting into a `let*' form or being combined under a `setq'
form.")

(defun loopy--destructure-for-with-vars (bindings)
  "Destructure BINDINGS into bindings suitable for something like `let*'.

This function named by this variables receives the bindings given
to the `with' macro argument and should usually return a list of
two elements:

1. A function/macro that works like `let*' and can be used to wrap
   the expanded macro code.
2. The bindings that will be given to this macro.

For example, an acceptable return value might be something like

    (list 'pcase-let* BINDINGS)

which will be used to wrap the loop and other code."
  (funcall (or loopy--destructuring-for-with-vars-function
               #'loopy--destructure-for-with-vars-default)
           bindings))

(defun loopy--destructure-for-with-vars-default (bindings)
  "Destructure BINDINGS into bindings suitable for something like `let*'.

Returns a list of two elements:
1. The symbol `let*'.
2. A new list of bindings."
  (list 'let*
        (mapcan (cl-function
                 (lambda ((var val))
                   (loopy--destructure-sequence var val)))
                bindings)))

(cl-defun loopy--find-special-macro-arguments (names body)
  "Find any usages of special macro arguments NAMES in BODY, given aliases.

NAMES can be either a single quoted name or a list of quoted names.

Aliases can be found in `loopy-command-aliases'."
  (let ((aliases (map-pairs loopy-command-aliases)))
    (dolist (keyword
             (if (listp names)
                 (append names
                         (cl-loop for alias in aliases
                                  if (memq (cdr alias) names)
                                  collect (car alias)))
               (cons names (cl-loop for alias in aliases
                                    if (eq (cdr alias) names)
                                    collect (car alias)))))
      (when-let ((target (cdr (assq keyword body))))
        (cl-return-from loopy--find-special-macro-arguments target)))))


;;;; The Macro Itself
(defun loopy--expand-to-loop ()
  "Create the loop body according to the variables found in `loopy--variables'.

The function creates quoted code that should be used by a macro."


  ;; Construct the expanded code from the inside out.  The result should work
  ;; something like the below code.  Unlike below, constructs are only used
  ;; when needed.
  ;;
  ;; `(cl-symbol-macrolet ,loopy--generalized-vars
  ;;    (let* ,loopy--with-vars
  ;;      (let ,loopy--accumulation-vars
  ;;        (let* ,loopy--iteration-vars
  ;;          (let ((loopy--early-return-capture
  ;;                 (cl-block ,loopy--loop-name
  ;;                   (cl-tagbody
  ;;                    ,@loopy--before-do
  ;;                    (while ,(cl-case (length loopy--pre-conditions)
  ;;                              (0 t)
  ;;                              (1 (car loopy--pre-conditions))
  ;;                              (t (cons 'and loopy--pre-conditions)))
  ;;                      (cl-tagbody
  ;;                       ,@loopy--main-body
  ;;                       loopy--continue-tag
  ;;                       ,@loopy--latter-body
  ;;                       (unless ,loopy--post-conditions
  ;;                         (cl-return-from ,loopy--loop-name
  ;;                           ,loopy--implicit-return))))
  ;;                    ,@loopy--after-do
  ;;                    loopy--non-returning-exit-tag
  ;;                    ,loopy--accumulation-final-updates))
  ;;                 ,loopy--implicit-return))
  ;;            ,@loopy--final-do
  ;;            ,(if loopy--final-return
  ;;                 loopy--final-return
  ;;               'loopy--early-return-capture))))))
  (let (result
        ;; Need a variable to track whether `result' is currently one
        ;; expression, as that affects how it should be built.  For example,
        ;; `(progn (thing1) (thing2))' vs `((thing1) (thing2))'
        result-is-one-expression)

    ;; This temporary function is just for convenience.  Since it checks the
    ;; structure of `result', it should always be used like:
    ;; ,@(get-result).
    (cl-flet ((get-result () (if result-is-one-expression
                                 (list result)
                               result)))

      (setq result loopy--main-body)

      (when (eq loopy--skip-used loopy--skip-tag-name)
        (setq result `(cl-tagbody ,@result ,loopy--skip-tag-name)
              result-is-one-expression t))

      (when loopy--latter-body
        (setq result (append result loopy--latter-body)))

      (setq result (append result (list '(setq loopy-first-iteration nil))))

      (when loopy--post-conditions
        (setq result
              (append result
                      `((unless ,(cl-case (length loopy--post-conditions)
                                   (0 t)
                                   (1 (car loopy--post-conditions))
                                   (t (cons 'and loopy--post-conditions)))
                          ;; If the loop exits early, we should still use the
                          ;; implicit return.  That isn't a problem for the
                          ;; `while' loop, but we need to be more explicit
                          ;; here.
                          (cl-return-from ,loopy--loop-name
                            ,loopy--implicit-return))))))

      ;; Now wrap loop body in the `while' form.
      (setq result `(while ,(cl-case (length loopy--pre-conditions)
                              (0 t)
                              (1 (car loopy--pre-conditions))
                              (t (cons 'and loopy--pre-conditions)))
                      ;; If using a `cl-tag-body', just insert that one
                      ;; expression, but if not, break apart into the while
                      ;; loop's body.
                      ,@(get-result))
            ;; Will always be a single expression after wrapping with `while'.
            result-is-one-expression t)

      ;; Make sure that the implicit accumulation variable is correctly
      ;; updated after the loop, if need be.
      (when loopy--accumulation-final-updates
        (setq result
              (if loopy--non-returning-exit-used
                  `(,@(get-result)
                    ,@(mapcar #'cdr loopy--accumulation-final-updates)
                    (setq loopy--accumulations-updated t))
                `(,@(get-result)
                  ,@(mapcar #'cdr loopy--accumulation-final-updates)))
              result-is-one-expression nil))

      ;; Try to apply wrapping forms so that they're not disturbed by variable
      ;; updates or leaving the loop early.
      (when loopy--wrapping-forms
        (dolist (form (reverse loopy--wrapping-forms))
          (setq result (if (and (consp form)
                                (not (eq (cl-first form) 'lambda)))
                           `(,@form ,@(get-result))
                         `(,form ,@(get-result)))
                result-is-one-expression t)))

      ;; Now ensure return value is nil and add the code to run before and
      ;; after the `while' loop.
      (cond
       ((and loopy--before-do loopy--after-do)
        (setq result `(,@loopy--before-do ,@(get-result) ,@loopy--after-do)
              result-is-one-expression nil))
       (loopy--before-do
        (setq result `(,@loopy--before-do ,@(get-result))
              result-is-one-expression nil))
       (loopy--after-do
        (setq result `(,@(get-result) ,@loopy--after-do)
              result-is-one-expression nil)))

      (when (eq loopy--non-returning-exit-used
                loopy--non-returning-exit-tag-name)
        (setq result (if loopy--accumulation-final-updates
                         `(cl-tagbody
                           ,@(get-result)
                           ,loopy--non-returning-exit-tag-name
                           (if loopy--accumulations-updated
                               nil
                             ,@(mapcar #'cdr loopy--accumulation-final-updates)))
                       `(cl-tagbody
                         ,@(get-result)
                         ,loopy--non-returning-exit-tag-name))
              result-is-one-expression t))

      ;; Always wrap in `cl-block', as any arbitrary Lisp code could call
      ;; `cl-return-from'.  For example, it's possible that a user is using a
      ;; loop to change variables, and they might wish to stop changing things
      ;; at a certain point.
      (setq result `(cl-block ,loopy--loop-name
                      ,@(get-result)
                      ;; Be sure that the `cl-block' defaults to returning the
                      ;; implicit return, which can be nil.  This can be
                      ;; overridden by any call to `cl-return-from'.
                      ,(if (and loopy--split-implied-accumulation-results
                                loopy--implicit-return
                                (or loopy--after-do
                                    loopy--final-do
                                    loopy--final-return))
                           `(setq loopy-result ,loopy--implicit-return)
                         loopy--implicit-return))
            ;; Will always be a single expression after wrapping with
            ;; `cl-block'.
            result-is-one-expression t)

      ;; Try to keep the return value of the expanded code as `nil' by
      ;; default.
      ;; - If final-return is used, then there's no problem, and we just use
      ;;   that.
      ;; - If there's final-do, be sure to return the value of the `cl-block'
      ;;   (which defaults to nil) by using `prog1'.
      (if loopy--final-return
          (if loopy--final-do
              (setq result `(,@(get-result)
                             ,@loopy--final-do ,loopy--final-return)
                    result-is-one-expression nil)
            (setq result `(,@(get-result)
                           ,loopy--final-return)
                  result-is-one-expression nil))
        (when loopy--final-do
          (setq result `(prog1 ,result ,@loopy--final-do)
                result-is-one-expression t)))

      ;; Bind `loopy-first-iteration'.
      (setq result `(let ((loopy-first-iteration t))
                      ,@(get-result))
            result-is-one-expression t)

      ;; Declare the loop variables.
      (when loopy--iteration-vars
        (setq result `(let* ,loopy--iteration-vars ,@(get-result))
              result-is-one-expression t))

      ;; If there are final updates to made and a tag-body exit that can skip
      ;; them, then we must initialize `loopy--accumulations-updated'.
      (when (and loopy--accumulation-final-updates
                 loopy--non-returning-exit-used)
        (setq result `(let ((loopy--accumulations-updated nil))
                        ,@(get-result))
              result-is-one-expression t))

      ;; Declare accumulation variables.
      (when loopy--accumulation-vars
        (setq result `(let* ,loopy--accumulation-vars ,@(get-result))
              result-is-one-expression t))

      ;; Bind `loopy-result' if using split accumulation variables.
      ;; In such case, no command requests this, so we do it here.
      (when (and loopy--split-implied-accumulation-results
                 loopy--implicit-return
                 (or loopy--after-do
                     loopy--final-do
                     loopy--final-return))
        (setq result `(let ((loopy-result nil)) ,@(get-result))
              result-is-one-expression t))

      ;; Declare the With variables.
      (when loopy--with-vars
        (setq result `(,@(loopy--destructure-for-with-vars loopy--with-vars)
                       ,@(get-result))
              result-is-one-expression t))

      ;; Declare the symbol macros.
      (when loopy--generalized-vars
        (setq result `(cl-symbol-macrolet ,loopy--generalized-vars
                        ,@(get-result))
              result-is-one-expression t))

      ;; Final check: If `result' is not one expression, then wrap `result' in
      ;; a `progn'.  Otherwise, the return value of the first expression would
      ;; be used as a function.
      (unless result-is-one-expression
        (push 'progn result))

      ;; Return the constructed code.
      result)))

(defmacro loopy--process-special-marco-args (names &rest body)
  "Process the special macro arguments named by NAMES.

BODY is the processing.

Variables available:
- `all-names' is all of the names found
- `such-args' are all arguments that match elements in
  `all-names'
- `arg-value' is the value of the arg if there is only one match
- `arg-name' the name of the arg found if there is only one match"
  (declare (indent 1))
  `(let* ((all-names (loopy--find-all-names ,names))
          (such-args (map-filter (lambda (arg-name _)
                                   (memq arg-name all-names))
                                 body)))
     (cl-case (length such-args)
       (0 nil)
       (1 (let ((arg-name  (caar such-args))
                (arg-value (cdar such-args)))
            (ignore arg-value)
            ,@body))
       (t (error "Conflicting arguments: %s" such-args)))))

(cl-defun loopy--process-instruction (instruction &key erroring-instructions)
  "Process INSTRUCTION, assigning values to the variables in `loopy--variables'.

If INSTRUCTION is in ERRORING-INSTRUCTIONS, then an error is raised.

In `loopy', processing instructions is stateful.  This function
merely pushes values into the correct variables.  The proper
ordering of those variables is handled elsewhere, such as in the
macro `loopy' itself."
  ;; Do it this way instead of with `set', cause was getting errors
  ;; about void variables.
  (let ((instruction-type (cl-first instruction))
        (instruction-value (cl-second instruction)))
    (when (memq instruction-type erroring-instructions)
      (error "Attempted to process should-error instruction: %s"
             instruction))
    (cl-case (cl-first instruction)
      (loopy--generalized-vars
       (loopy--validate-binding instruction-value)
       (push instruction-value loopy--generalized-vars))
      (loopy--iteration-vars
       (loopy--validate-binding instruction-value)
       ;; Don't want to accidentally rebind variables to `nil'.
       (unless (loopy--bound-p (cl-first instruction-value))
         (push instruction-value loopy--iteration-vars)))
      (loopy--accumulation-vars
       (loopy--validate-binding instruction-value)
       ;; Don't want to accidentally rebind variables to `nil'.
       (unless (loopy--bound-p (cl-first instruction-value))
         (push instruction-value loopy--accumulation-vars)))
      (loopy--pre-conditions
       (push instruction-value loopy--pre-conditions))
      (loopy--main-body
       (push instruction-value loopy--main-body))
      (loopy--latter-body
       (push instruction-value loopy--latter-body))
      (loopy--post-conditions
       (push instruction-value loopy--post-conditions))
      (loopy--implicit-return
       (unless (loopy--already-implicit-return instruction-value)
         (push instruction-value loopy--implicit-return)))
      (loopy--accumulation-final-updates
       ;; These instructions are of the form `(l--a-f-u (var . update))'
       (let* ((var-to-update (car instruction-value))
              (update-code (cdr instruction-value)))
         (if-let ((existing-update
                   (map-elt loopy--accumulation-final-updates
                            var-to-update)))
             (unless (equal existing-update update-code)
               (error "Incompatible final update for %s:\n%s\n%s"
                      var-to-update
                      existing-update
                      update-code))
           (push instruction-value
                 loopy--accumulation-final-updates))))

      ;; Code for conditionally constructing the loop body.
      (loopy--skip-used
       (setq loopy--skip-used instruction-value))
      (loopy--non-returning-exit-used
       (setq loopy--non-returning-exit-used instruction-value))

      ;; Instructions from the `at' command.
      (loopy--at-instructions
       (let ((target-loop (cl-first instruction-value))
             (at-instructions (cl-rest instruction-value)))
         (loopy--check-target-loop-name target-loop)
         (map-let ((t external)
                   (nil internal))
             (seq-group-by (lambda (x)
                             (if (loopy--valid-external-at-target-p (cl-first x))
                                 t
                               nil))
                           at-instructions)
           (setf (alist-get target-loop loopy--at-instructions)
                 (append (alist-get target-loop
                                    loopy--at-instructions)
                         external))
           (dolist (instr internal)
             (loopy--process-instruction instr)))))

      ;; Places users probably shouldn't push to, but can if they want:
      (loopy--before-do
       (push instruction-value loopy--before-do))
      (loopy--after-do
       (push instruction-value loopy--after-do))
      (loopy--final-do
       (push instruction-value loopy--final-do))
      (loopy--final-return
       (push instruction-value loopy--final-return))
      (t
       (error "Loopy: Unknown body instruction: %s" instruction)))))

(cl-defun loopy--process-instructions (instructions &key erroring-instructions)
  "Process the list of INSTRUCTIONS via `loopy--process-instruction'.

If any instruction is in ERRORING-INSTRUCTIONS, then an error is raised.

In `loopy', processing instructions is stateful."
  (dolist (instruction instructions)
    (when (memq (cl-first instruction) erroring-instructions)
      (error "Attempted to process should-error instruction: %s"
             instruction)))
  (mapc #'loopy--process-instruction instructions))

;;;###autoload
(cl-defmacro loopy (&rest body)
  "A looping macro.

The macro takes several top level arguments, all, except a loop
name, being a list beginning with one of the keywords below.  To
name a loop, pass in an unquoted symbol as an argument.

- `with', `init', `let*': Declare variables before the loop.

- `without', `no-with', `no-init': Variables that `loopy' should not try to
  initialize.  `loopy' tries to initialize all the variables it
  uses in a `let'-like form, but that isn’t always desired.

- `before-do', `before', `initially-do', `initially': Run Lisp
  expressions before the loop starts.

- `after-do', `after', `else-do', `else': Run Lisp expressions
  after the loop successfully completes.  This is similar to
  Python’s `else' loop clause.

- `finally-do', `finally': Always run Lisp expressions after the
  loop exits.

- `finally-return', `return': Return a value, regardless of how
  the loop completes.  Accumulation commands have an implicit
  return value, but this overrides them.

- `flag', `flags': Options that change the behavior of `loopy'.

The loop body and any expressions that are part of the
`before-do' and `after-do' arguments are contained in a single
`cl-block'.  Naming the loop really just names the block,
allowing for more specific exiting via ~cl-return~ and the loop
commands that wrap it.

Finally, `(finally-return 1 2 3)' is the same as
`(finally-return (list 1 2 3))'.  This is convenient when using
`seq-let', `pcase-let', `cl-destructuring-bind', and the like.

Any other argument in BODY is assumed to be a loop command.  For
more information, including a list of available loop commands,
see the Info node `(loopy)' distributed with this package."

  (declare (debug (&rest ;; TODO: Is this correct?
                   [&or
                    ([&or "with" "let*" "init"] &rest (symbolp &optional form))
                    ([&or "without" "no-with" "no-init"] &rest symbolp)
                    ([&or "flag" "flags"] &rest symbolp)
                    ([&or "before-do" "before" "initially-do" "initially"] body)
                    [&or (symbolp ;; This one covers most commands.
                          &optional
                          [&or symbolp sexp] ; destructured arg
                          form
                          [&or symbolp function-form lambda-expr])
                         ([&or "when" "if" "unless"] form body)
                         ([&or "expr" "exprs" "set"] [&or symbolp sexp]
                          &optional [&rest form])
                         ("cond" &rest (body))
                         ("group" body)]
                    ([&or "after-do" "after" "else-do" "else"] body)
                    ([&or "finally-do" "finally"] body)
                    ("finally-return" form &optional [&rest form]) ])))

  ;; Bind variables in `loopy--variables' around code to build the expanded
  ;; loop.
  (loopy--wrap-variables-around-body

;;;;; Process the special macro arguments.
   ;; TODO: What is the best way to handle `nil' occurring in `body'?
   (let ((loop-name (seq-find #'symbolp body)))
     (setq loopy--loop-name loop-name
           loopy--skip-tag-name (loopy--produce-skip-tag-name loop-name)
           loopy--non-returning-exit-tag-name
           (loopy--produce-non-returning-exit-tag-name loop-name))
     (cl-callf2 seq-remove #'symbolp body))

   ;; There should be only one of each of these arguments.

   ;; Flags
   ;; Process any flags passed to the macro.  In case of conflicts, the
   ;; processing order is:
   ;;
   ;; 1. Flags in `loopy-default-flags'.
   ;; 2. Flags in the `flag' macro argument, which can undo the first group.
   (mapc #'loopy--apply-flag loopy-default-flags)
   (loopy--process-special-marco-args '(flag flags)
     (mapc #'loopy--apply-flag arg-value)
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name))
                body))

   ;; With
   ;; Note: These values don't have to be used literally, due to destructuring.
   (loopy--process-special-marco-args '(with let* init)
     (setq loopy--with-vars
           (mapcar (lambda (binding)
                     (cond ((symbolp binding)      (list binding nil))
                           ((= 1 (length binding)) (list (cl-first binding)
                                                         nil))
                           (t                       binding)))
                   arg-value))
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

   ;; Without
   (loopy--process-special-marco-args '(without no-with no-init)
     (setq loopy--without-vars arg-value)
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

   ;; Wrap
   (loopy--process-special-marco-args '(wrap)
     (setq loopy--wrapping-forms arg-value)
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))


   ;; Before do
   (loopy--process-special-marco-args '( before-do before
                                         initially-do initially)
     (setq loopy--before-do arg-value)
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

   ;; After do
   (loopy--process-special-marco-args '(after-do after else-do else)
     (setq loopy--after-do arg-value)
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

   ;; Finally Do
   (loopy--process-special-marco-args '(finally-do finally)
     (setq loopy--final-do arg-value)
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

   ;; Final Return
   (loopy--process-special-marco-args '(finally-return)
     (setq loopy--final-return (if (= 1 (length arg-value))
                                   (cl-first arg-value)
                                 (cons 'list arg-value)))
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

;;;;; Check the loop name and loop commands.

   ;; Body forms have the most variety.
   ;; An instruction is (PLACE-TO-ADD . THING-TO-ADD).
   ;; Things added are expanded in place.
   (push loopy--loop-name loopy--known-loop-names)
   (unwind-protect
       (progn
         (loopy--process-instructions (loopy--parse-loop-commands body))
         ;; Process any `at' instructions from loops lower in the call list.
         (loopy--process-instructions (map-elt loopy--at-instructions
                                               loopy--loop-name)))
     (pop loopy--known-loop-names)
     (cl-callf map-delete loopy--at-instructions loopy--loop-name)
     (cl-callf2 seq-drop-while (lambda (x) (eq loopy--loop-name (caar x)))
                loopy--accumulation-list-end-vars)
     (cl-callf2 seq-drop-while (lambda (x) (eq loopy--loop-name (caar x)))
                loopy--accumulation-variable-info))

   ;; Now that instructions processed, make sure the order-dependent lists are
   ;; in the correct order.
   (setq loopy--main-body (nreverse loopy--main-body)
         loopy--iteration-vars (nreverse loopy--iteration-vars)
         loopy--accumulation-vars (nreverse loopy--accumulation-vars)
         loopy--implicit-return (when (consp loopy--implicit-return)
                                  (if (= 1 (length loopy--implicit-return))
                                      ;; If implicit return is just a single thing,
                                      ;; don't use a list.
                                      (car loopy--implicit-return)
                                    ;; If multiple items, be sure to use a list
                                    ;; in the correct order.
                                    `(list ,@(nreverse loopy--implicit-return)))))

   ;; Constructing/Creating the returned code.
   (loopy--expand-to-loop)))



(provide 'loopy)
;;; loopy.el ends here
