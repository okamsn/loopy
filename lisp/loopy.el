;;; loopy.el --- A looping macro -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: November 2020
;; URL: https://github.com/okamsn/loopy
;; Version: 0.14.0
;; Package-Requires: ((emacs "27.1") (map "3.3.1") (seq "2.22") (compat "29.1.3") (stream "2.4.0"))
;; Keywords: extensions
;; LocalWords:  Loopy's emacs Edebug

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
(require 'macroexp)
(require 'map)
(require 'pcase)
(require 'seq)
(require 'subr-x)
(require 'loopy-misc)
(require 'loopy-commands)
(require 'loopy-vars)
(require 'loopy-destructure)
(require 'loopy-instrs)

;;;; Built-in flags

;;;;;; Default
;; It doesn't make sense to allow the disabling of this one.
(defun loopy--enable-flag-default ()
  "Set `loopy' behavior back to its default state for the loop."
  (setq loopy--destructuring-for-with-vars-function
        #'loopy--destructure-for-with-vars-default
        loopy--destructuring-accumulation-parser
        #'loopy--parse-destructuring-accumulation-command-default))

(cl-callf map-insert loopy--flag-settings 'default #'loopy--enable-flag-default)

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

(defun loopy--destructure-for-with-vars (bindings)
  "Destructure BINDINGS into bindings suitable for something like `let*'.

This function named by this variables receives the bindings given
to the `with' macro argument and should usually return a list of
two elements:

1. A function/macro that works like `let*' and can be used to wrap
   the expanded macro code.
2. The bindings that will be given to this macro.

For example, an acceptable return value might be something like

    (list \\='pcase-let* BINDINGS)

which will be used to wrap the loop and other code."
  (funcall (or loopy--destructuring-for-with-vars-function
               #'loopy--destructure-for-with-vars-default)
           bindings))

(defun loopy--destructure-for-with-vars-default (bindings)
  "Destructure BINDINGS into bindings suitable for something like `let*'.

Returns a list of two elements:
1. The symbol `pcase-let*'.
2. A new list of bindings."
  ;; We do this instead of passing to `pcase-let*' so that:
  ;; 1) We sure that variables are bound even when unmatched.
  ;; 2) We can signal an error if the pattern doesn't match a value.
  ;; This keeps the behavior of the old implementation.
  ;;
  ;; Note: Binding the found variables to `nil' would overwrite any values that
  ;;       we might try to access while binding, so we can't do that like we do
  ;;       for iteration commands in which we already know the scope.
  ;; (let ((new-binds)
  ;;       (all-set-exprs))
  ;;   (dolist (bind bindings)
  ;;     (cl-destructuring-bind (var val)
  ;;         bind
  ;;       (if (symbolp var)
  ;;           (push `(,var ,val) new-binds)
  ;;         (let ((sym (gensym)))
  ;;           (push `(,sym ,val) new-binds)
  ;;           (cl-destructuring-bind (set-expr found-vars)
  ;;               (loopy--pcase-destructure-for-iteration `(loopy ,var) sym :error t)
  ;;             (dolist (v found-vars)
  ;;               (push `(,v nil) new-binds))
  ;;             (push set-expr all-set-exprs))))))
  ;;   (list 'let* (nreverse new-binds) (macroexp-progn (nreverse
  ;;                                                     all-set-exprs))))
  (let ((new-binds))
    (dolist (bind bindings)
      (cl-destructuring-bind (var val)
          bind
        (if (symbolp var)
            (push `(,var ,val) new-binds)
          (let ((sym (gensym)))
            (push `(,sym ,val) new-binds)
            (cl-destructuring-bind (set-expr found-vars)
                (loopy--pcase-destructure-for-iteration `(loopy ,var) sym :error t)
              (dolist (v found-vars)
                (push `(,v nil) new-binds))
              (push `(_ ,set-expr) new-binds))))))
    (list 'let* (nreverse new-binds))))

(cl-defun loopy--find-special-macro-arguments (names body)
  "Find any usages of special macro arguments NAMES in BODY, given aliases.

NAMES can be either a single quoted name or a list of quoted names.

Aliases can be found in `loopy-aliases'."
  (let ((aliases (map-pairs loopy-aliases)))
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
  ;;                    ,@loopy--before-do
  ;;                    (catch loopy--non-returning-exit-tag-name
  ;;                      (while ,(cl-case (length loopy--pre-conditions)
  ;;                                (0 t)
  ;;                                (1 (car loopy--pre-conditions))
  ;;                                (t (cons 'and loopy--pre-conditions)))
  ;;                         (catch loopy--skip-tag-name
  ;;                          ,@loopy--main-body)
  ;;                         ,@loopy--latter-body
  ;;                         (unless ,loopy--post-conditions
  ;;                           (cl-return-from ,loopy--loop-name
  ;;                             ,loopy--implicit-return)))
  ;;                      ,loopy--vars-final-updates
  ;;                      ,@loopy--after-do))
  ;;                 ,loopy--implicit-return))
  ;;            ,@loopy--final-do
  ;;            ,(if loopy--final-return
  ;;                 loopy--final-return
  ;;               'loopy--early-return-capture))))))
  (let* ((result)
         ;; Need a variable to track whether `result' is currently one
         ;; expression, as that affects how it should be built.  For example,
         ;; `(progn (thing1) (thing2))' vs `((thing1) (thing2))'
         (result-is-one-expression)
         (actual-accumulation-updates
          (cl-loop for (_ . update) in loopy--vars-final-updates
                   when update
                   collect update))
         (accum-updates-exist (car actual-accumulation-updates)))

    ;; This temporary function is just for convenience.  Since it checks the
    ;; structure of `result', it should always be used like:
    ;; ,@(get-result).
    (cl-flet ((get-result () (if result-is-one-expression
                                 (list result)
                               result)))

      (setq result loopy--main-body
            result-is-one-expression (zerop (length result)))

      (when (eq loopy--skip-used loopy--skip-tag-name)
        (setq result `(catch (quote ,loopy--skip-tag-name) ,@result)
              result-is-one-expression t))

      (when loopy--latter-body
        (setq result `(,@(get-result) ,@loopy--latter-body)
              result-is-one-expression nil))

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
                      ,@(get-result))
            result-is-one-expression t)

      ;; Make sure that the implicit accumulation variable is correctly
      ;; updated after the loop, if need be.  Note that to avoid errors,
      ;; a variable's final update will at least be `nil'.
      (when accum-updates-exist
        (setq result `(,@(get-result)
                       ,@actual-accumulation-updates)
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

      ;; Now add the code to run after the `while' loop.
      (when loopy--after-do
        (setq result `(,@(get-result) ,@loopy--after-do)
              result-is-one-expression nil))

      ;; Add the wrapper for the non-returning exit tag.
      (when loopy--non-returning-exit-used
        ;; If there are final updates, then we need to make sure that they run
        ;; even if a non-returning exit tag is used.  Note that variables that
        ;; aren't updated will have a final update of `nil'.
        (if accum-updates-exist
            (setq result `(if (catch (quote ,loopy--non-returning-exit-tag-name)
                                ,@(get-result)
                                nil)
                              ,(macroexp-progn actual-accumulation-updates))
                  result-is-one-expression t)
          (setq result `(catch (quote ,loopy--non-returning-exit-tag-name)
                          ,@(get-result))
                result-is-one-expression t)))

      ;; Now add the code to run before the `while' loop.
      (when loopy--before-do
        (setq result `(,@loopy--before-do ,@(get-result))
              result-is-one-expression nil))

      ;; Always wrap in `cl-block', as any arbitrary Lisp code could call
      ;; `cl-return-from'.  For example, it's possible that a user is using a
      ;; loop to change variables, and they might wish to stop changing things
      ;; at a certain point.
      (setq result `(cl-block ,loopy--loop-name
                      ,@(get-result)
                      ;; Be sure that the `cl-block' defaults to returning the
                      ;; implicit return, which can be nil.  This can be
                      ;; overridden by any call to `cl-return-from'.
                      ,loopy--implicit-return)
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

      ;; Handle `final-protect'.  This surround the loop but is inside
      ;; the variable declarations.
      (when loopy--final-protect
        (setq result `(unwind-protect ,(if result-is-one-expression
                                           result
                                         (macroexp-progn result))
                        ,@loopy--final-protect)
              result-is-one-expression t))

      ;; Declare the loop variables.
      (when loopy--iteration-vars
        (setq result `(let* ,loopy--iteration-vars ,@(get-result))
              result-is-one-expression t))

      (when loopy--other-vars
        (setq result `(let* ,loopy--other-vars ,@(get-result))
              result-is-one-expression t))

      ;; Declare accumulation variables.
      (when loopy--accumulation-vars
        (setq result `(let* ,loopy--accumulation-vars ,@(get-result))
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
  `(let* ((all-names (loopy--get-all-names ,names))
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

;;;; Create special arg processors
(defmacro loopy--def-special-processor (name &rest body)
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
  `(defun ,(intern (format "loopy--process-special-arg-%s" name))
       (body &optional ignored)
     ,(format "Process the special macro argument `%s' and its aliases.

Returns BODY without the `%s' argument."
              name name)
     (let* ((all-names (loopy--get-all-names (quote ,name)
                                             :from-true t
                                             :ignored ignored))
            (matching-args (seq-filter (lambda (x) (memq (car-safe x) all-names))
                                       body)))
       (cl-case (length matching-args)
         (0 body)
         (1 (let ((arg-name  (caar matching-args))
                  (arg-value (cdar matching-args)))
              (ignore arg-value)
              ,@body))
         (t (error "Conflicting arguments: %s" matching-args))))))

(defun loopy--process-special-arg-loop-name (body)
  "Process BODY and the loop name listed therein."
  (let ((names)
        (new-body))
    (dolist (arg body)
      (cond ((symbolp arg)
             (push arg names))
            ((and (memq (car-safe arg) (loopy--get-all-names 'named :from-true t)))
             (if (/= 2 (length arg))
                 (error "Wrong number of arguments for loop name: %s" arg)
               (push (cl-second arg) names)))
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

(loopy--def-special-processor flag
  ;; Process any flags passed to the macro.  In case of conflicts, the
  ;; processing order is:
  ;;
  ;; 1. Flags in `loopy-default-flags'.
  ;; 2. Flags in the `flag' macro argument, which can undo the first group.
  ;; (mapc #'loopy--apply-flag loopy-default-flags)
  (mapc #'loopy--apply-flag arg-value)
  (seq-remove (lambda (x) (eq (car x) arg-name)) body))

(loopy--def-special-processor with
  (setq loopy--with-vars
        ;; Note: These values don't have to be used literally, due to
        ;;       destructuring.
        (mapcar (lambda (binding)
                  (cond ((symbolp binding)      (list binding nil))
                        ((= 1 (length binding)) (list (cl-first binding)
                                                      nil))
                        (t                       binding)))
                arg-value))
  (seq-remove (lambda (x) (eq (car x) arg-name)) body))

(loopy--def-special-processor without
  (setq loopy--without-vars arg-value)
  (seq-remove (lambda (x) (eq (car x) arg-name)) body))

(loopy--def-special-processor accum-opt
  (pcase-dolist ((or `(,var ,pos) var) arg-value)
    (push var loopy--optimized-accum-vars)
    (when pos
      (loopy--update-accum-place-count loopy--loop-name var pos 1.0e+INF)))
  (seq-remove (lambda (x) (eq (car x) arg-name)) body))

(loopy--def-special-processor wrap
  (setq loopy--wrapping-forms arg-value)
  (seq-remove (lambda (x) (eq (car x) arg-name)) body))

(loopy--def-special-processor before-do
  (setq loopy--before-do arg-value)
  (seq-remove (lambda (x) (eq (car x) arg-name)) body))

(loopy--def-special-processor after-do
  (setq loopy--after-do arg-value)
  (seq-remove (lambda (x) (eq (car x) arg-name)) body))

(loopy--def-special-processor finally-do
  (setq loopy--final-do arg-value)
  (seq-remove (lambda (x) (eq (car x) arg-name)) body))

(loopy--def-special-processor finally-return
  (setq loopy--final-return (if (= 1 (length arg-value))
                                (cl-first arg-value)
                              (cons 'list arg-value)))
  (seq-remove (lambda (x) (eq (car x) arg-name)) body))

(loopy--def-special-processor finally-protect
  (setq loopy--final-protect arg-value)
  (seq-remove (lambda (x) (eq (car x) arg-name)) body))

(defun loopy--clean-up-stack-vars ()
  "Clean up the special stack variables.

Some variables can't simply be `let'-bound around the expansion
code and must instead be cleaned up manually."
  (pop loopy--known-loop-names)
  (pop loopy--accumulation-places)
  (cl-callf map-delete loopy--at-instructions loopy--loop-name)
  (cl-callf2 seq-drop-while (lambda (x) (eq loopy--loop-name (caar x)))
             loopy--accumulation-list-end-vars)
  (cl-callf2 seq-drop-while (lambda (x) (eq loopy--loop-name (caar x)))
             loopy--accumulation-variable-info))

(defmacro loopy--with-protected-stack (&rest body)
  "Protect the stack variables from BODY during unwind and cleanup."
  `(unwind-protect
       ,(macroexp-progn body)
     (loopy--clean-up-stack-vars)))

;;;;; Process Instructions
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
      (error "Attempted to process should-error instruction: %s" instruction))

    (cl-case (cl-first instruction)
      (loopy--generalized-vars
       (loopy--validate-binding instruction-value)
       (push instruction-value loopy--generalized-vars))

      (loopy--iteration-vars
       (loopy--validate-binding instruction-value)
       ;; Don't want to accidentally rebind variables to `nil'.
       (pcase-let ((`(,var ,new-val) instruction-value))
         (pcase var
           ((pred loopy--with-bound-p) nil)
           ((app loopy--command-bound-p `(,place . ,old-val))
            (signal 'loopy-reinitializing-iteration-variable
                    (list :in place :var var :old old-val :new new-val)))
           (_ (push instruction-value loopy--iteration-vars)))))

      (loopy--accumulation-vars
       (loopy--validate-binding instruction-value)
       ;; Don't want to accidentally rebind variables to `nil'
       ;; or to accidentally mis-use commands that need
       ;; different initial values.
       (loopy--pcase-let-workaround (var new-val)
         (pcase-let ((`(,var ,new-val) instruction-value))
           (pcase var
             ((pred loopy--with-bound-p) nil)
             ((and (app loopy--command-bound-p `(,_place . ,old-val))
                   (guard (not (equal new-val old-val))))
              ;; TODO: Switch from raising a warning to raising an error.
              ;; (signal 'loopy-incompatible-accumulation-initializations
              ;;         (list :in place :var var :old old-val :new new-val))
              (display-warning
               'loopy
               (format "loopy: Conflicting accumulation starting values: `%s', %s, %s\nThis will be an error in the future.  To resolve this error, use `with' to explicitly specify a starting value."
                       var old-val new-val)
               :warning))
             (_ (push instruction-value loopy--accumulation-vars))))))

      (loopy--other-vars
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

      (loopy--vars-final-updates
       ;; These instructions are of the form `(l--a-f-u (var . update))'
       (let ((var-to-update (car instruction-value))
             (update-code (cdr instruction-value)))
         (if (map-contains-key loopy--vars-final-updates var-to-update)
             (let ((existing-update (map-elt loopy--vars-final-updates
                                             var-to-update)))
               (unless (equal existing-update update-code)
                 (signal 'loopy-incompatible-accumulation-final-updates
                         (list var-to-update existing-update update-code))))
           (push instruction-value
                 loopy--vars-final-updates))))

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

;; Here we define the Edebug specifications for better warnings.
;; TODO:
;; - Can we make Edebug work with aliases?
;; - Can we make it work with added functions?
(def-edebug-spec loopy--special-macro-arg-edebug-spec
  [&or ([&or "with" "let*" "init"] &rest (symbolp &optional form))
       ([&or "without" "no-with" "no-init"] &rest symbolp)
       ([&or "flag" "flags"] &rest symbolp)
       ([&or "accum-opt" "opt-accum"]
        [&or symbolp (symbolp [&or "end" "start" "beginning"])])
       ;; This is basically the same as the spec used by
       ;; `thread-first':
       ("wrap" &rest [&or symbolp (sexp &rest form)])
       ;; "body" is the same as "&rest form":
       ([&or "before-do" "before" "initially-do" "initially"] body)
       ([&or "after-do" "after" "else-do" "else"] body)
       ([&or "finally-do" "finally"] body)
       ([&or "finally-protect" "finally-protected"] body)
       ("finally-return" form &optional [&rest form])])

(def-edebug-spec loopy--destr-var-name-edebug-spec
  [&or symbolp
       (&rest loopy--destr-var-name-edebug-spec)
       (vector &rest loopy--destr-var-name-edebug-spec)
       (loopy--destr-var-name-edebug-spec . loopy--destr-var-name-edebug-spec)])

(def-edebug-spec loopy--seq-iter-keywords-edebug-spec
  [&or [[&or ":from" ":upfrom" ":downfrom" ":to" ":downto" ":upto"
             ":above" ":below" ":by"]
        numberp]
       [":index" symbolp]])

(def-edebug-spec loopy--accum-common-keywords-edebug-spec
  [&or [":at" [&or "start" "end" "beginning"]]
       [":into" loopy--destr-var-name-edebug-spec]
       [":test" form]
       [":key" form]
       [":init" form]])

(def-edebug-spec loopy--command-edebug-specs
  [&or
   ;; `at':
   (symbolp symbolp &rest loopy--command-edebug-specs)
   ;; `subloop':
   (symbolp &rest [&or loopy--special-macro-arg-edebug-spec
                       loopy--command-edebug-specs])
   ;; `nums',  `nums-dir':
   ( symbolp symbol [&rest [&optional numberp]]
     &rest [[&or ":from" ":upfrom" ":downfrom" ":to" ":downto" ":upto"
                 ":above" ":below" ":by"]
            numberp])
   ;; `array', `string', `seq', and `seq-index':
   (symbolp loopy--destr-var-name-edebug-spec
            form [&optional [&rest form]]
            [&optional [&rest loopy--seq-iter-keywords-edebug-spec]])
   ;; `cons', `list':
   (symbolp loopy--destr-var-name-edebug-spec form [&optional [&rest form]]
            [&optional ":by" form])
   ;; `map':
   (symbolp loopy--destr-var-name-edebug-spec form [&optional ":unique" form])
   ;; Ref specs are like normal forms, but use "place" instead of "form".
   ;; `seq-ref', `array-ref':
   (symbolp loopy--destr-var-name-edebug-spec
            place [&optional [&rest loopy--seq-iter-keywords-edebug-spec]])
   ;; `list-ref'
   (symbolp loopy--destr-var-name-edebug-spec place [&optional ":by" form])
   ;; `map-ref':
   (symbolp loopy--destr-var-name-edebug-spec place
            [&optional [&rest [&or [":unique" form]
                                   [":key" symbolp]]]])
   ;; Accumulation commands:
   (symbolp [&optional loopy--destr-var-name-edebug-spec] form
            [&optional [&rest loopy--accum-common-keywords-edebug-spec]])
   ;; `accumulate' and `reduce':
   (symbolp [&optional symbolp] form form [&optional ":init" form])
   ;; `find':
   (symbolp [&optional symbolp] form form [&optional ":on-failure" form])
   ;; `set':
   (symbolp loopy--destr-var-name-edebug-spec
            [&optional &rest form] [&optional ":init" form])
   ;; `set-prev':
   ( symbolp symbolp form
     &optional [&rest [&or [":init" form] [":back" numberp]]])
   ;; `cycle':
   (symbolp [&optional symbolp] numberp)
   ;; `command-do'
   (symbolp &rest loopy--command-edebug-specs)
   ;; `when', `unless', `if'
   (symbolp form &rest loopy--command-edebug-specs)
   ;; `cond':
   (symbolp &rest (form [&rest loopy--command-edebug-specs]))
   ;; `return-from':
   (symbolp symbolp form)
   ;; This is so general that it should be checked last.
   ;; `do', `always', `never', `thereis', `return', `while', `until':
   (symbolp body)
   ;; `skip-from', `leave-from'
   (symbolp symbolp)
   ;; `skip', `leave'
   (symbolp)])

(cl-defun loopy--correct-var-structure (&key exclude-main-body)
  "Correct the structure of some variables.

- If list order-dependent, make it in the correct order.
- Make `loopy--implicit-return' a list value if needed.

When EXCLUDE-MAIN-BODY is non-nil, don't reverse `loopy--main-body'."
  (unless exclude-main-body
    (setq loopy--main-body (nreverse loopy--main-body)))
  (setq loopy--iteration-vars (nreverse loopy--iteration-vars)
        loopy--accumulation-vars (nreverse loopy--accumulation-vars)
        ;; This one technically isn't needed yet, but it might be in the
        ;; future.
        loopy--other-vars (nreverse loopy--other-vars)
        ;; Correct conditions for things like `iter', which generates
        ;; values to check whether all values are yielded.
        loopy--pre-conditions (nreverse loopy--pre-conditions)
        loopy--post-conditions (nreverse loopy--post-conditions)
        loopy--implicit-return (when (consp loopy--implicit-return)
                                 (if (= 1 (length loopy--implicit-return))
                                     ;; If implicit return is just a single thing,
                                     ;; don't use a list.
                                     (car loopy--implicit-return)
                                   ;; If multiple items, be sure to use a list
                                   ;; in the correct order.
                                   `(list ,@(nreverse loopy--implicit-return))))))



;; (cl-defmacro loopy (&rest body)
;;   "A looping macro.
;;
;; The macro takes several top level arguments, all, except a loop
;; name, being a list beginning with one of the keywords below.  To
;; name a loop, pass in an unquoted symbol as an argument.
;;
;; - `with', `init', `let*': Declare variables before the loop.
;;
;; - `without', `no-with', `no-init': Variables that `loopy' should not try to
;;   initialize.  `loopy' tries to initialize all the variables it
;;   uses in a `let'-like form, but that isn’t always desired.
;;
;; - `before-do', `before', `initially-do', `initially': Run Lisp
;;   expressions before the loop starts.
;;
;; - `after-do', `after', `else-do', `else': Run Lisp expressions
;;   after the loop successfully completes.  This is similar to
;;   Python’s `else' loop clause.
;;
;; - `finally-do', `finally': Always run Lisp expressions after the
;;   loop exits.
;;
;; - `finally-return', `return': Return a value, regardless of how
;;   the loop completes.  Accumulation commands have an implicit
;;   return value, but this overrides them.
;;
;; - `flag', `flags': Options that change the behavior of `loopy'.
;;
;; The loop body and any expressions that are part of the
;; `before-do' and `after-do' arguments are contained in a single
;; `cl-block'.  Naming the loop really just names the block,
;; allowing for more specific exiting via ~cl-return~ and the loop
;; commands that wrap it.
;;
;; Finally, `(finally-return 1 2 3)' is the same as
;; `(finally-return (list 1 2 3))'.  This is convenient when using
;; `seq-let', `pcase-let', `cl-destructuring-bind', and the like.
;;
;; Any other argument in BODY is assumed to be a loop command.  For
;; more information, including a list of available loop commands,
;; see the Info node `(loopy)' distributed with this package."
;;
;;   (declare (debug (&rest [&or loopy--command-edebug-specs
;;                               loopy--special-macro-arg-edebug-spec])))
;;
;;   ;; Bind variables in `loopy--variables' around code to build the expanded
;;   ;; loop.
;;   (loopy--wrap-variables-around-body
;; ;;;;; Process the special macro arguments.
;;    (mapc #'loopy--apply-flag loopy-default-flags)
;;    (setq body (loopy--process-special-arg-loop-name body))
;;    (setq body (loopy--process-special-arg-flag body))
;;    (setq body (loopy--process-special-arg-with body))
;;    (setq body (loopy--process-special-arg-without body))
;;    (setq body (loopy--process-special-arg-accum-opt body))
;;    (setq body (loopy--process-special-arg-wrap body))
;;    (setq body (loopy--process-special-arg-before-do body))
;;    (setq body (loopy--process-special-arg-after-do body))
;;    (setq body (loopy--process-special-arg-finally-do body))
;;    (setq body (loopy--process-special-arg-finally-return body))
;;    (setq body (loopy--process-special-arg-finally-protect body))
;;
;; ;;;;; Check the loop name and loop commands.
;;
;;    ;; Body forms have the most variety.
;;    ;; An instruction is (PLACE-TO-ADD . THING-TO-ADD).
;;    ;; Things added are expanded in place.
;;    (unwind-protect
;;        (progn
;;          (loopy--process-instructions (loopy--parse-loop-commands body))
;;
;;          ;; (cl-callf2 mapcar #'loopy--accum-code-expansion loopy--main-body)
;;          ;; Expand any uses of `loopy--optimized-accum' as if it were a macro,
;;          ;; using the function `loopy--expand-optimized-accum'.
;;          ;;
;;          ;; Prevent the expansion of, at the very least, `cl-block',
;;          ;; `cl-return-from', and `cl-return' shouldn't be expanded.
;;          ;;
;;          ;; TODO: Is there a way to more precisely only expand
;;          ;;       `loopy--optimized-accum'?
;;          ;; Another option is this, but it massively slows down expansion:
;;          ;;     (cl-loop for i being the symbols
;;          ;;              when (eq (car-safe (symbol-function i)) 'macro)
;;          ;;              collect (cons i nil))
;;          (setq loopy--main-body
;;                (cl-loop
;;                 with macro-funcs = `(,@(cl-loop for i in loopy--suppressed-macros
;;                                                 collect (cons i nil))
;;                                      (loopy--optimized-accum
;;                                       . loopy--expand-optimized-accum)
;;                                      ,@macroexpand-all-environment)
;;                 for i in loopy--main-body
;;                 collect (macroexpand-all i macro-funcs)))
;;
;;          ;; Process any `at' instructions from loops lower in the call list.
;;          (loopy--process-instructions (map-elt loopy--at-instructions
;;                                                loopy--loop-name)))
;;      (loopy--clean-up-stack-vars))
;;
;;    ;; Now that instructions processed, make sure the order-dependent lists are
;;    ;; in the correct order.
;;    (loopy--correct-var-structure)
;;
;;    ;; Constructing/Creating the returned code.
;;    (loopy--expand-to-loop)))

;;;;; Other features

;; TODO: We didn't implement these using `loopy' to avoid a weird error about
;;       `loopy--process-special-arg-loop-name' not being defined.  This error
;;       doesn't seem to occur in `loopy-iter.el', in which we already use
;;       `loopy'.

;;;###autoload
(defalias 'loopy-dsetq 'loopy-setq) ; Named for Iterate's `dsetq'.

;;;###autoload
(defmacro loopy-setq (&rest args)
  "Use Loopy destructuring in a `setq' form.

This macro supports only the built-in style of destructuring, and
is unaffected by flags like `seq' or `pcase'.  For example, if
you wish to use `pcase' destructuring, you should use `pcase-let'
instead of this macro.

\(fn SYM VAL SYM VAL ...)"
  (declare (debug (&rest [sexp form])))
  (macroexp-progn
   (cl-loop for (var val) on args by #'cddr
            collect (car (loopy--destructure-for-iteration-default var val)))))

;;;###autoload
(defmacro loopy-let* (bindings &rest body)
  "Use Loopy destructuring on BINDINGS in a `let*' form wrapping BODY.

This macro supports only the built-in style of destructuring, and
is unaffected by flags like `seq' or `pcase'.  For example, if
you wish to use `pcase' destructuring, you should use `pcase-let'
instead of this macro."
  (declare (debug ((&rest [sexp form]) body))
           (indent 1))
  ;; Because Emacs versions less than 28 weren't guaranteed to bind all
  ;; variables in Pcase, we need to use the same approach we do for
  ;; destructuring `with' bindings, instead of just passing the bindings to
  ;; `pcase' directly.
  (let ((new-binds))
    (dolist (bind bindings)
      (cl-destructuring-bind (var val)
          bind
        (if (symbolp var)
            (push bind new-binds)
          (let ((sym (gensym)))
            (push `(,sym ,val) new-binds)
            (cl-destructuring-bind (var-set-expr var-list)
                (loopy--pcase-destructure-for-iteration `(loopy ,var) sym :error t)
              (dolist (var var-list)
                (push var new-binds))
              (push `(_ ,var-set-expr) new-binds))))))
    `(let* ,(nreverse new-binds)
       ,@body)))

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

(defgroup loopy-iter nil
  "Options specifically for the `loopy-iter' macro."
  :group 'loopy
  :prefix "loopy-iter-")

(defcustom loopy-iter-keywords '(accum for exit arg)
  "Keywords that `loopy-iter' can use to recognize loop commands.

By default, `loopy-iter' can use keywords to clearly distinguish
loop commands and special macro arguments from other Emacs
features.

A loop command or special macro argument can be preceded by any
of the keywords in this list.  For example, by default, \"(for
collect i)\" and \"(accum collect i)\" are both valid way of
identifying the `collect' loop command.

Without these keywords, one must use one of the names given in
`loopy-iter-bare-commands' or
`loopy-iter-bare-special-macro-arguments'."
  :type '(repeat symbol))

(def-edebug-spec loopy-iter--special-macro-arg-edebug-spec
  ;; This is the same as for `loopy', but without `let*'.
  [&or ([&or "with" "init"] &rest (symbolp &optional form))
       ([&or "without" "no-with" "no-init"] &rest symbolp)
       ([&or "flag" "flags"] &rest symbolp)
       ([&or "accum-opt" "opt-accum"]
        [&or symbolp (symbolp [&or "end" "start" "beginning"])])
       ;; This is basically the same as the spec used by
       ;; `thread-first':
       ("wrap" &rest [&or symbolp (sexp &rest form)])
       ;; "body" is the same as "&rest form":
       ([&or "before-do" "before" "initially-do" "initially"] body)
       ([&or "after-do" "after" "else-do" "else"] body)
       ([&or "finally-do" "finally"] body)
       ([&or "finally-protect" "finally-protected"] body)
       ("finally-return" form &optional [&rest form])])

(def-edebug-spec loopy-iter--command-edebug-specs
  ([&optional symbolp] . loopy--command-edebug-specs))

;;;; For parsing commands

(defcustom loopy-iter-bare-commands
  '(accumulating
    adjoining
    always
    appending
    arraying
    arraying-index
    arraying-ref
    at
    collecting
    concating
    consing
    continuing
    continuing-from
    counting
    cycling
    finding
    iterating
    leaving
    leaving-from
    listing
    listing-index
    listing-ref
    mapping
    mapping-pairs
    mapping-ref
    maximizing
    minimizing
    multiplying
    nconcing
    never
    numbering
    numbering-down
    numbering-up
    nunioning
    prepending
    pushing
    pushing-into
    reducing
    repeating
    returning
    returning-from
    seqing
    seqing-index
    seqing-ref
    sequencing
    sequencing-index
    sequencing-ref
    setting
    setting-accum
    setting-prev
    skipping
    skipping-from
    string
    string-index
    string-ref
    stringing
    stringing-index
    stringing-ref
    streaming
    substreaming
    summing
    thereis
    unioning
    vconcating)
  "Commands recognized in `loopy-iter' without a preceding keyword.

For special marco arguments, see `loopy-iter-bare-special-macro-arguments'."
  :type '(repeat symbol)
  :group 'loopy-iter)

(defvar loopy-iter--command-parsers nil
  "Parsers used by `loopy-iter'.

This variable is bound while `loopy-iter' is running, combining
`loopy-command-parsers' and
`loopy-iter-overwritten-command-parsers'.")

(defun loopy-iter--parse-command (command)
  "Parse COMMAND using parsers in`loopy-iter--command-parsers'.

See also `loopy--parse-loop-command'."
  (let* ((cmd-name (cl-first command))
         (parser (loopy--get-command-parser
                  cmd-name
                  :parsers loopy-iter--command-parsers))
         (instructions (remq nil (funcall parser command))))
    (or instructions
        (signal 'loopy-parser-instructions-missing
                (list command parser)))))

(defvar loopy-iter--non-main-body-instructions nil
  "Used to capture other instructions while expanding.

Expanding functions `push' lists of instructions into this
variable.  The contents of main-body instructions are inserted
into the expanded body in the command's place during macro
expansion.")

(defvar loopy-iter-suppressed-macros '(cl-block cl-return cl-return-from)
  "Macros that shouldn't be expanded as the `loopy-iter' expansion is built.

Some macros interact in a way where they might break if one is
expanded without the context of the other.  Others might not work
for other reasons.  The macros `cl-block', `cl-return-from', and
`cl-return' are known to fall into the first group.")

;;;;; Expanders

(defvar loopy-iter--level nil
  "The level of the expression `loopy-iter' is currently processing.

For example, iteration commands should only be processed during
level 1, which is the top level.  The next level of nesting is
level 2, and so on.  If `loopy-iter--level' is greater than 1,
then `loopy--in-sub-level' is set to `t'.

The macro initially `let'-binds this variable to 0, and it is
incremented upon parsing a new function.")

(defun loopy-iter--opt-accum-expand-val (arg)
  "Macro expand only the value of the optimized accumulation expression ARG.

Optimized accumulations are expanded into a special form, after
which this function will recursively expand the expression of the
accumulated value.

To avoid an infinite loop, this function replaces the `loopy--optimized-accum'
in the expression with `loopy--optimized-accum-2', which is then processed
during a second pass on the expanded code."
  (loopy (with (plist (cadr arg)))
         (cons (k v) plist :by #'cddr)
         (collect k)
         ;; By this point, command expansion are already defined, so we don't
         ;; need to try to handle instructions.
         (collect (if (eq k :val)
                      (let ((loopy-iter--level (1+ loopy-iter--level))
                            (loopy--in-sub-level t))
                        (macroexpand-all v macroexpand-all-environment))
                    v))
         (finally-return `(loopy--optimized-accum-2 (quote ,loopy-result)))))

;;;;; Overwritten parser definitions

(defcustom loopy-iter-overwritten-command-parsers
  '((at       . loopy-iter--parse-at-command))
  "Overwritten command parsers.

This is an alist of dotted pairs of base names and parsers, as in
`loopy-command-parsers'.

Some parsers reasonably assume that all of their body arguments are
also commands.  For `loopy-iter', this cannot work, so some parsers
need to be tweaked."
  :type '(alist :key-type symbol :value-type function)
  :group 'loopy-iter)

(cl-defun loopy-iter--parse-at-command ((_ target-loop &rest commands))
  "Parse the `at' command as (at &rest COMMANDS).

These commands affect other loops higher up in the call list."
  (loopy--check-target-loop-name target-loop)
  ;; We need to capture all non-main-body instructions into a new `at'
  ;; instruction, so we just temporarily `let'-bind
  ;; `loopy-iter--non-main-body-instructions' while the expanding functions push
  ;; to it, which we then wrap back in a new instruction and pass up to the
  ;; calling function, which consumes instructions.
  (let ((loopy-iter--non-main-body-instructions nil)
        (loopy--loop-name target-loop)
        (loopy--in-sub-level t)
        (loopy-iter--level (1+ loopy-iter--level)))
    `(,@(mapcar (lambda (expr)
                  `(loopy--main-body ,(macroexpand-all
                                       expr
                                       macroexpand-all-environment)))
                commands)
      (loopy--at-instructions
       (,target-loop
        ,@(thread-last loopy-iter--non-main-body-instructions
                       nreverse
                       (apply #'append)))))))

;;;; For parsing special macro arguments

(defcustom loopy-iter-bare-special-macro-arguments
  '( after-do        after else-do else
     before-do       before initially-do initially
     finally-do      finally
     finally-return
     finally-protect finally-protected
     flag            flags
     named
     accum-opt       opt-accum
     with            init
     without         no-with no-init
     wrap)
  "Symbols naming recognized special macro arguments and their aliases.

These should not overwrite any other macros or functions in Emacs Lisp."
  :type '(repeat symbol)
  :group 'loopy-iter)

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
            (bare-names (cl-ecase mode
                          (basic all-names)
                          (iter
                           (cl-intersection all-names
                                            loopy-iter-bare-special-macro-arguments))))
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
          (cl-case mode
            (basic all-sma-names)
            (iter
             (cl-intersection all-sma-names
                              loopy-iter-bare-special-macro-arguments)))))
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
        (push (list loopy--loop-name) loopy--accumulation-places)))
    ;; Return non-name args.
    (nreverse new-body)))

(loopy--internal--def-special-processor with
  ;; Note: These values don't have to be used literally, due to
  ;;       destructuring.
  (setq loopy--with-vars
        (cl-loop for binding in arg-value
                 collect (cond ((symbolp binding)      (list binding nil))
                               ((= 1 (length binding)) (list (cl-first binding) nil))
                               (t                       binding)))))


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

;; TODO: Separate aliases and commands for easier overrides?
(defun loopy--internal--name-parser-conclusion-hashtable (mode)
  "Combine `loopy-aliases' and `loopy-command-parsers' into final alist of command name-parser pairs."
  (let ((ht (make-hash-table :test #'eq :size 200)))
    (cl-loop for (cmd . parser) in loopy-command-parsers
             do (puthash cmd parser ht))
    (when (eq mode 'iter)
      (cl-loop for (cmd . parser) in loopy-iter-overwritten-command-parsers
               do (puthash cmd parser ht)))
    (cl-loop for (orig . aliases) in loopy--obsolete-aliases
             for parser = (gethash orig ht)
             when parser
             do (cl-loop for alias in aliases
                         do (puthash alias parser ht)))
    (cl-loop for (orig . aliases) in loopy-aliases
             for parser = (gethash orig ht)
             when parser
             do (cl-loop for alias in aliases
                         do (puthash alias parser ht)))
    ht)

  ;; (append (cl-loop for (orig . aliases) in loopy-aliases
  ;;                  for parser = (map-elt loopy-command-parsers orig)
  ;;                  when parser
  ;;                  append (cl-loop for alias in aliases
  ;;                                  collect (cons alias parser)))
  ;;         loopy-command-parsers
  ;;         (cl-loop for (orig . aliases) in loopy--obsolete-aliases
  ;;                  for parser = (map-elt loopy-command-parsers orig)
  ;;                  when parser
  ;;                  append (cl-loop for alias in aliases
  ;;                                  collect (cons alias parser))))
  )

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

     (let ((name-parser-hashtable (loopy--internal--name-parser-conclusion-hashtable mode))
           (loopy-iter--non-main-body-instructions)
           (loopy-iter--level 0))

       (loopy--with-protected-stack
        (loopy--mode-let* ((basic . ((suppressed-expanders (mapcar #'list loopy--suppressed-macros))
                                     ;; TODO: Creating a closure here slows down
                                     ;; the tests from taking around 7 seconds
                                     ;; to taking around 20 seconds.  TODO: Why
                                     ;; doesn't this effect the `iter' version?
                                     (command-env nil)
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
                                    ;; NOTE: We can't call
                                    ;; `loopy--process-instructions' in these
                                    ;; expanders because they might be bubbling
                                    ;; up to an `at' command, which would wrap
                                    ;; them.
                                    (command-env
                                     (append
                                      (cl-loop
                                       for keyword in loopy-iter-keywords
                                       collect
                                       (cons keyword
                                             (lambda (&rest args)
                                               (loopy--bind-main-body (main other)
                                                   ;; Bind here in case a command required to be
                                                   ;; in the top level is found in an expression
                                                   ;; while parsing an actual top-level command.
                                                   (let* ((loopy-iter--level (1+ loopy-iter--level))
                                                          (loopy--in-sub-level (> loopy-iter--level 1)))
                                                     (funcall (gethash (car args) name-parser-hashtable)
                                                              args))
                                                 (push other loopy-iter--non-main-body-instructions)
                                                 (macroexp-progn main)))))
                                      (cl-loop
                                       for command in loopy-iter-bare-commands
                                       collect
                                       (cons command
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
                                                       (funcall (gethash cmd name-parser-hashtable)
                                                                (cons cmd args)))
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

          (cl-labels (;; Process body, insert data for optimized accumulations,
                      ;; then process the other instructions:
                      (first-pass (body)
                        (prog1
                            (mapcar (cl-ecase loopy--mode
                                      (basic
                                       (lambda (expr)
                                         (if-let ((parser (map-elt name-parser-hashtable (car-safe expr))))
                                             (loopy--bind-main-body (main other)
                                                 (funcall parser expr)
                                               (loopy--process-instructions other)
                                               (macroexp-progn main))
                                           (signal 'loopy-unknown-command (list expr)))))
                                      (iter
                                       ;; A wrapper to set `loopy--in-sub-level'
                                       ;; correctly: If this is a known command,
                                       ;; expand as normal.  The command parser
                                       ;; will handle sub-level-ness.
                                       ;; Otherwise, while EXPR isn't a command
                                       ;; itself, bind `loopy--in-sub-level' in
                                       ;; case of any commands further down.
                                       (lambda (expr)
                                         (if (map-elt command-env (car expr))
                                             (macroexpand-all expr first-pass-env)
                                           (let ((loopy-iter--level (1+ loopy-iter--level))
                                                 (loopy--in-sub-level t))
                                             (macroexpand-all expr first-pass-env))))))
                                    body)
                          (loopy--process-instructions
                           (thread-last loopy-iter--non-main-body-instructions
                                        nreverse
                                        (apply #'append)))))
                      ;; Expand the optimized accumulation variables,
                      ;; then process the `at' instructions for this loop:
                      (second-pass (body)
                        (prog1
                            (mapcar (lambda (expr)
                                      (macroexpand-all expr second-pass-env))
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

  (declare (debug (&rest [&or loopy--command-edebug-specs
                              loopy--special-macro-arg-edebug-spec])))

  (loopy--internal 'basic body)  )

;;;###autoload
(defmacro loopy-iter (&rest body)
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
to use `loopy' in general.

\(fn CODE-or-COMMAND...)"

  (loopy--internal 'iter body))

(provide 'loopy)
;;; loopy.el ends here
