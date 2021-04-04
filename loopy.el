;;; loopy.el --- A looping macro -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: November 2020
;; URL: https://github.com/okamsn/loopy
;; Version: 0.4
;; Package-Requires: ((emacs "27.1"))
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
;; `loopy' is a macro that is used similarly to `cl-loop'.  It provides "loop
;; commands" that define a loop body and it's surrounding environment, as well
;; as exit conditions.
;;
;; There are several special macro arguments:
;;
;;   - `with' declares variables that are bound in order before and around the
;;     loop, like in a `let*' binding.
;;
;;   - `without' declares variables that ~loopy~ should not try to initialize.
;;
;;   - `before-do' is a list of normal Lisp expressions to run before the loop
;;     executes.
;;
;;   - `after-do' is a list of normal Lisp expressions to run after the successful
;;     completion of the loop.
;;
;;   - `finally-do' is a list of normal Lisp expressions that always run,
;;     regardless of whether an early return was triggered in the loop body.
;;
;;   - `finally-return' is an expression whose value is always returned, regardless
;;     of whether an early return was triggered in the loop body.
;;
;;   - `flags' is a list of symbols that change the macro's behavior.
;;
;; Additionally, a symbol can be used to name the loop.
;;
;; Any argument that doesn't match the above is taken to be a loop command.  The
;; loop commands generally follow the form `(COMMAND VARIABLE-NAME &rest ARGS)'.
;; For example,
;;
;; - To iterate through a sequence, use `(seq elem [1 2 3])' (for
;;   efficiency, there are also more specific commands, like `list').
;; - To collect values into a list, use `(collect my-collection collected-value)'.
;; - To just bind a variable to the result of a Lisp expression, use
;;   `(expr my-var (my-func))'
;;
;; For more information, including the full list of loop commands and how to
;; extend the macro, see this package's Info documentation under Info node
;; `(loopy)'.

;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'seq)
(require 'subr-x)
(require 'loopy-commands)

(defvar loopy-iter--lax-naming) ; A flag defined in file "loopy-iter.el".

;;;; Custom User Options
(defgroup loopy nil
  "A looping and iteration macro."
  :group 'tools
  :prefix "loopy-"
  :link '(url-link "https://github.com/okamsn/loopy"))

(defcustom loopy-default-flags nil
  "Which flags should alter the behavior of `loopy' by default.

This is a list of symbols, each symbol corresponding to a
function in the variable `loopy--flag-settings'."
  :type '(repeat symbol))

;;;; Flags
;;;;; Variables that can be set by flags
(defvar loopy--split-implied-accumulation-results nil
  "Whether implicit accumulation commands should use separate variables.

Nil means that each accumulation command without a named
accumulation variable should accumulate into the same variable,
by default named `loopy-result'.")

(defvar loopy--destructuring-for-with-vars-function nil
  "The function used for destructuring `with' variables.

This function named by this variables receives the bindings given
to the `with' macro argument and should usually return a list of
two elements:

1. A function/macro that works like `let*' and can be used to wrap
   the expanded macro code.
2. The bindings that will be given to this macro.

For example, an acceptable return value might be something like

    (list 'pcase-let* BINDINGS)

which will be used to wrap the loop and other code.

If nil, use `loopy--destructure-for-with-vars-default'.")

(defvar loopy--destructuring-for-iteration-function nil
  "The function to use for destructuring during iteration commands.

The function named by this variable receives a sequence of
variable names and a value expression.  It should return an
expression that can be used in the loop's main body and a list of
variables which must be initialized in the loop.

Generally, the main-body expression should use `setq' to assign
to the variables found in the sequence of variable names, and the
list of variables to initialize will include the variables in
said sequence and any others that might leek through.

If nil, use `loopy--destructure-for-iteration-default'.")

(defvar loopy--destructuring-accumulation-parser nil
  "The function used to parse destructuring accumulation commands.

Unlike `loopy--destructuring-for-iteration-function', the
function named by this variable returns instructions, not a list
of variable-value pairs.

If nil, use `loopy--parse-destructuring-accumulation-command'.")

;;;;; For setting up flags
(defvar loopy--flag-settings nil
  "Alist of functions to run on presence of their respective flag.

These functions will enable features.

Each item is of the form (FLAG . FLAG-ENABLING-FUNCTION).")

;;;;; Built-in flags
;;;;;; Split
(defun loopy--enable-flag-split ()
  "Set `loopy-split-implied-accumulation-results' to t inside the loop."
  (setq loopy--split-implied-accumulation-results t))

(defun loopy--disable-flag-split ()
  "Set `loopy-split-implied-accumulation-results' to t inside the loop."
  ;; Currently redundant, but leaves room for possibilities.
  (if loopy--split-implied-accumulation-results
      (setq loopy--split-implied-accumulation-results nil)))

(add-to-list 'loopy--flag-settings (cons 'split #'loopy--enable-flag-split))
(add-to-list 'loopy--flag-settings (cons '+split #'loopy--enable-flag-split))
(add-to-list 'loopy--flag-settings (cons '-split #'loopy--disable-flag-split))

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

(add-to-list 'loopy--flag-settings
             (cons 'default #'loopy--enable-flag-default))

;;;; Important Variables
;; These only set in the `loopy' macro, but that might change in the future.  It
;; might be cleaner code to modify from the parsing function, after the macro
;; has already set them to nil.
(defvar loopy--flags nil
  "Symbols/flags whose presence changes the behavior of `loopy'.

NOTE: This functionality might change in the future.")

(defvar loopy--valid-macro-arguments
  '( flag flags with let* without no-init before-do before initially-do
     initially after-do after else-do else finally-do finally finally-return)
  "List of valid keywords for `loopy' macro arguments.

This variable is used to signal an error instead of silently failing.")

(defvar loopy--loop-name nil
  "A symbol that names the loop, appropriate for use in `cl-block'.")

(defvar loopy--with-vars nil
  "With Forms are variables explicitly created using the `with' keyword.

This is a list of ((VAR1 VAL1) (VAR2 VAL2) ...).  If VAR is a
sequence, then it will be destructured.  How VAR and VAL are
used, as well as how the bindings are expanded into the loop's
surrounding code, is determined by the destructuring system being
used.

They are created by passing (with (VAR1 VAL1) (VAR2 VAL2) ...) to
`loopy'.")

(defvar loopy--without-vars nil
  "A list of variables that `loopy' won't try to initialize.

`loopy' tries to initialize all variables that it uses in a
`let'-like form, but this isn't always desired.

This is used in `loopy--bound-p', and is of the form (VAR1 VAR2 ...).
There are no values in this list, only variable names.")

(defvar loopy--generalized-vars nil
  "A list of symbols and macro expansions explicitly named in loop commands.

To create `setf'-able variables, the symbol needs to be expanded
to a form that can be treated as such.  In this case, with
`cl-symbol-macrolet'.")

(defvar loopy--iteration-vars nil
  "A list of symbols and values to initialize variables for iteration commands.

These initializations are sensitive to order.

This list includes variables explicitly named in a command (such
as the `i' in `(list i my-list)'), variables required for
destructuring in iteration commands, and other variables required
for iteration.")

(defvar loopy--accumulation-vars nil
  "Initializations of the variables needed for accumulation.

This list includes variables explicitly named in a command (such
as the `collection' in `(collect collection value)') and variables
required for destructuring in accumulation commands.

Unlike in `loopy--iteration-vars', these variables should be
accessible from anywhere in the macro, and should not be reset
for sub-loops.")

(defvar loopy--before-do nil
  "A list of expressions to evaluate before the loop starts.
This is done using a `progn'.")

(defvar loopy--pre-conditions nil
  "The list of expressions that determine whether the `while' loop starts/loops.
These are fed to an `and', so all conditions must be true for the
  `while' to start/loop.")

(defvar loopy--main-body nil
  "A list of expressions to run (in order) inside the loop.
These expressions are created by parsing the loop commands passed to `loopy'.

For example, the command (list i my-list) effectively puts
\(setq i (pop my-list)) into the loop body.  Most commands require some setup,
and so don't affect only the loop body.")

(defvar loopy--latter-body nil
  "A list of expressions to run after the main loop body.
These expressions are created by parsing the loop commands passed to `loopy'.

For example, updating an indexing variable should only happen
after the variable is used.")

(defvar loopy--post-conditions nil
  "Post-conditions that could cause the loop to exit evaluating the loop body.

All expressions in the list must be true for the program to continue.
This is similar to `do-while' in other languages.")

(defvar loopy--after-do nil
  "Expressions to run (in order) after the loop successfully completes.
These run in a `progn'.")

(defvar loopy--final-do nil
  "A list of expressions always run (in order) after the loop finishes/exits.")

(defvar loopy--final-return nil
  "What the macro finally returns.  This overrides any early return value.")

(defvar loopy--implicit-return nil
  "The implicit return value of loops that use accumulation commands.")

(defvar loopy-result nil
  "The result of using implicit accumulation commands in `loopy'.

All accumulation commands with no given variable (such
as `(collect my-val)') will accumulate into `loopy-result'.

While `loopy-result' is an implied return value, it need not be
the only implied value, and can still be returned in a list with
other implied return values, if any.")

;;;;; Variables for constructing the code
(defvar loopy--in-sub-level nil
  "Whether the commands parsed are not in the top level of a loop.

Certain commands (e.g., `list' or `array') can only occur in the
top level of a loop.  Sub-loops (those created by the `sub-loop'
command) create for themselves a new, local top level.")

;; These variable affect how the code is expanded.
(defvar loopy--skip-used nil
  "Whether a skip/continue command is present in the loop main body.")

(defvar loopy--tagbody-exit-used nil
  "Whether a command uses a tag-body to jump to the end of the `cl-block'.

This has the effect of leaving the loop without immediately
returning a value.")

(defvar loopy--implicit-accumulation-final-update nil
  "Actions to perform on the implicit accumulation variable.

So as to avoid conflicts, there can be only one final action.
This variable is a list of such actions, but only the action at
the head of the list will be performed.

For example, it is usually more efficient to build a list in
reverse order, so a final update might be to reverse a backwards
list so that it is in the correct order.")

(defvar loopy--implicit-accumulation-updated nil
  "Whether the implicit accumulation commands were finally updated.

If a `cl-tagbody' exit is used (such by a `while' or `until'
command, which don't return values, just leaving the loop), then
the `after-do' body is skipped.  This also has the consequence of
skipping the final update to implicit accumulation variables,
which needs to run before the `after-do' body so that the
variable is safe when accessed.

To work around this, the final update before the `after-do' will
set this variable to t if it has run.  This value will be
checked after the tag-body exit if `loopy--tagbody-exit-used' is
t.")

(defvar loopy--variables
  '(loopy--loop-name
    loopy--with-vars
    loopy--without-vars
    loopy--before-do
    loopy--after-do
    loopy--final-do
    loopy--final-return

    ;; -- Vars for processing loop commands --
    loopy--iteration-vars
    loopy--accumulation-vars
    loopy--generalized-vars
    loopy--pre-conditions
    loopy--main-body
    loopy--latter-body
    loopy--post-conditions
    loopy--implicit-return

    ;; -- Variables for constructing code --
    loopy--skip-used
    loopy--tagbody-exit-used
    loopy--implicit-accumulation-final-update
    loopy--in-sub-level

    ;; -- Flag Variables --
    loopy-iter--lax-naming
    loopy--destructuring-for-with-vars-function
    loopy--destructuring-accumulation-parser
    loopy--split-implied-accumulation-results)
  "These variables must be `let'-bound around the loop.

This list is mainly fed to the macro `loopy--wrap-variables-around-body'.")

;;;; Miscellaneous and Utility Functions
(defun loopy--bound-p (var-name)
  "Check if VAR-NAME (a symbol) is already bound for the macro.

This can happen when multiple loop commands refer to the same
variable, or when a variable is introduced via `with'.

The variable can exist in `loopy--with-vars',
`loopy--iteration-vars', `loopy--accumulation-vars', or
`loopy--generalized-vars'."
  (or (memq var-name (mapcar #'car loopy--with-vars))
      (memq var-name (mapcar #'car loopy--iteration-vars))
      (memq var-name (mapcar #'car loopy--accumulation-vars))
      (memq var-name (mapcar #'car loopy--generalized-vars))
      (memq var-name loopy--without-vars)))

(defun loopy--already-implicit-return (var-name)
  "Check whether variable VAR-NAME is in the list of implied return values.

Accumulation commands can operate on the same variable, and we
  don't want that variable to appear more than once as an implied return."
  (memq var-name loopy--implicit-return))

(defun loopy--special-macro-argument-p (symbol arguments-list)
  "Whether SYMBOL is a special macro argument (including aliases).

Special macro arguments are listed in ARGUMENTS-LIST
or `loopy-custom-command-aliases'."
  (memq symbol (append arguments-list
                       (let ((results))
                         (dolist (alias loopy-custom-command-aliases)
                           (when (memq (cdr alias) arguments-list)
                             (push (car alias) results)))
                         results))))

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

;;;; Destructuring functions.
;; Note that functions which are only used for commands are found in
;; `loopy-commands.el'.  The functions found here are used generally.

(defun loopy--basic-builtin-destructuring (var value-expression)
  "Destructure VALUE-EXPRESSION according to VAR.

Return a list of variable-value pairs (not dotted), suitable for
substituting into a `let*' form or being combined under a
`setq' form."
  (cl-typecase var
    (symbol
     `((,(if (eq var '_) (gensym "discarded-value-") var)
        ,value-expression)))
    (list
     ;; NOTE: (A . (B C)) is really just (A B C), so you can't have a
     ;;       non-proper list with a list as the last element.  However, the
     ;;       last element can be an array.
     ;;
     (let* ((is-proper-list (proper-list-p var))
            (normalized-reverse-var nil))
       ;; If `var' is a list, always create a "normalized" variable list,
       ;; since proper lists are easier to work with, as many looping/mapping
       ;; functions expect them.
       (while (car-safe var)
         (push (pop var) normalized-reverse-var))
       ;; If the last element in `var' was a dotted pair, then `var' is now a
       ;; single symbol, which must still be added to the normalized `var'
       ;; list.
       (when var (push var normalized-reverse-var))

       ;; The `last' of (A B . C) is (B . C), but we actually want C, so we
       ;; check the "normalized" var list.
       (let* ((last-var (cl-first normalized-reverse-var))
              (last-var-is-symbol (symbolp last-var))
              ;; To only evaluate `value-expression' once, we bind it's value
              ;; to last declared element/variable in `var', and set the
              ;; remaining variables by `pop'-ing that lastly listed, firstly
              ;; set variable.  However, if that variable is actually a
              ;; sequence, then we need to use a `value-holder' instead.
              (value-holder (if last-var-is-symbol
                                (if (eq '_ last-var)
                                    (gensym "discarded-value-")
                                  last-var)
                              (gensym "destructuring-list-")))
              (destructurings
               ;; Will push lists of destructurings, and then append together
               ;; with `apply'.
               `(((,value-holder ,value-expression)))))

         (let ((passed-value-expression `(pop ,value-holder)))
           (dolist (symbol-or-seq (reverse (cl-rest normalized-reverse-var)))
             (push (loopy--basic-builtin-destructuring
                    symbol-or-seq passed-value-expression)
                   destructurings)))

         ;; Now come back to end.  If `var' is not a proper list and
         ;; `last-var' is a symbol (as with the B in '(A . B)) , then B is now
         ;; already the correct value (which is the ending `cdr' of the list),
         ;; and we don't have to do anything else.
         (if is-proper-list
             (if last-var-is-symbol
                 ;; Otherwise, if `var' is a proper list and `last-var' is a
                 ;; symbol, then we need to take the `car' of that `cdr'.
                 (push `((,value-holder (car ,value-holder)))
                       destructurings)
               ;; If `last-var' is not a symbol, then if `var' is a proper list,
               ;; then we now have a list like ((C D)) from (A B (C D)).  We
               ;; only want to pass in the (C D).
               (push (loopy--basic-builtin-destructuring
                      last-var `(car ,value-holder))
                     destructurings))
           ;; Otherwise, we might have something like [C D] from
           ;; (A B . [C D]), where we don't need to take the `car'.
           (unless last-var-is-symbol
             (push (loopy--basic-builtin-destructuring
                    last-var value-holder)
                   destructurings)))

         ;; Return the list of instructions.
         (apply #'append (nreverse destructurings)))))

    (array
     ;; For arrays, we always need a value holder so that `value-expression'
     ;; is evaluated only once.
     (let* ((value-holder (gensym "destructuring-array-"))
            (destructurings
             `(((,value-holder ,value-expression)))))
       (cl-loop for symbol-or-seq across var
                for index from 0
                do (push (loopy--basic-builtin-destructuring
                          symbol-or-seq `(aref ,value-holder ,index))
                         destructurings))

       ;; Return the list of instructions.
       (apply #'append (nreverse destructurings))))
    (t
     (error "Don't know how to destructure this: %s" var))))

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
                   (loopy--basic-builtin-destructuring var val)))
                bindings)))

(cl-defun loopy--find-special-macro-arguments (names body)
  "Find any usages of special macro arguments NAMES in BODY, given aliases.

NAMES can be either a single quoted name or a list of quoted names.

Aliases can be found in `loopy-custom-command-aliases'."
  (dolist (keyword
           (if (listp names)
               (append names
                       (cl-loop for alias in loopy-custom-command-aliases
                                if (memq (cdr alias) names)
                                collect (car alias)))
             (cons names (cl-loop for alias in loopy-custom-command-aliases
                                  if (eq (cdr alias) names)
                                  collect (car alias)))))
    (when-let ((target (cdr (assq keyword body))))
      (cl-return-from loopy--find-special-macro-arguments target))))


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
  ;;      (let* ,loopy--iteration-vars
  ;;        (let ,loopy--accumulation-vars
  ;;          ;; If we need to, capture early return, those that has less
  ;;          ;; priority than a final return.
  ;;          (let ((loopy--early-return-capture
  ;;                 (cl-block ,loopy--loop-name
  ;;                   ,@loopy--before-do
  ;;                   (cl-tagbody
  ;;                    (while ,loopy--pre-conditions
  ;;                      (cl-tagbody
  ;;                       ,@loopy--main-body
  ;;                       loopy--continue-tag
  ;;                       ,@loopy--latter-body
  ;;                       (unless ,loopy--post-conditions
  ;;                         (cl-return-from ,loopy--loop-name
  ;;                           ,loopy--implicit-return))))
  ;;                    ,@loopy--after-do)
  ;;                   loopy--non-returning-exit-tag
  ;;                   ,loopy--implicit-accumulation-final-update
  ;;                   ,loopy--implicit-return)))
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

      (when loopy--skip-used
        (setq result `(cl-tagbody ,@result loopy--continue-tag)
              result-is-one-expression t))

      (when loopy--latter-body
        (setq result (append result loopy--latter-body)))

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
      (when loopy--implicit-accumulation-final-update
        (setq result
              (if loopy--tagbody-exit-used
                  `(,@(get-result)
                    ,(car loopy--implicit-accumulation-final-update)
                    (setq loopy--implicit-accumulation-updated t))
                `(,@(get-result)
                  ,(car loopy--implicit-accumulation-final-update)))
              result-is-one-expression nil))

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

      (when loopy--tagbody-exit-used
        (setq result (if loopy--implicit-accumulation-final-update
                         `(cl-tagbody
                           ,@(get-result)
                           loopy--non-returning-exit-tag
                           ;; Even if leave the loop early, make sure the
                           ;; update is always run.
                           (if loopy--implicit-accumulation-updated
                               nil
                             ,(car loopy--implicit-accumulation-final-update)))
                       `(cl-tagbody
                         ,@(get-result)
                         loopy--non-returning-exit-tag))
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

      ;; Declare the loop variables.
      (when loopy--iteration-vars
        (setq result `(let* ,loopy--iteration-vars ,@(get-result))
              result-is-one-expression t))

      ;; If there are final updates to made and a tag-body exit that can skip
      ;; them, then we must initialize `loopy--implicit-accumulation-updated'.
      (when (and loopy--implicit-accumulation-final-update
                 loopy--tagbody-exit-used)
        (setq result `(let ((loopy--implicit-accumulation-updated nil))
                        ,@(get-result))
              result-is-one-expression t))

      (when loopy--accumulation-vars
        (setq result `(let ,loopy--accumulation-vars ,@(get-result))
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

(defmacro loopy--wrap-variables-around-body (&rest body)
  "Wrap variables in `loopy--variables' in `let*' bindings around BODY."
  (macroexp-let* (mapcar (lambda (x) (list x nil))
                         loopy--variables)
                 (macroexp-progn body)))

;;;###autoload
(cl-defmacro loopy (&rest body)
  "A looping macro.

The macro takes several top-level arguments, all, except a loop
name, being a list beginning with one of the keywords below.  To
name a loop, pass in an unquoted symbol as an argument.

- `with', `let*': Declare variables before the loop.

- `without', `no-init': Variables that `loopy' should not try to
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

Any another argument is assumed to be a loop command.  For more
information, including a list of available loop commands, see the
Info node `(loopy)' distributed with this package."

  (declare (debug (&rest ;; TODO: Is this correct?
                   [&or
                    ([&or "with" "let*"] &rest (symbolp &optional form))
                    ([&or "without" "no-init"] &rest symbolp)
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

   ;; There should be only one of each of these arguments.

   ;; Flags

   ;; Process any flags passed to the macro.  In case of conflicts, the
   ;; processing order is:
   ;;
   ;; 1. Flags in `loopy-default-flags'.
   ;; 2. Flags in the `flag' macro argument, which can
   ;;    undo the first group.

   (when-let ((loopy--all-flags
               (append loopy-default-flags
                       (loopy--find-special-macro-arguments '(flag flags)
                                                            body))))
     (dolist (flag loopy--all-flags)
       (if-let ((func (cdr (assq flag loopy--flag-settings))))
           (funcall func)
         (error "Loopy: Flag not defined: %s" flag))))

   ;; With
   ;; Note: These values don't have to be used literally, due to
   ;; destructuring.
   (setq loopy--with-vars
         (let ((result))
           (dolist (binding (loopy--find-special-macro-arguments
                             '(with let*) body))
             (push (cond
                    ((symbolp binding)
                     (list binding nil))
                    ((= 1 (length binding))
                     (list (cl-first binding) nil))
                    (t
                     binding))
                   result))
           (nreverse result)))

   ;; Without
   (setq loopy--without-vars
         (loopy--find-special-macro-arguments '(without no-init) body))

   ;; Before do
   (setq loopy--before-do
         (loopy--find-special-macro-arguments '( before-do before
                                                 initially-do initially)
                                              body))

   ;; After do
   (setq loopy--after-do
         (loopy--find-special-macro-arguments '( after-do after
                                                 else-do else)
                                              body))

   ;; Finally Do
   (setq loopy--final-do
         (loopy--find-special-macro-arguments '(finally-do finally) body))

   ;; Final Return
   (setq loopy--final-return
         (when-let ((return-val
                     (loopy--find-special-macro-arguments 'finally-return
                                                          body)))
           (if (= 1 (length return-val))
               (car return-val)
             (cons 'list return-val))))

;;;;; Check the loop name and loop commands.

   (dolist (arg body)
     (cond
      ((symbolp arg)
       (setq loopy--loop-name arg))
      ;; Do nothing for macro arguments, since they were already processed.
      ((loopy--special-macro-argument-p
        (car-safe arg) loopy--valid-macro-arguments)
       t)
      (t
       ;; Body forms have the most variety.
       ;; An instruction is (PLACE-TO-ADD . THING-TO-ADD).
       ;; Things added are expanded in place.
       (dolist (instruction (loopy--parse-loop-command arg))
         ;; Do it this way instead of with `set', cause was getting errors
         ;; about void variables.
         (cl-case (car instruction)
           (loopy--generalized-vars
            (loopy--validate-binding (cdr instruction))
            (push (cdr instruction) loopy--generalized-vars))
           (loopy--iteration-vars
            (loopy--validate-binding (cdr instruction))
            ;; Don't want to accidentally rebind variables to `nil'.
            (unless (loopy--bound-p (cadr instruction))
              (push (cdr instruction) loopy--iteration-vars)))
           (loopy--accumulation-vars
            (loopy--validate-binding (cdr instruction))
            ;; Don't want to accidentally rebind variables to `nil'.
            (unless (loopy--bound-p (cadr instruction))
              (push (cdr instruction) loopy--accumulation-vars)))
           (loopy--pre-conditions
            (push (cdr instruction) loopy--pre-conditions))
           (loopy--main-body
            (push (cdr instruction) loopy--main-body))
           (loopy--latter-body
            (push (cdr instruction) loopy--latter-body))
           (loopy--post-conditions
            (push (cdr instruction) loopy--post-conditions))
           (loopy--implicit-return
            (unless (loopy--already-implicit-return (cdr instruction))
              (push (cdr instruction) loopy--implicit-return)))
           (loopy--implicit-accumulation-final-update
            (push (cdr instruction) loopy--implicit-accumulation-final-update))

           ;; Code for conditionally constructing the loop body.
           (loopy--skip-used
            (setq loopy--skip-used t))
           (loopy--tagbody-exit-used
            (setq loopy--tagbody-exit-used t))

           ;; Places users probably shouldn't push to, but can if they want:
           (loopy--before-do
            (push (cdr instruction) loopy--before-do))
           (loopy--after-do
            (push (cdr instruction) loopy--after-do))
           (loopy--final-do
            (push (cdr instruction) loopy--final-do))
           (loopy--final-return
            (push (cdr instruction) loopy--final-return))
           (t
            (error "Loopy: Unknown body instruction: %s" instruction)))))))

   ;; Make sure the order-dependent lists are in the correct order.
   (setq loopy--main-body (nreverse loopy--main-body)
         loopy--iteration-vars (nreverse loopy--iteration-vars)
         loopy--implicit-return (when (consp loopy--implicit-return)
                                  (if (= 1 (length loopy--implicit-return))
                                      ;; If implicit return is just a single thing,
                                      ;; don't use a list.
                                      (car loopy--implicit-return)
                                    ;; If multiple items, be sure to use a list
                                    ;; in the correct order.
                                    `(list ,@(nreverse loopy--implicit-return)))))

;;;;; Constructing/Creating the returned code.
   (loopy--expand-to-loop)))

(provide 'loopy)
;;; loopy.el ends here
