;;; loopy.el --- A looping macro -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: November 2020
;; URL: https://github.com/okamsn/loopy
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
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
;; For more information, see Loopy’s README.org or the Info files derived from
;; that README with the command `info'.


;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'seq)
(require 'subr-x)

;;;; Important Variables
;; These only set in the `loopy' macro, but that might change in the future.  It
;; might be cleaner code to modify from the parsing function, after the macro
;; has already set them to nil.

(defvar loopy--loop-name nil
  "A symbol that names the loop, appropriate for use in `cl-block'.")

(defvar loopy--with-vars nil
  "With Forms are variables explicitly created using the `with' keyword.

This is a list of ((VAR1 VAL1) (VAR2 VAL2) ...).
They are inserted into the variable declarations of a `let*' binding.
They are created by passing (with (VAR1 VAL1) (VAR2 VAL2) ...) to `loopy'.")

(defvar loopy--implicit-vars nil
  "A list of variables and their values implicitly created by loop commands.

This is a list of ((VAR1 VAL1) (VAR2 VAL2) ...).
They are inserted into the variable declarations of a `let' binding.

For example, using (list i '(1 2 3)) will create an implicit variable
containing '(1 2 3).  This makes iteration easier.")

(defvar loopy--explicit-vars nil
  "A list of variables and values explicitly named in loop commands.

This is a list of ((VAR1 VAL1) (VAR2 VAL2) ...).
They are inserted into the variable declarations of a `let' binding.

This is useful for lexically scoping variables, and for declaring
an initial value before a different value assigned in the loop.

For example, using (list i '(1 2 3)) will create an explicit variable `i'.")

(defvar loopy--explicit-generalized-vars nil
  "A list of symbols and macro expansions explicitly named in loop commands.

To create `setf'-able variables, the symbol needs to be expanded
to a form that can be treated as such.  In this case, with
`cl-symbol-macrolet'.")

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

;;;;; Variables for constructing the code

;; These variable affect how the code is expanded.
(defvar loopy--skip-used nil
  "Whether a skip/continue command is present in the loop main body.")

(defvar loopy--tagbody-exit-used nil
  "Whether a command uses a tag-body to jump to the end of the `cl-block'.")

;;;; Errors
(define-error 'loopy-error
  "Error in `loopy' macro")

(define-error 'loopy-unknown-command
  "Loopy: Unknown command"
  'loopy-error)

(define-error 'loopy-wrong-number-of-command-arguments
  "Loopy: Wrong number of command arguments"
  '(loopy-error wrong-number-of-arguments))

(define-error 'loopy-bad-command-arguments
  "Loopy: Bad command arguments"
  'loopy-error)

;;;; Miscellaneous and Utility Functions
(defun loopy--bound-p (var-name)
  "Check if VAR-NAME (a symbol) is already bound for the macro.

This can happen when multiple loop commands refer to the same
variable, or when a variable is introduced via `with'.

The variable can exist in `loopy--with-vars', `loopy--explicit-vars',
or `loopy--explicit-generalized-vars'."
  (or (memq var-name (mapcar #'car loopy--with-vars))
      (memq var-name (mapcar #'car loopy--explicit-vars))
      (memq var-name (mapcar #'car loopy--explicit-generalized-vars))))

(defun loopy--already-implicit-return (var-name)
  "Check whether variable VAR-NAME is in the list of implied return values.

Accumulation commands can operate on the same variable, and we
  don't want that variable to appear more than once as an implied return."
  (memq var-name loopy--implicit-return))

(defun loopy--get-function-symbol (function-form)
  "Return the actual symbol described by FUNCTION-FORM.

When a quoted argument is passed to a macro, it can appear
as `(quote my-var)' or `(function my-func)' inside the body.  For
expansion, we generally only want the actual symbol."
  (if (nlistp function-form)
      function-form
    (cl-case (car function-form)
      ((function quote) (cadr function-form))
      (lambda function-form)
      (t (error "This function form is unrecognized: %s" function-form)))))

(defun loopy--initialize-vars (place vars &optional value)
  "Create instructions for initializing variables in VARS to VALUE in PLACE.

PLACE should be `loopy--explicit-vars' or `loopy--implicit-vars'."
  ;; Just make sure it's always a sequence.
  (when (symbolp vars) (setq vars (list vars)))
  (mapcar (lambda (var) (cons place `(,var ,value))) vars))

(cl-defun loopy--create-destructured-assignment
    (var value-expression &optional generalized)
  "If needed, use destructuring to initialize and assign to variables.

VAR is a symbol for a variable name, or a list of such
symbols (as a dotted pair or as a normal list).  VALUE-EXPRESSION
is the value expression to be assigned and maybe destructured,
such as an expression meaning the head of a list or an element in
an array.

Optional GENERALIZED means to create a generalized variable in
`loopy--explicit-generalized-vars' instead of creating a normal
variable."
  (if generalized
      (cl-typecase var
        ;; Check if `var' is a single symbol.
        (symbol
         (if (eq var '_)
             `((loopy--explicit-generalized-vars
                . (,(gensym "destructuring-ref-") ,value-expression)))
           ;; Most functions expect a list of instructions, not one.
           `((loopy--explicit-generalized-vars . (,var ,value-expression)))))
        (list
         ;; If `var' is not proper, then the end of `var' can't be `car'-ed
         ;; safely, as it is just a symbol and not a list.  Therefore, if `var'
         ;; is still non-nil after the `pop'-ing, we know to set the remaining
         ;; symbol that is now `var' to some Nth `cdr'.
         (let ((instructions) (index 0))
           (while (car-safe var)
             (push (loopy--create-destructured-assignment
                    (pop var) `(nth ,index ,value-expression) 'generalized)
                   instructions)
             (setq index (1+ index)))
           (when var
             (push (loopy--create-destructured-assignment
                    var `(nthcdr ,index ,value-expression) 'generalized)
                   instructions))
           (apply #'append (nreverse instructions))))
        (array
         (cl-loop for symbol-or-seq across var
                  for index from 0
                  append (loopy--create-destructured-assignment
                          symbol-or-seq `(aref ,value-expression ,index)
                          'generalized)))
        (t
         (error "Don't know how to destructure this: %s" var)))

    ;; Otherwise assigning normal variables:
    (cl-typecase var
      (symbol
       (if (eq var '_)
           (let ((value-holder (gensym "discarded-value-")))
             `((loopy--explicit-vars . (,value-holder nil))
               (loopy--main-body     . (setq ,value-holder ,value-expression))))
         `((loopy--explicit-vars . (,var nil))
           (loopy--main-body     . (setq ,var ,value-expression)))))
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
                (instructions
                 `(((,(if last-var-is-symbol    ; We're going to append lists
                          'loopy--explicit-vars ; of instructions, so we make
                        'loopy--implicit-vars)  ; a list of 1 element, and
                     . (,value-holder nil))     ; in that element place two
                    (loopy--main-body           ; instructions.
                     . (setq ,value-holder ,value-expression))))))

           (let ((passed-value-expression `(pop ,value-holder)))
             (dolist (symbol-or-seq (reverse (cl-rest normalized-reverse-var)))
               (push (loopy--create-destructured-assignment
                      symbol-or-seq passed-value-expression)
                     instructions)))

           ;; Now come back to end.  If `var' is not a proper list and
           ;; `last-var' is a symbol (as with the B in '(A . B)) , then B is now
           ;; already the correct value (which is the ending `cdr' of the list),
           ;; and we don't have to do anything else.
           (if (and last-var-is-symbol is-proper-list)
               ;; Otherwise, if `var' is a proper list and `last-var' is a
               ;; symbol, then we need to take the `car' of that `cdr'.
               (push `((loopy--main-body
                        . (setq ,value-holder (car ,value-holder))))
                     instructions)
             ;; Otherwise, `last-var' is a sequence.
             (if is-proper-list
                 ;; If `var' is a proper list, then we now have a list like ((C
                 ;; D)) from (A B (C D)).  We only want to pass in the (C D).
                 (push (loopy--create-destructured-assignment
                        last-var `(car ,value-holder))
                       instructions)
               ;; Otherwise, we might have something like [C D] from
               ;; (A B . [C D]), where we don't need to take the `car'.
               (push (loopy--create-destructured-assignment
                      last-var value-holder)
                     instructions)))

           ;; Return the list of instructions.
           (apply #'append (nreverse instructions)))))

      (array
       ;; For arrays, we always need a value holder so that `value-expression'
       ;; is evaluated only once.
       (let* ((value-holder (gensym "destructuring-array-"))
              (instructions
               `(((loopy--implicit-vars . (,value-holder nil))
                  (loopy--main-body . (setq ,value-holder ,value-expression))))))
         (cl-loop for symbol-or-seq across var
                  for index from 0
                  do (push (loopy--create-destructured-assignment
                            symbol-or-seq `(aref ,value-holder ,index))
                           instructions))

         ;; Return the list of instructions.
         (apply #'append (nreverse instructions))))
      (t
       (error "Don't know how to destructure this: %s" var)))))

;;;; Custom Commands and Parsing
(defgroup loopy nil
  "A looping macro similar to `cl-loop'."
  :group 'extensions
  :prefix "loopy-")

;;;###autoload
(defcustom loopy-custom-command-parsers nil
  "An alist of pairs of a quoted command name and a parsing function.

The parsing function is chosen based on the command name (such as
`list' in `(list i my-list)'), not the usage of the command.  That is,

  (my-command var1)

and

  (my-command var1 var2)

are both parsed by the same function, but that parsing function
is not limited in how it responds to different usages.  If you
really want, it can return different instructions each time.
Learn more with `(info \"(emacs)loopy\")'.

For example, to add a `when' command (if one didn't already
exist), one could do

  (add-to-list \'loopy-custom-command-parsers
                (cons 'when #'my-loopy-parse-when-command))"
  :group 'loopy
  :type '(alist :key-type sexp :value-type function))

(defun loopy--get-custom-command-parser (command)
  "Get the parsing function for COMMAND from `loopy-custom-command-parsers'.
This uses the command name (such as `list' in `(list i my-list)')."
  (alist-get (car command) loopy-custom-command-parsers))

;;;; Included parsing functions.

(cl-defun loopy--parse-when-unless-command ((name condition &rest body))
  "Parse `when' and `unless' commands.

- NAME is `when' or `unless'.
- CONDITION is the condition.
- BODY is the sub-commands."
  (let (full-instructions
        conditional-body)
    (dolist (instruction (loopy--parse-loop-commands body))
      (if (eq 'loopy--main-body (car instruction))
          (push (cdr instruction) conditional-body)
        (push instruction full-instructions)))
    (push `(loopy--main-body . (,name ,condition ,@(nreverse conditional-body)))
          full-instructions)
    full-instructions))

(cl-defun loopy--parse-group-command ((_ &rest body))
  "Parse the `group' loop command.

BODY is one or more commands to be grouped by a `progn' form."
  (let ((full-instructions) (progn-body))
    (dolist (instruction (loopy--parse-loop-commands body))
      (if (eq (car instruction) 'loopy--main-body)
          (push (cdr instruction) progn-body)
        (push instruction full-instructions)))
    (push (cons 'loopy--main-body
                (cons 'progn (nreverse progn-body)))
          full-instructions)
    full-instructions))

(cl-defun loopy--parse-if-command ((_
                                    condition
                                    &optional if-true
                                    &rest if-false))
  "Parse the `if' loop command.  This takes the entire command.

- CONDITION is a Lisp expression.
- IF-TRUE is the first sub-command of the `if' command.
- IF-FALSE are all the other sub-commands."
  (let (full-instructions
        if-true-main-body
        if-false-main-body)
    (dolist (instruction (loopy--parse-loop-command if-true))
      (if (eq 'loopy--main-body (car instruction))
          (push (cdr instruction) if-true-main-body)
        (push instruction full-instructions)))
    (dolist (instruction (loopy--parse-loop-commands if-false))
      (if (eq 'loopy--main-body (car instruction))
          (push (cdr instruction) if-false-main-body)
        (push instruction full-instructions)))
    ;; Push the actual main-body instruction.
    (setq if-true-main-body
          (if (= 1 (length if-true-main-body))
              (car if-true-main-body)
            (cons 'progn (nreverse if-true-main-body))))
    (push `(loopy--main-body
            . (if ,condition
                  ,if-true-main-body
                ,@(nreverse if-false-main-body)))
          full-instructions)
    ;; Return the list of instructions.
    full-instructions))

(cl-defun loopy--parse-cond-command ((_ &rest clauses))
  "Parse the `cond' command.  This works like the `cond' special form.

CLAUSES are lists of a Lisp expression followed by one or more
loop commands.

The Lisp expression and the loopy-body instructions from each
command are inserted into a `cond' special form."
  (let (full-instructions actual-cond-clauses)
    (dolist (clause clauses)
      (let ((instructions (loopy--parse-loop-commands (cl-rest clause)))
            clause-body)
        (dolist (instruction instructions)
          (if (eq (car instruction) 'loopy--main-body)
              (push (cdr instruction) clause-body)
            (push instruction full-instructions)))
        ;; Create a list of the condition and the loop-body code.
        (push (cons (cl-first clause) (nreverse clause-body))
              actual-cond-clauses)))
    ;; Wrap the `actual-cond-clauses' in a `cond' special form, and return all
    ;; instructions.
    (cons `(loopy--main-body . ,(cons 'cond (nreverse actual-cond-clauses)))
          full-instructions)))

(cl-defun loopy--parse-do-command ((_ &rest expressions))
  "Parse the `do' loop command.

Expressions are normal Lisp expressions, which are inserted into
the loop literally (not even in a `progn')."
  (mapcar (lambda (expr) (cons 'loopy--main-body expr))
          expressions))

(cl-defun loopy--parse-expr-command ((_ var &rest vals))
  "Parse the `expr' command.

- VAR is the variable to assign.
- VALS are the values to assign to VAR."
  (let ((arg-length (length vals))
        (value-selector (gensym "expr-value-selector-")))
    (cl-case arg-length
      ;; If no values, repeatedly set to `nil'.
      (0 (loopy--create-destructured-assignment
          var nil))
      ;; If one value, repeatedly set to that value.
      (1 (loopy--create-destructured-assignment
          var (car vals)))
      ;; If two values, repeatedly check against `value-selector' to
      ;; determine if we should assign the first or second value.  This is
      ;; how `cl-loop' does it.
      (2
       `((loopy--implicit-vars . (,value-selector t))
         ,@(loopy--create-destructured-assignment
            var `(if ,value-selector ,(cl-first vals) ,(cl-second vals)))
         (loopy--latter-body . (setq ,value-selector nil))))
      (t
       `((loopy--implicit-vars . (,value-selector 0))
         (loopy--latter-body
          . (when (< ,value-selector (1- ,arg-length))
              (setq ,value-selector (1+ ,value-selector))))
         ;; Assign to var based on the value of value-selector.  For
         ;; efficiency, we want to check for the last expression first,
         ;; since it will probably be true the most times.  To enable
         ;; that, the condition is whether the counter is greater than
         ;; the index of EXPR in REST minus one.
         ;;
         ;; E.g., for '(a b c),
         ;; use '(cond ((> cnt 1) c) ((> cnt 0) b) ((> cnt -1) a))
         ,@(loopy--create-destructured-assignment
            var (let ((body-code nil) (index 0))
                  (dolist (value vals)
                    (push `((> ,value-selector ,(1- index))
                            ,value)
                          body-code)
                    (setq index (1+ index)))
                  (cons 'cond body-code))))))))

(cl-defun loopy--parse-array-command
    ((_ var val)
     &optional
     (value-holder (gensym "array-")) (index-holder (gensym "index-")))
  "Parse the `array' command.

- VAR is a variable name.
- VAL is an array value.
- Optional VALUE-HOLDER holds the array value.
- Optional INDEX-HOLDER holds the index value."
  `((loopy--implicit-vars  . (,value-holder ,val))
    (loopy--implicit-vars  . (,index-holder 0))
    ,@(loopy--create-destructured-assignment var
                                             `(aref ,value-holder ,index-holder))
    (loopy--latter-body    . (setq ,index-holder (1+ ,index-holder)))
    (loopy--pre-conditions . (< ,index-holder (length ,value-holder)))))

(cl-defun loopy--parse-array-ref-command
    ((_ var val)
     &optional
     (value-holder (gensym "array-ref-")) (index-holder (gensym "index-")))
  "Parse the `array-ref' command by editing the `array' command's instructions.

VAR is a variable name.  VAL is an array value.  VALUE-HOLDER
holds the array value.  INDEX-HOLDER holds the index value."
  `(,@(loopy--create-destructured-assignment
       var `(aref ,value-holder ,index-holder) 'generalized)
    (loopy--implicit-vars  . (,value-holder ,val))
    (loopy--implicit-vars  . (,index-holder 0))
    (loopy--latter-body    . (setq ,index-holder (1+ ,index-holder)))
    (loopy--pre-conditions . (< ,index-holder (length ,value-holder)))))

(cl-defun loopy--parse-cons-command ((_ var val &optional (func #'cdr)))
  "Parse the `cons' loop command.

VAR is a variable name.  VAL is a cons cell value.  Optional FUNC
is a function by which to update VAR (default `cdr')."
  (if (symbolp var)
      `((loopy--explicit-vars . (,var ,val))
        (loopy--latter-body
         . (setq ,var (,(loopy--get-function-symbol func) ,var)))
        (loopy--pre-conditions . (consp ,var)))
    ;; TODO: For destructuring, do we actually need the extra variable?
    (let ((value-holder (gensym "cons-")))
      `((loopy--implicit-vars . (,value-holder ,val))
        ,@(loopy--create-destructured-assignment var value-holder)
        (loopy--latter-body
         . (setq ,value-holder (,(loopy--get-function-symbol func)
                                ,value-holder)))
        (loopy--pre-conditions . (consp ,value-holder))))))

(cl-defun loopy--parse-list-command
    ((_ var val &optional (func #'cdr)) &optional (val-holder (gensym "list-")))
  "Parse the `list' loop command.

VAR is a variable name or a list of such names (dotted pair or
normal).  VAL is a list value.  FUNC is a function used to update
VAL (default `cdr').  VAL-HOLDER is a variable name that holds
the list."
  `((loopy--implicit-vars . (,val-holder ,val))
    (loopy--latter-body
     . (setq ,val-holder (,(loopy--get-function-symbol func) ,val-holder)))
    (loopy--pre-conditions . (consp ,val-holder))
    ,@(loopy--create-destructured-assignment var `(car ,val-holder))))

(cl-defun loopy--parse-list-ref-command
    ((_ var val &optional (func #'cdr)) &optional (val-holder (gensym "list-ref-")))
  "Parse the `list-ref' loop command, editing the `list' commands instructions.

VAR is the name of a setf-able place.  VAL is a list value.  FUNC
is a function used to update VAL (default `cdr').  VAL-HOLDER is
a variable name that holds the list."
  `((loopy--implicit-vars . (,val-holder ,val))
    ,@(loopy--create-destructured-assignment var `(car ,val-holder) 'generalized)
    (loopy--latter-body . (setq ,val-holder (,(loopy--get-function-symbol func)
                                             ,val-holder)))
    (loopy--pre-conditions . (consp ,val-holder))))

(cl-defun loopy--parse-repeat-command ((_ var-or-count &optional count))
  "Parse the `repeat' loop command.

The command can be of the form (repeat VAR  COUNT) or (repeat COUNT).

VAR-OR-COUNT is a variable name or an integer.  Optional COUNT is
an integer, to be used if a variable name is provided."
  (if count
      `((loopy--implicit-vars . (,var-or-count 0))
        (loopy--latter-body . (setq ,var-or-count (1+ ,var-or-count)))
        (loopy--pre-conditions . (< ,var-or-count ,count)))
    (let ((value-holder (gensym "repeat-limit-")))
      `((loopy--implicit-vars . (,value-holder 0))
        (loopy--latter-body . (setq ,value-holder (1+ ,value-holder)))
        (loopy--pre-conditions . (< ,value-holder ,var-or-count))))))

(cl-defun loopy--parse-seq-command
    ((_ var val)
     &optional (value-holder (gensym "seq-")) (index-holder (gensym "index-")))
  "Parse the `seq' loop command.

VAR is a variable name.  VAL is a sequence value.  VALUE-HOLDER
holds VAL.  INDEX-HOLDER holds an index that point into VALUE-HOLDER."
  ;; NOTE: `cl-loop' just combines the logic for lists and arrays, and
  ;;       just checks the type for each iteration, so we do that too.
  `((loopy--implicit-vars . (,value-holder ,val))
    (loopy--implicit-vars . (,index-holder 0))
    ,@(loopy--create-destructured-assignment
       var `(if (consp ,value-holder)
                (pop ,value-holder)
              (aref ,value-holder ,index-holder)))
    (loopy--latter-body   . (setq ,index-holder (1+ ,index-holder)))
    (loopy--pre-conditions
     . (and ,value-holder (or (consp ,value-holder)
                              (< ,index-holder (length ,value-holder)))))))

(cl-defun loopy--parse-seq-ref-command
    ((_ var val)
     &optional
     (value-holder (gensym "seq-ref-")) (index-holder (gensym "index-"))
     (length-holder (gensym "seq-ref-length-")))
  "Parse the `seq-ref' loop command.

VAR is a variable name.  VAL is a sequence value.  VALUE-HOLDER
holds VAL.  INDEX-HOLDER holds an index that point into
VALUE-HOLDER.  LENGTH-HOLDER holds than length of the value of
VALUE-HOLDER, once VALUE-HOLDER is initialized."
  `((loopy--implicit-vars . (,value-holder ,val))
    (loopy--implicit-vars . (,length-holder (length ,value-holder)))
    (loopy--implicit-vars . (,index-holder 0))
    ,@(loopy--create-destructured-assignment
       var `(elt ,value-holder ,index-holder) 'generalized)
    (loopy--latter-body   . (setq ,index-holder (1+ ,index-holder)))
    (loopy--pre-conditions . (< ,index-holder ,length-holder))))

;; TODO: Some of the accumulations commands can be made more
;;       efficient/complicated depending on how the variables are being used.
;;       See `cl--parse-loop-clause' for examples.
(cl-defun loopy--parse-accumulation-commands ((name var-or-val &optional val))
  "Parse the accumulation loop commands, like `collect', `append', etc.

NAME is the name of the command.  VAR is a variable name.  VAL is a value."
  (if val
      (cl-etypecase var-or-val
        (symbol
         `((loopy--explicit-vars
            ;;  Not all commands can have the variable initialized to nil.
            . (,var-or-val ,(cl-case name
                              ((sum count)    0)
                              ((max maximize) -1.0e+INF)
                              ((min minimize) +1.0e+INF))))
           (loopy--main-body
            . ,(cl-ecase name
                 (append
                  `(setq ,var-or-val (append ,var-or-val ,val)))
                 (collect
                  `(setq ,var-or-val (append ,var-or-val (list ,val))))
                 (concat
                  `(setq ,var-or-val (concat ,var-or-val ,val)))
                 (vconcat
                  `(setq ,var-or-val (vconcat ,var-or-val ,val)))
                 (count
                  `(if ,val (setq ,var-or-val (1+ ,var-or-val))))
                 ((max maximize)
                  `(setq ,var-or-val (max ,var-or-val ,val)))
                 ((min minimize)
                  `(setq ,var-or-val (min ,var-or-val ,val)))
                 (nconc
                  `(setq ,var-or-val (nconc ,var-or-val ,val)))
                 ((push-into push)
                  `(push ,val ,var-or-val))
                 (sum
                  `(setq ,var-or-val (+ ,var-or-val ,val)))))
           (loopy--implicit-return . ,var-or-val)))
        (list
         (let ((value-holder (gensym (concat (symbol-name name) "-destructuring-list-")))
               (is-proper-list (proper-list-p var-or-val))
               (normalized-reverse-var))
           (let ((instructions `(((loopy--implicit-vars . (,value-holder nil))
                                  (loopy--main-body . (setq ,value-holder ,val))))))
             ;; If `var-or-val' is a list, always create a "normalized" variable
             ;; list, since proper lists are easier to work with, as many
             ;; looping/mapping functions expect them.
             (while (car-safe var-or-val)
               (push (pop var-or-val) normalized-reverse-var))
             ;; If the last element in `var-or-val' was a dotted pair, then
             ;; `var-or-val' is now a single symbol, which must still be added
             ;; to the normalized `var-or-val' list.
             (when var-or-val (push var-or-val normalized-reverse-var))

             (dolist (symbol-or-seq (reverse (cl-rest normalized-reverse-var)))
               (push (loopy--parse-accumulation-commands
                      (list name symbol-or-seq `(pop ,value-holder)))
                     instructions))

             ;; Decide what to do for final assignment.
             (push (loopy--parse-accumulation-commands
                    (list name (cl-first normalized-reverse-var)
                          (if is-proper-list
                              `(pop ,value-holder)
                            value-holder)))
                   instructions)

             (apply #'append (nreverse instructions)))))

        (array
         (let* ((value-holder (gensym (concat (symbol-name name) "-destructuring-array-")))
                (instructions
                 `(((loopy--implicit-vars . (,value-holder nil))
                    (loopy--main-body . (setq ,value-holder ,val))))))
           (cl-loop for symbol-or-seq across var-or-val
                    for index from 0
                    do (push (loopy--parse-accumulation-commands
                              (list
                               name symbol-or-seq `(aref ,value-holder ,index)))
                             instructions))
           (apply #'append (nreverse instructions)))))

    ;; If not `val' given, then `var-or-val' is the value expression.  There is
    ;; no destructuring in this case.
    (let ((value-holder (gensym (concat (symbol-name name) "-implicit-"))))
      `((loopy--implicit-vars . (,value-holder ,(cl-case name
                                                  ((sum count)    0)
                                                  ((max maximize) -1.0e+INF)
                                                  ((min minimize) +1.0e+INF))))
        ,@(cl-ecase name
            ;; NOTE: Some commands have different behavior when a
            ;;       variable is not specified.
            ;;       - `collect' uses the `push'-`nreverse' idiom.
            ;;       - `append' uses the `reverse'-`nconc'-`nreverse' idiom.
            ;;       - `nconc' uses the `nreverse'-`nconc'-`nreverse' idiom.
            (append
             `((loopy--main-body
                . (setq ,value-holder (nconc (reverse ,var-or-val)
                                             ,value-holder)))
               (loopy--implicit-return . (nreverse ,value-holder))))
            (collect
             `((loopy--main-body
                . (setq ,value-holder (cons ,var-or-val ,value-holder)))
               (loopy--implicit-return . (nreverse ,value-holder))))
            (concat
             `((loopy--main-body
                . (setq ,value-holder (concat ,value-holder ,var-or-val)))
               (loopy--implicit-return . ,value-holder)))
            (vconcat
             `((loopy--main-body
                . (setq ,value-holder (vconcat ,value-holder ,var-or-val)))
               (loopy--implicit-return . ,value-holder)))
            (count
             `((loopy--main-body
                . (if ,var-or-val (setq ,value-holder (1+ ,value-holder))))
               (loopy--implicit-return . ,value-holder)))
            ((max maximize)
             `((loopy--main-body
                . (setq ,value-holder (max ,value-holder ,var-or-val)))
               (loopy--implicit-return . ,value-holder)))
            ((min minimize)
             `((loopy--main-body
                . (setq ,value-holder (min ,value-holder ,var-or-val)))
               (loopy--implicit-return . ,value-holder)))
            (nconc
             `((loopy--main-body
                . (setq ,value-holder (nconc (nreverse ,var-or-val) ,value-holder)))
               (loopy--implicit-return . (nreverse ,value-holder))))
            ((push-into push)
             `((loopy--main-body . (push ,var-or-val ,value-holder))
               (loopy--implicit-return . ,value-holder)))
            (sum
             `((loopy--main-body
                . (setq ,value-holder (+ ,value-holder ,var-or-val)))
               (loopy--implicit-return . ,value-holder))))))))

(cl-defun loopy--parse-early-exit-commands ((&whole command name &rest args))
  "Parse the  `return' and `return-from' loop commands.

COMMAND is the whole command.  NAME is the command name.  ARGS is
a loop name, return values, or a list of both."
  ;; Check arguments.  Really, the whole reason to have these commands is to not
  ;; mess the arguments to `cl-return-from' or `cl-return', and to provide a
  ;; clearer meaning.
  (let ((arg-length (length args)))
    (cl-case name
      (return
       `((loopy--main-body
          . (cl-return-from nil ,(cond
                                  ((zerop arg-length) nil)
                                  ((= 1 arg-length)  (car args))
                                  (t                 `(list ,@args)))))))
      (return-from
       (let ((arg-length (length args)))
         (when (zerop arg-length) ; Need at least 1 arg.
           (signal 'loopy-wrong-number-of-arguments command))
         `((loopy--main-body
            . (cl-return-from ,(cl-first args)
                ,(cond
                  ((= 1 arg-length) nil)
                  ((= 2 arg-length) (cl-second args))
                  (t                `(list ,@(cl-rest args))))))))))))

(cl-defun loopy--parse-leave-command (_)
  "Parse the `leave' command."
  '((loopy--tagbody-exit-used . t)
    (loopy--main-body . (go loopy--non-returning-exit-tag))))

(cl-defun loopy--parse-skip-command (_)
  "Parse the `skip' loop command."
  '((loopy--skip-used . t)
    (loopy--main-body . (go loopy--continue-tag))))

(cl-defun loopy--parse-while-until-commands ((name condition &rest conditions))
  "Parse the `while' and `until' commands.

NAME is `while' or `until'.  CONDITION is a required condition.
CONDITIONS is the remaining optional conditions."
  `((loopy--tagbody-exit-used . t)
    (loopy--main-body
     . ,(cl-ecase name
          (until `(if ,(if (zerop (length conditions))
                           condition
                         `(and ,condition ,@conditions))
                      (go loopy--non-returning-exit-tag)))
          (while `(if ,(if (zerop (length conditions))
                           condition
                         `(or ,condition ,@conditions))
                      nil (go loopy--non-returning-exit-tag)))))))

(defun loopy--parse-loop-command (command)
  "Parse COMMAND, returning a list of instructions in the same received order.

This function gets the parser, and passes the command to that parser."
  (let ((parser (loopy--get-command-parser command)))
    (if-let ((instructions (funcall parser command)))
        instructions
      (error "Loopy: No instructions returned by command parser: %s"
             parser))))

;; TODO: Allow for commands to return single instructions, instead of requiring
;; list of instructions.
(defun loopy--parse-loop-commands (command-list)
  "Parse commands in COMMAND-LIST via `loopy--parse-loop-command'.
Return a single list of instructions in the same order as
COMMAND-LIST."
  (mapcan #'loopy--parse-loop-command command-list))

;; TODO: Is there a cleaner way than this?  Symbol properties?
(defconst loopy--builtin-command-parsers
  ;; A few of these are just aliases.
  '((append      . loopy--parse-accumulation-commands)
    (array       . loopy--parse-array-command)
    (array-ref   . loopy--parse-array-ref-command)
    (arrayf      . loopy--parse-array-ref-command)
    (collect     . loopy--parse-accumulation-commands)
    (concat      . loopy--parse-accumulation-commands)
    (cond        . loopy--parse-cond-command)
    (cons        . loopy--parse-cons-command)
    (conses      . loopy--parse-cons-command)
    (continue    . loopy--parse-skip-command)
    (count       . loopy--parse-accumulation-commands)
    (do          . loopy--parse-do-command)
    (expr        . loopy--parse-expr-command)
    (leave       . loopy--parse-leave-command)
    (group       . loopy--parse-group-command)
    (if          . loopy--parse-if-command)
    (list        . loopy--parse-list-command)
    (list-ref    . loopy--parse-list-ref-command)
    (max         . loopy--parse-accumulation-commands)
    (maximize    . loopy--parse-accumulation-commands)
    (min         . loopy--parse-accumulation-commands)
    (minimize    . loopy--parse-accumulation-commands)
    (nconc       . loopy--parse-accumulation-commands)
    (push        . loopy--parse-accumulation-commands)
    (push-into   . loopy--parse-accumulation-commands)
    (repeat      . loopy--parse-repeat-command)
    (return      . loopy--parse-early-exit-commands)
    (return-from . loopy--parse-early-exit-commands)
    (seq         . loopy--parse-seq-command)
    (seq-ref     . loopy--parse-seq-ref-command)
    (seqf        . loopy--parse-seq-ref-command)
    (skip        . loopy--parse-skip-command)
    (sum         . loopy--parse-accumulation-commands)
    (unless      . loopy--parse-when-unless-command)
    (until       . loopy--parse-while-until-commands)
    (while       . loopy--parse-while-until-commands)
    (vconcat     . loopy--parse-accumulation-commands)
    (when        . loopy--parse-when-unless-command))
  "An alist of pairs of command names and built-in parser functions.")

(defun loopy--get-command-parser (command)
  "Get the parsing function for COMMAND, based on the command name.

First check in `loopy--builtin-command-parsers', then
`loopy-custom-command-parsers'."

  (or (alist-get (car command) loopy--builtin-command-parsers)
      (alist-get (car command) loopy-custom-command-parsers)
      (signal 'loopy-unknown-command command)))

;;;; The Macro Itself
;;;###autoload
(cl-defmacro loopy (&rest body)
  "A looping macro.

There are several possible arguments that make up BODY:
- a name for the loop, unquoted
- variables to declare before the loop, as in
  (with (VAR1 VAL1) [(VAR2 VAL2) ...])
- code to run before the loop, as in (before-do FORM1 [FORM2 ...])
- special commands that define the loop, as in (loop COMMAND1 [COMMAND2 ...])
- code to run if the loop completes, as in (after-do FORM1 [FORM2 ...])
- code to always run after the loop, as in (finally-do FORM1 [FORM2 ...])
- a value to always return, as in (finally-return FORM1 [FORM2 ...])

Returns are always explicit.  See this package's README for more information."
  (declare (debug (&optional ;; TODO: Is this correct?
                   ([&or "with" "let*"] &rest (symbolp &optional form))
                   ([&or "before-do" "before-progn" "before"] body)
                   ([&optional "loop"]
                    &rest [&or (symbolp ;; This one covers most commands.
                                &optional
                                symbolp
                                form
                                [&or symbolp function-form lambda-expr])
                               ([&or "when" "if" "unless"] form body)
                               ("cond" &rest (body))])
                   ([&or "after-do" "after-progn" "after"] body)
                   ([&or "finally-do" "finally-progn"] body)
                   ([&or "finally-return" "return"] form &optional [&rest form]))))
  (let (;; -- Top-level expressions other than loop body --
        (loopy--loop-name)
        (loopy--with-vars)
        (loopy--before-do)
        (loopy--after-do)
        (loopy--final-do)
        (loopy--final-return)

        ;; -- Vars for processing loop commands --
        (loopy--implicit-vars)
        (loopy--explicit-vars)
        (loopy--explicit-generalized-vars)
        (loopy--pre-conditions)
        (loopy--main-body)
        (loopy--latter-body)
        (loopy--post-conditions)
        (loopy--implicit-return)

        ;; -- Variables for constructing code --
        (loopy--skip-used)
        (loopy--tagbody-exit-used))

;;;;; Interpreting the macro arguments.
    ;; Check what was passed to the macro.
    (dolist (arg body)
      (cond
       ((symbolp arg)
        (setq loopy--loop-name arg))
       ((memq (car-safe arg) '(with let*))            ; This undone by another
        (setq loopy--with-vars (nreverse (cdr arg)))) ; `nreverse' later.
       ((memq (car-safe arg) '(before-do before))
        (setq loopy--before-do (cdr arg)))
       ((memq (car-safe arg) '(after-do after else else-do))
        (setq loopy--after-do (cdr arg)))
       ((memq (car-safe arg) '(finally-do finally))
        (setq loopy--final-do (cdr arg)))
       ((memq (car-safe arg) '(finally-return return))
        (setq loopy--final-return
              (if (= 1 (length (cdr arg)))
                  (cadr arg)
                (cons 'list (cdr arg)))))
       (t
        ;; Body forms have the most variety.
        ;; An instruction is (PLACE-TO-ADD . THING-TO-ADD).
        ;; Things added are expanded in place.
        (dolist (instruction (loopy--parse-loop-commands
                              (if (eq (car-safe arg) 'loop)
                                  (cdr arg)
                                arg)))
          ;; Do it this way instead of with `set', cause was getting errors
          ;; about void variables.
          (cl-case (car instruction)
            (loopy--explicit-generalized-vars
             (push (cdr instruction) loopy--explicit-generalized-vars))
            (loopy--implicit-vars
             ;; Don't wont to accidentally rebind variables to `nil'.
             (unless (loopy--bound-p (cadr instruction))
               (push (cdr instruction) loopy--implicit-vars)))
            (loopy--explicit-vars
             (unless (loopy--bound-p (cadr instruction))
               (push (cdr instruction) loopy--explicit-vars)))
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

            ;; Code for conditionally constructing the loop body.
            (loopy--skip-used
             (setq loopy--skip-used t))
            (loopy--tagbody-exit-used
             (setq loopy--tagbody-exit-used t))

            ;; Places users probably shouldn't push to, but can if they want:
            (loopy--with-vars
             (push (cdr instruction) loopy--with-vars))
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
          loopy--with-vars (nreverse loopy--with-vars))

;;;;; Constructing/Creating the returned code.

    ;; Construct the expanded code from the inside out.  The result should work
    ;; something like the below code.  Unlike below, constructs are only used
    ;; when needed.
    ;;
    ;; `(cl-symbol-macrolet ,loopy--explicit-generalized-vars
    ;;    (let* ,loopy--with-vars
    ;;      (let ,(append loopy--implicit-vars loopy--explicit-vars)
    ;;        ;; If we need to, capture early return, those that has less
    ;;        ;; priority than a final return.
    ;;        (let ((loopy--early-return-capture
    ;;               (cl-block ,loopy--loop-name
    ;;                 ,@loopy--before-do
    ;;                 (while ,(cl-case (length loopy--pre-conditions)
    ;;                           (0 t)
    ;;                           (1 (car loopy--pre-conditions))
    ;;                           (t (cons 'and loopy--pre-conditions)))
    ;;                   (cl-tagbody
    ;;                    ,@loopy--main-body
    ;;                    loopy--continue-tag
    ;;                    ,@loopy--latter-body))
    ;;                 ,@loopy--after-do
    ;;                 nil)))
    ;;          ,@loopy--final-do
    ;;          ,(if loopy--final-return
    ;;               loopy--final-return
    ;;             'loopy--early-return-capture)))))

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
                            (cl-return-from ,loopy--loop-name
                              ,(cond
                                ((null loopy--implicit-return) nil)
                                ((= 1 (length loopy--implicit-return))
                                 (car loopy--implicit-return))
                                (t
                                 `(list ,@(nreverse loopy--implicit-return))))))))))

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

        ;; Now ensure return value is nil and add the code to run before and
        ;; after the `while' loop.
        (cond
         ((and loopy--before-do loopy--after-do)
          (setq result `(,@loopy--before-do ,result ,@loopy--after-do)
                result-is-one-expression nil))
         (loopy--before-do
          (setq result `(,@loopy--before-do ,result)
                result-is-one-expression nil))
         (loopy--after-do
          (setq result `(,result ,@loopy--after-do)
                result-is-one-expression nil)))

        (when loopy--tagbody-exit-used
          (setq result `(cl-tagbody
                         ,@(get-result)
                         loopy--non-returning-exit-tag)
                result-is-one-expression t))

        ;; Always wrap in `cl-block', as any arbitrary Lisp code could call
        ;; `cl-return-from'.  For example, it's possible that a user is using a
        ;; loop to change variables, and they might wish to stop changing things
        ;; at a certain point.
        (setq result `(cl-block ,loopy--loop-name
                        ,@(get-result)
                        ;; Be sure that the `cl-block' defaults to returning
                        ;; nil.  This can be overridden by any call to
                        ;; `cl-return-from'.
                        ,(cond
                          ((null loopy--implicit-return) nil)
                          ((= 1 (length loopy--implicit-return))
                           (car loopy--implicit-return))
                          (t
                           `(list ,@(nreverse loopy--implicit-return)))))
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

        ;; Declare the implicit and explicit variables.
        ;; Implicit variables must be in a `let*' in case one refers to another,
        ;; like in `seq-ref'.
        (when loopy--implicit-vars
          (setq result `(let* ,(nreverse loopy--implicit-vars)
                          ,@(get-result))
                result-is-one-expression t))

        (when loopy--explicit-vars
          (setq result `(let ,loopy--explicit-vars
                          ,@(get-result))
                result-is-one-expression t))

        ;; Declare the With variables.
        (when loopy--with-vars
          (setq result `(let* ,loopy--with-vars ,@(get-result))
                result-is-one-expression t))

        ;; Declare the symbol macros.
        (when loopy--explicit-generalized-vars
          (setq result `(cl-symbol-macrolet ,loopy--explicit-generalized-vars
                          ,@(get-result))
                ;; TODO: Not using this, but maybe later?
                ;; result-is-one-expression t
                ))

        ;; Final check: If `result' is not one expression, then wrap `result' in
        ;; a `progn'.  Otherwise, the return value of the first expression would
        ;; be used as a function.
        (unless result-is-one-expression
          (push 'progn result))

        ;; Return the constructed code.
        result))))

(provide 'loopy)
;;; loopy.el ends here
