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
;; These are only ever set locally.

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

;;;;; Variables for constructing the code

;; These variable affect how the code is expanded.
(defvar loopy--skip-used nil
  "Whether a skip/continue command is present in the loop  main body.")

;;;; Errors
(define-error 'loopy-form
  "Loopy: Bad form"
  'loopy-error)

(define-error 'loopy-command-arguments
  "Loopy: Bad command arguments"
  'loopy-error)

;;;; Miscellaneous Functions
(defun loopy--bound-p (var-name)
  "Check if VAR-NAME (a symbol) is already bound for the macro.

This can happen when multiple loop commands refer to the same
variable, or when a variable is introduced via `with'.

The variable can exist in `loopy--with-vars', `loopy--explicit-vars',
or `loopy--explicit-generalized-vars'."
  (or (memq var-name (mapcar #'car loopy--with-vars))
      (memq var-name (mapcar #'car loopy--explicit-vars))
      (memq var-name (mapcar #'car loopy--explicit-generalized-vars))))

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

(cl-defun loopy--parse-if-command ((name
                                    condition
                                    &optional if-true
                                    &rest if-false))
  "Parse the `if' loop command.  This takes the entire command.

- NAME is the command name (so `if').
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

(cl-defun loopy--parse-cond-command ((name &rest clauses))
  "Parse the `cond' command.  This works like the `cond' special form.

NAME is the name of the command.  CLAUSES are lists of a Lisp
expression followed by one or more loop commands.

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

(cl-defun loopy--parse-early-exit-commands ((name &rest args))
  "Parse `return', `return-from', `leave', and `leave-from' commands."
  ;; Check arguments.
  (cl-case name
    ((return leave-from)
     (unless (= (length args) 1)
       (signal 'wrong-number-of-arguments (cons name args))))
    (leave
     (unless (= (length args) 0)
       (signal 'wrong-number-of-arguments (cons name args))))
    (return-from
     (unless (= (length args) 2)
       (signal 'wrong-number-of-arguments (cons name args)))))
  ;; Parse
  (cl-case name
    (return
     `(loopy--main-body . (cl-return-from nil ,(cl-first args))))
    (return-from
     `(loopy--main-body
       . (cl-return-from ,(cl-first args) ,(cl-second args))))
    ((leave break)
     `(loopy--main-body . (cl-return-from nil nil)))
    ((leave-from break-from)
     `(loopy--main-body . (cl-return-from ,(cl-first args) nil)))))

(cl-defun loopy--parse-expr-command ((name var &rest vals))
  "Parse the `expr' command.

- NAME is the name of the command (so `expr').
- VAR is the variable to assign.
- VALS are the values to assign to VAR."
  (let ((arg-length (length vals))
        (value-selector (gensym))
        instructions)
    ;; Declare `var'.
    (push `(loopy--explicit-vars . (,var nil)) instructions)
    (cl-case arg-length
      ;; If no values, repeatedly set to `nil'.
      (0 (push `(loopy--main-body . (setq ,var nil)) instructions))
      ;; If one value, repeatedly set to that value.
      (1 (push `(loopy--main-body . (setq ,var ,(car vals))) instructions))
      ;; If two values, repeatedly check against `value-selector' to determine
      ;; if we should assign the first or second value.  This is how `cl-loop'
      ;; does it.
      (2
       (push `(loopy--implicit-vars . (,value-selector t)) instructions)
       (push `(loopy--main-body
               . (setq ,var (if ,value-selector
                                ,(cl-first vals)
                              ,(cl-second vals))))
             instructions)
       (push `(loopy--latter-body . (setq ,value-selector nil)) instructions))
      (t
       (push `(loopy--implicit-vars . (,value-selector 0)) instructions)
       (push `(loopy--latter-body
               . (when (< ,value-selector (1- ,arg-length))
                   (setq ,value-selector (1+ ,value-selector))))
             instructions)
       ;; Assign to var based on the value of value-selector.  For
       ;; efficiency, we want to check for the last expression first,
       ;; since it will probably be true the most times.  To enable
       ;; that, the condition is whether the counter is greater than
       ;; the index of EXPR in REST minus one.
       ;;
       ;; E.g., for '(a b c),
       ;; use '(cond ((> cnt 1) c) ((> cnt 0) b) ((> cnt -1) a))
       (push `(loopy--main-body
               . (setq ,var ,(let ((body-code nil) (index 0))
                               (dolist (value vals)
                                 (push `((> ,value-selector ,(1- index))
                                         ,value)
                                       body-code)
                                 (setq index (1+ index)))
                               (cons 'cond body-code))))
             instructions)))))

(cl-defun loopy--parse-array-command
    ((name var val) &optional (value-holder (gensym)) (index-holder (gensym)))
  "Parse the `array' command.

- NAME is the name of the command.
- VAR is a variable name.
- VAL is an array value.
- Optional VALUE-HOLDER holds the array value.
- Optional INDEX-HOLDER holds the index value."
  `((loopy--implicit-vars  . (,value-holder ,val))
    (loopy--implicit-vars  . (,index-holder 0))
    (loopy--explicit-vars  . (,var nil))
    (loopy--main-body      . (setq ,var (aref ,value-holder ,index-holder)))
    (loopy--latter-body    . (setq ,index-holder (1+ ,index-holder)))
    (loopy--pre-conditions . (< ,index-holder (length ,value-holder)))))

(cl-defun loopy--parse-array-ref-command
    ((name var val) &optional (value-holder (gensym)) (index-holder (gensym)))
  "Parse the `array-ref' command by editing the `array' command's instructions.

NAME is the name of the command.  VAR is a variable name.  VAL is
an array value.  VALUE-HOLDER holds the array value.
INDEX-HOLDER holds the index value."
  `((loopy--explicit-generalized-vars
     . (,var (aref ,value-holder ,index-holder)))
    (loopy--implicit-vars  . (,value-holder ,val))
    (loopy--implicit-vars  . (,index-holder 0))
    (loopy--explicit-vars  . (,var nil))
    (loopy--latter-body    . (setq ,index-holder (1+ ,index-holder)))
    (loopy--pre-conditions . (< ,index-holder (length ,value-holder)))))

(cl-defun loopy--parse-cons-command ((name var val &optional (func #'cdr)))
  "Parse the `cons' loop command.

NAME is the name of the command.  VAR is a variable name.  VAL
is a cons cell value.  Optional FUNC is a function by which to update
VAR (default `cdr')."
  `((loopy--explicit-vars . (,var ,val))
    (loopy--latter-body . (setq ,var (,(loopy--get-function-symbol func) ,var)))
    (loopy--pre-conditions . (consp ,var))))

(cl-defun loopy--parse-list-command
    ((name var val &optional (func #'cdr)) &optional (val-holder (gensym)))
  "Parse the `list' loop command.

NAME is the command name.  VAR is a variable name.  VAL is a list
value.  FUNC is a function used to update VAL (default `cdr').
VAL-HOLDER is a variable name that holds the list."
  `((loopy--implicit-vars . (,val-holder ,val))
    (loopy--explicit-vars . (,var nil))
    (loopy--main-body
     . (setq ,var (car ,val-holder)))
    (loopy--latter-body
     . (setq ,val-holder (,(loopy--get-function-symbol func) ,val-holder)))
    (loopy--pre-conditions . (consp ,val-holder))))

(cl-defun loopy--parse-list-ref-command
    ((name var val &optional (func #'cdr)) &optional (val-holder (gensym)))
  "Parse the `list-ref' loop command, editing the `list' commands instructions.

NAME is the command name.  VAR is the name of a setf-able place.
VAL is a list value.  FUNC is a function used to update
VAL (default `cdr').  VAL-HOLDER is a variable name that holds
the list."
  `((loopy--implicit-vars . (,val-holder ,val))
    (loopy--explicit-generalized-vars . (,var (car ,val-holder)))
    (loopy--latter-body . (setq ,val-holder (,(loopy--get-function-symbol func)
                                             ,val-holder)))
    (loopy--pre-conditions . (consp ,val-holder))))

;; TODO: Break this up into smaller functions.
(defun loopy--parse-loop-command (command &optional loop-name)
  "Parse COMMAND, returning a list of instructions in the same received order.

Some commands use specific parsing functions, which are called by
`loopy--parse-loop-command' (this function) as needed.

Optionally, take LOOP-NAME for early exiting."
  (let (instructions)
    (cl-flet ((push-instruction (instr) (push instr instructions)))
      (pcase command
;;;;; Generic body clauses
        ;; A DO form for a generic lisp body. Not searched for special forms.
        ((or `(do . ,body) `(progn . ,body))
         (if (= 1 (length body))
             (push-instruction `(loopy--main-body . ,(car body)))
           (push-instruction `(loopy--main-body . (progn ,@body)))))
        ((or `(expr ,var . ,rest) `(exprs ,var . ,rest)
             `(set ,var . ,rest))
         (mapc #'push-instruction (loopy--parse-expr-command command)))

;;;;; Iteration Clauses
        ;; TODO:
        ;; - obarrays?
        ;; - key-codes/key-bindings and key-seqs?
        ;; - overlays?
        ;; - intervals of constant text properties?
        (`(array . ,rest)
         (mapc #'push-instruction (loopy--parse-array-command command)))

        ((or `(arrayf ,var ,val) `(array-ref ,var ,val))
         (mapc #'push-instruction (loopy--parse-array-ref-command command)))

        ((or `(cons . ,rest) `(conses . ,rest))
         (mapc #'push-instruction
               (loopy--parse-cons-command command)))

        (`(list . ,rest)
         (mapc #'push-instruction (loopy--parse-list-command command)))

        ((or `(list-ref . ,rest) `(listf . ,rest))
         (mapc #'push-instruction (loopy--parse-list-ref-command command)))

        (`(repeat ,count)
         (let ((val-holder (gensym)))
           (push-instruction `(loopy--implicit-vars . (,val-holder 0)))
           (push-instruction `(loopy--latter-body . (cl-incf ,val-holder)))
           (push-instruction `(loopy--pre-conditions . (< ,val-holder ,count)))))

        (`(repeat ,var ,count)
         (push-instruction `(loopy--implicit-vars . (,var 0)))
         (push-instruction `(loopy--latter-body . (cl-incf ,var)))
         (push-instruction `(loopy--pre-conditions . (< ,var ,count))))

        (`(seq ,var ,val)
         ;; Note: `cl-loop' just combines the logic for lists and arrays, and
         ;;       just checks the type for each iteration, so we do that too.
         (let ((val-holder (gensym))
               (index-holder (gensym)))
           (push-instruction `(loopy--implicit-vars . (,val-holder ,val)))
           (push-instruction `(loopy--implicit-vars . (,index-holder 0)))
           (push-instruction `(loopy--explicit-vars . (,var nil)))
           (push-instruction
            `(loopy--main-body . (setq ,var
                                       (if (consp ,val-holder)
                                           (pop ,val-holder)
                                         (aref ,val-holder ,index-holder)))))
           (push-instruction
            `(loopy--latter-body . (setq ,index-holder (1+ ,index-holder))))
           (push-instruction `(loopy--pre-conditions
                               . (and ,val-holder
                                      (or (consp ,val-holder)
                                          (< ,index-holder
                                             (length ,val-holder))))))))

        ((or `(seq-ref ,var ,val) `(seqf ,var ,val))
         (let ((val-holder (gensym))
               (index-holder (gensym)))
           (push-instruction `(loopy--implicit-vars . (,val-holder ,val)))
           (push-instruction `(loopy--implicit-vars . (,index-holder 0)))
           (push-instruction `(loopy--explicit-generalized-vars
                               . (,var (elt ,val-holder ,index-holder))))
           (push-instruction `(loopy--latter-body
                               . (setq ,index-holder (1+ ,index-holder))))
           ;; TODO: Length of sequence not changing, so don't have to
           ;;       recompute each time.
           (push-instruction `(loopy--pre-conditions
                               . (< ,index-holder (length ,val-holder))))))

;;;;; Conditional Body Forms
        ;; Since these can contain other commands/clauses, it's easier if they
        ;; have their own parsing functions, which call back into this one to
        ;; parse sub-clauses.
        ((or `(when . ,rest) `(unless . ,rest))
         (mapc #'push-instruction (loopy--parse-when-unless-command command)))

        (`(if . ,rest)
         (mapc #'push-instruction (loopy--parse-if-command command)))

        (`(cond . ,rest)
         (mapc #'push-instruction (loopy--parse-cond-command command)))

;;;;; Exit and Return Clauses
        ((or '(skip) '(continue))
         (push-instruction '(loopy--main-body . (go loopy--continue-tag)))
         (push-instruction '(loopy--skip-used . t)))
        (`(return ,val)
         (push-instruction `(loopy--main-body . (cl-return-from ,loop-name ,val))))
        (`(return-from ,name ,val)
         (push-instruction `(loopy--main-body . (cl-return-from ,name ,val))))
        ((or '(leave) '(break))
         (push-instruction `(loopy--main-body . (cl-return-from ,loop-name nil))))
        ((or `(leave-from ,name) `(break-from ,name))
         (push-instruction `(loopy--main-body . (cl-return-from ,name nil))))

;;;;; Accumulation Clauses
        (`(append ,var ,val)
         (push-instruction `(loopy--explicit-vars . (,var nil)))
         (push-instruction `(loopy--main-body . (setq ,var (append ,var ,val)))))
        (`(collect ,var ,val)
         (push-instruction `(loopy--explicit-vars . (,var nil)))
         (push-instruction `(loopy--main-body . (setq ,var (append ,var
                                                                   (list ,val))))))
        (`(concat ,var ,val)
         (push-instruction `(loopy--explicit-vars . (,var nil)))
         (push-instruction `(loopy--main-body . (setq ,var (concat ,var ,val)))))
        (`(vconcat ,var ,val)
         (push-instruction `(loopy--explicit-vars . (,var nil)))
         (push-instruction `(loopy--main-body . (setq ,var (vconcat ,var ,val)))))
        (`(count ,var ,val)
         (push-instruction `(loopy--explicit-vars . (,var 0)))
         (push-instruction `(loopy--main-body . (when ,val (cl-incf ,var)))))
        ((or `(max ,var ,val) `(maximize ,var ,val))
         (push-instruction `(loopy--explicit-vars . (,var -1.0e+INF)))
         (push-instruction `(loopy--main-body . (setq ,var (max ,var ,val)))))
        ((or `(min ,var ,val) `(minimize ,var ,val))
         (push-instruction `(loopy--explicit-vars . (,var 1.0e+INF)))
         (push-instruction `(loopy--main-body . (setq ,var (min ,var ,val)))))
        (`(nconc ,var ,val)
         (push-instruction `(loopy--explicit-vars . (,var nil)))
         (push-instruction `(loopy--main-body . (setq ,var (nconc ,var ,val)))))
        ((or `(push-into ,var ,val) `(push ,var ,val))
         (push-instruction `(loopy--explicit-vars . (,var nil)))
         (push-instruction `(loopy--main-body . (push ,val ,var))))
        (`(sum ,var ,val)
         (push-instruction `(loopy--explicit-vars . (,var 0)))
         (push-instruction `(loopy--main-body . (setq ,var (+ ,var ,val)))))

;;;;; Custom commands
        (_
         (if-let ((command-parser (loopy--get-custom-command-parser command)))
             (if-let ((custom-instructions (funcall command-parser command)))
                 (mapc #'push-instruction custom-instructions)
               (error "Loopy: No instructions returned by command parser: %s"
                      command-parser))
           (error "Loopy: This command unknown: %s" command)))))
    (nreverse instructions)))

(defun loopy--parse-loop-commands (command-list &optional loop-name)
  "Parse commands in COMMAND-LIST via `loopy--parse-loop-command'.
Return a single list of instructions in the same order as
COMMAND-LIST.  Optionally needs LOOP-NAME for block returns."
  (let (instructions)
    (dolist (command command-list instructions)
      (setq instructions (append instructions
                                 (loopy--parse-loop-command command))))))

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
        (loopy--name-arg)
        (loopy--with-vars)
        (loopy--before-do)
        (loopy--after-do)
        (loopy--final-do)
        (loopy--final-return)

        ;; -- Vars for processing loop clauses --
        (loopy--implicit-vars)
        (loopy--explicit-vars)
        (loopy--explicit-generalized-vars)
        (loopy--pre-conditions)
        (loopy--main-body)
        (loopy--latter-body)
        (loopy--post-conditions)

        ;; -- Variables for constructing code --
        (loopy--skip-used))

;;;;; Interpreting the macro arguments.
    ;; Check what was passed to the macro.
    (dolist (arg body)
      (cond
       ((symbolp arg)
        (setq loopy--name-arg arg))
       ((memq (car-safe arg) '(with let*))
        (setq loopy--with-vars (cdr arg)))
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
                                arg)
                              loopy--name-arg))
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

            ;; Code for conditionally constructing the loop body.
            (loopy--skip-used
             (setq loopy--skip-used t))

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
    ;;               (cl-block ,loopy--name-arg
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
          result-is-one-expression
          ;; Note whether we're using a `cl-block', which we default to
          ;; returning `nil'.  If not present, try to ensure that the macro
          ;; defaults to returning `nil'.
          using-cl-block)

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
                            (cl-return-from ,loopy--name-arg nil))))))

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

        ;; Always wrap in `cl-block', as any arbitrary Lisp code could call
        ;; `cl-return-from'.  For example, it's possible that a user is using a
        ;; loop to change variables, and they might wish to stop changing things
        ;; at a certain point.
        (setq result `(cl-block ,loopy--name-arg
                        ,@(get-result)
                        ;; Be sure that the `cl-block' defaults to returning
                        ;; nil.  This can be overridden by any call to
                        ;; `cl-return-from'.
                        nil)
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
        (when (or loopy--implicit-vars loopy--explicit-vars)
          (setq result `(let ,(append loopy--implicit-vars loopy--explicit-vars)
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
