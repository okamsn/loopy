;;; loopy-commands.el --- Loop commands for loopy -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: February 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.4
;; Package-Requires: ((emacs "27.1") (loopy "0.4"))
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
;; This package provides the "loop commands" used in the `loop' macro argument
;; of the `loopy' macro, as well as functions and variables for working with
;; those commands.
;;
;; For convenience, the functions related to destructuring in accumulation
;; commands are also included in this library, as those are specific to the
;; accumulation commands.
;;
;; For more information, see this package's Info documentation under Info node
;; `(loopy)'.

;;; Code:
;; Cant require `loopy', as that would be recursive.
(declare-function loopy--destructure-variables "loopy")
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(declare-function loopy--bound-p "loopy")
(declare-function loopy--destructure-variables-default "loopy")
(defvar loopy--in-sub-level)

;;;; Variables from flags
(defvar loopy--destructuring-accumulation-parser)
(defvar loopy--split-implied-accumulation-results)
(defvar loopy--basic-destructuring-function)
(defvar loopy--destructuring-for-iteration-function)

;;;; Variables from macro arguments
(defvar loopy--loop-name)

;;;; Custom Commands and Parsing
(defgroup loopy nil
  "A looping macro similar to `cl-loop'."
  :group 'extensions
  :prefix "loopy-"
  :link '(url-link "https://github.com/okamsn/loopy"))

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
  :type '(alist :key-type symbol :value-type function))

(defun loopy--get-custom-command-parser (command)
  "Get the parsing function for COMMAND from `loopy-custom-command-parsers'.
This uses the command name (such as `list' in `(list i my-list)')."
  (alist-get (car command) loopy-custom-command-parsers))

;;;###autoload
(defcustom loopy-custom-command-aliases nil
  "An alist of pairs of a quoted alias and a quoted true name.

For example, to create the alias `add' for the command `sum', one would add

  '(add . sum)

to this list."
  :group 'loopy
  :type '(alist :key-type symbol :value-type symbol))

(defmacro loopy-defalias (alias definition)
  "Add alias ALIAS for loop command DEFINITION.

Neither argument need be quoted."
  `(push (cons ,(if (eq (car-safe alias) 'quote)
                    alias
                  `(quote ,alias))
               ,(if (eq (car-safe definition) 'quote)
                    definition
                  `(quote ,definition)))
         loopy-custom-command-aliases))

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

(defun loopy--signal-bad-iter (command-name)
  "Signal an error for COMMAND-NAME."
  (user-error "Can only use command \"%s\" in top level of `loopy' or sub-loop"
              command-name))

(defun loopy--signal-must-be-top-level (command-name)
  "Signal an error for COMMAND-NAME."
  (user-error "Can't use \"%s\" in `loopy' outside top-level" command-name))

;;;; Helpful Functions
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

(defun loopy--extract-main-body (instructions)
  "Separate main-body instructions from others in INSTRUCTIONS.

This returns a list of two sub-lists:
1. Expression that should be inserted into a main-body instruction.
2. Other instructions.

The lists will be in the order parsed (correct for insertion)."
  (let ((wrapped-main-body)
        (other-instructions))
    (dolist (instruction instructions)
      (if (eq (car instruction) 'loopy--main-body)
          (push (cdr instruction) wrapped-main-body)
        (push instruction other-instructions)))

    ;; Return the sub-lists.
    (list (nreverse wrapped-main-body) (nreverse other-instructions))))


;;;; Included parsing functions.
;;;;; Misc.
(cl-defun loopy--parse-sub-loop-command ((_ &rest body))
  "Parse the `loop' or `sub-loop' command.

A sub-loop does not have its own return value, but can have its
own exit conditions and name.  It does not support special macro
arguments like `after-do' or `finally-do'.

BODY is one or more loop commands."
  ;; TODO: There's a lot of repetition between this and the main macro.
  ;;       Does it make sense to put this repetition in a function instead?
  (let ((loopy--in-sub-level nil)
        (wrapped-main-body)
        (wrapped-latter-body)
        (wrapped-pre-conditions)
        (wrapped-post-conditions)
        (wrapped-iteration-vars)
        (non-wrapped-instructions)
        (wrapped-skip-used)
        (wrapped-tagbody-exit-used)
        (loopy--loop-name (gensym "sub-loop-")))

    ;; Process the instructions.
    (dolist (command body)
      (if (symbolp command)
          (setq loopy--loop-name command)
        (dolist (instruction (loopy--parse-loop-command command))
          (cl-case (car instruction)
            (loopy--pre-conditions
             (push (cdr instruction) wrapped-pre-conditions))
            (loopy--main-body
             (push (cdr instruction) wrapped-main-body))
            (loopy--latter-body
             (push (cdr instruction) wrapped-latter-body))
            (loopy--post-conditions
             (push (cdr instruction) wrapped-post-conditions))
            ;; Code for conditionally constructing the loop body.
            (loopy--skip-used
             (setq wrapped-skip-used t))
            (loopy--tagbody-exit-used
             (setq wrapped-tagbody-exit-used t))
            ;; Vars need to be reset every time the loop is entered.
            (loopy--iteration-vars
             (unless (loopy--bound-p (cadr instruction))
               (push (cdr instruction) wrapped-iteration-vars)))
            (t
             (push instruction non-wrapped-instructions))))))

    ;; Make sure lists are in the correct order.
    (setq wrapped-main-body (nreverse wrapped-main-body)
          wrapped-iteration-vars (nreverse wrapped-iteration-vars))

    ;; Create the sub-loop code.
    ;; See the main macro `loopy' for a more detailed version of this.
    (let ((result nil)
          (result-is-one-expression nil))
      (cl-flet ((get-result () (if result-is-one-expression
                                   (list result)
                                 result)))
        (setq result wrapped-main-body)

        (when wrapped-skip-used
          ;; This must stay `loopy--continue-tag', as
          ;; this name is used by `loopy--parse-skip-command'.
          (setq result `(cl-tagbody ,@result loopy--continue-tag)
                result-is-one-expression t))

        (when wrapped-latter-body
          (setq result (append result wrapped-latter-body)))

        (when wrapped-post-conditions
          (setq result
                (append result
                        `((unless ,(cl-case (length wrapped-post-conditions)
                                     (0 t)
                                     (1 (car wrapped-post-conditions))
                                     (t (cons 'and wrapped-post-conditions)))
                            ;; Unlike the normal loop, sub-loops don't have a
                            ;; return value, so we can just return nil.
                            (cl-return-from ,loopy--loop-name nil))))))
        (setq result `(while ,(cl-case (length wrapped-pre-conditions)
                                (0 t)
                                (1 (car wrapped-pre-conditions))
                                (t (cons 'and wrapped-pre-conditions)))
                        ;; If using a `cl-tag-body', just insert that one
                        ;; expression, but if not, break apart into the while
                        ;; loop's body.
                        ,@(get-result))
              ;; Will always be a single expression after wrapping with `while'.
              result-is-one-expression t)

        (when wrapped-tagbody-exit-used
          (setq result `(cl-tagbody
                         ,@(get-result)
                         ;; This must stay `loopy--non-returning-exit-tag', as
                         ;; this name is used by `loopy--parse-leave-command'.
                         loopy--non-returning-exit-tag)
                result-is-one-expression t))

        (setq result `(cl-block ,loopy--loop-name ,@(get-result) nil)
              ;; Will always be a single expression after wrapping with
              ;; `cl-block'.
              result-is-one-expression t)

        (when wrapped-iteration-vars
          (setq result `(let* ,wrapped-iteration-vars
                          ,@(get-result))
                result-is-one-expression t))

        (unless result-is-one-expression
          (push 'progn result)))

      ;; Return the new instructions.
      (cons `(loopy--main-body . ,result)
            non-wrapped-instructions))))

;;;;; Genereric Evaluation
(cl-defun loopy--parse-expr-command ((_ var &rest vals))
  "Parse the `expr' command.

- VAR is the variable to assign.
- VALS are the values to assign to VAR."
  (let ((arg-length (length vals))
        (value-selector (gensym "expr-value-selector-")))
    (cl-case arg-length
      ;; If no values, repeatedly set to `nil'.
      (0 (loopy--destructure-for-iteration-command
          var nil))
      ;; If one value, repeatedly set to that value.
      (1 (loopy--destructure-for-iteration-command
          var (car vals)))
      ;; If two values, repeatedly check against `value-selector' to
      ;; determine if we should assign the first or second value.  This is
      ;; how `cl-loop' does it.
      (2
       `((loopy--iteration-vars . (,value-selector t))
         ,@(loopy--destructure-for-iteration-command
            var `(if ,value-selector ,(cl-first vals) ,(cl-second vals)))
         ;; This needs to happen right after running the above.
         (loopy--main-body . (setq ,value-selector nil))))
      (t
       `((loopy--iteration-vars . (,value-selector 0))
         ;; Assign to var based on the value of value-selector.  For
         ;; efficiency, we want to check for the last expression first,
         ;; since it will probably be true the most times.  To enable
         ;; that, the condition is whether the counter is greater than
         ;; the index of EXPR in REST minus one.
         ;;
         ;; E.g., for '(a b c),
         ;; use '(cond ((> cnt 1) c) ((> cnt 0) b) ((> cnt -1) a))
         ,@(loopy--destructure-for-iteration-command
            var (let ((body-code nil) (index 0))
                  (dolist (value vals)
                    (push `((> ,value-selector ,(1- index))
                            ,value)
                          body-code)
                    (setq index (1+ index)))
                  (cons 'cond body-code)))
         ;; This needs to happen right after running the above.
         (loopy--main-body
          . (when (< ,value-selector ,(1- arg-length))
              (setq ,value-selector (1+ ,value-selector)))))))))

(cl-defun loopy--parse-group-command ((_ &rest body))
  "Parse the `group' loop command.

BODY is one or more commands to be grouped by a `progn' form."
  (let ((loopy--in-sub-level t)
        (full-instructions) (progn-body))
    (dolist (instruction (loopy--parse-loop-commands body))
      (if (eq (car instruction) 'loopy--main-body)
          (push (cdr instruction) progn-body)
        (push instruction full-instructions)))
    ;; Return the instructions.
    (cons `(loopy--main-body . (progn ,@(nreverse progn-body)))
          (nreverse full-instructions))))

(cl-defun loopy--parse-do-command ((_ &rest expressions))
  "Parse the `do' loop command.

Expressions are normal Lisp expressions, which are inserted into
the loop literally (not even in a `progn')."
  (mapcar (lambda (expr) (cons 'loopy--main-body expr))
          expressions))

;;;;; Conditionals
(cl-defun loopy--parse-if-command
    ((_ condition &optional if-true &rest if-false))
  "Parse the `if' loop command.  This takes the entire command.

- CONDITION is a Lisp expression.
- IF-TRUE is the first sub-command of the `if' command.
- IF-FALSE are all the other sub-commands."
  (let ((loopy--in-sub-level t)
        (full-instructions)
        (if-true-main-body)
        (if-false-main-body))
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

    ;; Return the list of instructions.
    (cons `(loopy--main-body
            . (if ,condition
                  ,if-true-main-body
                ,@(nreverse if-false-main-body)))
          (nreverse full-instructions))))

(cl-defun loopy--parse-cond-command ((_ &rest clauses))
  "Parse the `cond' command.  This works like the `cond' special form.

CLAUSES are lists of a Lisp expression followed by one or more
loop commands.

The Lisp expression and the loopy-body instructions from each
command are inserted into a `cond' special form."
  (let ((loopy--in-sub-level t)
        (full-instructions)
        (actual-cond-clauses))
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
          (nreverse full-instructions))))

(cl-defun loopy--parse-when-unless-command ((name condition &rest body))
  "Parse `when' and `unless' commands.

- NAME is `when' or `unless'.
- CONDITION is the condition.
- BODY is the sub-commands."
  (let ((loopy--in-sub-level t)
        full-instructions
        conditional-body)
    (dolist (instruction (loopy--parse-loop-commands body))
      (if (eq 'loopy--main-body (car instruction))
          (push (cdr instruction) conditional-body)
        (push instruction full-instructions)))
    ;; Return the instructions.
    (cons `(loopy--main-body . (,name ,condition ,@(nreverse conditional-body)))
          (nreverse full-instructions))))

;;;;; Iteration
(cl-defun loopy--parse-array-command ((_ var val))
  "Parse the `array' command.

- VAR is a variable name.
- VAL is an array value.
- Optional VALUE-HOLDER holds the array value.
- Optional INDEX-HOLDER holds the index value."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'array))
  (let ((value-holder (gensym "array-"))
        (index-holder (gensym "array-index-"))
        (length-holder (gensym "array-length-")))
    `((loopy--iteration-vars . (,value-holder ,val))
      (loopy--iteration-vars . (,index-holder 0))
      (loopy--iteration-vars . (,length-holder (length ,value-holder)))
      ,@(loopy--destructure-for-iteration-command var
                                                  `(aref ,value-holder ,index-holder))
      (loopy--latter-body    . (setq ,index-holder (1+ ,index-holder)))
      (loopy--pre-conditions . (< ,index-holder ,length-holder)))))

(cl-defun loopy--parse-array-ref-command ((_ var val))
  "Parse the `array-ref' command by editing the `array' command's instructions.

VAR is a variable name.  VAL is an array value.  VALUE-HOLDER
holds the array value.  INDEX-HOLDER holds the index value."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'array-ref))
  (let ((value-holder (gensym "array-"))
        (index-holder (gensym "array-ref-index-"))
        (length-holder (gensym "array-ref-length-")))
    `(,@(loopy--destructure-for-generalized-command
         var `(aref ,value-holder ,index-holder))
      (loopy--iteration-vars  . (,value-holder ,val))
      (loopy--iteration-vars  . (,index-holder 0))
      (loopy--iteration-vars . (,length-holder (length ,value-holder)))
      (loopy--latter-body    . (setq ,index-holder (1+ ,index-holder)))
      (loopy--pre-conditions . (< ,index-holder ,length-holder)))))

(cl-defun loopy--parse-cons-command ((_ var val &optional (func #'cdr)))
  "Parse the `cons' loop command.

VAR is a variable name.  VAL is a cons cell value.  Optional FUNC
is a function by which to update VAR (default `cdr')."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'cons))
  (if (symbolp var)
      `((loopy--iteration-vars . (,var ,val))
        (loopy--latter-body
         . (setq ,var (,(loopy--get-function-symbol func) ,var)))
        (loopy--pre-conditions . (consp ,var)))
    ;; TODO: For destructuring, do we actually need the extra variable?
    (let ((value-holder (gensym "cons-")))
      `((loopy--iteration-vars . (,value-holder ,val))
        ,@(loopy--destructure-for-iteration-command var value-holder)
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
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'list))
  `((loopy--iteration-vars . (,val-holder ,val))
    (loopy--latter-body
     . (setq ,val-holder (,(loopy--get-function-symbol func) ,val-holder)))
    (loopy--pre-conditions . (consp ,val-holder))
    ,@(loopy--destructure-for-iteration-command var `(car ,val-holder))))

(cl-defun loopy--parse-list-ref-command
    ((_ var val &optional (func #'cdr)) &optional (val-holder (gensym "list-ref-")))
  "Parse the `list-ref' loop command, editing the `list' commands instructions.

VAR is the name of a setf-able place.  VAL is a list value.  FUNC
is a function used to update VAL (default `cdr').  VAL-HOLDER is
a variable name that holds the list."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'list-ref))
  `((loopy--iteration-vars . (,val-holder ,val))
    ,@(loopy--destructure-for-generalized-command var `(car ,val-holder))
    (loopy--latter-body . (setq ,val-holder (,(loopy--get-function-symbol func)
                                             ,val-holder)))
    (loopy--pre-conditions . (consp ,val-holder))))

(cl-defun loopy--parse-repeat-command ((_ var-or-count &optional count))
  "Parse the `repeat' loop command.

The command can be of the form (repeat VAR  COUNT) or (repeat COUNT).

VAR-OR-COUNT is a variable name or an integer.  Optional COUNT is
an integer, to be used if a variable name is provided."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'repeat))
  (if count
      `((loopy--iteration-vars . (,var-or-count 0))
        (loopy--latter-body . (setq ,var-or-count (1+ ,var-or-count)))
        (loopy--pre-conditions . (< ,var-or-count ,count)))
    (let ((value-holder (gensym "repeat-limit-")))
      `((loopy--iteration-vars . (,value-holder 0))
        (loopy--latter-body . (setq ,value-holder (1+ ,value-holder)))
        (loopy--pre-conditions . (< ,value-holder ,var-or-count))))))

(cl-defun loopy--parse-seq-command ((_ var val))
  "Parse the `seq' loop command.

VAR is a variable name.  VAL is a sequence value.  VALUE-HOLDER
holds VAL.  INDEX-HOLDER holds an index that point into VALUE-HOLDER."
  ;; NOTE: `cl-loop' just combines the logic for lists and arrays, and
  ;;       just checks the type for each iteration, so we do that too.
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'seq))
  (let ((value-holder (gensym "seq-"))
        (index-holder (gensym "seq-index-"))
        (length-holder (gensym "seq-length-")))
    `((loopy--iteration-vars . (,value-holder ,val))
      (loopy--iteration-vars . (,index-holder 0))
      (loopy--iteration-vars . (,length-holder (length ,value-holder)))
      ,@(loopy--destructure-for-iteration-command
         var `(if (consp ,value-holder)
                  (pop ,value-holder)
                (aref ,value-holder ,index-holder)))
      (loopy--latter-body   . (setq ,index-holder (1+ ,index-holder)))
      (loopy--pre-conditions
       . (and ,value-holder (or (consp ,value-holder)
                                (< ,index-holder ,length-holder)))))))

(cl-defun loopy--parse-seq-ref-command ((_ var val))
  "Parse the `seq-ref' loop command.

VAR is a variable name.  VAL is a sequence value.  VALUE-HOLDER
holds VAL.  INDEX-HOLDER holds an index that point into
VALUE-HOLDER.  LENGTH-HOLDER holds than length of the value of
VALUE-HOLDER, once VALUE-HOLDER is initialized."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'seq-ref))
  (let ((value-holder (gensym "seq-ref-"))
        (index-holder (gensym "seq-ref-index-"))
        (length-holder (gensym "seq-ref-length-")))
    `((loopy--iteration-vars . (,value-holder ,val))
      (loopy--iteration-vars . (,length-holder (length ,value-holder)))
      (loopy--iteration-vars . (,index-holder 0))
      ,@(loopy--destructure-for-generalized-command
         var `(elt ,value-holder ,index-holder))
      (loopy--latter-body   . (setq ,index-holder (1+ ,index-holder)))
      (loopy--pre-conditions . (< ,index-holder ,length-holder)))))

(defun loopy--accumulation-starting-value (command-name)
  "Get the appropriate starting value for COMMAND-NAME."
  (cl-case command-name
    ((sum summing count counting)    0)
    ((max maxing maximize maximizing)
     -1.0e+INF)
    ((min minning minimize minimizing)
     +1.0e+INF)
    (t nil)))

;;;;; Accumulation
(defun loopy--parse-accumulation-commands (accumulation-command)
  "Pass ACCUMULATION-COMMAND to the appropriate parser, returning instructions.

- If no variable named, use `loopy--parse-implicit-accumulation-commands'.
- If only one variable name given, create a list of instructions here.
- Otherwise, use the value of `loopy--destructuring-accumulation-parser'
  or the value of `loopy-default-accumulation-parsing-function'."
  (cond
   ((= 2 (length accumulation-command))
    ;; If only two arguments, use an implicit accumulating variable.
    (loopy--parse-implicit-accumulation-commands accumulation-command))
   ;; If there is only one symbol (i.e., no destructuring), just do what's
   ;; normal.
   ((symbolp (cl-second accumulation-command))
    (cl-destructuring-bind (name var val) accumulation-command
      `((loopy--accumulation-vars . (,var ,(loopy--accumulation-starting-value
                                            name)))
        (loopy--implicit-return . ,var)
        (loopy--main-body
         . ,(cl-ecase name
              ((append appending) `(setq ,var (append ,var ,val)))
              ((collect collecting) `(setq ,var (append ,var (list ,val))))
              ((concat concating) `(setq ,var (concat ,var ,val)))
              ((vconcat vconcating) `(setq ,var (vconcat ,var ,val)))
              ((count counting) `(if ,val (setq ,var (1+ ,var))))
              ((max maxing maximize maximizing) `(setq ,var (max ,val ,var)))
              ((min minning minimize minimizing) `(setq ,var (min ,val ,var)))
              ((nconc nconcing) `(setq ,var (nconc ,var ,val)))
              ((prepend prepending) `(setq ,var (append ,val ,var)))
              ((push-into pushing-into push pushing) `(push ,val ,var))
              ((sum summing) `(setq ,var (+ ,val ,var))))))))
   (t
    (funcall (or loopy--destructuring-accumulation-parser
                 #'loopy--parse-destructuring-accumulation-command)
             accumulation-command))))

(cl-defun loopy--parse-implicit-accumulation-commands ((name value-expression))
  "Parse the accumulation command with implicit variable.

For better efficiency, accumulation commands with implicit variables can
have different behavior than their explicit counterparts.

NAME is the command name.  VALUE-EXPRESSION is an expression
whose value is to be accumulated."
  ;; NOTE: This function only applies to commands whose implicit behavior
  ;;       differs from the explicit behavior.  For commands that don't differ,
  ;;       we call back into `loopy--parse-accumulation-commands' with
  ;;       a `value-holder' as the explicit variable.

  (let* ((value-holder (if loopy--split-implied-accumulation-results
                           (gensym (concat (symbol-name name) "-implicit-"))
                         ;; Note: This must be `intern', not `make-symbol', as
                         ;;       the user can refer to it later.
                         (intern "loopy-result"))))
    `((loopy--accumulation-vars
       . (,value-holder ,(loopy--accumulation-starting-value name)))
      ,@(cl-ecase name
          ;; NOTE: Some commands have different behavior when a
          ;;       variable is not specified.
          ;;       - `collect' uses the `push'-`nreverse' idiom.
          ;;       - `append' uses the `reverse'-`nconc'-`nreverse' idiom.
          ;;       - `nconc' uses the `nreverse'-`nconc'-`nreverse' idiom.
          ((append appending)
           `((loopy--main-body
              . (setq ,value-holder (nconc (reverse ,value-expression)
                                           ,value-holder)))
             ,@(if loopy--split-implied-accumulation-results
                   (list `(loopy--implicit-return . (nreverse ,value-holder)))
                 (list
                  `(loopy--implicit-accumulation-final-update
                    . (setq ,value-holder (nreverse ,value-holder)))
                  `(loopy--implicit-return . ,value-holder)))))
          ((collect collecting)
           `((loopy--main-body
              . (setq ,value-holder (cons ,value-expression ,value-holder)))
             ,@(if loopy--split-implied-accumulation-results
                   (list `(loopy--implicit-return . (nreverse ,value-holder)))
                 (list
                  `(loopy--implicit-accumulation-final-update
                    . (setq ,value-holder (nreverse ,value-holder)))
                  `(loopy--implicit-return . ,value-holder)))))
          ((concat concating)
           `((loopy--main-body
              . (setq ,value-holder (cons ,value-expression ,value-holder)))
             ,@(if loopy--split-implied-accumulation-results
                   (list `(loopy--implicit-return
                           . (apply #'concat (nreverse ,value-holder))))
                 (list
                  `(loopy--implicit-accumulation-final-update
                    . (setq ,value-holder (apply #'concat
                                                 (nreverse ,value-holder))))
                  `(loopy--implicit-return . ,value-holder)))))
          ((vconcat vconcating)
           `((loopy--main-body
              . (setq ,value-holder (cons ,value-expression ,value-holder)))
             ,@(if loopy--split-implied-accumulation-results
                   (list `(loopy--implicit-return
                           . (apply #'vconcat (nreverse ,value-holder))))
                 (list
                  `(loopy--implicit-accumulation-final-update
                    . (setq ,value-holder (apply #'vconcat
                                                 (nreverse ,value-holder))))
                  `(loopy--implicit-return . ,value-holder)))))
          ((nconc nconcing)
           `((loopy--main-body
              . (setq ,value-holder (nconc (nreverse ,value-expression) ,value-holder)))
             ,@(if loopy--split-implied-accumulation-results
                   (list `(loopy--implicit-return . (nreverse ,value-holder)))
                 (list
                  `(loopy--implicit-accumulation-final-update
                    . (setq ,value-holder (nreverse ,value-holder)))
                  `(loopy--implicit-return . ,value-holder)))))
          ((prepend prepending)
           `((loopy--main-body
              . (setq ,value-holder (nconc ,value-expression
                                           ,value-holder)))
             (loopy--implicit-return . ,value-holder)))
          ;; Feed back into `loopy--parse-accumulation-commands' to avoid
          ;; duplicating code.  We remove any setting of accumulation vars,
          ;; which we already do above.
          (t
           (cl-remove-if (lambda (x) (eq (car x) 'loopy--accumulation-vars))
                         (loopy--parse-accumulation-commands
                          (list name value-holder value-expression))))))))

;;;;; Exiting and Skipping
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
          . (cl-return-from ,loopy--loop-name
              ,(cond
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

;;;; Destructuring

(defun loopy--destructure-for-generalized-command (var value-expression)
  "Destructure for commands that use generalized (`setf'-able) places.

Return a list of instructions for naming these `setf'-able places.

VAR are the variables into to which to destructure the value of
VALUE-EXPRESSION."
  (let ((destructurings
         (loopy--destructure-generalized-variables var value-expression))
        (instructions nil))
    (dolist (destructuring destructurings)
      (push (cons 'loopy--generalized-vars
                  destructuring)
            instructions))
    (nreverse instructions)))

(defun loopy--destructure-generalized-variables (var value-expression)
  "Destructure `setf'-able places.

Returns a list of variable-value pairs (not dotted), suitable for
substituting into `cl-symbol-macrolet'.

VAR are the variables into to which to destructure the value of
VALUE-EXPRESSION."
  (cl-typecase var
    ;; Check if `var' is a single symbol.
    (symbol
     ;; Return a list of lists, even for only one symbol.
     `((,(if (eq var '_) (gensym "destructuring-ref-") var)
        ,value-expression)))
    (list
     ;; If `var' is not proper, then the end of `var' can't be `car'-ed
     ;; safely, as it is just a symbol and not a list.  Therefore, if `var'
     ;; is still non-nil after the `pop'-ing, we know to set the remaining
     ;; symbol that is now `var' to some Nth `cdr'.
     (let ((destructured-values) (index 0))
       (while (car-safe var)
         (push (loopy--destructure-generalized-variables
                (pop var) `(nth ,index ,value-expression))
               destructured-values)
         (setq index (1+ index)))
       (when var
         (push (loopy--destructure-generalized-variables
                var `(nthcdr ,index ,value-expression))
               destructured-values))
       (apply #'append (nreverse destructured-values))))
    (array
     (cl-loop for symbol-or-seq across var
              for index from 0
              append (loopy--destructure-generalized-variables
                      symbol-or-seq `(aref ,value-expression ,index))))
    (t
     (error "Don't know how to destructure this: %s" var))))

;; Note that function `loopy--destructure-variables-default' is defined in
;; 'loop.el', as it is also used for `with' variables.
(defun loopy--destructure-for-iteration-default (var val)
  "Destructure VAL according to VAR.

Returns a list.  The elements are:
1. An expression which binds the variables in VAR to the values
   in VAL.
2. A list of variables which exist outside of this expression and
   need to be `let'-bound."
  (let ((bindings (loopy--destructure-variables-default var val)))
    (list (cons 'setq (apply #'append bindings))
          (cl-remove-duplicates (mapcar #'cl-first bindings)))))

(defun loopy--destructure-for-iteration-command (var value-expression)
  "Destructure VALUE-EXPRESSION according to VAR for a loop command.

Note that this does not apply to commands which use generalized
variables (`setf'-able places).  For that, see the function
`loopy--destructure-for-generalized-command'.

Return a list of instructions for initializing the variables and
destructuring into them in the loop body."
  (cl-destructuring-bind (destructuring-expression var-list)
      (funcall (or loopy--destructuring-for-iteration-function
                   #'loopy--destructure-for-iteration-default)
               var value-expression)
    `((loopy--main-body . ,destructuring-expression)
      ,@(mapcar (lambda (x) `(loopy--iteration-vars . (,x nil)))
                var-list))))

(cl-defun loopy--parse-destructuring-accumulation-command
    ((name var val))
  "Return instructions for destructuring accumulation commands.

Unlike `loopy--destructure-variables-default', this function
does destructuring and returns instructions.

NAME is the name of the command.  VAR is a variable name.  VAL is a value."
  (cl-etypecase var
    (list
     (let ((value-holder (gensym (concat (symbol-name name) "-destructuring-list-")))
           (is-proper-list (proper-list-p var))
           (normalized-reverse-var))
       (let ((instructions `(((loopy--iteration-vars . (,value-holder nil))
                              (loopy--main-body . (setq ,value-holder ,val))))))
         ;; If `var' is a list, always create a "normalized" variable
         ;; list, since proper lists are easier to work with, as many
         ;; looping/mapping functions expect them.
         (while (car-safe var)
           (push (pop var) normalized-reverse-var))
         ;; If the last element in `var' was a dotted pair, then
         ;; `var' is now a single symbol, which must still be added
         ;; to the normalized `var' list.
         (when var (push var normalized-reverse-var))

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
             `(((loopy--iteration-vars . (,value-holder nil))
                (loopy--main-body . (setq ,value-holder ,val))))))
       (cl-loop for symbol-or-seq across var
                for index from 0
                do (push (loopy--parse-accumulation-commands
                          (list
                           name symbol-or-seq `(aref ,value-holder ,index)))
                         instructions))
       (apply #'append (nreverse instructions))))))

;;;; Selecting parsers
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
  '((append       . loopy--parse-accumulation-commands)
    (appending    . loopy--parse-accumulation-commands)
    (across       . loopy--parse-array-command)
    (across-ref   . loopy--parse-array-ref-command)
    (array        . loopy--parse-array-command)
    (array-ref    . loopy--parse-array-ref-command)
    (arrayf       . loopy--parse-array-ref-command)
    (collect      . loopy--parse-accumulation-commands)
    (collecting   . loopy--parse-accumulation-commands)
    (concat       . loopy--parse-accumulation-commands)
    (concating    . loopy--parse-accumulation-commands)
    (cond         . loopy--parse-cond-command)
    (cons         . loopy--parse-cons-command)
    (conses       . loopy--parse-cons-command)
    (continue     . loopy--parse-skip-command)
    (count        . loopy--parse-accumulation-commands)
    (counting     . loopy--parse-accumulation-commands)
    (do           . loopy--parse-do-command)
    (elements     . loopy--parse-seq-command)
    (elements-ref . loopy--parse-seq-ref-command)
    (expr         . loopy--parse-expr-command)
    (exprs        . loopy--parse-expr-command)
    (group        . loopy--parse-group-command)
    (if           . loopy--parse-if-command)
    (in           . loopy--parse-list-command)
    (in-ref       . loopy--parse-list-ref-command)
    (leave        . loopy--parse-leave-command)
    (list         . loopy--parse-list-command)
    (list-ref     . loopy--parse-list-ref-command)
    (listf        . loopy--parse-list-ref-command)
    (loop         . loopy--parse-sub-loop-command)
    (max          . loopy--parse-accumulation-commands)
    (maxing       . loopy--parse-accumulation-commands)
    (maximize     . loopy--parse-accumulation-commands)
    (maximizing   . loopy--parse-accumulation-commands)
    (min          . loopy--parse-accumulation-commands)
    ;; Unlike "maxing", there doesn't seem to be much on-line about the word
    ;; "minning", but the double-N follows conventional spelling rules, such as
    ;; in "sum" and "summing".
    (minning      . loopy--parse-accumulation-commands)
    (minimize     . loopy--parse-accumulation-commands)
    (minimizing   . loopy--parse-accumulation-commands)
    (nconc        . loopy--parse-accumulation-commands)
    (nconcing     . loopy--parse-accumulation-commands)
    (on           . loopy--parse-cons-command)
    (prepend      . loopy--parse-accumulation-commands)
    (prepending   . loopy--parse-accumulation-commands)
    (push         . loopy--parse-accumulation-commands)
    (pushing      . loopy--parse-accumulation-commands)
    (push-into    . loopy--parse-accumulation-commands)
    (pushing-into . loopy--parse-accumulation-commands)
    (repeat       . loopy--parse-repeat-command)
    (return       . loopy--parse-early-exit-commands)
    (return-from  . loopy--parse-early-exit-commands)
    (seq          . loopy--parse-seq-command)
    (seq-ref      . loopy--parse-seq-ref-command)
    (seqf         . loopy--parse-seq-ref-command)
    (sequence     . loopy--parse-seq-command)
    (sequencef    . loopy--parse-seq-ref-command)
    (set          . loopy--parse-expr-command)
    (skip         . loopy--parse-skip-command)
    (subloop      . loopy--parse-sub-loop-command)
    (sub-loop     . loopy--parse-sub-loop-command)
    (sum          . loopy--parse-accumulation-commands)
    (summing      . loopy--parse-accumulation-commands)
    (unless       . loopy--parse-when-unless-command)
    (until        . loopy--parse-while-until-commands)
    (vconcat      . loopy--parse-accumulation-commands)
    (vconcating   . loopy--parse-accumulation-commands)
    (when         . loopy--parse-when-unless-command)
    (while        . loopy--parse-while-until-commands))
  "An alist of pairs of command names and built-in parser functions.")

(defun loopy--get-command-parser (command)
  "Get the parsing function for COMMAND, based on the command name.

The following variables are checked:

1. `loopy-custom-command-aliases'
2. `loopy-custom-command-parsers'
3. `loopy--builtin-command-parsers'

Failing that, an error is signaled."

  (let ((key (car command)))
    (or (when-let ((alias-def (cdr (assq key loopy-custom-command-aliases))))
          (or (cdr (assq alias-def loopy-custom-command-parsers))
              (cdr (assq alias-def loopy--builtin-command-parsers))))
        (cdr (assq key loopy-custom-command-parsers))
        (cdr (assq key loopy--builtin-command-parsers))
        (signal 'loopy-unknown-command command))))

(provide 'loopy-commands)

;;; loopy-commands.el ends here
