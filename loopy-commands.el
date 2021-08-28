;;; loopy-commands.el --- Loop commands for loopy -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: February 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.8.1
;; Package-Requires: ((emacs "27.1") (loopy "0.8.1"))
;; Keywords: extensions
;; LocalWords:  Loopy's emacs alists

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

;; NOTE:
;;
;; For more easily finding loop commands using Imenu, you might wish to do
;; something like the below, which should work for most definitions.
;;
;; (progn
;;   (push (list "Loop Commands"
;;               (rx (seq bol
;;                        (zero-or-more (syntax whitespace))
;;                        "(loopy--def"
;;                        (or "iteration" "accumulation")
;;                        (one-or-more (syntax whitespace))
;;                        (group (one-or-more (or (syntax word)
;;                                                (syntax symbol)
;;                                                (seq "\\" nonl))))))
;;               1)
;;         imenu-generic-expression)
;;   (push (list "Loop Commands"
;;               (rx (seq bol
;;                        (zero-or-more (syntax whitespace))
;;                        "(defun loopy--parse-"
;;                        (group (one-or-more (or (syntax word)
;;                                                (syntax symbol)
;;                                                (seq "\\" nonl))))
;;                        "-command"))
;;               1)
;;         imenu-generic-expression))

;; TODO:
;;
;; - `cl-remove-if' doesn't seem to always make a copy in the way that we
;;   expect, so we use `cl-delete-if' with `copy-sequence' to be more
;;   predictable.

;; Can't require `loopy', as that would be recursive.
(require 'cl-lib)
(require 'gv)
(require 'loopy-misc)
(require 'loopy-vars)
(require 'map)
(require 'macroexp)
(require 'pcase)
(require 'seq)
(require 'subr-x)

(declare-function loopy--bound-p "loopy")
(defvar loopy--in-sub-level)

;;;; Aliases
(defmacro loopy-defalias (alias definition)
  "Add alias ALIAS for loop command DEFINITION.

Neither argument need be quoted."
  `(setf (map-elt loopy-command-aliases ,(if (eq (car-safe alias) 'quote)
                                             alias
                                           `(quote ,alias)))
         ,(if (eq (car-safe definition) 'quote)
              definition
            `(quote ,definition))))

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
;;;;; Quoted Symbols and Functions
(defun loopy--get-function-symbol (function-form)
  "Return the actual symbol described by FUNCTION-FORM.

When a quoted argument is passed to a macro, it can appear
as `(quote my-var)' or `(function my-func)' inside the body.  For
expansion, we generally only want the actual symbol."
  (if (symbolp function-form)
      function-form
    (cl-case (cl-first function-form)
      ((function quote cl-function) (cl-second function-form))
      (lambda function-form)
      (t (error "This function form is unrecognized: %s" function-form)))))

(defun loopy--get-quoted-symbol (quoted-form)
  "Return the actual symbol of QUOTED-FORM.

When quoted symbols are passed to the macro, these can show up as
\"(quote SYMBOL)\", where we only want SYMBOL.

For functions, use `loopy--get-function-symbol'."
  (cond
   ((symbolp quoted-form)
    quoted-form)
   ((eq (car-safe quoted-form) 'quote)
    (cl-second quoted-form))
   (t
    (error "This function form is unrecognized: %s" quoted-form))))

(defun loopy--quoted-form-p (form-or-symbol)
  "Whether FORM-OR-SYMBOL is quoted via `quote' or `function'.

If not, then it is possible that FORM is a variable."

  (and (listp form-or-symbol)
       (= 2 (length form-or-symbol))
       (or (eq (car form-or-symbol) 'quote)
           (eq (car form-or-symbol) 'function)
           (eq (car form-or-symbol) 'cl-function))))

(defun loopy--quoted-symbol-p (form)
  "Whether FORM is a quoted symbol."
  (and (memq (car-safe form) '(quote function cl-function))
       (symbolp (cl-second form))))

(defun loopy--quote-if-car-not-symbol-or-lambda (list)
  "Apply a heuristic to decide if LIST should be quoted."
  (if (or (symbolp (car-safe list))
          (eq (car-safe list) 'lambda))
      list
    `(quote ,list)))

(defun loopy--apply-function (func &rest args)
  "Return an expansion to appropriately apply FUNC to ARGS.

This expansion can apply FUNC directly or via `funcall'."
  (if (loopy--quoted-form-p func)
      `(,(loopy--get-function-symbol func) ,@args)
    `(funcall ,func ,@args)))

;;;;; Manipulating Instructions

;; If Emacs Lisp ever gets support for true multiple values (via `cl-values'),
;; this function might be a good candidate for use.
(defun loopy--extract-main-body (instructions)
  "Extract main-body expressions from INSTRUCTIONS.

This returns a list of two sub-lists:

1. A list of expressions (not instructions) that are meant to be
   use in the main body of the loop.

2. A list of instructions for places other than the main body.

The lists will be in the order parsed (correct for insertion)."
  (let ((wrapped-main-body)
        (other-instructions))
    (dolist (instruction instructions)
      (if (eq (cl-first instruction) 'loopy--main-body)
          (push (cl-second instruction) wrapped-main-body)
        (push instruction other-instructions)))

    ;; Return the sub-lists.
    (list (nreverse wrapped-main-body) (nreverse other-instructions))))

;;;;; Working with Plists and Keyword Arguments

;; Loopy uses property lists to handle keyword arguments.
(defun loopy--extract-keywords (list)
  "Extract the keywords from LIST according to `keywordp'."
  (cl-loop for i in list
           if (keywordp i)
           collect i))

(defun loopy--only-valid-keywords-p (correct list)
  "Return nil if a keyword in LIST is not in CORRECT.

Any keyword not in CORRECT is considered invalid.

CORRECT is a list of valid keywords.  The first item in LIST is
assumed to be a keyword."
  ;; (null (cl-set-difference (loopy--every-other list) correct))
  (null (cl-set-difference (loopy--extract-keywords list) correct)))

;;;;; Miscellaneous
(defun loopy--mimic-init-structure (var val)
  "Create a sequence of VAL that mimics the structure of VAR.

For some destructuring loop commands, and initialization value is
the same for all destructured variables.  For that to work, one
must sometimes create a sequence to be destructured."
  (cl-typecase var
    (symbol val)
    (list
     (let ((val-list))
       (while (car-safe var)
         (push (loopy--mimic-init-structure (pop var) val)
               val-list))
       (setq val-list (nreverse val-list))
       (if var
           `(,@val-list . ,(loopy--mimic-init-structure var val))
         val-list)))
    (array
     (cl-coerce (cl-loop for i across var
                         collect (loopy--mimic-init-structure i val))
                'array))))


;;;; Included parsing functions.
;;;;; Sub-Loops
;;;;;; At
(cl-defun loopy--parse-at-command ((_ target-loop &rest commands))
  "Parse the `at' command as (at &rest COMMANDS).

These commands affect other loops higher up in the call list."
  (loopy--check-target-loop-name target-loop)
  (let ((loopy--loop-name target-loop)
        (loopy--in-sub-level t))
    `((loopy--at-instructions
       (,target-loop
        ,@(loopy--parse-loop-commands commands))))))

;;;;;; Sub-Loop
(cl-defun loopy--parse-sub-loop-command ((_ &rest body))
  "Parse the `sub-loop' command as (sub-loop BODY).

The sub-loop is a full call to the `loopy' macro, supporting
special macro arguments.  In `loopy-iter', it is specially
handled to use `loopy-iter' instead.

The sub-loop is specially handled."
  `((loopy--main-body ,(macroexpand `(loopy ,@body)))))

;;;;;; Loopy
(cl-defun loopy--parse-loopy-command ((_ &rest body))
  "Parse the `loopy' command as (loopy BODY).

Unlike the `sub-loop' command, this command is not specially
handled by `loopy-iter'."
  `((loopy--main-body ,(macroexpand `(loopy ,@body)))))

;;;;; Genereric Evaluation
;;;;;; Expr
(cl-defun loopy--parse-expr-command ((_ var &rest vals))
  "Parse the `expr' command.

- VAR is the variable to assign.
- VALS are the values to assign to VAR."
  (let* ((length-vals (length vals))
         (using-init-arg (eq (nth (- length-vals 2) vals)
                             ':init))
         (init-arg (when using-init-arg
                     (nth (1- length-vals) vals))))
    (let ((arg-length (if using-init-arg
                          (- length-vals 2)
                        length-vals))
          (value-selector (gensym "expr-value-selector-")))
      (let ((needed-instructions
             (cl-case arg-length
               ;; If no values, repeatedly set to `nil'.
               (0 (loopy--destructure-for-iteration-command
                   var nil))
               ;; If one value, repeatedly set to that value.
               (1 (loopy--destructure-for-iteration-command
                   var (cl-first vals)))
               ;; If two values, repeatedly check against `value-selector' to
               ;; determine if we should assign the first or second value.  This
               ;; is how `cl-loop' does it.
               (2
                `((loopy--iteration-vars (,value-selector t))
                  ,@(loopy--destructure-for-iteration-command
                     var `(if ,value-selector ,(cl-first vals) ,(cl-second vals)))
                  ;; This needs to happen right after running the above.
                  (loopy--main-body (setq ,value-selector nil))))
               (t
                `((loopy--iteration-vars (,value-selector 0))
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
                   (when (< ,value-selector ,(1- arg-length))
                     (setq ,value-selector (1+ ,value-selector)))))))))
        (if init-arg
            (loopy--substitute-using-if
             (cl-function (lambda ((_ (var _)))
                            `(loopy--iteration-vars
                              (,var ,init-arg))))
             (lambda (x) (eq (cl-first x) 'loopy--iteration-vars))
             needed-instructions)
          needed-instructions)))))

;;;;;; Prev Expr
(cl-defun loopy--parse-prev-expr-command ((_ var val &key init back))
  "Parse the `prev-expr' command as (prev-expr VAR VAL &key init back).

VAR is set to a version of VAL in a past loop cycle.  With INIT,
initialize VAR to INIT.  With BACK, wait that many cycle before
beginning to update VAR.

This command does not wait for VAL to change before updating VAR."
  (let ((holding-vars (cl-loop for i from 1 to (or back 1)
                               collect (gensym "prev-expr-hold")))
        (init-value-holder (gensym "prev-expr-init"))
        (init-destr-value-holder (gensym "prev-expr-destr-init"))
        (using-destructuring (sequencep var)))
    ;; TODO: This feels more complicated than it needs to be, but the resulting
    ;;       code is pretty simple.
    `(,@(if init
            `((loopy--iteration-vars (,init-value-holder ,init))
              ;; When using destructuring, each variable in `holding-vars'
              ;; needs to be initialized to a value that can be destructured
              ;; according to VAR.
              ;;
              ;; We only want to calculate the initial value once, even for the
              ;; destructuring, so we require two holding variables.
              ,@(if using-destructuring
                    `((loopy--iteration-vars
                       (,init-destr-value-holder
                        (loopy--mimic-init-structure
                         (quote ,var) ,init-value-holder)))
                      ,@(mapcar (lambda (x)
                                  `(loopy--iteration-vars
                                    (,x ,init-destr-value-holder)))
                                holding-vars))
                  (mapcar (lambda (x)
                            `(loopy--iteration-vars  (,x ,init-value-holder)))
                          holding-vars)))
          (mapcar (lambda (x)
                    `(loopy--iteration-vars (,x nil)))
                  holding-vars))
      ,@(loopy--substitute-using
         (pcase-lambda ((and instr `(,place (,var ,_))))
           (if (eq place 'loopy--iteration-vars)
               `(,place (,var ,(when init
                                 init-value-holder)))
             instr))
         (loopy--destructure-for-iteration-command
          var (car (last holding-vars))))
      (loopy--latter-body
       (setq ,@(apply #'append
                      (nreverse (cl-loop for pvar = val then hv
                                         for hv in holding-vars
                                         collect (list hv pvar)))))))))

;;;;;; Group
(cl-defun loopy--parse-group-command ((_ &rest body))
  "Parse the `group' loop command.

BODY is one or more commands to be grouped by a `progn' form.
This command is suitable for using as the first sub-command in an
`if' command."
  (let ((loopy--in-sub-level t))
    (cl-destructuring-bind (progn-body rest)
        (loopy--extract-main-body (loopy--parse-loop-commands body))
      ;; Return the instructions.
      (cons `(loopy--main-body (progn ,@progn-body))
            rest))))

;;;;;; Do
(cl-defun loopy--parse-do-command ((_ &rest expressions))
  "Parse the `do' loop command.

Expressions are normal Lisp expressions, which are inserted into
the loop literally (not even in a `progn')."
  (mapcar (lambda (expr) (list 'loopy--main-body expr))
          expressions))

;;;;; Conditionals
;;;;;; If
(cl-defun loopy--parse-if-command
    ((_ condition &optional if-true &rest if-false))
  "Parse the `if' loop command.  This takes the entire command.

- CONDITION is a Lisp expression.
- IF-TRUE is the first sub-command of the `if' command.
- IF-FALSE are all the other sub-commands."
  (let ((loopy--in-sub-level t))
    (pcase-let ((`(,if-true-main-body ,true-rest)
                 (loopy--extract-main-body (loopy--parse-loop-command if-true)))
                (`(,if-false-main-body ,false-rest)
                 (loopy--extract-main-body (loopy--parse-loop-commands if-false))))

      ;; Handle if we need to wrap multiple main-body expressions.
      (setq if-true-main-body (macroexp-progn if-true-main-body))

      ;; Return the full instruction list.
      `((loopy--main-body
         (if ,condition ,if-true-main-body ,@if-false-main-body))
        ,@true-rest
        ,@false-rest))))

;;;;;; Cond
(cl-defun loopy--parse-cond-command ((_ &rest clauses))
  "Parse the `cond' command.  This works like the `cond' special form.

CLAUSES are lists of a Lisp expression followed by one or more
loop commands.

The Lisp expression and the loopy-body instructions from each
command are inserted into a `cond' special form."
  (let ((loopy--in-sub-level t)
        (cond-body nil)
        (rest-instructions nil))
    (cl-loop for clause in clauses
             for (main-body rest) = (loopy--extract-main-body
                                     (loopy--parse-loop-commands
                                      (cl-rest clause)))
             do
             (push (cons (cl-first clause) main-body) cond-body)
             (push rest rest-instructions))
    (cons `(loopy--main-body (cond ,@(nreverse cond-body)))
          (apply #'append (nreverse rest-instructions)))))

;;;;;; When Unless
(cl-defun loopy--parse-when-unless-command ((name condition &rest body))
  "Parse `when' and `unless' commands as (when/unless CONDITION [COMMANDS]).

- NAME is `when' or `unless'.
- CONDITION is the condition.
- BODY is the sub-commands."
  (let ((loopy--in-sub-level t)
        (when-body)
        (other-instructions))
    (dolist (cmd body)
      (cl-destructuring-bind (main-body rest)
          (loopy--extract-main-body (loopy--parse-loop-command cmd))
        (push main-body when-body)
        (push rest other-instructions)))
    (cons `(loopy--main-body
            (,name ,condition ,@(apply #'append (nreverse when-body))))
          (apply #'append (nreverse other-instructions)))))

;;;;; Iteration
(cl-defmacro loopy--defiteration
    (name doc-string &key keywords (required-vals 1) other-vals instructions)
  "Define an interation command parser for NAME.

An iteration command made with this macro has the layout of
\(command-name variable-name value [values] [keys]).  That is,
each command will always require a variable name and a value.
Some commands will have additional non-keyword arguments, which
will be either required or optional.  Some commands will have
keyword arguments, which, as in functions, are always optional.

Not all iteration loop commands are written using this macro.
This macro is for iteration commands that have the above
structure.

- NAME is the primary name of the loop command.  This creates a
  parsing function \"loopy--parse-NAME-command\".

- DOC-STRING is the documentation string for the produced parsing
  function.  It should describe the arguments of the loop command.

- KEYWORDS is an unquoted list of colon-prefixed keywords used by
  the command.

- REQUIRED-VALS is the number of required values.  For most
  iteration commands, this is one.  This can be one nil (or
  zero), a non-zero number, or an unquoted list of variable
  names.

  If a list of variable names, those names are used in the
  function definition.  Otherwise, the variables are named
  following the naming scheme \"valNUM\".

- OTHER-VALS is whether the command can take other arguments that
  are not keyword arguments.  This is one of nil, t, or an
  unquoted list of numbers.

  If a list of numbers, then those numbers describe the valid
  amounts of additional (non-keyword) arguments of the command.
  For example, (0 1) means that command has 1 optional
  non-keyword argument.  (1 3) means the command requires either
  1 or 3 additional non-keyword arguments.

  This means that if a command has no required arguments, but
  several optional non-keyword arguments, you can set
  REQUIRED-VALS to 0 and OTHER-VALS to a list of 0 and some
  integer.

  If t, no check if performed and the command takes any number of
  optional additional arguments.

- INSTRUCTIONS are the command's instructions.  This should be a
  single expression, such as a list of instructions or an
  expression capable of producing said instructions.

In the expanded parsing function, there are several values
automatically bound to variables, which can be used in
instructions:

- `name' is the name of the command.

- `cmd' is the entire command expression.

- `var' is the variable to be used by the command.

- `val' is the required value (generally, the sequence over which
  to iterate). If there are multiple required values, they are
  instead named according to the value of REQUIRED-VALS.

- `args' is a list of the remaining arguments of the command
   after `var' and `val'.  If either OTHER-VALS or KEYWORDS are
   nil, then this variable is not bound.  In that case, use the
   below two variables.

- `other-vals' is a list of additional values found in `args',
   if OTHER-VALS is non-nil.

- `opts' is a list of keyword arguments and their values,
  found in `args' after the elements of `other-vals'.
  It is assumed that this list can be treated as a property list.
  The first keyword in `args' determines the start of
  the optional keyword arguments.

  If OTHER-VALS is non-nil (i.e., no other values are allowed),
  then these keyword variables should be referenced directly
  instead of through the property list `opts'."

  (declare (indent defun) (doc-string 2))

  ;; Make sure `keywords' is a list.
  (when (nlistp keywords)
    (setq keywords (list keywords)))

  ;; Make sure `keywords' are all prefixed with a colon.
  (setq keywords (mapcar (lambda (x)
                           (if (eq ?: (aref (symbol-name x) 0))
                               x
                             (intern (format ":%s" x))))
                         keywords))

  ;; Check that `instructions' given.
  (unless instructions
    (error "Instructions required"))

  ;; Store the variable names of the keyword arguments so we only have to
  ;; compute them one.  E.g., "by" from ":by".
  ;; Currently, keywords are required to be prefixed by the colon.
  (let ((var-keys (when keywords
                    (cl-loop for sym in keywords
                             collect (intern (substring (symbol-name sym) 1))))))

    `(cl-defun ,(intern (format "loopy--parse-%s-command" name))
         (( &whole cmd name var
            ,@(cond
               ((eq 1 required-vals)
                '(val))
               ((or (eq required-vals nil)
                    (eq required-vals 0))
                nil)
               ((consp required-vals)
                required-vals)
               ((integerp required-vals)
                (cl-loop for i from 1 to required-vals
                         collect (intern (format "val%d" i))))
               (t
                (error "Bad value for `required-vals': %s" required-vals)))
            ,@(if keywords
                  (if other-vals
                      '(&rest args)
                    `(&key ,@var-keys))
                (when other-vals
                  '(&rest other-vals)))))
       ,doc-string

       (when loopy--in-sub-level
         (loopy--signal-bad-iter (quote ,name)))

       (let* ,(if keywords
                  (if other-vals
                      '((other-vals nil)
                        (opts nil))
                    ;; These can be referred to directly, but we'll keep
                    ;; the option open for using `opts'.
                    `((opts (list ,@(cl-loop for sym in keywords
                                             for var in var-keys
                                             append (list sym var))))))
                nil)

         ;; We only want to run this code if the values of `opts' and
         ;; `other-vals' are contained in `args'.  For other cases, the
         ;; function arguments perform this step for us.
         ,@(when (and other-vals keywords)
             `(;; Set `opts' as starting from the first keyword and `other-vals'
               ;; as everything before that.
               (cl-loop with other-val-holding = nil
                        for cons-cell on args
                        for arg = (car cons-cell)
                        until (keywordp arg)
                        do (push arg other-val-holding)
                        finally do (setq opts cons-cell
                                         other-vals (nreverse other-val-holding)))

               ;; Validate any keyword arguments:
               (unless (loopy--only-valid-keywords-p (quote ,keywords) opts)
                 (error "Wrong number of arguments or wrong keywords: %s" cmd))))

         ,(when (consp other-vals)
            `(unless (cl-member (length other-vals)
                                (quote ,other-vals)
                                :test #'=)
               (error "Wrong number of arguments or wrong keywords: %s" cmd)))

         (ignore cmd name
                 ;; We can only ignore variables if they're defined.
                 ,(if other-vals 'other-vals)
                 ,(if keywords 'opts))

         ,instructions))))

(defun loopy--find-start-by-end-dir-vals (plist)
  "Find the numeric start, end, and step, direction, and inclusivity.

The values are returned in a list in that order as a plist.

PLIST contains the keyword arguments passed to a sequence
iteration command.  The supported keywords are:

- from, upfrom (inclusive start)
- downfrom (inclusive start)
- to, upto (inclusive end)
- downto (inclusive end)
- above (exclusive end)
- below (exclusive end)
- by (increment)"

  (loopy--plist-bind ( :from from :upfrom upfrom :downfrom downfrom
                       :to to :upto upto :downto downto
                       :above above :below below
                       :by by)
      plist
    ;; Check the inputs:
    (when (or (< 1 (cl-count-if #'identity (list from upfrom downfrom)))
              (< 1 (cl-count-if #'identity (list to upto downto above below)))
              (and downfrom below)
              (and upfrom above))
      (error "Conflicting arguments: %s" plist))

    (let ((decreasing (or downfrom downto above)))

      ;; Check directions  for above and below.
      ;; :above is only for when the value is decreasing.
      ;; :below is only for when the value in increasing.
      (when (or (and below decreasing)
                (and above (not decreasing)))
        (error "Conflicting arguments: %s" plist))

      `(,@(when-let ((start (or from upfrom downfrom)))
            `(:start ,start))
        ,@(when by
            `(:by ,by))
        ,@(when-let ((end (or to downto above below upto)))
            `(:end ,end))
        :decreasing ,decreasing
        :inclusive ,(not (or above below))))))

;;;;;; Array
(defmacro loopy--distribute-array-elements (&rest arrays)
  "Distribute the elements of the ARRAYS into an array of lists.

For example, [1 2] and [3 4] gives [(1 3) (1 4) (2 3) (2 4)]."
  (let ((vars (cl-loop for _ in arrays
                       collect (gensym "array-var-")))
        (reverse-order (reverse arrays)))
    (cl-loop with expansion = `(cl-loop for ,(cl-first vars)
                                        across ,(cl-first reverse-order)
                                        do (setq result
                                                 (cons (list ,@(reverse vars))
                                                       result)))
             for var in (cl-rest vars)
             for array in (cl-rest reverse-order)
             do (setq expansion `(cl-loop for ,var across ,array
                                          do ,expansion))
             finally return `(let ((result nil))
                               ,expansion
                               (vconcat (nreverse result))))))

(loopy--defiteration array
  "Parse the `array' command as (array VAR VAL [VALS] &key KEYS).

KEYS is one or several of `:index', `:by', `:from', `:downfrom',
`:upfrom', `:to', `:downto', `:upto', `:above', or `:below'.

- `:index' names a variable used to store the accessed index of
  the array.
- `:by' is the increment step size as a positive value.
- `:from', `:downfrom', and `:upfrom' name the starting index
- `:to', `:downto', and `:upto' name the ending index (inclusive)
- `:below' and `:above' name an exclusive ending index.

`:downto' and `:downfrom' make the index decrease instead of increase.

If multiple values are given, their elements are distributed
using the function `loopy--distribute-array-elements'."
  :other-vals t
  :keywords (:index
             :by :from :downfrom :upfrom :to :downto :upto :above :below)
  :instructions
  (let ((value-holder (gensym "array-"))
        (index-holder (or (plist-get opts :index)
                          (gensym "array-index-")))
        (end-holder (gensym "array-end-"))
        (increment-holder (gensym "array-increment")))

    (loopy--plist-bind ( :start key-start :end key-end :by (by 1)
                         :decreasing decreasing :inclusive inclusive)

        (loopy--find-start-by-end-dir-vals opts)

      `((loopy--iteration-vars (,increment-holder ,by))
        (loopy--iteration-vars
         (,value-holder ,(if (null other-vals)
                               val
                             `(loopy--distribute-array-elements
                               ,val ,@other-vals))))
        (loopy--iteration-vars (,end-holder ,(or key-end
                                                   (if decreasing
                                                       -1
                                                     `(length ,value-holder)))))
        (loopy--iteration-vars (,index-holder ,(or key-start
                                                     (if decreasing
                                                         `(1- (length ,value-holder))
                                                       0))))
        ,@(loopy--destructure-for-iteration-command
           var `(aref ,value-holder ,index-holder))
        (loopy--latter-body    (setq ,index-holder (,(if decreasing #'- #'+)
                                                      ,index-holder
                                                      ,increment-holder)))
        (loopy--pre-conditions (,(if (or (null key-end)
                                           (not inclusive))
                                       (if decreasing #'> #'<)
                                     (if decreasing #'>= #'<=))
                                  ,index-holder ,end-holder))))))

;;;;;; Array Ref
(loopy--defiteration array-ref
  "Parse the `array-ref' command as (array-ref VAR VAL &key KEYS).

KEYS is one or several of `:index', `:by', `:from', `:downfrom',
`:upfrom', `:to', `:downto', `:upto', `:above', or `:below'.

- `:index' names a variable used to store the accessed index of
  the array.
- `:by' is the increment step size as a positive value.
- `:from', `:downfrom', and `:upfrom' name the starting index
- `:to', `:downto', and `:upto' name the ending index (inclusive)
- `:below' and `:above' name an exclusive ending index.

`:downto' and `:downfrom' make the index decrease instead of increase."
  :keywords (:index :by :from :downfrom :upfrom :to :downto :upto :above :below)
  :instructions
  (let ((value-holder (gensym "array-ref-"))
        (index-holder (or (plist-get opts :index)
                          (gensym "array-ref-index-")))
        (end-holder (gensym "array-ref-end-"))
        (increment-holder (gensym "array-ref-increment-")))

    (loopy--plist-bind ( :start key-start :end key-end :by (by 1)
                         :decreasing decreasing :inclusive inclusive)

        (loopy--find-start-by-end-dir-vals opts)

      `((loopy--iteration-vars (,increment-holder ,by))
        (loopy--iteration-vars (,value-holder ,val))
        (loopy--iteration-vars (,end-holder ,(or key-end
                                                   (if decreasing
                                                       -1
                                                     `(length ,value-holder)))))
        (loopy--iteration-vars (,index-holder ,(or key-start
                                                     (if decreasing
                                                         `(1- (length ,value-holder))
                                                       0))))
        ,@(loopy--destructure-for-generalized-command
           var `(aref ,value-holder ,index-holder))
        (loopy--latter-body    (setq ,index-holder (,(if decreasing #'- #'+)
                                                      ,index-holder
                                                      ,increment-holder)))
        (loopy--pre-conditions (,(if (or (null key-end)
                                           (not inclusive))
                                       (if decreasing #'> #'<)
                                     (if decreasing #'>= #'<=))
                                  ,index-holder ,end-holder))))))

;;;;;; Cons
(loopy--defiteration cons
  "Parse the `cons' loop command as (cons VAR VAL &key by).

VAR is a variable name.  VAL is a cons cell value.  Keyword BY
is a function by which to update VAR (default `cdr')."
  :keywords (:by)
  :instructions
  (let ((value-holder (gensym "cons-")))
    `((loopy--iteration-vars (,value-holder ,val))
      ,@(loopy--destructure-for-iteration-command var value-holder)
      (loopy--latter-body
       (setq ,value-holder ,(loopy--apply-function (or by (quote #'cdr))
                                                     value-holder)))
      (loopy--pre-conditions (consp ,value-holder)))))

;;;;;; List
(defmacro loopy--distribute-list-elements (&rest lists)
  "Distribute the elements of LISTS into a list of lists.

For example, (1 2) and (3 4) would give ((1 3) (1 4) (2 3) (2 4))."
  (let ((vars (cl-loop for _ in lists
                       collect (gensym "list-var-")))
        (reverse-order (reverse lists)))
    (cl-loop with expansion = `(dolist (,(cl-first vars)
                                        ,(cl-first reverse-order))
                                 (setq result (cons (list ,@(reverse vars))
                                                    result)))
             for var in (cl-rest vars)
             for list in (cl-rest reverse-order)
             do (setq expansion `(dolist (,var ,list)
                                   ,expansion))
             finally return `(let ((result nil))
                               ,expansion
                               (nreverse result)))))

(loopy--defiteration list
  "Parse the list command as (list VAR VAL [VALS] &key by).

BY is function to use to update the list.  It defaults to `cdr'.

If multiple values are given, their elements are distributed
using the function `loopy--distribute-list-elements'."
  :other-vals t
  :keywords (:by)
  :instructions
  (let* ((by-func (or (plist-get opts :by)
                      ;; Need to quote as if passed in to macro
                      (quote #'cdr))))
    (let ((value-holder (gensym "list-")))
      `((loopy--iteration-vars
         (,value-holder ,(if (null other-vals)
                             val
                           `(loopy--distribute-list-elements
                             ,val ,@other-vals))))
        (loopy--latter-body
         (setq ,value-holder ,(loopy--apply-function by-func value-holder)))
        (loopy--pre-conditions (consp ,value-holder))
        ,@(loopy--destructure-for-iteration-command
           var `(car ,value-holder))))))

;;;;;; List Ref
(loopy--defiteration list-ref
  "Parse the `list-ref' loop command as (list-ref VAR VAL &key by).

BY is the function to use to move through the list (default `cdr')."
  :keywords (:by)
  :instructions
  (let ((val-holder (gensym "list-ref"))
        ;; Need to quote as if passed in to macro
        (by-func (or by (quote #'cdr))))
    `((loopy--iteration-vars (,val-holder ,val))
      ,@(loopy--destructure-for-generalized-command var `(car ,val-holder))
      (loopy--latter-body
       (setq ,val-holder ,(loopy--apply-function by-func val-holder)))
      (loopy--pre-conditions (consp ,val-holder)))))

;;;;;; Map
(cl-defun loopy--parse-map-command ((_ var val))
  "Parse the `map' loop command.

Iterates through an alist of (key . value) dotted pairs,
extracted from a hash-map, association list, property list, or
vector using the library `map.el'."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'map))
  ;; Previously, this command iterated through proper lists, not dotted.  This
  ;; was changed to make it behave more like the function `map-pairs'.  We
  ;; signal a warning when a proper list is used, since now that is usually the
  ;; wrong behavior.
  (when (proper-list-p var)
    (lwarn '(loopy) :warning "Loopy: `map' iterates through dotted pairs: %s"
           var))

  (let ((value-holder (gensym "map-")))
    `((loopy--iteration-vars (,value-holder (map-pairs ,val)))
      ,@(loopy--destructure-for-iteration-command var `(car ,value-holder))
      (loopy--pre-conditions (consp ,value-holder))
      (loopy--latter-body (setq ,value-holder (cdr ,value-holder))))))

;;;;;; Map-Ref
(cl-defun loopy--parse-map-ref-command ((_ var val &key key))
  "Parse the `map-ref' command as (map-ref VAR VAL).

KEY is a variable name in which to store the current key.

Uses `map-elt' as a `setf'-able place, iterating through the
map's keys.  Duplicate keys are ignored."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'map-ref))
  (let ((key-list (gensym "map-ref-keys")))
    `((loopy--iteration-vars (,key-list (seq-uniq (map-keys ,val))))
      ,@(when key
          `((loopy--iteration-vars (,key nil))
            (loopy--main-body (setq ,key (car ,key-list)))))
      ,@(loopy--destructure-for-generalized-command
         var `(map-elt ,val ,(or key `(car ,key-list))))
      (loopy--pre-conditions (consp ,key-list))
      (loopy--latter-body (setq ,key-list (cdr ,key-list))))))

;;;;;; Nums
(loopy--defiteration nums
  "Parse the `nums' command as (nums VAR [START [END]] &key KEYS).

- START is the starting index, if given.
- END is the ending index (inclusive), if given.

KEYS is one or several of `:index', `:by', `:from', `:downfrom',
`:upfrom', `:to', `:downto', `:upto', `:above', or `:below'.

- `:by' is the increment step size as a positive value.
- `:from', `:downfrom', and `:upfrom' name the starting index
- `:to', `:downto', and `:upto' name the ending index (inclusive)
- `:below' and `:above' name an exclusive ending index.

`:downto' and `:downfrom' make the index decrease instead of increase."
  :keywords (:by :from :downfrom :upfrom :to :downto :upto :above :below)
  :required-vals 0
  :other-vals (0 1 2)
  :instructions
  ;; TODO: `cl-destructuring-bind' signals error here.  Why?
  (seq-let (explicit-start explicit-end) other-vals

    (loopy--plist-bind ( :start key-start :end key-end :by (by 1)
                         :decreasing decreasing :inclusive inclusive)

        (loopy--find-start-by-end-dir-vals opts)

      ;; Check that nothing conflicts.
      (when (or (and explicit-start key-start)
                (and explicit-end key-end))
        (error "Conflicting commands given: %s" cmd))

      (let ((increment-val-holder (gensym "nums-increment"))
            (end (or explicit-end key-end))
            (end-val-holder (gensym "nums-end"))
            (start (or explicit-start key-start 0)))

        `((loopy--iteration-vars (,var ,start))
          (loopy--iteration-vars (,increment-val-holder ,by))
          ,@(when end
              `((loopy--iteration-vars (,end-val-holder ,end))
                (loopy--pre-conditions (,(if inclusive
                                               (if decreasing #'>= #'<=)
                                             (if decreasing #'> #'<))
                                          ,var ,end-val-holder))))
          (loopy--latter-body (setq ,var ,(cond
                                             (by   `(,(if decreasing #'- #'+)
                                                     ,var ,increment-val-holder))
                                             (decreasing `(1- ,var))
                                             (t    `(1+ ,var))))))))))

;;;;;; Nums Up
(loopy--defiteration nums-up
  "Parse the `nums-up' command as (nums-up START [END] &key by).

This is for increasing indices.

- START is the starting index.
- END is the ending index (inclusive), if given.
- BY is the step size."
  :other-vals (0 1)
  :keywords (:by)
  :instructions
  (loopy--plist-bind (:by by) opts
    (loopy--parse-loop-command `(nums ,var ,val
                                      :upto ,(car other-vals)
                                      :by ,by))))

;;;;;; Nums Down
(loopy--defiteration nums-down
  "Parse the `nums-down' command as (nums-up START [END] &key by).

This is for decreasing indices.

- START is the starting index.
- END is the ending index (inclusive), if given.
- BY is the step size.  Even though the index value is decreasing,
  this should still be a positive value."
  :other-vals (0 1)
  :keywords (:by)
  :instructions
  (loopy--plist-bind (:by by) opts
    (loopy--parse-loop-command `(nums ,var ,val
                                      :downto ,(car other-vals)
                                      :by ,by))))

;;;;;; Repeat
(cl-defun loopy--parse-repeat-command ((_ var-or-count &optional count))
  "Parse the `repeat' loop command as (repeat [VAR] VAL).

VAR-OR-COUNT is a variable name or an integer.  Optional COUNT is
an integer, to be used if a variable name is provided."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'repeat))
  (if count
      `((loopy--iteration-vars (,var-or-count 0))
        (loopy--latter-body (setq ,var-or-count (1+ ,var-or-count)))
        (loopy--pre-conditions (< ,var-or-count ,count)))
    (let ((value-holder (gensym "repeat-limit-")))
      `((loopy--iteration-vars (,value-holder 0))
        (loopy--latter-body (setq ,value-holder (1+ ,value-holder)))
        (loopy--pre-conditions (< ,value-holder ,var-or-count))))))

;;;;;; Seq
(defmacro loopy--distribute-sequence-elements (&rest sequences)
  "Distribute the elements of SEQUENCES into a vector of lists.

For example, [1 2] and (3 4) give [(1 3) (1 4) (2 3) (2 4)]."
  (let ((vars (cl-loop for _ in sequences
                       collect (gensym "seq-var-")))
        (reverse-order (reverse sequences)))
    (cl-loop with expansion = `(cl-loop for ,(cl-first vars)
                                        being the elements of ,(cl-first reverse-order)
                                        do (setq result
                                                 (cons (list ,@(reverse vars))
                                                       result)))
             for var in (cl-rest vars)
             for sequence in (cl-rest reverse-order)
             do (setq expansion `(cl-loop for ,var being the elements of ,sequence
                                          do ,expansion))
             finally return `(let ((result nil))
                               ,expansion
                               (vconcat (nreverse result))))))

(loopy--defiteration seq
  "Parse the `seq' command as (seq VAR EXPR [EXPRS] &key KEYS).

KEYS is one or several of `:index', `:by', `:from', `:downfrom',
`:upfrom', `:to', `:downto', `:upto', `:above', or `:below'.

- `:index' names a variable used to store the accessed index of
  the sequence.
- `:by' is the increment step size as a positive value.
- `:from', `:downfrom', and `:upfrom' name the starting index
- `:to', `:downto', and `:upto' name the ending index (inclusive)
- `:below' and `:above' name an exclusive ending index.

`:downto' and `:downfrom' make the index decrease instead of increase.

If multiple sequence values are given, their elements are
distributed using the function `loopy--distribute-sequence-elements'."
  :keywords (:index :by :from :downfrom :upfrom :to :downto :upto :above :below)
  :other-vals t
  :instructions
  (let ((value-holder (gensym "seq-"))
        (index-holder (or (plist-get opts :index)
                          (gensym "seq-index-")))
        (end-index-holder (gensym "seq-end-index-"))
        (increment-holder (gensym "seq-increment-")))

    (loopy--plist-bind ( :start starting-index :end ending-index :by (by 1)
                         :decreasing going-down :inclusive inclusive)

        (loopy--find-start-by-end-dir-vals opts)


      `((loopy--iteration-vars (,increment-holder ,by))
        (loopy--iteration-vars
         (,value-holder ,(if other-vals
                               `(loopy--distribute-sequence-elements
                                 ,val ,@other-vals)
                             val)))
        (loopy--iteration-vars
         (,index-holder ,(or starting-index (if going-down
                                                  `(1- (length ,value-holder))
                                                0))))
        (loopy--iteration-vars
         (,end-index-holder ,(or ending-index (if going-down
                                                    -1
                                                  `(length ,value-holder)))))
        (loopy--latter-body (setq ,index-holder (,(if going-down '- '+)
                                                   ,index-holder
                                                   ,increment-holder)))

        ;; Optimize for the case of traversing from start to end, as done in
        ;; `cl-loop'.  Currently, all other case use `elt'.
        ,@(cond
           ((and (not going-down)
                 (= 1 by)
                 (or (eql 0 starting-index)
                     (null starting-index)))

            `(,@(loopy--destructure-for-iteration-command
                 var `(if (consp ,value-holder)
                          (pop ,value-holder)
                        (aref ,value-holder ,index-holder)))
              (loopy--pre-conditions
               (and ,value-holder
                      ,(if ending-index
                           `(,(if inclusive #'<= #'<)
                             ,index-holder ,end-index-holder)
                         `(or (consp ,value-holder)
                              (< ,index-holder ,end-index-holder)))))))

           (t
            `(,@(loopy--destructure-for-iteration-command
                 var `(elt ,value-holder ,index-holder))
              (loopy--pre-conditions (,(if (or (null ending-index)
                                                 (not inclusive))
                                             (if going-down '> '<)
                                           (if going-down '>= '<=))
                                        ,index-holder
                                        ,end-index-holder)))))))))

;;;;;; Seq Index
(loopy--defiteration seq-index
  "Parse the `seq-index' command as (seq-index VAR VAL &key KEYS).

KEYS is one or several of `:by', `:from', `:downfrom', `:upfrom',
`:to', `:downto', `:upto', `:above', or `:below'.

- `:by' is the increment step size as a positive value.
- `:from', `:downfrom', and `:upfrom' name the starting index
- `:to', `:downto', and `:upto' name the ending index (inclusive)
- `:below' and `:above' name an exclusive ending index.

`:downto' and `:downfrom' make the index decrease instead of increase."

  :keywords (:by :from :downfrom :upfrom :to :downto :upto :above :below)
  :instructions
  (let ((value-holder (gensym "array-"))
        (end-holder (gensym "array-end-"))
        (index-holder (gensym "seq-index-index-"))
        (increment-holder (gensym "array-increment")))

    (loopy--plist-bind ( :start key-start :end key-end :by (by 1)
                         :decreasing decreasing :inclusive inclusive)

        (loopy--find-start-by-end-dir-vals opts)

      `((loopy--iteration-vars (,var nil))
        (loopy--iteration-vars (,increment-holder ,by))
        (loopy--iteration-vars (,value-holder ,val))
        (loopy--iteration-vars (,end-holder ,(or key-end
                                                   (if decreasing
                                                       -1
                                                     `(length ,value-holder)))))
        (loopy--iteration-vars (,index-holder ,(or key-start
                                                     (if decreasing
                                                         `(1- (length ,value-holder))
                                                       0))))
        (loopy--main-body      (setq ,var ,index-holder))
        (loopy--latter-body    (setq ,index-holder (,(if decreasing #'- #'+)
                                                      ,index-holder
                                                      ,increment-holder)))
        (loopy--pre-conditions (,(if (or (null key-end)
                                           (not inclusive))
                                       (if decreasing #'> #'<)
                                     (if decreasing #'>= #'<=))
                                  ,index-holder ,end-holder))))))

;;;;;; Seq Ref
(loopy--defiteration seq-ref
  "Parse the `seq' command as (seq VAR EXPR &key KEYS).

KEYS is one or several of `:index', `:by', `:from', `:downfrom',
`:upfrom', `:to', `:downto', `:upto', `:above', or `:below'.

- `:index' names a variable used to store the accessed index of
  the sequence.
- `:by' is the increment step size as a positive value.
- `:from', `:downfrom', and `:upfrom' name the starting index
- `:to', `:downto', and `:upto' name the ending index (inclusive)
- `:below' and `:above' name an exclusive ending index.

`:downto' and `:downfrom' make the index decrease instead of increase."
  :keywords (:index :by :from :downfrom :upfrom :to :downto :upto :above :below)
  :instructions
  (let ((value-holder (gensym "seq-ref-"))
        (index-holder (or (plist-get opts :index)
                          (gensym "seq-ref-index-")))
        (end-index-holder (gensym "seq-ref-end-index-"))
        (increment-holder (gensym "seq-ref-increment-")))

    (loopy--plist-bind ( :start starting-index :end ending-index :by (by 1)
                         :decreasing going-down :inclusive inclusive)

        (loopy--find-start-by-end-dir-vals opts)

      `((loopy--iteration-vars (,increment-holder ,by))
        (loopy--iteration-vars (,value-holder ,val))
        (loopy--iteration-vars
         (,index-holder ,(or starting-index (if going-down
                                                  `(1- (length ,value-holder))
                                                0))))
        (loopy--iteration-vars
         (,end-index-holder ,(or ending-index (if going-down
                                                    -1
                                                  `(length ,value-holder)))))
        (loopy--latter-body (setq ,index-holder (,(if going-down '- '+)
                                                   ,index-holder
                                                   ,increment-holder)))
        ,@(loopy--destructure-for-generalized-command
           var `(elt ,value-holder ,index-holder))
        (loopy--pre-conditions (,(if (or (null ending-index)
                                           (not inclusive))
                                       (if going-down '> '<)
                                     (if going-down '>= '<=))
                                  ,index-holder
                                  ,end-index-holder))))))

;;;;; Accumulation
(defvar loopy--accumulation-list-end-vars)
(defvar loopy--accumulation-variable-info)

(defun loopy--get-accumulation-list-end-var (loop var)
  "Return a variable for referring to the last link in VAR in LOOP.

This function addresses that all accumulation commands
manipulating the same variable should use the same variable to
keep track of a list's last link.

This function uses `loopy--accumulation-list-end-vars' to store
end-tracking variables."
  (let ((key (cons loop var)))
    (or (alist-get key loopy--accumulation-list-end-vars nil nil #'equal)
        ;; `map-put!' would fail here, since the association doesn't exist
        ;; yet.  `setf' isn't as useful, since it first tries `map-put!'.
        (let ((tracking-var (gensym (format "%s-last-link-" var))))
          (push (cons key tracking-var)
                loopy--accumulation-list-end-vars)
          tracking-var))))

(defun loopy--check-accumulation-compatibility
    (loop-name variable category command)
  "Check accumulation command compatibility.

Known accumulation commands are listed in
`loopy--accumulation-variable-info'.

LOOP-NAME is the name of the loop in which VARIABLE accumulates.
VARIABLE is the accumulation variable.  CATEGORY is one of
`list', `string', `vector', `value', and `reverse-list'.  COMMAND
is the accumulation command.

- Strings are only made by `concat'.
- Vectors are only made by `vconcat'.
- Lists are made by commands like `append', `collect', and `union'.
- Reverse-lists are made by commands which construct lists in
  reverse for efficiency, whose normal result is a list.  This
  excludes commands like `concat' and `vconcat', and is
  unaffected by commands which coerce the type of result after
  the loop, such as `collect'."
  (unless (memq category '(list string vector value reverse-list))
    (error "Bad accumulation description: %s" category))

  (let ((key (cons loop-name variable)))
    (if-let ((existing-description
              (alist-get key loopy--accumulation-variable-info
                         nil nil #'equal)))
        (seq-let (existing-category existing-command)
            existing-description
          (unless (eq category existing-category)
            (error "Loopy: Incompatible accumulation commands:\n%s\n%s"
                   existing-command
                   command)))
      (push (cons key (list category command))
            loopy--accumulation-variable-info))))

(defun loopy--produce-collect-end-tracking (var val)
  "Produce instructions for an end-tracking accumulation of single items.

This is used in accumulation commands like `collect'.

For efficiency, accumulation command use references to track the
end location of the results list.  For larger lists, this is much
more efficient than repeatedly traversing the list."
  ;; End tracking is a bit slower than `nconc' for short lists, but much faster
  ;; for longer lists.
  (let ((last-link (loopy--get-accumulation-list-end-var loopy--loop-name var)))
    `((loopy--accumulation-vars (,last-link (last ,var)))
      (loopy--main-body
       (cond
        (,last-link
         (setcdr ,last-link (list ,val))
         (setq ,last-link (cdr ,last-link)))
        ;; Check if `var' was modified.  If so, reset
        ;; `last-link'.  If not, set `var' directly.
        (,var
         (setq ,last-link (last ,var))
         (setcdr ,last-link (list ,val))
         (setq ,last-link (cdr ,last-link)))
        (t
         (setq ,var (list ,val)
               ,last-link ,var)))))))

(defun loopy--produce-adjoin-end-tracking (var val membership-test)
  "Produce instructions for an end-tracking accumulation of single items.

This is used in accumulation commands like `collect'.

For efficiency, accumulation command use references to track the
end location of the results list.  For larger lists, this is much
more efficient than repeatedly traversing the list."
  ;; End tracking is a bit slower than `nconc' for short lists, but much faster
  ;; for longer lists.
  (let ((last-link (loopy--get-accumulation-list-end-var loopy--loop-name var)))
    `((loopy--accumulation-vars (,last-link nil))
      (loopy--main-body
       (cond
        (,membership-test nil)
        ;; If `last-link' is know, set it's cdr.
        (,last-link
         (setcdr ,last-link (list ,val))
         (setq ,last-link (cdr ,last-link)))
        ;; If `var' was updated without `last-link',
        ;; reset `last-link'.
        (,var
         (setq ,last-link (last ,var))
         (setcdr ,last-link (list ,val))
         (setq ,last-link (cdr ,last-link)))
        ;; Otherwise, set `var' and `last-link' directly.
        (t
         (setq ,var (list ,val)
               ,last-link ,var)))))))

(defun loopy--produce-multi-item-end-tracking (var val &optional destructive)
  "Produce instructions for an end-tracking accumulation of copy-joined lists.

This is used in accumulation commands like `append' and `nconc'.

For efficiency, accumulation command use references to track the
end location of the results list.  For larger lists, this is much
more efficient than repeatedly traversing the list."
  ;; End tracking is a bit slower than `nconc' for short lists, but much faster
  ;; for longer lists.
  (let ((last-link (loopy--get-accumulation-list-end-var loopy--loop-name var))
        (accum-val (if destructive val `(copy-sequence ,val))))
    `((loopy--accumulation-vars (,last-link (last ,var)))
      (loopy--main-body
       (cond
        (,last-link
         (setcdr ,last-link ,accum-val)
         (setq ,last-link (last ,last-link)))
        (,var
         (setq ,last-link (last ,var))
         (setcdr ,last-link ,accum-val)
         (setq ,last-link (last ,last-link)))
        (t
         (setq ,var ,accum-val
               ,last-link (last ,var))))))))

(defun loopy--produce-union-end-tracking
    (var val test-method &optional destructive)
  "Produce instructions for an end-tracking accumulation of modify-joined lists.

This is used in accumulation commands like `union' and `nunion'.

For efficiency, accumulation command use references to track the
end location of the results list.  For larger lists, this is much
more efficient than repeatedly traversing the list."
  ;; End tracking is a bit slower than `nconc' for short
  ;; lists, but much faster for longer lists.
  (let ((last-link (loopy--get-accumulation-list-end-var loopy--loop-name var))
        (accum-val (if destructive val `(copy-sequence ,val)))
        (new-items (gensym "new-items")))
    `((loopy--accumulation-vars (,last-link nil))
      (loopy--main-body
       (if-let ((,new-items (cl-delete-if ,test-method ,accum-val)))
           (cond
            (,last-link
             (setcdr ,last-link ,new-items)
             (setq ,last-link (last ,last-link)))
            (,var
             (setq ,last-link (last ,var))
             (setcdr ,last-link ,new-items)
             (setq ,last-link (last ,last-link)))
            (t
             (setq ,var ,new-items
                   ,last-link (last ,var)))))))))

(cl-defmacro loopy--defaccumulation (name
                                     doc-string
                                     &key
                                     (num-args 2)
                                     keywords
                                     implicit explicit )
  "Produce parsing function for an accumulation command.

- NAME is name of command.
- DOC-STRING is documentation string for the produced function.
- NUM-ARGS is number of required basic arguments
  (including variable name).
- IMPLICIT is instructions for when no variable is given.  If not
  supplied, instructions for the explicit case are used.
- EXPLICIT is instructions for the explicit case.  This argument
  is required.
- KEYWORDS is the optional key-word parameters.  This macro does
  not automate the processing of these parameters.

Each command automatically accepts an `:into' keyword argument,
which, in the implicit case, can name the variable into which to
accumulate values.  This is more similar to the syntax of
`cl-loop' and less like the convention of listing the variable
first.  This `:into' keyword need not be counted in the above
KEYWORDS.

There are several values automatically bound to variables, which
you can use in the instructions:

- `name' is the name of the command.
- `cmd' is the entire command.
- `args' is the arguments of the command that are not optional
  (except for the variable name).  These are the arguments before
  any keyword arguments.
- `var' is the variable to be used by the command.  This can be
  explicitly given, `loopy-result', or automatically generated.
- `val' is the value to be accumulated.
- `opts' is the list of optional arguments that were given.  These are the
  arguments after those described as basic by NUM-ARGS."
  (declare (indent defun) (doc-string 2))

  (unless explicit
    (error "Key-argument `explicit' not optional"))

  ;; Make sure `keywords' is a list.
  (when (nlistp keywords)
    (setq keywords (list keywords)))

  ;; Make sure `keywords' are all prefixed with a colon.
  (setq keywords (mapcar (lambda (x)
                           (if (eq ?: (aref (symbol-name x) 0))
                               x
                             (intern (format ":%s" x))))
                         keywords))

  ;; TODO: We use `ignore' to stop some errors about unused lexical variables
  ;;       when using this macro, but is there a better way?  Not all
  ;;       accumulations need use all variables.

  `(cl-defun ,(intern (format "loopy--parse-%s-command" name))
       ((&whole cmd name &rest parser-args))
     ,doc-string
     ,(let ((explicit-num-args num-args)
            (implicit-num-args (1- num-args)))
        `(let ((args)
               (opts))
           ;; Compare with `loopy' for the equivalent code:
           ;; (loopy (cons (&whole cell arg . _) parser-args)
           ;;        (until (keywordp arg))
           ;;        (collect arg)
           ;;        (finally-do
           ;;         (if cell
           ;;             (setq opts cell args loopy-result)
           ;;           (setq args parser-args))))
           (cl-loop with args-holding = nil
                    for cons-cell on parser-args
                    for arg = (car cons-cell)
                    until (keywordp arg)
                    do (push arg args-holding)
                    finally do
                    ;; If a keyword is found
                    (if cons-cell
                        (setq opts cons-cell
                              args (nreverse args-holding))
                      (setq args parser-args)))
           (ignore args opts)
           (let ((arg-length (length args)))
             (cond
              ((= arg-length ,implicit-num-args)
               (unless (loopy--only-valid-keywords-p (quote ,(cons :into keywords))
                                                     opts)
                 (error "Wrong number of arguments or wrong keywords: %s" cmd))
               (let* ((into-var (plist-get opts :into))
                      (var (or into-var
                               (and loopy--split-implied-accumulation-results
                                    (gensym (symbol-name name)))
                               'loopy-result))
                      (val (cl-first args)))
                 (ignore var val)
                 ;; Substitute in the instructions.
                 ;;
                 ;; If `:into' is used, then we must act as if this is the
                 ;; explicit case, since the variable is named and can therefore
                 ;; be accessed during the loop.
                 ,(if implicit
                      `(if into-var
                           ;; Make sure to adjust `args' for commands that
                           ;; might depend on positions, such as `find'.
                           (let ((args (cons into-var args)))
                             (ignore args)
                             ,explicit)
                         ,implicit)
                    explicit)))

              ((= arg-length ,explicit-num-args)
               ,(when keywords
                  `(unless (loopy--only-valid-keywords-p (quote ,keywords) opts)
                     (error "Wrong number of arguments or wrong keywords: %s" cmd)))
               (let ((var (cl-first args))
                     (val (cl-second args)))
                 (ignore var val)
                 (if (sequencep var)
                     ;; If we need to destructure the sequence `var', we use the
                     ;; function named by
                     ;; `loopy--destructuring-accumulation-parser' or the function
                     ;; `loopy--parse-destructuring-accumulation-command'.
                     (funcall (or loopy--destructuring-accumulation-parser
                                  #'loopy--parse-destructuring-accumulation-command)
                              cmd)
                   ;; Substitute in the instructions.
                   ,explicit)))
              (t
               (error "Wrong number of arguments or wrong keywords: %s" cmd))))))))

(defun loopy--get-union-test-method (var &optional key test)
  "Get a function testing for values in VAR in `union' and `nunion'.

This function is fed to `cl-remove-if' or `cl-delete-if'.  See
the definitions of those commands for more context.

TEST is use to check for equality (default `eql').  KEY modifies
the inputs to test."
  ;;  KEY applies to the value being tested as well as the elements in the list.
  (let ((function-arg (gensym "union-function-arg")))
    `(lambda (,function-arg)
       ,(if key
            (let ((test-val (gensym "union-test-val"))
                  (test-var (gensym "union-test-var")))
              ;; Can't rely on lexical variables around a `lambda' in
              ;; `cl-member-if', so we perform this part more manually.
              `(cl-loop with ,test-val = ,(loopy--apply-function key
                                                                 function-arg)
                        for ,test-var in ,var
                        thereis ,(loopy--apply-function
                                  (or test (quote #'eql))
                                  (loopy--apply-function key test-var)
                                  test-val)))
          `(cl-member ,function-arg ,var :test ,test)))))

;;;;;; Accumulate
(loopy--defaccumulation accumulate
  "Parse the `accumulate command' as (accumulate VAR VAL FUNC &key init)."
  :keywords (init)
  :num-args 3
  :explicit (loopy--plist-bind (:init init) opts
              (loopy--check-accumulation-compatibility loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var ,init))
                (loopy--main-body
                 (setq ,var ,(loopy--apply-function (cl-third args) val var)))))
  :implicit (loopy--plist-bind (:init init) opts
              (loopy--check-accumulation-compatibility loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var ,init))
                (loopy--main-body
                 (setq ,var ,(loopy--apply-function (cl-second args) val var)))
                (loopy--implicit-return ,var))))

;;;;;; Adjoin
(loopy--defaccumulation adjoin
  "Parse the `adjoin' command as (adjoin VAR VAL &key test key result-type at)

RESULT-TYPE can be used to `cl-coerce' the return value."
  :keywords (test key result-type at)
  ;; This is same as implicit behavior, so we only need to specify the explicit.
  :explicit
  (loopy--plist-bind ( :test test :key key :at (pos 'end)
                       :result-type (result-type 'list))
      opts
    (loopy--check-accumulation-compatibility loopy--loop-name var 'list cmd)
    `((loopy--accumulation-vars (,var nil))
      ,@(cond
         ((member pos '(start beginning 'start 'beginning))
          `((loopy--main-body
             (setq ,var (cl-adjoin ,val ,var :test ,test :key ,key)))))
         ((member pos '(end nil 'end))
          (let* ((val-is-expression (not (symbolp val)))
                 (value-holder (if val-is-expression
                                   (gensym "adjoin-value")
                                 val))
                 (membership-test
                  ;; `adjoin' applies KEY to both the new item and old items in
                  ;; list, while `member' only applies KEY to items in the list.
                  ;; To be consistent and apply KEY to all items, we use
                  ;; `cl-member-if' with a custom predicate instead.
                  (if key
                      (let ((func-arg (gensym "adjoin-func-arg")))
                        `(cl-member-if
                          (lambda (,func-arg)
                            ,(loopy--apply-function
                              (or test (quote #'eql))
                              (loopy--apply-function key func-arg)
                              (loopy--apply-function key value-holder)))
                          ,var))
                    `(cl-member ,value-holder ,var :test ,test))))
            `(,@(when val-is-expression
                  `((loopy--accumulation-vars (,value-holder nil))
                    (loopy--main-body (setq ,value-holder ,val))))
              ,@(loopy--produce-adjoin-end-tracking var value-holder
                                                    membership-test))))
         (t
          (error "Bad `:at' position: %s" cmd)))
      ,(when (and result-type (not (eq result-type 'list)))
         `(loopy--accumulation-final-updates
           (,var . (setq ,var (cl-coerce ,var
                                         (quote ,(loopy--get-quoted-symbol
                                                  result-type)))))))))
  :implicit
  (loopy--plist-bind ( :test test :key key :at (pos 'end)
                       :result-type (result-type 'list))
      opts
    `((loopy--accumulation-vars (,var nil))
      ,@(cond
         ((member pos '(start beginning 'start 'beginning))
          (loopy--check-accumulation-compatibility loopy--loop-name var 'list cmd)
          `((loopy--main-body
             (setq ,var (cl-adjoin ,val ,var :test ,test :key ,key)))))
         ((member pos '(end 'end))
          (let* ((val-is-expression (not (symbolp val)))
                 (value-holder (if val-is-expression
                                   (gensym "adjoin-value")
                                 val))
                 (membership-test
                  (if key
                      ;; `adjoin' applies KEY to both the new item
                      ;; and old items in list, while `member' only
                      ;; applies KEY to items in the list.  To be
                      ;; consistent and apply KEY to all items, we
                      ;; use `cl-member-if' with a custom predicate
                      ;; instead.
                      (let ((func-arg (gensym "adjoin-func-arg")))
                        `(cl-member-if
                          (lambda (,func-arg)
                            ,(loopy--apply-function
                              (or test (quote #'eql))
                              (loopy--apply-function key func-arg)
                              (loopy--apply-function key value-holder)))
                          ,var))
                    `(cl-member ,value-holder ,var :test ,test))))

            `(,@(when val-is-expression
                  `((loopy--accumulation-vars (,value-holder nil))
                    (loopy--main-body (setq ,value-holder ,val))))
              ,@(if loopy--split-implied-accumulation-results
                    (progn
                      (loopy--check-accumulation-compatibility
                       loopy--loop-name var 'reverse-list cmd)
                      `((loopy--main-body (if ,membership-test
                                              nil
                                            (setq ,var (cons ,value-holder ,var))))))
                  (loopy--check-accumulation-compatibility
                   loopy--loop-name var 'list cmd)
                  (loopy--produce-adjoin-end-tracking
                   var value-holder membership-test))))))
      ,(let ((reversing (and loopy--split-implied-accumulation-results
                             (member pos '(end 'end)))))
         (if (not (member result-type '(list 'list)))
             `(loopy--accumulation-final-updates
               (,var
                . (setq ,var (cl-coerce ,(if reversing `(nreverse ,var) var)
                                        (quote ,(loopy--get-quoted-symbol
                                                 result-type))))))
           (when reversing
             `(loopy--accumulation-final-updates
               (,var . (setq ,var (nreverse ,var)))))))
      (loopy--implicit-return ,var))))

;;;;;; Append
(loopy--defaccumulation append
  "Parse the `append' command as (append VAR VAL &key at)."
  :keywords (at)
  :explicit
  (loopy--plist-bind (:at (pos 'end)) opts
    (loopy--check-accumulation-compatibility loopy--loop-name var 'list cmd)
    `((loopy--accumulation-vars (,var nil))
      ,@(cond
         ;; TODO: Is there a better way of appending to the beginning
         ;;       of a list?
         ((member pos '(start beginning 'start 'beginning))
          ;; `append' doesn't copy the last argument.
          `((loopy--main-body (setq ,var (append ,val ,var)))))
         ((member pos '(end 'end))
          (loopy--produce-multi-item-end-tracking var val))
         (error "Bad `:at' position: %s" cmd))))
  :implicit
  (loopy--plist-bind (:at (pos 'end)) opts
    `((loopy--accumulation-vars (,var nil))
      ,@(cond
         ;; TODO: Is there a better way of appending to the beginning
         ;;       of a list?
         ((member pos '(start beginning 'start 'beginning))
          (loopy--check-accumulation-compatibility loopy--loop-name var 'list cmd)
          ;; `append' doesn't copy the last argument.
          `((loopy--main-body (setq ,var (append ,val ,var)))))
         ((member pos '(end 'end))
          (if loopy--split-implied-accumulation-results
              (progn
                (loopy--check-accumulation-compatibility
                 loopy--loop-name var 'reverse-list cmd)
                `((loopy--main-body (setq ,var (nconc (reverse ,val) ,var)))
                  (loopy--accumulation-final-updates
                   (,var . (setq ,var (nreverse ,var))))))
            (loopy--check-accumulation-compatibility
             loopy--loop-name var 'list cmd)
            (loopy--produce-multi-item-end-tracking var val)))
         (t
          (error "Bad `:at' position: %s" cmd)))
      (loopy--implicit-return ,var))))

;;;;;; Collect
(loopy--defaccumulation collect
  "Parse the `collect' command as (collect VAR VAL &key result-type at)."
  :keywords (result-type at)
  :explicit (loopy--plist-bind ( :at (pos (quote 'end))
                                 :result-type (result-type 'list))
                opts
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'list cmd)
              `((loopy--accumulation-vars (,var nil))
                ,@(cond
                   ((member pos '(start beginning 'start 'beginning))
                    `((loopy--main-body (setq ,var (cons ,val ,var)))))
                   ((member pos '(end 'end))
                    (loopy--produce-collect-end-tracking var val))
                   (t
                    (error "Bad `:at' position: %s" cmd)))
                ,(unless (member result-type '(list 'list))
                   `(loopy--accumulation-final-updates
                     (,var . (setq ,var
                                   (cl-coerce ,var (quote
                                                    ,(loopy--get-quoted-symbol
                                                      result-type)))))))))

  :implicit (loopy--plist-bind ( :at (pos (quote 'end))
                                 :result-type (result-type 'list))
                opts
              `((loopy--accumulation-vars (,var nil))
                ,@(cond
                   ((member pos '(start beginning 'start 'beginning))
                    (loopy--check-accumulation-compatibility
                     loopy--loop-name var 'list cmd)
                    `((loopy--main-body (setq ,var (cons ,val ,var)))))
                   ((member pos '(end 'end))
                    (if loopy--split-implied-accumulation-results
                        ;; If we can, construct the list in reverse.
                        (progn
                          (loopy--check-accumulation-compatibility
                           loopy--loop-name var 'reverse-list cmd)
                          `((loopy--main-body (setq ,var (cons ,val ,var)))))
                      (loopy--check-accumulation-compatibility
                       loopy--loop-name var 'list cmd)
                      (loopy--produce-collect-end-tracking var val)))
                   (t
                    (error "Bad `:at' position: %s" cmd)))
                ,(if (and (member pos '(end 'end))
                          loopy--split-implied-accumulation-results)
                     (if (member result-type '(list 'list))
                         `(loopy--accumulation-final-updates
                           (,var . (setq ,var (nreverse ,var))))
                       `(loopy--accumulation-final-updates
                         (,var . (setq ,var
                                       (cl-coerce (nreverse ,var)
                                                  (quote
                                                   ,(loopy--get-quoted-symbol
                                                     result-type)))))))
                   (unless (member result-type '(list 'list))
                     `(loopy--accumulation-final-updates
                       (,var . (setq ,var
                                     (cl-coerce ,var
                                                (quote
                                                 ,(loopy--get-quoted-symbol
                                                   result-type))))))))
                (loopy--implicit-return ,var))))

;;;;;; Concat
(loopy--defaccumulation concat
  "Parse the `concat' command as (concat VAR VAL &key at)."
  :keywords (at)
  :explicit (loopy--plist-bind (:at (pos 'end)) opts
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'string cmd)
              `((loopy--accumulation-vars (,var nil))
                (loopy--main-body
                 (setq ,var
                       ,(cond
                         ((member pos '(start beginning 'start 'beginning))
                          `(concat ,val ,var))
                         ((member pos '(end 'end))
                          `(concat ,var ,val))
                         (t
                          (error "Bad `:at' position: %s" cmd)))))))
  :implicit (loopy--plist-bind (:at (pos 'end)) opts
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'string cmd)
              `((loopy--accumulation-vars (,var nil))
                (loopy--main-body (setq ,var (cons ,val ,var)))
                (loopy--accumulation-final-updates
                 (,var . (setq ,var
                               ,(cond
                                 ((member pos '( start beginning
                                                 'start 'beginning))
                                  `(apply #'concat ,var))
                                 ((member pos '(end 'end))
                                  `(apply #'concat (reverse ,var)))
                                 (t
                                  (error "Bad `:at' position: %s" cmd))))))
                (loopy--implicit-return ,var))))

;;;;;; Count
(loopy--defaccumulation count
  "Parse the `count' command as (count VAR VAL)."
  ;; This is same as implicit behavior, so we only need to specify the explicit.
  :explicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var 0))
                (loopy--main-body (if ,val (setq ,var (1+ ,var))))))
  :implicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var 0))
                (loopy--main-body (if ,val (setq ,var (1+ ,var))))
                (loopy--implicit-return ,var))))

;;;;;; Find
(loopy--defaccumulation find
  "Parse a command of the form `(finding VAR EXPR TEST &key ON-FAILURE)'."
  :num-args 3
  :keywords (on-failure)
  :explicit (let* ((test-arg (cl-third args))
                   (test-form (if (loopy--quoted-form-p test-arg)
                                  `(,(loopy--get-function-symbol test-arg) ,val)
                                test-arg))
                   (on-failure (plist-get opts :on-failure))
                   (tag-name (loopy--produce-non-returning-exit-tag-name
                              loopy--loop-name)))

              `((loopy--non-returning-exit-used ,tag-name)
                (loopy--accumulation-vars (,var nil))
                (loopy--main-body (when ,test-form
                                    (setq ,var ,val)
                                    (go ,tag-name)))
                ;; If VAR nil, bind to ON-FAILURE.
                ,(when on-failure
                   `(loopy--accumulation-final-updates
                     (,var . (if ,var nil (setq ,var ,on-failure)))))))
  :implicit (let* ((test-arg (cl-second args))
                   (test-form (if (loopy--quoted-symbol-p test-arg)
                                  `(,(loopy--get-function-symbol test-arg) ,val)
                                test-arg))
                   (on-failure (plist-get opts :on-failure))
                   (tag-name (loopy--produce-non-returning-exit-tag-name
                              loopy--loop-name)))
              `((loopy--non-returning-exit-used ,tag-name)
                (loopy--accumulation-vars (,var nil))
                (loopy--main-body (when ,test-form
                                    (setq ,var ,val)
                                    (go ,tag-name)))
                ;; If VAR nil, bind to ON-FAILURE.
                ,(when on-failure
                   `(loopy--accumulation-final-updates
                     (,var . (if ,var nil (setq ,var ,on-failure)))))
                (loopy--implicit-return   ,var))))

;;;;;; Max
(loopy--defaccumulation max
  "Parse the `max' command as (max VAR VAL)."
  :explicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var -1.0e+INF))
                (loopy--main-body (setq ,var (max ,val ,var)))))
  :implicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var -1.0e+INF))
                (loopy--main-body (setq ,var (max ,val ,var)))
                (loopy--implicit-return ,var))))

;;;;;; Min
(loopy--defaccumulation min
  "Parse the `min' command as (min VAR VAL)."
  :explicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var +1.0e+INF))
                (loopy--main-body (setq ,var (min ,val ,var)))))
  :implicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var +1.0e+INF))
                (loopy--main-body (setq ,var (min ,val ,var)))
                (loopy--implicit-return ,var))))

;;;;;; Multiply
(loopy--defaccumulation multiply
  "Parse the `multiply' command as (multiply VAR VAL)."
  :explicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var 1))
                (loopy--main-body (setq ,var (* ,val ,var)))))
  :implicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var 1))
                (loopy--main-body (setq ,var (* ,val ,var)))
                (loopy--implicit-return ,var))))

;;;;;; Nconc
(loopy--defaccumulation nconc
  "Parse the `nconc' command as (nconc VAR VAL &key at)."
  :keywords (at)
  :explicit (loopy--plist-bind (:at (pos 'end)) opts
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'list cmd)
              `((loopy--accumulation-vars (,var nil))
                ,@(cond
                   ((member pos '(start beginning 'start 'beginning))
                    `((loopy--main-body (setq ,var (nconc ,val ,var)))))
                   ((member pos '(end 'end))
                    (loopy--produce-multi-item-end-tracking var val 'destructive))
                   (t
                    (error "Bad `:at' position: %s" cmd)))))
  :implicit
  (loopy--plist-bind (:at (pos 'end)) opts
    `((loopy--accumulation-vars (,var nil))
      ,@(cond
         ((member pos '(start beginning 'start 'beginning))
          (loopy--check-accumulation-compatibility
           loopy--loop-name var 'list cmd)
          `((loopy--main-body (setq ,var (nconc ,val ,var)))))
         ((member pos '(end 'end))
          (if loopy--split-implied-accumulation-results
              (progn
                (loopy--check-accumulation-compatibility
                 loopy--loop-name var 'reverse-list cmd)
                `((loopy--main-body (setq ,var (nconc (nreverse ,val) ,var)))
                  (loopy--accumulation-final-updates
                   (,var . (setq ,var (nreverse ,var))))))
            (loopy--check-accumulation-compatibility
             loopy--loop-name var 'list cmd)
            (loopy--produce-multi-item-end-tracking var val 'destructive)))
         (t
          (error "Bad `:at' position: %s" cmd)))
      (loopy--implicit-return ,var))))

;;;;;; Nunion
(loopy--defaccumulation nunion
  "Parse the `nunion' command as (nunion VAR VAL &key test key at)."
  :keywords (test key at)
  :explicit
  (loopy--plist-bind (:at (pos 'end) :key key :test test) opts
    (loopy--check-accumulation-compatibility loopy--loop-name var 'list cmd)
    `((loopy--accumulation-vars (,var nil))
      ,@(let ((test-method (loopy--get-union-test-method var key test)))
          (cond
           ((member pos '(start beginning 'start 'beginning))
            `((loopy--main-body
               (setq ,var (nconc (cl-delete-if ,test-method ,val) ,var)))))
           ((member pos '(end 'end))
            (loopy--produce-union-end-tracking var val test-method 'destructive))
           (t
            (error "Bad `:at' position: %s" cmd))))))
  :implicit
  (loopy--plist-bind (:at (pos 'end) :key key :test test) opts
    `((loopy--accumulation-vars (,var nil))
      (loopy--implicit-return ,var)
      ,@(let ((test-method (loopy--get-union-test-method var key test)))
          (cond
           ((member pos '(start beginning 'start 'beginning))
            (loopy--check-accumulation-compatibility
             loopy--loop-name var 'list cmd)
            `((loopy--main-body
               (setq ,var (nconc (cl-delete-if ,test-method ,val) ,var)))))
           ((member pos '(end 'end))
            (if loopy--split-implied-accumulation-results
                (progn
                  (loopy--check-accumulation-compatibility
                   loopy--loop-name var 'reverse-list cmd)
                  `((loopy--main-body
                     (setq ,var (nconc (nreverse (cl-delete-if ,test-method ,val))
                                       ,var)))
                    (loopy--accumulation-final-updates
                     (var . (setq ,var (nreverse ,var))))))
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'list cmd)
              (loopy--produce-union-end-tracking var val test-method 'destructive)))
           (t
            (error "Bad `:at' position: %s" cmd)))))))

;;;;;; Prepend
(loopy--defaccumulation prepend
  "Parse the `prepend' command as (prepend VAR VAL)."
  :explicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'list cmd)
              `((loopy--accumulation-vars (,var nil))
                (loopy--main-body (setq ,var (append ,val ,var)))))
  :implicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'list cmd)
              `((loopy--accumulation-vars (,var nil))
                (loopy--main-body (setq ,var (nconc ,(if (symbolp val)
                                                         `(copy-sequence  ,val)
                                                       val)
                                                    ,var)))
                (loopy--implicit-return ,var))))

;;;;;; Push Into
(loopy--defaccumulation push-into
  "Parse the `push' command as (push VAR VAL)."
  :explicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'list cmd)
              `((loopy--accumulation-vars (,var nil))
                (loopy--main-body (setq ,var (cons ,val  ,var)))))
  :implicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'list cmd)
              `((loopy--accumulation-vars (,var nil))
                (loopy--main-body (setq ,var (cons ,val  ,var)))
                (loopy--implicit-return ,var))))

;;;;;; Reduce
(loopy--defaccumulation reduce
  "Parse the `reduce' command as (reduce VAR VAL FUNC &key init).

With INIT, initialize VAR to INIT.  Otherwise, VAR starts as nil."
  :num-args 3
  :keywords (init)
  :implicit (loopy--plist-bind (:init init) opts
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var ,init))
                (loopy--main-body
                 (setq ,var ,(loopy--apply-function (cl-second args) var val)))
                (loopy--implicit-return ,var)))
  :explicit (loopy--plist-bind (:init init) opts
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var ,init))
                (loopy--main-body
                 (setq ,var ,(loopy--apply-function (cl-third args) var val))))))

;;;;;; Sum
(loopy--defaccumulation sum
  "Parse the `sum' command as (sum VAR VAL)."
  ;; This is same as implicit behavior, so we only need to specify the explicit.
  :explicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var 0))
                (loopy--main-body (setq ,var (+ ,val ,var)))))
  :implicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'value cmd)
              `((loopy--accumulation-vars (,var 0))
                (loopy--main-body (setq ,var (+ ,val ,var)))
                (loopy--implicit-return ,var))))

;;;;;; Union
(loopy--defaccumulation union
  "Parse the `union' command as (union VAR VAL &key test key at)."
  :keywords (test key at)
  :explicit
  (loopy--plist-bind (:at (pos 'end) :key key :test test) opts
    (loopy--check-accumulation-compatibility loopy--loop-name var 'list cmd)
    `((loopy--accumulation-vars (,var nil))
      ,@(let ((test-method (loopy--get-union-test-method var key test)))
          (cond
           ((member pos '(start beginning 'start 'beginning))
            `((loopy--main-body
               (setq ,var (nconc (cl-delete-if ,test-method (copy-sequence ,val))
                                 ,var)))))
           ((member pos '(end 'end))
            (loopy--produce-union-end-tracking var val test-method))
           (t
            (error "Bad `:at' position: %s" cmd))))))
  :implicit
  (loopy--plist-bind (:at (pos 'end) :key key :test test) opts
    `((loopy--accumulation-vars (,var nil))
      (loopy--implicit-return ,var)
      ,@(let ((test-method (loopy--get-union-test-method var key test)))
          (cond
           ((member pos '(start beginning 'start 'beginning))
            (loopy--check-accumulation-compatibility
             loopy--loop-name var 'list cmd)
            `((loopy--main-body
               (setq ,var
                     (nconc (cl-delete-if ,test-method (copy-sequence ,val))
                            ,var)))))
           ((member pos '(end 'end))
            (if loopy--split-implied-accumulation-results
                (progn
                  (loopy--check-accumulation-compatibility
                   loopy--loop-name var 'reverse-list cmd)
                  `((loopy--main-body
                     (setq ,var (nconc (reverse ,val) ,var)))
                    (loopy--accumulation-final-updates
                     (,var . (setq ,var (nreverse ,var))))))
              (loopy--produce-union-end-tracking var val test-method)))
           (t
            (error "Bad `:at' position: %s" cmd)))))))

;;;;;; Vconcat
(loopy--defaccumulation vconcat
  "Parse the `vconcat' command as (vconcat VAR VAL &key at)."
  :keywords (at)
  :explicit (loopy--plist-bind (:at (pos 'end)) opts
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'vector cmd)
              `((loopy--accumulation-vars (,var nil))
                (loopy--main-body
                 (setq ,var
                       ,(cond
                         ((member pos '(start beginning 'start 'beginning))
                          `(vconcat ,val ,var))
                         ((member pos '(end 'end))
                          `(vconcat ,var ,val))
                         (t
                          (error "Bad `:at' position: %s" cmd)))))))
  :implicit
  (loopy--plist-bind (:at (pos 'end)) opts
    (loopy--check-accumulation-compatibility loopy--loop-name var 'vector cmd)
    `((loopy--accumulation-vars (,var nil))
      (loopy--main-body (setq ,var (cons ,val ,var)))
      (loopy--accumulation-final-updates
       (,var . (setq ,var ,(cond
                            ((member pos '( start beginning 'start 'beginning))
                             `(apply #'vconcat ,var))
                            ((member pos '(end 'end))
                             `(apply #'vconcat (reverse ,var)))
                            (t
                             (error "Bad `:at' position: %s" cmd))))))
      (loopy--implicit-return ,var))))


;;;;; Boolean Commands
;;;;;; Always
(cl-defun loopy--parse-always-command ((_ condition &rest other-conditions))
  "Parse a command of the form `(always CONDITION [CONDITIONS])'.

If any condition is nil, `loopy' should immediately return nil.
Otherwise, `loopy' should return the final value of CONDITIONS,
or t if the command is never evaluated."
  ;; NOTE: This cannot be `gensym', as it needs to be the same for all `always'
  ;;       and `never' commands operating in the same loop.
  (let ((return-val (intern (if loopy--loop-name
                                (format "loopy--%s-always-never-return-val"
                                        loopy--loop-name)
                              "loopy-always-never-return-val"))))
    `((loopy--iteration-vars (,return-val t))
      (loopy--implicit-return ,return-val)
      (loopy--main-body (progn
			    (setq ,return-val
                                  ,(if other-conditions
                                       `(and ,condition ,@other-conditions)
                                     condition))
			    (unless ,return-val
			      (cl-return-from ,loopy--loop-name nil)))))))

;;;;;; Never
(cl-defun loopy--parse-never-command ((_ condition &rest other-conditions))
  "Parse a command of the form `(never CONDITION [CONDITIONS])'.

If any condition is t, `loopy' should immediately return nil.
Otherwise, `loopy' should return t."
  ;; NOTE: This cannot be `gensym', as it needs to be the same for all `always'
  ;;       and `never' commands operating in the same loop.
  (let ((return-val (intern (if loopy--loop-name
                                (format "loopy--%s-always-never-return-val"
                                        loopy--loop-name)
                              "loopy-always-never-return-val"))))
    `((loopy--iteration-vars (,return-val t))
      (loopy--implicit-return  ,return-val)
      (loopy--main-body (when ,(if other-conditions
                                     `(or ,condition ,@other-conditions)
                                   condition)
                            (cl-return-from ,loopy--loop-name nil))))))

;;;;;; Thereis
(cl-defun loopy--parse-thereis-command ((_ condition &rest other-conditions))
  "Parse the `thereis' command as (thereis CONDITION [CONDITIONS]).

If any condition is non-nil, its value is immediately returned
and the loop is exited.  Otherwise the loop continues and nil is
returned."
  (let ((value-holder (gensym "thereis-var-")))
    `((loopy--implicit-return  nil)
      (loopy--main-body
       (if-let ((,value-holder ,(if other-conditions
                                      `(and ,condition ,@other-conditions)
                                    condition)))
	     (cl-return-from ,loopy--loop-name ,value-holder))))))


;;;;; Exiting and Skipping
;;;;;; Leave
(cl-defun loopy--parse-leave-command (_)
  "Parse the `leave' command."
  (let ((tag-name (loopy--produce-non-returning-exit-tag-name loopy--loop-name)))
    `((loopy--non-returning-exit-used ,tag-name)
      (loopy--main-body (go ,tag-name)))))

(cl-defun loopy--parse-leave-from-command ((_ target-loop))
  "Parse the `leave-from' command."
  (loopy--check-target-loop-name target-loop)
  (let ((tag-name (loopy--produce-non-returning-exit-tag-name target-loop)))
    `((loopy--at-instructions (,target-loop
                               (loopy--non-returning-exit-used ,tag-name)))
      (loopy--main-body (go ,tag-name)))))

;;;;;; Return
(cl-defun loopy--parse-return-command ((_ &rest values))
  "Parse the `return' command as (return [VALUES])."
  `((loopy--main-body
     (cl-return-from ,loopy--loop-name
       ,(cond
         ((cl-rest values)  `(list ,@values))
         ((cl-first values) (cl-first values))
         (t                 nil))))))

(cl-defun loopy--parse-return-from-command ((_ loop-name &rest values))
  "Parse the `return-from' command as (return-from LOOP-NAME [VALUES])."
  ;; (loopy--check-target-loop-name loop-name)
  `((loopy--main-body
     (cl-return-from ,loop-name
       ,(cond
         ((cl-rest values)  `(list ,@values))
         ((cl-first values) (cl-first values))
         (t                 nil))))))

;;;;;; Skip
(cl-defun loopy--parse-skip-command (_)
  "Parse the `skip' loop command."
  (let ((tag-name (loopy--produce-skip-tag-name loopy--loop-name)))
    `((loopy--skip-used ,tag-name)
      (loopy--main-body (go ,tag-name)))))

(cl-defun loopy--parse-skip-from-command ((_ target-loop))
  "Parse the `skip-from' loop command as (skip-from LOOP-NAME)."
  (loopy--check-target-loop-name target-loop)
  (let ((tag-name (loopy--produce-skip-tag-name target-loop)))
    `((loopy--at-instructions (,target-loop
                               (loopy--skip-used ,tag-name)))
      (loopy--main-body (go ,tag-name)))))

;;;;;; While Until
(cl-defun loopy--parse-while-until-commands ((name condition &rest conditions))
  "Parse the `while' and `until' commands.

NAME is `while' or `until'.  CONDITION is a required condition.
CONDITIONS is the remaining optional conditions."
  (let ((tag-name (loopy--produce-non-returning-exit-tag-name loopy--loop-name))
        (condition (if (zerop (length conditions))
                       condition
                     `(and ,condition ,@conditions))))
    `((loopy--non-returning-exit-used ,tag-name)
      (loopy--main-body (if ,condition
                            ,@(cl-ecase name
                                (until `((go ,tag-name)))
                                (while `(nil (go ,tag-name)))))))))

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
      (push (list 'loopy--generalized-vars
                  destructuring)
            instructions))
    (nreverse instructions)))

(defalias 'loopy--destructure-generalized-variables
  #'loopy--destructure-generalized-sequence)

(defun loopy--destructure-for-iteration-default (var val)
  "Destructure VAL according to VAR.

Returns a list.  The elements are:
1. An expression which binds the variables in VAR to the values
   in VAL.
2. A list of variables which exist outside of this expression and
   need to be `let'-bound."
  (let ((bindings (loopy--destructure-sequence var val)))
    (list (cons 'setq (apply #'append bindings))
          (cl-remove-duplicates (mapcar #'cl-first bindings)))))

(defun loopy--destructure-for-iteration-command (var value-expression)
  "Destructure VALUE-EXPRESSION according to VAR for a loop command.

Note that this does not apply to commands which use generalized
variables (`setf'-able places).  For that, see the function
`loopy--destructure-for-generalized-command'.

Return a list of instructions for initializing the variables and
destructuring into them in the loop body."
  (if (symbolp var)
      `((loopy--iteration-vars (,var nil))
        (loopy--main-body (setq ,var ,value-expression)))
    (cl-destructuring-bind (destructuring-expression var-list)
        (funcall (or loopy--destructuring-for-iteration-function
                     #'loopy--destructure-for-iteration-default)
                 var value-expression)
      `((loopy--main-body ,destructuring-expression)
        ,@(mapcar (lambda (x) `(loopy--iteration-vars (,x nil)))
                  var-list)))))

(cl-defun loopy--parse-destructuring-accumulation-command
    ((name var val &rest args))
  "Return instructions for destructuring accumulation commands.

Unlike `loopy--basic-builtin-destructuring', this function
does destructuring and returns instructions.

NAME is the name of the command.  VAR is a variable name.  VAL is a value."
  (let* ((remaining-var var)
         (value-holder (gensym (format "%s-destructured-seq-" name)))
         (instructions `((loopy--iteration-vars (,value-holder nil))
                         (loopy--main-body (setq ,value-holder ,val)))))

    ;; Handle the whole var.
    (when (eq (seq-first var) '&whole)
      (dolist (instr (loopy--parse-loop-command
                      `(,name ,(seq-elt var 1) ,value-holder ,@args)))
        (push instr instructions))
      (setq remaining-var (seq-drop remaining-var 2)))

    ;; How variables are set depends on type.  For lists, we wish to use `pop'
    ;; to avoid traversing the list more than once.  For arrays, we must use
    ;; `aref'.
    (cl-etypecase remaining-var
      (symbol
       (push `(loopy--accumulation-vars (,remaining-var ,value-holder))
             instructions))
      (list
       (let ((key-vars)
             (this-var)
             (looking-at-key-vars)
             (var-is-dotted (not (proper-list-p remaining-var))))

         (while (car-safe remaining-var)

           (setq this-var (car remaining-var))
           (cond
            ((eq this-var '_)         ; Do nothing in this case.
             (push `(loopy--main-body (setq ,value-holder (cdr ,value-holder)))
                   instructions)
             (setq remaining-var (cdr remaining-var)))

            ((eq this-var '&rest)
             (setq looking-at-key-vars nil)
             (when var-is-dotted
               (error "Can't use `&rest' in dotted list: %s" var))
             (dolist (instr (loopy--parse-loop-command
                             `( ,name ,(cl-second remaining-var)
                                ,value-holder ,@args)))
               (push instr instructions))
             (setq remaining-var (cddr remaining-var)))

            ((memq this-var '(&key &keys))
             (setq looking-at-key-vars t
                   remaining-var (cdr remaining-var)))

            (looking-at-key-vars
             (push this-var key-vars)
             (setq remaining-var (cdr remaining-var)))

            (t
             (dolist (instr (loopy--parse-loop-command
                             `(,name ,this-var (pop ,value-holder) ,@args)))
               (push instr instructions))
             (setq remaining-var (cdr remaining-var)))))

         ;; If `remaining-var' is not nil, then it is now the final atom of an
         ;; improper list.
         (when remaining-var
           (dolist (instr (loopy--parse-loop-command
                           `(,name ,remaining-var ,value-holder ,@args)))
             (push instr instructions)))

         ;; TODO: In Emacs 28, `pcase' was changed so that all named variables
         ;; are at least bound to nil.  Before that version, we should make sure
         ;; that `default' is bound.
         (let ((default nil))
           (ignore default)
           (pcase-dolist ((or `(,kvar ,default)
                              kvar)
                          key-vars)
             (dolist (instr
                      (loopy--parse-loop-command
                       `( ,name ,kvar
                          ,(let ((key (intern (format ":%s" kvar))))
                             (if default
                                 `(if-let ((key-found (plist-member ,value-holder
                                                                    ,key)))
                                      (cl-second key-found)
                                    ,default)
                               `(plist-get ,value-holder ,key)))
                          ,@args)))
               (push instr instructions))))))

      (array
       (cl-loop named loop
                with array-length = (length remaining-var)
                for symbol-or-seq across remaining-var
                for index from 0
                do (cond
                    ((eq symbol-or-seq '_))
                    ((eq symbol-or-seq '&rest)
                     (let* ((next-idx (1+ index))
                            (next-var (aref remaining-var next-idx)))
                       ;; Check that the var after `&rest' is the last:
                       (when (> (1- array-length) next-idx)
                         (error "More than one variable after `&rest': %s"
                                var))

                       (dolist (instr
                                (loopy--parse-loop-command
                                 `( ,name ,next-var
                                    (cl-subseq ,value-holder ,index) ,@args)))
                         (push instr instructions)))
                     ;; Exit the loop.
                     (cl-return-from loop))
                    (t
                     (dolist (instr
                              (loopy--parse-loop-command
                               `( ,name ,symbol-or-seq
                                  (aref ,value-holder ,index) ,@args)))
                       (push instr instructions)))))))

    ;; Return the instructions in the correct order.
    (nreverse instructions)))

;;;; Selecting parsers
(defun loopy--parse-loop-command (command)
  "Parse COMMAND, returning a list of instructions in the same received order.

To allow for some flexibility in the command parsers, any nil
instructions are removed.

This function gets the parser, and passes the command to that parser."
  (let ((parser (loopy--get-command-parser (cl-first command))))
    (if-let ((instructions (funcall parser command)))
        (remq nil instructions)
      (error "Loopy: No instructions returned by command parser: %s"
             parser))))

;; TODO: Allow for commands to return single instructions, instead of requiring
;; list of instructions.
(defun loopy--parse-loop-commands (command-list)
  "Parse commands in COMMAND-LIST via `loopy--parse-loop-command'.
Return a single list of instructions in the same order as
COMMAND-LIST."
  (mapcan #'loopy--parse-loop-command command-list))

(defun loopy--get-command-parser (command-name)
  "Get the parsing function for COMMAND-NAME.

The following variables are checked:

1. `loopy-command-aliases'
2. `loopy-command-parsers'

Failing that, an error is signaled."
  (if-let ((alias-def (map-elt loopy-command-aliases command-name)))
      ;; Allow for recursive aliases.
      (loopy--get-command-parser alias-def)
    (or (map-elt loopy-command-parsers command-name)
        (signal 'loopy-unknown-command command-name))))

(provide 'loopy-commands)

;;; loopy-commands.el ends here
