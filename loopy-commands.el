;;; loopy-commands.el --- Loop commands for loopy -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: February 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.4
;; Package-Requires: ((emacs "27.1") (loopy "0.4"))
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
;; Cant require `loopy', as that would be recursive.
(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'map)

(declare-function loopy--bound-p "loopy")
(declare-function loopy--basic-builtin-destructuring "loopy")
(defvar loopy--in-sub-level)

;;;; Variables from flags
(defvar loopy--destructuring-accumulation-parser)
(defvar loopy--split-implied-accumulation-results)
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
  (if (symbolp function-form)
      function-form
    (cl-case (car function-form)
      ((function quote) (cadr function-form))
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
  "Whether form is quoted via `quote' or `function'.

If not, then it is possible that FORM is a variable."
  (and (listp form-or-symbol)
       (= 2 (length form-or-symbol))
       (or (eq (car form-or-symbol) 'quote)
           (eq (car form-or-symbol) 'function))))

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
      (if (eq (car instruction) 'loopy--main-body)
          (push (cdr instruction) wrapped-main-body)
        (push instruction other-instructions)))

    ;; Return the sub-lists.
    (list (nreverse wrapped-main-body) (nreverse other-instructions))))

;; Wrapper to make sure that the output sequence is of the same type.
(cl-defun loopy--substitute-using (new seq &key test)
  "Copy SEQ, substituting elements using output of NEW.

NEW receives the element as its only argument.

If given predicate TEST, replace only elements satisfying TEST.
This testing could also be done in NEW."
  (cl-map (if (cl-typep seq 'list) 'list 'array)
          (if test
              (lambda (x)
                (if (funcall test x)
                    (funcall new x)
                  x))
            (lambda (x) (funcall new x)))
          seq))

(cl-defun loopy--substitute-using-if (new test seq)
  "Copy SEQ, substituting elements satisfying TEST using output of NEW.

NEW receives the element as its only argument.

Unlike `loopy--substitute-using', the test is required."
  (loopy--substitute-using new seq :test test))

(defun loopy--every-other (list)
  "Return a list of every other element in LIST, starting with the first.

This is helpful when working with property lists."
  (cl-loop for i in list by #'cddr collect i))

(defun loopy--valid-keywords-p (correct list)
  "Check that LIST contains only valid keywords in every other position.

Any keyword not in CORRECT is considered invalid.

CORRECT is a list of valid keywords.  The first item in LIST is
assumed to be a keyword."
  (null (cl-set-difference (loopy--every-other list) correct)))

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
  (let* ((length-vals (length vals))
         (init-arg (when (eq (nth (- length-vals 2) vals)
                             ':init)
                     (nth (1- length-vals) vals))))
    (let ((arg-length (if init-arg
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
                   var (car vals)))
               ;; If two values, repeatedly check against `value-selector' to
               ;; determine if we should assign the first or second value.  This
               ;; is how `cl-loop' does it.
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
        (if init-arg
            (loopy--substitute-using-if
             (cl-function (lambda ((_ . (var _)))
                            `(loopy--iteration-vars
                              . (,var ,init-arg))))
             (lambda (x) (eq (car x) 'loopy--iteration-vars))
             needed-instructions)
          needed-instructions)))))

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
            `((loopy--iteration-vars . (,init-value-holder ,init))
              ;; When using destructuring, each variable in `holding-vars'
              ;; needs to be initialized to a value that can be destructured
              ;; according to VAR.
              ;;
              ;; We only want to calculate the initial value once, even for the
              ;; destructuring, so we require two holding variables.
              ,@(if using-destructuring
                    `((loopy--iteration-vars
                       . (,init-destr-value-holder
                          (loopy--mimic-init-structure
                           (quote ,var) ,init-value-holder)))
                      ,@(mapcar (lambda (x)
                                  `(loopy--iteration-vars
                                    . (,x ,init-destr-value-holder)))
                                holding-vars))
                  (mapcar (lambda (x)
                            `(loopy--iteration-vars . (,x ,init-value-holder)))
                          holding-vars)))
          (mapcar (lambda (x)
                    `(loopy--iteration-vars . (,x nil)))
                  holding-vars))
      ,@(loopy--substitute-using
         (pcase-lambda ((and instr `(,place . (,var ,_))))
           (if (eq place 'loopy--iteration-vars)
               `(,place . (,var ,(when init
                                   init-value-holder)))
             instr))
         (loopy--destructure-for-iteration-command
          var (car (last holding-vars))))
      (loopy--latter-body
       . (setq ,@(apply #'append
                        (nreverse (cl-loop for pvar = val then hv
                                           for hv in holding-vars
                                           collect (list hv pvar)))))))))

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

(cl-defun loopy--parse-map-command ((_ var val))
  "Parse the `map' loop command.

Iterates through an alist of (key value) un-dotted pairs,
extracted from a hash-map, association list, property list, or
vector using the library `map.el'."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'map))
  (let ((value-holder (gensym "map-")))
    `((loopy--iteration-vars . (,value-holder (map-apply #'list ,val)))
      ,@(loopy--destructure-for-iteration-command var `(car ,value-holder))
      (loopy--pre-conditions . (consp ,value-holder))
      (loopy--latter-body . (setq ,value-holder (cdr ,value-holder))))))

(cl-defun loopy--parse-nums-command ((&whole cmd _ var start &rest args))
  "Parse the `nums' command as (nums VAR START [END] &key BY DOWN).

If END is given, end the loop when the value of VAR is greater
than END.  BY is the positive value used to increment VAR from
START to END.  IF DOWN is given, end the loop when the value of
VAR is less than END."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'nums))

  ;; Verify args
  (when args
    (unless (and (if (keywordp (cl-first args))
                     (cl-member (length args) '(2 4) :test #'=)
                   (cl-member (length args) '(1 3 5) :test #'=))
                 (loopy--valid-keywords-p '(:by :down)
                                           (if (keywordp (cl-first args))
                                               args
                                             (cl-rest args))))
      (error "Bad arguments to `nums': %s" cmd)))


  (let ((end (unless (keywordp (cl-first args))
               (cl-first args))))
    (let ((down (plist-get (if end (cdr args) args) :down))
          (by   (plist-get (if end (cdr args) args) :by))
          (increment-val-holder (gensym "nums-increment")))

      `((loopy--iteration-vars . (,var ,start))
        ,(when by
           `(loopy--iteration-vars . (,increment-val-holder ,by)))
        ,(when end
           `(loopy--pre-conditions . (,(if down #'>= #'<=)
                                      ,var ,end)))
        (loopy--latter-body . (setq ,var ,(cond
                                           (by   `(,(if down #'- #'+)
                                                   ,var ,increment-val-holder))
                                           (down `(1- ,var))
                                           (t    `(1+ ,var)))))))))

(cl-defun loopy--parse-nums-up-command ((&whole cmd _ var start &rest args))
  "Parse the `nums-up' command as (nums-up START [END] &key by)."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'nums-up))

  (when args
    (let ((end-given (not (keywordp (cl-first args)))))
      (unless (and (if end-given
                       (cl-member (length args) '(1 3) :test #'=)
                     (= 2 (length args)))
                   (loopy--valid-keywords-p '(:by)
                                             (if end-given
                                                 (cl-rest args)
                                               args)))
        (error "Bad arguments to `nums-up': %s" cmd))))

  (loopy--parse-loop-command `(nums ,var ,start ,@args)))


(cl-defun loopy--parse-nums-down-command ((&whole cmd _ var start &rest args))
  "Parse the `nums-down' command as (nums-up START [END] &key by)."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'nums-up))

  (when args
    (let ((end-given (not (keywordp (cl-first args)))))
      (unless (and (if end-given
                       (cl-member (length args) '(1 3) :test #'=)
                     (= 2 (length args)))
                   (loopy--valid-keywords-p '(:by)
                                             (if end-given
                                                 (cl-rest args)
                                               args)))
        (error "Bad arguments to `nums-down': %s" cmd))))

  (loopy--parse-loop-command `(nums ,var ,start ,@args :down t)))


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

(cl-defun loopy--parse-seq-index-command ((_ var val))
  "Parse the `seq-index' command.

VAR is a variable name.  VAL is an array value.

This command does not support destructuring."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter 'seq-index))
  (let ((value-holder (gensym "seq-index-val"))
        (index-holder (gensym "seq-index-index-"))
        (length-holder (gensym "seq-index-val-length-")))
    `((loopy--iteration-vars . (,value-holder ,val))
      (loopy--iteration-vars . (,index-holder 0))
      (loopy--iteration-vars . (,length-holder (length ,value-holder)))
      (loopy--iteration-vars . (,var nil))
      (loopy--main-body . (setq ,var ,index-holder))
      (loopy--latter-body    . (setq ,index-holder (1+ ,index-holder)))
      (loopy--pre-conditions . (< ,index-holder ,length-holder)))))

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
    ((multiply multiplying) 1)
    ((max maxing maximize maximizing)
     -1.0e+INF)
    ((min minning minimize minimizing)
     +1.0e+INF)
    (t nil)))

;;;;; Accumulation
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
- `args' is the arguments of the command.
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
       ((&whole cmd name &rest args))
     ,doc-string
     ,(let* ((max-plist-length (+ (* 2 (length keywords)) 2)) ; Add 2 for `:into'.
             (implicit-num-args (1- num-args))
             (implicit-args-nums (number-sequence implicit-num-args
                                                  (+ implicit-num-args
                                                     max-plist-length)
                                                  2))
             (explicit-args-nums (number-sequence num-args
                                                  (+ num-args
                                                     max-plist-length)
                                                  2)))
        `(let ((arg-length (length args)))
           (cond
            ;; Implicit
            ((cl-member arg-length (quote ,implicit-args-nums) :test #'=)
             (let ((opts (nthcdr ,(1- num-args) args))
                   (val (cl-first args)))
               ;; Validate any keyword arguments:
               (when (cl-set-difference (loopy--every-other opts)
                                        (quote ,(cons :into keywords)))
                 (error "Wrong number of arguments or wrong keywords: %s" cmd))

               (let* ((into-var (plist-get opts :into))
                      (var (or into-var
                               (if loopy--split-implied-accumulation-results
                                   (gensym (symbol-name name))
                                 'loopy-result))))
                 (ignore var val)
                 ;; Substitute in the instructions.
                 ;;
                 ;; If `:into' is used, then we must act as if this is the
                 ;; explicit case, since the variable is named and can therefore
                 ;; be accessed during the loop.
                 ,(if implicit
                      `(if into-var
                           ,explicit
                         ,implicit)
                    explicit))))
            ;; Explicit
            ((cl-member arg-length (quote ,explicit-args-nums) :test #'=)
             (let ((var (cl-first args))
                   (val (cl-second args))
                   (opts (nthcdr ,num-args args)))

               ;; Validate any keyword arguments:
               (when (cl-set-difference (loopy--every-other opts)
                                        (quote ,(cons :into keywords)))
                 (error "Wrong number of arguments or wrong keywords: %s" cmd))

               (ignore var val)
               (if (sequencep var)
                   (loopy--parse-destructuring-accumulation-command cmd)
                 ;; Substitute in the instructions.
                 ,explicit)))
            (t
             (error "Wrong number of command arguments: %s" cmd)))))))

(defun loopy--get-union-test-method (var &optional key test)
  "Get a function testing for values in VAR in `union' and `nunion'.

This function is fed to `cl-remove-if' or `cl-delete-if'.  See
the definitions of those commands for more context.

TEST is use to check for equality (default `eql').  KEY modifies
the inputs to test."
  ;;  KEY applies to the value being tested as well as the elements in the list.
  `(lambda
     (x)
     ,(if key
          ;; TODO: Would it be better to simplify this code to always use
          ;;       `funcall'?  Would it help avoid errors?
          (let ((quoted-key (loopy--quoted-form-p key))
                (key-fn (loopy--get-function-symbol key))
                (test-val (gensym "union-test-val")))
            ;; Can't rely on lexical variables around a `lambda' in
            ;; `cl-member-if', so we perform this part more manually.
            `(let ((,test-val ,(if quoted-key
                                   `(,key-fn x)
                                 `(funcall ,key-fn x))))
               (cl-dolist (y ,var)
                 (if (,@(if test
                            (if (loopy--quoted-form-p test)
                                (list (loopy--get-function-symbol test))
                              `(funcall ,test))
                          '(eql))
                      ,(if quoted-key
                           `(,key-fn y)
                         `(funcall ,key y))
                      ,test-val)
                     (cl-return y)))))
        `(cl-member x ,var :test ,test))))

(loopy--defaccumulation accumulate
  "Parse the `accumulate command' as (accumulate VAR VAL FUNC &key init)."
  :keywords (init)
  :num-args 3
  :explicit `((loopy--accumulation-vars
               . (,var ,(if-let ((init (plist-get opts :init)))
                            init
                          (loopy--accumulation-starting-value name))))
              (loopy--main-body . (setq ,var (funcall ,(cl-third args)
                                                      ,val ,var)))
              (loopy--implicit-return . ,var))
  :implicit `((loopy--accumulation-vars
               . (,var ,(if-let ((init (plist-get opts :init)))
                            init
                          (loopy--accumulation-starting-value name))))
              (loopy--main-body . (setq ,var (funcall ,(cl-second args)
                                                      ,val ,var)))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation adjoin
  "Parse the `adjoin' command as (adjoin VAR VAL &key test key result-type at)

RESULT-TYPE can be used to `cl-coerce' the return value."
  :keywords (test key result-type at)
  ;; This is same as implicit behavior, so we only need to specify the explicit.
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              ,@(let ((test (plist-get opts :test))
                      (key (plist-get opts :key))
                      (pos (plist-get opts :at)))
                  (cond
                   ((member pos '(start beginning 'start 'beginning))
                    `((loopy--main-body
                       . (setq ,var (cl-adjoin ,val ,var :test ,test :key ,key)))))
                   ((member pos '(end nil 'end))
                    ;; If `val' is an expression, we need another temporary
                    ;; variable to hold that value.
                    (let* ((list-end (gensym "adjoin-list-end"))
                           (val-is-expression (listp val))
                           (value-holder (if val-is-expression
                                             (gensym "adjoin-value")
                                           val)))
                      `((loopy--accumulation-vars . (,list-end (last ,var)))
                        ,@(when val-is-expression
                            `((loopy--accumulation-vars . (,value-holder nil))
                              (loopy--main-body . (setq ,value-holder ,val))))
                        (loopy--main-body
                         . (if ,(if key
                                    ;; `adjoin' applies KEY to both the new item
                                    ;; and old items in list, while `member' only
                                    ;; applies KEY to items in the list.  To be
                                    ;; consistent and apply KEY to all items, we
                                    ;; use `cl-member-if' with a custom predicate
                                    ;; instead.
                                    `(cl-member-if
                                      ;; TODO: Simplify this by just using
                                      ;;       `funcall' always?  Would that be
                                      ;;       less error prone?  Affect speed?
                                      (lambda (x)
                                        (,@(if test
                                               (if (loopy--quoted-form-p test)
                                                   (list (loopy--get-function-symbol test))
                                                 `(funcall ,test))
                                             '(eql))
                                         ,@(if (loopy--quoted-form-p key)
                                               (let ((key-fn (loopy--get-function-symbol key)))
                                                 `((,key-fn x)
                                                   (,key-fn ,value-holder)))
                                             `((funcall ,key x)
                                               (funcall ,key ,value-holder)))))
                                      ,var)
                                  `(cl-member ,value-holder ,var :test ,test))
                               nil
                             (if ,list-end
                                 (progn
                                   (setcdr ,list-end  (list ,value-holder))
                                   (setq ,list-end (cdr ,list-end)))
                               (setq ,var (list ,value-holder)
                                     ,list-end ,var)))))))
                   (t
                    (error "Bad `:at' position: %s" cmd))))
              ,(let ((result-type (plist-get opts :result-type)))
                 (when (and result-type
                            (not (eq result-type 'list)))
                   `(loopy--accumulation-final-updates
                     . (,var . (setq ,var
                                     (cl-coerce ,var (quote
                                                      ,(loopy--get-quoted-symbol
                                                        result-type))))))))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation append
  "Parse the `append' command as (append VAR VAL &key at)."
  :keywords (at)
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body
               . (setq ,var ,(let ((pos (plist-get opts :at)))
                               (cond
                                ((member pos '(start beginning 'start 'beginning))
                                 `(append ,val ,var))
                                ((member pos '(end nil 'end))
                                 `(append ,var ,val))
                                (t
                                 (error "Bad `:at' position: %s" cmd))))))
              (loopy--implicit-return . ,var))
  :implicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              ,@(let ((pos (plist-get opts :at)))
                  (cond
                   ;; TODO: Is there a better way of appending to the beginning
                   ;;       of a list?
                   ((member pos '(start beginning 'start 'beginning))
                    `((loopy--main-body
                       . (setq ,var (nconc ,(if (symbolp val)
                                                `(copy-sequence  ,val)
                                              val)
                                           ,var)))))
                   ((member pos '(end nil 'end))
                    `((loopy--main-body
                       . (setq ,var (nconc (reverse ,val) ,var)))
                      (loopy--accumulation-final-updates
                       . (,var . (setq ,var (nreverse ,var))))))
                   (t
                    (error "Bad `:at' position: %s" cmd))))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation collect
  "Parse the `collect' command as (collect VAR VAL &key result-type at)."
  :keywords (result-type at)
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body
               . (setq ,var ,(let ((pos (plist-get opts :at)))
                               (cond
                                ((member pos '(start beginning 'start 'beginning))
                                 `(cons ,val ,var))
                                ((member pos '(end nil 'end))
                                 `(append ,var (list ,val)))
                                (t
                                 (error "Bad `:at' position: %s" cmd))))))
              ,(let ((result-type (plist-get opts :result-type)))
                 (if (and result-type
                          (not (eq result-type 'list)))
                     `(loopy--accumulation-final-updates
                       . (,var . (setq ,var
                                       (cl-coerce ,var (quote
                                                        ,(loopy--get-quoted-symbol
                                                          result-type))))))))
              (loopy--implicit-return . ,var))

  :implicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body
               ;; This works for all `:at' positions.  If position is `end',
               ;; then we just need to reverse for a final update.
               . (setq ,var (cons ,val ,var)))
              ;; The final update of VAR (if any) depends on both `:at' and
              ;; `:result-type'.
              ,(let ((pos (plist-get opts :at))
                     (result-type (plist-get opts :result-type)))
                 (cond
                  ((member pos '(start beginning 'start 'beginning))
                   (if (and result-type
                            (not (eq result-type 'list)))
                       `(loopy--accumulation-final-updates
                         . (,var . (setq ,var
                                         (cl-coerce ,var (quote
                                                          ,(loopy--get-quoted-symbol
                                                            result-type))))))))
                  ((member pos '(end nil 'end))
                   (if (and result-type
                            (not (eq result-type 'list)))
                       `(loopy--accumulation-final-updates
                         . (,var . (setq ,var
                                         (cl-coerce (nreverse ,var)
                                                    (quote ,(loopy--get-quoted-symbol
                                                             result-type))))))
                     `(loopy--accumulation-final-updates
                       . (,var . (setq ,var (nreverse ,var))))))
                  (t
                   (error "Bad `:at' position: %s" cmd))))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation concat
  "Parse the `concat' command as (concat VAR VAL &key at)."
  :keywords (at)
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body
               . (setq ,var
                       ,(let ((pos (plist-get opts :at)))
                          (cond
                           ((member pos '(start beginning 'start 'beginning))
                            `(concat ,val ,var))
                           ((member pos '(end nil 'end))
                            `(concat ,var ,val))
                           (t
                            (error "Bad `:at' position: %s" cmd))))))

              (loopy--implicit-return . ,var))
  :implicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body . (setq ,var (cons ,val ,var)))
              (loopy--accumulation-final-updates
               . (,var . (setq ,var
                               ,(let ((pos (plist-get opts :at)))
                                  (cond
                                   ((member pos '(start beginning 'start 'beginning))
                                    `(apply #'concat ,var))
                                   ((member pos '(end nil 'end))
                                    `(apply #'concat (reverse ,var)))
                                   (t
                                    (error "Bad `:at' position: %s" cmd)))))))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation count
  "Parse the `count' command as (count VAR VAL)."
  ;; This is same as implicit behavior, so we only need to specify the explicit.
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body . (if ,val (setq ,var (1+ ,var))))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation find
  "Parse a command of the form `(finding EXPR TEST &key ON-FAILURE)'."
  :num-args 3
  :keywords (on-failure)
  :explicit (let* ((test-arg (cl-third args))
                   (test-form (if (loopy--quoted-form-p test-arg)
                                  `(,(loopy--get-function-symbol test-arg) ,val)
                                test-arg))
                   (on-failure (plist-get opts :on-failure)))
              `((loopy--tagbody-exit-used . t)
                (loopy--accumulation-vars . (,var nil))
                (loopy--main-body . (when ,test-form
                                      (setq ,var ,val)
                                      (go loopy--non-returning-exit-tag)))
                ;; If VAR nil, bind to ON-FAILURE.
                ,(when on-failure
                   `(loopy--accumulation-final-updates
                     . (,var . (if ,var nil (setq ,var ,on-failure)))))
                (loopy--implicit-return   . ,var)))
  :implicit (let* ((test-arg (cl-second args))
                   (test-form (if (loopy--quoted-form-p test-arg)
                                  `(,(loopy--get-function-symbol test-arg) ,val)
                                test-arg))
                   (on-failure (plist-get opts :on-failure)))
              `((loopy--tagbody-exit-used . t)
                (loopy--accumulation-vars . (,var nil))
                (loopy--main-body . (when ,test-form
                                      (setq ,var ,val)
                                      (go loopy--non-returning-exit-tag)))
                ;; If VAR nil, bind to ON-FAILURE.
                ,(when on-failure
                   `(loopy--accumulation-final-updates
                     . (,var . (if ,var nil (setq ,var ,on-failure)))))
                (loopy--implicit-return   . ,var))))

(loopy--defaccumulation max
  "Parse the `max' command as (max VAR VAL)."
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body . (setq ,var (max ,val ,var)))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation min
  "Parse the `min' command as (min VAR VAL)."
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body . (setq ,var (min ,val ,var)))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation multiply
  "Parse the `multiply' command as (multiply VAR VAL)."
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body . (setq ,var (* ,val ,var)))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation nconc
  "Parse the `nconc' command as (nconc VAR VAL &key at)."
  :keywords (at)
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body
               . (setq ,var
                       ,(let ((pos (plist-get opts :at)))
                          (cond
                           ((member pos '(start beginning 'start 'beginning))
                            `(nconc ,val ,var))
                           ((member pos '(end nil 'end))
                            `(nconc ,var ,val))
                           (t
                            (error "Bad `:at' position: %s" cmd))))))
              (loopy--implicit-return . ,var))
  :implicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              ,@(let ((pos (plist-get opts :at)))
                  (cond
                   ((member pos '(start beginning 'start 'beginning))
                    `((loopy--main-body . (setq ,var (nconc ,val ,var)))))
                   ((member pos '(end nil 'end))
                    `((loopy--main-body . (setq ,var (nconc (nreverse ,val) ,var)))
                      (loopy--accumulation-final-updates
                       . (,var . (setq ,var (nreverse ,var))))))
                   (t
                    (error "Bad `:at' position: %s" cmd))))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation nunion
  "Parse the `nunion' command as (nunion VAR VAL &key test key at)."
  :keywords (test key at)
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              ,@(let ((pos (plist-get opts :at))
                      (key (plist-get opts :key))
                      (test (plist-get opts :test)))
                  (let ((test-method (loopy--get-union-test-method var key test)))
                    (cond
                     ((member pos '(start beginning 'start 'beginning))
                      `((loopy--main-body
                         . (setq ,var (nconc (cl-delete-if ,test-method ,val)
                                             ,var)))))
                     ((member pos '(end nil 'end))
                      (let ((list-end (gensym "union-end"))
                            (new-items (gensym "new-union-items")))
                        `((loopy--accumulation-vars . (,list-end (last ,var)))
                          (loopy--main-body
                           . (if-let ((,new-items
                                       (cl-delete-if ,test-method ,val)))
                                 (if ,list-end
                                     (progn
                                       (setcdr ,list-end ,new-items)
                                       (setq ,list-end (last ,list-end)))
                                   (setq ,var ,new-items
                                         ,list-end (last ,var))))))))
                     (t
                      (error "Bad `:at' position: %s" cmd)))))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation prepend
  "Parse the `prepend' command as (prepend VAR VAL)."
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body . (setq ,var (append ,val ,var)))
              (loopy--implicit-return . ,var))
  :implicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body . (setq ,var (nconc ,(if (symbolp val)
                                                         `(copy-sequence  ,val)
                                                       val)
                                                    ,var)))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation push-into
  "Parse the `push' command as (push VAR VAL)."
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body . (setq ,var (cons ,val  ,var)))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation reduce
  "Parse the `reduce' command as (reduce VAR VAL FUNC &key init).

With INIT, initialize VAR to INIT.  Otherwise, VAR starts as nil."
  :num-args 3
  :keywords (init)
  :implicit `((loopy--accumulation-vars
               . (,var ,(if-let ((init (plist-get opts :init)))
                            init
                          (loopy--accumulation-starting-value name))))
              (loopy--main-body . (setq ,var
                                        (funcall ,(cl-second args) ,var ,val)))
              (loopy--implicit-return . ,var))
  :explicit `((loopy--accumulation-vars
               . (,var ,(if-let ((init (plist-get opts :init)))
                            init
                          (loopy--accumulation-starting-value name))))
              (loopy--main-body . (setq ,var
                                        (funcall ,(cl-third args) ,var ,val)))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation sum
  "Parse the `sum' command as (sum VAR VAL)."
  ;; This is same as implicit behavior, so we only need to specify the explicit.
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body . (setq ,var (+ ,val ,var)))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation union
  "Parse the `union' command as (union VAR VAL &key test key at)."
  :keywords (test key at)
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              ,@(let ((pos (plist-get opts :at))
                      (key (plist-get opts :key))
                      (test (plist-get opts :test)))
                  (let ((test-method (loopy--get-union-test-method var key test)))
                    (cond
                     ((member pos '(start beginning 'start 'beginning))
                      `((loopy--main-body
                         . (setq ,var (nconc (cl-remove-if ,test-method ,val)
                                             ,var)))))
                     ((member pos '(end nil 'end))
                      (let ((list-end (gensym "union-end"))
                            (new-items (gensym "new-union-items")))
                        `((loopy--accumulation-vars . (,list-end (last ,var)))
                          (loopy--main-body
                           . (if-let ((,new-items
                                       (cl-remove-if ,test-method ,val)))
                                 (if ,list-end
                                     (progn
                                       (setcdr ,list-end ,new-items)
                                       (setq ,list-end (last ,list-end)))
                                   (setq ,var ,new-items
                                         ,list-end (last ,var))))))))
                     (t
                      (error "Bad `:at' position: %s" cmd)))))
              (loopy--implicit-return . ,var)))

(loopy--defaccumulation vconcat
  "Parse the `vconcat' command as (vconcat VAR VAL &key at)."
  :keywords (at)
  :explicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body
               . (setq ,var
                       ,(let ((pos (plist-get opts :at)))
                          (cond
                           ((member pos '(start beginning 'start 'beginning))
                            `(vconcat ,val ,var))
                           ((member pos '(end nil 'end))
                            `(vconcat ,var ,val))
                           (t
                            (error "Bad `:at' position: %s" cmd))))))

              (loopy--implicit-return . ,var))
  :implicit `((loopy--accumulation-vars
               . (,var ,(loopy--accumulation-starting-value name)))
              (loopy--main-body . (setq ,var (cons ,val ,var)))
              (loopy--accumulation-final-updates
               . (,var . (setq ,var
                               ,(let ((pos (plist-get opts :at)))
                                  (cond
                                   ((member pos '(start beginning 'start 'beginning))
                                    `(apply #'vconcat ,var))
                                   ((member pos '(end nil 'end))
                                    `(apply #'vconcat (reverse ,var)))
                                   (t
                                    (error "Bad `:at' position: %s" cmd)))))))
              (loopy--implicit-return . ,var)))


;;;; Boolean Commands
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
    `((loopy--iteration-vars . (,return-val t))
      (loopy--implicit-return . ,return-val)
      (loopy--main-body . (progn
			    (setq ,return-val
                                  ,(if other-conditions
                                       `(and ,condition ,@other-conditions)
                                     condition))
			    (unless ,return-val
			      (cl-return-from ,loopy--loop-name nil)))))))

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
    `((loopy--iteration-vars . (,return-val t))
      (loopy--implicit-return  . ,return-val)
      (loopy--main-body . (when ,(if other-conditions
                                     `(or ,condition ,@other-conditions)
                                   condition)
                            (cl-return-from ,loopy--loop-name nil))))))

(cl-defun loopy--parse-thereis-command ((_ condition &rest other-conditions))
  "Parse a command of the form `(thereis CONDITION [CONDITIONS]).'

If any condition is non-nil, its value is immediately returned and the loop is exited.
Otherwise the loop continues and nil is returned."
  (let ((value-holder (gensym "thereis-var-")))
    `((loopy--implicit-return  . nil)
      (loopy--main-body
       . (if-let ((,value-holder ,(if other-conditions
                                      `(and ,condition ,@other-conditions)
                                    condition)))
	     (cl-return-from ,loopy--loop-name ,value-holder))))))


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

;; Note that function `loopy--basic-builtin-destructuring' is defined in
;; 'loop.el', as it is also used for `with' variables.
(defun loopy--destructure-for-iteration-default (var val)
  "Destructure VAL according to VAR.

Returns a list.  The elements are:
1. An expression which binds the variables in VAR to the values
   in VAL.
2. A list of variables which exist outside of this expression and
   need to be `let'-bound."
  (let ((bindings (loopy--basic-builtin-destructuring var val)))
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
      `((loopy--iteration-vars . (,var nil))
        (loopy--main-body . (setq ,var ,value-expression)))
    (cl-destructuring-bind (destructuring-expression var-list)
        (funcall (or loopy--destructuring-for-iteration-function
                     #'loopy--destructure-for-iteration-default)
                 var value-expression)
      `((loopy--main-body . ,destructuring-expression)
        ,@(mapcar (lambda (x) `(loopy--iteration-vars . (,x nil)))
                  var-list)))))

(cl-defun loopy--parse-destructuring-accumulation-command
    ((name var val &rest args))
  "Return instructions for destructuring accumulation commands.

Unlike `loopy--basic-builtin-destructuring', this function
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
           (push (loopy--parse-loop-command
                  `(,name ,symbol-or-seq (pop ,value-holder)
                          ,@args))
                 instructions))

         ;; Decide what to do for final assignment.
         (push (loopy--parse-loop-command
                `(,name ,(cl-first normalized-reverse-var)
                        ,(if is-proper-list
                             `(pop ,value-holder)
                           value-holder)
                        ,@args))
               instructions)

         (apply #'append (nreverse instructions)))))

    (array
     (let* ((value-holder (gensym (concat (symbol-name name) "-destructuring-array-")))
            (instructions
             `(((loopy--iteration-vars . (,value-holder nil))
                (loopy--main-body . (setq ,value-holder ,val))))))
       (cl-loop for symbol-or-seq across var
                for index from 0
                do (push (loopy--parse-loop-command
                          `(,name ,symbol-or-seq (aref ,value-holder ,index)
                                  ,@args))
                         instructions))
       (apply #'append (nreverse instructions))))))

;;;; Selecting parsers
(defun loopy--parse-loop-command (command)
  "Parse COMMAND, returning a list of instructions in the same received order.

To allow for some flexibility in the command parsers, any nil
instructions are removed.

This function gets the parser, and passes the command to that parser."
  (let ((parser (loopy--get-command-parser command)))
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

;; TODO: Is there a cleaner way than this?  Symbol properties?
(defconst loopy--builtin-command-parsers
  ;; A few of these are just aliases.
  '((accumulate   . loopy--parse-accumulate-command)
    (accumulating . loopy--parse-accumulate-command)
    (always       . loopy--parse-always-command)
    (append       . loopy--parse-append-command)
    (appending    . loopy--parse-append-command)
    (across       . loopy--parse-array-command)
    (across-ref   . loopy--parse-array-ref-command)
    (adjoin       . loopy--parse-adjoin-command)
    (adjoining    . loopy--parse-adjoin-command)
    (array        . loopy--parse-array-command)
    (array-index  . loopy--parse-seq-index-command)
    (arrayi       . loopy--parse-seq-index-command)
    (array-ref    . loopy--parse-array-ref-command)
    (arrayf       . loopy--parse-array-ref-command)
    (collect      . loopy--parse-collect-command)
    (collecting   . loopy--parse-collect-command)
    (concat       . loopy--parse-concat-command)
    (concating    . loopy--parse-concat-command)
    (cond         . loopy--parse-cond-command)
    (cons         . loopy--parse-cons-command)
    (conses       . loopy--parse-cons-command)
    (continue     . loopy--parse-skip-command)
    (count        . loopy--parse-count-command)
    (counting     . loopy--parse-count-command)
    (do           . loopy--parse-do-command)
    (each         . loopy--parse-list-command)
    (elements     . loopy--parse-seq-command)
    (elements-ref . loopy--parse-seq-ref-command)
    (expr         . loopy--parse-expr-command)
    (exprs        . loopy--parse-expr-command)
    (find         . loopy--parse-find-command)
    (finding      . loopy--parse-find-command)
    (group        . loopy--parse-group-command)
    (if           . loopy--parse-if-command)
    (in           . loopy--parse-list-command)
    (in-ref       . loopy--parse-list-ref-command)
    (leave        . loopy--parse-leave-command)
    (list         . loopy--parse-list-command)
    (list-index  . loopy--parse-seq-index-command)
    (listi       . loopy--parse-seq-index-command)
    (list-ref     . loopy--parse-list-ref-command)
    (listf        . loopy--parse-list-ref-command)
    (loop         . loopy--parse-sub-loop-command)
    (map          . loopy--parse-map-command)
    (max          . loopy--parse-max-command)
    (maxing       . loopy--parse-max-command)
    (maximize     . loopy--parse-max-command)
    (maximizing   . loopy--parse-max-command)
    (min          . loopy--parse-min-command)
    ;; Unlike "maxing", there doesn't seem to be much on-line about the word
    ;; "minning", but the double-N follows conventional spelling rules, such as
    ;; in "sum" and "summing".
    (minning      . loopy--parse-min-command)
    (minimize     . loopy--parse-min-command)
    (minimizing   . loopy--parse-min-command)
    (multiply     . loopy--parse-multiply-command)
    (multiplying  . loopy--parse-multiply-command)
    (never        . loopy--parse-never-command)
    (nconc        . loopy--parse-nconc-command)
    (nconcing     . loopy--parse-nconc-command)
    (num          . loopy--parse-nums-command)
    (number       . loopy--parse-nums-command)
    (nums         . loopy--parse-nums-command)
    (numbers      . loopy--parse-nums-command)
    (numup        . loopy--parse-nums-up-command)
    (numsup       . loopy--parse-nums-up-command)
    (num-up       . loopy--parse-nums-up-command)
    (number-up    . loopy--parse-nums-up-command)
    (nums-up      . loopy--parse-nums-up-command)
    (numbers-up   . loopy--parse-nums-up-command)
    (numdown      . loopy--parse-nums-down-command)
    (numsdown     . loopy--parse-nums-down-command)
    (num-down     . loopy--parse-nums-down-command)
    (number-down  . loopy--parse-nums-down-command)
    (nums-down    . loopy--parse-nums-down-command)
    (numbers-down . loopy--parse-nums-down-command)
    (nunion       . loopy--parse-nunion-command)
    (nunioning    . loopy--parse-nunion-command)
    (on           . loopy--parse-cons-command)
    (prepend      . loopy--parse-prepend-command)
    (prepending   . loopy--parse-prepend-command)
    (prev         . loopy--parse-prev-expr-command)
    (prev-expr    . loopy--parse-prev-expr-command)
    (push         . loopy--parse-push-into-command)
    (pushing      . loopy--parse-push-into-command)
    (push-into    . loopy--parse-push-into-command)
    (pushing-into . loopy--parse-push-into-command)
    (reduce       . loopy--parse-reduce-command)
    (reducing     . loopy--parse-reduce-command)
    (repeat       . loopy--parse-repeat-command)
    (return       . loopy--parse-early-exit-commands)
    (return-from  . loopy--parse-early-exit-commands)
    (seq          . loopy--parse-seq-command)
    (seq-index    . loopy--parse-seq-index-command)
    (seqi         . loopy--parse-seq-index-command)
    (seq-ref      . loopy--parse-seq-ref-command)
    (seqf         . loopy--parse-seq-ref-command)
    (sequence     . loopy--parse-seq-command)
    (sequencef    . loopy--parse-seq-ref-command)
    (set          . loopy--parse-expr-command)
    (skip         . loopy--parse-skip-command)
    (subloop      . loopy--parse-sub-loop-command)
    (sub-loop     . loopy--parse-sub-loop-command)
    (sum          . loopy--parse-sum-command)
    (summing      . loopy--parse-sum-command)
    (thereis      . loopy--parse-thereis-command)
    (union        . loopy--parse-union-command)
    (unioning     . loopy--parse-union-command)
    (unless       . loopy--parse-when-unless-command)
    (until        . loopy--parse-while-until-commands)
    (vconcat      . loopy--parse-vconcat-command)
    (vconcating   . loopy--parse-vconcat-command)
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
