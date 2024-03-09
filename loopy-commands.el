;;; loopy-commands.el --- Loop commands for loopy -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

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
(require 'generator)
(require 'gv)
(require 'loopy-misc)
(require 'loopy-destructure)
(require 'loopy-instrs)
(require 'loopy-vars)
(require 'map)
(require 'macroexp)
(require 'pcase)
(require 'seq)
(require 'subr-x)

(declare-function loopy--bound-p "loopy")
(declare-function loopy--process-instructions "loopy")
(declare-function loopy--process-instruction "loopy")
(defvar loopy--in-sub-level)

;;;; Helpful Functions

;;;;; Manipulating Instructions

;; If Emacs Lisp ever gets support for true multiple values (via `cl-values'),
;; this function might be a good candidate for use.

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
  (null (cl-set-difference (loopy--extract-keywords list) correct)))

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

;;;;;; Loopy
(cl-defun loopy--parse-loopy-command ((_ &rest body))
  "Parse the `loopy' command as (loopy BODY).

Unlike the `sub-loop' command, this command is not specially
handled by `loopy-iter'."
  `((loopy--main-body ,(macroexpand `(loopy ,@body)))))

;;;;; Genereric Evaluation
;;;;;; Set
(cl-defun loopy--parse-set-command ((&whole cmd _ var &rest vals))
  "Parse the `set' command.

- VAR is the variable to assign.
- VALS are the values to assign to VAR."
  (let* ((value-selector (gensym "set-value-selector-"))
         (arg-length (length vals)))
    (cl-case arg-length
      ;; If no values, repeatedly set to `nil'.
      (0 (loopy--destructure-for-other-command
          var nil))
      ;; If one value, repeatedly set to that value.
      (1 (loopy--destructure-for-other-command
          var (cl-first vals)))
      ;; If two values, repeatedly check against `value-selector' to
      ;; determine if we should assign the first or second value.  This
      ;; is how `cl-loop' does it.
      (2
       `((loopy--other-vars (,value-selector t))
         ,@(loopy--destructure-for-other-command
            var `(if ,value-selector ,(cl-first vals) ,(cl-second vals)))
         ;; This needs to happen right after running the above.
         (loopy--main-body (setq ,value-selector nil))))
      (t
       `((loopy--other-vars (,value-selector 0))
         ;; Assign to var based on the value of value-selector.  For
         ;; efficiency, we want to check for the last expression first,
         ;; since it will probably be true the most times.  To enable
         ;; that, the condition is whether the counter is greater than
         ;; the index of EXPR in REST minus one.
         ;;
         ;; E.g., for '(a b c),
         ;; use '(cond ((> cnt 1) c) ((> cnt 0) b) ((> cnt -1) a))
         ,@(loopy--destructure-for-other-command
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

;;;;;; Prev Expr
;; TODO: Use body of when with-bound and destructuring to allow for `back' not
;;       being known at compile time (but still only being evaluated once.)
;;       (#194)
(cl-defun loopy--parse-set-prev-command
    ((&whole cmd _ var val &key back))
  "Parse the `set-prev' command as (set-prev VAR VAL &key back).

VAR is set to a version of VAL in a past loop cycle.  With BACK,
wait that many cycle before beginning to update VAR.

This command does not wait for VAL to change before updating VAR."
  (let* ((holding-vars (cl-loop for i from 1 to (or back 1)
                                collect (gensym "set-prev-hold")))
         (using-destructuring (seqp var))
         (with-bound (if using-destructuring
                         (cl-some #'loopy--with-bound-p
                                  (cl-second (loopy--destructure-for-iteration var val)))
                       (loopy--with-bound-p var)))
         ;; We don't use `cl-shiftf' in the main body because we want the
         ;; holding variables to update regardless of whether we update
         ;; VAR.
         (holding-vars-setq `(loopy--latter-body
                              (cl-shiftf ,@holding-vars ,val))))
    (if with-bound
        (if using-destructuring
            (let ((cnt-holder (gensym "count"))
                  (back-holder (gensym "back")))
              `((loopy--other-vars (,cnt-holder 0))
                (loopy--latter-body (setq ,cnt-holder (1+ ,cnt-holder)))
                (loopy--other-vars (,back-holder ,back))
                ,@(mapcar (lambda (x) `(loopy--other-vars (,x nil)))
                          holding-vars)
                ,@(loopy--bind-main-body (main-exprs rest-instr)
                      (loopy--destructure-for-other-command
                       var (car holding-vars))
                    `((loopy--main-body (when (>= ,cnt-holder ,back-holder)
                                          ,@main-exprs))
                      ,@rest-instr))
                ,holding-vars-setq))
          (let ((val-holder (gensym "set-prev-val")))
            `((loopy--other-vars (,val-holder ,var))
              ,@(mapcar (lambda (x) `(loopy--other-vars (,x ,val-holder)))
                        holding-vars)
              (loopy--main-body (setq ,var ,(car holding-vars)))
              ,holding-vars-setq)))
      `(,@(mapcar (lambda (x) `(loopy--other-vars (,x nil)))
                  holding-vars)
        ,@(loopy--destructure-for-other-command
           var (car holding-vars))
        ,holding-vars-setq))))

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

  If OTHER-VALS is nil (i.e., no other values are allowed),
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
         ;; Warn with the used name and the true name.
         (loopy--signal-bad-iter name (quote ,name)))

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
                 (signal 'loopy-wrong-number-of-command-arguments-or-bad-keywords
                         (list cmd)))))

         ,(when (consp other-vals)
            `(unless (cl-member (length other-vals)
                                (quote ,other-vals)
                                :test #'=)
               (signal 'loopy-wrong-number-of-command-arguments-or-bad-keywords
                       (list cmd))))

         (ignore cmd name
                 ;; We can only ignore variables if they're defined.
                 ,(if other-vals 'other-vals)
                 ,(if keywords 'opts))

         ,instructions))))

(defun loopy--find-start-by-end-dir-vals (plist &optional cmd)
  "Find the numeric start, end, and step, direction, and inclusivity.

The values are returned in a list in that order as a plist.

PLIST contains the keyword arguments passed to a sequence
iteration command.  The supported keywords are:

- `:from', `:upfrom' (inclusive start)
- `:downfrom' (inclusive start)
- `:to', `:upto' (inclusive end)
- `:downto' (inclusive end)
- `:above' (exclusive end)
- `:below' (exclusive end)
- `:by' (increment)
- `:test' (comparison function)

CMD is the command usage for error reporting."

  (loopy--plist-bind ( :from from :upfrom upfrom :downfrom downfrom
                       :to to :upto upto :downto downto
                       :above above :below below
                       :by by :test test)
      plist
    ;; Check the inputs:
    (when (or (< 1 (cl-count-if #'identity (list from upfrom downfrom)))
              (< 1 (cl-count-if #'identity (list to upto downto above below)))
              (and (or downfrom downto above)
                   (or upfrom upto below)))
      (signal 'loopy-conflicting-command-arguments (list (or cmd plist))))

    (let ((dir-given (or above below downfrom downto upfrom upto))
          (end-given (or to downto above below upto))
          (start-given (or from downfrom upfrom))
          (decreasing (or downfrom downto above))
          (inclusive (not (or above below))))

      ;; Check directions  for above and below.
      ;; :above is only for when the value is decreasing.
      ;; :below is only for when the value in increasing.
      (when (or (and below decreasing)
                (and above (not decreasing)))
        (signal 'loopy-conflicting-command-arguments (list (or cmd plist))))

      ;; If we're using a directional word, then we shouldn't be giving a test.
      (when (and dir-given test)
        (signal 'loopy-conflicting-command-arguments (list (or cmd plist))))

      ;; The first guess is that if we're continuing indefinitely, then there is
      ;; no test that we can run.  The second guess is that this is incorrect
      ;; because the commands use a default value for the start and ends, so a
      ;; testing function is still valid.  The answer is that without knowing
      ;; the direction, then there is no way to know the correct default values,
      ;; and if the direction is known, then the test is not needed.
      (when (and test (null end-given))
        (signal 'loopy-conflicting-command-arguments (list (or cmd plist))))

      (when (and test (null start-given))
        (signal 'loopy-conflicting-command-arguments (list (or cmd plist))))

      `(,@(when start-given
            `(:start ,start-given))
        ,@(when by
            `(:by ,by))
        ,@(when end-given
            `(:end ,end-given))
        :dir-given ,dir-given
        :test-given ,test
        :test ,(or test (if inclusive
                            ;; Functions are double quoted to match as if the
                            ;; user passed a sharp-quoted function to the macro.
                            (if decreasing
                                (quote #'>=)
                              (quote #'<=))
                          (if decreasing
                              (quote #'>)
                            (quote #'<))))
        :decreasing ,decreasing
        :inclusive ,inclusive))))

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
`:upfrom', `:to', `:downto', `:upto', `:above', `:below', and
`:test'.

- `:index' names a variable used to store the accessed index of
  the array.
- `:by' is the increment step size as a positive value.
- `:from', `:downfrom', and `:upfrom' name the starting index
- `:to', `:downto', and `:upto' name the ending index (inclusive)
- `:below' and `:above' name an exclusive ending index.
- `:test' is the test function.

`:downto' and `:downfrom' make the index decrease instead of increase.

If multiple values are given, their elements are distributed
using the function `loopy--distribute-array-elements'."
  :other-vals t
  :keywords ( :index :test
              :by :from :downfrom :upfrom :to :downto :upto :above :below)
  :instructions

  (loopy--plist-bind ( :start key-start :end key-end :by (by 1)
                       :decreasing decreasing
                       :test-given test-given :test test)
      (loopy--find-start-by-end-dir-vals opts)
    (loopy--instr-let-const* ((value-holder (if (null other-vals)
                                                val
                                              `(loopy--distribute-array-elements
                                                ,val ,@other-vals)))
                              (end-holder (or key-end
                                              (if decreasing
                                                  0
                                                `(1- (length ,value-holder)))))
                              (increment-holder by)
                              (test test))
        loopy--iteration-vars
      (loopy--instr-let-var* ((index-holder (or key-start
                                                (if decreasing
                                                    `(1- (length ,value-holder))
                                                  0))
                                            (plist-get opts :index)))
          loopy--iteration-vars
        `(,@(loopy--destructure-for-iteration-command
             var `(aref ,value-holder ,index-holder))
          (loopy--latter-body
           (setq ,index-holder (,(cond
                                  (test-given #'+)
                                  (decreasing #'-)
                                  (t #'+))
                                ,index-holder ,increment-holder)))
          (loopy--pre-conditions (funcall ,test
                                          ,index-holder
                                          ,end-holder)))))))

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
  :keywords (:index :by :from :downfrom :upfrom :to :downto :upto :above :below :test)
  :instructions
  (loopy--plist-bind ( :start key-start :end key-end :by (by 1)
                       :decreasing decreasing
                       :test-given test-given :test test)
      (loopy--find-start-by-end-dir-vals opts)
    (loopy--instr-let-const* ((value-holder val)
                              (end-holder (or key-end
                                              (if decreasing
                                                  0
                                                `(1- (length ,value-holder)))))
                              (increment-holder by)
                              (test test))
        loopy--iteration-vars
      (loopy--instr-let-var* ((index-holder (or key-start
                                                (if decreasing
                                                    `(1- (length ,value-holder))
                                                  0))
                                            (plist-get opts :index)))
          loopy--iteration-vars
        `(,@(loopy--destructure-for-generalized-command
             var `(aref ,value-holder ,index-holder))
          (loopy--latter-body
           (setq ,index-holder (,(cond
                                  (test-given #'+)
                                  (decreasing #'-)
                                  (t #'+))
                                ,index-holder ,increment-holder)))
          (loopy--pre-conditions (funcall ,test
                                          ,index-holder
                                          ,end-holder)))))))

;;;;;; Cons
(loopy--defiteration cons
  "Parse the `cons' loop command as (cons VAR VAL &key by).

VAR is a variable name.  VAL is a cons cell value.  Keyword BY
is a function by which to update VAR (default `cdr')."
  :keywords (:by)
  :instructions
  (loopy--instr-let-const* ((cons-by (or by (quote #'cdr))))
      loopy--iteration-vars
    (let ((indirect (or (loopy--with-bound-p var)
                        (sequencep var))))
      (loopy--instr-let-var* ((cons-value val (unless indirect var)))
          loopy--iteration-vars
        `(;; NOTE: The benchmarks show that `consp' is faster than no `consp',
          ;;       at least for some commands.
          (loopy--pre-conditions (consp ,cons-value))
          ,@(when indirect
              (loopy--destructure-for-iteration-command var cons-value))
          (loopy--latter-body
           (setq ,cons-value (funcall ,cons-by ,cons-value))))))))

;;;;;; Iter
(loopy--defiteration iter
  "Parse the `iter' command as (iter [VAR] VAL &key yield-result (close t)).

VAR is the variable.  VAL is an generator-producing function, as
from `iter-defun' or `iter-lambda'.  YIELD-RESULT is the optional
value of `iter-next'. CLOSE is whether the iterator should be
closed after the loop completes."
  :required-vals 0 ;; Require values /after/ `var'.
  :other-vals (0 1)
  :keywords (:yield-result :close)
  :instructions
  (let ((obj-holder (gensym "iter-obj"))
        (val-holder (gensym "iter-val"))
        ;; If `other-vals', then we specified an iteration variable.
        ;; Otherwise, don't bother saving anything.
        (using-var other-vals)
        (no-intermediate (and other-vals (not (or (sequencep var)
                                                  (loopy--with-bound-p var))))))
    (loopy--plist-bind ( :yield-result (yield-result (quote nil))
                         :close (close t))
        opts
      ;; We always capture the iterator object in a generator,
      ;; since it might be produced by a function call.
      `((loopy--iteration-vars (,obj-holder ,(if other-vals
                                                 (cl-first other-vals)
                                               var)))
        ,@(cond
           (no-intermediate
            `((loopy--iteration-vars (,var nil))
              (loopy--pre-conditions
               (condition-case nil
                   (setq ,var (iter-next ,obj-holder ,yield-result))
                 (iter-end-of-sequence nil)
                 (:success t)))))
           (using-var
            `((loopy--iteration-vars (,val-holder nil))
              (loopy--pre-conditions
               (condition-case nil
                   (setq ,val-holder (iter-next ,obj-holder ,yield-result))
                 (iter-end-of-sequence nil)
                 (:success t)))
              ,@(loopy--destructure-for-iteration-command var val-holder)))
           (t
            `((loopy--pre-conditions
               (condition-case nil
                   (iter-next ,obj-holder ,yield-result)
                 (iter-end-of-sequence nil)
                 (:success t))))))
        ,(when close
           `(loopy--vars-final-updates
             (,obj-holder . (iter-close ,obj-holder))))))))



;;;;;; List
;; TODO: Make this a normal function.
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
  (loopy--instr-let-const* ((list-func (or (plist-get opts :by)
                                           (quote #'cdr))))
      loopy--iteration-vars
    (loopy--instr-let-var* ((list-val (if (null other-vals)
                                          val
                                        `(loopy--distribute-list-elements
                                          ,val ,@other-vals))))
        loopy--iteration-vars
      `(;; NOTE: The benchmarks show that `consp' is faster than no `consp',
        ;;       at least for some commands.
        (loopy--pre-conditions (consp ,list-val))
        ,@(loopy--destructure-for-iteration-command var `(car ,list-val))
        (loopy--latter-body
         (setq ,list-val (funcall ,list-func ,list-val)))))))

;;;;;; List Ref
(loopy--defiteration list-ref
  "Parse the `list-ref' loop command as (list-ref VAR VAL &key by).

BY is the function to use to move through the list (default `cdr')."
  :keywords (:by)
  :instructions
  (loopy--instr-let-const* ((list-func (or by (quote #'cdr))))
      loopy--iteration-vars
    (loopy--instr-let-var* ((list-val val))
        loopy--iteration-vars
      `(;; NOTE: The benchmarks show that `consp' is faster than no `consp',
        ;;       at least for some commands.
        (loopy--pre-conditions (consp ,list-val))
        ,@(loopy--destructure-for-generalized-command var `(car ,list-val))
        (loopy--latter-body
         (setq ,list-val (funcall ,list-func ,list-val)))))))

;;;;;; Map
;; TODO: Instead of using `seq-uniq' at the start,
;;       check as we go.
(cl-defun loopy--parse-map-command ((name var val &key (unique t)))
  "Parse the `map' loop command.

Iterates through an alist of (key . value) dotted pairs,
extracted from a hash-map, association list, property list, or
vector using the library `map.el'."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter name 'map))
  (let ((value-holder (gensym "map-")))
    `((loopy--iteration-vars
       (,value-holder ,(if unique
                           `(seq-uniq (map-pairs ,val) #'loopy--car-equal-car)
                         `(map-pairs ,val))))
      ,@(loopy--destructure-for-iteration-command var `(car ,value-holder))
      ;; NOTE: The benchmarks show that `consp' is faster than no `consp',
      ;;       at least for some commands.
      (loopy--pre-conditions (consp ,value-holder))
      (loopy--latter-body (setq ,value-holder (cdr ,value-holder))))))

;;;;;; Map-Ref
(cl-defun loopy--parse-map-ref-command ((name var val &key key (unique t)))
  "Parse the `map-ref' command as (map-ref VAR VAL).

KEY is a variable name in which to store the current key.

Uses `map-elt' as a `setf'-able place, iterating through the
map's keys.  Duplicate keys are ignored."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter name 'map-ref))
  (let ((key-list (gensym "map-ref-keys")))
    `((loopy--iteration-vars (,key-list ,(if unique
                                             `(seq-uniq (map-keys ,val))
                                           `(map-keys ,val))))
      ,@(when key
          `((loopy--iteration-vars (,key nil))
            (loopy--main-body (setq ,key (car ,key-list)))))
      ,@(loopy--destructure-for-generalized-command
         var `(map-elt ,val ,(or key `(car ,key-list))))
      ;; NOTE: The benchmarks show that `consp' is faster than no `consp',
      ;;       at least for some commands.
      (loopy--pre-conditions (consp ,key-list))
      (loopy--latter-body (setq ,key-list (cdr ,key-list))))))

;;;;;; Numbers
(loopy--defiteration numbers
  "Parse the `numbers' command as (numbers VAR &key KEYS).

- START is the starting index, if given.
- END is the ending index (inclusive), if given.
- STEP is a positive or negative step size, if given.

KEYS is one or several of `:index', `:by', `:from', `:downfrom',
`:upfrom', `:to', `:downto', `:upto', `:above', `:below', or
`:test'.

- `:by' is the increment step size as a positive value.
- `:from', `:downfrom', and `:upfrom' name the starting index
- `:to', `:downto', and `:upto' name the ending index (inclusive)
- `:below' and `:above' name an exclusive ending index.
- `:test' is the function that checks whether the loop ends,
  as in `(TEST VAR END)'.  `:test' can only be used
  when a direction is not already given.

`:downto' and `:downfrom' make the index decrease instead of increase."
  :keywords (:by :from :downfrom :upfrom :to :downto :upto :above :below :test)
  :required-vals 0
  :other-vals (0 1 2 3)
  :instructions
  ;; TODO: Use `loopy--instr-let-const*' to simplify, after the non-keyword arguments
  ;;       have been removed.
  (cl-destructuring-bind (&optional explicit-start explicit-end explicit-by)
      other-vals
    (loopy--plist-bind ( :start key-start :end key-end :by key-by
                         :decreasing decreasing :inclusive inclusive)

        (condition-case nil
            (loopy--find-start-by-end-dir-vals opts)
          (loopy-conflicting-command-arguments
           (signal 'loopy-conflicting-command-arguments (list cmd))))

      ;; We have to do this here because of how we treat the explicit arguments.
      ;; Once they are removed, we can move this into the above
      ;; `loopy--plist-bind'.
      (let ((key-test (plist-get opts :test)))

        ;; Warn that the non-keyword arguments are deprecated.
        (when (or explicit-start
                  explicit-end
                  explicit-by)
          (warn "`loopy': `numbers': The non-keyword arguments are deprecated.
  Instead, use the keyword arguments, possibly including the new `:test' argument.
  Warning trigger: %s" cmd))

        ;; Check that nothing conflicts.
        (when (or (and explicit-start key-start)
                  (and explicit-end   key-end)
                  (and explicit-by    key-by))
          (signal 'loopy-conflicting-command-arguments (list cmd)))

        (let* ((end (or explicit-end key-end))
               (end-val-holder (gensym "nums-end"))
               (start (or explicit-start key-start 0))
               (by (or explicit-by key-by 1))
               (number-by (numberp by))
               (number-by-and-end (and number-by (numberp end)))
               (increment-val-holder (gensym "nums-increment"))
               (var-val-holder (if (loopy--with-bound-p var)
                                   (gensym "num-test-var")
                                 var)))

          `((loopy--iteration-vars (,var-val-holder ,start))
            ,(when (loopy--with-bound-p var)
               `(loopy--main-body (setq ,var ,var-val-holder)))

            (loopy--latter-body
             (setq ,var-val-holder
                   ,(let ((inc (if number-by
                                   by
                                 increment-val-holder)))
                      (cond (explicit-by `(+ ,var-val-holder ,inc))
                            (key-test        `(+ ,var-val-holder ,inc))
                            (key-by      `(,(if decreasing #'- #'+)
                                           ,var-val-holder ,inc))
                            (decreasing  `(1- ,var-val-holder))
                            (t           `(1+ ,var-val-holder))))))

            ,@(cond
               (number-by-and-end
                `((loopy--pre-conditions (funcall
                                          ,(cond
                                            (key-test key-test)
                                            (explicit-by
                                             (if (cl-plusp by) '#'<= '#'>=))
                                            (inclusive
                                             (if decreasing '#'>= '#'<=))
                                            (t  (if decreasing '#'> '#'<)))
                                          ,var-val-holder ,end))))
               ;; `end' is not a number.  `by' might be a number.
               (end
                `((loopy--iteration-vars (,end-val-holder ,end))
                  ,(when (and (not number-by)
                              (or key-by explicit-by))
                     `(loopy--iteration-vars (,increment-val-holder ,by)))
                  ,@(cond
                     (key-test `((loopy--pre-conditions
                                  ,(loopy--apply-function
                                    key-test var-val-holder end-val-holder))))
                     ((not explicit-by) ; `key-by' or default
                      `((loopy--pre-conditions ,(loopy--apply-function
                                                 (if inclusive
                                                     (if decreasing '#'>= '#'<=)
                                                   (if decreasing '#'> '#'<))
                                                 var-val-holder end-val-holder))))
                     (number-by
                      `((loopy--pre-conditions ,(loopy--apply-function
                                                 (if (cl-plusp by) '#'<= '#'>=)
                                                 var-val-holder end-val-holder))))
                     ;; Ambiguous, so need to check
                     (t
                      (let ((fn (gensym "nums-fn")))
                        `((loopy--iteration-vars
                           (,fn (if (cl-plusp ,increment-val-holder) #'<= #'>=)))
                          (loopy--pre-conditions (funcall ,fn ,var-val-holder
                                                          ,end-val-holder))))))))

               ;; No `end'. We gave a non-number as `by', so we need a holding var.
               ((and by (not number-by))
                `((loopy--iteration-vars (,increment-val-holder ,by)))))))))))


;;;;;; Numbers Up
(loopy--defiteration numbers-up
  "Parse the `numbers-up' command as (numbers-up START [END [STEP]] &key by).

This is for increasing indices.

- START is the starting index.
- END is the ending index (inclusive), if given.
- BY is the step size."
  :other-vals (0 1 2)
  :keywords (:by)
  :instructions
  (loopy--plist-bind (:by by) opts
    (when (and by (cl-second other-vals))
      (signal 'loopy-conflicting-command-arguments (list cmd)))
    (loopy--parse-loop-command
     `(numbers ,var :from ,val
               :upto ,(cl-first other-vals)
               :by ,(or by (cl-second other-vals))))))

;;;;;; Numbers Down
(loopy--defiteration numbers-down
  "Parse the `numbers-down' command as (numbers-down START [END [STEP]] &key by).

This is for decreasing indices.

- START is the starting index.
- END is the ending index (inclusive), if given.
- BY is the step size.  Even though the index value is decreasing,
  this should still be a positive value."
  :other-vals (0 1 2)
  :keywords (:by)
  :instructions
  (loopy--plist-bind (:by by) opts
    (when (and by (cl-second other-vals))
      (signal 'loopy-conflicting-command-arguments (list cmd)))
    (loopy--parse-loop-command
     `(numbers ,var :from ,val
               :downto ,(cl-first other-vals)
               :by ,(or by (cl-second other-vals))))))

;;;;;; Cycle/repeat
(cl-defun loopy--parse-cycle-command
    ((name var-or-count &optional (count nil count-given)))
  "Parse the `cycle' loop command as (repeat [VAR] VAL).

VAR-OR-COUNT is a variable name or an integer.  Optional COUNT is
an integer, to be used if a variable name is provided.
NAME is the name of the command."
  (when loopy--in-sub-level
    (loopy--signal-bad-iter name 'cycle))
  (let* ((bound-and-given (and count-given
                               (loopy--with-bound-p var-or-count)))
         (value-holder (if (or (not count-given)
                               bound-and-given)
                           (gensym "repeat-limit-")
                         var-or-count))
         (num-steps (if count-given
                        count
                      var-or-count)))
    `((loopy--iteration-vars (,value-holder 0))
      ,(when bound-and-given
         `(loopy--main-body (setq ,var-or-count ,value-holder)))
      (loopy--latter-body (setq ,value-holder (1+ ,value-holder)))
      (loopy--pre-conditions (< ,value-holder ,num-steps)))))

;;;;;; Seq
;; TODO: Turn this into a function.
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
`:upfrom', `:to', `:downto', `:upto', `:above', `:below', and
`:test'.

- `:index' names a variable used to store the accessed index of
  the sequence.
- `:by' is the increment step size as a positive value.
- `:from', `:downfrom', and `:upfrom' name the starting index
- `:to', `:downto', and `:upto' name the ending index (inclusive)
- `:below' and `:above' name an exclusive ending index.

`:downto' and `:downfrom' make the index decrease instead of increase.

If multiple sequence values are given, their elements are
distributed using the function `loopy--distribute-sequence-elements'."
  :keywords (:index :by :from :downfrom :upfrom :to :downto :upto :above :below :test)
  :other-vals t
  :instructions
  (loopy--plist-bind ( :start starting-index :end ending-index :by (by 1)
                       :decreasing going-down :test test :test-given test-given)

      (loopy--find-start-by-end-dir-vals opts)

    ;; If we are going up, then we can use `nthcdr' for lists instead of
    ;; searching from the beginning each time with `elt'.
    ;;
    ;; It's a bit weird in that if we're going up, we want to know the starting
    ;; index before we calculate the initial value, so that we can call `nthcdr'
    ;; if needed.  However, if we're going down, then the starting index is the
    ;; 1 minus the length of the initial value, so we would like to have the
    ;; initial value first.  To compromise, we just use a maybe-variable holding
    ;; the the declared start or zero, which we may or may not use in the
    ;; expansion.  Another option would be to duplicate most of the code and
    ;; branch on that single condition, but the cost of the variable should be
    ;; negligible.
    (let ((optimize (and (not going-down) (not test-given))))
      (loopy--instr-let-const* ((by by)
                                (maybe-start (or starting-index 0))
                                (test test))
          loopy--iteration-vars
        (loopy--instr-let-var* ((temp-val (if other-vals
                                              `(loopy--distribute-sequence-elements
                                                ,val ,@other-vals)
                                            val))
                                (is-list (if optimize
                                             `(consp ,temp-val)
                                           nil))
                                (seq-val (if optimize
                                             `(if (and ,is-list
                                                       (> ,maybe-start 0))
                                                  (nthcdr ,maybe-start ,temp-val)
                                                ,temp-val)
                                           temp-val))
                                (seq-index (if optimize
                                               maybe-start
                                             (if starting-index
                                                 maybe-start
                                               `(1- (length ,seq-val))))
                                           (plist-get opts :index)))
            loopy--iteration-vars
          (loopy--instr-let-const* ((end (cond
                                          (ending-index)
                                          (going-down 0)
                                          ;; Only calculate length when actually needed.
                                          (t `(unless ,is-list
                                                (1- (length ,seq-val)))))))
              loopy--iteration-vars
            `((loopy--pre-conditions ,(if optimize
                                          `(if ,is-list
                                               (consp ,seq-val)
                                             (funcall ,test ,seq-index ,end))
                                        `(funcall ,test ,seq-index ,end)))
              ,@(loopy--destructure-for-iteration-command
                 var (if optimize
                         `(if ,is-list
                              (car ,seq-val)
                            (aref ,seq-val ,seq-index))
                       `(elt ,seq-val ,seq-index)))
              (loopy--latter-body
               ,(cond
                 (test-given `(setq ,seq-index (+ ,seq-index ,by)))
                 (going-down `(setq ,seq-index (- ,seq-index ,by)))
                 ;; If the user intends to use the index, we need
                 ;; to make sure that we're always updating it.
                 ((plist-member opts :index)
                  `(progn
                     (when ,is-list
                       (setq ,seq-val (nthcdr ,by ,seq-val)))
                     (setq ,seq-index (,(if going-down #'- #'+)
                                       ,seq-index ,by))))
                 ;; Otherwise, we only have to update it
                 ;; when not using the list.
                 (t
                  `(if ,is-list
                       (setq ,seq-val (nthcdr ,by ,seq-val))
                     (setq ,seq-index (,(if going-down #'- #'+)
                                       ,seq-index ,by)))))))))))))

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

  :keywords (:by :from :downfrom :upfrom :to :downto :upto :above :below :test)
  :instructions
  (let ((with-bound (loopy--with-bound-p var)))
    (loopy--plist-bind ( :start key-start :end key-end :by (by 1)
                         :decreasing decreasing :test key-test)
        (loopy--find-start-by-end-dir-vals opts)
      (loopy--instr-let-var* ((seq-index-val val)
                              (seq-index-index (cond (key-start)
                                                     (decreasing `(1- (length ,seq-index-val)))
                                                     (t 0))
                                               (unless with-bound var)))
          loopy--iteration-vars
        (loopy--instr-let-const* ((seq-index-index-end (or key-end
                                                           (if decreasing
                                                               0
                                                             `(1- (length ,seq-index-val)))))
                                  (test key-test)
                                  (by by))
            loopy--iteration-vars
          `((loopy--pre-conditions (funcall ,test ,seq-index-index ,seq-index-index-end))
            ,@(when with-bound
                `((loopy--iteration-vars (,var nil))
                  (loopy--main-body (setq ,var ,seq-index-index))))
            (loopy--latter-body
             ,(cond
               (decreasing `(setq ,seq-index-index (- ,seq-index-index ,by)))
               (t          `(setq ,seq-index-index (+ ,seq-index-index ,by)))))))))))

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
- `:test' is the test function.

`:downto' and `:downfrom' make the index decrease instead of increase."
  :keywords (:index :by :from :downfrom :upfrom :to :downto :upto :above :below :test)
  :instructions
  (loopy--plist-bind ( :start starting-index :end ending-index :by (by 1)
                       :decreasing going-down :test test :test-given test-given)

      (loopy--find-start-by-end-dir-vals opts)

    ;; If we are going up, then we can use `nthcdr' for lists instead of
    ;; searching from the beginning each time with `elt'.
    ;;
    ;; It's a bit weird in that if we're going up, we want to know the starting
    ;; index before we calculate the initial value, so that we can call `nthcdr'
    ;; if needed.  However, if we're going down, then the starting index is the
    ;; 1 minus the length of the initial value, so we would like to have the
    ;; initial value first.  To compromise, we just use a maybe-variable holding
    ;; the the declared start or zero, which we may or may not use in the
    ;; expansion.  Another option would be to duplicate most of the code and
    ;; branch on that single condition, but the cost of the variable should be
    ;; negligible.
    (let ((optimize (and (not going-down) (not test-given))))
      (loopy--instr-let-const* ((by by)
                                (maybe-start (or starting-index 0))
                                (test test))
          loopy--iteration-vars
        (loopy--instr-let-var* ((temp-val val)
                                (is-list (if optimize
                                             `(consp ,temp-val)
                                           nil))
                                (seq-val (if optimize
                                             `(if (and ,is-list
                                                       (> ,maybe-start 0))
                                                  (nthcdr ,maybe-start ,temp-val)
                                                ,temp-val)
                                           temp-val))
                                (seq-index (if optimize
                                               maybe-start
                                             (if starting-index
                                                 maybe-start
                                               `(1- (length ,seq-val))))
                                           (plist-get opts :index)))
            loopy--iteration-vars
          (loopy--instr-let-const* ((end (cond
                                          (ending-index)
                                          (going-down 0)
                                          ;; Only calculate length when actually needed.
                                          (t `(unless ,is-list
                                                (1- (length ,seq-val)))))))
              loopy--iteration-vars
            `((loopy--pre-conditions ,(if optimize
                                          `(if ,is-list
                                               (consp ,seq-val)
                                             (funcall ,test ,seq-index ,end))
                                        `(funcall ,test ,seq-index ,end)))
              ;; NOTE: Yes, we can use `if' with `setf', apparently.
              ,@(loopy--destructure-for-generalized-command
                 var (if optimize
                         `(if ,is-list
                              (car ,seq-val)
                            (aref ,seq-val ,seq-index))
                       `(elt ,seq-val ,seq-index)))
              (loopy--latter-body
               ,(cond
                 (test-given `(setq ,seq-index (+ ,seq-index ,by)))
                 (going-down `(setq ,seq-index (- ,seq-index ,by)))
                 ;; If the user intends to use the index, we need
                 ;; to make sure that we're always updating it.
                 ((plist-member opts :index)
                  `(progn
                     (when ,is-list
                       (setq ,seq-val (nthcdr ,by ,seq-val)))
                     (setq ,seq-index (,(if going-down #'- #'+)
                                       ,seq-index ,by))))
                 ;; Otherwise, we only have to update it
                 ;; when not using the list.
                 (t
                  `(if ,is-list
                       (setq ,seq-val (nthcdr ,by ,seq-val))
                     (setq ,seq-index (,(if going-down #'- #'+)
                                       ,seq-index ,by)))))))))))))

;;;;; Accumulation
;;;;;; Compatibility
(defvar loopy--known-accumulation-categories
  '( list reverse-list
     string reverse-string
     vector reverse-vector
     boolean-thereis boolean-always-never
     number generic)
  "Known accumulation categories.

Used for error checking with `loopy--check-accumulation-compatibility.'")

(defun loopy--check-accumulation-compatibility
    (loop-name variable category command)
  "Check accumulation command compatibility.

Known accumulation commands are listed in
`loopy--accumulation-variable-info'.

LOOP-NAME is the name of the loop in which VARIABLE accumulates.
VARIABLE is the accumulation variable.  CATEGORY is one of
`list', `reverse-list', `string', `reverse-string', `vector',
`reverse-vector', `boolean-thereis', `boolean-always-never',
`number', and `generic'.  It describes how the accumulation is
being built and its return type.  COMMAND is the accumulation
command.

- Strings are only made by `concat'.
- Vectors are only made by `vconcat'.
- Lists are made by commands like `append', `collect', and `union'.
- Reverse-lists are made by commands which construct lists in
  reverse for efficiency, whose normal result is a list.  This
  excludes commands like `concat' and `vconcat', and is
  unaffected by commands which coerce the type of result after
  the loop, such as `collect'.
- `boolean-thereis' is only used by the `thereis' command.
- `boolean-always-never' is only used by the `always' and `never' commands."
  (unless (memq category loopy--known-accumulation-categories)
    (signal 'loopy-bad-accum-category (list category)))

  (let ((key (cons loop-name variable)))
    (if-let ((existing-description
              (alist-get key loopy--accumulation-variable-info
                         nil nil #'equal)))
        (cl-destructuring-bind (existing-category existing-command)
            existing-description
          (unless (eq category existing-category)
            (signal 'loopy-incompatible-accumulations
                    (list existing-command
                          command))))
      (push (cons key (list category command))
            loopy--accumulation-variable-info))))

;;;;;; End Tracking
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

(defun loopy--produce-collect-end-tracking (var val)
  "Produce instructions for an end-tracking accumulation of single items.

VAR is the variable whose end is to be tracked.  VAL is the value
to be added to the end of VAR.  This is used in accumulation
commands like `collect'.

For efficiency, accumulation commands use references to track the
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

(cl-defun loopy--produce-adjoin-end-tracking (var val &key test key)
  "Produce instructions for an end-tracking accumulation of single items.

VAR is the variable whose end is to be tracked.  VAL is the value
to be added to the end of VAR. TEST is the test function.  KEY is
the transform function.  This is used in
accumulation commands like `adjoin'.

For efficiency, accumulation commands use references to track the
end location of the results list.  For larger lists, this is much
more efficient than repeatedly traversing the list."
  ;; End tracking is a bit slower than `nconc' for short lists, but much faster
  ;; for longer lists.
  (let ((last-link (loopy--get-accumulation-list-end-var loopy--loop-name var)))
    `((loopy--accumulation-vars (,last-link nil))
      ,@(loopy--instr-let-const* ((test-val test)
                                  (key-val key))
            loopy--accumulation-vars
          `((loopy--main-body
             ,(cl-once-only ((adjoin-value val))
                `(cond
                  ((loopy--member-p ,var ,adjoin-value :test ,test-val :key ,key-val)
                   nil)
                  ;; If `last-link' is know, set it's cdr.
                  (,last-link
                   (setcdr ,last-link (list ,adjoin-value))
                   (setq ,last-link (cdr ,last-link)))
                  ;; If `var' was updated without `last-link',
                  ;; reset `last-link'.
                  (,var
                   (setq ,last-link (last ,var))
                   (setcdr ,last-link (list ,adjoin-value))
                   (setq ,last-link (cdr ,last-link)))
                  ;; Otherwise, set `var' and `last-link' directly.
                  (t
                   (setq ,var (list ,adjoin-value)
                         ,last-link ,var))))))))))

(defun loopy--produce-multi-item-end-tracking (var val &optional destructive)
  "Produce instructions for an end-tracking accumulation of copy-joined lists.

VAR is the variable whose end is to be tracked.  VAL is the value
to be added to the end of VAR.  DESTRUCTIVE determines whether
VAL is added to end of VAR destructively.  This is used in
accumulation commands like `append' and `nconc'.

For efficiency, accumulation commands use references to track the
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

(cl-defun loopy--produce-union-end-tracking
    (var val &key test key destructive)
  "Produce instructions for an end-tracking accumulation of modify-joined lists.

VAR is the variable whose end is to be tracked.  VAL is the value
to be added to the end of VAR.  TEST is the function used to
determine presence.  KEY is the transform function.  DESTRUCTIVE
determines whether VAL is added to end of VAR destructively.
This is used in accumulation commands like `union' and `nunion'.

For efficiency, accumulation commands use references to track the
end location of the results list.  For larger lists, this is much
more efficient than repeatedly traversing the list."
  ;; End tracking is a bit slower than `nconc' for short
  ;; lists, but much faster for longer lists.
  (let ((last-link (loopy--get-accumulation-list-end-var loopy--loop-name var)))
    `((loopy--accumulation-vars (,last-link nil))
      ,@(loopy--instr-let-const* ((test-val test)
                                  (key-val key))
            loopy--accumulation-vars
          `((loopy--main-body
             ,(cl-with-gensyms (new-items)
                `(if-let ((,new-items
                           (cl-delete-if ,(loopy--get-union-test-method
                                           var
                                           :test test-val
                                           :key key-val)
                                         ,(if destructive
                                              val
                                            `(copy-sequence ,val)))))
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
                             ,last-link (last ,var))))))))))))

;;;;;; Test Methods
(cl-defun loopy--get-union-test-method (var &key key test)
  "Get a function testing for values in VAR in `union' and `nunion'.

This function is fed to `cl-remove-if' or `cl-delete-if'.  See
the definitions of those commands for more context.

TEST is use to check for equality (default `equal').  KEY modifies
the inputs to test."
  ;;  KEY applies to the value being tested as well as the elements in the list.
  (cl-with-gensyms (arg)
    `(lambda (,arg)
       (loopy--member-p ,var ,arg :test ,test :key ,key))))

;;;;;; Optimized Accumulations
(defun loopy--expand-optimized-accum (arg)
  "Produce an expansion from the quoted data passed to `loopy--optimized-accum'.

ARG is a quoted plist.  That is, in the macro expansion, it a
list of two elements, with the second being the plist.  The plist
should have at least the keys `:cmd' and `:loop'.

Then entire plist is passed to the constructor found in
`loopy--accumulation-constructors'.

`loopy--optimized-accum' is a fake function.  It only used in a
second pass of macro expansion."
  ;; Data is quoted to prevent recursive macro expansion.
  (let ((plist (cl-second arg)))
    (loopy--plist-bind (:name name :loop loop)
        plist
      (let ((true-name (loopy--get-true-name name)))
        (if-let ((func (map-elt loopy--accumulation-constructors true-name)))
            (cl-destructuring-bind (main-body other-instrs)
                (loopy--extract-main-body (funcall func plist))
              (loopy--process-instructions
               `((loopy--at-instructions (,loop ,@(remq nil other-instrs)))))
              (macroexp-progn main-body))
          (signal 'loopy-accum-constructor-missing (list name)))))))

(cl-defun loopy--update-accum-place-count (loop var place &optional (value 1))
  "Keep track of where things are being placed.

LOOP is the current loop.  VAR is the accumulation variable.
PLACE is one of `start' or `end'.  VALUE is the integer by which
to increment the count (default 1)."
  (loopy--check-target-loop-name loop)
  (cl-symbol-macrolet ((loop-map (map-elt loopy--accumulation-places loop)))
    (unless (map-elt loop-map var)
      (setf (map-elt loop-map var)
            (list (cons 'start 0) (cons 'end 0))))
    (setq place (loopy--normalize-symbol place))
    (when (eq place 'beginning) (setq place 'start))
    (loopy--check-position-name place)
    (cl-incf (map-elt (map-elt loop-map var) place) value)))

;;;;;; Commands
(cl-defmacro loopy--defaccumulation (name
                                     doc-string
                                     &key
                                     (num-args 2)
                                     category
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
- CATEGORY is the category used by
  `loopy--check-accumulation-compatibility'.  It is an unquoted
  symbol or an unquoted plist with the keys `:implicit' and
  `:explicit'.

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
            (implicit-num-args (1- num-args))
            (explicit-category)
            (implicit-category))
        (when category
          (if (symbolp category)
              (setq explicit-category category
                    implicit-category category)
            (setq explicit-category (plist-get category :explicit)
                  implicit-category (plist-get category :implicit)))
          (unless (and explicit-category implicit-category)
            (error "Explicit or implicit accumulation category is nil.")))
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
                 (signal 'loopy-wrong-number-of-command-arguments-or-bad-keywords
                         (list cmd)))
               (let* ((into-var (plist-get opts :into))
                      (var (or into-var 'loopy-result))
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
                             ,(when category
                                `(loopy--check-accumulation-compatibility
                                  loopy--loop-name var (quote ,explicit-category) cmd))
                             ,explicit)
                         ,(when category
                            `(loopy--check-accumulation-compatibility
                              loopy--loop-name var (quote ,implicit-category) cmd))
                         ,implicit)
                    `(progn
                       ,(when category
                          `(loopy--check-accumulation-compatibility
                            loopy--loop-name var (quote ,explicit-category) cmd))
                       ,explicit))))

              ((= arg-length ,explicit-num-args)
               ,(when keywords
                  `(unless (loopy--only-valid-keywords-p (quote ,keywords) opts)
                     (signal 'loopy-wrong-number-of-command-arguments-or-bad-keywords
                             (list cmd))))
               (let ((var (cl-first args))
                     (val (cl-second args)))
                 (ignore var val)

                 (if (sequencep var)
                     ;; If we need to destructure the sequence `var', we use the
                     ;; function named by
                     ;; `loopy--destructuring-accumulation-parser' or the function
                     ;; `loopy--parse-destructuring-accumulation-command-default'.
                     (funcall (or loopy--destructuring-accumulation-parser
                                  #'loopy--parse-destructuring-accumulation-command-default)
                              cmd)

                   ;; Substitute in the instructions.
                   ,(when category
                      `(loopy--check-accumulation-compatibility
                        loopy--loop-name var (quote ,explicit-category) cmd))
                   ,explicit)))
              (t
               (signal 'loopy-wrong-number-of-command-arguments-or-bad-keywords
                       (list cmd)))))))))


;;;;;;; Accumulate
;; TODO: Should the function be evaluated only once for `accumulate'?
;;       It would produce faster code, by as an accumulation argument/value,
;;       should it be able to change during the loop?
(loopy--defaccumulation accumulate
  "Parse the `accumulate command' as (accumulate VAR VAL FUNC)."
  :num-args 3
  :explicit (progn
              (loopy--check-accumulation-compatibility loopy--loop-name var 'generic cmd)
              `((loopy--accumulation-vars (,var nil))
                (loopy--main-body
                 (setq ,var ,(loopy--apply-function (cl-third args) val var)))))
  :implicit (progn
              (loopy--check-accumulation-compatibility loopy--loop-name var 'generic cmd)
              `((loopy--accumulation-vars (,var nil))
                (loopy--main-body
                 (setq ,var ,(loopy--apply-function (cl-second args) val var)))
                (loopy--implicit-return ,var))))

;;;;;;; Adjoin
(defun loopy--construct-accum-adjoin (plist)
  "Construct optimized accumulation for `adjoin' from PLIST."
  (loopy--plist-bind ( :cmd cmd :loop loop :var var :val val
                       :test test :key key :at pos)
      plist
    (map-let (('start start)
              ('end end))
        (loopy--get-accum-counts loop var 'adjoin)
      (let* ((at-start-instrs
              (loopy--instr-let-const* ((test-val test)
                                        (key-val key))
                  loopy--accumulation-vars
                `((loopy--main-body
                   ,(cl-once-only ((adjoin-value val))
                      `(unless (loopy--member-p ,var ,adjoin-value
                                                :test ,test-val :key ,key-val)
                         (cl-callf2 cons ,adjoin-value ,var)))))))
             (at-end-instrs (loopy--produce-adjoin-end-tracking var val
                                                                :test test :key key)))
        `((loopy--accumulation-vars (,var nil))
          ,@(if (>= start end)
                ;; Create list in normal order.
                (progn
                  (loopy--check-accumulation-compatibility loop var 'list cmd)
                  `(,@(if (eq pos 'start)
                          at-start-instrs
                        at-end-instrs)
                    (loopy--vars-final-updates (,var . nil))))

              ;; Create list in reverse order.
              (loopy--check-accumulation-compatibility loop var 'reverse-list cmd)
              `(,@(if (eq pos 'start)
                      at-end-instrs
                    at-start-instrs)
                (loopy--vars-final-updates
                 (,var . (setq ,var (nreverse ,var)))))))))))

(loopy--defaccumulation adjoin
  "Parse the `adjoin' command as (adjoin VAR VAL &key test key at)."
  :keywords (test key at)
  ;; This is same as implicit behavior, so we only need to specify the explicit.
  :explicit
  (loopy--plist-bind ( :test (test (quote #'equal)) :key key :at (pos 'end))
      opts
    (setq pos (loopy--normalize-symbol pos))
    (when (eq pos 'beginning) (setq pos 'start))
    (unless (memq pos '(start beginning end))
      (signal 'loopy-bad-position-command-argument (list pos cmd)))

    (if (memq var loopy--optimized-accum-vars)
        (progn
          (loopy--update-accum-place-count loopy--loop-name var pos)
          `((loopy--main-body
             (loopy--optimized-accum '( :cmd ,cmd :name ,name
                                        :var ,var :val ,val
                                        :test ,test :key ,key :at ,pos)))))

      (loopy--check-accumulation-compatibility loopy--loop-name var 'list cmd)
      `((loopy--accumulation-vars (,var nil))
        ,@(cond
           ((member pos '(start beginning 'start 'beginning))
            (loopy--instr-let-const* ((test-val test)
                                      (key-val key))
                loopy--accumulation-vars
              `((loopy--main-body
                 ,(cl-once-only ((adjoin-value val))
                    `(unless (loopy--member-p ,var ,adjoin-value
                                              :test ,test-val :key ,key-val)
                       (cl-callf2 cons ,adjoin-value ,var)))))))
           ((member pos '(end nil 'end))
            (loopy--produce-adjoin-end-tracking var val :test test :key key))
           (t
            (signal 'loopy-bad-position-command-argument (list pos cmd))))
        (loopy--vars-final-updates (,var . nil)))))
  :implicit
  (loopy--plist-bind ( :test (test (quote #'equal)) :key key :at (pos 'end))
      opts
    (setq pos (loopy--normalize-symbol pos))
    (when (eq pos 'beginning) (setq pos 'start))
    (unless (memq pos '(start beginning end))
      (signal 'loopy-bad-position-command-argument (list pos cmd)))
    (loopy--update-accum-place-count loopy--loop-name var pos)
    `((loopy--main-body
       (loopy--optimized-accum '( :cmd ,cmd :name ,name
                                  :var ,var :val ,val
                                  :test ,test :key ,key :at ,pos)))
      (loopy--implicit-return ,var))))

;;;;;;; Append
(defun loopy--construct-accum-append (plist)
  "Produce accumulation code for `append' from PLIST."
  (loopy--plist-bind ( :cmd cmd :loop loop
                       :var var :val val
                       :at (pos 'end))
      plist
    (setq pos (loopy--get-quoted-symbol pos))
    (map-let (('start start)
              ('end end))
        (loopy--get-accum-counts loop var 'append)
      (if (>= start end)
          ;; Create list in normal order.
          (progn
            (loopy--check-accumulation-compatibility loop var 'list cmd)
            `(,@(if (eq pos 'start)
                    ;; TODO: Is there a better way of appending to the beginning
                    ;;       of a list?
                    ;; `append' doesn't copy the last argument.
                    `((loopy--main-body (setq ,var (append ,val ,var))))
                  (loopy--produce-multi-item-end-tracking var val))
              (loopy--vars-final-updates (,var . nil))))

        ;; Create list in reverse order.
        (loopy--check-accumulation-compatibility loop var 'reverse-list cmd)
        `(,@(if (eq pos 'end)
                `((loopy--main-body (setq ,var (nconc (reverse ,val) ,var))))
              (loopy--produce-multi-item-end-tracking var `(reverse ,val)))
          (loopy--vars-final-updates
           (,var . (setq ,var (nreverse ,var)))))))))

(loopy--defaccumulation append
  "Parse the `append' command as (append VAR VAL &key at)."
  :keywords (at)
  :explicit
  (loopy--plist-bind (:at (pos 'end))
      opts
    (setq pos (loopy--normalize-symbol pos))
    (when (eq pos 'beginning) (setq pos 'start))
    (unless (memq pos '(start beginning end))
      (signal 'loopy-bad-position-command-argument (list pos cmd)))
    (if (memq var loopy--optimized-accum-vars)
        (progn
          (loopy--update-accum-place-count loopy--loop-name var pos)
          `((loopy--accumulation-vars (,var nil))
            (loopy--main-body
             (loopy--optimized-accum '( :loop ,loopy--loop-name
                                        :var ,var :val ,val
                                        :cmd ,cmd :name ,name :at ,pos)))))
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
           (t
            (signal 'loopy-bad-position-command-argument (list pos cmd))))
        (loopy--vars-final-updates (,var . nil)))))
  :implicit
  (loopy--plist-bind (:at (pos 'end))
      opts
    (setq pos (loopy--normalize-symbol pos))
    (when (eq pos 'beginning) (setq pos 'start))
    (unless (memq pos '(start beginning end))
      (signal 'loopy-bad-position-command-argument (list pos cmd)))
    (loopy--update-accum-place-count loopy--loop-name var pos)
    `((loopy--accumulation-vars (,var nil))
      (loopy--main-body
       (loopy--optimized-accum '( :loop ,loopy--loop-name :var ,var :val ,val
                                  :cmd ,cmd :name ,name :at ,pos)))
      (loopy--implicit-return ,var))))

;;;;;;; Collect
(defun loopy--construct-accum-collect (plist)
  "Construct an optimized `collect' accumulation from PLIST."
  (loopy--plist-bind ( :cmd cmd :loop loop :var var :val val :at (pos 'end))
      plist
    (setq pos (loopy--get-quoted-symbol pos))
    `((loopy--accumulation-vars (,var nil))
      ,@(map-let (('start start)
                  ('end end))
            (loopy--get-accum-counts loop var 'collect)
          (if (>= start end)
              ;; Create list in normal order.
              (progn
                (loopy--check-accumulation-compatibility loop var 'list cmd)
                `(,@(if (eq pos 'start)
                        `((loopy--main-body (setq ,var (cons ,val ,var))))
                      (loopy--produce-collect-end-tracking var val))
                  (loopy--vars-final-updates (,var . nil))))

            ;; Create list in reverse order.
            (loopy--check-accumulation-compatibility loop var 'reverse-list cmd)
            `(,@(if (eq pos 'end)
                    `((loopy--main-body (setq ,var (cons ,val ,var))))
                  (loopy--produce-collect-end-tracking var val))
              (loopy--vars-final-updates
               (,var . (setq ,var (nreverse ,var))))))))))

(loopy--defaccumulation collect
  "Parse the `collect' command as (collect VAR VAL &key at)."
  :keywords (at)
  :explicit (loopy--plist-bind ( :at (pos (quote 'end)))
                opts
              (setq pos (loopy--normalize-symbol pos))
              (when (eq pos 'beginning) (setq pos 'start))
              (unless (memq pos '(start beginning end))
                (signal 'loopy-bad-position-command-argument (list pos cmd)))
              (if (memq var loopy--optimized-accum-vars)
                  (progn
                    (loopy--update-accum-place-count loopy--loop-name var pos)
                    `((loopy--main-body
                       (loopy--optimized-accum
                        '( :loop ,loopy--loop-name :var ,var :val ,val
                           :cmd ,cmd :name ,name :at ,pos)))))
                (loopy--check-accumulation-compatibility
                 loopy--loop-name var 'list cmd)
                `((loopy--accumulation-vars (,var nil))
                  ,@(cond
                     ((member pos '(start beginning 'start 'beginning))
                      `((loopy--main-body (setq ,var (cons ,val ,var)))))
                     ((member pos '(end 'end))
                      (loopy--produce-collect-end-tracking var val))
                     (t
                      (signal 'loopy-bad-position-command-argument (list pos cmd))))
                  (loopy--vars-final-updates (,var . ,nil)))))

  :implicit (loopy--plist-bind ( :at (pos 'end))
                opts
              (setq pos (loopy--normalize-symbol pos))
              (when (eq pos 'beginning) (setq pos 'start))
              (unless (memq pos '(start beginning end))
                (signal 'loopy-bad-position-command-argument (list pos cmd)))
              (loopy--update-accum-place-count loopy--loop-name var pos)
              `((loopy--main-body
                 (loopy--optimized-accum
                  '( :loop ,loopy--loop-name :var ,var :val ,val
                     :cmd ,cmd :name ,name :at ,pos)))
                (loopy--implicit-return ,var))))

;;;;;;; Concat
(defun loopy--construct-accum-concat (plist)
  "Create accumulation code for `concat' from PLIST.

This function is called by `loopy--expand-optimized-accum'."
  (loopy--plist-bind ( :cmd cmd :loop loop :var var :val val
                       :at (pos 'end))
      plist
    (map-let (('start start)
              ('end end))
        (loopy--get-accum-counts loop var 'concat)
      ;; Forward list order.
      (if (>= start end)
          (progn
            (loopy--check-accumulation-compatibility loop var 'string cmd)
            `(,@(if (eq pos 'start)
                    `((loopy--main-body (setq ,var (cons ,val ,var))))
                  (loopy--produce-collect-end-tracking var val))
              (loopy--vars-final-updates
               (,var . (setq ,var (apply #'concat ,var))))))
        ;; Reverse list order.
        (loopy--check-accumulation-compatibility loop var 'reverse-string cmd)
        `(,@(if (eq pos 'start)
                (loopy--produce-collect-end-tracking var val)
              `((loopy--main-body (setq ,var (cons ,val ,var)))))
          (loopy--vars-final-updates
           (,var . (setq ,var (apply #'concat (nreverse ,var))))))))))

(loopy--defaccumulation concat
  "Parse the `concat' command as (concat VAR VAL &key at)."
  :keywords (at)
  :explicit (loopy--plist-bind (:at (pos 'end))
                opts
              (if (memq var loopy--optimized-accum-vars)
                  (progn
                    (loopy--update-accum-place-count loopy--loop-name var pos)
                    `((loopy--accumulation-vars (,var nil))
                      (loopy--main-body
                       (loopy--optimized-accum
                        '( :loop ,loopy--loop-name :var ,var :val ,val
                           :cmd ,cmd :name ,name :at ,pos)))
                      (loopy--implicit-return ,var)))
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
                            (signal 'loopy-bad-position-command-argument (list pos cmd))))))
                  (loopy--vars-final-updates (,var . nil)))))
  :implicit (loopy--plist-bind (:at (pos 'end))
                opts
              (setq pos (loopy--normalize-symbol pos))
              (when (eq pos 'beginning) (setq pos 'start))
              (unless (memq pos '(start beginning end))
                (signal 'loopy-bad-position-command-argument (list pos cmd)))
              (loopy--update-accum-place-count loopy--loop-name var pos)
              `((loopy--accumulation-vars (,var nil))
                (loopy--main-body
                 (loopy--optimized-accum
                  '( :loop ,loopy--loop-name :var ,var :val ,val
                     :cmd ,cmd :name ,name :at ,pos)))
                (loopy--implicit-return ,var))))

;;;;;;; Count
(loopy--defaccumulation count
  "Parse the `count' command as (count VAR VAL)."
  ;; This is same as implicit behavior, so we only need to specify the explicit.
  :explicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'number cmd)
              `((loopy--accumulation-vars (,var 0))
                (loopy--main-body (if ,val (setq ,var (1+ ,var))))))
  :implicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'number cmd)
              `((loopy--accumulation-vars (,var 0))
                (loopy--main-body (if ,val (setq ,var (1+ ,var))))
                (loopy--implicit-return ,var))))

;;;;;;; Find
(loopy--defaccumulation find
  "Parse a command of the form `(finding VAR EXPR TEST &key ON-FAILURE)'."
  :num-args 3
  :keywords (on-failure)
  :explicit (let* ((test-arg (cl-third args))
                   (test-form (if (loopy--quoted-form-p test-arg)
                                  `(,(loopy--get-function-symbol test-arg) ,val)
                                test-arg))
                   (of-used (plist-member opts :on-failure))
                   (on-failure (plist-get opts :on-failure))
                   (tag-name (loopy--produce-non-returning-exit-tag-name
                              loopy--loop-name))
                   (found (gensym "found")))

              `((loopy--non-returning-exit-used ,tag-name)
                (loopy--accumulation-vars (,var nil))
                ,@(if of-used
                      ;; If TEST always nil, bind to ON-FAILURE.
                      `((loopy--accumulation-vars (,found nil))
                        (loopy--main-body (when ,test-form
                                            (setq ,var ,val
                                                  ,found t)
                                            (throw (quote ,tag-name) t)))
                        (loopy--vars-final-updates
                         (,var . (unless ,found (setq ,var ,on-failure)))))
                    `((loopy--main-body (when ,test-form
                                          (setq ,var ,val)
                                          (throw (quote ,tag-name) t)))))))
  :implicit (let* ((test-arg (cl-second args))
                   (test-form (if (loopy--quoted-form-p test-arg)
                                  `(,(loopy--get-function-symbol test-arg) ,val)
                                test-arg))
                   (of-used (plist-member opts :on-failure))
                   (on-failure (plist-get opts :on-failure))
                   (found (gensym "found"))
                   (tag-name (loopy--produce-non-returning-exit-tag-name
                              loopy--loop-name)))
              `((loopy--non-returning-exit-used ,tag-name)
                (loopy--accumulation-vars (,var nil))
                ,@(if of-used
                      ;; If TEST always nil, bind to ON-FAILURE.
                      `((loopy--accumulation-vars (,found nil))
                        (loopy--main-body (when ,test-form
                                            (setq ,var ,val
                                                  ,found t)
                                            (throw (quote ,tag-name) t)))
                        (loopy--vars-final-updates
                         (,var . (unless ,found (setq ,var ,on-failure)))))
                    `((loopy--main-body (when ,test-form
                                          (setq ,var ,val)
                                          (throw (quote ,tag-name) t)))))
                (loopy--implicit-return   ,var))))

;;;;;;; Set Accum
(loopy--defaccumulation set-accum
  "Parse the `set-accum' command as (set-accum VAR EXPR).

EXPR is the value to bind to VAR."
  :num-args 2
  :category generic
  :implicit `((loopy--accumulation-vars (,var nil))
              (loopy--main-body (setq ,var ,val))
              (loopy--implicit-return ,var))
  :explicit `((loopy--accumulation-vars (,var nil))
              (loopy--main-body (setq ,var ,val))))

;;;;;;; Max
(loopy--defaccumulation max
  "Parse the `max' command as (max VAR VAL)."
  :explicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'number cmd)
              `((loopy--accumulation-vars (,var -1.0e+INF))
                (loopy--main-body (setq ,var (max ,val ,var)))))
  :implicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'number cmd)
              `((loopy--accumulation-vars (,var -1.0e+INF))
                (loopy--main-body (setq ,var (max ,val ,var)))
                (loopy--implicit-return ,var))))

;;;;;;; Min
(loopy--defaccumulation min
  "Parse the `min' command as (min VAR VAL)."
  :explicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'number cmd)
              `((loopy--accumulation-vars (,var +1.0e+INF))
                (loopy--main-body (setq ,var (min ,val ,var)))))
  :implicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'number cmd)
              `((loopy--accumulation-vars (,var +1.0e+INF))
                (loopy--main-body (setq ,var (min ,val ,var)))
                (loopy--implicit-return ,var))))

;;;;;;; Multiply
(loopy--defaccumulation multiply
  "Parse the `multiply' command as (multiply VAR VAL)."
  :explicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'number cmd)
              `((loopy--accumulation-vars (,var 1))
                (loopy--main-body (setq ,var (* ,val ,var)))))
  :implicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'number cmd)
              `((loopy--accumulation-vars (,var 1))
                (loopy--main-body (setq ,var (* ,val ,var)))
                (loopy--implicit-return ,var))))

;;;;;;; Nconc
(defun loopy--construct-accum-nconc (plist)
  "Create accumulation code for PLIST."
  (loopy--plist-bind (:cmd cmd :loop loop :var var :val val :at (pos 'end))
      plist
    (map-let (('start start)
              ('end end))
        (loopy--get-accum-counts loop var 'nconc)
      ;; Forward list order.
      (if (>= start end)
          (progn
            (loopy--check-accumulation-compatibility loop var 'list cmd)
            `(,@(if (eq pos 'start)
                    `((loopy--main-body (setq ,var (nconc ,val ,var))))
                  (loopy--produce-multi-item-end-tracking var val 'destructive))
              (loopy--vars-final-updates (,var . nil))))
        ;; Reverse list order.
        (loopy--check-accumulation-compatibility loop var 'reverse-list cmd)
        `(,@(if (eq pos 'start)
                (loopy--produce-multi-item-end-tracking var `(nreverse ,val) 'destructive)
              `((loopy--main-body (setq ,var (nconc (nreverse  ,val) ,var)))))
          (loopy--vars-final-updates
           (,var . (setq ,var (nreverse ,var)))))))))

(loopy--defaccumulation nconc
  "Parse the `nconc' command as (nconc VAR VAL &key at)."
  :keywords (at)
  :explicit (loopy--plist-bind (:at (pos 'end))
                opts
              (setq pos (loopy--normalize-symbol pos))
              (when (eq pos 'beginning) (setq pos 'start))
              (unless (memq pos '(start beginning end))
                (signal 'loopy-bad-position-command-argument (list pos cmd)))
              (if (memq var loopy--optimized-accum-vars)
                  (progn
                    (loopy--update-accum-place-count loopy--loop-name var pos)
                    `((loopy--accumulation-vars (,var nil))
                      (loopy--main-body (loopy--optimized-accum
                                         '( :loop ,loopy--loop-name :var ,var
                                            :val ,val :cmd ,cmd :name ,name :at ,pos)))))
                (loopy--check-accumulation-compatibility
                 loopy--loop-name var 'list cmd)
                `((loopy--accumulation-vars (,var nil))
                  ,@(cond
                     ((member pos '(start beginning 'start 'beginning))
                      `((loopy--main-body (setq ,var (nconc ,val ,var)))))
                     ((member pos '(end 'end))
                      (loopy--produce-multi-item-end-tracking var val 'destructive))
                     (t
                      (signal 'loopy-bad-position-command-argument (list pos cmd))))
                  (loopy--vars-final-updates (,var . nil)))))
  :implicit (loopy--plist-bind (:at (pos 'end))
                opts
              (setq pos (loopy--normalize-symbol pos))
              (when (eq pos 'beginning) (setq pos 'start))
              (unless (memq pos '(start beginning end))
                (signal 'loopy-bad-position-command-argument (list pos cmd)))
              (loopy--update-accum-place-count loopy--loop-name var pos)
              `((loopy--accumulation-vars (,var nil))
                (loopy--main-body (loopy--optimized-accum
                                   '( :loop ,loopy--loop-name :var ,var
                                      :val ,val :cmd ,cmd :name ,name :at ,pos)))
                (loopy--implicit-return ,var))))

;;;;;;; Nunion
(defun loopy--construct-accum-nunion (plist)
  "Create accumulation code for `nunion' from PLIST.

This function is used by `loopy--expand-optimized-accum'."
  (loopy--plist-bind ( :cmd cmd :loop loop :var var :val val :at (pos 'end)
                       :key key :test test)
      plist
    (cl-flet ((make-at-start (reverse)
                (loopy--instr-let-const* ((key-val key)
                                          (test-val test))
                    loopy--accumulation-vars
                  `((loopy--main-body
                     (setq ,var
                           (nconc ,(let ((del `(cl-delete-if ,(loopy--get-union-test-method
                                                               var
                                                               :test test-val
                                                               :key key-val)
                                                             ,val)))
                                     (if reverse
                                         `(nreverse ,del)
                                       del))
                                  ,var))))))
              (make-at-end (reverse)
                (loopy--produce-union-end-tracking var (if reverse
                                                           `(reverse ,val)
                                                         val)
                                                   :test test
                                                   :key key
                                                   :destructive t)))
      (map-let (('start start)
                ('end end))
          (loopy--get-accum-counts loop var 'nunion)
        ;; Forward list order.
        (if (>= start end)
            (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'list cmd)
              `(,@(if (eq pos 'start)
                      (make-at-start nil)
                    (make-at-end nil))
                (loopy--vars-final-updates (,var . nil))))
          ;; Reverse list order.
          (loopy--check-accumulation-compatibility
           loopy--loop-name var 'reverse-list cmd)
          `(,@(if (eq pos 'start)
                  (make-at-end t)
                (make-at-start t))
            (loopy--vars-final-updates
             (,var . (setq ,var (nreverse ,var))))))))))

(loopy--defaccumulation nunion
  "Parse the `nunion' command as (nunion VAR VAL &key test key at)."
  :keywords (test key at)
  :explicit
  (loopy--plist-bind (:at (pos 'end) :key key :test (test (quote #'equal)))
      opts
    (setq pos (loopy--normalize-symbol pos))
    (when (eq pos 'beginning) (setq pos 'start))
    (unless (memq pos '(start beginning end))
      (signal 'loopy-bad-position-command-argument (list pos cmd)))
    (if (memq var loopy--optimized-accum-vars)
        (progn
          (loopy--update-accum-place-count loopy--loop-name var pos)
          `((loopy--accumulation-vars (,var nil))
            (loopy--main-body
             (loopy--optimized-accum '( :loop ,loopy--loop-name :var ,var
                                        :val ,val :cmd ,cmd :name ,name :at ,pos
                                        :key ,key :test ,test)))))
      (loopy--check-accumulation-compatibility loopy--loop-name var 'list cmd)
      `((loopy--accumulation-vars (,var nil))
        ,@(cond
           ((member pos '(start beginning 'start 'beginning))
            (loopy--instr-let-const* ((test-val test)
                                      (key-val key))
                loopy--accumulation-vars
              `((loopy--main-body
                 (setq ,var (nconc (cl-delete-if
                                    ,(loopy--get-union-test-method var
                                                                   :test test-val
                                                                   :key key-val)
                                    ,val)
                                   ,var))))))
           ((member pos '(end 'end))
            (loopy--produce-union-end-tracking var val
                                               :test test
                                               :key key
                                               :destructive t))
           (t
            (signal 'loopy-bad-position-command-argument (list pos cmd))))
        (loopy--vars-final-updates (,var . nil)))))
  :implicit
  (loopy--plist-bind (:at (pos 'end) :key key :test (test (quote #'equal)))
      opts
    (setq pos (loopy--normalize-symbol pos))
    (when (eq pos 'beginning) (setq pos 'start))
    (unless (memq pos '(start beginning end))
      (signal 'loopy-bad-position-command-argument (list pos cmd)))
    (loopy--update-accum-place-count loopy--loop-name var pos)
    `((loopy--accumulation-vars (,var nil))
      (loopy--implicit-return ,var)
      (loopy--main-body
       (loopy--optimized-accum '( :loop ,loopy--loop-name :var ,var
                                  :val ,val :cmd ,cmd :name ,name :at ,pos
                                  :key ,key :test ,test))))))

;;;;;;; Prepend
(defun loopy--parse-prepend-command (arg)
  "Parse the `prepend' command as (append VAR VAL :at start)."
  (unless (member (length arg) '(2 3))
    (error "`%s': Wrong number of arguments: %s"
           (car arg) arg))
  ;; Needs to be named `append' for destructuring to work,
  ;; as destructuring just applies the named command to the sub-elements.
  (loopy--parse-append-command `(append ,@(cdr arg) :at start)))

;;;;;;; Push Into
(defun loopy--parse-push-into-command (arg)
  "Parse the `push-into' command as (collect VAR VAL :at start)."

  (unless (member (length arg) '(2 3))
    (error "`%s': Wrong number of arguments: %s"
           (car arg) arg))
  ;; Needs to be named `collect' for destructuring to work,
  ;; as destructuring just applies the named command to the sub-elements.
  (loopy--parse-collect-command `(collect ,@(cdr arg) :at start)))

;;;;;;; Reduce
;; TODO: Should the function be evaluated only once for `reduce'?
;;       It would produce faster code, by as an accumulation argument/value,
;;       should it be able to change during the loop?
(loopy--defaccumulation reduce
  "Parse the `reduce' command as (reduce VAR VAL FUNC).

VAR starts as nil.

By default, the first accumulated value is the value of VAL,
not a result of calling FUNC.  However, if VAR has an initial
value given by the =with= special macro argument, then the first
accumulated value is the result of `(funcall FUNC VAR VAL)', as
also done in the subsequent steps of the loop.  This use of
`with' is similar to the `:initial-value' keyword argument used
by `cl-reduce'."
  :num-args 3
  :category generic
  :implicit `(,@(cond
                 ((loopy--with-bound-p var)
                  `((loopy--main-body
                     (setq ,var ,(loopy--apply-function (cl-second args) var val)))))
                 (t
                  (let ((first-time (gensym "first-time")))
                    `((loopy--accumulation-vars (,var nil))
                      (loopy--accumulation-vars (,first-time t))
                      (loopy--main-body
                       (if ,first-time
                           (setq ,first-time nil
                                 ,var ,val)
                         (setq ,var ,(loopy--apply-function (cl-second args) var val))))))))
              (loopy--implicit-return ,var))
  :explicit `(,@(cond
                 ((loopy--with-bound-p var)
                  `((loopy--main-body
                     (setq ,var ,(loopy--apply-function (cl-third args) var val)))))
                 (t
                  (let ((first-time (gensym "first-time")))
                    `((loopy--accumulation-vars (,var nil))
                      (loopy--accumulation-vars (,first-time t))
                      (loopy--main-body
                       (if ,first-time
                           (setq ,first-time nil
                                 ,var ,val)
                         (setq ,var ,(loopy--apply-function (cl-third args) var val))))))))))

;;;;;;; Sum
(loopy--defaccumulation sum
  "Parse the `sum' command as (sum VAR VAL)."
  ;; This is same as implicit behavior, so we only need to specify the explicit.
  :explicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'number cmd)
              `((loopy--accumulation-vars (,var 0))
                (loopy--main-body (setq ,var (+ ,val ,var)))))
  :implicit (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'number cmd)
              `((loopy--accumulation-vars (,var 0))
                (loopy--main-body (setq ,var (+ ,val ,var)))
                (loopy--implicit-return ,var))))

;;;;;;; Union
(defun loopy--construct-accum-union (plist)
  "Create accumulation code for `nunion' from PLIST.

This function is used by `loopy--expand-optimized-accum'."
  (loopy--plist-bind ( :cmd cmd :loop loop :var var :val val :at (pos 'end)
                       :key key :test test)
      plist
    (cl-flet ((make-at-start (reverse)
                (loopy--instr-let-const* ((key-val key)
                                          (test-val test))
                    loopy--accumulation-vars
                  `((loopy--main-body
                     (setq ,var
                           (nconc ,(let ((del `(cl-delete-if ,(loopy--get-union-test-method
                                                               var
                                                               :test test-val
                                                               :key key-val)
                                                             (copy-sequence ,val))))
                                     (if reverse
                                         `(nreverse ,del)
                                       del))
                                  ,var))))))
              (make-at-end (reverse)
                (loopy--produce-union-end-tracking var (if reverse
                                                           `(reverse ,val)
                                                         val)
                                                   :test test
                                                   :key key
                                                   :destructive nil)))
      (map-let (('start start)
                ('end end))
          (loopy--get-accum-counts loop var 'union)
        ;; Forward list order.
        (if (>= start end)
            (progn
              (loopy--check-accumulation-compatibility
               loopy--loop-name var 'list cmd)
              `(,@(if (eq pos 'start)
                      (make-at-start nil)
                    (make-at-end nil))
                (loopy--vars-final-updates (,var . nil))))
          ;; Reverse list order.
          (loopy--check-accumulation-compatibility
           loopy--loop-name var 'reverse-list cmd)
          `(,@(if (eq pos 'start)
                  (make-at-end t)
                (make-at-start t))
            (loopy--vars-final-updates
             (,var . (setq ,var (nreverse ,var))))))))))

(loopy--defaccumulation union
  "Parse the `union' command as (union VAR VAL &key test key at)."
  :keywords (test key at)
  :explicit
  (loopy--plist-bind (:at (pos 'end) :key key :test (test (quote #'equal)))
      opts
    (if (memq var loopy--optimized-accum-vars)
        (progn
          (loopy--update-accum-place-count loopy--loop-name var pos)
          `((loopy--accumulation-vars (,var nil))
            (loopy--main-body
             (loopy--optimized-accum '( :loop ,loopy--loop-name :var ,var
                                        :val ,val :cmd ,cmd :name ,name :at ,pos
                                        :key ,key :test ,test)))))
      (loopy--check-accumulation-compatibility loopy--loop-name var 'list cmd)
      `((loopy--accumulation-vars (,var nil))
        ,@(cond
           ((member pos '(start beginning 'start 'beginning))
            (loopy--instr-let-const* ((test-val test)
                                      (key-val key))
                loopy--accumulation-vars
              `((loopy--main-body
                 (setq ,var (nconc (cl-delete-if
                                    ,(loopy--get-union-test-method var
                                                                   :test test-val
                                                                   :key key-val)
                                    (copy-sequence ,val))
                                   ,var))))))
           ((member pos '(end 'end))
            (loopy--produce-union-end-tracking var val
                                               :test test
                                               :key key
                                               :destructive nil))
           (t
            (signal 'loopy-bad-position-command-argument (list pos cmd))))
        (loopy--vars-final-updates (,var . nil)))))
  :implicit
  (loopy--plist-bind (:at (pos 'end) :key key :test (test (quote #'equal)))
      opts
    (setq pos (loopy--normalize-symbol pos))
    (when (eq pos 'beginning) (setq pos 'start))
    (unless (memq pos '(start beginning end))
      (signal 'loopy-bad-position-command-argument (list pos cmd)))
    (loopy--update-accum-place-count loopy--loop-name var pos)
    `((loopy--accumulation-vars (,var nil))
      (loopy--implicit-return ,var)
      (loopy--main-body
       (loopy--optimized-accum '( :loop ,loopy--loop-name :var ,var
                                  :val ,val :cmd ,cmd :name ,name :at ,pos
                                  :key ,key :test ,test))))))

;;;;;;; Vconcat
(defun loopy--construct-accum-vconcat (plist)
  "Create accumulation code for `vconcat' from PLIST.

This function is called by `loopy--expand-optimized-accum'."
  (loopy--plist-bind ( :cmd cmd :loop loop :var var :val val
                       :at (pos 'end))
      plist
    (map-let (('start start)
              ('end end))
        (loopy--get-accum-counts loop var 'vconcat)
      ;; Forward list order.
      (if (>= start end)
          (progn
            (loopy--check-accumulation-compatibility loop var 'vector cmd)
            `(,@(if (eq pos 'start)
                    `((loopy--main-body (setq ,var (cons ,val ,var))))
                  (loopy--produce-collect-end-tracking var val))
              (loopy--vars-final-updates
               (,var . (setq ,var (apply #'vconcat ,var))))))
        ;; Reverse list order.
        (loopy--check-accumulation-compatibility loop var 'reverse-vector cmd)
        `(,@(if (eq pos 'start)
                (loopy--produce-collect-end-tracking var val)
              `((loopy--main-body (setq ,var (cons ,val ,var)))))
          (loopy--vars-final-updates
           (,var . (setq ,var (apply #'vconcat (nreverse ,var))))))))))

(loopy--defaccumulation vconcat
  "Parse the `vconcat' command as (vconcat VAR VAL &key at)."
  :keywords (at)
  :explicit (loopy--plist-bind (:at (pos 'end))
                opts
              (setq pos (loopy--normalize-symbol pos))
              (when (eq pos 'beginning) (setq pos 'start))
              (unless (memq pos '(start beginning end))
                (signal 'loopy-bad-position-command-argument (list pos cmd)))
              (if (memq var loopy--optimized-accum-vars)
                  (progn
                    (loopy--update-accum-place-count loopy--loop-name var pos)
                    `((loopy--accumulation-vars (,var nil))
                      (loopy--main-body
                       (loopy--optimized-accum
                        '( :loop ,loopy--loop-name :var ,var :val ,val
                           :cmd ,cmd :name ,name :at ,pos)))))
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
                            (signal 'loopy-bad-position-command-argument (list pos cmd))))))
                  (loopy--vars-final-updates (,var . nil)))))
  :implicit (loopy--plist-bind (:at (pos 'end))
                opts
              (setq pos (loopy--normalize-symbol pos))
              (when (eq pos 'beginning) (setq pos 'start))
              (unless (memq pos '(start beginning end))
                (signal 'loopy-bad-position-command-argument (list pos cmd)))
              (loopy--update-accum-place-count loopy--loop-name var pos)
              `((loopy--accumulation-vars (,var nil))
                (loopy--main-body
                 (loopy--optimized-accum
                  '( :loop ,loopy--loop-name :var ,var :val ,val
                     :cmd ,cmd :name ,name :at ,pos)))
                (loopy--implicit-return ,var))))

;;;;; Boolean Commands
;;;;;; Always
(loopy--defaccumulation always
  "Parse a command of the form `(always VAR CONDITION &key into)'.

If CONDITION is nil, `loopy' should immediately return nil.
Otherwise, `loopy' should return the final value of CONDITION,
or t if the command is never evaluated."
  :category boolean-always-never
  :explicit `((loopy--accumulation-vars (,var t))
              (loopy--implicit-return ,var)
              (loopy--main-body (setq ,var ,val))
              ,@(loopy--bind-main-body (main-body rest)
                    (loopy--parse-leave-command 'ignored-arg)
                  (cons `(loopy--main-body (unless ,var ,@main-body))
                        rest))))

;;;;;; Never
(loopy--defaccumulation never
  "Parse a command of the form `(never VAR CONDITION &key into)'.

If CONDITION is t, `loopy' should immediately return nil.
Otherwise, `loopy' should return t."
  :category boolean-always-never
  :explicit `((loopy--accumulation-vars (,var t))
              (loopy--implicit-return ,var)
              ,@(loopy--bind-main-body (main-body rest)
                    (loopy--parse-leave-command 'ignored-arg)
                  (cons `(loopy--main-body (when ,val
                                             (setq ,var nil)
                                             ,@main-body))
                        rest))))

;;;;;; Thereis
(loopy--defaccumulation thereis
  "Parse the `thereis' command as (thereis VAR CONDITION &key into).

If CONDITION is non-nil, its value is immediately returned
and the loop is exited.  Otherwise the loop continues and nil is
returned."
  :category boolean-thereis
  :explicit `((loopy--accumulation-vars (,var nil))
              (loopy--implicit-return ,var)
              (loopy--main-body (setq ,var ,val))
              ,@(loopy--bind-main-body (main-body rest)
                    (loopy--parse-leave-command 'ignored-arg)
                  (cons `(loopy--main-body (when ,val ,@main-body))
                        rest))))

;;;;; Exiting and Skipping
;;;;;; Leave
(cl-defun loopy--parse-leave-command (_)
  "Parse the `leave' command."
  (let ((tag-name (loopy--produce-non-returning-exit-tag-name loopy--loop-name)))
    `((loopy--non-returning-exit-used ,tag-name)
      (loopy--main-body (throw (quote ,tag-name) t)))))

(cl-defun loopy--parse-leave-from-command ((_ target-loop))
  "Parse the `leave-from' command."
  (loopy--check-target-loop-name target-loop)
  (let ((tag-name (loopy--produce-non-returning-exit-tag-name target-loop)))
    `((loopy--at-instructions (,target-loop
                               (loopy--non-returning-exit-used ,tag-name)))
      (loopy--main-body (throw (quote ,tag-name) t)))))

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
      (loopy--main-body (throw (quote ,tag-name) t)))))

(cl-defun loopy--parse-skip-from-command ((_ target-loop))
  "Parse the `skip-from' loop command as (skip-from LOOP-NAME)."
  (loopy--check-target-loop-name target-loop)
  (let ((tag-name (loopy--produce-skip-tag-name target-loop)))
    `((loopy--at-instructions (,target-loop
                               (loopy--skip-used ,tag-name)))
      (loopy--main-body (throw (quote ,tag-name) t)))))

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
                                (until `((throw (quote ,tag-name) t)))
                                (while `(nil (throw (quote ,tag-name) t)))))))))

;;;; Destructuring

(defun loopy--destructure-for-generalized-command (var value-expression)
  "Destructure for commands that use generalized (`setf'-able) places.

Return a list of instructions for naming these `setf'-able places.

VAR are the variables into to which to destructure the value of
VALUE-EXPRESSION."
  (let ((destructurings
         (loopy--destructure-generalized-sequence var value-expression))
        (instructions nil))
    (dolist (destructuring destructurings)
      (push (list 'loopy--generalized-vars
                  destructuring)
            instructions))
    (nreverse instructions)))

(defun loopy--destructure-for-iteration-default (var val)
  "Destructure VAL according to VAR.

Returns a list.  The elements are:
1. An expression which binds the variables in VAR to the values
   in VAL.
2. A list of variables which exist outside of this expression and
   need to be `let'-bound."
  (let ((res (loopy--pcase-destructure-for-iteration `(loopy ,var) val :error t)))
    (if (null (cl-second res))
        (signal 'loopy-destructure-vars-missing (list var val))
      res)))

(defun loopy--destructure-for-iteration (var val)
  "Destructure VAL according to VAR.

Returns a list.  The elements are:
1. An expression which binds the variables in VAR to the values
   in VAL.
2. A list of variables which exist outside of this expression and
   need to be `let'-bound."
  (funcall (or loopy--destructuring-for-iteration-function
               #'loopy--destructure-for-iteration-default)
           var val))

;; TODO: Rename these so that the current "iteration" features
;;       are "generic" and the new "iteration" features
;;       a special case of the new "generic" features.
(defun loopy--destructure-for-iteration-command (var value-expression)
  "Return command instructions to destructure VALUE-EXPRESSION according to VAR.

Note that this does not apply to commands which use generalized
variables (`setf'-able places).  For that, see the function
`loopy--destructure-for-generalized-command'.

Return a list of instructions for initializing the variables and
destructuring into them in the loop body."
  (if (symbolp var)
      `((loopy--iteration-vars (,var nil))
        (loopy--main-body (setq ,var ,value-expression)))
    (cl-destructuring-bind (destructuring-expression var-list)
        (loopy--destructure-for-iteration var value-expression)
      `((loopy--main-body ,destructuring-expression)
        ,@(mapcar (lambda (x) `(loopy--iteration-vars (,x nil)))
                  var-list)))))

(defun loopy--destructure-for-other-command (var value-expression)
  "Destructure VALUE-EXPRESSION according to VAR for a loop command.

Note that this does not apply to commands which use generalized
variables (`setf'-able places).  For that, see the function
`loopy--destructure-for-generalized-command'.

Return a list of instructions for initializing the variables and
destructuring into them in the loop body.

A wrapper around `loopy--destructure-for-iteration-command'."
  (loopy--convert-iteration-vars-to-other-vars
   (loopy--destructure-for-iteration-command var value-expression)))

(cl-defun loopy--parse-destructuring-accumulation-command-default
    ((name var val &rest args))
  "Return instructions for destructuring accumulation commands.

Unlike `loopy--destructure-for-iteration-command', this function
does destructuring and returns instructions.

NAME is the name of the command.  VAR is a variable name.  VAL is a value."
  (loopy--pcase-parse-for-destructuring-accumulation-command
   `(,name (loopy ,var) ,val ,@args)
   :error t))

;;;; Selecting parsers
(defun loopy--parse-loop-command (command)
  "Parse COMMAND, returning a list of instructions in the same received order.

To allow for some flexibility in the command parsers, any nil
instructions are removed.

This function gets the parser, and passes the command to that parser."
  (let* ((parser (loopy--get-command-parser (cl-first command)))
         (instructions (remq nil (funcall parser command))))
    (or instructions
        (signal 'loopy-parser-instructions-missing
                (list command parser)))))

;; TODO: Allow for commands to return single instructions, instead of requiring
;; list of instructions.
(defun loopy--parse-loop-commands (command-list)
  "Parse commands in COMMAND-LIST via `loopy--parse-loop-command'.
Return a single list of instructions in the same order as
COMMAND-LIST."
  (mapcan #'loopy--parse-loop-command command-list))

(cl-defun loopy--get-command-parser (command-name &key (parsers loopy-command-parsers))
  "Get the parsing function for COMMAND-NAME.

The following variables are checked:

1. `loopy-aliases'
2. `loopy-command-parsers' or the value of PARSERS

Failing that, an error is signaled."

  (let ((true-name (loopy--get-true-name command-name)))
    (or (map-elt parsers true-name)
        (signal 'loopy-unknown-command (list command-name)))))

(provide 'loopy-commands)

;;; loopy-commands.el ends here
