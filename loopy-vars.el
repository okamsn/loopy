;;; loopy-vars.el --- Variables used by Loopy -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Earl Hyatt

;; Author: Earl Hyatt
;; Created: August 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.11.0
;; Package-Requires: ((emacs "27.1") (loopy "0.11.0"))
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
;; This file provides the package's user options and the variables `loopy' uses
;; for macro expansion and running the loop.
;;
;; For more information, see this package's Info documentation under Info node
;; `(loopy)'.

;;; Code:

(require 'map)
(require 'cl-lib)
(require 'loopy-misc)
(require 'seq)

;;;; User Options
(defgroup loopy nil
  "A looping and iteration macro."
  :group 'tools
  :prefix "loopy-"
  :link '(url-link "https://github.com/okamsn/loopy"))

;;;###autoload
(defcustom loopy-default-flags nil
  "Which flags should alter the behavior of `loopy' by default.

This is a list of symbols, each symbol corresponding to a
function in the variable `loopy--flag-settings'."
  :type '(repeat symbol))

;;;###autoload
(defmacro loopy-defalias (alias definition)
  "Add alias ALIAS for loop command DEFINITION.

Definition must exist.  Neither argument need be quoted."
  `(let ((alias (quote ,(loopy--get-quoted-symbol alias)))
         (definition (quote ,(loopy--get-quoted-symbol definition))))
     (let ((true-name (loopy--get-true-name definition)))
       (cond
        ((eq alias definition)
         (error "Can't alias name to itself: `%s' -> `%s'"
                alias definition))
        ((eq alias true-name)
         (error "Can't alias name to itself: `%s' -> `%s' -> ... -> `%s'"
                alias definition true-name))
        (t
         ;; Remove previous uses of that alias from all other names.
         (setq loopy-aliases (map-apply (lambda (k v)
                                          (cons k (remq alias v)))
                                        loopy-aliases))
         ;; Add the alias for the new target name.
         (push alias (map-elt loopy-aliases true-name)))))))

;;;###autoload
(defcustom loopy-aliases
  ;; TODO: Is there a faster way to search for aliases?
  ;;       Would using a hash table with a flatter structure be better?
  ;;       Using `map-do' on a hash table seemed to be a bit slower for what
  ;;       we want?
  '((accumulate      . (callf2 accumulating))
    (adjoin          . (adjoining))
    (after-do        . (else after else-do))
    (append          . (appending))
    (array           . (arraying string stringing across))
    (array-ref       . ( arraying-ref arrayf arrayingf
                         stringf stringingf
                         string-ref stringing-ref
                         across-ref))
    (at              . (atting))
    (before-do       . (initially-do initially before))
    (collect         . (collecting))
    (concat          . (concating))
    (cons            . (conses consing on))
    (count           . (counting))
    (cycle           . (cycling repeat repeating))
    (finally-do      . (finally))
    (finally-protect . (finally-protected))
    (find            . (finding))
    (flag            . (flags))
    (iter            . (iterating))
    (leave           . (leaving))
    (leave-from      . (leaving-from))
    (list            . (listing each in))
    (list-ref        . (listf listingf listing-ref in-ref))
    (map             . (mapping map-pairs mapping-pairs))
    (map-ref         . (mapf mappingf mapping-ref))
    (max             . (maximizing maximize maxing))
    ;; Unlike "maxing", there doesn't seem to be much on-line about the word
    ;; "minning", but the double-N follows conventional spelling rules, such as
    ;; in "sum" and "summing".
    (min             . (minimizing minimize minning))
    (multiply        . (multiplying))
    (nconc           . (nconcing))
    (numbers            . (num nums number numbering))
    (numbers-down       . ( nums-down numdown  number-down num-down numsdown
                            numbering-down))
    (numbers-up         . ( nums-up numup number-up num-up  numsup
                            numbering-up))
    (nunion          . (nunioning))
    (opt-accum       . (accum-opt))
    (prepend         . (prepending))
    (push-into       . (push pushing pushing-into))
    (reduce          . (reducing callf))
    (return          . (returning))
    (return-from     . (returning-from))
    (set             . (setting exprs expr))
    (set-accum       . (setting-accum))
    (set-prev        . (setting-prev prev prev-expr prev-set))
    (seq             . (seqing sequence sequencing elements))
    (seq-index       . ( sequence-index seqing-index sequencing-index
                         sequencing-index sequencei seqi list-index listing-index
                         listi array-index arraying-index arrayi
                         string-index stringing-index stringi))
    (seq-ref         . ( seqf seqing-ref
                         sequencef sequencingf sequence-ref
                         sequencing-ref
                         elements-ref))
    (skip            . (skipping continue continuing))
    (skip-from       . (skipping-from continue-from continuing-from))
    (sub-loop        . (sub-looping subloop sublooping loop looping))
    (sum             . (summing))
    (union           . (unioning))
    (vconcat         . (vconcating))
    (with            . (let* init))
    (without         . (no-with no-init)))
  "Aliases for loopy commands and special macro arguments.

This variable should not be modified directly.  For forward
compatibility, use `loopy-defalias'.  For now, these are pairs of
true names and lists of aliases.

 This user option is an alternative to modifying
`loopy-command-parsers' when the command parser is unknown."
  :group 'loopy
  :type '(alist :key-type symbol :value-type (repeat symbol)))

;;;###autoload
(defcustom loopy-command-parsers
  ;; TODO: This would probably be faster as a hash table,
  ;;       but then not as customizable.
  '((accumulate   . loopy--parse-accumulate-command)
    (always       . loopy--parse-always-command)
    (append       . loopy--parse-append-command)
    (adjoin       . loopy--parse-adjoin-command)
    (array        . loopy--parse-array-command)
    (array-ref    . loopy--parse-array-ref-command)
    (at           . loopy--parse-at-command)
    (collect      . loopy--parse-collect-command)
    (command-do   . loopy--parse-group-command)
    (concat       . loopy--parse-concat-command)
    (cond         . loopy--parse-cond-command)
    (cons         . loopy--parse-cons-command)
    (count        . loopy--parse-count-command)
    (cycle        . loopy--parse-cycle-command)
    (do           . loopy--parse-do-command)
    (find         . loopy--parse-find-command)
    (set-accum    . loopy--parse-set-accum-command)
    (group        . loopy--parse-group-command)
    (if           . loopy--parse-if-command)
    (iter         . loopy--parse-iter-command)
    (leave        . loopy--parse-leave-command)
    (leave-from   . loopy--parse-leave-from-command)
    (list         . loopy--parse-list-command)
    (list-ref     . loopy--parse-list-ref-command)
    (loopy        . loopy--parse-loopy-command)
    (map          . loopy--parse-map-command)
    (map-ref      . loopy--parse-map-ref-command)
    (max          . loopy--parse-max-command)
    (min          . loopy--parse-min-command)
    (multiply     . loopy--parse-multiply-command)
    (never        . loopy--parse-never-command)
    (nconc        . loopy--parse-nconc-command)
    (numbers      . loopy--parse-numbers-command)
    (numbers-up   . loopy--parse-numbers-up-command)
    (numbers-down . loopy--parse-numbers-down-command)
    (nunion       . loopy--parse-nunion-command)
    (prepend      . loopy--parse-prepend-command)
    (push-into    . loopy--parse-push-into-command)
    (reduce       . loopy--parse-reduce-command)
    (return       . loopy--parse-return-command)
    (return-from  . loopy--parse-return-from-command)
    (seq          . loopy--parse-seq-command)
    (seq-index    . loopy--parse-seq-index-command)
    (seq-ref      . loopy--parse-seq-ref-command)
    (set          . loopy--parse-set-command)
    (set-prev     . loopy--parse-set-prev-command)
    (skip         . loopy--parse-skip-command)
    (skip-from    . loopy--parse-skip-from-command)
    (sub-loop     . loopy--parse-sub-loop-command)
    (sum          . loopy--parse-sum-command)
    (thereis      . loopy--parse-thereis-command)
    (union        . loopy--parse-union-command)
    (unless       . loopy--parse-when-unless-command)
    (until        . loopy--parse-while-until-commands)
    (vconcat      . loopy--parse-vconcat-command)
    (when         . loopy--parse-when-unless-command)
    (while        . loopy--parse-while-until-commands))
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

  (add-to-list \'loopy-command-parsers
                (cons 'when #'my-loopy-parse-when-command))"
  :group 'loopy
  :type '(alist :key-type symbol :value-type function))



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


;;;; Special Macro Arguments
;; These only set in the `loopy' macro, but that might change in the future.  It
;; might be cleaner code to modify from the parsing function, after the macro
;; has already set them to nil.

(defvar loopy--special-macro-arguments
  '( flag with without before-do after-do finally-do finally-return wrap
     finally-protect accum-opt)
  "List of base names of built-in special macro arguments.

These are only the base names as found in `loopy-aliases'.")

(defvar loopy--special-maro-argument-processors
  '(loopy--process-special-arg-loop-name
    loopy--process-special-arg-flag
    loopy--process-special-arg-with
    loopy--process-special-arg-without
    loopy--process-special-arg-accum-opt
    loopy--process-special-arg-wrap
    loopy--process-special-arg-before-do
    loopy--process-special-arg-after-do
    loopy--process-special-arg-finally-do
    loopy--process-special-arg-finally-return
    loopy--process-special-arg-finally-protect)
  "Processing functions for special macro arguments.
These functions must be run in order.")

(defvar loopy--loop-name nil
  "A symbol that names the loop, appropriate for use in `cl-block'.")

(defvar loopy--known-loop-names nil
  "The stack of symbols of currently expanding loops.

This is used to check for errors with the `at' command.")


(defvar loopy--flags nil
  "Symbols/flags whose presence changes the behavior of `loopy'.

NOTE: This functionality might change in the future.")

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

(defvar loopy--wrapping-forms nil
  "Forms that should wrap the loop body, applied in order.

A form can be either a list or a symbol.  If a list, the loop
body is inserted into the end of the list.  If a symbol, the
symbol is applied as a function to the loop body.  This is
similar in use to the macros `thread-first' and `thread-last'.

These forms fall under the variable definitions used by the
loop (that is, they occur in the `let'-body instead of
surrounding it).  Only the loop body is wrapped.  If you wish to
wrap the return values and other parts of the macro expansion,
just wrap the macro expression as you normally would.")

(defvar loopy--before-do nil
  "A list of expressions to evaluate before the loop starts.
This is done using a `progn'.")




;;;; Loop Commands
;;;;; At Commands
(defvar loopy--at-instructions nil
  "These are instructions that affect a loop higher up the call list.

Mostly, these are variable declarations, as determined by
`loopy--external-at-targets'.

The form of the instructions that eventually set values in this variable
are `(loopy--at-instructions (LOOP-NAME INSTRUCTION INSTRUCTION ...))'.

These instructions are removed when that loop expansion is complete.")

(defvar loopy--valid-external-at-targets
  ;; Iteration vars currently needed for `expr'.
  ;;
  ;; TODO: We should probably change what the variables are named
  '( loopy--iteration-vars
     loopy--accumulation-vars
     loopy--vars-final-updates
     loopy--skip-used
     loopy--non-returning-exit-used
     loopy--implicit-return)
  "Valid targets for instructions pushed upwards by the `at' command.

Instructions not in this list are interpreted by the current
loop.")

;;;;; Loop Body Settings
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

(defvar loopy--skip-used nil
  "Whether a skip/continue command is present in the loop main body.")

(defvar loopy--skip-tag-name nil
  "The symbol used for the `cl-body' tag use by the `skip' command.")

(defvar loopy--non-returning-exit-used nil
  "Whether a command uses a tag-body to jump to the end of the `cl-block'.

This has the effect of leaving the loop without immediately
returning a value.")

(defvar loopy--non-returning-exit-tag-name nil
  "The tag used by the `leave', `while', and `until' commands.")

(defvar loopy--always-command-used nil
  "Whether an `always' command was used.
When an `always' command is used, the behavior of the `never' command changes.")

(defvar loopy--after-do nil
  "Expressions to run (in order) after the loop successfully completes.
These run in a `progn'.")

(defvar loopy--final-do nil
  "A list of expressions always run (in order) after the loop finishes/exits.")

(defvar loopy--final-protect nil
  "A list of expression always run, even if an error occurs in the loop body.")

(defvar loopy--final-return nil
  "What the macro finally returns.  This overrides any early return value.")

(defvar loopy--implicit-return nil
  "The implicit return value of loops that use accumulation commands.

This variable will contain a list of expressions that will be
returned by the macro if no other value is returned.")

;;;;; Loop Command Variables

(defvar loopy-result nil
  "The result of using implicit accumulation commands in `loopy'.

All accumulation commands with no given variable (such
as `(collect my-val)') will accumulate into `loopy-result'.

While `loopy-result' is an implied return value, it need not be
the only implied value, and can still be returned in a list with
other implied return values, if any.")

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

These initializations are sensitive to order.

This list includes variables explicitly named in a command (such
as the `collection' in `(collect collection value)') and variables
required for destructuring in accumulation commands.

Unlike in `loopy--iteration-vars', these variables should be
accessible from anywhere in the macro, and should not be reset
for sub-loops.")

(defvar loopy--other-vars nil
  "Variables that don't fit with others (or have their restrictions).

These are variables that are not from iteration commands and
accumulation commands.  For example, variables bound by
the `set' command.")

(defvar loopy--accumulation-list-end-vars nil
  "Associations of accumulation variables and variables pointing to their ends.

This map has two levels.  The first key is the current loop
name (as stored in `loopy--loop-name').  The second key is the
symbol naming the accumulation variable.  The value is a symbol
naming the end-tracking variable.  Therefore, an entry has the
form ((LOOP . VAR) . TRACKING-VAR).

Pairs at the top of the list are from the current loop, and are
removed when the instructions from that loop are done being
processed.

This variable is treated like a stack.  Alist entries are pushed
onto this stack while processing the loop, and are popped off
after the current loop is processed.

When working with lists, it is useful to be able to reference the
last link in the list.  This makes appending to the end of the
list much easier.  When using multiple accumulation commands, it
is important that such commands use the same variable to keep
track of the end of the list.")

(define-obsolete-variable-alias 'loopy--accumulation-final-updates
  'loopy--vars-final-updates "2022-11")
(defvar loopy--vars-final-updates nil
  "Alist of actions to perform on variables after the loop ends.

This variable's instructions are of the form `(VAR . ACTION)'.
To avoid accidentally updating a variable multiple times (such as
reversing a list twice), each VARIABLE can only be updated in a
single way.")

;;;;; Command Error Checking
(defvar loopy--accumulation-variable-info nil
  "Information about accumulation variables to ensure command compatibility.

Information is of the form (VARIABLE-NAME CATEGORY COMMAND).
Current categories are `list', `string', `vector', `value', and
`reverse-list'.

This variable is treated like a stack.  Alist entries are pushed
onto this stack while processing the loop, and are popped off
after the current loop is processed.

These entries are not instructions.  They are derived from
`loopy--vars-final-updates' while processing instructions
during macro expansion.  See
`loopy--check-accumulation-compatibility' for more.")

(defvar loopy--in-sub-level nil
  "Whether the commands parsed are not in the top level of a loop.

Certain commands (e.g., `list' or `array') can only occur in the
top level of a loop.  Sub-loops (those created by the `sub-loop'
command) create for themselves a new, local top level.")

;;;;; Optimized Accumulations
(defvar loopy--accumulation-places nil
  "Where some accumulation commands are placing values.

This variable keeps track some of the accumulation variables in a
loop and how there being used.  This allows for optimizing some
kinds accumulations.

Generally, this is used with commands that produce lists, such as
`collect' and `append'.")

(defun loopy--get-accum-counts (loop var cmd-name)
  "Get the count of accumulation places for VAR in LOOP.
CMD-NAME is used for signaling errors."
  (or (map-nested-elt loopy--accumulation-places (list loop var))
      (signal 'loopy-missing-accum-counters (list cmd-name))))

(defvar loopy--accumulation-constructors
  '((adjoin .  loopy--construct-accum-adjoin)
    (append .  loopy--construct-accum-append)
    (collect . loopy--construct-accum-collect)
    (concat . loopy--construct-accum-concat)
    (nconc . loopy--construct-accum-nconc)
    (nunion . loopy--construct-accum-nunion)
    (union . loopy--construct-accum-union)
    (vconcat . loopy--construct-accum-vconcat))
  "Functions that produce the code of an optimized accumulation.

This is used by the function `loopy--get-optimized-accum'.")

(defvar loopy--optimized-accum-vars nil
  "Explicit accumulations variables to optimize.

Arguments to the `accum-opt' special macro argument are symbols
or list of a symbol and a position.")

;;;;; Pseudo-macro expansion
;; TODO: This might need to be more aggressive.
(defvar loopy--suppressed-macros '(cl-block cl-return cl-return-from)
  "Macros that shouldn't be expanded as the `loopy' expansion is built.

Some macros interact in a way if one is expanded without the
context of the other.  Others might not work for other reasons.
The macros `cl-block', `cl-return-from', and `cl-return' are
known to fall into the first group.")

;;;; All variables
(eval-and-compile
  (defvar loopy--variables
    '(loopy--loop-name
      loopy--with-vars
      loopy--without-vars
      loopy--before-do
      loopy--wrapping-forms
      loopy--after-do
      loopy--final-do
      loopy--final-protect
      loopy--final-return

      ;; -- Vars for processing loop commands --
      ;; NOTE: `loopy--at-instructions' cannot be local to each loop:
      ;; loopy--at-instructions
      loopy--iteration-vars
      loopy--optimized-accum-vars
      loopy--accumulation-vars
      loopy--generalized-vars
      loopy--other-vars
      loopy--pre-conditions
      loopy--main-body
      loopy--latter-body
      loopy--post-conditions
      loopy--implicit-return

      ;; -- Variables for constructing code --
      loopy--skip-tag-name
      loopy--skip-used
      loopy--non-returning-exit-tag-name
      loopy--non-returning-exit-used
      loopy--vars-final-updates
      ;; loopy--accumulation-list-end-vars
      ;; loopy--accumulation-variable-info
      loopy--in-sub-level

      ;; -- Flag Variables --
      loopy--destructuring-for-with-vars-function
      loopy--destructuring-for-iteration-function
      loopy--destructuring-accumulation-parser
      loopy--split-implied-accumulation-results)
    "These variables must be `let'-bound around the loop.

This list is mainly fed to the macro `loopy--wrap-variables-around-body'."))

;;;; Functions to for macro expansion

(defun loopy--with-bound-p (var-name)
  "Whether VAR-NAME is bound in `loopy--with-vars' or `loopy--without-vars'.

Some iteration commands can produce more efficient code if there
is no request for a specific initialization value."
  (or (cl-loop for (var val) in loopy--with-vars
               when (eq var var-name)
               return (cons 'with val))
      (cl-loop for x in loopy--without-vars
               when (eq x var-name)
               return (cons 'without nil))))

(defun loopy--command-bound-p (var-name)
  "Whether VAR-NAME was bound by a command (and not a special macro argument).

The variable can exist in `loopy--iteration-vars',
`loopy--accumulation-vars', or `loopy--generalized-vars'."
  (or (cl-loop for (var val) in loopy--iteration-vars
               when (eq var var-name)
               return (cons 'iteration val))
      (cl-loop for (var val) in loopy--accumulation-vars
               when (eq var var-name)
               return (cons 'accumulation val))
      (cl-loop for (var val) in loopy--generalized-vars
               when (eq var var-name)
               return (cons 'generalized val))))

(defun loopy--bound-p (var-name)
  "Check if VAR-NAME (a symbol) is already bound for the macro.

This can happen when multiple loop commands refer to the same
variable, or when a variable is introduced via `with'.

See also `loopy--with-bound-p' and `loopy--command-bound-p'."
  (or (loopy--with-bound-p var-name)
      (loopy--command-bound-p var-name)))

(defun loopy--already-implicit-return (expression)
  "Check whether EXPRESSION is in the list of implied return values.

Accumulation commands can operate on the same variable, and we
  don't want that variable to appear more than once as an implied return."
  (member expression loopy--implicit-return))

(defun loopy--special-macro-argument-p (symbol arguments-list)
  "Whether SYMBOL is a special macro argument (including aliases).

Special macro arguments are listed in ARGUMENTS-LIST
or `loopy-aliases'."
  (memq symbol (append arguments-list
                       (let ((results))
                         (dolist (alias loopy-aliases)
                           (when (memq (cdr alias) arguments-list)
                             (push (car alias) results)))
                         results))))

(defun loopy--known-loop-name-p (target)
  "Whether TARGET is a known loop name."
  (memq target loopy--known-loop-names))

(defun loopy--check-target-loop-name (target)
  "Signal an error whether TARGET is not a valid loop name."
  (unless (loopy--known-loop-name-p target)
    (signal 'loopy-unknown-loop-target (list target))))

(defun loopy--check-position-name (pos)
  (unless (member pos '(start end beginning))
    (signal 'loopy-bad-position-command-argument (list pos))))

(defmacro loopy--wrap-variables-around-body (&rest body)
  "Wrap variables in `loopy--variables' in `let*' bindings around BODY."
  (macroexp-let* (mapcar (lambda (x) (list x nil))
                         loopy--variables)
                 (macroexp-progn body)))

(defun loopy--apply-flag (flag)
  "Apply the effects of the FLAG."
  (if-let ((func (map-elt loopy--flag-settings flag)))
      (funcall func)
    (error "Loopy: Flag not defined: %s" flag)))

(defun loopy--valid-external-at-target-p (target)
  "Check if variable TARGET is valid for an `at' command.

This predicate checks for presence in the list
`loopy--valid-external-at-targets'."
  (memq target loopy--valid-external-at-targets))

(cl-defun loopy--get-true-name (name)
  "Get the true name of possible alias NAME."
  (or (progn
        ;; Defensively return nil, since `map-do' in older versions
        ;; of `map.el' failed to return nil correctly.
        (map-do (lambda (k v)
                  (when (memq name v)
                    (cl-return-from loopy--get-true-name k)))
                loopy-aliases)
        nil)
      name))

(defun loopy--get-aliases (true-name)
  "Get the immediate aliases of TRUE-NAME.

See also `loopy--get-all-names', for when the true name
is not known."
  (map-elt loopy-aliases true-name))

(cl-defun loopy--get-all-names (name &key from-true ignored)
  "Get the true name of NAME and all of the true name's aliases.

If FROM-TRUE is non-nil, NAME is the true name.  IGNORED is a
list of names to be removed from the list of found names.

If no other names are found, a list of just NAME is returned.
This function does not check whether a name is known."
  (let ((names (if from-true
                   (cons name (loopy--get-aliases name))
                 (or (progn
                       ;; Defensively return nil, since `map-do' in
                       ;; older versions of `map.el' failed to
                       ;; return nil correctly.
                       (map-do (lambda (k v)
                                 (when (or (eq name k) (memq name v))
                                   (cl-return-from loopy--get-all-names
                                     (cons k v))))
                               loopy-aliases)
                       nil)
                     (list name)))))
    (if ignored
        (seq-difference names ignored #'eq)
      names)))

(provide 'loopy-vars)
;;; loopy-vars.el ends here
