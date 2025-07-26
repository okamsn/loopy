;;; loopy-vars.el --- Variables used by Loopy -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Earl Hyatt

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

(defun loopy--defalias-1 (alias definition)
  (if (eq alias definition)
      (error "Can't alias name to itself: `%s' -> `%s'"
             alias definition)
    (let ((true-name
           ;; Now that `loopy-aliases' is nil, we know that it can only
           ;; contain the true name by user intervention, in which
           ;; case it should have priority over `loopy-parsers'.
           (or (cl-loop for (orig . aliases) in loopy-aliases
                        when (memq definition aliases)
                        return orig)
               (and (map-contains-key loopy-parsers definition)
                    definition)
               (when-let* ((orig (gethash definition loopy--obsolete-aliases)))
                 (warn "`loopy': `%s' is an obsolete built-in alias of `%s'.  It will be removed in the future.  To add it as a custom alias, add it to `loopy-parsers'."
                       definition orig)
                 orig))))
      (if (eq alias true-name)
          (error "Can't alias name to itself: `%s' -> `%s' -> ... -> `%s'"
                 alias definition true-name)
        (if-let* ((fn (gethash true-name loopy-parsers)))
            (progn
              ;; Remove previous uses of that alias from all other names.
              ;; We don't want to trigger the setting warning unless we must,
              ;; so we check first.
              (when (map-some (lambda (_ v) (memq alias v))
                              loopy-aliases)
                (setq loopy-aliases (map-apply (lambda (k v)
                                                 (cons k (remq alias v)))
                                               loopy-aliases)))
              ;; Add the alias for the new target name.
              (puthash alias fn loopy-parsers))
          (error "Ultimate command `%S' for alias `%S' to `%S' is not a known command"
                 true-name alias definition))))))

;;;###autoload
(defmacro loopy-defalias (alias definition)
  "Add alias ALIAS for loop command DEFINITION.

Definition must exist.  Neither argument need be quoted."
  `(loopy--defalias-1 (quote ,(loopy--get-quoted-symbol alias))
                      (quote ,(loopy--get-quoted-symbol definition))))

(defvar loopy--obsolete-aliases
  #s(hash-table
     test eq
     data ( across        array
            arrayf        array-ref
            arrayingf     array-ref
            stringf       array-ref
            stringingf    array-ref
            across-ref    array-ref
            group         command-do
            on            cons
            in            list
            listf         list-ref
            listingf      list-ref
            in-ref        list-ref
            mapf          map-ref
            mappingf      map-ref
            num           numbers
            nums          numbers
            nums-down     numbers-down
            numdown       numbers-down
            num-down      numbers-down
            numsdown      numbers-down
            nums-up       numbers-up
            numup         numbers-up
            num-up        numbers-up
            numsup        numbers-up
            exprs         set
            expr          set
            prev          set-prev
            prev-expr     set-prev
            elements      sequence
            sequencei     sequence-index
            seqi          sequence-index
            listi         sequence-index
            arrayi        sequence-index
            stringi       sequence-index
            seqf          seq-ref
            seqingf       seq-ref
            sequencef     sequence-ref
            sequencingf   sequence-ref
            elements-ref  sequence-ref))
  "Aliases to be removed from the documentation.")

(defun loopy--expression-parser-map-p (obj)
  "Return when OBJ has the correct data for `loopy-expression-parsers'."
  (and (mapp obj)
       (map-every-p (lambda (k v)
                      (and (symbolp k)
                           (or (functionp v)
                               (and (symbolp k)
                                    (string-match-p "loopy--parse-.*-special-macro-argument"
                                                    (symbol-name v))))))
                    obj)))

(defvar loopy--parsers-internal nil
  "Internal version of `loopy-parsers' for current expansion.")

;;;###autoload
(defcustom loopy-parsers
  #s(hash-table
     test eq
     data (;; Special macro arguments
           ;;
           ;; NOTE: When editing the hash table, also edit the list of
           ;; pseudo-functions for special macro arguments used in the
           ;; definition of `loopy-iter'.
           accum-opt          loopy--parse-accum-opt-special-macro-argument
           opt-accum          loopy--parse-accum-opt-special-macro-argument
           after              loopy--parse-after-do-special-macro-argument
           after-do           loopy--parse-after-do-special-macro-argument
           else               loopy--parse-after-do-special-macro-argument
           else-do            loopy--parse-after-do-special-macro-argument
           before             loopy--parse-before-do-special-macro-argument
           before-do          loopy--parse-before-do-special-macro-argument
           initially          loopy--parse-before-do-special-macro-argument
           initially-do       loopy--parse-before-do-special-macro-argument
           finally            loopy--parse-finally-do-special-macro-argument
           finally-do         loopy--parse-finally-do-special-macro-argument
           finally-protect    loopy--parse-finally-protect-special-macro-argument
           finally-protected  loopy--parse-finally-protect-special-macro-argument
           finally-return     loopy--parse-finally-return-special-macro-argument
           flag               loopy--parse-flag-special-macro-argument
           flags              loopy--parse-flag-special-macro-argument
           init               loopy--parse-with-special-macro-argument
           let*               loopy--parse-with-special-macro-argument
           with               loopy--parse-with-special-macro-argument
           no-init            loopy--parse-without-special-macro-argument
           no-with            loopy--parse-without-special-macro-argument
           without            loopy--parse-without-special-macro-argument
           named              loopy--parse-named-special-macro-argument
           wrap               loopy--parse-wrap-special-macro-argument
           override           loopy--parse-override-special-macro-argument
           overrides          loopy--parse-override-special-macro-argument

           ;; Loop Commands
           accumulate        loopy--parse-accumulate-command
           accumulating      loopy--parse-accumulate-command
           callf2            loopy--parse-accumulate-command
           adjoin            loopy--parse-adjoin-command
           adjoining         loopy--parse-adjoin-command
           always            loopy--parse-always-command
           append            loopy--parse-append-command
           appending         loopy--parse-append-command
           array             loopy--parse-array-command
           arraying          loopy--parse-array-command
           string            loopy--parse-array-command
           stringing         loopy--parse-array-command
           array-ref         loopy--parse-array-ref-command
           arraying-ref      loopy--parse-array-ref-command
           string-ref        loopy--parse-array-ref-command
           stringing-ref     loopy--parse-array-ref-command
           at                loopy--parse-at-command
           atting            loopy--parse-at-command
           collect           loopy--parse-collect-command
           collecting        loopy--parse-collect-command
           command-do        loopy--parse-command-do-command
           concat            loopy--parse-concat-command
           concating         loopy--parse-concat-command
           cond              loopy--parse-cond-command
           cons              loopy--parse-cons-command
           conses            loopy--parse-cons-command
           consing           loopy--parse-cons-command
           count             loopy--parse-count-command
           counting          loopy--parse-count-command
           cycle             loopy--parse-cycle-command
           cycling           loopy--parse-cycle-command
           repeat            loopy--parse-cycle-command
           repeating         loopy--parse-cycle-command
           do                loopy--parse-do-command
           find              loopy--parse-find-command
           finding           loopy--parse-find-command
           if                loopy--parse-if-command
           iter              loopy--parse-iter-command
           iterating         loopy--parse-iter-command
           leave             loopy--parse-leave-command
           leaving           loopy--parse-leave-command
           leave-from        loopy--parse-leave-from-command
           leaving-from      loopy--parse-leave-from-command
           each              loopy--parse-list-command
           list              loopy--parse-list-command
           listing           loopy--parse-list-command
           list-ref          loopy--parse-list-ref-command
           listing-ref       loopy--parse-list-ref-command
           loopy             loopy--parse-loopy-command
           map               loopy--parse-map-command
           map-pairs         loopy--parse-map-command
           mapping           loopy--parse-map-command
           mapping-pairs     loopy--parse-map-command
           map-ref           loopy--parse-map-ref-command
           mapping-ref       loopy--parse-map-ref-command
           max               loopy--parse-max-command
           maximize          loopy--parse-max-command
           maximizing        loopy--parse-max-command
           maxing            loopy--parse-max-command
           min               loopy--parse-min-command
           minimize          loopy--parse-min-command
           minimizing        loopy--parse-min-command
           minning           loopy--parse-min-command
           multiply          loopy--parse-multiply-command
           multiplying       loopy--parse-multiply-command
           nconc             loopy--parse-nconc-command
           nconcing          loopy--parse-nconc-command
           never             loopy--parse-never-command
           number            loopy--parse-numbers-command
           numbering         loopy--parse-numbers-command
           numbers           loopy--parse-numbers-command
           number-down       loopy--parse-numbers-down-command
           numbering-down    loopy--parse-numbers-down-command
           numbers-down      loopy--parse-numbers-down-command
           number-up         loopy--parse-numbers-up-command
           numbering-up      loopy--parse-numbers-up-command
           numbers-up        loopy--parse-numbers-up-command
           nunion            loopy--parse-nunion-command
           nunioning         loopy--parse-nunion-command
           prepend           loopy--parse-prepend-command
           prepending        loopy--parse-prepend-command
           push              loopy--parse-push-into-command
           push-into         loopy--parse-push-into-command
           pushing           loopy--parse-push-into-command
           pushing-into      loopy--parse-push-into-command
           callf             loopy--parse-reduce-command
           reduce            loopy--parse-reduce-command
           reducing          loopy--parse-reduce-command
           return            loopy--parse-return-command
           returning         loopy--parse-return-command
           return-from       loopy--parse-return-from-command
           returning-from    loopy--parse-return-from-command
           seq               loopy--parse-seq-command
           seqing            loopy--parse-seq-command
           seq-ref           loopy--parse-seq-ref-command
           seqing-ref        loopy--parse-seq-ref-command
           sequence          loopy--parse-sequence-command
           sequencing        loopy--parse-sequence-command
           array-index       loopy--parse-sequence-index-command
           arraying-index    loopy--parse-sequence-index-command
           list-index        loopy--parse-sequence-index-command
           listing-index     loopy--parse-sequence-index-command
           seq-index         loopy--parse-sequence-index-command
           seqing-index      loopy--parse-sequence-index-command
           sequence-index    loopy--parse-sequence-index-command
           sequencing-index  loopy--parse-sequence-index-command
           string-index      loopy--parse-sequence-index-command
           stringing-index   loopy--parse-sequence-index-command
           sequence-ref      loopy--parse-sequence-ref-command
           sequencing-ref    loopy--parse-sequence-ref-command
           set-accum         loopy--parse-set-accum-command
           setting-accum     loopy--parse-set-accum-command
           set               loopy--parse-set-command
           setting           loopy--parse-set-command
           prev-set          loopy--parse-set-prev-command
           set-prev          loopy--parse-set-prev-command
           setting-prev      loopy--parse-set-prev-command
           continue          loopy--parse-skip-command
           continuing        loopy--parse-skip-command
           skip              loopy--parse-skip-command
           skipping          loopy--parse-skip-command
           continue-from     loopy--parse-skip-from-command
           continuing-from   loopy--parse-skip-from-command
           skip-from         loopy--parse-skip-from-command
           skipping-from     loopy--parse-skip-from-command
           stream            loopy--parse-stream-command
           streaming         loopy--parse-stream-command
           substream         loopy--parse-substream-command
           substreaming      loopy--parse-substream-command
           sum               loopy--parse-sum-command
           summing           loopy--parse-sum-command
           thereis           loopy--parse-thereis-command
           union             loopy--parse-union-command
           unioning          loopy--parse-union-command
           vconcat           loopy--parse-vconcat-command
           vconcating        loopy--parse-vconcat-command
           unless            loopy--parse-unless-command
           when              loopy--parse-when-command
           until             loopy--parse-while-until-commands
           while             loopy--parse-while-until-commands
           loopy-iter        loopy-iter--parse-loopy-iter-command))
  "Map of symbols to parsing functions.

This includes special macro arguments in addition to the loop commands.

Functions for special macro arguments are fake entries.  These entries
are used to identify special macro arguments.  These entries are _not_
used to find the function which parses special macro arguments.  How
special macro arguments are parsed is not configurable.  Special macro
arguments are handled specially.  See the Info node
`(loopy)Special Macro Arguments'.

Functions for parsing custom loop commands can be added to this mapping,
which are used after special macro arguments are processed.  See the Info node
`(loopy)Loop Commands' and the Info node `(loopy)Custom Commands'."
  :group 'loopy
  :type '(restricted-sexp :match-alternatives (loopy--expression-parser-map-p)))

(make-obsolete-variable 'loopy-command-parsers 'loopy-parsers "2025-07" 'set)
(defcustom loopy-command-parsers nil

  "An alist of pairs of a quoted command name and a parsing function.

This variable is obsolete.  See instead the customizable variable
`loopy-parsers'.

The parsing function is chosen based on the command name (such as
`list' in `(list i my-list)'), not the usage of the command.  That is,

  (my-command var1)

and

  (my-command var1 var2)

are both parsed by the same function, but that parsing function
is not limited in how it responds to different usages.  If you
really want, it can return different instructions each time.
Learn more in the Info node `(loopy)Custom Commands'.

For example, to add a `when' command (if one didn't already
exist), one could do

  (add-to-list \\='loopy-command-parsers
                (cons \\='when #\\='my-loopy-parse-when-command))"
  :group 'loopy
  :type '(alist :key-type symbol :value-type function))

(make-obsolete-variable 'loopy-aliases 'loopy-parsers "2025-07" 'set)
(defcustom loopy-aliases nil
  "Aliases for loopy commands and special macro arguments.

This variable is obsolete.  See instead the customizable variable
`loopy-parsers'.

This variable should not be modified directly.  For forward
compatibility, use `loopy-defalias'.  For now, these are pairs of
true names and lists of aliases.

 This user option is an alternative to modifying
`loopy-command-parsers' when the command parser is unknown."
  :group 'loopy
  :type '(alist :key-type symbol :value-type (repeat symbol)))


;;;; Flags
;;;;; Variables that can be set by flags
(defvar loopy--destructuring-for-with-vars-function nil
  "The function used for destructuring `with' variables.

This function named by this variables receives the bindings given
to the `with' macro argument and should usually return a list of
two elements:

1. A function/macro that works like `let*' and can be used to wrap
   the expanded macro code.
2. The bindings that will be given to this macro.

For example, an acceptable return value might be something like

    (list \\='pcase-let* BINDINGS)

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

If nil, use `loopy--parse-destructuring-accumulation-command-default'.")

;;;;; For setting up flags
(defvar loopy--flag-settings nil
  "Alist of functions to run on presence of their respective flag.

These functions will enable features.

Each item is of the form (FLAG . FLAG-ENABLING-FUNCTION).")


;;;; Special Macro Arguments
;; These only set in the `loopy' macro, but that might change in the future.  It
;; might be cleaner code to modify from the parsing function, after the macro
;; has already set them to nil.

(defvar loopy--loop-name nil
  "A symbol that names the loop, appropriate for use in `cl-block'.")

(defvar loopy--known-loop-names nil
  "The stack of symbols of currently expanding loops.

This is used to check for errors with the `at' command.")

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
  "The default variable used by accumulation and boolean commands in `loopy'.

When a variable is not specified for accumulation commands (such
as `collect') or for boolean commands (such as `always'), those
commands will use `loopy-result' for their operation and set it
as the implicit return value of the loop.

When optimized accumulation commands use `loopy-result', the
variables value is finalized after the loop ends.  Therefore, in
that situation, its value shouldn't be used while the loop is
running.

This variable can be safely referenced in code produced by
special macro arguments (such as `finally-return') after the loop
runs and its value is finalized.")

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
loop and how they are being used.  This allows for optimizing some
kinds accumulations.

Generally, this is used with commands that produce lists, such as
`collect' and `append'.")

(defun loopy--get-accum-counts (loop var cmd-name)
  "Get the count of accumulation places for VAR in LOOP.
CMD-NAME is used for signaling errors."
  (or (map-nested-elt loopy--accumulation-places (list loop var))
      (signal 'loopy-missing-accum-counters (list cmd-name))))

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

      loopy--parsers-internal

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
      loopy--destructuring-accumulation-parser)
    "These variables must be `let'-bound around the loop.

This list is mainly fed to the macro `loopy--wrap-variables-around-body'."))

;;;; Functions to for macro expansion

(defun loopy--with-bound-p (var-name)
  "Whether VAR-NAME is bound in `loopy--with-vars' or `loopy--without-vars'.

Some iteration commands (e.g., `reduce') will change their behavior
depending on whether the accumulation variable is given an initial
value."
  (or (cl-loop for (var val) in loopy--with-vars
               when (eq var var-name)
               return (cons 'with val))
      (cl-loop for x in loopy--without-vars
               when (eq x var-name)
               return (cons 'without nil))))

(defun loopy--command-bound-p (var-name)
  "Whether VAR-NAME was bound by a command (and not a special macro argument).

The variable can exist in `loopy--iteration-vars',
`loopy--accumulation-vars', `loopy--other-vars' (for commands like
`set'), or `loopy--generalized-vars'.

Re-initializing an iteration variable is an error."
  (or (cl-loop for (var val) in loopy--iteration-vars
               when (eq var var-name)
               return (cons 'iteration val))
      (cl-loop for (var val) in loopy--accumulation-vars
               when (eq var var-name)
               return (cons 'accumulation val))
      (cl-loop for (var val) in loopy--generalized-vars
               when (eq var var-name)
               return (cons 'generalized val))
      (cl-loop for (var val) in loopy--other-vars
               when (eq var var-name)
               return (cons 'other val))))

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

(defun loopy--check-target-loop-name (target)
  "Signal an error whether TARGET is not a valid loop name."
  (unless (memq target loopy--known-loop-names)
    (signal 'loopy-unknown-loop-target (list target))))

(defun loopy--check-position-name (pos)
  "Error if POS is not an accepted symbol describing how to add to a sequence.

Accepted places are the quoted symbols `start' or `end'.  The place
`beginning' is assumed to have been transformed by the function
`loopy--normalize-position-name' into `start' before calling
`loopy--check-position-name'.

For example, the `collect' command can add items at the beginning or end
of a sequence."
  (unless (member pos '(start end))
    (signal 'loopy-bad-position-command-argument (list pos))))

(defun loopy--normalize-position-name (pos)
  (pcase pos
    ((or 'beginning '(quote beginning) 'start '(quote start))
     'start)
    ((or 'end '(quote end))
     'end)
    (_
     (signal 'loopy-bad-position-command-argument (list pos)))))

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

(provide 'loopy-vars)
;;; loopy-vars.el ends here
