;;; loopy-iter.el --- A macro similar to CL's iterate  -*- lexical-binding: t; -*-

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
;; A macro similar to, and heavily inspired by, Common Lisp's Iterate package.
;; This package is somewhat like a translation of Iter into Loopy.
;;
;; See the info node `(loopy)The loopy-iter Macro'.

;;; Code:
(eval-when-compile (require 'loopy))
(require 'loopy)
(require 'loopy-vars)
(require 'loopy-misc)
(require 'loopy-commands)
(require 'loopy-instrs)
(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'macroexp)
(require 'subr-x)

;; How the Code Works:
;;
;; Instead of parsing commands using `loopy--parse-loop-command', we use
;; `loopy-iter--parse-command' with commands being expanded like a normal Lisp
;; macro.  The code in the value of main-body instructions is taken and further
;; expanded, while non-main-body instructions are stored and eventually
;; processed after processing all of the macro arguments.
;;
;; Previously, `loopy-iter' used its tree-walking functions, as done by CL's
;; Iterate, but now it just defers to what Emacs already does when expanding
;; macros, such as in `macroexpand-all'.
;;
;; NOTE: When using `macroexpand-all', always pass an environment like
;;       `macroexpand-all-environment'.  Otherwise, the environment is
;;       set to nil, which breaks any macros that build their own environment
;;       while expanding commands.  For example, `cl-flet'.

;;;; Custom User Options
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
  (loopy (with (loopy-iter--non-main-body-instructions nil)
               (loopy--loop-name target-loop)
               (loopy--in-sub-level t)
               (loopy-iter--level (1+ loopy-iter--level)))
         (list cmd commands)
         (collect (list 'loopy--main-body (macroexpand-all
                                           cmd
                                           macroexpand-all-environment)))
         (finally-return
          ;; Return list of instructions to comply with expectations of calling
          ;; function, which thinks that this is a normal loop-command parser.
          `(,@loopy-result
            (loopy--at-instructions
             (,target-loop
              ,@(thread-last loopy-iter--non-main-body-instructions
                             nreverse
                             (apply #'append))))))))

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

;; TODO: Combine this with `loopy--def-special-processor'.
(defmacro loopy-iter--def-special-processor (name &rest body)
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
  `(defun ,(intern (format "loopy-iter--process-special-arg-%s" name))
       (body)
     ,(format "Process the special macro argument `%s' and its aliases.

Returns BODY without the `%s' argument."
              name name)
     (loopy
      (accum-opt matching-args new-body)
      (with (all-names (loopy--get-all-names (quote ,name) :from-true t))
            (bare-names (loopy (list name all-names)
                               (when (memq name loopy-iter-bare-special-macro-arguments)
                                 (collect name)))))
      (listing expr body)
      (if (and (consp expr)
               (or (memq (cl-first expr) bare-names)
                   (and (memq (cl-first expr) loopy-iter-keywords)
                        (memq (cl-second expr) all-names))))
          (collecting matching-args expr)
        (collecting new-body expr))
      (finally-do (when matching-args
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
                        ,@body))))
      (finally-return
       new-body))))

(defun loopy-iter--process-special-arg-loop-name (body)
  "Process BODY and the loop name listed therein."
  (let* ((names)
         (new-body)
         (all-sma-names (loopy--get-all-names 'named :from-true t))
         (all-sma-bare-names
          (loopy (list name all-sma-names)
                 (when (memq name loopy-iter-bare-special-macro-arguments)
                   (collect name)))))
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
        (push (list loopy--loop-name) loopy--accumulation-places)
        ;; Return non-name args.
        (nreverse new-body)))))

(loopy-iter--def-special-processor with
  ;; Note: These values don't have to be used literally, due to
  ;;       destructuring.
  (loopy (list binding arg-value)
         (collect
          (cond ((symbolp binding)      (list binding nil))
                ((= 1 (length binding)) (list (cl-first binding) nil))
                (t                       binding)))
         (finally-do
          (setq loopy--with-vars loopy-result))))


(loopy-iter--def-special-processor finally-return
  (setq loopy--final-return (if (= 1 (length arg-value))
                                (cl-first arg-value)
                              (cons 'list arg-value))))

(loopy-iter--def-special-processor flag
  ;; Process any flags passed to the macro.  In case of conflicts, the
  ;; processing order is:
  ;;
  ;; 1. Flags in `loopy-default-flags'.
  ;; 2. Flags in the `flag' macro argument, which can undo the first group.
  ;; (mapc #'loopy--apply-flag loopy-default-flags)
  (mapc #'loopy--apply-flag arg-value))

(loopy-iter--def-special-processor without
  (setq loopy--without-vars arg-value))

(loopy-iter--def-special-processor accum-opt
  (pcase-dolist ((or `(,var ,pos) var) arg-value)
    (push var loopy--optimized-accum-vars)
    (when pos
      (loopy--update-accum-place-count loopy--loop-name var pos 1.0e+INF))))

(loopy-iter--def-special-processor wrap
  (setq loopy--wrapping-forms arg-value))

(loopy-iter--def-special-processor before-do
  (setq loopy--before-do arg-value))

(loopy-iter--def-special-processor after-do
  (setq loopy--after-do arg-value))

(loopy-iter--def-special-processor finally-do
  (setq loopy--final-do arg-value))

(loopy-iter--def-special-processor finally-protect
  (setq loopy--final-protect arg-value))

;;;; Misc

(defvar loopy-iter-suppressed-macros '(cl-block cl-return cl-return-from)
  "Macros that shouldn't be expanded as the `loopy-iter' expansion is built.

Some macros interact in a way where they might break if one is
expanded without the context of the other.  Others might not work
for other reasons.  The macros `cl-block', `cl-return-from', and
`cl-return' are known to fall into the first group.")

;;;; The macro itself
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

   (mapc #'loopy--apply-flag loopy-default-flags)

   (setq body (thread-first body
                            loopy-iter--process-special-arg-loop-name
                            loopy-iter--process-special-arg-flag
                            loopy-iter--process-special-arg-with
                            loopy-iter--process-special-arg-without
                            loopy-iter--process-special-arg-accum-opt
                            loopy-iter--process-special-arg-wrap
                            loopy-iter--process-special-arg-before-do
                            loopy-iter--process-special-arg-after-do
                            loopy-iter--process-special-arg-finally-do
                            loopy-iter--process-special-arg-finally-return
                            loopy-iter--process-special-arg-finally-protect))

   (loopy--with-protected-stack
    (let* ((suppressed-expanders (loopy (list i loopy-iter-suppressed-macros)
                                        (collect (cons i nil))))
           (loopy-iter--command-parsers (or loopy-iter--command-parsers
                                            (append loopy-iter-overwritten-command-parsers
                                                    loopy-command-parsers)))
           (loopy-iter--non-main-body-instructions)
           (loopy-iter--level 0)
           (command-env
            (append (loopy (list keyword loopy-iter-keywords)
                           (collect
                            (cons keyword
                                  (lambda (&rest args)
                                    (loopy--bind-main-body (main other)
                                        ;; Bind here in case a command required to be
                                        ;; in the top level is found in an expression
                                        ;; while parsing an actual top-level command.
                                        (let* ((loopy-iter--level (1+ loopy-iter--level))
                                               (loopy--in-sub-level (> loopy-iter--level 1)))
                                          (loopy-iter--parse-command args))
                                      (push other loopy-iter--non-main-body-instructions)
                                      (macroexp-progn main))))))
                    (loopy (list command loopy-iter-bare-commands)
                           (collect
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
                                            (loopy-iter--parse-command (cons cmd args)))
                                        (push other loopy-iter--non-main-body-instructions)
                                        (macroexp-progn main)))))))))
           (common-env `(,@suppressed-expanders
                         ,@command-env
                         ,@macroexpand-all-environment))
           (first-pass-env `((loopy--optimized-accum . loopy-iter--opt-accum-expand-val)
                             (loopy--optimized-accum-2 . nil)
                             ,@common-env))
           (second-pass-env `(;; Identify second version of optimized accumulation.
                              (loopy--optimized-accum-2 . loopy--expand-optimized-accum)
                              ,@common-env)))

      (cl-labels (;; A wrapper to set `loopy--in-sub-level' correctly:
                  ;; If this is a known command, expand as normal.  The command
                  ;; parser will handle sub-level-ness.  Otherwise, while EXPR
                  ;; isn't a command itself, bind `loopy--in-sub-level' in case
                  ;; of any commands further down.
                  (iter-macroexpand-all (expr)
                    (if (map-elt command-env (car expr))
                        (macroexpand-all expr first-pass-env)
                      (let ((loopy-iter--level (1+ loopy-iter--level))
                            (loopy--in-sub-level t))
                        (macroexpand-all expr first-pass-env))))
                  ;; Process body, insert data for optimized accumulations,
                  ;; then process the other instructions:
                  (first-pass (body)
                    (prog1
                        (mapcar #'iter-macroexpand-all body)
                      (loopy--process-instructions
                       (thread-last loopy-iter--non-main-body-instructions
                                    nreverse
                                    (apply #'append)))))
                  ;; Expand the optimized accumulation variables,
                  ;; then process the `at' instructions for this loop:
                  (second-pass (body)
                    (prog1
                        (mapcar (lambda (expr) (macroexpand-all expr second-pass-env))
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
      (loopy--expand-to-loop)))))

;;;; Add `loopy-iter' to `loopy'
(cl-defun loopy-iter--parse-loopy-iter-command ((_ &rest body))
  "Parse the `loopy-iter' command as (loopy-iter &rest BODY).

See the info node `(loopy)The loopy-iter Macro' for more."
  `((loopy--main-body ,(macroexpand `(loopy-iter ,@body)))))

(cl-callf map-insert loopy-command-parsers
  'loopy-iter #'loopy-iter--parse-loopy-iter-command)

(provide 'loopy-iter)
;;; loopy-iter.el ends here
