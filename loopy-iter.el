;;; loopy-iter.el --- A macro similar to CL's iterate  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Earl Hyatt

;; Author: Earl Hyatt
;; Created: March 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.10.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: extensions
;; LocalWords:  Loopy's emacs sexps

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

;;;; Flags (obsolete)
(make-obsolete-variable
 'loopy-iter--lax-naming
 "Use `loopy-iter-bare-special-macro-arguments' or
`loopy-iter-bare-commands' instead.  See the manual."
 "2022-07")
(defvar loopy-iter--lax-naming nil
  "Whether loop commands must be preceded by keywords to be recognized.

By default, `loopy-iter' requires loop commands to be preceded by
the keywords `for', `accum', or `exit', in order to distinguish
loop commands from other Emacs features.

The flag `lax-naming' disables this requirement, at the cost of
name collisions becoming more likely.")

(make-obsolete 'loopy-iter--enable-flag-lax-naming nil "2022-07")
(defun loopy-iter--enable-flag-lax-naming ()
  "Set `loopy-iter--lax-naming' to t inside the loop."
  (setq loopy-iter--lax-naming t))

(make-obsolete 'loopy-iter--disable-flag-lax-naming nil "2022-07")
(defun loopy-iter--disable-flag-lax-naming ()
  "Set `loopy-iter--lax-naming' to nil inside the loop if active."
  ;; Currently redundant, but leaves room for possibilities.
  (if loopy-iter--lax-naming
      (setq loopy-iter--lax-naming nil)))

(let ((f #'(lambda ()
             (warn (concat "loopy-iter: Flag `lax-naming' is now obsolete.  "
                           "See manual or changelog.")))))

  (dolist (flag '(lax-naming +lax-naming lax-names +lax-names))
    (setf loopy--flag-settings
          (map-insert loopy--flag-settings flag f)))

  (dolist (flag '(-lax-naming -lax-names))
    (setf loopy--flag-settings
          (map-insert loopy--flag-settings flag f))))

;;;; Custom User Options
(defgroup loopy-iter nil
  "Options specifically for the `loopy-iter' macro."
  :group 'loopy
  :prefix "loopy-iter-")

(make-obsolete-variable
 'loopy-iter-ignored-names
 "Use `loopy-iter-bare-special-macro-arguments' or
`loopy-iter-bare-commands' instead.  See the manual."
 "2022-07")
(defcustom loopy-iter-ignored-names '(let*)
  "Names of commands, special macro arguments, and their aliases to be ignored.

Some aliases and command names can cause conflicts, such as `let*' as
an alias of the special macro argument `with'.

This option always applies to special macro arguments.  This
option is used with commands when the `lax-naming' flag is
enabled."
  :type '(repeat symbol))

(define-obsolete-variable-alias 'loopy-iter-command-keywords
  'loopy-iter-keywords "2022-07")
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



;;;; Miscellaneous Helper Functions
;; (defun loopy-iter--valid-loop-command (name)
;;   "Check if NAME is a known command.
;;
;; This checks for NAME as a key in `loopy-aliases'
;; and `loopy-command-parsers', in that order."
;;   (if (and loopy-iter--lax-naming
;;            (memq name loopy-iter-ignored-names))
;;       nil
;;     (map-elt loopy-command-parsers (loopy--get-true-name name))))
;;
;; (defun loopy-iter--literal-form-p (form)
;;   "Whether FORM is a literal form that should not be interpreted."
;;   (or (and (consp form)
;;            (memq (cl-first form) loopy-iter--literal-forms))
;;       (arrayp form)))
;;
;; (defun loopy-iter--sub-loop-command-p (name)
;;   "Whether command named NAME is a sub-loop."
;;   (memq name (loopy--get-all-names 'sub-loop
;;                                    :from-true t
;;                                    :ignored loopy-iter-ignored-names)))



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
    leaving
    leaving-from
    listing
    listing-index
    listing-ref
    looping
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
    sequencing
    sequencing-index
    sequencing-ref
    setting
    setting-prev
    skipping
    skipping-from
    string
    string-index
    string-ref
    stringing
    stringing-index
    stringing-ref
    sub-looping
    thereis
    summing
    unioning
    vconcating)
  "Commands recognized in `loopy-iter' without a preceding keyword.

For special marco arguments, see `loopy-iter-bare-special-macro-arguments'.

This option replaces the flag `lax-naming', and is always in effect."
  :type '(repeat symbol)
  :group 'loopy-iter)

(defvar loopy-iter--command-parsers nil
  "Parsers used by `loopy-iter'.

This variable is bound while `loopy-iter' is running, combining
`loopy-command-parsers' and
`loopy-iter-overwritten-command-parsers'.")

(defun loopy-iter--parse-command (command)
  "An Iter version of `loopy--parse-loop-command'."
  (let* ((cmd-name (cl-first command))
         (parser (loopy--get-command-parser
                  cmd-name
                  :parsers loopy-iter--command-parsers)))
    (if-let ((instructions (funcall parser command)))
        (remq nil instructions)
      (error "Loopy Iter: No instructions returned by command parser: %s"
             parser))))

(defvar loopy-iter--non-main-body-instructions nil
  "Used to capture other instructions while expanding.

Expanding functions `push' lists of instructions into this variable.")

;;;;; Expanders

(defvar loopy-iter--sub-level-expanders nil
  "Macro expanders for sub-level expressions.")

(defvar loopy-iter--top-level-expanders nil
  "Macro expanders for top-level expressions.")

(defun loopy-iter--macroexpand-top (expr)
  "Expand a top-level expression using `loopy-iter--top-level-expanders'"
  (macroexpand-1 expr loopy-iter--top-level-expanders))

(defun loopy-iter--macroexpand-sub (expr)
  "Expand a top-level expression using `loopy-iter--sub-level-expanders'"
  (macroexpand-all expr loopy-iter--sub-level-expanders))

(defun loopy-iter--keyword-expander-top (&rest args)
  "Expand top-level commands preceded by keywords in `loopy-iter-keywords'."
  (cl-destructuring-bind (main other)
      (loopy--extract-main-body
       (loopy-iter--parse-command args))
    (push other loopy-iter--non-main-body-instructions)
    (macroexp-progn main)))

(defun loopy-iter--keyword-expander-sub (&rest args)
  "Expand sub-level commands preceded by keywords in `loopy-iter-keywords'."
  (cl-destructuring-bind (main other)
      (loopy--extract-main-body
       (let ((loopy--in-sub-level t))
         (loopy-iter--parse-command args)))
    (push other loopy-iter--non-main-body-instructions)
    (macroexp-progn main)))

(defun loopy-iter--opt-accum-expand-val (arg)
  "Macro expand only the value of an optimized accumulation.

Optimized accumulations are expanded into a special form, after
which this function will recursively expand the expression of the
accumulated value.

To avoid an infinite loop, this function replaces the `loopy--optimized-accum'
in the expression with `loopy--optimized-accum-2'."
  (loopy (with (plist (cadr arg)))
         (cons (k v) plist :by #'cddr)
         (collect k)
         ;; By this point, command expansion are already defined, so we don't
         ;; need to try to handle instructions.
         (collect (if (eq k :val)
                      (macroexpand-all v loopy-iter--sub-level-expanders)
                    v))
         (finally-return `(loopy--optimized-accum-2 (quote ,loopy-result)))))

;;;;; Overwritten definitions

(defcustom loopy-iter-overwritten-command-parsers
  '((at       . loopy-iter--parse-at-command)
    (sub-loop . loopy-iter--parse-sub-loop-command))
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
  ;; `loopy-iter--non-main-body-instructions' while expanders push to it,
  ;; we which then wrap back in a new instruction and pass up to the calling
  ;; function, which consumes instructions.
  (loopy (with (loopy-iter--non-main-body-instructions nil)
               (loopy--loop-name target-loop)
               (loopy--in-sub-level t))
         (list cmd commands)
         (collect (list 'loopy--main-body
                        (loopy-iter--macroexpand-sub cmd)))
         (finally-return
          ;; Return list of instructions to comply with expectations of calling
          ;; function, which thinks that this is a normal loop-command parser.
          `(,@loopy-result
            (loopy--at-instructions
             (,target-loop
              ,@(thread-last loopy-iter--non-main-body-instructions
                             nreverse
                             (apply #'append))))))))

(cl-defun loopy-iter--parse-sub-loop-command ((_ &rest body))
  "Parse the `sub-loop' command in `loopy-iter'."
  `((loopy--main-body ,(macroexpand `(loopy-iter ,@body)))))

;;;; For parsing special macro arguments

(defcustom loopy-iter-bare-special-macro-arguments
  '( after-do        after else-do else
     before-do       before initially-do initially
     finally-do      finally
     finally-return
     finally-protect finally-protected
     flag            flags
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
  (loopy--wrap-variables-around-body

   (mapc #'loopy--apply-flag loopy-default-flags)

   (setq body (loopy--process-special-arg-loop-name body))
   (setq body (loopy-iter--process-special-arg-flag body))
   (setq body (loopy-iter--process-special-arg-with body))
   (setq body (loopy-iter--process-special-arg-without body))
   (setq body (loopy-iter--process-special-arg-accum-opt body))
   (setq body (loopy-iter--process-special-arg-wrap body))
   (setq body (loopy-iter--process-special-arg-before-do body))
   (setq body (loopy-iter--process-special-arg-after-do body))
   (setq body (loopy-iter--process-special-arg-finally-do body))
   (setq body (loopy-iter--process-special-arg-finally-return body))
   (setq body (loopy-iter--process-special-arg-finally-protect body))

   ;; Process the main body.
   (unwind-protect
       (progn
         (let ((suppressed-alist (loopy (list i loopy-iter-suppressed-macros)
                                        (collect (cons i nil))))
               (loopy-iter--command-parsers
                (or loopy-iter--command-parsers
                    (append loopy-iter-overwritten-command-parsers
                            loopy-command-parsers))))

           ;; During the initial top-level expansion and the subsequent
           ;; all-level expansion, we make an effort to keep instructions in the
           ;; same order that they are received.  This might help to avoid
           ;; unexpected behavior regarding variable declarations.  For example,
           ;; if the top level of a following expression refers back to a
           ;; variable initialized in a preceeding sub-expression.
           (let ((loopy-iter--non-main-body-instructions)
                 (cmd-alist-1)
                 (cmd-alist-sub)
                 (keyword-alist-1)
                 (keyword-alist-sub))

             ;; Entries for command names.
             (dolist (cmd loopy-iter-bare-commands)
               (let ((cmd cmd))
                 (push (cons cmd (lambda (&rest args)
                                   (cl-destructuring-bind (main other)
                                       (loopy--extract-main-body
                                        (loopy-iter--parse-command
                                         (cons cmd args)))
                                     (push other
                                           loopy-iter--non-main-body-instructions)
                                     (macroexp-progn main))))
                       cmd-alist-1)
                 (push (cons cmd (lambda (&rest args)
                                   (cl-destructuring-bind (main other)
                                       (loopy--extract-main-body
                                        (let ((loopy--in-sub-level t))
                                          (loopy-iter--parse-command
                                           (cons cmd args))))
                                     (push other
                                           loopy-iter--non-main-body-instructions)
                                     (macroexp-progn main))))
                       cmd-alist-sub)))

             ;; Entries for keyword commands
             (dolist (keyword loopy-iter-keywords)
               (push (cons keyword #'loopy-iter--keyword-expander-top)
                     keyword-alist-1)
               (push (cons keyword #'loopy-iter--keyword-expander-sub)
                     keyword-alist-sub))

             (let* ((loopy-iter--top-level-expanders
                     `(,@suppressed-alist
                       (loopy--optimized-accum . loopy-iter--opt-accum-expand-val)
                       (loopy--optimized-accum-2 . nil)
                       ,@cmd-alist-1
                       ,@keyword-alist-1))
                    (loopy-iter--sub-level-expanders
                     `(,@suppressed-alist
                       (loopy--optimized-accum . loopy-iter--opt-accum-expand-val)
                       (loopy--optimized-accum-2 . nil)
                       ,@cmd-alist-sub
                       ,@keyword-alist-sub
                       ,@macroexpand-all-environment)))

               ;; Now process the main body.
               (loopy (accum-opt new-body)
                      (list expr body)
                      (collect new-body
                               (thread-last expr
                                            loopy-iter--macroexpand-top
                                            loopy-iter--macroexpand-sub))
                      (finally-do
                       (setq loopy--main-body new-body)
                       (loopy--process-instructions
                        (thread-last loopy-iter--non-main-body-instructions
                                     nreverse
                                     (apply #'append)))))))

           ;; Expand any uses of `loopy--optimized-accum' as if it were a macro,
           ;; using the function `loopy--expand-optimized-accum'.
           (loopy
            ;; TODO:
            ;; - Is there a way to only expand `loopy--optimized-accum'?
            (with (macro-funcs `(,@suppressed-alist
                                 ;; Identify second version of optimized accumulation.
                                 (loopy--optimized-accum-2 . loopy--expand-optimized-accum)
                                 ,@macroexpand-all-environment)))
            (list i loopy--main-body)
            (collect (macroexpand-all i macro-funcs))
            (finally-do (setq loopy--main-body loopy-result))))


         (loopy--process-instructions (map-elt loopy--at-instructions
                                               loopy--loop-name)
                                      :erroring-instructions
                                      '(loopy--main-body)))
     (loopy--clean-up-stack-vars))

   ;; Make sure the order-dependent lists are in the correct order.
   (setq loopy--iteration-vars (nreverse loopy--iteration-vars)
         loopy--accumulation-vars (nreverse loopy--accumulation-vars)
         loopy--implicit-return (when (consp loopy--implicit-return)
                                  (if (= 1 (length loopy--implicit-return))
                                      ;; If implicit return is just a single thing,
                                      ;; don't use a list.
                                      (car loopy--implicit-return)
                                    ;; If multiple items, be sure to use a list
                                    ;; in the correct order.
                                    `(list ,@(nreverse loopy--implicit-return)))))

   ;; Produce the expanded code, based on the `let'-bound variables.
   (loopy--expand-to-loop)))

;;;; Add `loopy-iter' to `loopy'
(cl-defun loopy-iter--parse-loopy-iter-command ((_ &rest body))
  "Parse the `loopy-iter' command as (loopy-iter &rest BODY).

See the info node `(loopy)The loopy-iter Macro' for more."
  `((loopy--main-body ,(macroexpand `(loopy-iter ,@body)))))

(cl-callf map-insert loopy-command-parsers
  'loopy-iter #'loopy-iter--parse-loopy-iter-command)

(provide 'loopy-iter)
;;; loopy-iter.el ends here
