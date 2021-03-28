;;; loopy-iter.el --- A macro similar to CL's iterate  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Earl Hyatt

;; Author: Earl Hyatt
;; Created: March 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
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
;; A macro similar to and heavily inspired by Common Lisp's Iterate package.
;; This package is somewhat like a translation of Iter into Loopy.
;;
;; To be able to arbitrarily nest structures, Loopy's constructs must be clearly
;; distinct from Lisp's other functions.
;;
;; Subject to change:
;; - loop commands start with `for': (for list i '(1 2 3))
;; - accumulation commands start with `accum': (accum collect i)
;; - early exit commands start with `exit': (exit return i)
;; - Special macro arguments should use unambiguous versions.  No aliases.
;;
;; See the `loopy' documentation for more details.

;;; Code:
(require 'loopy)
(require 'cl-lib)

(defvar loopy--flag-settings nil)

;;;; Flags
(defvar loopy-iter--lax-naming nil
  "Whether loop commands must be preceded by keywords to be recognized.

By default, `loopy-iter' requires loop commands to be preceded by
the keywords `for', `accum', or `exit', in order to distinguish
loop commands from other Emacs features.

The flag `lax-naming' disables this requirement, at the cost of
name collisions becoming more likely.")

(defun loopy-iter--enable-flag-lax-naming ()
  "Set `loopy-iter--lax-naming' to t inside the loop."
  (setq loopy-iter--lax-naming t))

(defun loopy-iter--disable-flag-lax-naming ()
  "Set `loopy-iter--lax-naming' to nil inside the loop if active."
  ;; Currently redundant, but leaves room for possibilities.
  (if loopy-iter--lax-naming
      (setq loopy-iter--lax-naming nil)))

(add-to-list 'loopy--flag-settings (cons 'lax-naming #'loopy-iter--enable-flag-lax-naming))
(add-to-list 'loopy--flag-settings (cons '+lax-naming #'loopy-iter--enable-flag-lax-naming))
(add-to-list 'loopy--flag-settings (cons '-lax-naming #'loopy-iter--disable-flag-lax-naming))

;; For convenience, add another variant `lax-names'.
(add-to-list 'loopy--flag-settings (cons 'lax-names #'loopy-iter--enable-flag-lax-naming))
(add-to-list 'loopy--flag-settings (cons '+lax-names #'loopy-iter--enable-flag-lax-naming))
(add-to-list 'loopy--flag-settings (cons '-lax-names #'loopy-iter--disable-flag-lax-naming))

;;;; Custom User Options
(defgroup loopy-iter nil
  "Options specifically for the `loopy-iter' macro."
  :group 'loopy
  :prefix "loopy-iter-")

(defcustom loopy-iter-ignored-commands '()
  "Commands and aliases that `loopy-iter' should ignore.

This option is used only when the `lax-naming' flag is enabled.

For example, `loopy' provides a loop command `if', but in
`loopy-iter', one would probably prefer to use the special form
`if' provided by Emacs instead.

`loopy-iter' automatically checks whether an expression is a
function, macro, or special form before checking whether it is a
loop command, but this user option can be used to help avoid
errors when that fails."
  :type '(repeat symbol))

(defcustom loopy-iter-command-keywords '(accum for exit)
  "Keywords that `loopy-iter' can use to recognize loop commands.

By default, `loopy-iter' requires keywords to clearly distinguish
loop commands from other Emacs features.  This requirement can be
disabled with the `lax-naming' flag.

A loop command can be preceded by any of the keywords in this
list.  For example, by default, \"(for collect i)\" and
\"(accum collect i)\" are both valid way of identifying the
`collect' loop command."
  :type '(repeat symbol))

;;;;
(defvar loopy-iter--valid-macro-arguments
  '( flag flags with without no-init before-do before initially-do
     initially after-do after else-do else finally-do finally finally-return)
  "List of valid keywords for `loopy-iter' macro arguments.

This variable is used to signal an error instead of silently failing.")

(defvar loopy-iter--let-forms '(let let*)
  "Forms to treat like `let'.

`let' forms might use constructs wrapped in variable definitions.")

;; TODO: Get this to eval to '(quote function) without need of fist symbol.
(defvar loopy-iter--literal-forms '(loopy-iter--junk-symbol quote)
  "Forms that shouldn't be evaluated.

Currently, `lambda' forms (which are automatically quoted) are
still evaluated.")


(defvar loopy-iter--setq-forms '(setq)
  "Special forms that work like `setq'.")

;;;; Miscellaneous Helper Functions
(defun loopy-iter--extract-main-body (instructions)
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

(defun loopy-iter--valid-loop-command (name)
  "Check if NAME is a known command.

This checks for NAME as a key in
`loopy-custom-command-aliases', `loopy-custom-command-parsers',
and `loopy--builtin-command-parsers', in that order."
  (or (assq name loopy-custom-command-aliases)
      (assq name loopy-custom-command-parsers)
      (assq name loopy--builtin-command-parsers)))

(defun loopy-iter--literal-form-p (form)
  "Whether FORM is a literal form that should not be interpreted."
  (or (and (consp form)
           (memq (cl-first form) loopy-iter--literal-forms))
      (arrayp form)))

;;;; Replacement functions
(defun loopy-iter--replace-in-tree (tree)
  "Replace loop commands in TREE in-place with their main-body code.

Other instructions are just pushed to their variables."
  (if (nlistp tree)
      ;; If `tree' is not a list, just return the object.  This can happen when
      ;; trying to expand sub-expressions, such as the "v" in "(let ((i v)) ...)".
      tree
    (let ((new-tree))
      ;; TODO: How to handle macro expansion?  Ideally, we only need to parse
      ;; the fundamental building blocks of source code, and macros would expand
      ;; to usages of these blocks.  Should this be `macroexpand-all'?
      (dolist (elem (if (not loopy--in-sub-level)
                        (macroexpand-all tree)
                      tree))
        (if (consp elem)
            ;; Depending on the structure that we're dealing with, we need to
            ;; expand differently.
            (let ((key (cl-first elem)))
              (cond
               ;; If it's a special macro argument, just remove it from the tree.
               ;; By this point, it's already been interpreted.
               ((and (not loopy--in-sub-level)
                     (memq key loopy-iter--valid-macro-arguments))
                t)

               ;; Check if it's a lambda form
               ((eq key 'lambda)
                (push (loopy-iter--replace-in-lambda-form elem)
                      new-tree))

               ;; Check if it's a literal form.
               ((loopy-iter--literal-form-p elem)
                (push elem new-tree))

               ;; Check if it's a `let'-like form.
               ((memq key loopy-iter--let-forms)
                (push (loopy-iter--replace-in-let-form elem)
                      new-tree))

               ;; Check if it's a `setq'-like form.
               ((memq key loopy-iter--setq-forms)
                (push (loopy-iter--replace-in-setq-form elem)
                      new-tree))

               ;; Check if it's a loop command
               (;; If lax-naming, just check the first element in the list.
                ;; Otherwise, check if the first element is an appropriate
                ;; keyword and the second element is a known command.
                (if loopy-iter--lax-naming
                    ;; It's better to prefer existing functions to loop commands
                    ;; when using `lax-naming', since the expanded code might
                    ;; include calls to the function `list', which is
                    ;; indistinguishable from the loop command `list' in such
                    ;; cases.
                    (and (not (or (functionp key)
                                  (macrop key)
                                  (special-form-p key)))
                         (loopy-iter--valid-loop-command (cl-first elem))
                         (not (memq key loopy-iter-ignored-commands)))
                  (and (memq key loopy-iter-command-keywords)
                       (loopy-iter--valid-loop-command (cl-second elem))))

                (seq-let (main-body other-instructions)
                    (loopy-iter--extract-main-body
                     ;; If using lax naming, then the entire `elem' is the loop
                     ;; command.  Otherwise, it is the `cdr'.
                     (loopy--parse-loop-command (if loopy-iter--lax-naming
                                                    elem
                                                  (cl-rest elem))))
                  ;; Some loop commands might interleave expressions and
                  ;; commands to produce a value, so we should also check the
                  ;; expansion.
                  (let ((recursively-parsed-main-body
                         (loopy-iter--replace-in-tree main-body)))
                    ;; Push the main body into the tree.
                    (push (if (= 1 (length recursively-parsed-main-body))
                              (cl-first recursively-parsed-main-body)
                            (cons 'progn recursively-parsed-main-body))
                          new-tree))
                  ;; Interpret the other instructions.
                  (loopy-iter--process-non-main-body other-instructions)))

               ;; Otherwise, recurse.
               (t
                (let ((loopy--in-sub-level t))
                  (push (loopy-iter--replace-in-tree elem)
                        new-tree)))))
          ;; Just add anything else to the tree.
          (push elem new-tree)))
      ;; Return branches in correct order.
      (nreverse new-tree))))

(defun loopy-iter--replace-in-let-form (tree)
  "Replace loop commands in `let'-like form TREE.

These forms can have loop commands in the values of variables or in the body."
  (let ((new-var-list))
    ;; Handle the var-list
    (dolist (pair (cl-second tree))
      ;; If the pair is not the expected list (e.g., a single symbol which `let'
      ;; treats as variable bound to nil), just push into the list.  Otherwise,
      ;; we need to interpret the value being assigned to the variable.
      (if (nlistp pair)
          (push pair new-var-list)
        ;; TODO: Why do we need to deal with quoted forms here specifically?
        ;;       The `quote' doesn't seem to be passed along.
        (let ((value (cl-second pair)))
          (if (loopy-iter--literal-form-p value)
              (push pair new-var-list)
            (push (list (cl-first pair)
                        (loopy-iter--replace-in-tree value))
                  new-var-list)))))
    ;; Return value
    `(,(cl-first tree) ,(nreverse new-var-list)
      ,@(loopy-iter--replace-in-tree (cddr tree)))))

(defun loopy-iter--replace-in-setq-form (tree)
  "Replace loop commands in `setq'-like form TREE.

These forms can have loop commands in every other expression
starting at the third element in TREE."
  (let ((new-var-val-pairs)
        (name (cl-first tree))
        ;; Just to make the `while' loop easier.
        (tree (cdr tree)))
    (while tree
      (push (list (pop tree)
                  (let ((val (pop tree)))
                    (if (loopy-iter--literal-form-p val)
                        val
                      (loopy-iter--replace-in-tree val))))
            new-var-val-pairs))
    ;; Return new tree.
    `(,name ,@(apply #'append (nreverse new-var-val-pairs)))))

(defun loopy-iter--replace-in-lambda-form (tree)
  "Replace loop commands in `lambda'-like expression TREE.

These expressions can have loop commands in the body."
  `(lambda ,(cl-second tree)
     ,@(loopy-iter--replace-in-tree (cddr tree))))

;;;; Functions for building the macro.

(defun loopy-iter--process-non-main-body (instructions)
  "Push the values of INSTRUCTIONS to the appropriate variable."
  ;; These variables are `let'-bound by the macro `loopy-iter'.
  (dolist (instruction instructions)
    (cl-case (car instruction)
      (loopy--generalized-vars
       (push (cdr instruction) loopy--generalized-vars))
      (loopy--iteration-vars
       ;; Don't want to accidentally rebind variables to `nil'.
       (unless (loopy--bound-p (cadr instruction))
         (push (cdr instruction) loopy--iteration-vars)))
      (loopy--accumulation-vars
       ;; Don't want to accidentally rebind variables to `nil'.
       (unless (loopy--bound-p (cadr instruction))
         (push (cdr instruction) loopy--accumulation-vars)))
      (loopy--pre-conditions
       (push (cdr instruction) loopy--pre-conditions))
      ;; NOTE: We shouldn't get any of these.
      ;; (loopy--main-body
      ;;  (push (cdr instruction) loopy--main-body))
      (loopy--latter-body
       (push (cdr instruction) loopy--latter-body))
      (loopy--post-conditions
       (push (cdr instruction) loopy--post-conditions))
      (loopy--implicit-return
       (unless (loopy--already-implicit-return (cdr instruction))
         (push (cdr instruction) loopy--implicit-return)))
      (loopy--implicit-accumulation-final-update
       (push (cdr instruction) loopy--implicit-accumulation-final-update))

      ;; Code for conditionally constructing the loop body.
      (loopy--skip-used
       (setq loopy--skip-used t))
      (loopy--tagbody-exit-used
       (setq loopy--tagbody-exit-used t))

      ;; Places users probably shouldn't push to, but can if they want:
      (loopy--before-do
       (push (cdr instruction) loopy--before-do))
      (loopy--after-do
       (push (cdr instruction) loopy--after-do))
      (loopy--final-do
       (push (cdr instruction) loopy--final-do))
      (loopy--final-return
       (push (cdr instruction) loopy--final-return))
      (t
       (error "Loopy: Unknown body instruction: %s" instruction)))))

;;;; The macro itself
(defmacro loopy-iter (&rest body)
  "An `iter'-like `loopy' macro.

See `loopy' for information about BODY.  One difference is that
`let*' is not an alias of the `with' special macro argument.  See
the Info node `(loopy)' for information on how to use `loopy' and
`loopy-iter'."
  (let (;; -- Top-level expressions other than loop body --
        (loopy--loop-name)
        (loopy--with-vars)
        (loopy--without-vars)
        (loopy--before-do)
        (loopy--after-do)
        (loopy--final-do)
        (loopy--final-return)

        ;; -- Vars for processing loop commands --
        (loopy--iteration-vars)
        (loopy--accumulation-vars)
        (loopy--generalized-vars)
        (loopy--pre-conditions)
        (loopy--main-body)
        (loopy--latter-body)
        (loopy--post-conditions)
        (loopy--implicit-return)

        ;; -- Variables for constructing code --
        (loopy--skip-used)
        (loopy--tagbody-exit-used)
        (loopy--implicit-accumulation-final-update)
        (loopy--in-sub-level)

        ;; -- Flag Variables --
        (loopy-iter--lax-naming)
        (loopy--basic-destructuring-function)
        (loopy--destructuring-accumulation-parser)
        (loopy--split-implied-accumulation-results))

    ;; Process the special macro arguments.
    ;; For now, allow aliases, but don't allow `let*' for `with'.

    ;; There should be only one of each of these arguments.

    ;; Flags

    ;; Process any flags passed to the macro.  In case of conflicts, the
    ;; processing order is:
    ;;
    ;; 1. Flags in `loopy-default-flags'.
    ;; 2. Flags in the `flag' macro argument, which can
    ;;    undo the first group.

    (when-let ((loopy--all-flags
                (append loopy-default-flags
                        (loopy--find-special-macro-arguments '(flag flags)
                                                             body))))
      (dolist (flag loopy--all-flags)
        (if-let ((func (cdr (assq flag loopy--flag-settings))))
            (funcall func)
          (error "Loopy: Flag not defined: %s" flag))))

    ;; With
    (setq loopy--with-vars
          ;; Process `with' for destructuring.
          (mapcan (lambda (var)
                    (loopy--destructure-variables (cl-first var)
                                                  (cl-second var)))
                  (loopy--find-special-macro-arguments ;; '(with let*)
                   '(with) ; No `let*'.
                   body)))

    ;; Without
    (setq loopy--without-vars
          (loopy--find-special-macro-arguments '(without no-init) body))

    ;; Before do
    (setq loopy--before-do
          (loopy--find-special-macro-arguments '( before-do before
                                                  initially-do initially)
                                               body))

    ;; After do
    (setq loopy--after-do
          (loopy--find-special-macro-arguments '( after-do after
                                                  else-do else)
                                               body))

    ;; Finally Do
    (setq loopy--final-do
          (loopy--find-special-macro-arguments '(finally-do finally) body))

    ;; Final Return
    (setq loopy--final-return
          (when-let ((return-val
                      (loopy--find-special-macro-arguments 'finally-return
                                                           body)))
            (if (= 1 (length return-val))
                (car return-val)
              (cons 'list return-val))))


    ;; Process the main body.
    (setq loopy--main-body
          (loopy-iter--replace-in-tree body))

    ;; Make sure the order-dependent lists are in the correct order.
    (setq loopy--iteration-vars (nreverse loopy--iteration-vars)
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

(provide 'loopy-iter)
;;; loopy-iter.el ends here
