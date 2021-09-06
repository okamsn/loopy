;;; loopy-iter.el --- A macro similar to CL's iterate  -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Earl Hyatt

;; Author: Earl Hyatt
;; Created: March 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.9.1
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
;; A macro similar to, and heavily inspired by, Common Lisp's Iterate package.
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
(require 'loopy-vars)
(require 'loopy-misc)
(require 'loopy-commands)
(require 'cl-lib)
(require 'seq)
(require 'map)
(require 'macroexp)

;; How the Code Works:
;;
;; Instead of using `loopy--parse-loop-commands' and
;; `loopy--process-instructions' directly, the list of arguments is treated as a
;; tree of code, and is passed to `loopy-iter--replace-in-tree'.  That function
;; recursively goes through each element in the argument list, looking for loop
;; commands according to `loopy-iter--lax-naming'.  Found commands are
;; processed, and their main-body instructions are substituted into the
;; command's place in the tree.  Other instructions are handled as normal.
;;
;; Some code requires specific handling, which is done using helper functions
;; like `loopy-iter--replace-in-setq-form'.  It is hoped that by using
;; `macroexpand-all' on the elements of the argument list that any macros will
;; be expanded into uses of special forms already handled.
;;
;; After the argument list is processed, `loopy--main-body' is set to that one,
;; new tree, and expansion proceeds as normal.

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

(dolist (flag '(lax-naming +lax-naming lax-names +lax-names))
  (setf loopy--flag-settings
        (map-insert loopy--flag-settings flag
                    #'loopy-iter--enable-flag-lax-naming)))

(dolist (flag '(-lax-naming -lax-names))
  (setf loopy--flag-settings
        (map-insert loopy--flag-settings flag
                    #'loopy-iter--disable-flag-lax-naming)))

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
(defvar loopy-iter--let-forms '(let let*)
  "Forms to treat like `let'.

`let' forms might use constructs wrapped in variable definitions.")

;; NOTE: The code "(list 'quote 'a)" is the same as "'a".  Therefore, "quote"
;; should be the last symbol in the list.
(defvar loopy-iter--literal-forms '(declare quote)
  "Forms that shouldn't be evaluated.

Currently, `lambda' forms (which are automatically quoted) are
still evaluated.")


(defvar loopy-iter--setq-forms '(setq)
  "Special forms that work like `setq'.")

;;;; Miscellaneous Helper Functions
(defun loopy-iter--valid-loop-command (name)
  "Check if NAME is a known command.

This checks for NAME as a key in `loopy-command-aliases'
and `loopy-command-parsers', in that order."
  (or (map-elt loopy-command-aliases name)
      (map-elt loopy-command-parsers name)))

(defun loopy-iter--literal-form-p (form)
  "Whether FORM is a literal form that should not be interpreted."
  (or (and (consp form)
           (memq (cl-first form) loopy-iter--literal-forms))
      (arrayp form)))

(defun loopy-iter--sub-loop-command-p (name)
  "Whether command named NAME is a sub-loop."
  (memq name (loopy--find-all-names '(loop sub-loop subloop))))

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
            (let* ((key (cl-first elem))
                   (cmd)
                   (subtree))

              (if loopy-iter--lax-naming
                  (setq cmd key
                        subtree (cl-rest elem))
                (setq cmd (cl-second elem)
                      subtree (cddr elem)))

              (cond

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

               ;; Check if it's the special `sub-loop' command before checking
               ;; if it's any other kind of loop command.
               ((loopy-iter--sub-loop-command-p cmd)
                (push (macroexpand `(loopy-iter ,@subtree))
                      new-tree))

               ;; Handle `at' commands specially.
               ((memq (if loopy-iter--lax-naming key (cl-second elem))
                      (loopy--find-all-names '(at)))
                (push (macroexp-progn
                       (loopy-iter--replace-in-at-command (cl-first subtree)
                                                          (cl-rest subtree)))
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
                    (loopy--extract-main-body
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
                    (push (macroexp-progn recursively-parsed-main-body)
                          new-tree))
                  ;; Interpret the other instructions.
                  (loopy--process-instructions
                   other-instructions :erroring-instructions '(loopy--main-body))))

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

(defmacro loopy-iter--wrap-at-targets (&rest body)
  "`let'-bind the variables in `loopy--valid-external-at-targets' for BODY."
  `(let ,loopy--valid-external-at-targets
     ,@body))

(defmacro loopy-iter--produce-at-instruction-from-targets ()
  "Make an `at' instruction from targets in `loopy--valid-external-at-targets'."
  `(list 'loopy--at-instructions
         (cons loopy--loop-name
               (append ,@(cl-loop for var in loopy--valid-external-at-targets
                                  collect `(mapcar (lambda (instr)
                                                     (list (quote ,var) instr))
                                                   ,var))))))

(defun loopy-iter--replace-in-at-command (target-loop tree)
  "Replace loop commands in TREE with respect to TARGET-LOOP."
  (loopy--check-target-loop-name target-loop)
  (loopy-iter--wrap-at-targets
   (let ((loopy--loop-name target-loop))
     (prog1
         (loopy-iter--replace-in-tree tree)
       (loopy--process-instruction
        (loopy-iter--produce-at-instruction-from-targets)
        :erroring-instructions '(loopy--main-body))))))

;;;; The macro itself
(defmacro loopy-iter (&rest body)
  "An `iter'-like `loopy' macro.

See `loopy' for information about BODY.

This macro allows for embedding many loop commands in arbitrary
code.  This can be more flexible than using the `do' loop command
in `loopy'.

One useful difference is that `let*' is not an alias of the
`with' special macro argument.  See the Info node `(loopy)' for
information on how to use `loopy' and `loopy-iter'.

\(fn CODE-or-COMMAND...)"

  (loopy--wrap-variables-around-body

   ;; Process the special macro arguments.
   ;; For now, allow aliases, but don't allow `let*' for `with'.

   ;; There should be only one of each of these arguments.

   (let ((loop-name (seq-find #'symbolp body)))
     (setq loopy--loop-name loop-name
           loopy--skip-tag-name (loopy--produce-skip-tag-name loop-name)
           loopy--non-returning-exit-tag-name
           (loopy--produce-non-returning-exit-tag-name loop-name))
     (cl-callf2 seq-remove #'symbolp body))

   ;; Flags
   ;; Process any flags passed to the macro.  In case of conflicts, the
   ;; processing order is:
   ;;
   ;; 1. Flags in `loopy-default-flags'.
   ;; 2. Flags in the `flag' macro argument, which can undo the first group.
   (mapc #'loopy--apply-flag loopy-default-flags)
   (loopy--process-special-marco-args '(flag flags)
     (mapc #'loopy--apply-flag arg-value)
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name))
                body))

   ;; With
   ;; Note: These values don't have to be used literally, due to destructuring.
   (loopy--process-special-marco-args '(with init)
     (setq loopy--with-vars
           (mapcar (lambda (binding)
                     (cond ((symbolp binding)      (list binding nil))
                           ((= 1 (length binding)) (list (cl-first binding)
                                                         nil))
                           (t                       binding)))
                   arg-value))
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

   ;; Without
   (loopy--process-special-marco-args '(without no-with no-init)
     (setq loopy--without-vars arg-value)
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

   ;; Wrap
   (loopy--process-special-marco-args '(wrap)
     (setq loopy--wrapping-forms arg-value)
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

   ;; Before do
   (loopy--process-special-marco-args '( before-do before
                                         initially-do initially)
     (setq loopy--before-do arg-value)
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

   ;; After do
   (loopy--process-special-marco-args '(after-do after else-do else)
     (setq loopy--after-do arg-value)
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

   ;; Finally Do
   (loopy--process-special-marco-args '(finally-do finally)
     (setq loopy--final-do arg-value)
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

   ;; Final Return
   (loopy--process-special-marco-args '(finally-return)
     (setq loopy--final-return (if (= 1 (length arg-value))
                                   (cl-first arg-value)
                                 (cons 'list arg-value)))
     (cl-callf2 seq-remove (lambda (x) (eq (car x) arg-name)) body))

   ;; Process the main body.
   (push loopy--loop-name loopy--known-loop-names)
   (unwind-protect
       (progn
         (setq loopy--main-body (loopy-iter--replace-in-tree body))
         (loopy--process-instructions (map-elt loopy--at-instructions
                                               loopy--loop-name)
                                      :erroring-instructions
                                      '(loopy--main-body)))
     (pop loopy--known-loop-names)
     (cl-callf map-delete loopy--at-instructions loopy--loop-name)
     (cl-callf2 seq-drop-while (lambda (x) (eq loopy--loop-name (caar x)))
                loopy--accumulation-list-end-vars))

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
