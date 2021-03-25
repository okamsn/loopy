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

;;; Variables
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

(defvar loopy-iter-progn-forms '(progn prog1 prog2)
  "Forms like `progn', `prog1', and `prog2'.")

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
  "Check if NAME is a known command."
  (or (assq name loopy-custom-command-aliases)
      (assq name loopy-custom-command-parsers)
      (assq name loopy--builtin-command-parsers)))

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
      ;; to usages of these blocks.
      (dolist (elem (macroexpand-1 tree))
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
               ((memq key loopy-iter--literal-forms)
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
               ((and (memq key '(for accum exit))
                     (loopy-iter--valid-loop-command (cl-second elem)))
                (seq-let (main-body other-instructions)
                    (loopy-iter--extract-main-body
                     (loopy--parse-loop-command (cdr elem)))
                  ;; Push the main body into the tree.
                  (push (if (= 1 (length main-body))
                            (cl-first main-body)
                          (cons 'progn main-body))
                        new-tree)
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
      ;; TODO: Why do we need to deal with quoted forms here specifically?
      ;;       The `quote' doesn't seem to be passed along.
      (let ((value (cl-second pair)))
        (if (and (consp value)
                 (memq (cl-first value) loopy-iter--literal-forms))
            (push pair new-var-list)
          (push (list (cl-first pair)
                      (loopy-iter--replace-in-tree value))
                new-var-list))))
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
                    (if (memq (cl-first val) loopy-iter--literal-forms)
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

;; The macro itself
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

    ;; Produce the expanded code
    (let (result
          ;; Need a variable to track whether `result' is currently one
          ;; expression, as that affects how it should be built.  For example,
          ;; `(progn (thing1) (thing2))' vs `((thing1) (thing2))'
          result-is-one-expression)

      ;; This temporary function is just for convenience.  Since it checks the
      ;; structure of `result', it should always be used like:
      ;; ,@(get-result).
      (cl-flet ((get-result () (if result-is-one-expression
                                   (list result)
                                 result)))

        (setq result loopy--main-body)

        (when loopy--skip-used
          (setq result `(cl-tagbody ,@result loopy--continue-tag)
                result-is-one-expression t))

        (when loopy--latter-body
          (setq result (append result loopy--latter-body)))

        (when loopy--post-conditions
          (setq result
                (append result
                        `((unless ,(cl-case (length loopy--post-conditions)
                                     (0 t)
                                     (1 (car loopy--post-conditions))
                                     (t (cons 'and loopy--post-conditions)))
                            ;; If the loop exits early, we should still use the
                            ;; implicit return.  That isn't a problem for the
                            ;; `while' loop, but we need to be more explicit
                            ;; here.
                            (cl-return-from ,loopy--loop-name
                              ,loopy--implicit-return))))))

        ;; Now wrap loop body in the `while' form.
        (setq result `(while ,(cl-case (length loopy--pre-conditions)
                                (0 t)
                                (1 (car loopy--pre-conditions))
                                (t (cons 'and loopy--pre-conditions)))
                        ;; If using a `cl-tag-body', just insert that one
                        ;; expression, but if not, break apart into the while
                        ;; loop's body.
                        ,@(get-result))
              ;; Will always be a single expression after wrapping with `while'.
              result-is-one-expression t)

        ;; Make sure that the implicit accumulation variable is correctly
        ;; updated after the loop, if need be.
        (when loopy--implicit-accumulation-final-update
          (setq result
                (if loopy--tagbody-exit-used
                    `(,@(get-result)
                      ,(car loopy--implicit-accumulation-final-update)
                      (setq loopy--implicit-accumulation-updated t))
                  `(,@(get-result)
                    ,(car loopy--implicit-accumulation-final-update)))
                result-is-one-expression nil))

        ;; Now ensure return value is nil and add the code to run before and
        ;; after the `while' loop.
        (cond
         ((and loopy--before-do loopy--after-do)
          (setq result `(,@loopy--before-do ,@(get-result) ,@loopy--after-do)
                result-is-one-expression nil))
         (loopy--before-do
          (setq result `(,@loopy--before-do ,@(get-result))
                result-is-one-expression nil))
         (loopy--after-do
          (setq result `(,@(get-result) ,@loopy--after-do)
                result-is-one-expression nil)))

        (when loopy--tagbody-exit-used
          (setq result (if loopy--implicit-accumulation-final-update
                           `(cl-tagbody
                             ,@(get-result)
                             loopy--non-returning-exit-tag
                             ;; Even if leave the loop early, make sure the
                             ;; update is always run.
                             (if loopy--implicit-accumulation-updated
                                 nil
                               ,(car loopy--implicit-accumulation-final-update)))
                         `(cl-tagbody
                           ,@(get-result)
                           loopy--non-returning-exit-tag))
                result-is-one-expression t))

        ;; Always wrap in `cl-block', as any arbitrary Lisp code could call
        ;; `cl-return-from'.  For example, it's possible that a user is using a
        ;; loop to change variables, and they might wish to stop changing things
        ;; at a certain point.
        (setq result `(cl-block ,loopy--loop-name
                        ,@(get-result)
                        ;; Be sure that the `cl-block' defaults to returning the
                        ;; implicit return, which can be nil.  This can be
                        ;; overridden by any call to `cl-return-from'.
                        ,loopy--implicit-return)
              ;; Will always be a single expression after wrapping with
              ;; `cl-block'.
              result-is-one-expression t)

        ;; Try to keep the return value of the expanded code as `nil' by
        ;; default.
        ;; - If final-return is used, then there's no problem, and we just use
        ;;   that.
        ;; - If there's final-do, be sure to return the value of the `cl-block'
        ;;   (which defaults to nil) by using `prog1'.
        (if loopy--final-return
            (if loopy--final-do
                (setq result `(,@(get-result)
                               ,@loopy--final-do ,loopy--final-return)
                      result-is-one-expression nil)
              (setq result `(,@(get-result)
                             ,loopy--final-return)
                    result-is-one-expression nil))
          (when loopy--final-do
            (setq result `(prog1 ,result ,@loopy--final-do)
                  result-is-one-expression t)))

        ;; Declare the loop variables.
        (when loopy--iteration-vars
          (setq result `(let* ,loopy--iteration-vars ,@(get-result))
                result-is-one-expression t))

        ;; If there are final updates to made and a tag-body exit that can skip
        ;; them, then we must initialize `loopy--implicit-accumulation-updated'.
        (when (and loopy--implicit-accumulation-final-update
                   loopy--tagbody-exit-used)
          (setq result `(let ((loopy--implicit-accumulation-updated nil))
                          ,@(get-result))
                result-is-one-expression t))

        (when loopy--accumulation-vars
          (setq result `(let ,loopy--accumulation-vars ,@(get-result))
                result-is-one-expression t))

        ;; Declare the With variables.
        (when loopy--with-vars
          (setq result `(let* ,loopy--with-vars ,@(get-result))
                result-is-one-expression t))

        ;; Declare the symbol macros.
        (when loopy--generalized-vars
          (setq result `(cl-symbol-macrolet ,loopy--generalized-vars
                          ,@(get-result))
                result-is-one-expression t))

        ;; Final check: If `result' is not one expression, then wrap `result' in
        ;; a `progn'.  Otherwise, the return value of the first expression would
        ;; be used as a function.
        (unless result-is-one-expression
          (push 'progn result))

        ;; Return the constructed code.
        result))))

(provide 'loopy-iter)
;;; loopy-iter.el ends here
