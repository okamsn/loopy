;;; loopy-misc.el --- Miscellaneous functions used with Loopy. -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: July 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.11.0
;; Package-Requires: ((emacs "27.1") (map "3.0"))
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
;; `loopy' is a macro that is used similarly to `cl-loop'.  It provides "loop
;; commands" that define a loop body and it's surrounding environment, as well
;; as exit conditions.
;;
;; This library provides features used by several of the libraries that form
;; the package `loopy'.  This separation exists for better organization.

;;; Code:

;; NOTE: This file can't require any of the other `loopy' files.
(require 'cl-lib)
(require 'gv)
(require 'pcase)
(require 'seq)
(require 'subr-x)


;;;; Errors
(define-error 'loopy-error
  "Error in `loopy' macro")

(define-error 'loopy-unknown-command
  "Loopy: Unknown command"
  'loopy-error)

(define-error 'loopy-unknown-loop-target
  "Loopy: Unknown loop target"
  'loopy-error)

;;;;; Errors on Command Arguments
(define-error 'loopy-bad-command-arguments
  "Loopy: Bad command arguments"
  'loopy-error)

(define-error 'loopy-wrong-number-of-command-arguments
  "Loopy: Wrong number of command arguments"
  '(loopy-error wrong-number-of-arguments loopy-bad-command-arguments))

(define-error 'loopy-bad-position-command-argument
  "Loopy: Bad `:at' position"
  '(loopy-error loopy-bad-command-arguments))

(define-error 'loopy-conflicting-command-arguments
  "Loopy: Conflicting command arguments"
  '(loopy-error loopy-bad-command-arguments))

;;;;; Errors on Accumulations
(define-error 'loopy-incompatible-accumulations
  "Loopy: Incompatible accumulations"
  'loopy-error)

(define-error 'loopy-missing-accum-counters
  "Loopy: Failed to set up accumulation counters"
  'loopy-error)

(define-error 'loopy-accum-constructor-missing
  "Loopy: No accumulation constructor for command or alias"
  'loopy-error)

(define-error 'loopy-bad-accum-category
  "Loopy: Bad accumulation category"
  'loopy-error)

;;;;; Errors on Iteration
(define-error 'loopy-iteration-in-sub-level
  "Loopy: Can only use iteration commands at top level of a loop or sub-loop"
  'loopy-error)

(defun loopy--signal-bad-iter (used-name true-name)
  "Signal an error for USED-NAME that is really TRUE-NAME."
  (signal 'loopy-iteration-in-sub-level (list used-name true-name)))

(defun loopy--signal-must-be-top-level (command-name)
  "Signal an error for COMMAND-NAME."
  (user-error "Can't use \"%s\" in `loopy' outside top-level" command-name))

;;;;; Errors on Destructuring
(define-error 'loopy-bad-desctructuring
  "Loopy: Bad destructuring"
  'loopy-error)

(define-error 'loopy-&whole-sequence
  "Loopy: `&whole' variable is sequence"
  'loopy-bad-desctructuring)

(define-error 'loopy-&whole-missing
  "Loopy: `&whole' variable is missing"
  'loopy-bad-desctructuring)

(define-error 'loopy-&rest-missing
  "Loopy: `&rest' variable is missing"
  'loopy-bad-desctructuring)

(define-error 'loopy-&rest-non-var
  "Loopy: Non-variable item after `&rest'"
  'loopy-bad-desctructuring)

(define-error 'loopy-&rest-multiple
  "Loopy: Multiple variables after `&rest'"
  'loopy-bad-desctructuring)

(define-error 'loopy-&rest-dotted
  "Loopy: Using `&rest' in dotted (improper) list"
  'loopy-bad-desctructuring)

(define-error 'loopy-&key-missing
  "Loopy: `&key' variable is missing"
  'loopy-bad-desctructuring)

(define-error 'loopy-destructure-type
  "Loopy: Can't destructure type"
  'loopy-bad-desctructuring)

(define-error 'loopy-destructure-vars-missing
  "Loopy: No variables bound"
  'loopy-bad-desctructuring)

;;;;; Errors on Quoted Forms
(define-error 'loopy-bad-function-form
  "Loopy: Unrecognized function form"
  'loopy-error)

(define-error 'loopy-bad-quoted-form
  "Loopy: Unrecognized quoted form"
  'loopy-error)


;;;; List Processing
(defalias 'loopy--car-equals-car #'loopy--car-equal-car)
(defun loopy--car-equal-car (a b)
  "Check whether the `car' of A equals the `car' of B."
  (equal (car a) (car b)))

(defun loopy--count-while (pred list)
  "Count the number of items while PRED is true in LIST.

This function returns 0 if PRED is immediately false.
PRED is a function taking one argument: the item.

For example, applying `cl-evenp' on (2 4 6 7) returns 3."
  ;; Could be done with `cl-position-if-not', except that
  ;; we want to return the length of the lists if
  ;; no counterexample found.
  (cl-loop for i in list
           while (funcall pred i)
           sum 1))

(defun loopy--count-until (pred list)
  "Count the number of items until PRED is true in LIST.

This function returns 0 if PRED is immediately true.
PRED is a function taking one argument: the item.

For example, applying `cl-oddp' on (2 4 6 7) returns 3."
  ;; Could be done with `cl-position-if', except that
  ;; we want to return the length of the lists if
  ;; no counterexample found.
  (cl-loop for i in list
           until (funcall pred i)
           sum 1))


(defun loopy--every-other (list)
  "Return a list of every other element in LIST, starting with the first.

This is helpful when working with property lists."
  (cl-loop for i in list by #'cddr collect i))

;; TODO: Would this be more useful as a `pcase' macro?

;; Note: This macro cannot currently be replaced by `cl-destructuring-bind' or
;;       `map-let'.
;;       - `map-let' provides no way to specify a default value when a key is
;;         not in PLIST.  This macro does.
;;       - `cl-destructuring-bind' signals an error when a key is in PLIST that
;;         is not in BINDINGS.  This macro does not.
(defmacro loopy--plist-bind (bindings plist &rest body)
  "Bind values in PLIST to variables in BINDINGS, surrounding BODY.

- PLIST is a property list.

- BINDINGS is of the form (KEY VAR KEY VAR ...).  VAR can
  optionally be a list of two elements: a variable name and a
  default value, similar to what one would use for expressing
  keyword parameters in `cl-defun' or `cl-destructuring-bind'.
  The default value is used /only/ when KEY is not found in
  PLIST.

- BODY is the same as in `let'.

This macro works the same as `cl-destructuring-bind', except for
the case when keys exist in PLIST that are not listed in
BINDINGS.  While `cl-destructuring-bind' would signal an error,
this macro simply ignores them."
  (declare (indent 2))
  (let ((value-holder (gensym "plist-let-"))
        (found-key (gensym "plist-prop-found-")))
    `(let* ((,value-holder ,plist)
            ,@(cl-loop for (key var . _) on bindings by #'cddr
                       if (consp var)
                       collect `(,(cl-first var)
                                 ;; Use `plist-member' instead of `plist-get' to
                                 ;; allow giving `nil' as an argument without
                                 ;; using the default value.
                                 (if-let ((,found-key (plist-member ,value-holder
                                                                    ,key)))
                                     (cl-second ,found-key)
                                   ,(cl-second var)))
                       else collect `(,var (plist-get ,value-holder ,key))))
       ,@body)))

(cl-defun loopy--substitute-using (new seq &key test)
  "Copy SEQ, substituting elements using output of function NEW.

NEW receives the element as its only argument.

If given predicate function TEST, replace only elements
satisfying TEST.  This testing could also be done in NEW."
  ;; In testing, `cl-map' seems the fastest way to do this.
  (cl-map (if (listp seq) 'list 'array)
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

(cl-defun loopy--split-list-before (list element &key key (test #'eq))
  "Split LIST on ELEMENT, so that ELEMENT begins the latter part.

TEST is used to determine equality.  KEY is applied to ELEMENT
and each item in LIST.

For example, using 2 as ELEMENT would split (1 2 3)
into (1) and (2 3)."
  (let ((first-part nil)
        (second-part nil))
    (setq second-part
          (if key
              (cl-loop for cell on list
                       for item = (car cell)
                       if (funcall test
                                   (funcall key item)
                                   (funcall key element))
                       return cell
                       else do (push item first-part))
            (cl-loop for cell on list
                     for item = (car cell)
                     if (funcall test item element)
                     return cell
                     else do (push item first-part))))
    (list (nreverse first-part)
          second-part)))

(defun loopy--split-off-last-var (var-list)
  "Split VAR-LIST, separating the last variable from the rest.

VAR-LIST can be a proper or dotted list.  For example,
splitting (1 2 3) or (1 2 . 3) returns ((1 2) 3)."
  (let ((var-hold))
    (while (car-safe var-list)
      (push (pop var-list) var-hold))
    (if var-list
        (list (nreverse var-hold) var-list)
      (let ((last (cl-first var-hold)))
        (list (nreverse (cl-rest var-hold)) last)))))


;;;; Destructuring
;; This better allows for things to change in the future.
(defun loopy--var-ignored-p (var)
  "Return whether VAR should be ignored."
  (eq var '_))

;; This was added to Emacs in commit 66509f2ead423b814378a44a55c9f63dcb1e149b.
;; We copy that definition here for lower versions, since we use it for
;; processing `&key' keys in the new destructuring features.
(unless (and (<= 28 emacs-major-version)
	     (function-get #'plist-get 'gv-expander))
  (require 'gv)
  (require 'macroexp)
  (gv-define-expander plist-get
    (lambda (do plist prop)
      (macroexp-let2 macroexp-copyable-p key prop
	(gv-letplace (getter setter) plist
          (macroexp-let2 nil p `(cdr (plist-member ,getter ,key))
            (funcall do
                     `(car ,p)
                     (lambda (val)
                       `(if ,p
                            (setcar ,p ,val)
                          ,(funcall setter
                                    `(cons ,key
                                           (cons ,val ,getter))))))))))))

;;;;; Destructuring normal values
;;
;; Note that functions which are only used for commands are found in
;; `loopy-commands.el'.  The functions found here are used generally.

(defun loopy--destructure-sequence (seq value-expression)
  "Return a list of bindings destructuring VALUE-EXPRESSION according to SEQ.

Return a list of variable-value pairs (not dotted), suitable for
substituting into a `let*' form or being combined under a `setq'
form.

If SEQ is `_', then a generated variable name will be used."
  (cl-typecase seq
    (symbol `((,(if (loopy--var-ignored-p seq) (gensym "ignored-value-") seq)
               ,value-expression)))
    (list (loopy--destructure-list seq value-expression))
    (array (loopy--destructure-array seq value-expression))
    (t (signal 'loopy-destructure-type (list seq)))))

(defun loopy--destructure-array (var value-expression)
  "Return a list of bindings destructuring VALUE-EXPRESSION according to VAR.

- If `&rest', bind the remaining values in the array.
- If `&whole', name a variable holding the whole value."
  (let ((bindings)
        (using-rest-var)
        (remaining-var var)
        (using-whole-var)
        (remaining-length (length var))
        (rest-pos nil)
        (starting-index 0))

    (when (eq '&whole (aref var 0))
      (cond ((or (= 1 remaining-length)
                 (eq (aref var 1) '&rest))
             (signal 'loopy-&whole-missing (list var)))
            ((sequencep (aref remaining-var 1))
             (signal 'loopy-&whole-sequence (list var)))
            (t
             (let ((whole-var (aref remaining-var 1)))
               (when (= 2 remaining-length)
                 (warn "`&whole' variable used alone: %s" var))

               (setq remaining-var (substring remaining-var 2)
                     remaining-length (max 0 (- remaining-length 2)))

               (if (loopy--var-ignored-p whole-var)
                   (warn "`&whole' variable ignored: %s" var)
                 (setq using-whole-var whole-var))))))

    (when-let ((pos (cl-position '&rest remaining-var :test #'eq)))
      (cond
       ((= (1+ pos) remaining-length)
        (signal 'loopy-&rest-missing (list var)))
       ((> remaining-length (+ 2 pos))
        (signal 'loopy-&rest-multiple (list var)))
       (t
        ;; (when (zerop pos)
        ;;   (warn "`&rest' variable used like `&whole': %s" var))
        (let ((rest-var (aref remaining-var (1+ pos))))
          (unless (loopy--var-ignored-p rest-var)
            (setq using-rest-var rest-var
                  rest-pos pos))
          (setq remaining-length (max 0 (- remaining-length 2))
                remaining-var (substring remaining-var 0 -2))))))

    ;; Remove ignored positions from the beginning and end.
    (cl-loop for x across remaining-var
             for ind from 0
             while (loopy--var-ignored-p x)
             finally (setq remaining-var (substring remaining-var ind)
                           remaining-length (- remaining-length ind)
                           ;; Be sure that we know what is left.
                           starting-index ind))

    ;; (loopy (array x var :index ind :downfrom (1- remaining-length))
    ;;        (unless (eq x '_)
    ;;          (do (setq remaining-var (substring remaining-var 0 (1+ ind))
    ;;                    remaining-length (1+ ind)))
    ;;          (leave)))

    (cl-loop for x across (reverse remaining-var)
             for ind downfrom (1- remaining-length)
             while (loopy--var-ignored-p x)
             finally do
             ;; `substring' is end exclusive.
             (setq remaining-var (substring remaining-var 0 (1+ ind))
                   remaining-length (1+ ind)))

    (cond
     ((zerop (length remaining-var))
      (signal 'loopy-destructure-vars-missing (list var)))
     ;; Check to see if we can avoid binding the holding variable.
     ((and (= 1 remaining-length)
           (not using-whole-var)
           (not using-rest-var))
      (push `(,(aref remaining-var 0) (aref ,value-expression ,starting-index))
            bindings))
     (t
      ;; Even if we're not using `whole-var', we still need to bind
      ;; a holding variable in case of `rest-var' or the positional
      ;; variables.
      (let ((holding-var (if using-whole-var
                             using-whole-var
                           (gensym "destr-array-"))))

        ;; Otherwise, bind the holding variable and repeatedly access it.
        (push `(,holding-var ,value-expression) bindings)

        (cl-loop named loop
                 for v across remaining-var
                 for idx from starting-index
                 do (cond
                     ((loopy--var-ignored-p v)) ; Do nothing if variable is `_'.
                     (t (if (sequencep v)
                            (dolist (binding (loopy--destructure-sequence
                                              v `(aref ,holding-var ,idx)))
                              (push binding bindings))
                          (push `(,v (aref ,holding-var ,idx))
                                bindings)))))

        ;; Now bind the `&rest' var, if needed.
        (when using-rest-var
          (let ((rest-val `(substring ,holding-var ,rest-pos)))
            (if (sequencep using-rest-var)
                (dolist (binding (loopy--destructure-sequence
                                  using-rest-var rest-val))
                  (push binding bindings))
              (push `(,using-rest-var ,rest-val) bindings))))

        (when (null (cdr bindings))
          (signal 'loopy-destructure-vars-missing (list var))))))

    (nreverse bindings)))

(cl-defun loopy--destructure-list (var value-expression)
  "Destructure VALUE-EXPRESSION according to VAR.

- If the first element of VAR is `&whole', then the next element
  names a variable containing the entire value.
- Positional variable names can be next.
- A variable named after `&rest' or after the dot in a dotted list
  sets that variable to the remainder of the list.  If no positional
  variables are given, then this is the same as `&whole'.
- Variables named after `&key' are values found using plist functions.
  These can optionally be a list of 2 element: (1) a variable name
  and (2) a default value if the corresponding key is not present.
  Keys are only sought in the remainder of the list, be that after
  positional variable names or in a variable named `&rest'.

Only the positional variables and the remainder can be recursive."
  (let ((bindings nil)                  ; The result of this function.
        (whole-var nil)                 ; Variable after `&whole'.
        ;; Variable after `&rest' or a generated symbol.
        (rest-var nil)
        ;; Sequence that was after `&rest', if any.
        (rest-var-was-sequence nil)
        ;; A value holder for if we only use keys.
        (key-target-var (gensym "key-target-"))
        ;; Whether we pop positional variables from `key-target-var'.
        ;; This determines whether we need to bind `key-target-var' before
        ;;  or after processing the positional variables.
        (popping-key-target-var)
        ;; Variables after `&key' or `&keys'.  These are the final values bound,
        ;; but must be detected before the positional variables are processed.
        (key-vars)
        ;; The positional variables processed.  This is a copy of `var', since
        ;; `var' is eaten away in a `while' loop while processing those
        ;; variables.  This affects where `&key' variables are sought.
        (positional-vars)
        ;; Copy for consumption.
        (remaining-var var))

    ;; Find `whole-var'.  If found, remove from `remaining-var'.
    (when (eq (cl-first remaining-var) '&whole)
      (if (null (cdr remaining-var))
          ;; Make sure there is a variable named.
          (signal 'loopy-&whole-missing (list var))
        (let ((possible-whole-var (cl-second remaining-var)))
          (cond
           ;; Make sure we have a variable and not a special symbol.
           ((memq possible-whole-var '(&rest &key &keys))
            (signal 'loopy-&whole-missing (list var)))
           ((sequencep possible-whole-var)
            (signal 'loopy-&whole-sequence (list var)))
           ;; If it's the only variable named, just bind it and return.
           ((and (not (cddr remaining-var))
                 (not (loopy--var-ignored-p possible-whole-var)))
            (warn "`&whole' used when only one variable listed: %s" var)
            (cl-return-from loopy--destructure-list
              `((,possible-whole-var ,value-expression))))
           (t
            ;; Now just operate on remaining variables.
            (setq remaining-var (cddr remaining-var))
            (if (loopy--var-ignored-p possible-whole-var)
                (warn "`&whole' variable ignored: %s" var)
              (setq whole-var possible-whole-var)
              (push `(,whole-var ,value-expression) bindings)))))))

    ;; Find any (_ &rest `rest') or (_ . `rest') variable.  If found, set
    ;; `rest-var' and remove them from the variable list `remaining-var'.
    (let ((possible-rest-var))
      (if (not (proper-list-p remaining-var))
          ;; If REMAINING-VAR is not a proper list, then the last cons cell is dotted.
          (seq-let (other-vars last-var)
              (loopy--split-off-last-var remaining-var)
            (setq remaining-var other-vars
                  possible-rest-var last-var))

        (seq-let (before after)
            (loopy--split-list-before remaining-var '&rest)

          (unless before
            (warn "`&rest' being treated same as `&whole': %s" var))

          (when after
            (let ((rest-var (cl-second after))
                  (vars-after-rest-var (cddr after)))
              (cond ((or (null rest-var)
                         (memq (cl-second after) '(&key &keys)))
                     (signal 'loopy-&rest-missing (list var)))
                    ;; This is the best place to check that argument only uses
                    ;; keys after the `rest-var'.
                    ((and vars-after-rest-var
                          (not (memq (cl-first vars-after-rest-var)
                                     '(&key &keys))))
                     (signal 'loopy-&rest-non-var (list var)))
                    (t
                     ;; Now just operate on remaining variables.
                     (setq remaining-var (append before vars-after-rest-var)
                           possible-rest-var rest-var)))))))

      ;; Finally, bind the &rest var, if any.
      (when (and possible-rest-var
                 (not (loopy--var-ignored-p possible-rest-var)))
        ;; NOTE: For sequence `&rest' vars, we need to destructure
        ;;       /after/ the normal variables have been `pop'-ed off
        ;;       of the value.
        (if (and possible-rest-var
                 (sequencep possible-rest-var))
            (setq rest-var-was-sequence possible-rest-var
                  rest-var (gensym "seq-rest-"))
          (setq rest-var possible-rest-var))))

    ;; Find the key vars, if any.  The key vars must be drawn from
    ;; the remaining part after the normal variables are bound.
    (seq-let (before after)
        (loopy--split-list-before remaining-var '&key)
      ;; We might as well be forgiving of this mistake.
      (unless after
        (seq-let (bef aft)
            (loopy--split-list-before remaining-var '&keys)
          (setq before bef after aft)))
      (when after
        (if (null (cdr after))
            (signal 'loopy-&key-missing (list var))
          (setq key-vars (cdr after)
                remaining-var before))))

    ;; Handle the positional variables.  Generally, we want to `pop' the
    ;; positional values off of some container variable.  This could be the
    ;; `rest' variable, the `whole' variable, the variable in which keys are
    ;; sought, or the last positional variable.
    ;;
    ;; NOTE: By this point, there may still be variable to ignore,
    ;;       so we bind the `rest' var inside of here.
    (when remaining-var
      ;; Whether `positional-vars' is non-nil affects where keys are sought.  We
      ;; just need to record that they exists before we consume `remaining-var'
      ;; in the `while' list.
      (setq positional-vars remaining-var)

      ;; If we can, we should skip over as many ignored values as possible.
      ;;
      ;; We still need to record where ignored values end so that we can
      ;; correctly bind the `rest' var, and where true values being
      ;; so that we can start popping from the correct place.
      (let* ((fist-positional-pos 0)
             (rest-pos (length remaining-var))
             (last-positional-pos (1- rest-pos)))

        (cl-loop for v in (copy-sequence remaining-var)
                 while (loopy--var-ignored-p v)
                 do
                 (cl-incf fist-positional-pos)
                 (setq remaining-var (cl-rest remaining-var)))

        ;; `cl-subseq' uses an exclusive final argument
        (cl-loop with final-exclusive-index = (- rest-pos fist-positional-pos)
                 for v in (reverse remaining-var)
                 while (loopy--var-ignored-p v)
                 do (progn
                      (cl-decf final-exclusive-index)
                      (cl-decf last-positional-pos))
                 finally do
                 (cl-callf cl-subseq remaining-var 0 final-exclusive-index))

        ;; If need be, bind the `rest' variable.  If there are no key vars,
        ;; no positional vars, and the rest var is a sequence, then we can just
        ;; destructure
        (when rest-var
          (let* ((val-expr (or whole-var value-expression))
                 (rest-val (if (zerop fist-positional-pos)
                               val-expr
                             `(nthcdr ,fist-positional-pos ,val-expr))))
            (cond
             (key-vars
              ;; Otherwise, if no positional variables, just bind the &rest var.
              (push `(,rest-var ,rest-val) bindings))
             ((null remaining-var)
              (if (and rest-var-was-sequence (null key-vars))
                  (dolist (binding (loopy--destructure-sequence
                                    rest-var-was-sequence rest-val))
                    (push binding bindings))
                (push `(,rest-var ,rest-val) bindings)))
             (t
              (push `(,rest-var ,rest-val) bindings)))))

        ;; If we don't need a pop target, then we can take a shortcut
        ;; and consume the single remaining variable.
        (when (and (null rest-var)
                   (null key-vars)
                   (= 1 (length remaining-var)))
          (let ((single-var (cl-first remaining-var))
                (final-val `(nth ,fist-positional-pos
                                 ,(or whole-var value-expression))))
            (if (sequencep single-var)
                (dolist (binding (loopy--destructure-sequence
                                  single-var final-val))
                  (push binding bindings))
              (push `(,single-var ,final-val) bindings)))
          ;; Consume final remaining positional var.
          (setq remaining-var nil))

        ;; Otherwise, if there are still unignored positional variables,
        ;; we need to decide how to pop them off.
        (when remaining-var

          (let (;; The positional variables sans those that can be ignored given the
                ;; destructuring requirements.
                (popped-vars)
                ;; Whence positional values are popped.  This can be a generated
                ;; variable.
                (pop-target)
                ;; Whether we'll need to do more destructuring after processing
                ;; the variables in `popped-vars'.  This is the orignal sequence,
                ;; not the generated variable.
                (pop-target-was-seq)
                ;; If `pop-target' is the last valid positional variable, then it
                ;; needs to be extracted from a list of remaining values after the
                ;; preceding positional variables are bound.  This is not a concern
                ;; when `rest-var' is the `pop-target'.
                (pop-target-is-positional-var))

            ;; Choose the variables to bind and whence they will be extracted.
            (cond
             ;; Rest var is bound in its own section, in case there are no
             ;; positional variables.  Otherwise, it would be bound here.
             (rest-var (setq pop-target rest-var
                             popped-vars remaining-var
                             pop-target-was-seq rest-var-was-sequence
                             pop-target-is-positional-var nil))

             (key-vars (setq pop-target key-target-var
                             popped-vars remaining-var
                             pop-target-was-seq nil
                             pop-target-is-positional-var nil)

                       ;; `key-target-var' is only used with `&key' without
                       ;; `&rest'.
                       (let ((val (or whole-var value-expression)))
                         (push `(,key-target-var
                                 ,(if (zerop fist-positional-pos)
                                      val
                                    `(nthcdr ,fist-positional-pos ,val)))
                               bindings))
                       (setq popping-key-target-var t))

             ;; TODO: Optimize when final var is pop var.
             (t (seq-let (other-vars last-var)
                    (loopy--split-off-last-var remaining-var)

                  (setq pop-target-is-positional-var t
                        pop-target-was-seq (and (sequencep last-var) last-var)
                        popped-vars other-vars
                        pop-target (if pop-target-was-seq
                                       (gensym "pop-target-")
                                     last-var))

                  (let ((val (or whole-var value-expression)))
                    (push `(,pop-target ,(if (zerop fist-positional-pos)
                                             val
                                           `(nthcdr ,fist-positional-pos ,val)))
                          bindings)))))

            ;; Now that variables are decided, pop `popped-vars' off of the value of
            ;; `pop-target'.  If there are sublists of ignored variables, we skip
            ;; over all of them and simply set the `pop-target' to some nth `cdr' of
            ;; itself.
            (while popped-vars
              (let ((i (car popped-vars)))
                (setq popped-vars (cdr popped-vars))
                (cond ((sequencep i)
                       (dolist (binding (loopy--destructure-sequence
                                         i `(pop ,pop-target)))
                         (push binding bindings)))
                      ((loopy--var-ignored-p i)
                       ;; Combine multiple ignored popped-vars.
                       (let ((count (loopy--count-while
                                     #'loopy--var-ignored-p popped-vars)))
                         ;; `nthcdr' is a C function, so it should be fast enough
                         ;; even for high counts.
                         (push `(,pop-target (nthcdr ,(1+ count) ,pop-target))
                               bindings)
                         (setq popped-vars (nthcdr count popped-vars))))
                      (t
                       (push `(,i (pop ,pop-target))
                             bindings)))))


            ;; Since we can ignore positions between the last unignored
            ;; positional variable and the rest var, we need to make sure that
            ;; the rest var is the correct Nth cdr now that we're done popping.
            ;;
            ;; `rest-pos' is technically just the length of the list of
            ;; positional variables before we started processing them,
            ;; so it is always bound to a number.
            (when (or rest-var key-vars)
              (let ((pos-diff (- rest-pos last-positional-pos))
                    (var (or rest-var key-target-var)))
                (when (> pos-diff 1)
                  (push `(,var (nthcdr ,(1- pos-diff) ,var))
                        bindings))))

            ;; Do final update of `pop-target' if need be.  We only need to do this
            ;; if it was a sequence (in which case there are more variables to bind)
            ;; or if it was a positional variable.
            (cond
             (pop-target-was-seq
              (dolist (bind (loopy--destructure-sequence
                             ;; If `pop-target' is `rest-var', then it is the
                             ;; remainder of the current list. Else, `pop-target' is
                             ;; an element of that list.
                             pop-target-was-seq (if rest-var
                                                    pop-target
                                                  `(car ,pop-target))))
                (push bind bindings)))
             (pop-target-is-positional-var
              (push `(,pop-target (car ,pop-target)) bindings)))))))

    ;; Now process the keys.
    (when key-vars
      (let ((target-var (or rest-var
                            ;; If we used positional variables, then they can be
                            ;; popped off of `key-target-var', which is bound
                            ;; to the value expression or `whole-var'.
                            (and positional-vars key-target-var)
                            whole-var
                            key-target-var)))

        ;; If we are only using keys, then we need to create a holding variable in
        ;; which to search.
        (when (and (eq target-var key-target-var)
                   (null popping-key-target-var))
          (let ((val (or whole-var value-expression)))
            (push `(,key-target-var
                    ,(if positional-vars
                         `(nthcdr ,(length positional-vars) ,val)
                       val))
                  bindings)))

        ;; TODO: In Emacs 28, `pcase' was changed so that all named variables
        ;; are at least bound to nil.  Before that version, we should make sure
        ;; that `default' is bound.
        (let ((default nil))
          (ignore default)
          (pcase-dolist ((or `(,key-var ,default)
                             key-var)
                         key-vars)
            (let ((key (intern (format ":%s" key-var))))
              (push `(,key-var
                      ,(if default
                           `(if-let ((key-found (plist-member ,target-var ,key)))
                                (cl-second key-found)
                              ,default)
                         `(plist-get ,target-var ,key)))
                    bindings))))))

    ;; Check that things were bound.
    (when (null bindings)
      (signal 'loopy-destructure-vars-missing (list var)))

    ;; Fix the order of the bindings and return.
    (nreverse bindings)))

;;;;; Destructuring Generalized Variables
(defun loopy--destructure-generalized-sequence (var value-expression)
  "Destructure VALUE-EXPRESSION according to VAR as `setf'-able places.

VALUE-EXPRESSION should itself be a `setf'-able place.

Returns a list of bindings suitable for `cl-symbol-macrolet'."
  (cl-typecase var
    (symbol (unless (loopy--var-ignored-p var)
              `((,var ,value-expression))))
    (list   (loopy--destructure-generalized-list var value-expression))
    (array  (loopy--destructure-generalized-array var value-expression))
    (t      (signal 'loopy-destructure-type (list var)))))

(defun loopy--destructure-generalized-array (var value-expression)
  "Destructure VALUE-EXPRESSION according to VAR as `setf'-able places.

VALUE-EXPRESSION should itself be a `setf'-able place.

Returns a list of bindings suitable for `cl-symbol-macrolet'.

- `&rest' references a subsequence place.
- `&whole' references the entire place."
  (let ((bindings)
        (using-rest-var)
        (remaining-var var)
        (using-whole-var)
        (remaining-length (length var)))

    (when (eq '&whole (aref var 0))
      (cond ((= 1 remaining-length)
             (signal 'loopy-&whole-missing (list var)))
            ((sequencep (aref remaining-var 1))
             (signal 'loopy-&whole-sequence (list var)))
            (t
             (let ((whole-var (aref remaining-var 1)))
               (when (= 2 remaining-length)
                 (warn "`&whole' variable used alone: %s" var))

               (setq remaining-var (substring remaining-var 2)
                     remaining-length (max 0 (- remaining-length 2)))

               (if (loopy--var-ignored-p whole-var)
                   (warn "`&whole' variable ignored: %s" var)
                 (setq using-whole-var whole-var))))))

    (when-let ((pos (cl-position '&rest remaining-var :test #'eq)))
      (cond
       ((= (1+ pos) remaining-length)
        (signal 'loopy-&rest-missing (list var)))
       ((> remaining-length (+ 2 pos))
        (signal 'loopy-&rest-multiple (list var)))
       (t
        (setq using-rest-var (aref remaining-var (1+ pos))
              remaining-length (max 0 (- remaining-length 2))
              remaining-var (substring remaining-var 0 -2)))))

    (when using-whole-var
      (push `(,using-whole-var ,value-expression) bindings))

    (cl-loop for v across remaining-var
             for idx from 0
             do (cond
                 ((loopy--var-ignored-p v)) ; Do nothing if variable is `_'.
                 (t (if (sequencep v)
                        (dolist (binding (loopy--destructure-generalized-sequence
                                          v `(aref ,value-expression ,idx)))
                          (push binding bindings))
                      (push `(,v (aref ,value-expression ,idx))
                            bindings)))))

    ;; Now bind the `&rest' var, if needed.
    (when (and using-rest-var
               (not (loopy--var-ignored-p using-rest-var)))
      ;; Note: Can't use the more specific `substring' here, as that would
      ;;       convert the sequence to a string in `setf'.
      (let ((rest-val `(cl-subseq ,value-expression ,remaining-length)))
        (if (sequencep using-rest-var)
            (dolist (binding (loopy--destructure-sequence
                              using-rest-var rest-val))
              (push binding bindings))
          (push `(,using-rest-var ,rest-val) bindings))))

    (nreverse bindings)))

(cl-defun loopy--destructure-generalized-list (var value-expression)
  "Destructure VALUE-EXPRESSION according to VAR as `setf'-able places.

VALUE-EXPRESSION should itself be a `setf'-able place.

returns a list of bindings suitable for `cl-symbol-macrolet'.

- `&rest' references a subsequence place.
- `&whole' references the entire place.

See `loopy--destructure-list' for normal values."
  (let ((var-list var)                  ; For reporting errors.
        (bindings nil))                 ; The result of this function.

    (when (eq (cl-first var) '&whole)
      (cond
       ;; Make sure there is a variable named.
       ((null (cdr var))
        (signal 'loopy-&whole-missing (list var-list)))
       ;; If it's the only variable named, just bind it and return.
       ((null (cddr var))
        (warn "`&whole' used when only one variable listed: %s"
              var-list)
        (cl-return-from loopy--destructure-generalized-list
          `((,(cl-second var) ,value-expression))))
       (t
        (let ((possible-whole-var (cl-second var)))
          (if (loopy--var-ignored-p possible-whole-var)
              (warn "`&whole' variable being ignored: %s" var-list)
            ;; Now just operate on remaining variables.
            (push `(,possible-whole-var ,value-expression)
                  bindings)))
        (setq var (cddr var)))))

    ;; Now handle the remaining variables.  Since we're not storing a value,
    ;; we don't need to do any `pop'-ing like in `loopy--destructure-list'.
    ;; However, we still need to keep track of where to look for keys.
    ;;
    ;; Since it's possible for `var' to be a dotted list, we only know
    ;; where to look after processing the entire list `var'.
    (let ((var-is-dotted (not (proper-list-p var)))
          (rest-var-value nil)
          (last-positional-var-index)
          (positional-vars-used)
          (key-vars))

      (let ((looking-at-key-vars nil)
            (index 0)
            (v nil))
        (while (car-safe var)
          (setq v (car var))
          (cond
           ((loopy--var-ignored-p v)
            (setq var (cdr var))
            (cl-incf index))                  ; Do nothing in this case.

           ((eq v '&rest)
            (setq looking-at-key-vars nil)
            (let ((rest-var (cl-second var))
                  (vars-after-rest-var (cddr var)))
              (cond
               (var-is-dotted
                (signal 'loopy-&rest-dotted (list var-list)))
               ((or (null rest-var)
                    (memq rest-var '(&key &keys)))
                (signal 'loopy-&rest-missing (list var-list)))
               ((and vars-after-rest-var
                     (not (memq (cl-first vars-after-rest-var)
                                '(&key &keys))))
                (signal 'loopy-&rest-non-var (list var-list)))
               (t
                (unless (loopy--var-ignored-p rest-var)
                  (setq rest-var-value `(nthcdr ,index ,value-expression))
                  (if (sequencep rest-var)
                      (dolist (bind (loopy--destructure-generalized-sequence
                                     rest-var rest-var-value))
                        (push bind bindings))
                    (push `(,rest-var ,rest-var-value)
                          bindings)))
                (setq var (cddr var))
                (cl-incf index 2)))))

           ;; For keys, we don't want to increase the index, just skip over
           ;; them.  Key variables stop once `&rest' or the last cdr of a
           ;; dotted list is reached (at which point the loop exits).
           ((memq v '(&key &keys))
            (setq looking-at-key-vars t
                  var (cl-rest var)))

           (looking-at-key-vars
            (push v key-vars)
            (setq var (cl-rest var)))

           (t
            (if (sequencep v)
                (dolist (bind (loopy--destructure-generalized-sequence
                               v `(nth ,index ,value-expression)))
                  (push bind bindings))
              (push `(,v (nth ,index ,value-expression)) bindings))
            (setq var (cl-rest var)
                  last-positional-var-index index
                  positional-vars-used t)
            (cl-incf index))))

        ;; If it was a dotted list, then `var' is now an atom.
        (when var
          ;; The first `cdr' is 1, not 0, so we must add 1 here to get the
          ;; remainder of the list after the last positional variable.
          (let ((cdr-value `(nthcdr ,(1+ last-positional-var-index)
                                    ,value-expression)))
            (setq rest-var-value cdr-value)
            (push `(,var ,cdr-value) bindings))))

      ;; Decide where to look for keys, if any.
      (when key-vars
        (let ((key-target-value (or rest-var-value
                                    (and positional-vars-used
                                         ;; The first `cdr' is 1, not 0, so we
                                         ;; must add 1 here to get the remainder
                                         ;; of the list after the last
                                         ;; positional variable.
                                         `(nthcdr ,(1+ last-positional-var-index)
                                                  ,value-expression))
                                    value-expression)))
          (dolist (k key-vars)
            (push `(,k (plist-get ,key-target-value
                                  ,(intern (format ":%s" k))))
                  bindings)))))

    ;; Fix the order of the bindings and return.
    (nreverse bindings)))


;;;; Loop Tag Names
(defun loopy--produce-non-returning-exit-tag-name (&optional loop-name)
  "Produce a tag from LOOP-NAME."
  (if loop-name
      (intern (format "loopy--%s-non-returning-exit-tag" loop-name))
    'loopy--non-returning-exit-tag))


(defun loopy--produce-skip-tag-name (&optional loop-name)
  "Produce a tag from LOOP-NAME."
  (if loop-name
      (intern (format "loopy-%s-skip-tag"  loop-name))
    'loopy--skip-tag))


;;;; Quoted Symbols and Functions
(defun loopy--get-function-symbol (function-form)
  "Return the actual symbol described by FUNCTION-FORM.

When a quoted argument is passed to a macro, it can appear
as `(quote my-var)' or `(function my-func)' inside the body.  For
expansion, we generally only want the actual symbol."
  (pcase function-form
    ((or (pred symbolp) `(lambda ,_))           function-form)
    ;; This could be something like "(function (lambda () ...))".
    (`(,(or 'function 'quote 'cl-function) ,fn) fn)
    (_ (signal 'loopy-bad-function-form (list function-form)))))

(defalias 'loopy--normalize-symbol #'loopy--get-quoted-symbol
  "Make QUOTED-FORM normally quoted instead of maybe doubly quoted.")
(defun loopy--get-quoted-symbol (quoted-form)
  "Return the actual symbol of QUOTED-FORM.

When quoted symbols are passed to the macro, these can show up as
\"(quote SYMBOL)\", where we only want SYMBOL.

For functions, use `loopy--get-function-symbol'."
  (pcase quoted-form
    ((pred symbolp) quoted-form)
    (`(quote ,form) form)
    (_              (signal 'loopy-bad-quoted-form (list quoted-form)))))

(defun loopy--quoted-form-p (form-or-symbol)
  "Whether FORM-OR-SYMBOL is quoted via `quote' or `function'.

If not, then it is possible that FORM is a variable."

  (and (listp form-or-symbol)
       (= 2 (length form-or-symbol))
       (or (eq (car form-or-symbol) 'quote)
           (eq (car form-or-symbol) 'function)
           (eq (car form-or-symbol) 'cl-function))))

(defun loopy--apply-function (func &rest args)
  "Return an expansion to appropriately apply FUNC to ARGS.

This expansion can apply FUNC directly or via `funcall'."
  (if (loopy--quoted-form-p func)
      `(,(loopy--get-function-symbol func) ,@args)
    `(funcall ,func ,@args)))


;;;; Indexing

(defun loopy--generate-inc-idx-instructions
    (index-holder increment-holder by decreasing)
  "Generate instructions for incrementing an index variable.

If possible, directly use a number in the code instead of storing
it in a variable, since that seems to be faster.

INDEX-HOLDER is the variable use for index.
INCREMENT-HOLDER is the variable to store the increment.
BY is the increment passed in the parsing function.
DECREASING is whether the increment should be decreasing.

Returns a list of instructions."
  (if (numberp by)
      `((loopy--latter-body
         (setq ,index-holder (,(if decreasing #'- #'+)
                              ,index-holder ,by))))
    `((loopy--iteration-vars (,increment-holder ,by))
      (loopy--latter-body
       (setq ,index-holder (,(if decreasing #'- #'+)
                            ,index-holder ,increment-holder))))))

(provide 'loopy-misc)
;;; loopy-misc.el ends here
