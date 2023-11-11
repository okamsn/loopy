;;; loopy-misc.el --- Miscellaneous functions used with Loopy. -*- lexical-binding: t; -*-

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
;; This library provides features used by several of the libraries that form
;; the package `loopy'.  This separation exists for better organization.

;;; Code:

;; NOTE: This file can't require any of the other `loopy' files.
(require 'cl-lib)
(require 'map)
(require 'compat)
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

(define-error 'loopy-wrong-number-of-command-arguments-or-bad-keywords
              "Loopy: Wrong number of arguments or wrong keywords"
              '(loopy-error loopy-bad-command-arguments))

;;;;; Errors on Accumulations
(define-error 'loopy-incompatible-accumulations
              "Loopy: Incompatible accumulations"
              'loopy-error)

(define-error 'loopy-incompatible-accumulation-final-updates
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

(define-error 'loopy-reinitializing-iteration-variable
  "Loopy: Can only define iteration once"
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

(define-error 'loopy-bad-run-time-destructuring
              "Loopy: Bad run-time destructuring (value doesn't match)"
              'loopy-error)

(define-error 'loopy-&whole-sequence
              "Loopy: `&whole' variable is sequence"
              'loopy-bad-desctructuring)

(define-error 'loopy-&whole-missing
              "Loopy: `&whole' variable is missing"
              'loopy-bad-desctructuring)

(define-error 'loopy-&whole-bad-position
              "Loopy: `&whole' in bad position"
              'loopy-bad-desctructuring)

(define-error 'loopy-&rest-missing
              "Loopy: `&rest' variable is missing"
              'loopy-bad-desctructuring)

(define-error 'loopy-&rest-non-var
              "Loopy: Non-variable item after `&rest'"
              'loopy-bad-desctructuring)

(define-error 'loopy-&optional-bad-position
              "Loopy: `&optional' in bad position"
              'loopy-bad-desctructuring)

(define-error 'loopy-&optional-ignored-default-or-supplied
              "Loopy: Using default or asking whether supplied for ignored `&optional'"
              'loopy-bad-desctructuring)

(define-error 'loopy-&optional-generalized-variable
              "Loopy: `&optional' variables not implemented for generalized variables"
              'loopy-bad-destructuring)

(define-error 'loopy-generalized-default
              "Loopy: Default values not implemented for generalized variables"
              'loopy-bad-destructuring)

(define-error 'loopy-generalized-supplied
              "Loopy: `SUPPLIED-P' variables not implemented for generalized variables"
              'loopy-bad-destructuring)

(define-error 'loopy-&rest-multiple
              "Loopy: Multiple variables after `&rest'"
              'loopy-bad-desctructuring)

(define-error 'loopy-&rest-dotted
              "Loopy: Using `&rest' in dotted (improper) list"
              'loopy-bad-desctructuring)

(define-error 'loopy-&rest-bad-position
              "Loopy: `&rest' or `&body' in bad position"
              'loopy-bad-desctructuring)

(define-error 'loopy-&key-missing
              "Loopy: `&key' variable is missing"
              'loopy-bad-desctructuring)

(define-error 'loopy-&key-bad-position
              "Loopy: `&key' or `&keys' in bad position"
              'loopy-bad-desctructuring)

(define-error 'loopy-&allow-other-keys-without-&key
              "Loopy: Used `&allow-other-keys' before or without `&key'"
              'loopy-bad-desctructuring)

(define-error 'loopy-&key-unmatched
              "Loopy: Value destructured by `&key' not matching without `&allow-other-keys'"
              'loopy-bad-desctructuring)

(define-error 'loopy-&key-var-malformed
              "Loopy: Malformed variable for `&key'"
              'loopy-bad-desctructuring)

(define-error 'loopy-&key-array
              "Loopy: Use of `&key' in array"
              'loopy-bad-desctructuring)

(define-error 'loopy-&key-key-from-sequence
              "Loopy: Can't create `&key' key from a sequence"
              'loopy-bad-desctructuring)

(define-error 'loopy-&map-var-malformed
              "Loopy: Malformed variable for `&map'"
              'loopy-bad-desctructuring)

(define-error 'loopy-&map-missing
              "Loopy: `&map' variable is missing"
              'loopy-bad-desctructuring)

(define-error 'loopy-&map-bad-position
              "Loopy: `&map'in bad position"
              'loopy-bad-desctructuring)

(define-error 'loopy-&map-key-from-sequence
              "Loopy: Can't create `&map' key from a sequence"
              'loopy-bad-desctructuring)

(define-error 'loopy-&aux-bad-position
              "Loopy: `&aux' in bad position"
              'loopy-bad-desctructuring)

(define-error 'loopy-&aux-var-malformed
              "Loopy: Malformed variable for `&aux'"
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

;; Similar to `seq--count-successive'.
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

(defmacro loopy--plist-bind (bindings plist &rest body)
  "Bind values in PLIST to variables in BINDINGS, surrounding BODY.

- PLIST is a property list.

- BINDINGS is of the form (KEY VAR KEY VAR ...).  VAR has the
  form (NAME [DEFAULT [PROVIDED]]) as in `cl-destructuring-bind'.

- BODY is the same as in `let'.

This is a wrapper around `cl-destructuring-bind'.  The difference is
that we do not need to specify `&allow-other-keys' and that
keywords and variables are separate."
  (declare (indent 2))
  `(cl-destructuring-bind (&key
                           ,@(cl-loop for (key var . _) on bindings by #'cddr
                                      if (consp var)
                                      collect `((,key ,(cl-first var)) ,@(cdr var))
                                      else collect `((,key ,var)))
                           &allow-other-keys)
       ,plist
     ,@body))

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

;; TODO: Byte optimization for `funcall' with a quoted argument
;;       should expand to (FUNC ARGS...), so we shouldn't need
;;       this function.
(defun loopy--apply-function (func &rest args)
  "Return an expansion to appropriately apply FUNC to ARGS.

This expansion can apply FUNC directly or via `funcall'."
  (if (loopy--quoted-form-p func)
      `(,(loopy--get-function-symbol func) ,@args)
    `(funcall ,func ,@args)))


;;;; Membership

(cl-defun loopy--member-p (list element &key (test #'equal) key)
  "Check whether ELEMENT is in LIST using TEST.

KEY is applied to both ELEMENT and the sequences of the list.

This function is like `seq-contains-p' and `cl-member',
but TEST is guaranteed to receive the value from the list
first and ELEMENT second."
  ;; `adjoin' applies KEY to both the new item and old items in
  ;; list, while `member' only applies KEY to items in the list.
  ;; To be consistent and apply KEY to all items, we use
  ;; `cl-member-if' with a custom predicate instead.
  ;;
  ;; The CLHS is wrong in how `adjoin' works.  See #170.
  (declare (compiler-macro loopy--member-p-comp))
  (setq test (or test #'equal))
  (if key
      (cl-loop with test-val = (funcall key element)
               for i in list
               thereis (funcall test (funcall key i) test-val))
    (pcase test
      ('equal (member element list))
      ('eql   (memql  element list))
      ('eq    (memq   element list))
      (_ (cl-loop for i in list
                  thereis (funcall test i element))))))

(cl-defun loopy--member-p-comp (form list element &key (test '#'equal) key)
  "Expand `loopy--member-p' to a more efficient function when possible.

FORM is the original use of the function.  LIST is the sequence
in which ELEMENT is sought.  TEST compare the elements of LIST and ELEMENT.
KEY transforms those elements and ELEMENT."
  (if key
      (cl-with-gensyms (test-val seq-val)
        `(cl-loop with ,test-val = (funcall ,key ,element)
                  for ,seq-val in ,list
                  thereis (funcall ,test (funcall ,key ,seq-val) ,test-val)))
    ;; This logic take from `cl--constr-expr-p'.
    (pcase (let ((test (macroexpand-all test macroexpand-all-environment)))
             (if (macroexp-const-p test)
                 (if (consp test)
                     (nth 1 test)
                   test)))
      ('equal `(member ,element ,list))
      ('eql   `(memql  ,element ,list))
      ('eq    `(memq   ,element ,list))
      (_ form))))

(cl-defmacro loopy--pcase-let-workaround (variables form)
  "Wrap FORM in a `let' with VARIABLES bound to nil on Emacs less than 28.

Prior to Emacs 28, it was not guaranteed that `pcase-let' bound
unmatched variables."
  (declare (indent 1))
  (if (eval-when-compile (< emacs-major-version 28))
      `(let ,(mapcar (lambda (sym) `(,sym nil))
                     variables)
         ,(cons 'ignore variables)
         ,form)
    form))

(provide 'loopy-misc)
;;; loopy-misc.el ends here
