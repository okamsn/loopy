;;; loopy-misc.el --- Miscellaneous functions used with Loopy. -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: July 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.8.1
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

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

;;;; List Processing

(defun loopy--count-while (pred list)
  "Count the number of items while PRED is true in LIST.

This function returns 0 if PRED is immediately false.
PRED is a function taking one argument: the item.

For example, applying `cl-evenp' on (2 4 6 7) returns 3."
  (cl-loop for i in list
           while (funcall pred i)
           sum 1))

(defun loopy--count-until (pred list)
  "Count the number of items until PRED is true in LIST.

This function returns 0 if PRED is immediately true.
PRED is a function taking one argument: the item.

For example, applying `cl-oddp' on (2 4 6 7) returns 3."
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

(defun loopy--split-off-last-item (list)
  "Split LIST, returning a list of the first items and the last item.

For example, splitting (1 2 3) returns ((1 2) 3)."
  ;; TODO: How does this compare with `last' and `butlast' for small lists?
  (let ((reverse-list (reverse list)))
    (list (reverse (cl-rest reverse-list))
          (cl-first reverse-list))))




;;;; Destructuring
;; This better allows for things to change in the future.
(defun loopy--var-ignored-p (var)
  "Return whether VAR should be ignored."
  (eq var '_))

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
    (symbol `((,(if (eq '_ seq) (gensym "ignored-value-") seq)
               ,value-expression)))
    (list (loopy--destructure-list seq value-expression))
    (array (loopy--destructure-array seq value-expression))
    (t (error "Type unknown: %s" seq))))

(defun loopy--destructure-array (var value-expression)
  "Return a list of bindings destructuring VALUE-EXPRESSION according to VAR.

- If `&rest', bind the remaining values in the array.
- If `&whole', name a variable holding the whole value."
  (let ((bindings)
        (holding-var)
        (remaining-var))
    (if (eq '&whole (aref var 0))
        (setq holding-var (aref var 1)
              remaining-var (seq-drop var 2))
      (setq holding-var (gensym "cl-array-")
            remaining-var var))

    (push `(,holding-var ,value-expression)
          bindings)

    (cl-loop named loop
             with array-length = (length remaining-var)
             for v across remaining-var
             for idx from 0
             do (cond
                 ((eq v '_)) ; Do nothing if variable is `_'.
                 ((eq v '&rest)
                  (let* ((next-idx (1+ idx))
                         (next-var (aref remaining-var next-idx)))
                    ;; Check that the var after `&rest' is the last:
                    (when (> (1- array-length) next-idx)
                      (error "More than one variable after `&rest': %s"
                             var))

                    (if (sequencep next-var)
                        (dolist (binding (loopy--destructure-sequence
                                          next-var `(seq-subseq ,holding-var
                                                                ,idx)))
                          (push binding bindings))
                      (push `(,next-var (seq-subseq ,holding-var ,idx))
                            bindings))
                    ;; Exit the loop.
                    (cl-return-from loop)))
                 (t (if (sequencep v)
                        (dolist (binding (loopy--destructure-sequence
                                          v `(aref ,holding-var ,idx)))
                          (push binding bindings))
                      (push `(,v (aref ,holding-var ,idx))
                            bindings)))))
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
        (rest-var nil)                  ; Variable after `&rest'.
        (rest-var-was-sequence nil)     ; Whether that var was a sequence.
        (key-target-var (gensym "key-target-")) ; Holder for if we only use keys.
        (key-vars)                      ; Variables after `&key' or `&keys'.
        (positional-vars))

    (when (eq (cl-first var) '&whole)
      (cond
       ;; Make sure there is a variable named.
       ((null (cdr var))
        (error "Bad destructuring: %s" var))
       ;; If it's the only variable named, just bind it and return.
       ((not (cddr var))
        (warn "`&whole' used when only one variable listed: %s" var)
        (cl-return-from loopy--destructure-list
          `((,(cl-second var) ,value-expression))))
       (t
        (let ((possible-whole-var (cl-second var)))
          (setq whole-var (if (eq possible-whole-var '_)
                              (progn
                                (warn "`&whole' won't be ignored: %s" var)
                                (gensym "list-whole-"))
                            possible-whole-var)
                ;; Now just operate on remaining variables.
                var (cddr var))
          (push `(,whole-var ,value-expression) bindings)))))

    ;; Find any (_ &rest `rest') or (_ . `rest') variable.
    (let ((possible-rest-var))
      (if (proper-list-p var)
          (seq-let (before after) (loopy--split-list-before var '&rest)

            (unless before (warn "`&rest' being treated same as `&whole': %s" var))

            (when after
              ;; This is the best place to check that argument only uses
              ;; keys after the `rest-var'.
              (if-let* ((vars-after-rest-var (cddr after)))
                  (progn
                    (unless (memq (cl-first vars-after-rest-var) '(&key &keys))
                      (error "Bad arguments after `&rest' var: %s"
                             vars-after-rest-var))
                    ;; Now just operate on remaining variables.
                    (setq var (append before vars-after-rest-var)))
                (setq var before))

              (setq possible-rest-var (cl-second after))))

        ;; If VAR is not a proper list, then the last cons cell is dotted.
        (setq possible-rest-var (cdr (last var))
              ;; Now just operate on remaining variables.
              ;; TODO: We use this `car-safe' phrasing in several places.
              ;;       Is there a better way?
              var (let ((var-copy var)
                        (result))
                    ;; For a dotted pair, the final `cdr' is not `car-safe',
                    ;; since it is not a list.  We do not perform that final `cdr'.
                    (while (car-safe var-copy)
                      (push (pop var-copy)
                            result))
                    (nreverse result))))

      ;; Finally, bind the &rest var, if any.
      (when (and possible-rest-var
                 (not (loopy--var-ignored-p possible-rest-var)))
        ;; NOTE: For sequence `&rest' vars, we need to destructure
        ;;       /after/ the normal variables have been `pop'-ed off
        ;;       of the value.
        (if (and possible-rest-var
                 (sequencep possible-rest-var))
            (setq rest-var-was-sequence possible-rest-var
                  rest-var (gensym "list-rest-"))
          (setq rest-var possible-rest-var))
        (push `(,rest-var ,(or whole-var value-expression))
              bindings)))

    ;; Find the key vars, if any.  The key vars must be drawn from
    ;; the remaining part after the normal variables of bound.
    (seq-let (before after)
        (loopy--split-list-before var '&key)
      ;; We might as well be forgiving of this mistake.
      (unless after
        (seq-let (bef aft)
            (loopy--split-list-before var '&keys)
          (setq before bef after aft)))
      (when after
        (setq key-vars (cdr after)
              var before)))

    ;; Handle the normal variables.  Generally, we want to `pop' the normal
    ;; values off of some container variable.  This could be the variable used
    ;; after `&rest', the variable in which keys after `&key' will be sought,
    ;; a copy of the variable after `&whole', or just the last variable given.
    ;;
    ;; TODO: This code is very simple, but could probably be condensed.
    (when var
      (setq positional-vars var)
      (cond
       (rest-var (while var
                   (let ((i (car var)))
                     (setq var (cdr var))
                     (cond ((sequencep i)
                            (dolist (binding (loopy--destructure-sequence
                                              i `(pop ,rest-var)))
                              (push binding bindings)))
                           ((eq i '_)
                            ;; Combine multiple ignored vars.
                            (let ((count (loopy--count-while
                                          #'loopy--var-ignored-p var)))
                              (push `(,rest-var (nthcdr ,(1+ count) ,rest-var))
                                    bindings)
                              (setq var (nthcdr count var))))
                           (t
                            (push `(,i (pop ,rest-var))
                                  bindings)))))

                 ;; NOTE: For sequence `&rest' vars, we need to destructure
                 ;;       /after/ the normal variables have been `pop'-ed off
                 ;;       of the value.
                 (when rest-var-was-sequence
                   (dolist (bind (loopy--destructure-sequence
                                  rest-var-was-sequence rest-var))
                     (push bind bindings))))

       ;; We want to limit the search for keys to those values after the
       ;; positional variables.
       (key-vars (if whole-var
                     (push `(,key-target-var ,whole-var)
                           bindings)
                   (push `(,key-target-var ,value-expression)
                         bindings))
                 (while var
                   (let ((i (car var)))
                     (setq var (cl-rest var))

                     ;; `pop' off of key-target-var to reduce the search for keys.
                     (cond ((sequencep i)
                            (dolist (binding (loopy--destructure-sequence
                                              i `(pop ,key-target-var)))
                              (push binding bindings)))
                           ((eq i '_)
                            ;; Combine multiple ignored vars.
                            (let ((count (loopy--count-while
                                          #'loopy--var-ignored-p var)))
                              (push `(,key-target-var
                                      (nthcdr ,(1+ count) ,key-target-var))
                                    bindings)
                              (setq var (nthcdr count var))))
                           (t
                            (push `(,i (pop ,key-target-var))
                                  bindings))))))

       ;; We want to create a copy of the `whole-var', then pop values
       ;; off of that copy.
       (whole-var (seq-let (other-vars last-var)
                      (loopy--split-off-last-item var)
                    ;; If the last variable is to be ignored, we would prefer
                    ;; to just find a valid variable.
                    (when (loopy--var-ignored-p last-var)
                      (let* ((reverse-var (reverse other-vars))
                             (count (loopy--count-while
                                     #'loopy--var-ignored-p reverse-var))
                             (found (nthcdr count reverse-var)))
                        (setq last-var (cl-first found)
                              other-vars (reverse (cl-rest found)))))
                    ;; TODO: Simplify these two branches.
                    (if (and last-var (sequencep last-var))
                        (let ((whole-copy (gensym "whole-copy-")))
                          (push `(,whole-copy ,whole-var) bindings)
                          (while other-vars
                            (let ((i (cl-first other-vars)))
                              (setq other-vars (cl-rest other-vars))
                              (cond
                               ;; Handle sequences.
                               ((sequencep i)
                                (dolist (binding (loopy--destructure-sequence
                                                  i `(pop ,whole-copy)))
                                  (push binding bindings)))
                               ;; Handle ignored variables.
                               ((eq i '_)
                                ;; Combine multiple ignored vars.
                                (let ((count (loopy--count-while
                                              #'loopy--var-ignored-p var)))
                                  (push `(,whole-copy
                                          (nthcdr ,(1+ count) ,whole-copy))
                                        bindings)
                                  (setq other-vars (nthcdr count other-vars))))
                               ;; Handle normal variables.
                               (t
                                (push `(,i (pop ,whole-copy))
                                      bindings)))))
                          ;; Now destructure the sequence on the remaining
                          ;; value after all of the `pop'-ing.
                          (dolist (bind (loopy--destructure-sequence
                                         last-var `(car ,whole-copy)))
                            (push bind bindings)))

                      ;; Otherwise, we just use the last variable as the copy.
                      (push `(,last-var ,whole-var) bindings)
                      (let ((using-other-vars other-vars))
                        (while other-vars
                          (let ((i (cl-first other-vars)))
                            (setq other-vars (cl-rest other-vars))
                            (cond
                             ((sequencep i)
                              (dolist (binding (loopy--destructure-sequence
                                                i `(pop ,last-var)))
                                (push binding bindings)))
                             ((eq i '_)
                              (let ((count (loopy--count-while
                                            #'loopy--var-ignored-p var)))
                                (push `(,last-var (nthcdr ,(1+ count) ,last-var))
                                      bindings)
                                (setq other-vars (nthcdr count other-vars))))
                             (t
                              (push `(,i (pop ,last-var))
                                    bindings)))))
                        ;; Otherwise, last-var is just a copy of whole-var.
                        (when using-other-vars
                          (push `(,last-var (car ,last-var)) bindings))))))

       ;; Else, we are only using positional variables.
       (t           (seq-let (other-vars last-var)
                        (loopy--split-off-last-item var)
                      ;; If the last variable is to be ignored, we would prefer
                      ;; to just find a valid variable.
                      (when (loopy--var-ignored-p last-var)
                        (let ((reverse-good-var
                               (seq-drop-while #'loopy--var-ignored-p
                                               (reverse other-vars))))
                          (setq last-var (cl-first reverse-good-var)
                                other-vars (reverse (cl-rest reverse-good-var)))))
                      (let ((last-var-is-seq (sequencep last-var))
                            ;; This only used if last-var is seq.
                            (list-copy (gensym "list-copy-")))
                        (if last-var-is-seq
                            (progn
                              (push `(,list-copy ,value-expression)
                                    bindings)
                              (setq last-var list-copy))
                          (push `(,last-var ,value-expression)
                                bindings))
                        (while other-vars
                          (let ((i (cl-first other-vars)))
                            (setq other-vars (cl-rest other-vars))
                            (cond
                             ((sequencep i)
                              (dolist (bind (loopy--destructure-sequence
                                             i `(pop ,last-var)))
                                (push bind bindings)))
                             ((eq i '_)
                              (let ((count (loopy--count-while
                                            #'loopy--var-ignored-p var)))
                                (push `(,last-var (nthcdr ,(1+ count) ,last-var))
                                      bindings)
                                (setq other-vars (nthcdr count other-vars))))
                             (t
                              (push `(,i (pop ,last-var))
                                    bindings)))))
                        (if last-var-is-seq
                            (dolist (bind (loopy--destructure-sequence
                                           list-copy `(car ,list-copy)))
                              (push bind bindings))
                          (push `(,last-var (car ,last-var))
                                bindings)))))))

    ;; Now process the keys.
    (when key-vars
      (unless bindings
        (push `(,key-target-var ,value-expression)
              bindings))
      (let ((target-var (or rest-var
                            ;; If we used positional variables.
                            (if positional-vars key-target-var)
                            whole-var
                            key-target-var)))
        (dolist (i key-vars)
          (push (if (consp i)
                    `(,(car i)
                      ,(let ((key (intern (format ":%s" (car i)))))
                         `(if-let ((key-found (plist-member ,target-var ,key)))
                              (cl-second key-found)
                            ,(cl-second i))))
                  (let ((key (intern (format ":%s" i))))
                    `(,i (plist-get ,target-var ,key))))
                bindings))))

    ;; Fix the order of the bindings and return.
    (nreverse bindings)))

;;;;; Destructuring Generalized Variables
(defun loopy--destructure-generalized-sequence (var value-expression)
  "Destructure VALUE-EXPRESSION according to VAR as `setf'-able places.

VALUE-EXPRESSION should itself be a `setf'-able place.

Returns a list of bindings suitable for `cl-symbol-macrolet'."
  (cl-typecase var
    (symbol (unless (eq var '_)
              `((,var ,value-expression))))
    (list   (loopy--destructure-generalized-list var value-expression))
    (array  (loopy--destructure-generalized-array var value-expression))
    (t      (error "Type not recognized: %s" var))))

(defun loopy--destructure-generalized-array (var value-expression)
  "Destructure VALUE-EXPRESSION according to VAR as `setf'-able places.

VALUE-EXPRESSION should itself be a `setf'-able place.

Returns a list of bindings suitable for `cl-symbol-macrolet'.

- `&rest' references a subsequence place.
- `&whole' references the entire place."
  (let ((bindings)
        (remaining-var))
    (if (eq '&whole (aref var 0))
        (progn
          (push `(,(aref var 1) ,value-expression)
                bindings)
          (setq remaining-var (seq-drop var 2)))
      (setq remaining-var var))

    (cl-loop named loop
             with array-length = (length remaining-var)
             for v across remaining-var
             for idx from 0
             do (cond
                 ((eq v '_)) ; Do nothing if variable is `_'.
                 ((eq v '&rest)
                  (let* ((next-idx (1+ idx))
                         (next-var (aref remaining-var next-idx)))
                    ;; Check that the var after `&rest' is the last:
                    (when (> (1- array-length) next-idx)
                      (error "More than one variable after `&rest': %s"
                             var))

                    (if (sequencep next-var)
                        (dolist (binding (loopy--destructure-generalized-sequence
                                          ;; Note: `seq-subseq' doesn't have a
                                          ;; setter, but `cl-subseq' does.
                                          next-var `(cl-subseq ,value-expression
                                                               ,idx)))
                          (push binding bindings))
                      (push `(,next-var (cl-subseq ,value-expression ,idx))
                            bindings))
                    ;; Exit the loop.
                    (cl-return-from loop)))
                 (t (if (sequencep v)
                        (dolist (binding (loopy--destructure-generalized-sequence
                                          v `(aref ,value-expression ,idx)))
                          (push binding bindings))
                      (push `(,v (aref ,value-expression ,idx))
                            bindings)))))
    (nreverse bindings)))

(cl-defun loopy--destructure-generalized-list (var value-expression)
  "Destructure VALUE-EXPRESSION according to VAR as `setf'-able places.

VALUE-EXPRESSION should itself be a `setf'-able place.

returns a list of bindings suitable for `cl-symbol-macrolet'.

- `&rest' references a subsequence place.
- `&whole' references the entire place.

See `loopy--destructure-list' for normal values."
  (let ((bindings nil))                  ; The result of this function.

    (when (eq (cl-first var) '&whole)
      (cond
       ;; Make sure there is a variable named.
       ((null (cdr var))
        (error "Bad destructuring: %s" var))
       ;; If it's the only variable named, just bind it and return.
       ((null (cddr var))
        (warn "`&whole' used when only one variable listed: %s"
              var)
        (cl-return-from loopy--destructure-generalized-list
          `((,(cl-second var) ,value-expression))))
       (t
        (let ((possible-whole-var (cl-second var)))
          (if (eq possible-whole-var '_)
              (warn "`&whole' variable being ignored: %s" var)
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
           ((eq v '_)
            (setq var (cdr var))
            (cl-incf index))                  ; Do nothing in this case.

           ((eq v '&rest)
            (setq looking-at-key-vars nil)
            (when var-is-dotted
              (error "Can't use `&rest' in dotted list: %s" var))
            (let ((rest-var (cl-second var)))
              (setq rest-var-value `(nthcdr ,index ,value-expression))
              (if (sequencep rest-var)
                  (dolist (bind (loopy--destructure-generalized-sequence
                                 rest-var rest-var-value))
                    (push bind bindings))
                (push `(,rest-var ,rest-var-value)
                      bindings)))
            (setq var (cddr var))
            (cl-incf index 2))

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


(provide 'loopy-misc)
;;; loopy-misc.el ends here
