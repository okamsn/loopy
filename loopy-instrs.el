;;; loopy-instrs.el --- Features for working with Loopy's instructions. -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Earl Hyatt

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
;; This library provides features for working with the "instructions" that
;; describe the contents of the macro expansion.  This separation exists for
;; better organization.

;;; Code:

(require 'loopy-misc)

;;;; Variable binding for instructions
;; TODO: Check not using `pcase' in github errors.

(defvar loopy--iteration-vars)
(defvar loopy--accumulation-vars)
(defvar loopy--other-vars)

(defmacro loopy--instr-let-var (place sym exp name &rest body)
  "Use SYM as EXP for BODY, creating an instruction to bind at PLACE.

Use this for values that should change during iteration.

For normal variables (that is, not needing instructions), see
also `macroexp-let2' and `cl-once-only'."
  (declare (indent 4)
           (debug (sexp sexp form sexp body)))
  (let ((bodysym (gensym "body"))
        (expsym (gensym "exp")))
    `(let* ((,expsym ,exp)
            (,sym (or ,name (gensym (symbol-name (quote ,sym)))))
            (,bodysym (progn ,@body)))
       (cons (list (quote ,place)
                   (list ,sym ,expsym))
             ,bodysym))))

(defmacro loopy--instr-let-var* (bindings place &rest body)
  "A multi-binding version of `loopy--instr-let-var'.

BINDINGS are variable-value pairs.  A third item in the list is
an expression that evaluates to a symbol to use to generate a
name to use in the binding.  PLACE is the Loopy variable to use
as the head of the instruction.  BODY are the forms for which the
binding exists."
  (declare (indent 2)
           (debug ((&rest (gate symbol form &optional form))
                   symbol
                   body)))
  (cl-reduce (cl-function (lambda (res (var val &optional name))
                            `(loopy--instr-let-var ,place ,var ,val ,name ,res)))
             (reverse bindings)
             :initial-value (macroexp-progn body)))

(defmacro loopy--instr-let-const (place sym exp name &rest body)
  "Use SYM as EXP for BODY, maybe creating an instruction to bind at PLACE.

Use for values that are evaluated only once, such as the optional
arguments to the iteration commands.  If the value of EXP is not
null and is not constant according to `macroexp-const-p', then a
binding is created.

For normal variables (that is, not needing instructions), see
also `macroexp-let2' and `cl-once-only'."
  (declare (indent 4)
           (debug (sexp sexp form sexp body)))
  (let ((bodysym (gensym "body"))
        (expsym (gensym "exp")))
    `(let* ((,expsym ,exp)
            (,sym (if (or (null ,expsym)
                          (macroexp-const-p ,expsym))
                      ,expsym
                    (or ,name
                        (gensym (symbol-name (quote ,sym))))))
            (,bodysym (progn ,@body)))
       (if (eq ,sym ,expsym)
           ,bodysym
         (cons (list (quote ,place)
                     (list ,sym ,expsym))
               ,bodysym)))))

(defmacro loopy--instr-let-const* (bindings place &rest body)
  "A multi-binding version of `loopy--instr-let-const'.

BINDINGS are variable-value pairs.  PLACE is the Loopy variable to use
as the head of the instruction.  BODY are the forms for which the
binding exists."
  (declare (indent 2)
           (debug ((&rest (gate symbol form &optional form))
                   symbol
                   body)))
  (cl-reduce (cl-function (lambda (res (var val &optional name))
                            `(loopy--instr-let-const ,place ,var ,val ,name ,res)))
             (reverse bindings)
             :initial-value (macroexp-progn body)))


(defun loopy--extract-main-body (instructions)
  "Extract main-body expressions from INSTRUCTIONS.

This returns a list of two sub-lists:

1. A list of expressions (not instructions) that are meant to be
   use in the main body of the loop.

2. A list of instructions for places other than the main body.

The lists will be in the order parsed (correct for insertion)."
  (let ((wrapped-main-body)
        (other-instructions))
    (dolist (instruction instructions)
      (if (eq (cl-first instruction) 'loopy--main-body)
          (push (cl-second instruction) wrapped-main-body)
        (push instruction other-instructions)))

    ;; Return the sub-lists.
    (list (nreverse wrapped-main-body) (nreverse other-instructions))))

;; We find ourselves doing this pattern a lot.
(cl-defmacro loopy--bind-main-body ((main-expr other-instrs) value &rest body)
  "Bind MAIN-EXPR and OTHER-INSTRS for those items in VALUE for BODY."
  (declare (indent 2))
  `(cl-destructuring-bind (,main-expr ,other-instrs)
       (loopy--extract-main-body ,value)
     ,@body))

(defun loopy--convert-iteration-vars-to-other-vars (instructions)
  "Convert instructions for `loopy--iteration-vars' to `loopy--other-vars'.

INSTRUCTIONS is a list of instructions, which don't all have to be
for `loopy--iteration-vars'."
  (loopy--substitute-using-if
   (cl-function (lambda ((_ init)) (list 'loopy--other-vars init)))
   (lambda (x) (eq (car x) 'loopy--iteration-vars))
   instructions))

(provide 'loopy-instrs)
;;; loopy-instrs.el ends here
