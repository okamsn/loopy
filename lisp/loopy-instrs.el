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

(cl-defmacro loopy--bind-main-body ((main-exprs other-instrs) value &rest body)
  "Bind MAIN-EXPRS and OTHER-INSTRS for those items in VALUE for BODY.

MAIN-EXPR is a list of main-body expressions (not instructions).
OTHER-INSTRS is a list of the remaining instructions."
  (declare (indent 2))
  (let ((main-temp (gensym "main-temp"))
        (other-temp (gensym "other-temp"))
        (instruction (gensym "instr")))
    `(let ((,main-temp nil)
           (,other-temp nil))
       (dolist (,instruction ,value)
         (if (eq (cl-first ,instruction) 'loopy--main-body)
             (push (cl-second ,instruction) ,main-temp)
           (push ,instruction ,other-temp)))
       (let ((,main-exprs (nreverse ,main-temp))
             (,other-instrs (nreverse ,other-temp)))
         ,@body))))

(provide 'loopy-instrs)
;;; loopy-instrs.el ends here
