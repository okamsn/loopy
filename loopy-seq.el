;;; loopy-seq.el --- Seq.el destructuring for `loopy' -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Earl Hyatt

;; Author: Earl Hyatt
;; Created: February 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.3
;; Package-Requires: ((emacs "25.1") (loopy "0.4"))
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
;; For more information, see Loopy’s README.org or the Info files derived from
;; that README with the command `info'.
;;
;; This package provides extra functions to use the library `seq.el'
;; (particularly `seq-let') for destructuring in `loopy'.  This is very similar
;; to the library `loopy-pcase.el'.

;;; Code:

;; NOTE: This code is basically the same as in `loopy-pcase.el', except we use
;; `seq-let' to produce values (which in turn uses `pcase-let') instead of
;; directly passing the variable list to `pcase-let'.

(require 'loopy)
(require 'seq)
(require 'pcase)
(require 'loopy-pcase)
(require 'macroexp)
(require 'cl-lib)

(defvar loopy--basic-destructuring-function)
(defvar loopy--destructuring-accumulation-parser)
(defvar loopy--flag-settings nil)

(defun loopy-seq--enable-flag-seq ()
  "Make this `loopy' loop use `seq-let' destructuring."
  (setq
   loopy--basic-destructuring-function
   #'loopy-seq--destructure-variables
   loopy--destructuring-for-iteration-function
   #'loopy-seq--destructure-for-iteration
   loopy--destructuring-accumulation-parser
   #'loopy-seq--parse-destructuring-accumulation-command))

(defun loopy-seq--disable-flag-seq ()
  "Make this `loopy' loop use `seq-let' destructuring."
  (if (eq loopy--basic-destructuring-function
          #'loopy-seq--destructure-variables)
      (setq loopy--basic-destructuring-function
            #'loopy--basic-builtin-destructuring))
  (if (eq loopy--destructuring-for-iteration-function
          #'loopy-seq--destructure-for-iteration)
      (setq loopy--destructuring-for-iteration-function
            #'loopy--destructure-for-iteration-default))
  (if (eq loopy--destructuring-accumulation-parser
          #'loopy-seq--parse-destructuring-accumulation-command)
      (setq loopy--destructuring-accumulation-parser
            #'loopy--parse-destructuring-accumulation-command)))

(add-to-list 'loopy--flag-settings
             (cons 'seq #'loopy-seq--enable-flag-seq))
(add-to-list 'loopy--flag-settings
             (cons '+seq #'loopy-seq--enable-flag-seq))
(add-to-list 'loopy--flag-settings
             (cons '-seq #'loopy-seq--disable-flag-seq))

(defun loopy-seq--get-variable-values (var val)
  "Destructure VAL according to VAR using `seq-let'.

Return a list of 2 sublists: (1) the needed generated variables
and (2) the variables actually named in VAR.

VAR should be a normal `seq' destructuring pattern, such as
\"(a . b)\" or \"`(a b c &rest rest)\"."
  ;; Using `seq-let' as an interface, since it is a public function.  This
  ;; itself uses `pcase-let'.  `pcase' knows to not assign variables if they are
  ;; unused, so we pass back in `var' (a quoted list) so that it thinks the
  ;; variables are used.
  (pcase-let* ((`(let* ,generated-vars (let ,named-vars . ,_))
                (macroexpand `(seq-let ,var ,val ,var))))
    (list generated-vars named-vars)))

(defun loopy-seq--destructure-variables (var val)
  "Destructure VAL according to VAR using `seq-let'.

VAR should be a normal `seq-let' destructuring pattern, such as
\"(a &rest b)\" or \"[_ _ _ &rest rest]\"."
  (apply #'append (loopy-seq--get-variable-values var val)))

(defun loopy-seq--get-variables (var)
  "Get the variables in sequence VAR, as a list."
  (let ((var-list))
    (seq-doseq (sym-or-seq var)
      (cond

       ((sequencep sym-or-seq)
        (dolist (symbol (loopy-seq--get-variables sym-or-seq))
          (push symbol var-list)))

       ((and (not (eq sym-or-seq '&rest))
             (not (eq sym-or-seq '_)))
        (push sym-or-seq var-list))))
    ;; Return the list of symbols in order of appearance.
    (nreverse var-list)))

(cl-defun loopy-seq--destructure-for-iteration (var val)
  "Destructure VAL according to VAR, as if by `seq-let'.

Returns a list.  The elements are:
1. An expression which binds the variables in VAR to the values
   in VAL.
2. A list of variables which exist outside of this expression and
   need to be `let'-bound."
  (loopy-pcase--destructure-for-iteration (seq--make-pcase-patterns var) val))

(cl-defun loopy-seq--parse-destructuring-accumulation-command ((name var val))
  "Destructure an accumulation loop command as if by `seq-let'.

NAME is the command name.  VAR is the variable sequence.  VAL is
the value to accumulate."
  ;; `seq-let' is really just a wrapper around `pcase-let' using a special
  ;; Pcase macro, so we can use functions from loopy-pcase.el.  The `setq'
  ;; bindings in the instruction should not be order-sensitive for accumulation
  ;; commands; the bindings should be independent.
  ;;
  ;; However, it might not parse the bindings in the right order, so while the
  ;; main-body instruction is correct, instructions for things like
  ;; implicit-return might be in the wrong order.  Therefore, we must still
  ;; produce some instructions ourselves.
  `(,@(cl-remove-if (lambda (x) (eq (car-safe x) 'loopy--implicit-return))
                    (loopy-pcase--parse-destructuring-accumulation-command
                     (list name (seq--make-pcase-patterns var) val)))
    ,@(mapcar (lambda (var) `(loopy--implicit-return . ,var))
              (loopy-seq--get-variables var))))

(provide 'loopy-seq)
;;; loopy-seq.el ends here
