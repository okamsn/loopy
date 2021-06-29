;;; loopy-seq.el --- Seq.el destructuring for `loopy' -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Earl Hyatt

;; Author: Earl Hyatt
;; Created: February 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.7.1
;; Package-Requires: ((emacs "25.1") (loopy "0.7.1"))
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

(defvar loopy--destructuring-for-with-vars-function)
(defvar loopy--destructuring-accumulation-parser)
(defvar loopy--flag-settings nil)

(defun loopy-seq--enable-flag-seq ()
  "Make this `loopy' loop use `seq-let' destructuring."
  (setq
   loopy--destructuring-for-iteration-function
   #'loopy-seq--destructure-for-iteration
   loopy--destructuring-for-with-vars-function
   #'loopy-seq--destructure-for-with-vars
   loopy--destructuring-accumulation-parser
   #'loopy-seq--parse-destructuring-accumulation-command))

(defun loopy-seq--disable-flag-seq ()
  "Make this `loopy' loop use `seq-let' destructuring."
  (if (eq loopy--destructuring-for-iteration-function
          #'loopy-seq--destructure-for-iteration)
      (setq loopy--destructuring-for-iteration-function
            #'loopy--destructure-for-iteration-default))
  (if (eq loopy--destructuring-for-with-vars-function
          #'loopy-seq--destructure-for-with-vars)
      (setq loopy--destructuring-for-with-vars-function
            #'loopy--destructure-for-with-vars-default))
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

(defun loopy-seq--destructure-for-with-vars (bindings)
  "Return a way to destructure BINDINGS as if by a `seq-let*'.

Returns a list of two elements:
1. The symbol `loopy-seq--seq-let*'.
2. A new list of bindings."
  (list 'loopy-seq--seq-let* bindings))

(defmacro loopy-seq--seq-let* (bindings &rest body)
  "Bind variables in BINDINGS according via `seq-let' and `let'.

BODY is the normal list of expressions around which to bind the
variables."
  (let ((result body)
        (result-is-one-expression (cdr-safe body)))
    (cl-flet ((get-result () (if result-is-one-expression
                                 (list result)
                               result)))
      (dolist (binding (reverse bindings))
        (let ((var   (cl-first binding))
              (value (cl-second binding)))
          (setq result
                `(,@(if (sequencep var)
                        (list 'seq-let var value)
                      (list 'let `(,binding)))
                  ,@(get-result))
                result-is-one-expression t))))
    result))

(cl-defun loopy-seq--destructure-for-iteration (var val)
  "Destructure VAL according to VAR, as if by `seq-let'.

Returns a list.  The elements are:
1. An expression which binds the variables in VAR to the values
   in VAL.
2. A list of variables which exist outside of this expression and
   need to be `let'-bound."
  (loopy-pcase--destructure-for-iteration (seq--make-pcase-patterns var) val))

(cl-defun loopy-seq--parse-destructuring-accumulation-command
    ((name var val &rest args))
  "Destructure an accumulation loop command as if by `seq-let'.

NAME is the command name.  VAR is the variable sequence.  VAL is
the value to accumulate."
  ;; `seq-let' is really just a wrapper around `pcase-let' using a special
  ;; Pcase macro, so we can use functions from loopy-pcase.el.  The `setq'
  ;; bindings in the instruction should not be order-sensitive for accumulation
  ;; commands; the bindings should be independent.
  (loopy-pcase--parse-destructuring-accumulation-command
   `(,name ,(seq--make-pcase-patterns var) ,val ,@args)))

(provide 'loopy-seq)
;;; loopy-seq.el ends here
