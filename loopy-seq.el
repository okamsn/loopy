;;; loopy-seq.el --- Seq.el destructuring for `loopy' -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: February 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (loopy "0.1"))
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

(require 'seq)
(require 'pcase)
(require 'macroexp)
(require 'cl-lib)

(defvar loopy--basic-destructuring-function)
(defvar loopy--destructuring-accumulation-parser)
(defvar loopy--flags-setup nil)

(defun loopy-seq--flag-setup ()
  "Make this `loopy' loop use `seq-let' destructuring."
  (setq
   loopy--basic-destructuring-function
   #'loopy-seq--destructure-variables
   loopy--destructuring-accumulation-parser
   #'loopy-seq--parse-destructuring-accumulation-command))

(add-to-list 'loopy--flags-setup (cons 'seq #'loopy-seq--flag-setup))

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

(cl-defun loopy-seq--parse-destructuring-accumulation-command ((name var val))
  "Parse the accumulation loop command using `seq-let' for destructuring.

NAME is the name of the command.  VAR-OR-VAL is a variable name
or, if using implicit variables, a value .  VAL is a value, and
should only be used if VAR-OR-VAL is a variable."
  (seq-let (generated-vars named-vars)
      (loopy-seq--get-variable-values var val)
    (let ((instructions))
      (dolist (required-var generated-vars)
        (push `(loopy--loop-vars . (,(car required-var) nil))
              instructions))
      (dolist (named-var named-vars)
        (push `(loopy--loop-vars . (,(car named-var) ,(cl-case name
                                                        ((sum count)    0)
                                                        ((max maximize) -1.0e+INF)
                                                        ((min minimize) +1.0e+INF)
                                                        (t nil))))
              instructions))
      ;; Push update of accumulation variables before setting required
      ;; variables to avoid needing to reverse the list of instructions.
      (push `(loopy--main-body
              . (setq ,@(mapcan
                         (pcase-lambda (`(,var ,val))
                           (cl-ecase name
                             (append `(,var (append ,var ,val)))
                             (collect `(,var (append ,var (list ,val))))
                             (concat `(,var (concat ,var ,val)))
                             (vconcat `(,var (vconcat ,var ,val)))
                             (count `(if ,val (,var (1+ ,var))))
                             ((max maximize) `(,var (max ,val ,var)))
                             ((min minimize) `(,var (min ,val ,var)))
                             (nconc `(,var (nconc ,var ,val)))
                             ((push-into push) `(push ,val ,var))
                             (sum `(,var (+ ,val ,var)))))
                         named-vars)))
            instructions)
      ;; Finally, push the setting of the generated variables required by
      ;; `seq-let', which should happen first in the loop body.
      (push `(loopy--main-body . (setq ,@(apply #'append generated-vars)))
            instructions))))

(provide 'loopy-seq)
;;; loopy-seq.el ends here
