;;; loopy-pcase.el --- pcase destructuring for `loopy' -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Earl Hyatt

;; Author: Earl Hyatt
;; Created: February 2021
;; URL: https://github.com/okamsn/loopy
;; Version: 0.2
;; Package-Requires: ((emacs "25.1") (loopy "0.3"))
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
;; This package provides extra functions to use `pcase' for destructuring
;; in `loopy'.

;;; Code:
(require 'loopy)
(require 'macroexp)
(require 'pcase)
(require 'cl-lib)

(defvar loopy--basic-destructuring-function)
(defvar loopy--destructuring-accumulation-parser)
(defvar loopy--flag-settings nil)

(defun loopy-pcase--enable-flag-pcase ()
  "Make this `loopy' loop use `pcase' destructuring."
  (setq
   loopy--basic-destructuring-function
   #'loopy-pcase--destructure-variables
   loopy--destructuring-accumulation-parser
   #'loopy-pcase--parse-destructuring-accumulation-command))

(defun loopy-pcase--disable-flag-pcase ()
  "Make this `loopy' loop use `pcase' destructuring."
  (if (eq loopy--basic-destructuring-function
          #'loopy-pcase--destructure-variables)
      (setq loopy--basic-destructuring-function
            #'loopy--destructure-variables-default))
  (if (eq loopy--destructuring-accumulation-parser
          #'loopy-pcase--parse-destructuring-accumulation-command)
      (setq loopy--destructuring-accumulation-parser
            #'loopy--parse-destructuring-accumulation-command)))

(add-to-list 'loopy--flag-settings
             (cons 'pcase #'loopy-pcase--enable-flag-pcase))
(add-to-list 'loopy--flag-settings
             (cons '+pcase #'loopy-pcase--enable-flag-pcase))
(add-to-list 'loopy--flag-settings
             (cons '-pcase #'loopy-pcase--disable-flag-pcase))

(defun loopy-pcase--get-variable-values (var val)
  "Destructure VAL according to VAR using `pcase'.

Return a list of 2 sublists: (1) the needed generated variables
and (2) the variables actually named in VAR.

VAR should be a normal `pcase' destructuring pattern, such as
\"`(a . ,b)\" or \"`(1 2 3 . ,rest)\"."
  ;; Using `pcase-let*' as an interface, since it is a public function.
  ;; `pcase' knows to not assign variables if they are unused, so we pass
  ;; back in `var' (a backquoted list) so that it thinks the variables
  ;; are used.
  ;;
  ;; This will give a form like
  ;; (let* (temp-vars) (let (actual-vars) VAR))
  ;;
  ;; NOTE: Named variables might be in reverse order.  Not sure if this is
  ;; reliable behavior.
  (pcase-let* ((`(let* ,temp-vars (let ,true-vars . ,_))
                (macroexpand `(pcase-let* ((,var ,val)) ,var))))
    (list temp-vars true-vars)))

(defun loopy-pcase--destructure-variables (var val)
  "Destructure VAL according to VAR using `pcase'.

VAR should be a normal `pcase' destructuring pattern, such as
\"`(a . ,b)\" or \"`(1 2 3 . ,rest)\"."
  (apply #'append (loopy-pcase--get-variable-values var val)))

(cl-defun loopy-pcase--parse-destructuring-accumulation-command ((name var val))
  "Parse the accumulation loop command using `pcase' for destructuring.

NAME is the name of the command.  VAR-OR-VAL is a variable name
or, if using implicit variables, a value .  VAL is a value, and
should only be used if VAR-OR-VAL is a variable."
  (pcase-let ((`(,generated-vars ,named-vars)
               (loopy-pcase--get-variable-values var val)))
    (let ((instructions))
      (dolist (required-var generated-vars)
        (push `(loopy--accumulation-vars . (,(car required-var) nil))
              instructions))
      ;; NOTE: Named variables might be in reverse order.  Not sure if this is
      ;; reliable behavior.
      (dolist (named-var named-vars)
        (push `(loopy--accumulation-vars . (,(car named-var) ,(cl-case name
                                                                ((sum count)    0)
                                                                ((max maximize) -1.0e+INF)
                                                                ((min minimize) +1.0e+INF)
                                                                (t nil))))
              instructions)
        (push `(loopy--implicit-return . ,(car named-var))
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
      ;; `pcase', which should happen first in the loop body.
      (push `(loopy--main-body . (setq ,@(apply #'append generated-vars)))
            instructions))))

(provide 'loopy-pcase)
;;; loopy-pcase.el ends here
