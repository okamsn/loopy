;;; loopy-pcase.el --- pcase destructuring for `loopy' -*- lexical-binding: t; -*-

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
;; This package provides extra functions to use `pcase' for destructuring
;; in `loopy'.

;;; Code:
(require 'loopy)
(require 'macroexp)
(require 'pcase)
(require 'cl-lib)

(defvar loopy--destructuring-accumulation-parser)
(defvar loopy--flag-settings nil)

(defun loopy-pcase--enable-flag-pcase ()
  "Make this `loopy' loop use `pcase' destructuring."
  (setq
   loopy--destructuring-for-iteration-function
   #'loopy-pcase--destructure-for-iteration
   loopy--destructuring-for-with-vars-function
   #'loopy-pcase--destructure-for-with-vars
   loopy--destructuring-accumulation-parser
   #'loopy-pcase--parse-destructuring-accumulation-command))

(defun loopy-pcase--disable-flag-pcase ()
  "Make this `loopy' loop use `pcase' destructuring."
  (if (eq loopy--destructuring-for-with-vars-function
          #'loopy-pcase--destructure-for-with-vars)
      (setq loopy--destructuring-for-with-vars-function
            #'loopy--destructure-for-with-vars-default))
  (if (eq loopy--destructuring-for-iteration-function
          #'loopy-pcase--destructure-for-iteration)
      (setq loopy--destructuring-for-iteration-function
            #'loopy--destructure-for-iteration-default))
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

(defun loopy-pcase--destructure-for-iteration (var val)
  "Destructure VAL according to VAR as by `pcase-let'.

Returns a list.  The elements are:
1. An expression which binds the variables in VAR to the values
   in VAL.
2. A list of variables which exist outside of this expression and
   need to be `let'-bound."
  (let ((var-list)
        (destructuring-expression))
    ;; This sets `destructuring-expression' and `var-list'.
    (setq destructuring-expression
          (if (fboundp 'pcase-compile-patterns)
              (pcase-compile-patterns
               val
               (list
                (cons var
                      (lambda (varvals &rest _)
                        (cons 'setq (mapcan (cl-function
                                             (lambda ((var val &rest _))
                                               (push var var-list)
                                               (list var val)))
                                            varvals))))))
            ;; NOTE: In Emacs versions less than 28, this functionality
            ;;       technically isn't public, but this is what the developers
            ;;       recommend.
            (pcase--u
             `((,(pcase--match val
                               (pcase--macroexpand
                                `(or ,var pcase--dontcare)))
                ,(lambda (vars)
                   (cons 'setq
                         (mapcan (lambda (v)
                                   (let ((destr-var (car v))
                                         ;; Use `cadr' for Emacs 28+, `cdr' for less.
                                         (destr-val (if (version< emacs-version "28")
                                                        (cdr v)
                                                      (warn "loopy-pcase: Update Emacs 28 to use `pcase-compile-patterns'.")
                                                      (cadr v))))
                                     (push destr-var var-list)
                                     (list destr-var destr-val)))
                                 vars))))))))
    (list destructuring-expression var-list)))

(defun loopy-pcase--destructure-for-with-vars (bindings)
  "Return a way to destructure BINDINGS by `pcase-let*'.

Returns a list of two elements:
1. The symbol `pcase-let*'.
2. A new list of bindings."
  (list 'pcase-let* bindings))

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
  (let* ((instructions)
         (full-main-body))
    (if (fboundp 'pcase-compile-patterns)
        (setq full-main-body
              (pcase-compile-patterns
               val
               (list
                (cons var
                      (lambda (varvals &rest _)
                        (let ((destr-main-body))
                          (dolist (varval varvals)
                            (let ((destr-var (cl-first varval))
                                  (destr-val (cl-second varval)))
                              (seq-let (main-body other-instructions)
                                  (loopy--extract-main-body
                                   (loopy--parse-accumulation-commands
                                    (list name destr-var destr-val)))
                                ;; Just push the other instructions, but
                                ;; gather the main body expressions.
                                (dolist (instr other-instructions)
                                  (push instr instructions))
                                (push main-body destr-main-body))))

                          ;; The lambda returns the destructured main body,
                          ;; which needs to be wrapped by Pcase's
                          ;; destructured bindings.
                          (macroexp-progn (apply #'append destr-main-body))))))))
      ;; NOTE: In Emacs versions less than 28, this functionality technically
      ;; isn't public, but this is what the developers recommend.
      (setq full-main-body
            (pcase--u `((,(pcase--match val
                                        (pcase--macroexpand
                                         `(or ,var pcase--dontcare)))
                         ,(lambda (vars)
                            (let ((destr-main-body))
                              (dolist (v vars)
                                (let ((destr-var (car v))
                                      ;; Use `cadr' for Emacs 28+, `cdr' for less.
                                      (destr-val (if (version< emacs-version "28")
                                                     (cdr v)
                                                   (warn "loopy-pcase: Update Emacs 28 to use `pcase-compile-patterns'.")
                                                   (cadr v))))
                                  (seq-let (main-body other-instructions)
                                      (loopy--extract-main-body
                                       (loopy--parse-accumulation-commands
                                        (list name destr-var destr-val)))
                                    ;; Just push the other instructions, but
                                    ;; gather the main body expressions.
                                    (dolist (instr other-instructions)
                                      (push instr instructions))
                                    (push main-body destr-main-body))))
                              ;; The lambda returns the destructured main body,
                              ;; which needs to be wrapped by Pcase's
                              ;; destructured bindings.
                              (macroexp-progn (apply #'append destr-main-body)))))))))
    ;; Finally, return the instructions.
    `((loopy--main-body . ,full-main-body)
      ,@(nreverse instructions))))

(provide 'loopy-pcase)
;;; loopy-pcase.el ends here
