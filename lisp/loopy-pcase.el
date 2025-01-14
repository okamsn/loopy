;;; loopy-pcase.el --- pcase destructuring for `loopy' -*- lexical-binding: t; -*-

;; Copyright (c) 2021 Earl Hyatt

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
(require 'loopy-destructure)
(require 'loopy-vars)

(defun loopy-pcase--enable-flag-pcase ()
  "Make this `loopy' loop use `pcase' destructuring."
  (setq loopy--destructuring-for-iteration-function
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
            #'loopy--parse-destructuring-accumulation-command-default)))

(add-to-list 'loopy--flag-settings
             (cons 'pcase #'loopy-pcase--enable-flag-pcase))
(add-to-list 'loopy--flag-settings
             (cons '+pcase #'loopy-pcase--enable-flag-pcase))
(add-to-list 'loopy--flag-settings
             (cons '-pcase #'loopy-pcase--disable-flag-pcase))

(defalias 'loopy-pcase--destructure-for-iteration
  #'loopy--pcase-destructure-for-iteration)

(defalias 'loopy-pcase--destructure-for-with-vars
  #'loopy--pcase-destructure-for-with-vars)

(defalias 'loopy-pcase--parse-destructuring-accumulation-command
  #'loopy--pcase-parse-for-destructuring-accumulation-command)

(provide 'loopy-pcase)
;;; loopy-pcase.el ends here
