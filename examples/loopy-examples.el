;;; loopy-examples.el --- Examples for the Loopy macro -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: November 2020
;; URL: https://github.com/okamsn/loopy
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (selectrum "1.0"))

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
;; This file contains the full versions of commands described in the Loopy
;; documentation.  This file is not required to use the `loopy' macro.

;;; Code:

(require 'loopy)

(require 'selectrum)
(defvar selectrum-should-sort-p)
(defvar selectrum-swiper-history)
(defvar selectrum-outline-history)

(declare-function selectrum-read "ext:selectrum")
(autoload 'selectrum-read "ext:selectrum")


(defun loopy-examples-selectrum-swiper ()
  "Search for a matching line and jump to the beginning of its text.
The default candidate is the line closest to the current one.
Obeys narrowing."
  (interactive)
  (loopy (with (current-line-number (line-number-at-pos (point) t))
               (buffer-text-lines (split-string (buffer-string) "\n"))
               (number-format
                (format "L%%0%dd: "
                        (length (number-to-string
                                 (line-number-at-pos (point-max) t))))))

         ;; Loop through lines in the buffer.
         (list line-text buffer-text-lines)
         (expr line-num (line-number-at-pos (point-min) t) (1+ line-num))

         (unless (string-empty-p line-text)

           ;; Once the distance between candidates and the current line starts
           ;; to increase, select the previously formatted candidate (i.e., the
           ;; candidate closest to the current line) as the default.
           ;;
           ;; There are a few different ways that you could express this idea.
           (when (null default-candidate)
             (expr prev-dist +1.0e+INF dist-to-default-cand)
             (expr dist-to-default-cand (abs (- current-line-number
                                                line-num)))
             (when (> dist-to-default-cand prev-dist)
               ;; Note that we don't need to declare `default-candidate'.
               (expr default-candidate formatted-candidate)))

           ;; Format the current line.
           (expr formatted-candidate
                 (propertize
                  line-text
                  'selectrum-candidate-display-prefix
                  (propertize (format number-format line-num)
                              'face 'completions-annotations)
                  'line-num line-num))

           ;; Create a list of formatted candidates.
           (collect formatted-candidate))

         (finally-do
          (let* ((selectrum-should-sort nil)
                 (chosen-line-number
                  (get-text-property
                   0 'line-num
                   (selectrum-read "Jump to matching line: "
                                   loopy-result
                                   :default-candidate default-candidate
                                   :history 'selectrum-swiper-history
                                   :require-match t
                                   :no-move-default-candidate t))))
            (push-mark (point) t)
            (forward-line (- chosen-line-number current-line-number))
            (beginning-of-line-text 1)))))


(defcustom selectrum-outline-formats
  ;; Groups: (1) level determinant, (2) heading text.
  ;; The top level is 0, for a zero-length determinant.
  '((emacs-lisp-mode
     . "^;;;\\(?1:;*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    (lisp-mode
     . "^;;;\\(?1:;*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    (gfm-mode ; Github Flavored Markdown
     . "^#\\(?1:#*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    (markdown-mode
     . "^#\\(?1:#*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    (outline-mode
     . "^\\*\\(?1:\\**\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    ;; For Org, see also ‘org-goto’.
    (org-mode
     . "^\\*\\(?1:\\**\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    (python-mode
     . "^##\\(?1:\\**\\|#*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'"))
  "Alist of regexps used for identifying outline headings in each major mode.

The ‘car’ of an item in the list should be a symbol of the major mode.
The ‘cdr’ should be a regular expression with two required match groups:
1. Match group 1, whose length determines the outline level of that heading.
   For best formatting, the top level should be level 0 for zero length.
2. Match group 2, which is the actual heading text.

A heading is assumed to be on only one line."
  :group 'selectrum
  :type '(alist
          :key-type (symbol :tag "Major mode symbol")
          :value-type (string :tag "Regexp")))

(defun loopy-examples-selectrum-outline ()
  "Jump to a heading.  Regexps are pre-defined.  Obeys narrowing."
  (interactive)
  (let ((heading-regexp (alist-get major-mode selectrum-outline-formats)))
    ;; Signal a `user-error' if we don't have a regexp for this major mode.  We
    ;; could also check this in a `before-do' macro argument to `loopy', but
    ;; that would happen /after/ the `with' variables are defined, which is
    ;; inefficient.
    (unless heading-regexp
      (user-error "Selectrum-outline: No heading regexp for mode %s" major-mode))
    (save-match-data
      (loopy
       (with (default-heading)
             (buffer-lines (split-string (buffer-string) "\n"))
             (line-number-format
              (concat "L%0"
                      (number-to-string
                       (length (number-to-string (length buffer-lines))))
                      "d: "))
             (current-line-number (line-number-at-pos (point))))

       ;; Iterate through a list of lines.
       (list text-line buffer-lines)
       ;; Keep track of the line number of the candidate.
       (expr line-number 0 (1+ line-number))

       (when (string-match heading-regexp text-line)
         (expr prev-heading-text nil heading-text)
         (expr prev-heading-level nil heading-level)

         (expr heading-text (match-string-no-properties 2 text-line))
         (expr heading-level (length (match-string 1 text-line)))

         ;; Decide whether to update the prefix list and the previous
         ;; heading level.
         (cond
          ;; If we've moved to a greater level (further down the tree),
          ;; add the previous heading to the heading prefix list so
          ;; that we can prepend it to the current heading when
          ;; formatting.
          ((> heading-level (or prev-heading-level
                                heading-level))
           (push-into backwards-prefix-list
                      prev-heading-text))
          ((< heading-level (or prev-heading-level
                                heading-level))
           ;; Otherwise, if we've moved to a lower level (higher up the
           ;; tree), and need to remove the most recently added prefix
           ;; from the list (i.e., go from '(c b a) back to '(b a)).
           (loop (repeat (- prev-heading-level heading-level))
                 (do (pop backwards-prefix-list)))))

         ;; Finally, add to list of formatted headings.
         ;; Create heading of form "L#: a/b/c" as:
         ;; - having a text property holding the line number
         ;; - prepended with a formatted line number,
         ;;   with the face ‘completions-annotations’.
         (expr formatted-heading
               (propertize
                (concat (string-join (reverse backwards-prefix-list) "/")
                        (and backwards-prefix-list "/")
                        heading-text)
                'line-number line-number
                'selectrum-candidate-display-prefix
                (propertize
                 (format line-number-format line-number)
                 'face 'completions-annotations)))

         ;; If needed, set default candidate.
         (when (and (null default-heading)
                    (> line-number current-line-number))
           (expr default-heading formatted-heading))

         ;; Collect formatted heading into `loopy-result'.
         (collect formatted-heading))

       (finally-do
        (let* ((selectrum-should-sort nil)
               (chosen-heading
                (selectrum-read "Jump to heading: "
                                loopy-result
                                :default-candidate default-heading
                                :history 'selectrum-outline-history
                                :require-match t
                                :no-move-default-candidate t)))
          ;; Push mark, in case we want to return to current location.  This
          ;; needs to happen /after/ the user has made it clear that they
          ;; want to go somewhere.
          (push-mark (point) t)
          ;; Move to beginning of chosen line.
          (forward-line (- (get-text-property 0 'line-number chosen-heading)
                           current-line-number))
          (beginning-of-line-text 1)))))))


(provide 'loopy-examples)
;;; loopy-examples.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
