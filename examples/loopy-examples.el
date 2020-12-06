;;; loopy-examples.el --- Examples for the Loopy macro -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: November 2020
;; URL: https://github.com/okamsn/loopy

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
(defvar selectrum-mode)
(defvar selectrum-outline-formats)
(defvar selectrum-swiper-history)
(defvar selectrum-outline-history)

(declare-function selectrum-read "ext:selectrum")
(autoload 'selectrum-read "ext:selectrum")


(defun selectrum-swiper-loopy ()
  "Search for a matching line and jump to the beginning of its text.
The default candidate is the line closest to the current one.
Obeys narrowing."
  (interactive)
  (let (selectrum-should-sort-p
        (current-line-number (line-number-at-pos (point) t)))
    (cl-destructuring-bind (default-candidate formatted-candidates)
        (loopy
         (with (buffer-text-lines (split-string (buffer-string) "\n"))
               (number-format (concat "L%0"
                                      (number-to-string
                                       (length (number-to-string
                                                (length buffer-text-lines))))
                                      "d: ")))
         (loop (list line-text buffer-text-lines)
               (expr line-num (line-number-at-pos (point-min) t)
                              (1+ line-num))
               (unless (string-empty-p line-text)
                 (push-into formatted-candidates
                            (propertize
                             line-text
                             'selectrum-candidate-display-prefix
                             (propertize (format number-format line-num)
                                         'face 'completions-annotations)
                             'line-num line-num))
                 ;; There are a few different ways that you could express
                 ;; this.
                 (when (null default-candidate)
                   (expr prev-dist +1.0e+INF dist-to-default-cand)
                   (expr dist-to-default-cand (abs (- current-line-number
                                                      line-num)))
                   (when (> dist-to-default-cand prev-dist)
                     (expr default-candidate
                           (cl-second formatted-candidates))))))
         (finally-return default-candidate
                         (nreverse formatted-candidates)))
      (let ((chosen-line-number
             (get-text-property
              0 'line-num
              (selectrum-read "Jump to matching line: "
                              formatted-candidates
                              :default-candidate default-candidate
                              :history 'selectrum-swiper-history
                              :require-match t
                              :no-move-default-candidate t))))
        (push-mark (point) t)
        (forward-line (- chosen-line-number current-line-number))
        (beginning-of-line-text 1)))))


(defun selectrum-outline-loopy ()
  "Jump to a heading.  Regexps are pre-defined.  Obeys narrowing."
  (interactive)
  ;; Signal a `user-error' if we don't have a regexp for this major mode.
  (if-let ((heading-regexp (alist-get major-mode selectrum-outline-formats)))
      (let (selectrum-should-sort-p
            (current-line-number (line-number-at-pos (point) t)))
        (save-match-data
          (cl-multiple-value-bind (default-candidate formatted-candidates)
              (loopy
               (with (buffer-lines (split-string (buffer-string) "\n"))
                     (line-number-format
                      (concat "L%0"
                              (number-to-string
                               (length (number-to-string (length buffer-lines))))
                              "d: ")))
               (loop (expr line-number 1 (1+ line-number))
                     (list text-line buffer-lines)
                     (when (string-match heading-regexp text-line)
                       (expr heading-text
                             (match-string-no-properties 2 text-line))
                       (expr heading-level
                             (length (match-string-no-properties 1 text-line)))

                       ;; Decide whether to update the prefix list and the
                       ;; previous heading level.
                       (cond
                        ;; If we've moved to a greater level (further down the
                        ;; tree), add the previous heading to the heading prefix
                        ;; list so that we can prepend it to the current heading
                        ;; when formatting.
                        ((> heading-level (or prev-heading-level heading-level))

                         (push-into backwards-prefix-list prev-heading-text)
                         (expr prev-heading-level heading-level))
                        ;; Otherwise, if we've moved to a lower level (higher up
                        ;; the tree), and need to remove the most recently added
                        ;; prefix from the list (i.e., go from '(c b a) back to
                        ;; '(b a)).
                        ((< heading-level (or prev-heading-level heading-level))
                         (expr backwards-prefix-list (last backwards-prefix-list
                                                           heading-level))
                         (expr prev-heading-level heading-level)))

                       ;; Regardless of what happens, update the previous
                       ;; heading text.
                       (expr prev-heading-text heading-text)

                       ;; Decide whether the previous formatted heading was the
                       ;; default.
                       (when (and (null default-heading)
                                  (> line-number current-line-number))
                         (expr default-heading (car formatted-headings)))

                       ;; Finally, add to list of formatted headings.
                       ;; Create heading of form "L#: a/b/c" as:
                       ;; - having a text property holding the line number
                       ;; - prepended with a formatted line number,
                       ;;   with the face ‘completions-annotations’.
                       (push-into
                        formatted-headings
                        (propertize
                         (concat (string-join
                                  (reverse backwards-prefix-list) "/")
                                 (and backwards-prefix-list "/")
                                 heading-text)
                         'line-number line-number
                         'selectrum-candidate-display-prefix
                         (propertize (format line-number-format line-number)
                                     'face 'completions-annotations)))))
               (finally-return default-heading (nreverse formatted-headings)))
            (let ((chosen-heading
                   (selectrum-read "Jump to heading: "
                                   formatted-candidates
                                   :default-candidate default-candidate
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
              (beginning-of-line-text 1)))))
    (user-error "Selectrum-outline: No headings defined for %s" major-mode)))


(provide 'loopy-examples)
;;; loopy-examples.el ends here

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
