Below are some examples on how one could use `loopy`. Feel free to add you own.

-----

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [Working with Selectrum](#working-with-selectrum)
    - [A `swiper`-like command](#a-swiper-like-command)
    - [An `outline`-like command](#an-outline-like-command)

<!-- markdown-toc end -->

-----

# Working with Selectrum

[Selectrum](https://github.com/raxod502/selectrum) is a completion framework that aims to work better with Emacs's existing completion features.  Completion usually involves working with a list of candidates, and Loopy is a good tool for producing candidates. 

The below commands need the following to run:

``` emacs-lisp
(require 'loopy)
(require 'selectrum)
(defvar selectrum-should-sort-p)
(defvar selectrum-swiper-history)
(defvar selectrum-outline-history)

(declare-function selectrum-read "ext:selectrum")
(autoload 'selectrum-read "ext:selectrum")
```

Other versions of these commands can be found on the [Selectrum wiki](https://github.com/raxod502/selectrum/wiki/Useful-Commands#jumping-to-lines).

## A `swiper`-like command

```el

(defun loopy-examples-selectrum-swiper ()
  "Search for a matching line and jump to the beginning of its text.
The default candidate is the line closest to the current one.
Obeys narrowing."
  (interactive)
  (loopy (with (selectrum-should-sort-p nil)
               (current-line-number (line-number-at-pos (point) t))
               (buffer-text-lines (split-string (buffer-string) "\n"))
               (number-format
                (format "L%%0%dd: "
                        (length
                         (number-to-string (line-number-at-pos (point-max)
                                                               t))))))
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
                     ;; Note that we don't need to declare `default-candidate'.
                     (expr default-candidate
                           (cl-second formatted-candidates))))))

         (finally-do
          (let ((chosen-line-number
                 (get-text-property
                  0 'line-num
                  (selectrum-read "Jump to matching line: "
                                  (nreverse formatted-candidates)
                                  :default-candidate default-candidate
                                  :history 'selectrum-swiper-history
                                  :require-match t
                                  :no-move-default-candidate t))))
            (push-mark (point) t)
            (forward-line (- chosen-line-number current-line-number))
            (beginning-of-line-text 1)))))
```

## An `outline`-like command

```el
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
  ;; Signal a `user-error' if we don't have a regexp for this major mode.  We
  ;; could also check this in a `before-do' macro argument to `loopy', but that
  ;; would happen /after/ the `with' variables are defined, which is
  ;; inefficient.
  (if-let ((heading-regexp (alist-get major-mode selectrum-outline-formats)))
      (save-match-data
        (loopy
         (with (selectrum-should-sort-p nil)
               (current-line-number (line-number-at-pos (point) t))
               (buffer-lines (split-string (buffer-string) "\n"))
               (line-number-format
                (format "L%%0%dd: "
                        (length
                         (number-to-string (line-number-at-pos (point-max)
                                                               t))))))
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
                   (push-into backwards-prefix-list prev-heading-text))
                  ;; Otherwise, if we've moved to a lower level (higher up
                  ;; the tree), and need to remove the most recently added
                  ;; prefix from the list (i.e., go from '(c b a) back to
                  ;; '(b a)).
                  ((< heading-level (or prev-heading-level heading-level))
                   (expr backwards-prefix-list (last backwards-prefix-list
                                                     heading-level))))

                 ;; Regardless of what happens, update the previous
                 ;; heading text and level.
                 (expr prev-heading-text heading-text)
                 (expr prev-heading-level heading-level)

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
         (finally-do
          (let ((chosen-heading
                 (selectrum-read "Jump to heading: "
                                 (nreverse formatted-headings)
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
            (beginning-of-line-text 1)))))
    (user-error "Selectrum-outline: No headings defined for %s" major-mode)))
```
 