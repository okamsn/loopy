;;; loopy-destructure.el --- Miscellaneous functions used with Loopy. -*- lexical-binding: t; -*-

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
;; This library provides features for destructuring for the features provided in
;; the main file.  This separation exists for better organization.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'compat)
(require 'loopy-misc)
(require 'loopy-instrs)
(require 'pcase)
(require 'stream)
(require 'seq)
(require 'subr-x)

(declare-function loopy--parse-loop-command "ext:loopy-commands" (command))

;; This better allows for things to change in the future.
(defun loopy--var-ignored-p (var)
  "Return whether VAR should be ignored for destructuring."
  (and (symbolp var)
       (eq (aref (symbol-name var) 0) ?_)))

(defconst loopy--destructure-symbols '( &seq &whole &optional &rest &body
                                        &key &keys &allow-other-keys
                                        &aux  &map)
  "Symbols affecting how following elements destructure.")

;; Having a single function for all categories allows us to have most of the
;; ordering rules in once place.
(defconst loopy--get-var-groups-cache (make-hash-table :test 'equal :size 300)
  "Cache of variable groups in a pattern.
See also the function `loopy--get-var-groups'.")

(defun loopy--get-var-groups (var-seq)
  "Return the alist of variable groups in sequence VAR-SEQ.
Type is one of `list' or `array'."
  (or (gethash var-seq loopy--get-var-groups-cache nil)
      (let* ((is-seq)
             (whole-var) (processing-whole)
             (pos-var)
             (opt-var) (processing-opts)
             (rest-var) (processing-rest) (dotted-rest-var)
             (key-var) (processing-keys) (allow-other-keys)
             (map-var) (processing-maps)
             (aux-var) (processing-auxs)
             (proper-list-p (proper-list-p var-seq))
             (type (cl-etypecase var-seq
                     (list 'list)
                     (array 'array)))
             (improper-list (and (eq type 'list)
                                 (not proper-list-p)))
             (remaining-seq (if improper-list
                                (cl-copy-list var-seq)
                              (copy-sequence var-seq))))

        (when improper-list
          (cl-shiftf dotted-rest-var
                     (cdr (last remaining-seq))
                     nil))

        (cl-flet ((missing-after (seq) (or (seq-empty-p seq)
                                           (memq (seq-elt seq 0)
                                                 loopy--destructure-symbols)))
                  (stop-processing () (setq processing-whole nil
                                            processing-opts nil
                                            processing-rest nil
                                            processing-keys nil
                                            processing-maps nil)))

          ;; Use `seq' functions to support arrays now and maybe other things later.
          (while (not (seq-empty-p remaining-seq))
            (seq-let [first &rest rest]
                remaining-seq
              (pcase first
                ;; Since `&seq' must be first, we could check for it outside of
                ;; processing, but we keep it with the other processing for
                ;; consistency.
                ('&seq             (cond
                                    ((or is-seq whole-var pos-var opt-var rest-var key-var
                                         allow-other-keys aux-var map-var)
                                     (signal 'loopy-&seq-bad-position (list var-seq)))
                                    ((seq-empty-p rest)
                                     (signal 'loopy-bad-desctructuring
                                             (list var-seq)))
                                    (t
                                     (stop-processing)
                                     (setq is-seq t))))
                ('&whole           (cond
                                    ;; Make sure there is a variable named.
                                    ((missing-after rest)
                                     (signal 'loopy-&whole-missing (list var-seq)))
                                    ;; Make sure `&whole' is before all else.
                                    ((or whole-var pos-var opt-var rest-var key-var
                                         allow-other-keys aux-var map-var)
                                     (signal 'loopy-&whole-bad-position (list var-seq)))
                                    (t
                                     (stop-processing)
                                     (setq processing-whole t))))

                ('&optional        (cond
                                    ((missing-after rest)
                                     (signal 'loopy-&optional-missing
                                             (list var-seq)))
                                    ;; Make sure `&optional' does not occur after
                                    ;; `&rest'.
                                    ((or opt-var rest-var key-var map-var aux-var)
                                     (signal 'loopy-&optional-bad-position
                                             (list var-seq)))
                                    (t
                                     (stop-processing)
                                     (setq processing-opts t))))

                ((or '&rest '&body) (cond
                                     (dotted-rest-var
                                      (signal 'loopy-&rest-dotted
                                              (list var-seq)))
                                     ((missing-after rest)
                                      (signal 'loopy-&rest-missing
                                              (list var-seq)))
                                     ((and (> (seq-length rest) 1)
                                           (let ((after-var (seq-elt rest 1)))
                                             (not (memq after-var loopy--destructure-symbols))))
                                      (signal 'loopy-&rest-multiple (list var-seq)))
                                     ;; In CL Lib, `&rest' must come before `&key',
                                     ;; but we decided to allow it to come after.
                                     ((or aux-var rest-var)
                                      (signal 'loopy-&rest-bad-position
                                              (list var-seq)))
                                     (t
                                      (stop-processing)
                                      (setq processing-rest t))))

                ((or '&key '&keys) (cond
                                    ((not (eq type 'list))
                                     (signal 'loopy-&key-array
                                             (list var-seq)))
                                    ((missing-after rest)
                                     (signal 'loopy-&key-missing
                                             (list var-seq)))
                                    ((or aux-var key-var)
                                     (signal 'loopy-&key-bad-position
                                             (list var-seq)))
                                    (t
                                     (stop-processing)
                                     (setq processing-keys t))))

                ('&allow-other-keys (cond
                                     ((not (eq type 'list))
                                      (signal 'loopy-&key-array
                                              (list var-seq)))
                                     ((not processing-keys)
                                      (signal 'loopy-&allow-other-keys-without-&key
                                              (list var-seq)))
                                     (t
                                      (stop-processing)
                                      (setq allow-other-keys t))))

                ('&map             (cond
                                    ((missing-after rest)
                                     (signal 'loopy-&map-missing (list var-seq)))
                                    ((or aux-var map-var)
                                     (signal 'loopy-&map-bad-position
                                             (list var-seq)))
                                    (t
                                     (stop-processing)
                                     (setq processing-maps t))))

                ('&aux
                 (if (or (missing-after rest)
                         aux-var)
                     (signal 'loopy-&aux-bad-position (list var-seq))
                   (stop-processing)
                   (setq processing-auxs t)))

                ('&environment
                 (signal 'loopy-bad-desctructuring (list var-seq)))

                ((guard processing-whole)
                 (cond
                  ((loopy--var-ignored-p first)
                   (signal 'loopy-&whole-missing (list var-seq)))
                  (t
                   (setq whole-var first
                         processing-whole nil))))

                ((guard processing-rest)
                 ;; `&rest' var can be ignored for clarity,
                 ;; but it is probably an error to ignore it
                 ;; when there are no positional or optional variables.
                 (if (and (loopy--var-ignored-p first)
                          (null pos-var)
                          (null opt-var))
                     (signal 'loopy-&rest-missing
                             (list var-seq))
                   (setq rest-var first
                         processing-rest nil)))

                ((guard processing-opts)
                 (if (and (consp first)
                          (cdr first)
                          (loopy--var-ignored-p (car first)))
                     (signal 'loopy-&optional-ignored-default-or-supplied
                             (list var-seq))
                   (push first opt-var)))

                ((guard processing-keys)
                 (push first key-var))

                ((guard processing-maps)
                 (push first map-var))

                ((guard processing-auxs)
                 (push first aux-var))

                (_
                 (if (or opt-var rest-var key-var map-var aux-var
                         allow-other-keys)
                     (signal 'loopy-bad-desctructuring (list var-seq))
                   (push first pos-var)))))

            (setq remaining-seq (seq-rest remaining-seq))))

        (let ((val `((whole . ,whole-var)
                     (pos . ,(nreverse pos-var))
                     (opt . ,(nreverse opt-var))
                     (rest . ,(or dotted-rest-var rest-var))
                     (key . ,(nreverse key-var))
                     (allow-other-keys . ,allow-other-keys)
                     (map . ,(nreverse map-var))
                     (aux . ,(nreverse aux-var))
                     (seq . ,is-seq))))
          (puthash var-seq val loopy--get-var-groups-cache)
          val))))

;; TODO: Turn these into records?
(defun loopy--get-&optional-spec (form)
  "Get the spec of the `&optional' variable FORM as (VAR DEFAULT SUPPLIED LEN)."
  (let ((var)
        (default)
        (supplied)
        (len))
    (loopy--pcase-let-workaround (var2 def2 sup2)
      (pcase form
        ;; Uses `nil' if not long enough.
        ((and (seq var2 def2 sup2) form2) (setq var var2
                                                default def2
                                                supplied sup2
                                                len (seq-length form2)))
        (form2 (setq var form2
                     len 0))))
    (list var default supplied len)))

(defun loopy--get-&key-spec (var-form)
  "Get the spec of `&key' VAR-FORM as (KEY VAR DEFAULT SUPPLIED)."
  (loopy--pcase-let-workaround (key var default supplied)
    (pcase-let (((or (or (seq (seq key var) default supplied)
                         (seq (seq key var) default)
                         (seq (seq key var)))
                     (and (or (seq var default supplied)
                              (seq var default)
                              (seq var)
                              (and (pred symbolp)
                                   var))
                          ;; Strip a leading underscore, since it
                          ;; only means that this argument is
                          ;; unused, but shouldn't affect the
                          ;; key's name (bug#12367).
                          (let key (if (seqp var)
                                       (signal 'loopy-&key-key-from-sequence
                                               (list var-form))
                                     (intern
                                      (format ":%s"
                                              (let ((name (symbol-name var)))
                                                (if (eq ?_ (aref name 0))
                                                    (substring name 1)
                                                  name))))))))
                 var-form))
      (unless var
        (signal 'loopy-&key-var-malformed
                (list var-form)))
      (list key var default supplied))))

(defun loopy--get-&map-spec (var-form)
  "Get the spec of `&map' VAR-FORM as (KEY VAR DEFAULT SUPPLIED)."
  (loopy--pcase-let-workaround (key var default supplied)
    (pcase-let (((or (seq key var default supplied)
                     (seq key var default)
                     (seq key var)
                     (and (or (seq var)
                              (and (pred symbolp)
                                   var))
                          (let key
                            (if (seqp var-form)
                                (signal 'loopy-&map-key-from-sequence
                                        (list var-form))
                              `(quote ,var)))))
                 var-form))
      (unless var
        (signal 'loopy-&map-var-malformed
                (list var-form)))
      (list key var default supplied))))

(defun loopy--get-&aux-spec (var-form)
  "Get the spec of `&aux' VAR-FORM as (VAR VAL)."
  (loopy--pcase-let-workaround (var val)
    (pcase-let (((or (seq var val)
                     (seq var)
                     (and (pred symbolp)
                          var))
                 var-form))
      (unless var
        (signal 'loopy-&aux-malformed-var (list var-form)))
      (list var val))))

(defun loopy--get-var-list (var-seq)
  "Get the variables in VAR-SEQ as a flat, unordered list."
  (let ((groups (loopy--get-var-groups var-seq))
        (result nil))
    (cl-labels ((fn (val) (if (seqp val)
                              (dolist (val2 (loopy--get-var-list val))
                                (cl-pushnew val2 result :test #'eq))
                            (cl-pushnew val result)))
                (opt-fn (val) (loopy--pcase-let-workaround (var supplied)
                                (seq-let [var _ supplied _]
                                    (loopy--get-&optional-spec val)
                                  (fn var)
                                  (when supplied
                                    (fn supplied)))))
                (key-fn (val) (loopy--pcase-let-workaround (var supplied)
                                (seq-let [_ var _ supplied]
                                    (loopy--get-&key-spec val)
                                  (fn var)
                                  (when supplied
                                    (fn supplied)))))
                (map-fn (val) (loopy--pcase-let-workaround (var supplied)
                                (seq-let [_ var _ supplied]
                                    (loopy--get-&map-spec val)
                                  (fn var)
                                  (when supplied
                                    (fn supplied)))))
                (aux-fn (val) (loopy--pcase-let-workaround (var)
                                (seq-let [var _]
                                    (loopy--get-&map-spec val)
                                  (fn var)))))
      (map-do (lambda (k v)
                (when v
                  (pcase k
                    ((or 'whole 'rest) (fn v))
                    ('pos (mapc #'fn v))
                    ('opt (mapc #'opt-fn v))
                    ('key (mapc #'key-fn v))
                    ('map (mapc #'map-fn v))
                    ('aux (mapc #'aux-fn v))
                    ((or 'seq 'allow-other-keys) nil)
                    (_ (error "Unknown key")))))
              groups))
    result))

;;;; Pcase pattern

(defun loopy--pcase-flip (fn arg2)
  "Wrapper macro for compatibility with obsoletion of `pcase--flip'.

FN is the function.  ARG2 is the argument to move to the second
postion of the call to FN in the pattern."
  (static-if (>= emacs-major-version 30)
      `(,fn _ ,arg2)
    `(pcase--flip ,fn ,arg2)))

(defun loopy--get-var-pattern (var)
  "Get the correct variable pattern.

If VAR is ignored according to `loopy--var-ignored-p', return
`_'.  Otherwise, if VAR is a sequence according to `seqp',
return `(loopy VAR)'.  In all other cases, VAR is returned."
  (cond
   ((loopy--var-ignored-p var) '_)
   ((seqp var) `(loopy ,var))
   (t var)))

;; TODO: Use this in `list' pattern.
(defun loopy--pcase-let-nil-list (pat)
  "Return a list of patterns binding variables in PAT to nil."
  ;; Need to quote `nil' for it to be a `pcase' pattern.
  (pcase pat
    (`(loopy ,(and (pred seqp) seq))
     (cl-loop for v in (loopy--get-var-list seq)
              collect `(let ,v 'nil)))
    (_
     `((let ,pat 'nil)))))

(defun loopy--pcase-pat-positional-list-pattern (pos-vars opt-vars rest-var map-or-key-vars)
  "Build a pattern for the positional, `&optional', and `&rest' variables.

POS-VARS is the list of the positional variables.  OPT-VARS is the list of
the optional variables.  REST-VAR is the `&rest' variable.
MAP-OR-KEY-VARS is whether there are map or key variables."
  ;; A modified version of the back-quote pattern to better work with
  ;; optional values.
  (cond
   (pos-vars `(and (pred consp)
                   (app car-safe ,(let ((var (car pos-vars)))
                                    (loopy--get-var-pattern var)))
                   (app cdr-safe ,(loopy--pcase-pat-positional-list-pattern
                                   (cdr pos-vars) opt-vars
                                   rest-var map-or-key-vars))))
   (opt-vars (loopy--pcase-let-workaround (var default supplied)
               (pcase-let* ((`(,var ,default ,supplied ,_length)
                             (loopy--get-&optional-spec (car opt-vars)))
                            (var2 (loopy--get-var-pattern var)))
                 `(and (pred listp)
                       (app car-safe (or (and (pred null)
                                              ,@(when supplied
                                                  `((let ,supplied nil)))
                                              (let ,var2 ,default))
                                         ,(if supplied
                                              `(and (let ,supplied t)
                                                    ,var2)
                                            var2)))
                       (app cdr-safe ,(loopy--pcase-pat-positional-list-pattern
                                       nil (cdr opt-vars)
                                       rest-var map-or-key-vars))))))
   (rest-var (loopy--get-var-pattern rest-var))
   ;; `pcase' allows `(,a ,b) to match (1 2 3), so we need to make
   ;; sure there aren't more values left.  However, if we are using
   ;; `&key', then we allow more values.
   (map-or-key-vars '_)
   ;; Unlike `cl-lib', we don't require that all of the positional values have a
   ;; corresponding variable/pattern.  Instead, we do like `seq' and allow the
   ;; destructuring pattern to be shorter than the sequence.
   (t '_)))

(defun loopy--pcase-pat-positional-array-pattern (pos-vars opt-vars rest-var map-or-key-vars)
  "Build a pattern for the positional, `&optional', and `&rest' variables.

POS-VARS is the list of the positional variables.  OPT-VARS is the list of
the optional variables.  REST-VAR is the `&rest' variable.
MAP-OR-KEY-VARS is whether there are map or key variables."
  (let ((pos-len (length pos-vars))
        (opt-len (length opt-vars)))
    ;; We allow the variable form to be shorter than the
    ;; destructured sequence.
    `(and (pred ,(loopy--pcase-flip (compat-function length>) (1- pos-len)))
          ,@(cl-loop for var in pos-vars
                     for idx from 0
                     collect `(app ,(loopy--pcase-flip 'aref idx)
                                   ,(loopy--get-var-pattern var)))
          ,@(when opt-vars
              (let ((opt-var-specs (seq-into (mapcar #'loopy--get-&optional-spec
                                                     opt-vars)
                                             'vector)))
                `((or ,@(cl-loop with use->= = (or rest-var map-or-key-vars)
                                 and pat-idx-low = pos-len
                                 and spec-idx-max = (1- opt-len)
                                 for checked-len from (+ pos-len opt-len) downto pos-len
                                 for spec-idx-high downfrom (1- opt-len) to 0
                                 collect
                                 ;; If the length matches, then all of the
                                 ;; remaining variables were supplied, then
                                 ;; the one variable was not supplied and we
                                 ;; need to check the remaining ones.
                                 `(and ,(if use->=
                                            `(pred ,(loopy--pcase-flip (compat-function length>) (1- checked-len)))
                                          `(pred ,(loopy--pcase-flip (compat-function length=) checked-len)))
                                       ;; Variables that should be bound with the value in
                                       ;; the array.
                                       ,@(cl-loop
                                          for spec-idx2 from 0 to spec-idx-high
                                          for arr-idx from pat-idx-low
                                          append (pcase-let* ((`(,var2 ,_ ,supplied2 ,len2)
                                                               (aref opt-var-specs spec-idx2))
                                                              (var3 (loopy--get-var-pattern var2)))
                                                   (if (= len2 3)
                                                       `((app ,(loopy--pcase-flip 'aref arr-idx)
                                                              ,var3)
                                                         (let ,supplied2 t))
                                                     `((app ,(loopy--pcase-flip 'aref arr-idx)
                                                            ,var3)))))
                                       ;; Variables that should be bound to nil or their
                                       ;; default.
                                       ,@(cl-loop
                                          for spec-idx2 from (1+ spec-idx-high) to spec-idx-max
                                          for arr-idx from pat-idx-low
                                          append (pcase-let* ((`(,var2 ,default2 ,supplied2 ,len2)
                                                               (aref opt-var-specs spec-idx2))
                                                              (var3 (loopy--get-var-pattern var2)))
                                                   (pcase-exhaustive len2
                                                     (3 `((let ,var3 ,default2)
                                                          (let ,supplied2 nil)))
                                                     (2 `((let ,var3 ,default2)))
                                                     (_
                                                      ;; (loopy--pcase-let-nil-list var3)
                                                      `((let ,var3 ,default2))
                                                      ))))))
                      ;; A pattern for when nothing matches.
                      (and ,@(cl-loop for spec across opt-var-specs
                                      append (pcase-let* ((`(,var2 ,default2 ,supplied2 ,len2) spec)
                                                          (var3 (loopy--get-var-pattern var2)))
                                               (pcase-exhaustive len2
                                                 (3
                                                  `((let ,var3 ,default2)
                                                    (let ,supplied2 nil)))
                                                 (2
                                                  `((let ,var3 ,default2)))
                                                 (_
                                                  `((let ,var3 ,default2))
                                                  ;; (loopy--pcase-let-nil-list var3)
                                                  )))))))))

          ,@(when rest-var
              (let ((len-sum (+ pos-len opt-len))
                    (rest-pat (loopy--get-var-pattern rest-var))
                    (seqsym (gensym "seqsym")))
                ;; Rec-checking the length is fast for arrays.
                `((or (and (pred ,(loopy--pcase-flip (compat-function length>) len-sum))
                           (app ,(loopy--pcase-flip 'substring len-sum) ; 0-indexed
                                ,rest-pat))
                      (app (lambda (,seqsym) (substring ,seqsym 0 0))
                           ,rest-pat))))))))

(defun loopy--seq-length= (seq n)
  "Check whether the length of SEQ is equal to N."
  (if (sequencep seq)
      (compat-call length= seq n)
    (= (seq-length seq) n)))

(defun loopy--seq-length> (seq n)
  "Check whether the length of SEQ is greater than to N."
  (cond
   ((sequencep seq)
    (compat-call length> seq n))
   ;; Take advantage of lazy evaluation of streams.
   ((streamp seq)
    (not (stream-empty-p (seq-drop seq n))))
   ((seqp seq)
    (> (seq-length seq) n))
   (t
    (error "Not a known sequence type"))))

(defun loopy--pcase-pat-positional-&seq-pattern (pos-vars opt-vars rest-var map-or-key-vars)
  "Build a pattern for the positional, `&optional', and `&rest' variables.

Unlike the build-in `seq' pattern, we don't match the sequence
if the destructuring pattern is longer than the
destructured value.

POS-VARS is the list of the positional variables.  OPT-VARS is the list of
the optional variables.  REST-VAR is the `&rest' variable.
MAP-OR-KEY-VARS is whether there are map or key variables."
  (let ((pos-len (length pos-vars))
        (opt-len (length opt-vars)))
    (cl-labels ((make-pos-pats ()
                  (cl-loop for v in pos-vars
                           for i from 0
                           collect `(app ,(loopy--pcase-flip 'seq-elt i)
                                         ,(loopy--get-var-pattern v)))))
      `(and
        ;; If there are optional values, then we can avoid the length check here
        ;; by running the length check for the optional values, which we need to
        ;; do anyway.
        ,@(when (null opt-vars)
            `((pred ,(loopy--pcase-flip 'loopy--seq-length> (1- pos-len)))
              ,@(make-pos-pats)))
        ;; Optional variables may or may not be expensive for generic
        ;; sequences.  This is the same logic as for arrays, just using the
        ;; `seq-' functions.
        ,@(when opt-vars
            (let ((opt-var-specs (seq-into (mapcar #'loopy--get-&optional-spec
                                                   opt-vars)
                                           'vector)))
              ;; TODO: When do we need this to be `=' instead of `> (1- ...)'?
              `((or ,@(cl-loop with use->= = (or rest-var map-or-key-vars)
                               and pat-idx-low = pos-len
                               and spec-idx-max = (1- opt-len)
                               for checked-len from (+ pos-len opt-len) downto pos-len
                               for spec-idx-high downfrom (1- opt-len) to 0
                               collect
                               ;; If the length matches, then all of the
                               ;; remaining variables were supplied, then
                               ;; the one variable was not supplied and we
                               ;; need to check the remaining ones.
                               `(and ,(if use->=
                                          `(pred ,(loopy--pcase-flip 'loopy--seq-length> (1- checked-len)))
                                        `(pred ,(loopy--pcase-flip 'loopy--seq-length= checked-len)))
                                     ,@(when pos-vars
                                         (make-pos-pats))
                                     ;; Variables that should be bound with the value in
                                     ;; the array.
                                     ,@(cl-loop
                                        for spec-idx2 from 0 to spec-idx-high
                                        for arr-idx from pat-idx-low
                                        append (pcase-let* ((`(,var2 ,_ ,supplied2) (aref opt-var-specs spec-idx2))
                                                            (var3 (loopy--get-var-pattern var2)))
                                                 (if supplied2
                                                     `((app ,(loopy--pcase-flip 'seq-elt arr-idx)
                                                            ,var3)
                                                       (let ,supplied2 t))
                                                   `((app ,(loopy--pcase-flip 'seq-elt arr-idx)
                                                          ,var3)))))
                                     ;; Variables that should be bound to nil or their
                                     ;; default.
                                     ,@(cl-loop
                                        for spec-idx2 from (1+ spec-idx-high) to spec-idx-max
                                        for arr-idx from pat-idx-low
                                        append (pcase-let* ((`(,var2 ,default2 ,supplied2 ,len2)
                                                             (aref opt-var-specs spec-idx2))
                                                            (var3 (loopy--get-var-pattern var2)))
                                                 (pcase-exhaustive len2
                                                   (3 `((let ,var3 ,default2)
                                                        (let ,supplied2 nil)))
                                                   (2 `((let ,var3 ,default2)))
                                                   (_
                                                    `((let ,var3 ,default2))
                                                    ;; (loopy--pcase-let-nil-list var3)
                                                    ))))))
                    ;; A pattern for when nothing matches.
                    (and ,@(cl-loop for spec across opt-var-specs
                                    append (pcase-let* ((`(,var2 ,default2 ,supplied2 ,_len2) spec)
                                                        (var3 (loopy--get-var-pattern var2)))
                                             `(,@(when supplied2
                                                   `((let ,supplied2 nil)))
                                               (let ,var3 ,default2)
                                               ;; ,@(cond
                                               ;;    ((or (eq default2 '_)
                                               ;;         (= len2 1))
                                               ;;     (loopy--pcase-let-nil-list var3))
                                               ;;    ((= len2 2)
                                               ;;     `((let ,var3 ,default2))))
                                               )))
                         ,@(when pos-vars
                             `((pred ,(loopy--pcase-flip 'loopy--seq-length> (1- pos-len)))
                               ,@(make-pos-pats))))))))
        ,@(when rest-var
            `((app ,(loopy--pcase-flip 'seq-drop (+ pos-len opt-len))
                   ,(loopy--get-var-pattern rest-var))))))))

(defun loopy--pcase-pat-&key-pattern (key-vars allow-other-keys)
  "Build a `pcase' pattern for the `&key' variables.

KEY-VARS are the forms of the key variables.  ALLOW-OTHER-KEYS is
whether `&allow-other-keys' was used.  PLIST-VAR is the variable
holding the property list."
  ;; If we aren't checking whether all keys in EXPVAL were given,
  ;; then we can use simpler patterns since we don't need to store the
  ;; value of the key.
  (cl-flet ((get-var-data (var-form)
              (loopy--pcase-let-workaround (key var default supplied)
                (pcase-let ((`(,key ,var ,default ,supplied)
                             (loopy--get-&key-spec var-form)))
                  (list key (loopy--get-var-pattern var)
                        default supplied)))))
    (if allow-other-keys
        `(and ,@(mapcar (lambda (var-form)
                          (pcase-let ((`(,key ,var ,default ,supplied) (get-var-data var-form))
                                      (key-found (gensym "key-found"))
                                      (plist (gensym "plist")))
                            (cond (supplied
                                   `(app (lambda (,plist)
                                           (let ((,key-found (plist-member ,plist ,key)))
                                             (if ,key-found
                                                 (cons t (cadr ,plist))
                                               (cons nil ,default))))
                                         (,'\` ((,'\, ,supplied) . (,'\, ,var)))))
                                  (default
                                   `(app (lambda (,plist)
                                           (let ((,key-found (plist-member ,plist ,key)))
                                             (if ,key-found
                                                 (cadr ,plist)
                                               ,default)))
                                         ,var))
                                  (t
                                   `(app ,(loopy--pcase-flip 'plist-get key)
                                         ,var)))))
                        key-vars))
      ;; If we are checking whether there are no other keys in EXPVAL,
      ;; then we use a single function for extracting the associated
      ;; values and performing the check, whose output we match against
      ;; a list of patterns.
      (let ((res (gensym "res"))
            (keys (gensym "keys"))
            (plist (gensym "plist"))
            (pats nil))
        `(app (lambda (,plist)
                (let ((,res nil)
                      (,keys nil))
                  ,@(cl-loop
                     for (key var default supplied) in (mapcar #'get-var-data key-vars)
                     collect (macroexp-let2* nil ((keyval key))
			       `(progn
				  (push ,keyval ,keys)
				  ,(cond
				    (supplied
				     (push supplied pats)
				     (push var pats)
				     (cl-once-only ((key-found `(plist-member ,plist ,keyval)))
				       `(if ,key-found
					    (progn
					      (push t ,res)
					      (push (cadr ,key-found) ,res))
					  (push nil ,res)
					  (push ,default ,res))))
				    (default
				     (push var pats)
				     (cl-once-only ((key-found `(plist-member ,plist ,keyval)))
				       `(if ,key-found
					    (push (cadr ,key-found) ,res)
					  (push ,default ,res))))
				    (t
				     (push var pats)
				     `(push (plist-get ,plist ,keyval)
					    ,res))))))
                  (push (or (plist-get ,plist :allow-other-keys)
                            (cl-loop for (key _val) on ,plist by #'cddr
                                     always (memq key ,keys)))
                        ,res)
                  ;; Reverse in case a latter pattern use a variable
                  ;; from an earlier pattern.
                  (nreverse ,res)))
              (,'\` ,(cl-loop for pat in (reverse (cons
						   ;; Use `identity' instead
						   ;; of `(not null)' to support
						   ;; older version of Emacs.
						   '(pred identity)
                                                   pats))
                              collect `(,'\, ,pat))))))))

(defun loopy--pcase-pat-&map-pattern (map-vars)
  "Build a `pcase' pattern for the `&map' variables MAP-VARS."
  (let ((mapsym (gensym "map")))
    `(and (pred mapp)
          ,@(mapcar (loopy--pcase-let-workaround (key var default supplied)
                      (lambda (var-form)
                        (pcase-let ((`(,key ,var ,default ,supplied)
                                     (loopy--get-&map-spec var-form)))
                          (unless var
                            (signal 'loopy-&map-var-malformed (list var-form)))
                          (setq var (loopy--get-var-pattern var))
                          (cond
                           (supplied
                            `(app (lambda (,mapsym)
                                    ;; The default implementations of `map-elt'
				    ;; uses `map-contains-key' (which might be
				    ;; expensive) when given a default value, so
				    ;; we use a generated default to avoid
				    ;; calling it twice.
                                    ,(let ((defsym (list 'quote (gensym "loopy--map-contains-test")))
					   (valsym (gensym "loopy--map-elt")))
				       (macroexp-let2* nil ((keysym key))
					 `(let ((,valsym (map-elt ,mapsym ,keysym ,defsym)))
					    (if (equal ,valsym ,defsym)
						(cons nil ,default)
					      (cons t ,valsym))))))
                                  (,'\` ((,'\, ,supplied) . (,'\, ,var)))))
                           (default
                            `(app (lambda (,mapsym) (map-elt ,mapsym ,key ,default))
                                  ,var))
                           (t
                            `(app ,(loopy--pcase-flip 'map-elt key) ,var))))))
                    map-vars))))

(defun loopy--pcase-pat-&aux-pattern (aux-vars)
  "Build `pcase' pattern for `&aux' variables.

AUX-VARS is the list of bindings."
  `(and ,@(cl-loop
           for bind in aux-vars
           for (var val) = (loopy--get-&aux-spec bind)
           if (null var)
           do (signal 'loopy-&aux-malformed-var (list bind))
           else
           collect `(let ,(loopy--get-var-pattern var)
                      ,val)
           end)))

;;;###autoload
(pcase-defmacro loopy (sequence)
  "Match for Loopy destructuring.

See the Info node `(loopy)Basic Destructuring'."
  (cond
   ((loopy--var-ignored-p sequence) '_)
   ((symbolp sequence) sequence)
   (t
    (let* ((groups (loopy--get-var-groups sequence))
           (is-seq (alist-get 'seq groups))
           (whole-var (alist-get 'whole groups))
           (pos-vars (alist-get 'pos groups))
           (opt-vars (alist-get 'opt groups))
           (rest-var (alist-get 'rest groups))
           (key-vars (alist-get 'key groups))
           (allow-other-keys (alist-get 'allow-other-keys groups))
           (map-vars (alist-get 'map groups))
           (aux-vars (alist-get 'aux groups)))
      (remq nil
            `(and ,(when whole-var
                     whole-var)
                  ,@(when (or pos-vars opt-vars rest-var
                              key-vars map-vars allow-other-keys)
                      (cond
                       (is-seq
                        `((pred seqp)
                          ,(when (or pos-vars opt-vars rest-var)
                             (loopy--pcase-pat-positional-&seq-pattern
                              pos-vars opt-vars
                              rest-var (or key-vars map-vars)))
                          ,(when key-vars
                             (signal 'loopy-&key-seq (list sequence)))))
                       ((listp sequence)
                        `((pred listp)
                          ,(when (or pos-vars opt-vars rest-var)
                             (loopy--pcase-pat-positional-list-pattern
                              pos-vars opt-vars
                              rest-var (or key-vars map-vars)))
                          ,(when key-vars
                             (cond
                              ((and rest-var
                                    (not (loopy--var-ignored-p rest-var)))
                               `(app (lambda (_) ,rest-var)
                                     ,(loopy--pcase-pat-&key-pattern
                                       key-vars allow-other-keys)))
                              ((or pos-vars opt-vars)
                               `(app (nthcdr ,(+ (length pos-vars)
                                                 (length opt-vars)))
                                     ,(loopy--pcase-pat-&key-pattern
                                       key-vars allow-other-keys)))
                              (t (loopy--pcase-pat-&key-pattern
                                  key-vars allow-other-keys))))))
                       ((arrayp sequence)
                        `((pred arrayp)
                          ,(when (or pos-vars opt-vars rest-var)
                             (loopy--pcase-pat-positional-array-pattern
                              pos-vars opt-vars
                              rest-var key-vars))
                          ,(when key-vars
                             (signal 'loopy-&key-array (list sequence)))))
                       (t
                        (signal 'loopy-bad-desctructuring
                                (list sequence)))))
                  ,(when map-vars
                     (cond
                      ((and rest-var
                            (not (loopy--var-ignored-p rest-var)))
                       `(app (lambda (_) ,rest-var)
                             ,(loopy--pcase-pat-&map-pattern map-vars)))
                      ((or pos-vars opt-vars)
                       `(app ,(loopy--pcase-flip 'seq-drop (+ (length pos-vars)
                                                              (length opt-vars)))
                             ,(loopy--pcase-pat-&map-pattern map-vars)))
                      (t
                       (loopy--pcase-pat-&map-pattern map-vars))))
                  ,(when aux-vars
                     (loopy--pcase-pat-&aux-pattern aux-vars))))))))

;;;; Destructuring for Iteration and Accumulation Commands

(cl-defun loopy--pcase-destructure-for-iteration (var val &key error)
  "Destructure VAL according to VAR as by `pcase-let'.

Returns a list.  The elements are:
1. An expression which binds the variables in VAR to the values
   in VAL.
2. A list of variables which exist outside of this expression and
   need to be `let'-bound.

If ERROR is non-nil, then signal an error in the produced code if
the pattern doesn't match."
  (if (symbolp var)
      `((setq ,var ,val)
        ,var)
    (let ((var-list)
          (destructuring-expression)
          (val-holder (gensym "loopy--pcase-workaround")))
      (cl-flet ((signaler (&rest _)
                  `(signal 'loopy-bad-run-time-destructuring
                           (list (quote ,var)
                                 ,val-holder))))
        ;; This sets `destructuring-expression' and `var-list'.
        (setq destructuring-expression
              ;; This holding variable seems to be needed for the older method,
              ;; before the introduction of `pcase-compile-patterns'.  In some cases,
              ;; it just evaluates `VAL' repeatedly, which is bad for functions
              ;; that work with state and bad for efficiency.
              ;;
              ;; Regardless, we also use it to report the value that caused the
              ;; error.
              `(let ((,val-holder ,val))
                 ,(if (fboundp 'pcase-compile-patterns)
                      (pcase-compile-patterns
                       val-holder
                       (remq nil
                             (list (cons var
                                         (lambda (varvals &rest _)
                                           (cons 'setq (mapcan (cl-function
                                                                (lambda ((var val &rest rest))
                                                                  (push var var-list)
                                                                  (list var val)))
                                                               varvals))))
                                   (when error
                                     (cons '_ #'signaler)))))
                    ;; NOTE: In Emacs versions less than 28, this functionality
                    ;;       technically isn't public, but this is what the developers
                    ;;       recommend.
                    (pcase--u
                     (remq
                      nil
                      (list (list (pcase--match val-holder
                                                (pcase--macroexpand
                                                 (if error
                                                     var
                                                   `(or ,var pcase--dontcare))))
                                  (lambda (vars)
                                    (cons 'setq
                                          (mapcan (lambda (v)
                                                    (let ((destr-var (car v))
                                                          ;; Use `cadr' for Emacs 28+, `cdr' for less.
                                                          (destr-val (funcall (eval-when-compile
                                                                                (if (version< emacs-version "28")
                                                                                    #'cdr
                                                                                  #'cadr))
                                                                              v)))
                                                      (push destr-var var-list)
                                                      (list destr-var destr-val)))
                                                  vars))))
                            (when error
                              (list (pcase--match val-holder
                                                  (pcase--macroexpand '_))
                                    #'signaler)))))))))
      (list destructuring-expression
            (seq-uniq var-list #'eq)))))

(defun loopy--pcase-destructure-for-with-vars (bindings)
  "Return a way to destructure BINDINGS by `pcase-let*'.

Returns a list of two elements:
1. The symbol `pcase-let*'.
2. A new list of bindings."
  (list 'pcase-let* bindings))

(cl-defun loopy--pcase-parse-for-destructuring-accumulation-command
    ((name var val &rest args) &key error)
  "Parse the accumulation loop command using `pcase' for destructuring.

NAME is the name of the command.  VAR-OR-VAL is a variable name
or, if using implicit variables, a value .  VAL is a value, and
should only be used if VAR-OR-VAL is a variable.  ERROR is when
an error should be signaled if the pattern doesn't match."
  (let* ((instructions)
         (full-main-body)
         ;; This holding variable seems to be needed for the older method,
         ;; before the introduction of `pcase-compile-patterns'.  In some cases,
         ;; it just evaluates `VAL' repeatedly, which is bad for functions
         ;; that work with state and bad for efficiency.
         (value-holder (gensym "loopy--pcase-workaround")))
    (cl-flet ((signaler (&rest _)
                `(signal 'loopy-bad-run-time-destructuring
                         (list (quote ,var)
                               ,value-holder))))
      (if (fboundp 'pcase-compile-patterns)
          (setq full-main-body
                (pcase-compile-patterns
                 value-holder
                 (remq nil
                       (list (cons var
                                   (lambda (varvals &rest _)
                                     (let ((destr-main-body))
                                       (dolist (varval varvals)
                                         (let ((destr-var (cl-first varval))
                                               (destr-val (cl-second varval)))
                                           (loopy--bind-main-body (main-body other-instructions)
                                               (loopy--parse-loop-command
                                                `(,name ,destr-var ,destr-val ,@args))
                                             ;; Just push the other instructions, but
                                             ;; gather the main body expressions.
                                             (dolist (instr other-instructions)
                                               (push instr instructions))
                                             (push main-body destr-main-body))))
                                       ;; The lambda returns the destructured main body,
                                       ;; which needs to be wrapped by Pcase's
                                       ;; destructured bindings.
                                       (macroexp-progn (apply #'append destr-main-body)))))
                             (when error
                               (cons '_ #'signaler))))))
        ;; NOTE: In Emacs versions less than 28, this functionality technically
        ;; isn't public, but this is what the developers recommend.
        (setq full-main-body
              (pcase--u
               (remq nil `((,(pcase--match value-holder
                                           (pcase--macroexpand
                                            (if error
                                                var
                                              `(or ,var pcase--dontcare))))
                            ,(lambda (vars)
                               (let ((destr-main-body))
                                 (dolist (v vars)
                                   (let ((destr-var (car v))
                                         ;; Use `cadr' for Emacs 28+, `cdr' for less.
                                         (destr-val (funcall (eval-when-compile
                                                               (if (version< emacs-version "28")
                                                                   #'cdr
                                                                 #'cadr))
                                                             v)))
                                     (seq-let (main-body other-instructions)
                                         (loopy--extract-main-body
                                          (loopy--parse-loop-command
                                           `(,name ,destr-var ,destr-val ,@args)))
                                       ;; Just push the other instructions, but
                                       ;; gather the main body expressions.
                                       (dolist (instr other-instructions)
                                         (push instr instructions))
                                       (push main-body destr-main-body))))
                                 ;; The lambda returns the destructured main body,
                                 ;; which needs to be wrapped by Pcase's
                                 ;; destructured bindings.
                                 (macroexp-progn (apply #'append destr-main-body)))))
                           ,(when error
                              (list (pcase--match value-holder (pcase--macroexpand '_))
                                    #'signaler))))))))
    ;; Finally, return the instructions.
    ;; We don't know all of the cases when the value holder is needed,
    ;; so we just always use it.
    `((loopy--main-body (let ((,value-holder ,val))
                          ,full-main-body))
      ,@(nreverse instructions))))

;;;; Destructuring Generalized Variables

(defun loopy--destructure-generalized-sequence (var value-expression)
  "Destructure VALUE-EXPRESSION according to VAR as `setf'-able places.

VALUE-EXPRESSION should itself be a `setf'-able place.

Returns a list of bindings suitable for `cl-symbol-macrolet'."
  (pcase var
    ((pred symbolp) (unless (loopy--var-ignored-p var)
                      `((,var ,value-expression))))
    ((pred (lambda (x) (map-elt (loopy--get-var-groups x) 'seq)))
     (loopy--destructure-generalized-&seq var value-expression))
    ((pred listp)   (loopy--destructure-generalized-list var value-expression))
    ((pred arrayp)  (loopy--destructure-generalized-array var value-expression))
    (_      (signal 'loopy-destructure-type (list var)))))

(define-inline loopy--destructure-seq-drop (sequence n)
  "A wrapper for `seq-drop' so that `setf' places can be recursive."
  (declare (gv-expander
            (lambda (do)
              (gv-letplace (getter setter) `(gv-delay-error ,sequence)
                (macroexp-let2* nil ((n n))
                  (funcall do
                           `(seq-drop ,getter ,n)
                           (lambda (v)
                             (macroexp-let2 nil v v
                               `(progn
                                  ,(funcall setter
                                            `(loopy--destructure-seq-replace
                                              ,getter ,v ,n))
                                  ,v)))))))))
  (inline-letevals (sequence n)
    (inline-quote (seq-drop ,sequence ,n))))

(cl-defgeneric loopy--destructure-seq-replace (sequence replacements start &optional end)
  "Replace elements of SEQUENCE from START to END with elements of REPLACEMENTS.
END is exclusive."
  (let* ((len (seq-length sequence))
         (signal-fn (lambda ()
                      (signal 'args-out-of-range
                              (if end
                                  (list sequence start end)
                                (list sequence start)))))
         (signal-or-val-fn (lambda (val)
                             (cond
                              ((> val len)
                               (funcall signal-fn))
                              ((< val 0)
                               (let ((val2 (+ val len)))
                                 (if (< val2 0)
                                     (funcall signal-fn)
                                   val2)))
                              (t
                               val))))
         (idx-start (funcall signal-or-val-fn start))
         (idx-end (if (null end)
                      len
                    (funcall signal-or-val-fn end))))
    (if (> idx-start idx-end)
        (funcall signal-fn)
      (let ((replacement-idx 0)
            (replacement-len (seq-length replacements)))
        (seq-into (seq-map-indexed (lambda (elem idx)
                                     (if (and (<= idx-start idx)
                                              (< idx idx-end)
                                              (< replacement-idx replacement-len))
                                         (prog1
                                             (seq-elt replacements replacement-idx)
                                           (setq replacement-idx (1+ replacement-idx)))
                                       elem))
                                   sequence)
                  (if (listp sequence)
                      'list
                    (type-of sequence)))))))

(cl-defmethod loopy--destructure-seq-replace (sequence (replacements list) start &optional end)
  "Replace elements of SEQUENCE from START to END with elements of REPLACEMENTS.
END is exclusive."
  (let* ((len (seq-length sequence))
         (signal-fn (lambda ()
                      (signal 'args-out-of-range
                              (if end
                                  (list sequence start end)
                                (list sequence start)))))
         (signal-or-val-fn (lambda (val)
                             (cond
                              ((> val len)
                               (funcall signal-fn))
                              ((< val 0)
                               (let ((val2 (+ val len)))
                                 (if (< val2 0)
                                     (funcall signal-fn)
                                   val2)))
                              (t
                               val))))
         (idx-start (funcall signal-or-val-fn start))
         (idx-end (if (null end)
                      len
                    (funcall signal-or-val-fn end))))
    (if (> idx-start idx-end)
        (funcall signal-fn)
      (seq-into (seq-map-indexed (lambda (elem idx)
                                   (if (and (<= idx-start idx)
                                            (< idx idx-end)
                                            replacements)
                                       (pop replacements)
                                     elem))
                                 sequence)
                (if (listp sequence)
                    'list
                  (type-of sequence))))))

(defun loopy--destructure-generalized-&seq (var-form value-expression)
  "Destructure VALUE-EXPRESSION according to VAR-FORM as `setf'-able places.

VALUE-EXPRESSION should itself be a `setf'-able place.

Returns a list of bindings suitable for `cl-symbol-macrolet'.

- `&rest' references a subsequence place.
- `&whole' references the entire place.
- `&optional' is not supported.
- `&map' references the values in the map.
- `&key' references the values in the property list."
  (map-let (('whole whole-var)
            ('pos   pos-vars)
            ('opt   opt-vars)
            ('rest  rest-var)
            ('key   key-vars)
            ('allow-other-keys allow-other-keys)
            ('map   map-vars)
            ('aux   aux-vars))
      (loopy--get-var-groups var-form)
    `(,@(when whole-var
          `((,whole-var ,value-expression)))
      ,@(when pos-vars
          (cl-loop for v in pos-vars
                   for n from 0
                   for expr = `(seq-elt ,value-expression ,n)
                   if (seqp v)
                   append (loopy--destructure-generalized-sequence
                           v expr)
                   else
                   append `((,v ,expr))))
      ,@(when opt-vars
          (signal 'loopy-&optional-generalized-variable
                  (list var-form value-expression)))
      ,@(when rest-var
	  (let ((rest-val `(loopy--destructure-seq-drop
                            ,value-expression
                            ,(+ (length pos-vars)
				(length opt-vars)))))
	    (if (seqp rest-var)
		(loopy--destructure-generalized-sequence rest-var rest-val)
	      `((,rest-var ,rest-val)))))
      ,@(when (or key-vars allow-other-keys)
          (signal 'loopy-&key-seq
                  (list var-form value-expression)))
      ,@(when map-vars
          (cl-loop
	   for elem in map-vars
           for (key var2 default supplied) = (loopy--get-&map-spec elem)
           for expr = `(map-elt
                        (loopy--destructure-seq-drop ,value-expression
					             ,(+ (length pos-vars)
					                 (length opt-vars)))
			,key ,default)
           if default
           do (signal 'loopy-generalized-default
                      (list var-form value-expression))
           else if supplied
           do  (signal 'loopy-generalized-supplied
                       (list var-form value-expression))
           else if (seqp var2)
           append (loopy--destructure-generalized-sequence
                   var2 expr)
           else
           append `((,var2 ,expr))
           end))
      ,@(when aux-vars
          (cl-loop for elem in aux-vars
                   for (var val) = (loopy--get-&aux-spec elem)
                   collect `(,var ,val))))))

(defun loopy--destructure-generalized-array (var-form value-expression)
  "Destructure VALUE-EXPRESSION according to VAR-FORM as `setf'-able places.

VALUE-EXPRESSION should itself be a `setf'-able place.

Returns a list of bindings suitable for `cl-symbol-macrolet'.

- `&rest' references a subsequence place.
- `&whole' references the entire place.
- `&optional' is not supported.
- `&map' references the values in the map.
- `&key' references the values in the property list."
  (map-let (('whole whole-var)
            ('pos   pos-vars)
            ('opt   opt-vars)
            ('rest  rest-var)
            ('key   key-vars)
            ('allow-other-keys allow-other-keys)
            ('map   map-vars)
            ('aux   aux-vars))
      (loopy--get-var-groups var-form)
    `(,@(when whole-var
          `((,whole-var ,value-expression)))
      ,@(when pos-vars
          (cl-loop for v in pos-vars
                   for n from 0
                   for expr = `(aref ,value-expression ,n)
                   if (seqp v)
                   append (loopy--destructure-generalized-sequence
                           v expr)
                   else
                   append `((,v ,expr))))
      ,@(when opt-vars
          (signal 'loopy-&optional-generalized-variable
                  (list var-form value-expression)))
      ,@(when rest-var
	  (let ((rest-val `(cl-subseq ,value-expression
                                      ,(+ (length pos-vars)
					  (length opt-vars)))))
	    (if (seqp rest-var)
		(loopy--destructure-generalized-sequence rest-var rest-val)
	      `((,rest-var ,rest-val)))))
      ,@(when (or key-vars allow-other-keys)
          (signal 'loopy-&key-array
                  (list var-form value-expression)))

      ,@(when map-vars
          (cl-loop
	   for elem in map-vars
           for (key var2 default supplied) = (loopy--get-&map-spec elem)
           for expr = `(map-elt (cl-subseq ,value-expression
					   ,(+ (length pos-vars)
					       (length opt-vars)))
				,key ,default)
           if default
           do (signal 'loopy-generalized-default
                      (list var-form value-expression))
           else if supplied
           do  (signal 'loopy-generalized-supplied
                       (list var-form value-expression))
           else if (seqp var2)
           append (loopy--destructure-generalized-sequence
                   var2 expr)
           else
           append `((,var2 ,expr))
           end))
      ,@(when aux-vars
          (cl-loop for elem in aux-vars
                   for (var val) = (loopy--get-&aux-spec elem)
                   collect `(,var ,val))))))

(cl-defun loopy--destructure-generalized-list (var-form value-expression)
  "Destructure list VALUE-EXPRESSION with generalized variables via VAR-FORM."
  (map-let (('whole whole-var)
            ('pos   pos-vars)
            ('opt   opt-vars)
            ('rest  rest-var)
            ('key   key-vars)
            ('allow-other-keys allow-other-keys)
            ('map   map-vars)
            ('aux   aux-vars))
      (loopy--get-var-groups var-form)
    `(,@(when whole-var
          `((,whole-var ,value-expression)))
      ,@(when pos-vars
          (cl-loop for v in pos-vars
                   for n from 0
                   for expr = `(nth ,n ,value-expression)
                   if (seqp v)
                   append (loopy--destructure-generalized-sequence
                           v expr)
                   else
                   append `((,v ,expr))))
      ,@(when opt-vars
          (signal 'loopy-&optional-generalized-variable
                  (list var-form value-expression)))
      ,@(when rest-var
	  (let ((rest-val `(nthcdr ,(+ (length pos-vars)
                                       (length opt-vars))
                                   ,value-expression)))
	    (if (seqp rest-var)
		(loopy--destructure-generalized-sequence rest-var rest-val)
	      `((,rest-var ,rest-val)))))

      ,@(when (or key-vars allow-other-keys)
	  (cl-loop
	   for elem in key-vars
           for (key var2 default supplied) =  (loopy--get-&key-spec elem)
           for expr = `(compat-call plist-get (nthcdr ,(+ (length pos-vars)
							  (length opt-vars))
						      ,value-expression)
				    ,key)
           if default
           do (signal 'loopy-generalized-default
                      (list var-form value-expression))
           else if supplied
           do  (signal 'loopy-generalized-supplied
                       (list var-form value-expression))
           else if (seqp var2)
           append (loopy--destructure-generalized-sequence
                   var2 expr)
           else
           append `((,var2 ,expr))
           end))

      ,@(when map-vars
          (cl-loop for elem in map-vars
                   for (key var2 default supplied) =
                   (loopy--get-&map-spec elem)
                   for expr = `(map-elt (nthcdr ,(+ (length pos-vars)
						    (length opt-vars))
						,value-expression)
					,key ,default)
                   if default
                   do (signal 'loopy-generalized-default
                              (list var-form value-expression))
                   else if supplied
                   do  (signal 'loopy-generalized-supplied
                               (list var-form value-expression))
                   else if (seqp var2)
                   append (loopy--destructure-generalized-sequence
                           var2 expr)
                   else
                   append `((,var2 ,expr))
                   end))
      ,@(when aux-vars
          (cl-loop for elem in aux-vars
                   for (var val) = (loopy--get-&aux-spec elem)
                   collect `(,var ,val))))))

(provide 'loopy-destructure)
;;; loopy-destructure.el ends here
