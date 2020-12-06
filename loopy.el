;;; loopy.el --- A looping macro -*- lexical-binding: t; -*-

;; Copyright (c) 2020 Earl Hyatt

;; Author: Earl Hyatt
;; Created: November 2020
;; URL: https://github.com/okamsn/loopy
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
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


;;; Code:

(require 'cl-lib)
(require 'pcase)
(require 'seq)
(require 'subr-x)

;;;; Important Variables
;; These are only ever set locally.

(defvar loopy--with-vars nil
  "With Forms are variables explicitly created using the `with' keyword.

This is a list of ((VAR1 VAL1) (VAR2 VAL2) ...).
They are inserted into the variable declarations of a `let*' binding.
They are created by passing (with (VAR1 VAL1) (VAR2 VAL2) ...) to `loopy'.")

(defvar loopy--implicit-vars nil
  "A list of variables and their values implicitly created by loop commands.

This is a list of ((VAR1 VAL1) (VAR2 VAL2) ...).
They are inserted into the variable declarations of a `let' binding.

For example, using (list i '(1 2 3)) will create an implicit variable
containing '(1 2 3).  This makes iteration easier.")

(defvar loopy--explicit-vars nil
  "A list of variables and values explicitly named in loop commands.

This is a list of ((VAR1 VAL1) (VAR2 VAL2) ...).
They are inserted into the variable declarations of a `let' binding.

This is useful for lexically scoping variables, and for declaring
an initial value before a different value assigned in the loop.

For example, using (list i '(1 2 3)) will create an explicit variable `i'.")

(defvar loopy--explicit-generalized-vars nil
  "A list of symbols and macro expansions explicitly named in loop commands.

To create `setf'-able variables, the symbol needs to be expanded
to a form that can be treated as such.  In this case, with
`cl-symbol-macrolet'.")

(defvar loopy--before-do nil
  "A list of expressions to evaluate before the loop starts.
This is done using a `progn'.")

(defvar loopy--pre-conditions nil
  "The list of expressions that determine whether the `while' loop starts/loops.
These are fed to an `and', so all conditions must be true for the
  `while' to start/loop.")

(defvar loopy--main-body nil
  "A list of expressions to run (in order) inside the loop.
These expressions are created by parsing the loop commands passed to `loopy'.

For example, the command (list i my-list) effectively puts
\(setq i (pop my-list)) into the loop body.  Most commands require some setup,
and so don't affect only the loop body.")

(defvar loopy--latter-body nil
  "A list of expressions to run after the main loop body.
These expressions are created by parsing the loop commands passed to `loopy'.

For example, updating an indexing variable should only happen
after the variable is used.")

(defvar loopy--post-conditions nil
  "Post-conditions that could cause the loop to exit evaluating the loop body.

All expressions in the list must be true for the program to continue.
This is similar to `do-while' in other languages.")

(defvar loopy--after-do nil
  "Expressions to run (in order) after the loop successfully completes.
These run in a `progn'.")

(defvar loopy--final-do nil
  "A list of expressions always run (in order) after the loop finishes/exits.")

(defvar loopy--final-return nil
  "What the macro finally returns.  This overrides any early return value.")

;;;;; Variables for constructing the code

;; These variable affect how the code is expanded.
(defvar loopy--early-return-used nil
  "Whether a leave/return is present in the loop main body.")

(defvar loopy--skip-used nil
  "Whether a skip/continue command is present in the loop  main body.")

;;;; Errors
(define-error 'loopy-form
  "Loopy: Bad form"
  'loopy-error)

(define-error 'loopy-command-arguments
  "Loopy: Bad command arguments"
  'loopy-error)

;;;; Miscellaneous Functions
(defun loopy--bound-p (var-name)
  "Check if VAR-NAME (a symbol) is already bound for the macro.

This can happen when multiple loop commands refer to the same
variable, or when a variable is introduced via `with'.

The variable can exist in `loopy--with-vars', `loopy--explicit-vars',
or `loopy--explicit-generalized-vars'."
  (or (memq var-name (mapcar #'car loopy--with-vars))
      (memq var-name (mapcar #'car loopy--explicit-vars))
      (memq var-name (mapcar #'car loopy--explicit-generalized-vars))))

;;;; Custom Commands and Parsing
(defgroup loopy nil
  "A looping macro similar to `cl-loop'."
  :group 'extensions
  :prefix "loopy-")

;;;###autoload
(defcustom loopy-custom-command-parsers nil
  "An alist of pairs of a quoted command name and a parsing function.

The parsing function is chosen based on the command name (such as
`list' in `(list i my-list)'), not the usage of the command.  That is,

  (my-command var1)

and

  (my-command var1 var2)

are both parsed by the same function, but that parsing function
is not limited in how it responds to different usages.  If you
really want, it can return different instructions each time.
Learn more with `(info \"(emacs)loopy\")'.

For example, to add a `when' command (if one didn't already
exist), one could do

  (add-to-list \'loopy-custom-command-parsers
                (cons 'when #'my-loopy-parse-when-command))"
  :group 'loopy
  :type '(alist :key-type sexp :value-type function))

(defun loopy--get-custom-command-parser (command)
  "Get the parsing function for COMMAND from `loopy-custom-command-parsers'.
This uses the command name (such as `list' in `(list i my-list)')."
  (alist-get (car command) loopy-custom-command-parsers))

;;;; Included parsing functions.

(defun loopy--parse-conditional-forms (wrapper condition forms &optional loop-name)
  "Parse FORMS, wrapping `loopy--main-body' expressions in a conditional form.
The instructions (e.g., return expressions) are wrapped with a
WRAPPER with CONDITION.  Optionally needs LOOP-NAME for block
returns."
  (let ((full-instructions)
        (sub-instructions (loopy--parse-body-forms forms loop-name))
        (conditional-body))
    (dolist (instruction sub-instructions)
      (cl-case (car instruction)
        (loopy--main-body (push (cdr instruction) conditional-body))
        (t                (push instruction full-instructions))))
    (push `(loopy--main-body . (,wrapper ,condition ,@conditional-body))
          full-instructions)
    full-instructions))

(defun loopy--parse-cond-form (forms &optional loop-name)
  "Parse FORMS where the `car' is a condition.  Use COND forms for IF-ELSE.
Optionally needs LOOP-NAME for block returns.
Wrapped forms are things that would occur in the loop body, including returns.

This takes the `cdr' of the COND form (i.e., doesn't start with \"cond\")."
  (let ((full-instructions)
        (cond-body))
    (dolist (cond-and-body forms)
      (let ((condition (car cond-and-body))
            (sub-instructions (loopy--parse-body-forms (cdr cond-and-body)
                                                       loop-name))
            (instructions-for-wrapping))
        (dolist (instruction sub-instructions)
          (cl-case (car instruction)
            (loopy--main-body
             (push (cdr instruction) instructions-for-wrapping))
            (t (push instruction full-instructions))))
        (push (cons condition instructions-for-wrapping)
              cond-body)))
    (push `(loopy--main-body . ,(cons 'cond (nreverse cond-body))) full-instructions)
    full-instructions))

(defun loopy--parse-body-forms (forms &optional loop-name)
  "Get update and continue conditions from FORMS.
Optionally needs LOOP-NAME for block returns."
  (let ((instructions))
    (dolist (form forms instructions)
      ;; Make a quick function, since we keeping missing the second argument of
      ;; `push'.
      (cl-flet ((add-instruction (instr) (push instr instructions)))
        (pcase form
;;;;; Generic body clauses
          ;; A DO form for a generic lisp body. Not searched for special forms.
          (`(do . ,body)
           (add-instruction `(loopy--main-body . (progn ,@body))))
          ((or `(expr ,var . ,rest) `(exprs ,var . ,rest)
               `(set ,var . ,rest))
           (let ((arg-length (length rest))
                 (counter-holder (gensym)))
             (add-instruction `(loopy--explicit-vars . (,var nil)))
             (cond
              ((= arg-length 0)
               (add-instruction `(loopy--main-body . (setq ,var nil))))
              ((= arg-length 1)
               (add-instruction `(loopy--main-body . (setq ,var ,(car rest)))))
              ((= arg-length 2)
               (add-instruction `(loopy--implicit-vars . (,counter-holder t)))
               (add-instruction `(loopy--main-body
                                  . (setq ,var (if ,counter-holder
                                                   ,(cl-first rest)
                                                 ,(cl-second rest)))))
               (add-instruction `(loopy--latter-body . (setq ,counter-holder nil))))
              (t
               (add-instruction `(loopy--implicit-vars . (,counter-holder 0)))
               (add-instruction `(loopy--latter-body
                                  . (when (< ,counter-holder (1- ,arg-length))
                                      (setq ,counter-holder (1+ ,counter-holder)))))
               ;; Assign to var based on the value of counter-holder.  For
               ;; efficiency, we want to check for the last expression first,
               ;; since it will probably be true the most times.  To enable
               ;; that, the condition is whether the counter is greater than
               ;; the index of EXPR in REST minus one.
               ;;
               ;; E.g., for '(a b c),
               ;; use '(cond ((> cnt 1) c) ((> cnt 0) b) ((> cnt -1) a))
               (add-instruction
                `(loopy--main-body
                  . (setq ,var ,(let ((body-code nil) (index 0))
                                  (dolist (expr rest)
                                    (push `((> ,counter-holder ,(1- index))
                                            ,expr)
                                          body-code)
                                    (setq index (1+ index)))
                                  (cons 'cond body-code)))))))))

;;;;; Iteration Clauses
          ;; TODO:
          ;; - obarrays?
          ;; - key-codes/key-bindings and key-seqs?
          ;; - overlays?
          ;; - intervals of constant text properties?
          (`(array ,var ,val)
           (let ((val-holder (gensym))
                 (index-holder (gensym)))
             (add-instruction `(loopy--implicit-vars . (,val-holder ,val)))
             (add-instruction `(loopy--implicit-vars . (,index-holder 0)))
             (add-instruction `(loopy--explicit-vars . (,var nil)))
             (add-instruction `(loopy--main-body . (setq ,var
                                                         (aref ,val-holder
                                                               ,index-holder))))
             (add-instruction `(loopy--latter-body . (setq ,index-holder
                                                           (1+ ,index-holder))))
             (add-instruction `(loopy--pre-conditions . (< ,index-holder
                                                           (length ,val-holder))))))

          ((or `(array-ref ,var ,val) `(arrayf ,var ,val))
           (let ((val-holder (gensym))
                 (index-holder (gensym)))
             (add-instruction `(loopy--implicit-vars . (,val-holder ,val)))
             (add-instruction `(loopy--implicit-vars . (,index-holder 0)))
             (add-instruction `(loopy--explicit-generalized-vars
                                . (,var (aref ,val-holder ,index-holder))))
             (add-instruction `(loopy--latter-body
                                . (setq ,index-holder (1+ ,index-holder))))
             (add-instruction `(loopy--pre-conditions
                                . (< ,index-holder (length ,val-holder))))))

          ((or `(cons ,var ,val . ,func) `(conses ,var ,val . ,func))
           (let ((actual-func (cond
                               ((null func)
                                'cdr)
                               ((and (consp (car func))
                                     (memq (caar func) '(quote function)))
                                (eval (car func)))
                               (t (car func)))))
             (add-instruction `(loopy--explicit-vars . (,var ,val)))
             (add-instruction `(loopy--latter-body
                                . (setq ,var (,actual-func ,var))))
             (add-instruction `(loopy--pre-conditions . (consp ,var)))))

          (`(list ,var ,val . ,func)
           (let ((val-holder (gensym))
                 ;; The function argument may or may not be quoted. We need it
                 ;; to be unquoted for the syntax to work.
                 ;; E.g., "(#'cdr val-holder)" won't work.
                 (actual-func (cond
                               ((null func)
                                'cdr)
                               ((and (consp (car func))
                                     (memq (caar func) '(quote function)))
                                (eval (car func)))
                               (t (car func)))))
             (add-instruction `(loopy--implicit-vars . (,val-holder ,val)))
             (add-instruction `(loopy--explicit-vars . (,var nil)))
             (add-instruction `(loopy--main-body
                                . (setq ,var (car ,val-holder))))
             (add-instruction `(loopy--latter-body
                                . (setq ,val-holder (,actual-func ,val-holder))))
             (add-instruction `(loopy--pre-conditions . (consp ,val-holder)))))

          ((or `(list-ref ,var ,list . ,func) `(listf ,var ,list . ,func))
           (let ((val-holder (gensym))
                 (actual-func (cond
                               ((null func)
                                'cdr)
                               ((and (consp (car func))
                                     (memq (caar func) '(quote function)))
                                (eval (car func)))
                               (t (car func)))))
             (add-instruction `(loopy--implicit-vars . (,val-holder ,list)))
             (add-instruction `(loopy--explicit-generalized-vars
                                . (,var (car ,val-holder))))
             (add-instruction `(loopy--latter-body
                                . (setq ,val-holder (,actual-func ,val-holder))))
             (add-instruction `(loopy--pre-conditions . (consp ,val-holder)))))

          (`(repeat ,count)
           (let ((val-holder (gensym)))
             (add-instruction `(loopy--implicit-vars . (,val-holder 0)))
             (add-instruction `(loopy--latter-body . (cl-incf ,val-holder)))
             (add-instruction `(loopy--pre-conditions . (< ,val-holder ,count)))))

          (`(repeat ,var ,count)
           (add-instruction `(loopy--implicit-vars . (,var 0)))
           (add-instruction `(loopy--latter-body . (cl-incf ,var)))
           (add-instruction `(loopy--pre-conditions . (< ,var ,count))))

          (`(seq ,var ,val)
           ;; Note: `cl-loop' just combines the logic for lists and arrays, and
           ;;       just checks the type for each iteration, so we do that too.
           (let ((val-holder (gensym))
                 (index-holder (gensym)))
             (add-instruction `(loopy--implicit-vars . (,val-holder ,val)))
             (add-instruction `(loopy--implicit-vars . (,index-holder 0)))
             (add-instruction `(loopy--explicit-vars . (,var nil)))
             (add-instruction
              `(loopy--main-body . (setq ,var (if (consp ,val-holder)
                                                  (pop ,val-holder)
                                                (aref ,val-holder ,index-holder))
                                         ,index-holder (1+ ,index-holder))))
             (add-instruction `(loopy--pre-conditions
                                . (and ,val-holder
                                       (or (consp ,val-holder)
                                           (< ,index-holder
                                              (length ,val-holder))))))))

          ((or `(seq-ref ,var ,val) `(seqf ,var ,val))
           (let ((val-holder (gensym))
                 (index-holder (gensym)))
             (add-instruction `(loopy--implicit-vars . (,val-holder ,val)))
             (add-instruction `(loopy--implicit-vars . (,index-holder 0)))
             (add-instruction `(loopy--explicit-generalized-vars
                                . (,var (elt ,val-holder ,index-holder))))
             (add-instruction `(loopy--latter-body
                                . (setq ,index-holder (1+ ,index-holder))))
             ;; TODO: Length of sequence not changing, so don't have to
             ;;       recompute each time.
             (add-instruction `(loopy--pre-conditions
                                . (< ,index-holder (length ,val-holder))))))

;;;;; Conditional Body Forms
          ;; Since these can contain other commands/clauses, it's easier if they
          ;; have their own parsing functions, which call back into this one to
          ;; parse sub-clauses.
          (`(when ,cond . ,body)
           (mapc #'add-instruction
                 (loopy--parse-conditional-forms 'when cond body loop-name)))

          (`(unless ,cond . ,body)
           (mapc #'add-instruction
                 (loopy--parse-conditional-forms 'unless cond body loop-name)))

          (`(if ,cond . ,body)
           (mapc #'add-instruction
                 (loopy--parse-conditional-forms 'if cond body loop-name)))

          (`(cond . ,body)
           (mapc #'add-instruction
                 (loopy--parse-cond-form body loop-name)))

;;;;; Exit and Return Clauses
          ((or '(skip) '(continue))
           (add-instruction '(loopy--main-body . (go loopy--continue-tag))))
          (`(return ,val)
           (add-instruction `(loopy--main-body . (cl-return-from ,loop-name ,val))))
          (`(return-from ,name ,val)
           (add-instruction `(loopy--main-body . (cl-return-from ,name ,val))))
          ((or '(leave) '(break))
           (add-instruction `(loopy--main-body . (cl-return-from ,loop-name nil))))
          ((or `(leave-from ,name) `(break-from ,name))
           (add-instruction `(loopy--main-body . (cl-return-from ,name nil))))

;;;;; Accumulation Clauses
          (`(append ,var ,val)
           (add-instruction `(loopy--explicit-vars . (,var nil)))
           (add-instruction `(loopy--main-body . (setq ,var (append ,var ,val)))))
          (`(collect ,var ,val)
           (add-instruction `(loopy--explicit-vars . (,var nil)))
           (add-instruction `(loopy--main-body . (setq ,var (append ,var
                                                                    (list ,val))))))
          (`(concat ,var ,val)
           (add-instruction `(loopy--explicit-vars . (,var nil)))
           (add-instruction `(loopy--main-body . (setq ,var (concat ,var ,val)))))
          (`(vconcat ,var ,val)
           (add-instruction `(loopy--explicit-vars . (,var nil)))
           (add-instruction `(loopy--main-body . (setq ,var (vconcat ,var ,val)))))
          (`(count ,var ,val)
           (add-instruction `(loopy--explicit-vars . (,var 0)))
           (add-instruction `(loopy--main-body . (when ,val (cl-incf ,var)))))
          ((or `(max ,var ,val) `(maximize ,var ,val))
           (add-instruction `(loopy--explicit-vars . (,var -1.0e+INF)))
           (add-instruction `(loopy--main-body . (setq ,var (max ,var ,val)))))
          ((or `(min ,var ,val) `(minimize ,var ,val))
           (add-instruction `(loopy--explicit-vars . (,var 1.0e+INF)))
           (add-instruction `(loopy--main-body . (setq ,var (min ,var ,val)))))
          (`(nconc ,var ,val)
           (add-instruction `(loopy--explicit-vars . (,var nil)))
           (add-instruction `(loopy--main-body . (setq ,var (nconc ,var ,val)))))
          ((or `(push-into ,var ,val) `(push ,var ,val))
           (add-instruction `(loopy--explicit-vars . (,var nil)))
           (add-instruction `(loopy--main-body . (push ,val ,var))))
          (`(sum ,var ,val)
           (add-instruction `(loopy--explicit-vars . (,var 0)))
           (add-instruction `(loopy--main-body . (setq ,var (+ ,var ,val)))))

;;;;; Custom commands
          (_
           (if-let ((command-parser (loopy--get-custom-command-parser form)))
               (if-let ((custom-instructions (funcall command-parser form)))
                   (mapc #'add-instruction custom-instructions)
                 (error "Loopy: No instructions returned by command parser: %s"
                        command-parser))
             (error "Loopy: This form unkown: %s" form))))))))

;;;; The Macro Itself
;;;###autoload
(cl-defmacro loopy (&rest body)
  "A looping macro.

There are several possible arguments that make up BODY:
- a name for the loop, unquoted
- variables to declare before the loop, as in
  (with (VAR1 VAL1) [(VAR2 VAL2) ...])
- code to run before the loop, as in (before-do FORM1 [FORM2 ...])
- special commands that define the loop, as in (loop COMMAND1 [COMMAND2 ...])
- code to run if the loop completes, as in (after-do FORM1 [FORM2 ...])
- code to always run after the loop, as in (finally-do FORM1 [FORM2 ...])
- a value to always return, as in (finally-return FORM1 [FORM2 ...])

Returns are always explicit.  See this package's README for more information."
  (declare (debug (&optional ;; TODO: Is this correct?
                   ([&or "with" "let*"] &rest (symbolp &optional form))
                   ([&or "before-do" "before-progn" "before"] body)
                   ([&optional "loop"]
                    &rest [&or (symbolp ;; This one covers most commands.
                                &optional
                                symbolp
                                form
                                [&or symbolp function-form lambda-expr])
                               ([&or "when" "if" "unless"] form body)
                               ("cond" &rest (body))])
                   ([&or "after-do" "after-progn" "after"] body)
                   ([&or "finally-do" "finally-progn"] body)
                   ([&or "finally-return" "return"] form &optional [&rest form]))))
  (let (;; -- Top-level expressions other than loop body --
        (loopy--name-arg)
        (loopy--with-vars)
        (loopy--before-do)
        (loopy--after-do)
        (loopy--final-do)
        (loopy--final-return)

        ;; -- Vars for processing loop clauses --
        (loopy--implicit-vars)
        (loopy--explicit-vars)
        (loopy--explicit-generalized-vars)
        (loopy--pre-conditions)
        (loopy--main-body)
        (loopy--latter-body)
        (loopy--post-conditions)

        ;; -- Variables for constructing code --
        (loopy--early-return-used)
        (loopy--skip-used)

        ;; The expanded code.
        (result))

;;;;; Interpreting the macro arguments.
    ;; Check what was passed to the macro.
    (dolist (arg body)
      (cond
       ((symbolp arg)
        (setq loopy--name-arg arg))
       ((memq (car-safe arg) '(with let*))
        (setq loopy--with-vars (cdr arg)))
       ((memq (car-safe arg) '(before-do before))
        (setq loopy--before-do (cdr arg)))
       ((memq (car-safe arg) '(after-do after else else-do))
        (setq loopy--after-do (cdr arg)))
       ((memq (car-safe arg) '(finally-do finally))
        (setq loopy--final-do (cdr arg)))
       ((memq (car-safe arg) '(finally-return return))
        (setq loopy--final-return
              (if (= 1 (length (cdr arg)))
                  (cadr arg)
                (cons 'list (cdr arg)))))
       (t
        ;; Body forms have the most variety.
        ;; An instruction is (PLACE-TO-ADD . THING-TO-ADD).
        ;; Things added are expanded in place.
        (dolist (instruction (loopy--parse-body-forms (if (eq (car-safe arg) 'loop)
                                                          (cdr arg)
                                                        arg)
                                                      loopy--name-arg))
          ;; Do it this way instead of with `set', cause was getting errors
          ;; about void variables.
          (cl-case (car instruction)
            (loopy--explicit-generalized-vars
             (push (cdr instruction) loopy--explicit-generalized-vars))
            (loopy--implicit-vars
             ;; Don't wont to accidentally rebind variables to `nil'.
             (unless (loopy--bound-p (cadr instruction))
               (push (cdr instruction) loopy--implicit-vars)))
            (loopy--explicit-vars
             (unless (loopy--bound-p (cadr instruction))
               (push (cdr instruction) loopy--explicit-vars)))
            (loopy--pre-conditions
             (push (cdr instruction) loopy--pre-conditions))
            (loopy--main-body
             (push (cdr instruction) loopy--main-body))
            (loopy--latter-body
             (push (cdr instruction) loopy--latter-body))
            (loopy--post-conditions
             (push (cdr instruction) loopy--post-conditions))

            ;; Places users probably shouldn't push to, but can if they want:
            (loopy--with-vars
             (push (cdr instruction) loopy--with-vars))
            (loopy--before-do
             (push (cdr instruction) loopy--before-do))
            (loopy--after-do
             (push (cdr instruction) loopy--after-do))
            (loopy--final-do
             (push (cdr instruction) loopy--final-do))
            (loopy--final-return
             (push (cdr instruction) loopy--final-return))
            (t
             (error "Loopy: Unknown body instruction: %s" instruction)))))))

    ;; Post-conditions can just go at end of latter-body.
    (when loopy--post-conditions
      (setq loopy--latter-body
            (append loopy--latter-body
                    `((unless ,(cl-case (length loopy--post-conditions)
                                 (0 t)
                                 (1 (car loopy--post-conditions))
                                 (t (cons 'and loopy--post-conditions)))
                        (cl-return-from ,loopy--name-arg nil))))))

;;;;; Constructing/Creating the returned code.
    ;; Note: `let'/`let*' will signal an error if we accidentally substitute
    ;;       `nil' as the variable declaration, since it will assume we are
    ;;       trying to redefine a constant.  To avoid that, we just bind `_' to
    ;;       `nil', which is used (at least in `pcase') as a throw-away symbol.
    `(cl-symbol-macrolet (,@(or loopy--explicit-generalized-vars
                                (list (list (gensym) nil))))
       (let* (,@(or loopy--with-vars '((_))))
         (let (,@(or (append loopy--implicit-vars loopy--explicit-vars)
                     '((_))))
           ;; If we need to, capture early return, those that has less
           ;; priority than a final return.
           (let ((loopy--early-return-capture
                  (cl-block ,loopy--name-arg
                    ,@loopy--before-do
                    (while ,(cl-case (length loopy--pre-conditions)
                              (0 t)
                              (1 (car loopy--pre-conditions))
                              (t (cons 'and loopy--pre-conditions)))
                      (cl-tagbody
                       ;; Note: `push'-ing things into the instruction list in
                       ;;       `loopy--parse-body-forms' and then reading them
                       ;;       back and then pushing into `loopy--main-body'
                       ;;       counters out the flipped order normally caused by
                       ;;       `push'.
                       ,@loopy--main-body
                       loopy--continue-tag
                       ,@loopy--latter-body))
                    ,@loopy--after-do
                    ;; We don't want anything in `loopy--after-do' accidentally
                    ;; giving us a return value, so we explicitly return nil.
                    ;;
                    ;; TODO: Is this actually needed?
                    nil)))
             ,@loopy--final-do
             ,(if loopy--final-return
                  loopy--final-return
                'loopy--early-return-capture)))))))

(provide 'loopy)
;;; loopy.el ends here
