
# Table of Contents

1.  [Introduction](#orgefa2eaf)
2.  [How to use](#how-to-use)
    1.  [Macro Arguments](#macro-arguments)
    2.  [Loop Commands](#loop-commands)
        1.  [Commands for Generic Evaluation](#commands-for-generic-evaluation)
        2.  [Iteration and Looping Commands](#iteration-and-looping-commands)
        3.  [Accumulation Commands](#accumulation-commands)
        4.  [Control Flow](#control-flow)
            1.  [Conditionals](#conditionals)
            2.  [Skipping an Iteration](#skipping-an-iteration)
            3.  [Exiting the Loop Early](#exiting-the-loop-early)
3.  [Adding Custom Commands](#adding-custom-commands)
    1.  [Background Information](#background-information)
    2.  [A Small Example](#a-small-example)
    3.  [A Slightly More Complicated Example](#a-slightly-more-complicated-example)
4.  [How does it compare to `cl-loop`?](#how-does-it-compare-to-other-approaches)
    1.  [Translating from `cl-loop`](#translating-from-cl-loop)
        1.  [For Clauses](#for-clauses)
        2.  [Iteration Clauses](#iteration-clauses)
        3.  [Accumulation Clauses](#accumulation-clauses)
        4.  [Other Clauses](#other-clauses)
5.  [Real-World Examples](#orga915407)

`loopy` is a macro meant for iterating and looping.  It is similar in usage to
`cl-loop` <sup><a id="fnr.1" class="footref" href="#fn.1">1</a></sup> but uses symbolic expressions rather than keywords.

`loopy` should be complementary to the features provided by the Seq <sup><a id="fnr.2" class="footref" href="#fn.2">2</a></sup> and
CL <sup><a id="fnr.3" class="footref" href="#fn.3">3</a></sup> libraries (including `cl-loop`) and Emacs's regular looping and
mapping features.


<a id="orgefa2eaf"></a>

# Introduction

The `loopy` macro has several possible arguments, each beginning with a
keyword.

-   `with` declares variables that are bound in order before and around the
    loop, like in a `let*` binding.
-   `before-do` is a list of normal Lisp expressions to run before the loop
    executes.
-   `loop` is a list of special commands that create the loop body.  These
    commands are described in detail in the section [How to Use](#how-to-use).
-   `after-do` is a list of normal Lisp expressions to run after the successful
    completion of the loop.
-   `finally-do` is a list of normal Lisp expressions that always run,
    regardless of whether an early return was triggered in the loop body.
-   `finally-return` is an expression whose value is always returned, regardless
    of whether an early return was triggered in the loop body.

The loop commands generally follow the form `(COMMAND VARIABLE-NAME &rest ARGS)`.
For example,

-   To iterate through a sequence<sup><a id="fnr.4" class="footref" href="#fn.4">4</a></sup>, use `(seq elem [1 2 3])` (for
    efficiency, there are also more specific commands, like `list`).
-   To collect values into a list, use `(collect my-collection collected-value)`.
-   To just bind a variable to the result of a Lisp expression, use
    `(expr my-var (my-func))`

Below is a full example of the arguments of the `loopy` macro.  The top-level
forms have a flexible-order, but meaning is clearest if they have the
following order.  All of the arguments are technically optional, but having a
loop without a body wouldn't be useful.

    ;; Returns: '((2 4 6 8 10) (1 3 5 7 9)) and prints messages.
    (loopy
     ;; Name the loop `my-loop'.
     my-loop
     ;; Create the lexically scoped variable `success-p', initialized to `nil'.
     (with (success-p nil))
     ;; Before starting the loop, print a message.
     (before-do (message "Beginning loop ..."))
     ;; Create the loop body.
     (loop (list i (number-sequence 1 10))        ; Assign `i' from 1 through 10.
           (do (message "Checking number: %d" i)) ; Report the value of `i'.
           (if (cl-evenp i)                       ; If `i' is even, add to the list
               (collect found-evens i)            ; of even numbers, otherwise add
             (collect found-odds i)))             ; to the list of odd numbers.
     ;; If the loop completes successfully, print a message and update `success-p'.
     (after-do (message "Loop completed successfully.")
               (setq success-p t))
     ;; Always report based on the value of `success-p', and message the value of
     ;; the lists of even and odd numbers.
     (finally-do (if success-p
                     (message "Confirmed success reported.")
                   (message "W: Success not confirmed!"))
                 (message "Found evens: %s" found-evens)
                 (message "Found odds: %s" found-odds))
     ;; Always return a list containing the list of even numbers and the list of odd
     ;; numbers.
     (finally-return (list found-evens found-odds)))

Loopy is not yet feature complete.  Here are some things that would be nice to
add/have:

-   Add equivalent features for all that `cl-loop` can do.  Many things are
    already covered, but not everything.  See [Translating from `cl-loop`](#translating-from-cl-loop) for
    more.
-   De-structuring can be useful, but this can already be done using repeated
    `expr` commands.
-   Currently, code is evaluated even if it is unused.  Conditionally building
    the code that is evaluated would be more efficient, but decreases
    readability.


<a id="how-to-use"></a>

# How to use

Macro arguments set up the lexical environment the loop runs in, Lisp code
that runs before or after the loop, and the ultimate return value of the
macro.  See the section [Macro Arguments](#macro-arguments).

Loop commands are the main feature of the `loopy` macro.  By "command", I mean
the expressions that make up the `loop` macro argument, such as `list` in
`(list i '(1 2 3))`.  A command inserts code into the loop body, but can also
perform additional setup, such as initializing specified variables or creating
extra ones.  Many commands set up a condition for ending the loop.  See the
section [Loop Commands](#loop-commands).

The loop ends when any condition required by a loop command evaluates to
false.  If no conditions are needed, the loop runs infinitely until a `return`
or `leave` command is reached.  See the section [Exiting the Loop Early](#exiting-the-loop-early).

Return values must be stated explicitly, either as an early return in the loop
body via the `return` or `return-from` commands, or as part of the
`finally-return` macro argument.  `nil` is returned by default.


<a id="macro-arguments"></a>

## Macro Arguments

`loopy` takes at most 7 arguments, which are all technically optional.
Calling the `loopy` macro without arguments creates an infinite loop that
does nothing.

You can name a loop by passing `loopy` an unquoted symbol.  The loop body and
any expressions that are part of the `before-do` and `after-do` arguments are
contained in a `cl-block`.  Naming the loop really just names the block,
allowing for more specific exiting via the function `cl-return` and the loop
commands that wrap it.

All other arguments are expressions that begin with a keyword from the table
below.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Keyword</th>
<th scope="col" class="org-left">Other Names</th>
<th scope="col" class="org-left">Usage</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><code>with</code></td>
<td class="org-left"><code>let*</code></td>
<td class="org-left">Declare variables before the loop.</td>
</tr>


<tr>
<td class="org-left"><code>before-do</code></td>
<td class="org-left"><code>before</code></td>
<td class="org-left">Run Lisp expressions before loop starts.</td>
</tr>


<tr>
<td class="org-left"><code>loop</code></td>
<td class="org-left">Can be excluded.</td>
<td class="org-left">Add expressions to loop body, performing further setup.</td>
</tr>


<tr>
<td class="org-left"><code>after-do</code></td>
<td class="org-left"><code>after</code>, <code>else</code>, <code>else-do</code></td>
<td class="org-left">Run Lisp expressions after loop successfully completes.</td>
</tr>


<tr>
<td class="org-left"><code>finally-do</code></td>
<td class="org-left"><code>finally</code></td>
<td class="org-left">Always run Lisp expressions after loop exits.</td>
</tr>


<tr>
<td class="org-left"><code>finally-return</code></td>
<td class="org-left"><code>return</code></td>
<td class="org-left">Return a value, regardless of how the loop completes.</td>
</tr>
</tbody>
</table>

Additionally, `(finally-return 1 2 3)` is the same as
`(finally-return (list 1 2 3))`.


<a id="loop-commands"></a>

## Loop Commands

Loop commands are only valid when inside the `loop` macro argument.  For
convenience, you do not need to include `loop` in the argument.  An
expression that doesn’t match any other possible argument is assumed to be
the `loop` argument.

Therefore, these are valid:

    (loopy (loop (list i '(1 2 3))
                 (collect coll i))
           (finally-return coll))
    
    (loopy ((list i '(1 2 3))
            (collect coll i))
           (return coll))

and this is not:

    (loopy (with (list i '(1 2 3)))
           (return (collect coll i)))

Trying to use loop commands where they don't belong will result in errors
when the code is evaluated.

Underneath, interpreting a command results in "instructions" that describe
how to substitute code into the loop body and other locations.  This process
is described in detail in [Background Information](#background-information).

Some examples of instructions are:

-   Declaring a given variable in a let form to make sure it's lexically
    scoped.
-   Declaring a generated variable in a let form to contain a given value.
-   Adding a condition for continuing/exiting the loop.
-   Adding code to be run during the main loop body.
-   Adding code to be run after the main loop body.

The implementation details of commands generally shouldn't matter, except
that code from commands is evaluated in the order it was found.  This means
that attempting to do something like

    (loopy (loop (collect coll (+ i 2))
                 (list i '(1 2 3)))
           (return coll))

won't work, as `i` is assigned a value after collecting `(+ i 2)` into
`coll`.

For convenience and understanding, the same command can have multiple names
(such as `do` having the alias `progn`), and some commands can take optional
arguments (such as `list`).

For simplicity, the commands are described using the following notation:

-   If a command has multiple names, the names are separated by a vertical
    bar, such as in `do|progn`.
-   `VAR` is an unquoted symbol that will be used as a variable name, such as
    `i` in `(list i my-list)`.
-   `FUNC` is a Lisp function name, such as `my-func`, `#'my-func` or
    `'my-func`.
-   `NAME` is an unquoted name of a loop (or, more accurately, of a
    `cl-block`).
-   `EXPR` is a single Lisp expression, such as `(+ 1 2)`, `'(1 2 3)`,
    `my-var`, or `(some-function my-var)`.  `EXPRS` means multiple expressions.
    Really, we are concerned with the value of the expression, not the
    expression itself.
-   `CMD` is a loop command, as opposed to a normal Lisp expression.
    `(list i '(1 2 3))`, `(repeat 5)`, and `(return-from outer-loop 7)`
    are examples of loop commands.  `CMDS` means multiple commands.
-   Optional arguments are surround by brackets.  `[EXPR]` is an optional
    expression, and `[CMD]` is an optional command.  By extension,
    `[EXPRS]` is equivalent to `[EXPR [EXPR [...]]]`, and `[CMDS]` to
    `[CMD [CMD [...]]]`.


<a id="commands-for-generic-evaluation"></a>

### Commands for Generic Evaluation

-   **`(do|progn EXPRS)`:** Evaluate multiple Lisp expressions, like a
    `progn`.
    
    You cannot include arbitrary code in the loop body.  Trying to do so will
    result in errors, as the macro will attempt to interpret such code as a
    command.
    
        (loopy ((list i '(1 2 3))
                (do (message "%d" i))))

-   **`(expr|exprs|set VAR [EXPRS])`:** Bind `VAR` to each `EXPR` in order.
    Once the last `EXPR` is reached, it is used repeatedly for the rest of the
    loop.  With no `EXPR`, `VAR` is repeatedly bound to `nil`.
    
    **NOTE**: Loops are lexically scoped, so this using this command does not
    always have the same effect as using `(do (setq VAR EXPR))`.
    
        (loopy ((repeat 5)
                (expr i 1 2 3)
                (collect coll i))
               (return coll)) ; => '(1 2 3 3 3)
        
        (loopy ((repeat 5)
                (expr i 0 (1+ i))
                (collect coll i))
               (return coll)) ; => '(0 1 2 3 4)


<a id="iteration-and-looping-commands"></a>

### Iteration and Looping Commands

Iteration commands bind local variables and determine when the loop ends.
If no command sets that condition, then the loop runs forever.

-   **`(array VAR EXPR)`:** Loop through the elements of the array `EXPR`.
    
        (loopy ((array i [1 2 3])
                (do (message "%d" i))))

-   **`(array-ref|arrayf VAR EXPR)`:** Loop through the elements of the array
    `EXPR`, binding `VAR` as a `setf`-able place.
    
        (loopy (with (my-str "cat"))
               (loop (array-ref i my-str)
                     (do (setf i ?a)))
               (return my-str)) ; => "aaa"

-   **`(cons|conses VAR EXPR [FUNC])`:** Loop through the cons cells of `EXPR`.
    Optionally, find the cons cells via `FUNC` instead of `cdr`.
    
        (loopy (loop (cons i '(1 2 3))
                     (collect coll i))
               (finally-return coll)) ; => ((1 2 3) (2 3) (3))

-   **`(list VAR EXPR [FUNC])`:** Loop through the elements of the list `EXPR`.
    Optionally, update the list by `FUNC` instead of `cdr`.
    
        (loopy ((list i (number-sequence 1 10 3)) ; Inclusive, so '(1 4 7 10).
                (do (message "%d" i))))

-   **`(list-ref|listf VAR EXPR [FUNC])`:** Loop through the elements of the
    list `EXPR`, binding `VAR` as a `setf`-able place.  Optionally, update the
    list by `FUNC` instead of `cdr`.
    
        (loopy (with (my-list '(1 2 3)))
               (loop (list-ref i my-list)
                     (do (setf i 7)))
               (finally-return my-list)) ; Returns '(7 7 7).

-   **`(repeat EXPR)`:** Add a condition that the loop should stop after
    `EXPR` iterations.
    
        (loopy ((repeat 3)
                (do (message "Messaged three times."))))

-   **`(repeat VAR EXPR)`:** Add a condition that the loop should stop after
    `EXPR` iterations.  `VAR` starts at 0, and is incremented by 1 at the
    end of the loop.
    
        (loopy ((repeat i 3)
                (do (message "%d" i))))

-   **`(seq VAR EXPR)`:** Loop through the sequence `val`, binding `var` to
    the elements of the sequence.
    
        (loopy ((seq i [1 2 3]) (collect coll i))
               (return coll)) ; => (1 2 3)

-   **`(seq-ref|seqf VAR EXPR)`:** Loop through the elements of the sequence
    `val`, binding `var` as a `setf`-able place.
    
        (loopy (with (my-seq '(1 2 3 4)))
               (loop (seq-ref i my-seq)
                     (do (setf i 7)))
               (return my-seq)) ; => '(7 7 7 7)


<a id="accumulation-commands"></a>

### Accumulation Commands

Unlike in `cl-loop`, the presence of an accumulation command does not imply
a return value.  You must provide a variable in which to store the
accumulated value.  If you wish, you can return that variable.

-   **`(append VAR EXPR)`:** Repeatedly `append` the value of `EXPR` to `VAR`.
    `VAR` starts as `nil`.
    
        (loopy ((list i '((1 2 3) (4 5 6)))
                (append coll i))
               (return coll)) ; => '(1 2 3 4 5 6)

-   **`(collect VAR EXPR)`:** Repeatedly `append` a list containing value of
    `EXPR` to `VAR`.  `VAR` starts as `nil`.
    
        (loopy ((seq i [1 2 3])
                (collect coll i))
               (finally-return coll)) ; => '(1 2 3)
    
    In `cl-loop`, `collect EXPR` means to repeatedly `push` the value of
    `EXPR` into the accumulated list, and then `nreverse` that list for a
    return value.  If you specifically want this behavior, then you should use
    the `push-into` command like in its example below.

-   **`(concat VAR EXPR)`:** Repeatedly `concat` the value of `EXPR` onto the
    end of `VAR`.  `VAR` starts as `nil`.  See the `vconcat` command for
    vectors.
    
        (loopy ((list i '("a" "b" "c"))
                (concat str i))
               (return str)) ; => "abc"

-   **`(count VAR EXPR)`:** Count the number of times that `EXPR` evaluates to a
    non-nil value, adding 1 to `VAR` each time.  `VAR` starts at 0.
    
        (loopy ((list i '(1 nil 3 nil 5))
                (count non-nil-count i))
               (return non-nil-count)) ; => 3

-   **`(max|maximize VAR EXPR)`:** Repeatedly set `VAR` to the greater of `VAR`
    and the value of `EXPR`.  `VAR` starts at `-1.0e+INF`, so that any other
    value should be greater that it.
    
        (loopy ((list i '(1 11 2 10 3 9 4 8 5 7 6))
                (max my-max i))
               (return my-max)) ; => 11

-   **`(min|minimize VAR EXPR)`:** Repeatedly set `VAR` to the lesser of `VAR`
    and the value of `EXPR`.  `VAR` starts at `1.0e+INF`, so that any other
    value should be less than it.
    
        (loopy ((list i '(1 11 2 10 3 0 9 4 8 5 7 6))
                (min my-min i))
               (return my-min)) ; => 0

-   **`(nconc VAR EXPR)`:** Repeatedly concatenate the value of `EXPR` onto
    `VAR` with `nconc`.  Unlike `append`, `nconc` does not concatenate copies
    of the lists, but modifies `VAR` directly.
    
        (loopy (loop (list i '((1 2 3 4) (5 6 7 8)))
                     (nconc my-new-list i))
               (return my-new-list)) ; => '(1 2 3 4 5 6 7 8)

-   **`(push|push-into VAR EXPR)`:** Repeatedly `push` `EXPR` into `VAR`.  `VAR`
    stars as `nil`.
    
        (loopy ((seq i [1 2 3])
                (push reversed i))
               (finally-return (nreverse reversed))) ; => '(1 2 3)

-   **`(sum VAR EXPR)`:** Repeatedly set `VAR` to the sum of the value of `EXPR`
    and `VAR`.  `VAR` starts at 0.
    
        (loopy ((list i '(1 2 3 4))
                (sum my-sum i))
               (return my-sum)) ; => 10

-   **`(vconcat VAR EXPR)`:** Repeatedly `vconcat` the value of `EXPR` onto
    `VAR`.  `VAR` starts as `nil`.
    
        (loopy ((list i '([1 2 3] [4 5 6]))
                (vconcat vector i))
               (return vector)) ; => [1 2 3 4 5 6]


<a id="control-flow"></a>

### Control Flow


<a id="conditionals"></a>

#### Conditionals

Conditional commands in `loopy` can take multiple sub-commands, and work
like their Lisp counterparts.  There is therefore no need for an `and`
command as used in `cl-loop`.

-   **`(when EXPR CMDS)`:** Run `CMDS` only if `EXPR` is non-nil.
    
        ;; Get only the inner lists with all even numbers.
        ;; => '((2 4 6) (8 10 12) (16 18 20))
        (loopy ((list i '((2 4 6) (8 10 12) (13 14 15) (16 18 20)))
                (when (loopy ((list j i)
                              (when (cl-oddp j)
                                (return nil)))
                             (else-do (cl-return t)))
                  (collect only-evens i)))
               (finally-return only-evens))

-   **`(if EXPR CMDS)`:** Run the first command if `EXPR` is non-nil.
    Otherwise, run the remaining commands.
    
        ;; => '((7 5 3 1) (6 4 2) (3 3 3))
        (loopy ((seq i [1 2 3 4 5 6 7])
                (if (cl-oddp i)
                    (push-into reversed-odds i)
                  (push-into reversed-evens i)
                  (push-into some-threes 3)))
               (finally-return (list reversed-odds
                                     reversed-evens
                                     some-threes)))

-   **`(cond [(EXPR CMDS) [...]])`:** For the first `EXPR` to evaluate to
    non-nil, run the following commands `CMDS`.
    
        (loopy ((list i '(1 2 3 "cat" 4 5 6 "dog"))
                (cond
                 ((not (numberp i)) (collect not-numbers i))
                 ((cl-evenp i)      (collect evens i))
                 (t                 (collect odds i))))
               (return evens odds not-numbers)) ; => '((2 4 6) (1 3 5) ("cat" "dog"))


<a id="skipping-an-iteration"></a>

#### Skipping an Iteration

-   **`(skip|continue)`:** Go to next loop iteration.
    
        ;; => (2 4 6 8 12 14 16 18)
        (loopy ((seq i (number-sequence 1 20))
                (when (zerop (mod i 10))
                  (skip))
                (when (cl-evenp i)
                  (push-into my-collection i)))
               (finally-return (nreverse my-collection)))


<a id="exiting-the-loop-early"></a>

#### Exiting the Loop Early

The loop is contained in a `cl-block`, and these forms are all variations
of `cl-return-from` underneath.  In fact, you could use `(do
     (cl-return-from NAME VAL))` to achieve the same effect.  These forms are
provided for convenience.

-   **`(return EXPR)`:** Leave the current loop, returning the value of `EXPR`.
    
        (loopy (with  (j 0))
               ((do (cl-incf j))
                (when (> j 5)
                  (return j))))

-   **`(return-from NAME EXPR)`:** Leave the loop `NAME`, returning the value
    of `EXPR`.
    
        (loopy outer-loop
               ((list inner-list '((1 2 3) (1 bad-val? 1) (4 5 6)))
                (do (loopy ((list i inner-list)
                            (when (eq i 'bad-val?)
                              (return-from outer-loop 'bad-val?)))))))

-   **`(leave|break)`:** Leave the loop, returning `nil`.
    
        (loopy ((list i '(1 2 3 "cat" 4 5 6))
                (if (numberp i)
                    (do (message "Number: %d" i))
                  (leave))))

-   **`(leave-from|break-from NAME)`:** Leave the loop `NAME`, returning `nil`.
    
        (loopy outer
               (with (failure-condition 'fail)
                     (failed-p nil))
               ((list i '((1 2 3) (4 5 6) (7 fail 8)))
                (do (loopy ((list j i)
                            (when (eq j failure-condition)
                              ;; Note: Can't do (expr failed-p t), since
                              ;;       `expr' is local to its own loop.
                              (do (setq failed-p t))
                              (break-from outer))))))
               (finally-do (if failed-p
                               (message "Failed!")
                             (message "Success!"))))


<a id="adding-custom-commands"></a>

# Adding Custom Commands


<a id="background-information"></a>

## Background Information

The core working of `loopy` is taking a command and generating code that is
substituted into a loop body.

For example, the parsing the command `(list i '(1 2 3))` produces the
following instructions.  Some commands require the creation of unique
temporary variables, such as `g3019` in the below output.

    ((loopy--implicit-vars g3019 '(1 2 3))
     (loopy--explicit-vars i nil)
     (loopy--pre-conditions consp g3019)
     (loopy--main-body setq i (car g3019))
     (loopy--latter-body setq g3019 (cdr g3019)))

The `car` of an instruction is the place to put code and the `cdr` of the
instruction is said code to put.  You can see that not all of the code to be
inserted is a valid Lisp form.  Some of it is inserted into variable lists
like in `let` and `let*` instead of being treated as an expression.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left">Place</th>
<th scope="col" class="org-left">Code</th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><code>loopy--implicit-vars</code></td>
<td class="org-left"><code>(g3019 '(1 2 3))</code></td>
</tr>


<tr>
<td class="org-left"><code>loopy--explicit-vars</code></td>
<td class="org-left"><code>(i nil)</code></td>
</tr>


<tr>
<td class="org-left"><code>loopy--pre-conditions</code></td>
<td class="org-left"><code>(consp g3019)</code></td>
</tr>


<tr>
<td class="org-left"><code>loopy--main-body</code></td>
<td class="org-left"><code>(setq i (car g3019))</code></td>
</tr>


<tr>
<td class="org-left"><code>loopy--latter-body</code></td>
<td class="org-left"><code>(setq g3019 (cdr g3019))</code></td>
</tr>
</tbody>
</table>

Commands are parsed by `loopy--parse-body-forms`, which receives a list of
commands and returns a list of instructions.  For commands that take
sub-commands as arguments (such as `cond`, `if`, and `when`), more specific
parsing functions are called in a mutually recursive fashion (e.g.,
Function-1 uses Function-2 which uses Function-1, and so on).

For example, consider the function `loopy--parse-conditional-forms`, which
parses the `if`, `when`, and `unless` commands.  It needs to be able to group
any code going to the loop body under its respective conditional control
structure and condition.  To do this, it uses `loopy--parse-body-forms` to
turn its sub-commands into a list of instructions, and then checks the `car`
of each instruction.

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

The hardest part of this exchange is making sure the inserted code ends up in
the correct order.

A loop body command has 7 main places to put code.  Here is a quick
description of each and an example taken mainly from parsing the `list`
command.

-   **`loopy--explicit-generalized-vars`:** Lists of a symbol and a macro
    expansion that will be given to `cl-symbol-macrolet`.  This is used for
    `setf`-able variables.

-   **`loopy--implicit-vars`:** Lists of a symbol and an expression that will
    be given to `let`.  This is used for creating variables that are not
    named by must exists, such as for holding `'(1 2 3)` in
    `(list i '(1 2 3))`.

-   **`loopy--explicit-vars`:** Lists of a symbol and an expression that will
    be given to `let`.  This is needed to ensure that named variables in
    commands are lexically scoped, such as the `i` in `(list i '(1 2 3))`.

-   **`loopy--pre-conditions`:** Expressions that determine if the `while`
    loop runs/continues, such as whether a list still has elements in it.
    If there is more than one expression, than all expressions are used in
    an `and` special form.

-   **`loopy--main-body`:** Expressions that make up the main body of the
    loop.

-   **`loopy--latter-body`:** Expressions that need to be run after the main
    body, such as updating implicit variables.

-   **`loopy--post-conditions`:** Expressions that determine whether the
    `while` loop continues, but checked after the loop body has run.  The
    code from this is ultimately appended to the latter body before being
    substituted in.

There are 5 more variables a loop command can push to, but they are derived
from the macro's arguments.  Adding to them after using a macro argument
might lead to unintended behavior.  You might wish to use them if, for
example, you are concerned with what happens after the loop exits/completes.

-   **`loopy--with-vars`:** Lists of a symbol and an expression that will be
    given to `let*`.  These are derived from the `with` macro argument.

-   **`loopy--before-do`:** Expressions to evaluate before the loop.  These are
    derived from the `before-do` macro argument.

-   **`loopy--after-do`:** Expressions to evaluate after the loop completes
    successfully.  These are derived from the `after-do` macro argument.

-   **`loopy--final-do`:** Expressions to evaluate after the loop completes,
    regardless of success.  These are derived from the `finally-do` macro
    argument.

-   **`loopy--final-return`:** An expression that is always returned by the
    macro, regardless of any early returns in the loop body.  This is
    derived from the `finally-return` macro argument.

These variables will be substituted into the following list of code, which is
returned by the `loopy` macro for evaluation.

    `(cl-symbol-macrolet (,@(or loopy--explicit-generalized-vars
                                (list (list (gensym) nil))))
       (let* (,@(or loopy--with-vars '((_))))
         (let (,@(or (append loopy--implicit-vars loopy--explicit-vars)
                     '((_))))
           (let ((loopy--early-return-capture
                  (cl-block ,loopy--name-arg
                    ,@loopy--before-do
                    (while ,(cl-case (length loopy--pre-conditions)
                              (0 t)
                              (1 (car loopy--pre-conditions))
                              (t (cons 'and loopy--pre-conditions)))
                      (cl-tagbody
                       ,@loopy--main-body
                       loopy--continue-tag
                       ,@loopy--latter-body))
                    ,@loopy--after-do
                    nil)))
             ,@loopy--final-do
             ,(if loopy--final-return
                  loopy--final-return
                'loopy--early-return-capture)))))


<a id="a-small-example"></a>

## A Small Example

To implement a custom loop body command, `loopy` needs two pieces of
information:

1.  The keyword that names your command
2.  The parsing function that can turn uses of your command into instructions.

Importantly, your custom commands cannot share a name.

For example, say that you're tired of typing out
`(do (message "Hello, %s" first last))` and would prefer to instead use
`(greet FIRST [LAST])`.  This only requires pushing code into the main
loopy body, so the definition of the parsing function is quite simple.

    (cl-defun my-loopy-greet-command-parser ((_ first &optional last))
      "Greet one with first name FIRST and optional last name LAST."
      `((loopy--main-body . (if ,last
                                (message "Hello, %s %s" ,first ,last)
                              (message "Hello, %s" ,first)))))

`loopy` will pass the entire command expression to the parsing function, and
expects back a list of instructions.

To tell `loopy` about this function, add it and the command name `greet` to
`loopy-custom-command-parsers`.

    (add-to-list 'loopy-custom-command-parsers
                 '(greet . my-loopy-greet-command-parser))

After that, you can use your custom command in the loop body.

    (loopy ((list name '(("John" "Deer") ("Jane" "Doe") ("Jimmy")))
            (greet (car name) (cadr name))))

By running `M-x pp-macroexpand-last-sexp` on the above expression, you can
see that it expands to do what we want, as expected.

    (cl-symbol-macrolet ((g3314 nil))
      (let* ((_))
        (let ((g3313 '(("John" "Deer") ("Jane" "Doe") ("Jimmy")))
              (name nil))
          (let ((loopy--early-return-capture
                 (cl-block nil
                   (while (consp g3313)
                     (cl-tagbody
                      (setq name (car g3313))
                      (if (cadr name)
                          (message "Hello, %s %s" (car name) (cadr name))
                        (message "Hello, %s" (car name)))
                      loopy--continue-tag
                      (setq g3313 (cdr g3313))))
                   nil)))
            loopy--early-return-capture))))


<a id="a-slightly-more-complicated-example"></a>

## A Slightly More Complicated Example

Lets say we want to emulate `cl-loop`'s `always` clause, which causes the
loop to return `nil` if an expression evaluates to `nil` and `t` otherwise.

Here is an example:

    (cl-loop for i in (number-sequence 1 9) always (< i 10)) ; => t

Without custom commands, you could write the following in `loopy`.

    (loopy ((list i (number-sequence 1 9))
            (unless (< i 10) (return nil)))
           (else-do (cl-return t)))

This general approach is certainly wordier.  Here's how you could do it with
a custom command:

    (cl-defun my--loopy-always-command-parser ((_ &rest conditions))
      "Parse a command of the form `(always cond1 cond2)'.
    If any condition is `nil', `loopy' should immediately return `t'"
      (let (instructions)
        (push `(loopy--after-do . (cl-return t)) instructions)
        (dolist (condition conditions)
          (push `(loopy--post-conditions . ,condition) instructions))
        instructions))
    
    (add-to-list 'loopy-custom-command-parsers
                 (cons 'always #'my--loopy-always-command-parser))
    
    
    (loopy ((list i (number-sequence 1 9)) (always (< i 10)))) ; => t
    
    (loopy ((list i (number-sequence 1 9))
            (list j '(2 4 6 8 9))
            (always (< i 10) (cl-evenp j)))) ; => nil

It's still slightly more typing, but not by much.  I take this to mean that
`loopy` is better for more complicated loops rather than smaller ones.


<a id="how-does-it-compare-to-other-approaches"></a>

# How does it compare to `cl-loop`?

`loopy` should be comparable with `cl-loop` for most things, keeping in
mind the following:

-   It is probably less efficient than `cl-loop`, though I am so far trying to
    keep the same logic that `cl-loop` uses.
-   It has more flexible control-flow commands, under which you can easily group
    sub-commands, including assignments.
-   Using an accumulation command does not imply a return value.
-   It has a `skip` command to skip to skip the rest of the loop body and
    immediately start the next iteration.  Of course, a similar effect could be
    achieved using the `when` or `unless` commands.

`loopy` is not always one-to-one replacement for `cl-loop`, but it is easy to
use and extend, and performs well in the cases that it already handles.

Below is a simple example of `loopy` vs `cl-loop`.

    (require 'cl-lib)
    (cl-loop with some-thing = 5
             for i from 1 to 100
             do (message "I is %s" i)
             when (> (+ i 5) 20)
             return (format "Done: %d" i))
    
    (require 'loopy)
    (loopy (with (some-thing 5))
           ((list i (number-sequence 1 100))
            (do (message "I is %s" i))
            (when (> (+ i 5) 20)
              (return (format "Done: %d" i)))))

The main benefit (I believe) of Loopy is clearer grouping of constructs under
conditionals while still using a clean syntax, such as in the below example.

    (loopy ((list i (number-sequence 1 20))
            (when (cl-evenp i)
              (expr once i)
              (expr twice (* 2 i))
              (push-into together (cons once twice))))
           (finally-return (nreverse together)))

In my experience, `cl-loop` does not allow the easy grouping of assignment
statements under a `when` condition.  For example, below is something I would
like to try to do with `cl-loop`.

I am aware that in this example the `for` statements aren't necessary and that
the `collect` statements would be sufficient, but (when I come across things
like this in my work) I would like to use them to declare variables for
readability purposes.

    (require 'cl-lib)
    (save-match-data
      (cl-loop with pattern = "^Line\\([[:digit:]]\\)-Data\\([[:digit:]]\\)"
               for line in (split-string "Line1-Data1\nBad\nLine2-Data2")
               when (string-match pattern line)
               for line-num = (concat "L" (match-string 1 line))
               and for data-nums = (concat "D" (match-string 2 line))
    
               ;; … Further processing now that data is named …
    
               and collect (match-string 1 line) into line-nums
               and collect (match-string 2 line) into data-nums
               finally return (list line-nums data-nums)))
    
    ;; Normal Elisp:
    (save-match-data
      (let ((pattern "^Line\\([[:digit:]]\\)-Data\\([[:digit:]]\\)")
            (line-nums)
            (data-nums))
        (dolist (line (split-string "Line1-Data1\nBad\nLine2-Data2"))
          (when (string-match pattern line)
            (let ((line-num (concat "L" (match-string 1 line)))
                  (datum-num (concat "D" (match-string 2 line))))
    
              ;; … Further processing now that data is named …
    
              (push line-num line-nums)
              (push datum-num data-nums))))
        (list (nreverse line-nums) (nreverse data-nums))))

Here is how one could currently do it with `loopy`:

    (require 'loopy)
    (save-match-data
      (loopy (with (pattern "^Line\\([[:digit:]]\\)-Data\\([[:digit:]]\\)"))
             ((list line (split-string "Line1-Data1\nBad\nLine2-Data2"))
              (when (string-match pattern line)
                (expr line-num (concat "L" (match-string 1 line)))
                (expr datum-num (concat "D" (match-string 2 line)))
    
                ;; … Further processing now that data is named …
    
                (collect line-nums line-num)
                (collect data-nums datum-num)))
             (finally-return line-nums data-nums)))

I believe that the value of the macro increases for longer loop bodies with
several conditional commands.

Another nice ability, one that I'm not sure `cl-loop` has, is
skipping/continuing a loop iteration.

    ;; Returns even numbers that aren't multiples of 10.
    (loopy ((list i (number-sequence 1 20))
            (when (zerop (mod i 10))
              (skip))
            (when (cl-evenp i)
              (push-into my-collection i)))
           (finally-return (nreverse my-collection))) ; => (2 4 6 8 12 14 16 18)


<a id="translating-from-cl-loop"></a>

## Translating from `cl-loop`


<a id="for-clauses"></a>

### For Clauses

As Emacs has many functions that return lists, I decided to not implement an
exact equivalent for every for-clause that `cl-loop` has.  Instead, one can
just iterate through the return value of the appropriate function using the
`list` command.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left"><code>cl-loop</code></th>
<th scope="col" class="org-left"><code>loopy</code></th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><code>for VAR from EXPR1 to EXPR2 by EXPR3</code></td>
<td class="org-left"><code>(list VAR (number-sequence EXPR1 EXPR2 EXPR3))</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR in LIST [by FUNCTION]</code></td>
<td class="org-left"><code>(list VAR LIST [FUNC])</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR on LIST [by FUNCTION]</code></td>
<td class="org-left"><code>(cons VAR VAL [FUNC])</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR in-ref LIST by FUNCTION</code></td>
<td class="org-left"><code>(list-ref VAR LIST [FUNC])</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR across ARRAY</code></td>
<td class="org-left"><code>(array VAR ARRAY)</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR across-ref ARRAY</code></td>
<td class="org-left"><code>(array-ref VAR ARRAY)</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the elements of SEQUENCE</code></td>
<td class="org-left"><code>(seq VAR SEQUENCE)</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the elements of-ref SEQUENCE</code></td>
<td class="org-left"><code>(seq-ref VAR SEQUENCE)</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the symbols [of OBARRAY]</code></td>
<td class="org-left">None so far.</td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the hash-keys of HASH-TABLE</code></td>
<td class="org-left"><code>(list VAR (hash-table-keys HASH-TABLE))</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the hash-values of HASH-TABLE</code></td>
<td class="org-left"><code>(list VAR (hash-table-values HASH-TABLE))</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the key-codes of KEYMAP</code></td>
<td class="org-left">None so far.</td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the key-bindings of KEYMAP</code></td>
<td class="org-left">None so far.</td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the key-seqs of KEYMAP</code></td>
<td class="org-left">None so far.</td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the overlays [of BUFFER]</code></td>
<td class="org-left">None so far.</td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the intervals [of BUFFER]</code></td>
<td class="org-left">None so far.</td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the frames</code></td>
<td class="org-left"><code>(list VAR (frame-list))</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the windows [of FRAME]</code></td>
<td class="org-left"><code>(list VAR (window-list FRAME))</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR being the buffers</code></td>
<td class="org-left"><code>(list VAR (buffer-list))</code></td>
</tr>


<tr>
<td class="org-left"><code>for VAR = EXPR1 then EXPR2</code></td>
<td class="org-left"><code>(expr VAR EXPR1 EXPR2)</code></td>
</tr>
</tbody>
</table>


<a id="iteration-clauses"></a>

### Iteration Clauses

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left"><code>cl-loop</code></th>
<th scope="col" class="org-left"><code>loopy</code></th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><code>repeat INT</code></td>
<td class="org-left"><code>(repeat INT)</code></td>
</tr>


<tr>
<td class="org-left"><code>while COND</code></td>
<td class="org-left"><code>(unless COND (leave))</code></td>
</tr>


<tr>
<td class="org-left"><code>until COND</code></td>
<td class="org-left"><code>(when COND (leave))</code></td>
</tr>


<tr>
<td class="org-left"><code>iter-by iterator</code></td>
<td class="org-left">None so far.</td>
</tr>
</tbody>
</table>

The clauses `always`, `never`, `thereis` can be replaced with a combination
of `loopy`'s loop commands and macro arguments.  Below is an example from the
CL Lib manual.

    ;; With `cl-loop':
    (if (cl-loop for size in size-list always (> size 10))
        (only-big-sizes)
      (some-small-sizes))
    
    ;; With `loopy`:
    ;; Depends on whether the functions have a return value.
    (loopy ((list size size-list)
            ;; `return` is just a wrapper for `cl-return`.
            (when (< size 10) (return (some-small-sizes))))
           ;; Only runs if loop doesn't exit early.
           (after-do (cl-return (only-big-sizes))))

A seen in the above example, `loopy` does not always have a one-to-one
translation to `cl-loop` ([though you
could try a custom command](#a-slightly-more-complicated-example)).

It is not an explicit goal to be able to replace all uses of `cl-loop` with
`loopy`.  I'd prefer that `loopy` be useful in places where `cl-loop` might
not be enough, instead of forcing `loopy` into places that `cl-loop` already
works well.

Other options in the above example include `cl-every` and `seq-every-p`.


<a id="accumulation-clauses"></a>

### Accumulation Clauses

**NOTE**: In `loopy`, accumulation commands do not imply a return value.  You
cannot simply do `(collect FORM)`; you must always give a variable into which
to accumulate the form.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left"><code>cl-loop</code></th>
<th scope="col" class="org-left"><code>loopy</code></th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><code>append FORM into VAR</code></td>
<td class="org-left"><code>(append VAR FORM)</code></td>
</tr>


<tr>
<td class="org-left"><code>collect FORM into VAR</code></td>
<td class="org-left"><code>(collect VAR FORM)</code></td>
</tr>


<tr>
<td class="org-left"><code>concat FORM into VAR</code></td>
<td class="org-left"><code>(concat VAR FORM)</code></td>
</tr>


<tr>
<td class="org-left"><code>count FORM into VAR</code></td>
<td class="org-left"><code>(count VAR FORM)</code></td>
</tr>


<tr>
<td class="org-left"><code>maximize FORM into VAR</code></td>
<td class="org-left"><code>(max VAR FORM)</code></td>
</tr>


<tr>
<td class="org-left"><code>minimize FORM into VAR</code></td>
<td class="org-left"><code>(min VAR FORM)</code></td>
</tr>


<tr>
<td class="org-left"><code>nconc FORM into VAR</code></td>
<td class="org-left"><code>(nconc VAR FORM)</code></td>
</tr>


<tr>
<td class="org-left"><code>sum FORM into VAR</code></td>
<td class="org-left"><code>(sum VAR FORM)</code></td>
</tr>


<tr>
<td class="org-left"><code>vconcat FORM into VAR</code></td>
<td class="org-left"><code>(vconcat VAR FORM)</code></td>
</tr>
</tbody>
</table>


<a id="other-clauses"></a>

### Other Clauses

In `loopy`, `if`, `when`, and `unless` can take multiple loop commands as
arguments, and operate more like their Lisp counterparts.

This means that `if` is not a synonym for `when`.  Just like the normal Lisp
special form `if`, `(if COND cmd1 cmd2 cmd3)` only runs `cmd1` if `COND`
evaluates to non-nil, and only runs commands `cmd2` and `cmd3` if `COND`
evaluates to `nil`.

`loopy` also provides the command `cond`, which works like the normal Lisp
special form `cond`.

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="org-left" />

<col  class="org-left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="org-left"><code>cl-loop</code></th>
<th scope="col" class="org-left"><code>loopy</code></th>
</tr>
</thead>

<tbody>
<tr>
<td class="org-left"><code>with var = value</code></td>
<td class="org-left"><code>(with (VAR VALUE))</code> as a macro argument</td>
</tr>


<tr>
<td class="org-left"><code>if COND clause</code></td>
<td class="org-left"><code>(if COND CMDS)</code> as a loop command</td>
</tr>


<tr>
<td class="org-left"><code>when COND clause</code></td>
<td class="org-left"><code>(when COND CMDS)</code> as a loop command</td>
</tr>


<tr>
<td class="org-left"><code>unless COND clause</code></td>
<td class="org-left"><code>(unless COND CMDS)</code> as a loop command</td>
</tr>


<tr>
<td class="org-left"><code>named NAME</code></td>
<td class="org-left"><code>NAME</code> as a macro argument</td>
</tr>


<tr>
<td class="org-left"><code>initially [do] EXPRS</code></td>
<td class="org-left"><code>(before-do EXPRS)</code> as a macro argument</td>
</tr>


<tr>
<td class="org-left"><code>finally [do] EXPRS</code></td>
<td class="org-left"><code>(finally-do EXPRS)</code> as a macro argument</td>
</tr>


<tr>
<td class="org-left"><code>finally return EXPR</code></td>
<td class="org-left"><code>(finally-return EXPR)</code> as a macro argument</td>
</tr>


<tr>
<td class="org-left"><code>do EXPR</code></td>
<td class="org-left"><code>(do EXPRS)</code> as a loop command</td>
</tr>


<tr>
<td class="org-left"><code>return EXPR</code></td>
<td class="org-left"><code>(return EXPR)</code> as a loop command</td>
</tr>
</tbody>
</table>


<a id="orga915407"></a>

# Real-World Examples

> This section contains examples of loops that exist in real-world commands.
> To these loops in context, full examples of those commands can be found in
> the file <examples.el>.

One command that could see a benefit from using `loopy` is `selectrum-swiper`
from the [Selectrum wiki](https://github.com/raxod502/selectrum/wiki/Useful-Commands#swiper-like-jumping-to-matching-lines).  This command allows a user to jump to a matched
line in the buffer.  Candidates are created by looping through text lines,
formatting non-empty lines and collecting the formatted lines into a list of
candidates.  At the same time, it selects a default candidate by finding the
non-empty line closest to the current line.

Here is the main portion of the command, which uses `cl-loop`.

    ;; ...
    (cl-loop with minimum-line-number = (line-number-at-pos (point-min) t)
             with buffer-text-lines = (split-string (buffer-string) "\n")
             with number-format = (concat
                                   "L%0"
                                   (number-to-string
                                    (length (number-to-string
                                             (length buffer-text-lines))))
                                   "d: ")
    
             with closest-candidate = nil
             with distance-to-current-line = nil
             with smallest-distance-to-current-line = most-positive-fixnum
    
             with formatted-line = nil
             with formatted-lines = nil
    
             for txt in buffer-text-lines
             for num = minimum-line-number then (1+ num)
             unless (string-empty-p txt) ; Just skip empty lines.
             do
             (setq formatted-line (propertize
                                   txt
                                   'selectrum-candidate-display-prefix
                                   (propertize
                                    (format number-format num)
                                    'face 'completions-annotations)
                                   'line-num num)
                   distance-to-current-line (abs (- current-line-number num)))
             (push formatted-line formatted-lines)
             (when (< distance-to-current-line
                      smallest-distance-to-current-line)
               (setq smallest-distance-to-current-line distance-to-current-line
                     closest-candidate formatted-line))
             finally return (cons closest-candidate
                                  (nreverse formatted-lines)))
    ;; ...

Here is how it could be done with `loopy`.  The logic in the original is a
bit different, but this is still a good demonstration of the value of grouping
loop commands under conditionals.  Because `cl-loop` doesn’t have an
equivalent feature, one needs to declare more variables with the `with`
keyword and rely on Lisp expressions for processing instead of clauses.

    ;; ...
    (loopy (with (buffer-text-lines (split-string (buffer-string) "\n"))
                 (number-format
                  (concat "L%0"
                          (number-to-string
                           (length (number-to-string
                                    (length buffer-text-lines))))
                          "d: ")))
           (loop (list line-text buffer-text-lines)
                 (expr line-num (line-number-at-pos (point-min) t) (1+ line-num))
                 (unless (string-empty-p line-text)
                   (push-into formatted-lines
                              (propertize line-text
                                          'selectrum-candidate-display-prefix
                                          (propertize
                                           (format number-format line-num)
                                           'face 'completions-annotations)
                                          'line-num line-num))
                   ;; There are a few different ways that you could express this.
                   (when (null default-cand)
                     (expr prev-dist +1.0e+INF dist-to-default-cand)
                     (expr dist-to-default-cand (abs (- current-line-number
                                                        line-num)))
                     (when (> dist-to-default-cand prev-dist)
                       (expr default-cand (cadr formatted-lines))))))
           ;; Could also use `cl-multiple-value-bind' and `finally-return',
           ;; which has the benefit of not being captured by the loop's
           ;; `let'-forms.
           (finally-do (setq default-candidate default-cand
                             formatted-candidates (nreverse formatted-lines))))
    ;; ...

Another command to compare against is `selectrum-outline`, from the same page.
This command is a bit more complicated than `selectrum-swiper`, but the logic
is similar.  The code loops through each line in the buffer, ignoring empty
lines.  For each non-empty line found that matches a pre-determined regular
expression (which describes a one-line heading), a history of parent headings
are prepended to the string, which is collected into a list of formatted
candidates.  At the same time, the loop find the current heading, which is
assumed to be the closest heading before the current line.

Because of the stated limitation with grouping under conditionals, I found this
easier to write in normal ELisp.  Since I just want to compare structure, I’ve
removed the comments.  You can find a commented version on Selectrum’s wiki.

    ;; ...
    (let ((selectrum-should-sort-p nil)
          (buffer-lines (split-string (buffer-string) "\n"))
          (line-number 0)
          (line-number-format)
          (default-heading)
          (current-line-number (line-number-at-pos (point)))
          (backwards-prefix-list)
          (prev-heading-text)
          (prev-heading-level)
          (formatted-headings))
    
      (setq line-number-format
            (concat "L%0"
                    (number-to-string
                     (length (number-to-string (length buffer-lines))))
                    "d: "))
    
      (save-match-data
        (dolist (text-line buffer-lines)
          (cl-incf line-number)
          (when (string-match heading-regexp text-line)
            (let ((heading-text (match-string-no-properties 2 text-line))
                  (heading-level
                   (length (match-string-no-properties 1 text-line)))
                  (formatted-heading))
    
              (when (null prev-heading-level)
                (setq prev-heading-level heading-level))
    
              (cond ((> heading-level prev-heading-level)
                     (setq backwards-prefix-list (cons prev-heading-text
                                                       backwards-prefix-list)
                           prev-heading-level heading-level))
                    ((< heading-level prev-heading-level)
                     (setq backwards-prefix-list (last backwards-prefix-list
                                                       heading-level)
                           prev-heading-level heading-level)))
    
              (setq prev-heading-text heading-text)
    
              (when (and (null default-heading)
                         (> line-number current-line-number))
                (setq default-heading (car formatted-headings)))
    
              (push (propertize
                     (concat (string-join (reverse backwards-prefix-list) "/")
                             (and backwards-prefix-list "/")
                             heading-text)
                     'line-number line-number
                     'selectrum-candidate-display-prefix
                     (propertize
                      (format line-number-format line-number)
                      'face 'completions-annotations))
                    formatted-headings)))))
      ;; ...
      )

Here is a version in `loopy`.

    ;; ...
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
    
                     (cond ((> heading-level (or prev-heading-level
                                                 heading-level))
                            (push-into backwards-prefix-list prev-heading-text))
                           ((< heading-level (or prev-heading-level
                                                 heading-level))
                            (expr backwards-prefix-list
                                  (last backwards-prefix-list heading-level))))
    
                     (expr prev-heading-text heading-text)
                     (expr prev-heading-level heading-level)
    
                     (when (and (null default-heading)
                                (> (- line-number current-line-number) 0))
                       (expr default-heading (car formatted-headings)))
    
                     (push-into
                      formatted-headings
                      (propertize
                       (concat (string-join (reverse backwards-prefix-list) "/")
                               (and backwards-prefix-list "/")
                               heading-text)
                       'line-number line-number
                       'selectrum-candidate-display-prefix
                       (propertize (format line-number-format line-number)
                                   'face 'completions-annotations)))))
             (finally-return default-heading (nreverse formatted-headings)))
          ;; ...
          )))

In my opinion, the `loopy` version is a bit cleaner.  If one we’re writing
code like this often (say, in a library), then the loop body could be
simplified even further with [custom commands](#adding-custom-commands).


# Footnotes

<sup><a id="fn.1" href="#fnr.1">1</a></sup> <https://www.gnu.org/software/emacs/manual/html_node/cl/Loop-Facility.html#Loop-Facility>

<sup><a id="fn.2" href="#fnr.2">2</a></sup> <https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequence-Functions.html>,
<elisp#Sequence Functions>

<sup><a id="fn.3" href="#fnr.3">3</a></sup> <https://www.gnu.org/software/emacs/manual/html_node/cl/index.html>, <cl>

<sup><a id="fn.4" href="#fnr.4">4</a></sup> <https://www.gnu.org/software/emacs/manual/html_node/elisp/Sequences-Arrays-Vectors.html>,
<elisp#Sequences Arrays Vectors>
