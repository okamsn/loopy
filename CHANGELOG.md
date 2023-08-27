# CHANGELOG

This document describes the user-facing changes to Loopy.

## Unreleased

### Bugs Fixed

- Rewrite `prepend` in terms of `append`.  Rewrite `push-into` in terms of
  `collect`.  This change makes these commands work with the new system for
  optimized accumulation variables.

- Without an explicit starting value for the accumulation variable, `reduce` now
  uses the first accumulated value without passing it to the function.  This is
  how reducing actually works, and was the intended behavior. Previously, the
  function was always called. _This is a breaking change._ See [#164].

  ```emacs-lisp
  ;; Now correct behavior (would previously error):
  ;; => 6
  (loopy (list i '(1 2 3))
         (reduce i #'*))
  ```

### Breaking Changes

- Make it an error to re-use iteration variables with multiple iteration
  commands.  The resulting code shouldn't have worked anyway, but we now report
  it as an error during macro expansion.

  ```elisp
  ;; Will now signal an error during expansion:
  (loopy (list i '(1 2 3))
         (list i '(4 5 6)))
  ```

- Using multiple conditions in `always`, `never`, and `thereis` is deprecated.
  These commands will be changed to have call argument lists more like
  accumulation commands, such as `(always [VAR] VAL &key into)`.  This will
  simplify the code and remove an inconsistency between them and the other
  commands.

- `:result-type` is deprecated.  This feature was taken from Common Lisp's
  Iterate.  While useful, this can be done more directly using named
  accumulation variables (such as `loopy-result`) in special macro arguments,
  such as `finally-return`.  Because of `accum-opt`, using named variables is
  more flexible.

  ```elisp
  ;; Can't be done using only `:result-type'.
  ;; => ([2 4 6] [3 6 9])
  (loopy (accum-opt doubles triples)
         (list i '(1 2 3))
         (collect doubles (* i 2))
         (collect triples (* i 3))
         (finally-return (cl-coerce doubles 'vector)
                         (cl-coerce triples 'vector)))
  ```

- `:init` is deprecated.  Some commands had special behavior with `:init`, such
  as `set-prev`, but this has been changed to work with `with` too.  Some
  iteration commands, such as `numbers`, change behavior based on whether
  a variable is `with` bound.  Removing `:init` increases consistency
  with these commands and decreases the duplication of features.
  - Relatedly, remove documentation that said `adjoin` supported `:init`.  It
    does not.

### Command Improvements

- To produce faster code, some commands now avoid creating an intermediate
  variable by initializing iteration variables to their first value.  This
  initialization can be controlled using the `with` special macro argument,
  which can result in slower code.  Previously, these iteration variables were
  always initialized to `nil` and updated to the first value at the location of
  the command.
  - `cycle` already has this behavior, but can now be slower.
  - `cons` will initialize to the list value.
  - `nums` and `seq-index` will initialize to the first numeric value.

- The behavior of `always`, `never`, and `thereis` has been slightly changed to
  be more convenient and consistent with other commands.
  - The commands now exit the loop without forcing a return value, which allows
    implicit return values to be finalized.
  - The commands now use variables to store the implicit return values of the
    loop, defaulting to `loopy-result` and which can be specified via `:into`,
    similar to accumulation commands.
    ```elisp
    ;; => "hello there"
    (loopy (list i '(1 1 1 1))
           ;; The return value of `(and (< i 2) "hello")' is "hello".
           (always (< i 2) "hello")
           (finally-return (concat loopy-result " there")))

    ;; => 7
    (loopy (list i '(nil nil 3 nil))
           (thereis i)
           (finally-return (+ loopy-result 4)))
    ```
  - As with other incompatible commands, an error is now signaled when trying to
    use `thereis` with `always` or `never` **when using the same variable**

### Other Changes

- Add `loopy--other-vars`, given the more explicit restriction on
  `loopy--iteration-vars`.  For example, these are the variables bound by the
  `set` command, which are allowed to occur in more than one command.

[#164]: https://github.com/okamsn/loopy/pull/164


## 0.11.2

### Bugs Fixed

- Fix `nunion` for appending onto the start of end-optimized lists, as in the
  below example.

  ```elisp
  ;; => (10 8 9 7 5 6 4 1 2 3)
  (loopy (accum-opt (coll end))
               (array i (vector (list 1 2 3) (list 1 2 3)
                                (list 4 5 6) (list 7 8 9)))
               (nunion coll (copy-sequence i) :at start)
               (nunion coll (mapcar #'1+ i) :at start)
               (finally-return coll))
  ```

## 0.11.1

Released 2023-03-13.

### Bugs Fixed

- Fix `nconc`-ing onto the non-optimized end of an optimized collection, as in
  the below example ([#157]).

  ```elisp
  ;; => ((3 4 1 2) (1 2 3 4))
  (loopy (accum-opt (opt-end end) (opt-start start))
         (list i '((1 2) (3 4)))
         (nconc opt-end (copy-sequence i) :at start)
         (nconc opt-start (copy-sequence i) :at end)
         (finally-return opt-end opt-start))
  ```

### Other Changes

- Added [Compat](https://github.com/emacs-compat/compat) as a dependency
  ([#152]).  This is required for generalized variables in property lists.  This
  feature was added in Emacs 28, which we had been creating if not found.  It is
  better to let Compat do this for us.

[#152]: https://github.com/okamsn/loopy/pull/152
[#157]: https://github.com/okamsn/loopy/pull/157

## 0.11.0

Released 2022-11-25.

### Bugs Fixed

- Signal an error when destructuring doesn't produce any bindings ([#117]).
- Destructured bindings that should have signaled an error now do. For example,
  values after `&rest` and `&key` are now better checked. See [#117].

### Breaking Changes

#### Removals

- Remove the variable `loopy-first-iteration`.  This was added to be more like
  CL's Iterate, but it has little utility and was not being created
  conditionally.  To achieve similar behavior, simply create a variable like in
  the below example.

  ``` elisp
  ;; => ((1) (2 3))
  (loopy (list i '(1 2 3))
         (set first-cycle t nil)
         (if first-cycle
             (collect first i)
           (collect rest i))
         (finally-return first rest))
  ```

- Remove obsolete aliases for the variables `loopy-aliases` and
  `loopy-command-parsers`.

#### Feature Deprecations and Renaming Variables

- The command `sub-loop` is deprecated.  Use the commands `loopy` or
  `loopy-iter` instead.  Currently, the `sub-loop` command, depending on the
  macro in which it's used, just behaves as one or the other.  See [#130] and
  [#127].

  The `sub-loop` command was added before the commands `loopy` and `loopy-iter`.
  Previously, it was not a fully featured loop like the macro `loopy`.  Now that
  sub-loops can use special macro arguments, and now that the macros `loopy` and
  `loopy-iter` both have command versions of themselves, we no longer need this
  command.

- The `split` flag is deprecated.  Use the more general and controllable
  special macro argument `accum-opt` instead.  See [#124] and [#131].

  The `split` flag makes all accumulation commands with an implicit variable use
  separate, optimized variables.  There is no way to use splitting with some
  commands and not others using the `split` flag alone, but this can
  equivalently be done using the `accum-opt` flag.  Since `split` is just a
  specific usage of `accum-opt`, it is fine to remove `split`.

- Renamed `loopy--accumulation-final-updates` to `loopy--vars-final-updates`.
  The old name is now an obsolete alias.  The feature still works the same,
  but has been generalized to support the `close` argument of the new `iter`
  command.  See PR [#135].

#### For improving `loopy-iter`

- `loopy-iter` has been changed to rely on Emacs's macro expansion facilities
  instead of trying to use custom code-walking functions.  This should make the
  macro more robust. See [#119].  User options were added, updated, or obsoleted
  to allow this.  See the manual for more details.

  - The flag `lax-naming` is deprecated.  Its behavior is now the default for
    command aliases listed in `loopy-iter-bare-commands`.

    ```elisp
    ;; No need for flag `lax-naming' anymore:
    (loopy-iter (listing i '(1 2 3))
                (collecting i))
    ```

  - The user option `loopy-ignored-names` is now deprecated.  Instead of an
    exclusive approach, it is replaced by an inclusive approach using the new
    user options `loopy-iter-bare-commands` and
    `loopy-iter-bare-special-marco-arguments`.

  - Using keywords will continue to work.  See the variable
    `loopy-iter-keywords`, renamed from `loopy-iter-command-keywords`.  The
    old name is now an obsolete alias.  These keywords now work for special
    macro arguments too.

  - This change required better suppressing some macros, now in
    `loopy-iter-suppressed-macros`.  Defaults include `cl-return`, `cl-block`,
    and `cl-return-from`.

### Other Changes

- Improvements to destructuring ([#117]):
  - Document ignoring variables in the Org doc.  This feature has existed for a
    while.
  - Document `&keys` as an alternative to `&key`.  This feature has existed for
    a while.
  - Better skip ignored variables.
  - Add slight optimizations for common uses, such as for `(car . _)` and
    `(_ . cdr)`.

#### New Commands

- Added generic accumulation command `set-accum`.  This command is an
  accumulating version of `set`.  It is a generalization of the other generic
  accumulation commands `accumulate` and `reduce`.  See [#125] and [#129].

- Add the command `iter` for iterating through iterator objects, such as
  those created by using the output of functions defined with `iter-lambda`.
  This is not to be confused with the macro `loopy-iter`, which is named for
  Common Lisp's `iter` macro. See issues [#134] and [#136] and PR [#135].

#### Aliases

- Added the remaining present participle aliases to `loopy` and `loopy-iter`.
  They are now the default bare forms (see breaking changes above) for
  `loopy-iter`.  See [#119].

- Present-participle aliases (the "-ing" form) have been added for more
  commands, such as `listing` for `list` and `setting` for `set`.  They already
  existed for accumulation commands, such as `collecting` for `collect`.  See
  [#118] and [#104].

  - Aliases from `cl-loop`, such as `across` for the command `array` and `in`
    for the command `list`, have been de-emphasized.  The present-participle
    aliases should be used when not using the keywords in `loopy-iter`.

    ```elisp
    ;; => ((1 . 6) (2 . 7) (3 . 8) (4 . 9))
    (loopy-iter (listing i '(1 2 3 4))
                (for list j '(6 7 8 9))
                (collecting (cons i j)))
    ```

#### Other Changes to `loopy-iter`

- `loopy-iter` can now use keywords for naming special macro arguments, as done
  with commands.  This can help to avoid naming conflicts.  Added the keyword
  `arg`.  See [#119].

  ```elisp
  ;; No more conflict with special form `let*':
  (loopy-iter (arg let* (a 7))
              (returning a))
  ```


- Added `loopy-iter-suppressed-macros` (see breaking changes above).  See
  [#119].

- The new special macro argument `named` was added.  It is another way of
  specifying loop names, instead of just listing a symbol.  This might be useful
  in `loopy-iter`.  See issue [#123] and PR [#132].


[#104]: https://github.com/okamsn/loopy/issues/104
[#117]: https://github.com/okamsn/loopy/pull/117
[#118]: https://github.com/okamsn/loopy/pull/118
[#119]: https://github.com/okamsn/loopy/pull/119
[#123]: https://github.com/okamsn/loopy/issues/123
[#124]: https://github.com/okamsn/loopy/issues/124
[#125]: https://github.com/okamsn/loopy/issues/125
[#127]: https://github.com/okamsn/loopy/issues/127
[#129]: https://github.com/okamsn/loopy/pull/129
[#130]: https://github.com/okamsn/loopy/pull/130
[#131]: https://github.com/okamsn/loopy/pull/131
[#132]: https://github.com/okamsn/loopy/pull/132
[#134]: https://github.com/okamsn/loopy/issues/134
[#135]: https://github.com/okamsn/loopy/pull/135
[#136]: https://github.com/okamsn/loopy/issues/136


## 0.10.1

Released 2022-03-29.

### Bugs Fixed

- Fix badly formed condition case in `append` command.  This bug does not affect
  properly written loops, just how errors are signaled.  This bug was introduced
  in version 0.9.1.
- Better check final updates (such as from `:result-type`) when no final update
  requested.  For example using both `(collect i)` and `(collect i :result-type
  vector)` in the same loop would previously convert `loopy-result` to a vector,
  but now raises an error to note the discrepancy.
- In `loopy-dash.el`, fix using quoted symbols as keys for destructuring
  accumulations ([#101], [#102]).  The new approach should be more reliable,
  though might create more variables than strictly needed.

### Breaking Changes
- The default test function for commands like `adjoin` and `union` is now
  `equal`.  Previously, `loopy` copied `cl-loop` and used `eql`, but it makes
  more sense to copy the behavior of other Emacs Lisp libraries (like `seq.el`)
  than to copy the behavior of Common Lisp.
- The `map`command now filters out duplicate keys by default.  `map-ref` already 
  did this. The behavior is now optional in both commands using the `unique` 
  keyword argument.
- __The way to add aliases has changed.__ ([#100], [#105])
  - `loopy-command-aliases` was renamed to `loopy-aliases`.  The old name is
    obsolete.
  - The structure of `loopy-aliases` changed.  For forward compatibility,
    aliases should only be defined using `loopy-defalias`.
  - Recursive definitions are now resolved at definition.  This avoids needing
    to slowly search for many names for each expression passed to the macro,
    which was doubling the expansion time and complicating code.
- Add `loopy-iter-ignored-names`, which replaces `loopy-iter-ignored-commands`
  ([#100], [#105]).  This new variable also includes the aliases of special
  macro arguments, such as `let*` for `with`.  The name of the old variable is
  now an obsolete alias of the new variable, though note that the use is now
  more general.
- The variable names `loopy-custom-command-aliases` and
  `loopy-custom-command-parsers` are now obsolete.

### Other Changes

- Add the special macro argument `finally-protect`, which wraps part of the loop
  with `unwind-protect`.
- Clean up the Org documentation.
- Accumulations using implicit accumulation variables are now automatically
  optimized, without the need of the `split` flag.  Note that the `split` flag
  is still needed to prevent such commands accumulating into the same variable
  (`loopy-result`).  See [#89], [#96].
- Named accumulation variables can now be optimized using the `accum-opt`
  special macro argument, at the cost of not being correct during the loop.
  These are the same optimizations made for the implicit variables.
- The `repeat` command was renamed to `cycle`, with `repeat` as an alias.  This
  is a more accurate name, as an argument of 1 *cycles* the loop once but
  *repeats* the loop zero times.
- `set` is now the preferred name of the command named `expr`.  The name `expr`
  is now an alias of `set`, instead of the other way around.  For consistency,
  the preferred name of `prev-expr` is no `set-prev`.  `prev-expr` is now an
  alias of `set-prev`, which also gained the alias `prev-set`.

[#89]: https://github.com/okamsn/loopy/issues/89
[#96]: https://github.com/okamsn/loopy/pull/96
[#100]: https://github.com/okamsn/loopy/issues/100
[#101]: https://github.com/okamsn/loopy/issues/101
[#102]: https://github.com/okamsn/loopy/pull/102
[#105]: https://github.com/okamsn/loopy/pull/105

## 0.9.1

Released 2021-09-06.

### Breaking Changes

- Accumulation variables can no longer be edited outside of accumulation
  commands while the loop is running.  This allows the loop to be much faster
  when using named variables (required for destructuring) and simplifies the
  code.  See [#83].
- Duplicating special macro arguments (such as using `flags` twice) now signals
  an error.  See [#87].
- Sub-loops using the `sub-loop` command are now full `loopy` loops.
  - They can now take special macro arguments.
  - Their accumulation no longer affect the surrounding loop.  You must now use
    the `at` command.  See [#70], [#87].

### Other Changes

- An `at` command was added, which better controls interacting with surrounding
  loops.  This is similar to the `in` clause from Common Lisp's Iterate package.
  See [#70], [#87].
- The `leave-from` command has been re-added, now that code generation is more
  robust.  See [#87].
- A `skip-from` command was added.  See [#87].
- Commands `loopy` and `loopy-iter` were added.  See [#87].
- Add feature `loopy-lambda`.  This is like `pcase-lambda`, but uses `loopy`
  destructuring.  See [#87].
- List `seq.el` in the package requirements.  See [#87].


[#70]: https://github.com/okamsn/loopy/issues/70
[#83]: https://github.com/okamsn/loopy/issues/83
[#87]: https://github.com/okamsn/loopy/pull/87

## 0.8.1

Released 2021-08-28.

### Breaking Changes

- Instructions are no longer dotted pairs (such as `(A . B)`), but normal lists
  (such as `(A B)`).  For custom commands, simply switch the dotted pairs to
  un-dotted pairs.

### Bugs Fixed

- Fix parsing error in `expr` command when redundantly passing nil to `:init`.
  Previously, the literal symbol `:init` would be the last value assigned to the
  variable, instead of correctly stopping at the expression before the symbol.

### Other Changes

- Add the command `map-ref`, similar to `list-ref` and `seq-ref`.
- The accumulation commands `reduce` and `accumulate` have gained the aliases
  `callf` and `callf2`, respectively.  This is intended to help users remember
  the order of arguments.  Unlike the `callf*` macros from which these aliases
  derive, the function argument to these commands is the third argument and must
  be quoted.
- The default destructuring system now takes on some of the features of Emacs
  `cl-lib` destructuring functions.  The special arguments `&key`, `&whole`, and
  `&rest` were added in ways that make sense for Loopy.  Because the functions
  in `seq.el` can be extended for new types, this addition only makes
  `loopy-seq.el` redundant for the destructuring of lists and arrays.
  - See also the new features `loopy-dsetq`, `loopy-let*`, and `loopy-ref`,
    expose these destructuring features for uses outside of the loop.
  - The `setf`-able places using `&key` rely on using `gv.el` for `plist-get`.
    This feature was added to Emacs's master branch in September 2020, and its
    definition is copied into `loopy-misc.el`, which currently contains the
    destructuring functions.  On Emacs versions less than 28, that definition
    will be loaded if an existing setter function is not detected.

## 0.7.2

Released 2021-06-30.

### Bugs Fixed

- Fix understanding the arguments in `find` when using the `:into` keyword
  argument.  More tests were added.

### Other Changes

- Add alias `map-pairs` for `map`, since `map` uses `map-pairs` underneath.
- Small corrections to Org/Info documentation.

## 0.7.1

Released 2021-06-28.

### Breaking Changes

- Previously, some commands (`accumulate`, `cons`) required using a function
  symbol (quoted or not) for their function parameter.  This prevented passing
  variables containing function symbols as the argument.

  This behavior has been changed so that function symbols must now be quoted (no
  Issue or PR).  Unquoted symbol arguments will now be understood to be
  variables.

  This change only applies to function parameters.  Arguments like `end` for
  `collect`'s position parameter can still be unquoted.

- `append` now appends to the ends of explicit accumulation variables
  without copying first.  It is now much faster. [#78]

- The accumulation commands which build lists (`adjoin`, `append`, `collect`,
  `union`, `nonc`, `nunion`) are now more correct, at the cost of some speed
  ([#78]).
  - When adding the end of the list, variables explicitly named can now be
    better modified during the loop.
  - Accumulation commands can now better modify the end of the list
    `loopy-result`.  Using implicit variables is still faster than using
    explicit variables.
  - Accumulation loop commands are now even more efficient when using the
    `split` flag.  This is the fastest method to use build a result.

- Explicit accumulation variables are no longer used as implicit return values
  for the macro.  This is needed for more consistently handling the complexity
  that comes from allowing the use of Pcase's arbitrary destructuring macros.
  This is related to, but not a direct fix for, [#43].

- The command `map` now iterates through dotted pairs (via `map-pairs`) instead
  of undotted pairs (via `map-apply`).  This brings `loopy` more in line with
  other packages, better meeting user expectations.

### Bugs Fixed

- Correctly `let`-bind `loopy--destructuring-for-iteration-function` (no PR or
  Issue).  Previously, some destructuring settings would affect the next macro
  run.

- Split accumulation values are correctly gathered into `loopy-result` when the
  special macro arguments `after-do`, `finally-do`, or `finally-return` are
  used.

### Other Changes

- A more informative error is signaled when incompatible accumulation commands
  are used ([#78]), showing the commands themselves.  This addresses [#45].
- Update some of the user options ([#49]):
  - `loopy-custom-command-aliases` is now an alias of `loopy-command-aliases`.
  - `loopy-custom-command-parsers` is now an alias of `loopy-command-parsers`.
  - User aliases can now be recursive (i.e., an alias of an alias).
- Add the special macro argument `wrap`, which can wrap parts of the loop
  ([#68]).

[#43]: https://github.com/okamsn/loopy/issues/43
[#45]: https://github.com/okamsn/loopy/issues/45
[#49]: https://github.com/okamsn/loopy/issues/49
[#68]: https://github.com/okamsn/loopy/pull/68
[#78]: https://github.com/okamsn/loopy/pull/78

## 0.6.1

Released 2021-06-09.

### Breaking Changes

- Commands were updated to include keyword options more like those of `cl-loop`
  and `iterate`, such as the keywords `from`, `upfrom`, `downfrom`, `to`,
  `upto`, `downto`, `above`, `below`, `by`. ([#73], [#65])

  - List commands like `list` and `list-ref` only received `by`, which replaces
    the optional third argument `FUNC`.
  - `seq` and `array` (and their variants) received the above list and the
    keyword `index`, which names a variable to hold the index.

- The commands `list`, `array`, and `seq` can now take multiple sequences before
  keyword arguments, in which case the elements of those sequences are
  distributed.  See the documentation for more info. ([#73], [#65])

### Non-Breaking Changes

- The positional arguments of the `nums` command are now optional, and can be
  supplemented with keyword from the list above. ([#73])
- Re-arrange documentation.

### Bug Fixes

- Fix using variables for the new `by` argument of the commands `list-ref`,
  `list`, and `cons`.  This argument can now be a variable, which means that
  references to function symbols must now be quoted ([#73]).


[#65]: https://github.com/okamsn/loopy/issues/65
[#73]: https://github.com/okamsn/loopy/pull/73
