# CHANGELOG

This document describes the user-facing changes to Loopy.

## Unreleased

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
