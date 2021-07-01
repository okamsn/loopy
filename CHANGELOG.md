# CHANGELOG

This document describes the user-facing changes to Loopy.

## 0.7.2

### Bugs Fixed

- Fix understanding the arguments in `find` when using the `:into` keyword
  argument.  More tests were added.

### Other Changes

- Add alias `map-pairs` for `map`, since `map` uses `map-pairs` underneath.
- Small corrections to Org/Info documentation.

## 0.7.1

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

[#43]: https://github.com/okamsn/loopy/issues/43
[#45]: https://github.com/okamsn/loopy/issues/45
[#49]: https://github.com/okamsn/loopy/issues/49
[#78]: https://github.com/okamsn/loopy/pull/78

## 0.6.1

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
