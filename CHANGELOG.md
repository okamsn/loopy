# CHANGELOG

This document describes the user-facing changes to Loopy.

## Unreleased

## 0.6.1

### Breaking Changes

- Previously, some commands (`accumulate`, `cons`) required using a function
  symbol (quoted or not) for their function parameter.  This prevented passing
  variables containing function symbols as the argument.

  This behavior has been changed so that function symbols must now be quoted.
  Unquoted symbol arguments will now be understood to be variables.

  This change only applies to function parameters.  Arguments like `end` for
  `collect`'s position parameter can still be unquoted.

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
