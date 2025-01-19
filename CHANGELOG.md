# CHANGELOG

This document describes the user-facing changes to Loopy.

For Loopy Dash, see <https://github.com/okamsn/loopy-dash>.

## 0.14.0

### Commands for Generic (`seq.el`) Sequences

Loopy can now loop through generic sequences implemented by the library `seq.el`
([#215], [#150], [#136]).  Currently, this is done naively via the functions
`seq-elt` and `seq-length`.  Because a package might implement a generic
sequence using one of the built-in sequence types (lists and arrays), no attempt
is made to optimize behavior for particular kinds of sequences.  As a
comparison, the `sequence` command gives special consideration to lists in some
circumstances.

Because these commands use `seq-length`, they do not work with infinite
sequences.  For that, consider using the `stream` command.

The new commands are `seq` and `seq-ref`, which were previously aliases of
`sequence` and `sequence-ref`, respectively ([#126, #206]).  This change should
not cause an error, but the expanded code might be slower depending on the type
of the sequence.

`sequence-index`, which keeps the alias `seq-index`, has been changed to use
`seq-length` instead of `length`.  This command is simple enough that no special
version is needed for generic sequences.

### Breaking Changes

- Conflicting starting values for accumulation variables, such as from using
  `sum` (value: 0) and `multiply` (value: 1), now signal an error ([#169],
  [#203]).  In the future, they will signal an error.

  For now, Loopy continues the behavior of using the first found starting value
  from the macro arguments.  To silence this warning and to avoid this future
  error, use the `with` special macro argument to explicitly state a starting
  value for the accumulation variable.

- Remove the deprecated positional arguments to the `numbers` command ([#205]).

- To cut back on an over-abundance of choice and to simplify documentation, the
  following built-in aliases have been made obsolete ([#126], [#168], [#206],
  [#207]).  They can still be added manually via `loopy-defalias`.
  - `array`: `across`
  - `array-ref`: `arrayf`, `arrayingf`, `stringf`, `stringingf`, `across-ref`
  - `command-do`: `group`
  - `cons`: `on`
  - `list`: `in`
  - `list-ref`: `listf`, `listingf`, `in-ref`
  - `map-ref`: `mapf`, `mappingf`
  - `numbers`: `num`, `nums`
  - `numbers-down`: `nums-down`, `numdown`, `num-down`, `numsdown`
  - `numbers-up`: `nums-up`, `numup`, `num-up`, `numsup`
  - `set`: `expr`, `exprs`
  - `set-prev`: `prev`, `prev-expr`
  - `sequence`: `elements`
  - `sequence-index`: `sequencei`, `seqi`, `listi`, `arrayi`, `stringi`
  - `sequence-ref`: `sequencef`, `sequencingf`,
    `elements-ref`
  - `seq-ref`: `seqf` , `seqingf`

- Review when the values of keyword arguments are taken and used ([#210]):
  - Make the `close` keyword argument of the `iter` command able to be evaluated
    during run time ([#211]).  It was previously only used during macro expansion.
  - Make the `close` keyword argument of the `iter` command evaluated at the
    start of the loop to be consistent with other commands ([#211]).
  - Make the `on-failure` keyword argument of the `find` command evaluated at the
    start of the loop to be consistent with other commands ([#211]).
  - Allow the `unique` keyword argument of the commands `map` and `map-ref` to be
    evaluable at run time, instead of just checked at compile time ([#209]).

- Remove the initialization optimizations that produced faster code but
  could change final results ([#226, #204]).  For example, consider the
  difference results between `cl-loop` and SBCL's `loop`:

  ``` emacs-lisp
  ;; => (4 (1 2 3))
  (cl-loop for elem in (list 1 2 3)
           for i from 1
           collect i into is
           finally return (list i is))
  ```


  ``` common-lisp
  ;; => (3 (1 2 3))
  (loop for elem in (list 1 2 3)
        for num from 1
        collect num into nums
        finally (return (list num nums)))
  ```

  Loopy would give the same result as `cl-loop` when using the optimization and
  would given the same result as SBCL's `loop` when not using the optimization.

  Working around having different results based on unstated (though documented)
  settings would mean requiring users to fully know the implementations of each
  loop command and how they could change when optimized.  That position also
  argues against making use of the optimization more explicit via an added
  `iter-opt` special macro argument, as discussed in [#226] and [#204].

  Therefore, these optimizations are being removed and Loopy is reverting to its
  previous behavior of initializing the iteration variables to `nil` by default
  for the following commands:
  - `cons`
  - `cycle`
  - `iter`
  - `numbers`
  - `seq-index`
  - `substream`

### Improvements

- The `map` and `map-ref` commands now check for duplicate keys step by step,
  instead of all at once at the start of the loop ([#209], [#179]).  Testing
  showed that this is consistently faster than the old method.

- Recursive destructuring for generalized variables (`setf`-able places), such
  as in the below example, should now work as expected, due to a combination of
  custom GV setters and simplifying the produced code in some cases ([#212],
  [#213], [#184]).  This can sometimes result in redundant operations when
  setting the value of the generalized variable, but we've made an effort to
  reduce the number of occurences in the obvious cases.

  ```elisp
  ;; => [1 2 3 4 0 0 16]
  (let ((arr (vector 7 7 7 7 0 0 6)))
    (loopy-ref (([&seq a b c &rest d &map (3 sub-idx-3)] arr))
      (setf a 1 b 2 c 3 d [4])
      (cl-incf sub-idx-3 10))
    arr)
  ```

### Bug Fixes

- Correctly signal an error when attempting to destructure a stream for more
  values than it actually contains ([#217]).

[#126]: https://github.com/okamsn/loopy/issues/126
[#136]: https://github.com/okamsn/loopy/issues/136
[#150]: https://github.com/okamsn/loopy/issues/150
[#168]: https://github.com/okamsn/loopy/issues/168
[#169]: https://github.com/okamsn/loopy/issues/169
[#179]: https://github.com/okamsn/loopy/issues/179
[#184]: https://github.com/okamsn/loopy/issues/184
[#203]: https://github.com/okamsn/loopy/pull/203
[#204]: https://github.com/okamsn/loopy/issues/204
[#205]: https://github.com/okamsn/loopy/pull/205
[#206]: https://github.com/okamsn/loopy/pull/206
[#207]: https://github.com/okamsn/loopy/pull/207
[#209]: https://github.com/okamsn/loopy/pull/209
[#210]: https://github.com/okamsn/loopy/issues/210
[#211]: https://github.com/okamsn/loopy/pull/211
[#212]: https://github.com/okamsn/loopy/pull/212
[#213]: https://github.com/okamsn/loopy/pull/213
[#215]: https://github.com/okamsn/loopy/pull/215
[#217]: https://github.com/okamsn/loopy/pull/217
[#226]: https://github.com/okamsn/loopy/pull/226

## 0.13.0

### Breaking Changes

- The deprecated `:init` keyword argument has been removed ([#195], [#146]).
  Use the `with` special macro argument instead.

- The deprecated `:result-type` keyword argument has been removed ([#196],
  [#154]).  Use the `finally-return` special macro argument instead in
  combination with `cl-coerce`, `seq-into`, or a similar function.

- The commands `always`, `never`, and `thereis` now have the signature
  `(command [VAR] CONDITION &key into)`, similar to accumulation commands
  ([#197], [#145]).  These commands no longer take multiple conditions in the
  same command.

- The built-in destructuring system was switched to use `pcase` internally,
  so some warnings and errors are now reportedly differently ([#199]).

### Bugs Fixed

- Allow `back` in `set-prev` to not be known at compile time ([#202]).

### Documentation Improvements

- State explicitly that `set-prev` records values from the end
  of each loop cycle and that it does not modify its variable
  until the specified cycle ([#202]).

### Destructuring Improvements

- Added a `&seq` pattern, which can use `seq.el` ([#199]).

### Command Improvements

- Added new `substream` and `stream` commands, which works like `conses` but for
  steams as implemented by `stream.el` in ELPA ([#199]).


[#195]: https://github.com/okamsn/loopy/pull/195
[#196]: https://github.com/okamsn/loopy/pull/196
[#197]: https://github.com/okamsn/loopy/pull/197
[#199]: https://github.com/okamsn/loopy/pull/199
[#202]: https://github.com/okamsn/loopy/pull/202

## 0.12.2

- Correct the `.elpaignore` file.

## 0.12.1

- Increase version number to so that the ELPA process uses the `.elpaignore`
  file for the stable version of the package.

## 0.12.0

### Bugs Fixed

- Rewrite `prepend` in terms of `append`.  Rewrite `push-into` in terms of
  `collect` ([#160]).  This change makes these commands work with the new system for
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

- Fix `find` when `:on-failure` is nil ([#171]).  Previously, `nil` was
  interpreted as not passing `:on-failure`.

  ```emacs-lisp
   ;; Previously erroneously returned 27:
   ;; => nil
   (loopy (with (val 27))
          (list i '(1 2 3))
          (find val nil (> i 10) :on-failure nil)
          (finally-return val))
  ```

- Fix `find` when `EXPR` is nil and `:on-failure` is given ([#171]).
  Previously, after the test passed and `VAR` was set to `nil`, that `nil` was
  interpreted as not passing the test, so that `VAR` then bound to the value
  passed for `:on-failure`.

  ```emacs-lisp
  ;; Previously erroneously returned 27:
  ;; => nil
  (loopy (list i '(1 2 3))
         (find nil (> i 1) :on-failure 27))
  ```

- Better signal an error with conflicting arguments in `numbers`.  See [#172].

- Fix macro expansion in some cases by not resetting the macro environment
  ([#173]).  For example, we failed to pass the current/modified environment to
  `macroexpand-all` in some cases.

  ```emacs-lisp
  ;; Previously failed to use `cl-flet''s internally defined function
  ;; for `10+':
  ;; => (11 12 13 14 15)
  (loopy (named outer)
         (list i '((1 2) (3 4) (5)))
         (loopy-iter (listing j i)
                     (cl-flet ((10+ (y) (+ 10 y)))
                       (at outer
                           (collecting (10+ j))))))
  ```

- The documentation describes Loopy's default destructuring style as a super-set
  of that of `cl-lib`.  `&key` now behaves more like it does in `cl-lib`,
  signaling an error when appropriate ([#182]) and supporting the full form
  `((KEY VAR) DEFAULT SUPPLIED)`.

### Breaking Changes

- Fix how the first accumulated value is used in `reduce`.  See [#164] and the
  item above.

- Make it an error to re-use iteration variables with multiple iteration
  commands ([#142], [#144]).  The resulting code shouldn't have worked anyway,
  but we now report it as an error during macro expansion.

  ```elisp
  ;; Will now signal an error during expansion:
  (loopy (list i '(1 2 3))
         (list i '(4 5 6)))
  ```

- The keyword arguments of commands are now evaluated only once.  This is now
  consistent with passing function values of other loop commands.  If constant
  according to `macroexp-const-p`, then they are used directly.  Otherwise, the
  value is first stored in a variable.  See [#170], [#177], [#176], and [#180].

- In accumulation commands using the `test` keyword argument, the argument order
  of the two-argument test function is now document as `(SEQUENCE-ITEM,
  TESTED-ITEM)`, similar to `seq-contains-p`.  The argument order was previously
  undocumented and not guaranteed. See [#170] and [#177].

- Like in `cl-lib`, destructuring with `&key` will now signal an error if there
  are unmatched keys and `&allow-other-keys` was not given or
  `:allow-other-keys` is not present in the property list with a non-nil value
  ([#182]).

- The default destructuring style now uses `pcase` underneath ([#182]).  To
  accomodate this, some of the defined errors and error detections have changed.

#### Removals

- The deprecated flag `split` was removed ([#165], [#131], [#124]).  Instead,
  use named accumulation variables with the special macro argument `accum-opt`
  as appropriate.

- The deprecated flag `lax-naming` was removed ([#165], [#119]).  Instead, see
  `loopy-iter-bare-commands` and `loopy-iter-bare-special-macro-arguments`.

- The deprecated command `sub-loop` was removed ([#165], [#130], [#127]).  Use
  the commands/macros `loopy` and `loopy-iter` instead.  In `loopy`, `sub-loop`
  acted like `loopy`.  In `loopy-iter`, `sub-loop` acted like `loopy-iter`.

- The obsolete variable alias `loopy-iter-command-keywords` for the variable
  `loopy-iter-keywords` was removed ([#165], [#119]).


#### Deprecations

- Using multiple conditions in `always`, `never`, and `thereis` is deprecated
  ([#145], [#161]).  These commands will be changed to have call argument lists
  more like accumulation commands, such as `(always [VAR] VAL &key into)`.  This
  will simplify the code and remove an inconsistency between them and the other
  commands.

- `:result-type` is deprecated ([#154], [#162]).  This feature was taken from
  Common Lisp's Iterate.  While useful, this can be done more directly using
  named accumulation variables (such as `loopy-result`) in special macro
  arguments, such as `finally-return`.  Because of `accum-opt`, using named
  variables is more flexible.

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

- `:init` is deprecated ([#146], [#163]).  Some commands had special behavior with
  `:init`, such as `set-prev`, but this has been changed to work with `with`
  too.  Some iteration commands, such as `numbers`, change behavior based on
  whether a variable is `with` bound.  Removing `:init` increases consistency
  with these commands and decreases the duplication of features.
  - Relatedly, remove documentation that said `adjoin` supported `:init`.  It
    does not.

- The non-keyword arguments of `numbers` are deprecated ([#172]).  These
  arguments were meant to be similar to the arguments of Pythons `range`
  feature, but, depending on prior knowledge, would generally produce worse
  code.  Cases in which the direction of the iteration (up or down) is unknown
  can now be handled by the new `:test` argument, which is more flexible than
  the non-keyword arguments anyway.

### Command Improvements

- To produce faster code, some commands now avoid creating an intermediate
  variable by initializing iteration variables to their first value ([#142],
  [#144]).  This initialization can be controlled using the `with` special macro
  argument, which can result in slower code.  Previously, these iteration
  variables were always initialized to `nil` and updated to the first value at
  the location of the command.
  - `cycle` already has this behavior, but can now be slower.
  - `cons` will initialize to the list value.
  - `nums` and `seq-index` will initialize to the first numeric value.

- The behavior of `always`, `never`, and `thereis` has been slightly changed to
  be more convenient and consistent with other commands ([#144]).
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
    ([#144]).

- Add a `:test` keyword argument to `numbers`, `array`, `array-ref`, `sequence`,
  `sequence-ref`, and `sequence-index` ([#172], [#180]).  This is useful when
  the direction of the iteration is not known ahead of time.

  ```elisp
  ;; => (10 9.5 9.0 8.5 8.0 7.5 7.0 6.5 6.0 5.5)
  (loopy (with (start 10)
               (end 5)
               (func #'>)
               (step -0.5))
         (numbers i :to end :from start :by step :test func)
         (collect i))
  ```

- By using `macroexp-const-p`, Loopy now better uses constant values ([#176],
  [#180]).  Instead of always creating a variable in some cases, it now better
  uses the constant value directly, which Emacs can optimize to avoid some uses
  of `funcall`.

### Destructuring Improvements

- A `loopy` `pcase` pattern has been added ([#182]).  Destructuring is now based
  on `pcase`.
- A `&map` construct was added for destructuring, analogous to `&key` but using
  `map-elt` and the `map` `pcase` pattern ([#182]).  Extending the `map`
  pattern, `&map` also has a `SUPPLIED` parameter, as in `(KEY VAR DEFAULT
  SUPPLIED)`.
- An `&optional` construct was added, like in `cl-lib` ([#182]).
- `&key` now works like it does in `cl-lib`, including the `DEFAULT`,
  `SUPPLIED`, and `KEY` values in the full form and signaling an error
  when appropriate ([#182]).

### Other Changes

- Add `loopy--other-vars`, given the more explicit restriction on
  `loopy--iteration-vars` ([#144]).  For example, these are the variables bound
  by the `set` command, which are allowed to occur in more than one command.

- To reduce the maintenance burden, destructuring was re-implemented using
  `pcase` ([#182]).

[#144]: https://github.com/okamsn/loopy/issue/142
[#144]: https://github.com/okamsn/loopy/pull/144
[#145]: https://github.com/okamsn/loopy/issue/145
[#146]: https://github.com/okamsn/loopy/issue/146
[#154]: https://github.com/okamsn/loopy/issue/154
[#160]: https://github.com/okamsn/loopy/pull/160
[#161]: https://github.com/okamsn/loopy/pull/161
[#162]: https://github.com/okamsn/loopy/pull/162
[#163]: https://github.com/okamsn/loopy/pull/163
[#164]: https://github.com/okamsn/loopy/pull/164
[#165]: https://github.com/okamsn/loopy/pull/165
[#170]: https://github.com/okamsn/loopy/issues/170
[#171]: https://github.com/okamsn/loopy/pull/171
[#172]: https://github.com/okamsn/loopy/pull/172
[#173]: https://github.com/okamsn/loopy/pull/173
[#176]: https://github.com/okamsn/loopy/issues/176
[#177]: https://github.com/okamsn/loopy/pull/177
[#180]: https://github.com/okamsn/loopy/pull/180
[#182]: https://github.com/okamsn/loopy/pull/182

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
