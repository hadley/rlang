
# rlang 0.1.4.9000

* `new_cnd()` is now `cnd()` for consistency with other constructors.
  Also, `cnd_error()`, `cnd_warning()` and `cnd_message()` are now
  `error_cnd()`, `warning_cnd()` and `message_cnd()` to follow our
  naming scheme according to which the type of output is a suffix
  rather than a prefix.

* Condition signallers such as `cnd_signal()` and `abort()` now accept
  a call depth as `call` arguments. This allows plucking a call from
  further up the call stack (#30).

* `cnd_signal()` now returns invisibly.

* `cnd_signal()` and `cnd_abort()` now accept character vectors to
  create typed conditions with several S3 subclasses.

* `is_condition()` is now properly exported.

* New `env_set()` function to set a value in an environment or a
  scope. If the `create` argument is `FALSE`, it only overwrites
  existing bindings and issues an error otherwise (#162).

* New `fn_fmls<-` and `fn_fmls_names<-` setters.

* `ensym()` is a new variant of `enexpr()` that expects a symbol or a
  string and always returns a symbol. If a complex expression is
  supplied it fails with an error.

* New function `chr_translate_unicode()` for turning characters
  serialised to unicode point form (e.g. `<U+xxxx>`) to UTF-8. In
  addition, `as_utf8_character()` now translates those as well.

* When nested quosures are evaluated with `eval_tidy()`, the `.env`
  pronoun now correctly refers to the current quosure under evaluation
  (#174). Previously it would always refer to the environment of the
  outermost quosure.

* The new functions `cnd_warn()` and `cnd_inform()` transform
  conditions to warnings or messages before signalling them.

* New helper `catch_cnd()`. This is a small wrapper around
  `tryCatch()` that captures and returns any signalled condition. It
  returns `NULL` if none was signalled.

* `cnd_abort()` now adds the correct S3 classes for error
  conditions. This fixes error catching, for instance by
  `testthat::expect_error()`.

* `is_namespace()` is a snake case wrapper around `isNamespace()`.

* `new_fn()` takes a function and creates an object of class `fn`.
  The `fn` print method strips attributes before printing. The `fn`
  class is thus convenient to augment functions with attributes with a
  nicer print method.

* `env_get_list()` retrieves muliple bindings from an environment into
  a named list.

* `with_bindings()` and `scoped_bindings()` establish temporary
  bindings in an environment.

* New API for changing global options: `peek_options()` and
  `peek_option()` examine options; `push_options()` changes options
  indefinitely; `scoped_options()` and `with_options()` change options
  temporarily.

* The tidy eval `!!` operator now binds tightly. You no longer have to
  wrap it in parentheses, i.e. `!! x > y` will only unquote `x`.

  Technically the `!!` operator has the same precedence as unary `-`
  and `+`. This means that `!! a:b` and `!! a + b` are equivalent to
  `(!! a):b` and `(!! a) + b`. On the other hand `!! a^b` and `!! a$b`
  are equivalent to`!! (a^b)` and `!! (a$b)`.

* `!!!` now accepts any kind of objects for consistency. Scalar types
  are treated as vectors of length 1. Previously only symbolic objects
  like symbols and calls were treated as such in order to allow
  splicing of function bodies (which are not necessarily wrapped in a
  `{` block).

* `exprs()` and `quos()` gain a `.unquote_names` arguments to switch
  off interpretation of `:=` as a name operator. This should be useful
  for programming on the language targetting APIs such as
  data.table. For consistency `dots_list()` and `dots_splice()` gain
  that argument as well.

* The backend for `quos()`, `exprs()`, `dots_list()`, etc is now
  written in C. This greatly improve the performance of dots capture,
  especially with the splicing operator `!!!` which now scales much
  better (you'll see a 1000x performance gain in some cases). The
  unquoting algorithm has also been improved which makes `enexpr()`
  and `enquo()` more efficient as well.

* `enquo()` and `enexpr()` now deal with default values correctly (#201).

* Functions taking dots by value rather than by expression
  (e.g. regular functions, not quoting functions) have a more
  restricted set of unquoting operations. They only support `:=` and
  `!!!`, and only at top-level. I.e. `dots_list(!!! x)` is valid but
  not `dots_list(deep(!!! x))` (#217).

* Functions taking dots by value now support splicing a `NULL`
  value. `dots_list(!!! NULL)` is equivalent to `dots_list()` (#242).

* `exprs()` gains a `.named` option to auto-label its arguments (#267).

* Splicing a list no longer mutates it (#280).

* Capture operators now support evaluated arguments. Capturing a
  forced or evaluated argument is exactly the same as unquoting that
  argument: the actual object (even if a vector) is inlined in the
  expression. Capturing a forced argument occurs when you use
  `enquo()`, `enexpr()`, etc too late. It also happens when your
  quoting function is supplied to `lapply()` or when you try to quote
  the first argument of an S3 method (which is necessarily evaluated
  in order to detect which class to dispatch to). (#295, #300).

* Parentheses around `!!` are automatically removed. This makes the
  generated expression call cleaner: `(!! sym("name"))(arg)`. Note
  that removing the parentheses will never affect the actual
  precedence within the expression as the parentheses are only useful
  when parsing code as text. The parentheses will also be added by R
  when printing code if needed (#296).

* The print method for quosures now gives a cleaner output. Quosures
  are printed as a flat expression with the environment printed below.
  Previously they would appear as formulas.

* `expr_label()` now supports quoted function definition calls (#275).

* `is_symbol()` gains a `name` argument to check that that the symbol
  name matches a string (#287).

* New `rlang_box` class. Its purpose is similar to the `AsIs` class
  from `base::I()`, i.e. it protects a value temporarily. However it
  does so by wrapping the value in a scalar list. Use `new_box()` to
  create a boxed value, `is_box()` to test for a boxed value, and
  `unbox()` to unbox it. `new_box()` and `is_box()` accept optional
  subclass.

* New functions `inherits_any()`, `inherits_all()`, and
  `inherits_only()`. They allow testing for inheritance from multiple
  classes. The `_any` variant is equivalent to `base::inherits()` but
  is more explicit about its behaviour. `inherits_all()` checks that
  all classes are present in order and `inherits_only()` checks that
  the class vectors are identical.

* `UQ()` and `UQS()` are soft-deprecated in order to make the syntax
  of quasiquotation more consistent. The prefix forms are now
  `` `!!`() `` and `` `!!!`() `` which is consistent with other R
  operators (e.g. `` `+`(a, b) `` is the prefix form of `a + b`).

  Note that the prefix forms are not as relevant as before because
  `!!` now has the right operator precedence, i.e. the same as unary
  `-` or `+`. It is thus safe to mingle it with other operators,
  e.g. `!!a + !!b` does the right thing. In addition the parser now
  strips one level of parentheses around unquoted expressions. This
  way `(!!"foo")(...)` expands to `foo(...)`.  These changes make the
  prefix forms (and thus the named functional forms `UQ()` and
  `UQS()`) less useful.

  Finally, the named functional forms `UQ()` and `UQS()` were
  misleading because they suggested that existing knowledge about
  functions is applicable to quasiquotation. This was reinforced by
  the visible definitions of these functions exported by rlang and by
  the tidy eval parser interpreting `rlang::UQ()` as `!!`. In reality
  unquoting is *not* a function call, it is a syntactic operation. The
  operator forms `!!` and `!!!` make it clearer that unquoting is
  special.

* The quasiquotation parser now gives meaningful errors in corner
  cases to help you figure out what is wrong.

* `dots_formulas()` and `dots_patterns()` are like
  `dots_definitions()`. The former supports only two-sided formulas
  while the latter accepts both two-sided formulas and `:=`
  definitions.


## Breaking changes

* `is_node()` now returns `TRUE` for calls as well and `is_pairlist()`
  does not return `TRUE` for `NULL` objects. Use `is_node_list()` to
  determine whether an object either of type `pairlist` or `NULL`.

* `!!` now binds tightly in order to match intuitive parsing of tidy
  eval code, e.g. `!! x > y` is now equivalent to `(!! x) > y`.  A
  corollary of this new syntax is that you now have to be explicit
  when you want to unquote the whole expression on the right of `!!`.
  For instance you have to explicitly write `!! (x > y)` to unquote
  `x > y` rather than just `x`.

* `expr_interp()` now returns a formula instead of a quosure when
  supplied a formula.

* `:=` now issues an error when called directly. It previously was an
  alias for `~` to allow calling it directly. This caused surprising
  results when it was invoked in wrong places.

* The prefix form `` `!!`() `` is now an alias to `!!` rather than
  `UQE()`. This makes it more in line with regular R syntax where
  operators are parsed as regular calls, e.g. `a + b` is parsed as ``
  `+`(a, b) `` and both forms are completely equivalent. Also the
  prefix form `` `!!!`() `` is now equivalent to `!!!`.

* `UQE()` is now deprecated in order to simplify the syntax of
  quasiquotation. Please use `!! get_expr(x)` instead.

* `dots_definitions()` now only supports `:=` inputs. The return value
  has been simplified accordingly and is now a simple list of `:=`
  components for each input.


# rlang 0.1.4

* `eval_tidy()` no longer maps over lists but returns them literally.
  This behaviour is an overlook from past refactorings and was never
  documented.


# rlang 0.1.2

This hotfix release makes rlang compatible with the R 3.1 branch.


# rlang 0.1.1

This release includes two important fixes for tidy evaluation:

* Bare formulas are now evaluated in the correct environment in
  tidyeval functions.

* `enquo()` now works properly within compiled functions. Before this
  release, constants optimised by the bytecode compiler couldn't be
  enquoted.


## New functions:

* The `new_environment()` constructor creates a child of the empty
  environment and takes an optional named list of data to populate it.
  Compared to `env()` and `child_env()`, it is meant to create
  environments as data structures rather than as part of a scope
  hierarchy.

* The `new_language()` constructor creates calls out of a callable
  object (a function or an expression) and a pairlist of arguments. It
  is useful to avoid costly internal coercions between lists and
  pairlists of arguments.


## UI improvements:

* `env_child()`'s first argument is now `.parent` instead of `parent`.

* `mut_` setters like `mut_attrs()` and environment helpers like
  `env_bind()` and `env_unbind()` now return their (modified) input
  invisibly. This follows the tidyverse convention that functions
  called primarily for their side effects should return their input
  invisibly.

* `is_pairlist()` now returns `TRUE` for `NULL`. We added `is_node()`
  to test for actual pairlist nodes. In other words, `is_pairlist()`
  tests for the data structure while `is_node()` tests for the type.


## Bugfixes:

* `env()` and `env_child()` can now get arguments whose names start
  with `.`.  Prior to this fix, these arguments were partial-matching
  on `env_bind()`'s `.env` argument.

* The internal `replace_na()` symbol was renamed to avoid a collision
  with an exported function in tidyverse. This solves an issue
  occurring in old versions of R prior to 3.3.2 (#133).


# rlang 0.1.0

Initial release.
