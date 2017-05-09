
# rlang 0.1.9000

* `env_child()`'s first argument is now `.parent` instead of `parent`.

* `env()` and `env_child()` can now get arguments starting with `.`.
  Prior to this fix, these arguments were partial-matching on
  `env_bind()`'s `.env` argument.

* `mut_` setters like `mut_attrs()` now return their (modified) input
  invisibly. This follows the tidyverse convention that functions
  called primarily for their side effects should return their input
  invisibly.


# rlang 0.1

Initial release.
