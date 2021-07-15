#' Match an argument to a character vector
#'
#' @description
#'
#' This is equivalent to [base::match.arg()] with a few differences:
#'
#' * Partial matches trigger an error.
#'
#' * Error messages are a bit more informative and obey the tidyverse
#'   standards.
#'
#' `arg_match()` derives the possible values from the
#' [caller frame][caller_frame].
#'
#' @param arg A symbol referring to an argument accepting strings.
#' @return The string supplied to `arg`.
#' @importFrom utils adist
#' @seealso [arg_require()]
#' @export
#' @examples
#' fn <- function(x = c("foo", "bar")) arg_match(x)
#' fn("bar")
#'
#' # Throws an informative error for mismatches:
#' try(fn("b"))
#' try(fn("baz"))
arg_match <- function(arg, values = NULL) {
  arg_expr <- enexpr(arg)
  if (!is_symbol(arg_expr)) {
    abort("Internal error: `arg_match()` expects a symbol")
  }

  arg_nm <- as_string(arg_expr)

  if (is_null(values)) {
    fn <- caller_fn()
    values <- fn_fmls(fn)[[arg_nm]]
    values <- eval_bare(values, get_env(fn))
  }
  if (!is_character(arg)) {
    abort(sprintf("%s must be a character vector.", format_arg(arg_nm)))
  }
  if (length(arg) > 1 && !setequal(arg, values)) {
    abort(arg_match_invalid_msg(arg, values, arg_nm))
  }

  arg <- arg[[1]]
  arg_match0(arg, values, arg_nm)
}

#' @description
#' `arg_match0()` is a bare-bones version if performance is at a premium.
#' It requires a string as `arg` and explicit character `values`.
#' For convenience, `arg` may also be a character vector containing
#' every element of `values`, possibly permuted.
#' In this case, the first element of `arg` is used.
#'
#' @param values A character vector of possible values that `arg` can take.
#' @param arg_nm Argument name of `arg` to use in error messages. Can
#'   be a string or symbol.
#' @rdname arg_match
#' @export
#' @examples
#'
#' # Use the bare-bones version with explicit values for speed:
#' arg_match0("bar", c("foo", "bar", "baz"))
#'
#' # For convenience:
#' fn1 <- function(x = c("bar", "baz", "foo")) fn3(x)
#' fn2 <- function(x = c("baz", "bar", "foo")) fn3(x)
#' fn3 <- function(x) arg_match0(x, c("foo", "bar", "baz"))
#' fn1()
#' fn2("bar")
#' try(fn3("zoo"))
arg_match0 <- function(arg, values, arg_nm = substitute(arg)) {
  .External(ffi_arg_match0, arg, values, arg_nm)
}

stop_arg_match <- function(arg, values, arg_nm) {
  msg <- arg_match_invalid_msg(arg, values, arg_nm)

  # Try suggest the most probable and helpful candidate value
  candidate <- NULL
  i_partial <- pmatch(arg, values)
  if (!is_na(i_partial)) {
    candidate <- values[[i_partial]]
  }

  i_close <- adist(arg, values) / nchar(values)
  if (any(i_close <= 0.5)) {
    candidate <- values[[which.min(i_close)]]
  }

  if (is_null(candidate)) {
    # Make case-insensitive match only after failed case-sensitive one to be
    # more helpful in certain edge cases. For example,
    # `arg_match0("aa", c("AA", "aA"))`: here "aA" is the closest candidate.
    i_close_nocase <- adist(arg, values, ignore.case = TRUE) / nchar(values)
    if (any(i_close_nocase <= 0.5)) {
      candidate <- values[[which.min(i_close_nocase)]]
    }
  }

  if (!is_null(candidate)) {
    candidate <- chr_quoted(candidate, "\"")
    msg <- c(msg, i = paste0("Did you mean ", candidate, "?"))
  }

  abort(msg)
}

arg_match_invalid_msg <- function(arg, values, arg_nm) {
  msg <- paste0(format_arg(arg_nm), " must be one of ")
  msg <- paste0(msg, chr_enumerate(chr_quoted(values, "\"")))

  if (is_null(arg)) {
    msg <- paste0(msg, ".")
  } else {
    msg <- paste0(msg, sprintf(', not "%s\".', arg))
  }

  msg
}

#' Check that argument is supplied
#' Throws an informative error if `arg` is missing.
#' @param arg A function argument. Must be a symbol.
#' @seealso [arg_match()]
#' @examples
#' f <- function(x)  {
#'   arg_require(x)
#' }
#'
#' # Fails because `x` is not supplied
#' try(f())
#'
#' # Succeeds
#' f(NULL)
#' @export
arg_require <- function(arg) {
  if (!missing(arg)) {
    invisible(return(TRUE))
  }

  arg_expr <- substitute(arg)
  if (!is_symbol(arg_expr)) {
    abort("Internal error: `arg_require()` expects a symbol.")
  }
  arg <- as_string(arg_expr)

  call <- sys.calls()[[sys.parent()]]
  if (is_call(call) && is_symbol(call[[1]])) {
    fn <- as_string(call[[1]])
    msg <- sprintf(
      "%s requires the argument %s to be supplied.",
      mark_fn(fn),
      mark_arg(arg)
    )
  } else {
    msg <- sprintf(
      "The argument %s must be supplied.",
      mark_arg(arg)
    )
  }

  abort(format_error(msg))
}

chr_quoted <- function(chr, type = "`") {
  paste0(type, chr, type)
}
chr_enumerate <- function(chr, sep = ", ", final = "or") {
  n <- length(chr)

  if (n < 2) {
    return(chr)
  }

  n <- length(chr)
  head <- chr[seq_len(n - 1)]
  last <- chr[length(chr)]

  head <- paste(head, collapse = sep)

  # Write a or b. But a, b, or c.
  if (n > 2) {
    paste0(head, sep, final, " ", last)
  } else {
    paste0(head, " ", final, " ", last)
  }
}

#' Generate or handle a missing argument
#'
#' @description
#'
#' These functions help using the missing argument as a regular R
#' object.
#'
#' * `missing_arg()` generates a missing argument.
#'
#' * `is_missing()` is like [base::missing()] but also supports
#'   testing for missing arguments contained in other objects like
#'   lists.
#'
#' * `maybe_missing()` is useful to pass down an input that might be
#'   missing to another function, potentially substituting by a
#'   default value. It avoids triggering an "argument is missing" error.
#'
#'
#' @section Other ways to reify the missing argument:
#'
#' * `base::quote(expr = )` is the canonical way to create a missing
#'   argument object.
#'
#' * `expr()` called without argument creates a missing argument.
#'
#' * `quo()` called without argument creates an empty quosure, i.e. a
#'   quosure containing the missing argument object.
#'
#'
#' @section Fragility of the missing argument object:
#'
#' The missing argument is an object that triggers an error if and
#' only if it is the result of evaluating a symbol. No error is
#' produced when a function call evaluates to the missing argument
#' object. This means that expressions like `x[[1]] <- missing_arg()`
#' are perfectly safe. Likewise, `x[[1]]` is safe even if the result
#' is the missing object.
#'
#' However, as soon as the missing argument is passed down between
#' functions through an argument, you're at risk of triggering a
#' missing error. This is because arguments are passed through
#' symbols. To work around this, `is_missing()` and `maybe_missing(x)`
#' use a bit of magic to determine if the input is the missing
#' argument without triggering a missing error.
#'
#' `maybe_missing()` is particularly useful for prototyping
#' meta-programming algorithms in R. The missing argument is a likely
#' input when computing on the language because it is a standard
#' object in formals lists. While C functions are always allowed to
#' return the missing argument and pass it to other C functions, this
#' is not the case on the R side. If you're implementing your
#' meta-programming algorithm in R, use `maybe_missing()` when an
#' input might be the missing argument object.
#'
#'
#' @section Life cycle:
#'
#' * `missing_arg()` and `is_missing()` are stable.
#' * Like the rest of rlang, `maybe_missing()` is maturing.
#'
#' @param x An object that might be the missing argument.
#' @export
#' @examples
#' # The missing argument usually arises inside a function when the
#' # user omits an argument that does not have a default:
#' fn <- function(x) is_missing(x)
#' fn()
#'
#' # Creating a missing argument can also be useful to generate calls
#' args <- list(1, missing_arg(), 3, missing_arg())
#' quo(fn(!!! args))
#'
#' # Other ways to create that object include:
#' quote(expr = )
#' expr()
#'
#' # It is perfectly valid to generate and assign the missing
#' # argument in a list.
#' x <- missing_arg()
#' l <- list(missing_arg())
#'
#' # Just don't evaluate a symbol that contains the empty argument.
#' # Evaluating the object `x` that we created above would trigger an
#' # error.
#' # x  # Not run
#'
#' # On the other hand accessing a missing argument contained in a
#' # list does not trigger an error because subsetting is a function
#' # call:
#' l[[1]]
#' is.null(l[[1]])
#'
#' # In case you really need to access a symbol that might contain the
#' # empty argument object, use maybe_missing():
#' maybe_missing(x)
#' is.null(maybe_missing(x))
#' is_missing(maybe_missing(x))
#'
#'
#' # Note that base::missing() only works on symbols and does not
#' # support complex expressions. For this reason the following lines
#' # would throw an error:
#'
#' #> missing(missing_arg())
#' #> missing(l[[1]])
#'
#' # while is_missing() will work as expected:
#' is_missing(missing_arg())
#' is_missing(l[[1]])
missing_arg <- function() {
  .Call(ffi_missing_arg)
}

#' @rdname missing_arg
#' @export
is_missing <- function(x) {
  if (is_missing0(substitute(x), caller_env())) {
    TRUE
  } else {
    is_reference(x, quote(expr = ))
  }
}
is_missing0 <- function(arg, env) {
  is_symbol(arg) && inject(base::missing(!!arg), env)
}

#' @rdname missing_arg
#' @param default The object to return if the input is missing,
#'   defaults to `missing_arg()`.
#' @export
maybe_missing <- function(x, default = missing_arg()) {
  if (is_missing(x)) {
    default
  } else {
    x
  }
}
