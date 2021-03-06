% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lifecycle-retired.R
\name{set_attrs}
\alias{set_attrs}
\alias{mut_attrs}
\title{Add attributes to an object}
\usage{
set_attrs(.x, ...)

mut_attrs(.x, ...)
}
\arguments{
\item{.x}{An object to decorate with attributes.}

\item{...}{<\link[=dyn-dots]{dynamic}> A list of named attributes. Pass
a single unnamed \code{NULL} argument to zap all attributes from \code{.x}.}
}
\value{
\code{set_attrs()} returns a modified \link[=duplicate]{shallow copy}
of \code{.x}. \code{mut_attrs()} invisibly returns the original \code{.x}
modified in place.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#deprecated}{\figure{lifecycle-deprecated.svg}{options: alt='[Deprecated]'}}}{\strong{[Deprecated]}}

\code{set_attrs()} adds, changes, or zaps attributes of objects. Pass a
single unnamed \code{NULL} argument to zap all attributes. For
\link[=is_copyable]{uncopyable} types, use \code{mut_attrs()}.
}
\details{
Unlike \code{\link[=structure]{structure()}}, these setters have no special handling of
internal attributes names like \code{.Dim}, \code{.Dimnames} or \code{.Names}.
}
\section{Life cycle}{


These functions are deprecated since rlang 0.3.0.
}

\examples{
set_attrs(letters, names = 1:26, class = "my_chr")

# Splice a list of attributes:
attrs <- list(attr = "attr", names = 1:26, class = "my_chr")
obj <- set_attrs(letters, splice(attrs))
obj

# Zap attributes by passing a single unnamed NULL argument:
set_attrs(obj, NULL)
set_attrs(obj, !!! list(NULL))

# Note that set_attrs() never modifies objects in place:
obj

# For uncopyable types, mut_attrs() lets you modify in place:
env <- env()
mut_attrs(env, foo = "bar")
env
}
\keyword{internal}
