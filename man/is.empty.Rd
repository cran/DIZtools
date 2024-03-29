% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/is.empty.R
\name{is.empty}
\alias{is.empty}
\title{Check for empty/`NA`/`NULL`/`NaN`/etc. values.}
\source{
\href{https://github.com/Rapporter/rapportools/blob/master/R/utils.R}{Copied from `rapportools::is.empty()`}
}
\usage{
is.empty(x, trim = TRUE, all = FALSE, ...)
}
\arguments{
\item{x}{an object to check its emptiness}

\item{trim}{trim whitespace? (\code{TRUE} by default)}

\item{all}{return overall result over list/vector instead of vector of
results? \code{is.empty(x, all = TRUE)} is the same like
\code{all(unlist(is.empty(x)))}}

\item{...}{additional arguments for \code{\link{sapply}}}
}
\description{
Rails-inspired helper that checks if vector values are "empty",
  i.e. if it's: \code{NULL}, zero-length, \code{NA}, \code{NaN},
  \code{FALSE}, an empty string or \code{0}.
  Note that unlike its native R \code{is.<something>} sibling functions,
  \code{is.empty} is vectorised (hence the "values").
}
\examples{
\dontrun{
is.empty(NULL)     # [1] TRUE
is.empty(c())      # [1] TRUE
is.empty(NA)       # [1] TRUE
is.empty(NaN)      # [1] TRUE
is.empty("")       # [1] TRUE
is.empty(0)        # [1] TRUE
is.empty(0.00)     # [1] TRUE
is.empty("    ")   # [1] TRUE
is.empty("foobar") # [1] FALSE
is.empty("    ", trim = FALSE)             # [1] FALSE
## is.empty is vectorised!
all(is.empty(rep("", 10)))                 # [1] TRUE
all(is.empty(matrix(NA, 10, 10)))          # [1] TRUE
is.empty(matrix(NA, 10, 10), all = TRUE))  # [1] TRUE
}
}
