\name{Assert}
\alias{Assert}
\title{stopifnot() with an error message}

\description{
  This is a simple wrapper around stopifnot.  The main difference is
  that Assert takes an error message that is useful for explaining why
  the error occurred.
}

\usage{
Assert(boolean, err.msg=NULL)
}

\arguments{
  \item{boolean}{Argument to test}
  \item{err.msg}{If this is specified, it will be given as the error message}
}

\details{If boolean is TRUE, then nothing will happen.  Otherwise an
  error will be raised and some diagnostic information will be printed
  to standard error.

  R has a tendency towards invisible and/or inscrutible errors.
  \code{\link{stopifnot}} or this \code{Assert} function, can be used
  frequently to improve code reliability.
}

\examples{
\dontrun{
Assert("hello", "Error: hello is not TRUE")
}
Assert(1==1, "No error: TRUE is TRUE")
}
