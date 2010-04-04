\name{AssertSingle}
\alias{AssertSingle}
\title{Raise error if arguments don't have length 1}

\description{
  This is a simple convenience function.  Many errors in R are caused when
  a vector of length > 1 appears when an item of length 1 is expected.
  This function tests each argument and aborts with an error if any of
  them have length >= 1.
}

\usage{
AssertSingle(..., err.msg=NULL)
}

\arguments{
  \item{...}{Arguments to test}
  \item{err.msg}{If this is specified, it will be given as the error message}
}

\details{AssertSingle prints some helpful information to stderr when it
  raises an error.  Don't use AssertSingle if this behavior isn't
  intended.}

\examples{
x <- 3
y <- "hello"
z <- 1:5

AssertSingle(x, y)
\dontrun{
AssertSingle(x, y, z)
}
}
