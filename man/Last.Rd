\name{Last}
\alias{Last}
\title{Return last n elements of a vector or data frame}

\description{ This function is similar to \code{tail} but does more
  error checking.  In particular, it is an error if the argument is
  too small. }

\usage{
Last(x, n=1, allow.fewer=FALSE)
}

\arguments{
  \item{x}{A vector or data frame}
  \item{n}{The number of elements to return}
  \item{allow.fewer}{Boolean.  If FALSE, raise an error if \code{x} has
  length < \code{n}.}
}

\seealso{
  \code{\link{tail}}.
}

\examples{
x <- 1:2
Last(x)
\dontrun{Last(x, 3)}
tail(x)
}
