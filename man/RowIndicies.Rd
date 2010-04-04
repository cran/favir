\name{RowIndicies}
\alias{RowIndicies}
\title{Enumerate the row indicies of a data frame or array}

\description{ This is a simple convenience function.  For instance, if a
  data frame has 3 rows, RowIndicies returns 1:3.
}

\usage{
RowIndicies(x)
}

\arguments{
  \item{x}{A data frame or matrix}
}

\details{This may be useful for iterating over a data frame.  It can
  replace 1:nrow(x), which can lead to buggy code, because the data
  frame may be empty.  Correct is seq(length.out=nrow(x)) but this is
  more typing.
  }
  
\seealso{
  \code{\link{seq}},
}

  
\examples{
RowIndicies(data.frame(a=1:3))
RowIndicies(data.frame())
\dontshow{
stopifnot(identical(1:3, RowIndicies(data.frame(a=1:3))))
}
}
