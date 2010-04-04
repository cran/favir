\name{Orientation}
\alias{Orientation}
\alias{Orientation<-}
\title{Query or set the orientation of a favir.data.frame}

\description{ Control whether a favir.data.frame object is displayed
  in picture or landscape orientation. }

\usage{
Orientation(fdf)
Orientation(fdf) <- value
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{value}{Either \dQuote{normal} or \dQuote{sideways}}
}

\details{If a table is too wide to fit on a page, \code{Orientation} can
  be used to turn it sideways.  \code{\link{TextSize}} may also help
  here.
}

\seealso{
  \code{\link{FavirDF}},
  \code{\link{TextSize}}.
}


\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10, c=11:15))
Orientation(fdf)
Orientation(fdf) <- "sideways"
print(fdf)
}
