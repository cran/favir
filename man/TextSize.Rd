\name{TextSize}
\alias{TextSize}
\alias{TextSize<-}
\title{Query or set the text size in a favir.data.frame}

\description{ Set the latex text size for a favir.data.frame object}

\usage{
TextSize(fdf)
TextSize(fdf) <- value
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{value}{Text size as a string, see below}
}

\details{The text size will be passed through to latex.  Thus the
  following options are available (in order of smallest to biggest):
  \samp{tiny},
  \samp{scriptsize},
  \samp{footnotesize},
  \samp{small},
  \samp{normalize},
  \samp{large},
  \samp{Large},
  \samp{LARGE},
  \samp{huge},
  \samp{Huge}.
}

\seealso{
  \code{\link{FavirDF}},
  \code{\link{Orientation}}.
}


\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10, c=11:15))
TextSize(fdf)
TextSize(fdf) <- "small"
fdf
}
