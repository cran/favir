\name{Label}
\alias{Label}
\alias{Label<-}
\title{Query or set the latex label of a favir.data.frame}

\description{ Query or set the latex reference label in a favir.data.frame object}

\usage{
Label(fdf)
Label(fdf) <- value
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{value}{The label as a string}
}

\details{In latex, the label can be used to refer to a figure or table
  using the \code{ref} command, as in \samp{\\ref\{label\}}.
}

\seealso{
  \code{\link{FavirDF}},
  \code{\link{Caption}}.
}


\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10))
Label(fdf)
Label(fdf) <- "latex label"
fdf
}
