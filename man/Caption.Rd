\name{Caption}
\alias{Caption}
\alias{Caption<-}
\title{Query or set the caption in a favir.data.frame}

\description{ Query or set the latex caption of a favir.data.frame object}

\usage{
Caption(fdf)
Caption(fdf) <- value
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{value}{The caption as a string}
}

\details{Latex will render the caption as text below the table.
}

\seealso{
  \code{\link{FavirDF}},
  \code{\link{Label}}.
}


\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10))
Caption(fdf)
Caption(fdf) <- "Example Table"
fdf
}
