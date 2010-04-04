\name{FavirSweave}
\alias{FavirSweave}
\title{Sweave and LaTeX a FAViR paper}

\description{ This function Sweaves and then LaTeXs each paper.  It is a
  simple wrapper around the \code{Sweave} and \code{texi2dvi} functions.
}

\usage{
FavirSweave(filenames)
}

\arguments{
  \item{filenames}{Vector of character filenames of Rnw files}
}

\seealso{
  \code{\link{Sweave}},
  \code{\link{texi2dvi}}.
}

\examples{
\dontrun{
FavirSweave("example-paper.Rnw")
}
}
