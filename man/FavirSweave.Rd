\name{FavirSweave}
\alias{FavirSweave}
\title{Sweave and LaTeX a FAViR paper}

\description{ This function Sweaves and then LaTeXs each paper.  It is a
  simple wrapper around the \code{Sweave} and \code{texi2dvi} functions.
}

\usage{
FavirSweave(filenames, change.dir=TRUE, clean=TRUE)
}

\arguments{
  \item{filenames}{Vector of character filenames of Rnw files}
  \item{change.dir}{Whether to put output files in same directory}
  \item{clean}{TRUE to clean-up auxiliary latex files}
}

\details{ \code{FavirSweave} is a simple convenience function which runs
  \code{Sweave} and then \code{texi2dvi} on each of the filenames given.

  If \code{change.dir} is TRUE, the working directory will be changed
  before processing each file to the directory that file is in.
  Afterwards the working directory will be restored.  If
  \code{change.dir} is FALSE, the working directory will not be changed.

  The argument \code{clean} is passed to \code{texi2dvi}.  If TRUE,
  latex auxiliary files like \code{.log} and \code{.aux} will be
  removed.  This may not work on some platforms.
}

\seealso{
  \code{\link{Sweave}},
  \code{\link{texi2dvi}}.
}

\examples{
\dontrun{
FavirSweave("example-paper")
FavirSweave(c("path1/first.paper", "path2/second.paper"))
}
}
