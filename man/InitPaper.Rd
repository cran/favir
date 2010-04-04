\name{InitPaper}
\alias{InitPaper}
\title{Initialize FAViR paper settings}

\description{ This function is typically called at the beginning of a
  FAViR paper, written using Sweave.  It sets various global or
  environment settings.  }

\usage{
InitPaper()
}

\details{Currently this just modifies error handling and makes sure the
  graph directory exists.
}

\seealso{
  \code{\link{IncludePrelude}},
  \code{\link{FavirLoadModule}}.
}

\examples{
\dontrun{
InitPaper()
}
}