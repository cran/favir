\name{IncludeLegal}
\alias{IncludeLegal}
\title{Print default legal disclaimer and license info}

\description{ This function can be called near the end of FAViR paper,
  written using Sweave.  It specifies default licensing for the
  paper. }

\usage{
IncludeLegal(author, year)
}

\arguments{
  \item{author}{The author(s) of the paper}
  \item{year}{The year(s) to claim copyright for}
}

\details{The license specified is the GNU GPL v2 or later for the source
  code and the GNU all-permissive license for the resulting paper.
  Distributing your paper with the output of \code{IncludeLegal}
  licenses the paper under these terms.  }

\seealso{
  \code{\link{InitPaper}},
  \code{\link{IncludePrelude}}.
}

\examples{
IncludeLegal("John Doe and Jill Smith", 2008)
}
