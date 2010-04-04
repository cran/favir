\name{IncludePrelude}
\alias{IncludePrelude}
\title{Print default latex prelude}

\description{ This function is typically called at the beginning of a
  FAViR paper, written using Sweave.  It writes the default Latex
  introductory code.
}

\usage{
IncludePrelude(title, author, subtitle="")
}

\arguments{
  \item{title}{The paper's title}
  \item{author}{The author of the paper}
  \item{subtitle}{Paper subtitle, or the empty string if none}
}

\details{The latex code printed by this function loads the necessary
  latex packages as well as defining the default theme.
}

\seealso{
  \code{\link{IncludeLegal}},
  \code{\link{InitPaper}}.
}

\examples{
IncludePrelude("Paper title", "Random Actuary", "The shocking truth
about <actuarial topic>")
}
