\name{favir.colors}
\alias{favir.colors}
\title{Named vector of default colors}

\description{
  \code{favir.colors} is a vector that has the default graphics colors
  in web-hexidecimal format (e.g. \code{\#D3BDA8}).
}

\usage{
favir.colors
}

\details{The FAViR color scheme is based on three color hues, each with
  five shade variants, for 15 basic colors total.  The main group is
  abbreviated \dQuote{M}, the two accent groups are \dQuote{A} and
  \dQuote{B}.  Within each group the colors are numbered 1 to 5 in order
  of decreasing brightness.

  By default, \dQuote{M} is brown, \dQuote{A} is green, and \dQuote{B}
  is blue.

  Graphics functions should use colors from \code{favir.colors} so other
  users can more easily produce color-consistent graphics.
}
  
\seealso{
  \code{\link{RowColors}}.
}

\examples{
favir.colors["M1"] # Return main color, lightest shade
}

