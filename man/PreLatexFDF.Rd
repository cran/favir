\name{PreLatexFDF}
\alias{PreLatexFDF}
\alias{print.favir.prelatex}
\title{An object a favir data frame is converted into before printing}

\description{ When a favir data frame is printed, it is first converted
  into an object of type favir.prelatex.  This intermediate stage is
  returned by the \code{PreLatexFDF} function. }

\usage{
PreLatexFDF(favir.df)
print.favir.prelatex(x, ...)
}

\arguments{
  \item{favir.df}{An object of type favir.data.frame}
  \item{x}{An object of type favir.prelatex}
  \item{...}{Additional arguments supplied to print}
}


\details{A prelatex object still keeps track of which items are in which
  row and column, but it is otherwise much closer to LaTeX than a
  favir.data.frame.  In particular, all data is stored as strings (the
  type of each column has been lost).
}
  
\seealso{
  \code{\link{FavirDF}},
  \code{\link{RBindPreLatex}}.
}


\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10))
pl.fdf <- PreLatexFDF(fdf)
print(pl.fdf)
}
