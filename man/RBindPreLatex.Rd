\name{RBindPreLatex}
\alias{RBindPreLatex}
\title{Join several favir.prelatex objects into one}

\description{ When a favir data frame is printed, it is first converted
  into an object of type favir.prelatex.  This intermediate stage is
  returned by the \code{PreLatexFDF} function. }

\usage{
RBindPreLatex(..., include.headings=TRUE)
}

\arguments{
  \item{...}{Two or more favir.prelatex objects}
  \item{include.headings}{Whether to keep the headings of the prelatex
    objects (other than the first, which is always kept}
}


\details{\code{RBindPreLatex} may be useful for joining two tables
  horizontally even when they don't hold comparable information.
}
  
\seealso{
  \code{\link{PreLatexFDF}}.
}


\examples{
fdf1 <- FavirDF(data.frame(a=1:5, b=6:10))
pl.fdf1 <- PreLatexFDF(fdf1)

fdf2 <- FavirDF(data.frame(c=11:15, d=16:20))
pl.fdf2 <- PreLatexFDF(fdf2)

print(RBindPreLatex(pl.fdf1, pl.fdf2))
}
