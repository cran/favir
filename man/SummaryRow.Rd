\name{SummaryRow}
\alias{SummaryRow}
\alias{SummaryRow<-}
\title{Query or set the summary row of a favir.data.frame}

\description{ Query or set the last row of a favir.data.frame object }

\usage{
SummaryRow(fdf)
SummaryRow(fdf) <- value
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{value}{A list containing the summary row data}
}

\details{It's common to have a printed table with a final row that
  contains different information than the rest of the table.  For
  instance, each field's last entry may be the sum of the previous data.
  These functions allow that last row to be highlighted when printed.
}

\seealso{
  \code{\link{FavirDF}},
}


\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10))
SummaryRow(fdf)
SummaryRow(fdf) <- list(a="Total", b=sum(fdf$b))
fdf
}
