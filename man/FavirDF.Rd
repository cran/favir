\name{FavirDF}
\alias{FavirDF}
\alias{print.favir.data.frame}
\title{Return data frame formatted for latex}

\description{ Return favir.data.frame object, a data frame with extra
  latex formatting}

\usage{
FavirDF(df, field.formatters=list(),
        field.headings=list(), default.formatter=NULL,
        row.colors=NULL, orientation="normal",
        caption="", label=NULL, text.size="normalsize")
}

\arguments{
  \item{df}{The data frame object to format}
  \item{field.formatters}{See \code{\link{FieldFormatters}}}
  \item{field.headings}{See \code{\link{FieldHeadings}}}
  \item{default.formatter}{See \code{\link{DefaultFormatter}}}
  \item{row.colors}{See \code{\link{RowColors}}}
  \item{orientation}{See \code{\link{Orientation}}}
  \item{caption}{See \code{\link{Caption}}}
  \item{label}{See \code{\link{Label}}}
  \item{text.size}{See \code{\link{TextSize}}}
}


\details{This function accepts a normal data frame object as its main
  input and returns a favir.data.frame object, which is a normal data
  frame with attributes indicating how it should be displayed in latex.

  Because a favir.data.frame still has class data.frame, it can be used
  wherever normal data frames can be used.

  The other arguments control how the table should be formatted.  For
  instance caption sets the latex caption displayed below the table.
  For more information, see the get/set functions associated with each
  argument.
  }

\examples{
df <- data.frame(a=1:5, b=6:10)
df <- FavirDF(df, caption="Example Table")
print(df)
print(as.data.frame(df))
}
