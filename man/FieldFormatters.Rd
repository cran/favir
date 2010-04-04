\name{FieldFormatters}
\alias{FieldFormatters}
\alias{FieldFormatters<-}
\title{Get and set formatters for particular fields}

\description{ Get or set the field-level formatting operations for a
  favir.data.frame object }

\usage{
FieldFormatters(fdf)
FieldFormatters(fdf) <- value
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{value}{List of field formatters to use}
}


\details{When an object of class favir.data.frame is converted to latex,
  the data, which probably has type numeric, must be converted from the
  internal R representation to a string.  The system must decide how
  many decimal places to show, whether a comma should separate hundreds
  from thousands, etc.  It does this using formatter functions, which
  are functions of a single argument that return the string
  representation of the argument.

  Each column/field in the data frame can have a field formatter set,
  which controls the formatting for that particular field.  The argument
  \code{value} to \code{FieldFormatter<-} should be a list whose keys
  are the field names in the data frame, and whose values are formatter
  functions.
}

\seealso{
  \code{\link{formatters}},
  \code{\link{DefaultFormatter}},
  \code{\link{FavirDF}},
  \code{\link{MakeFormatter}}.
}

\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10))
print(fdf)
FieldFormatters(fdf) <- list(a=MakeFormatter(), b=MakeFormatter(digits=3))
print(fdf)
}
