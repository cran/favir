\name{DefaultFormatter}
\alias{DefaultFormatter}
\alias{DefaultFormatter<-}
\alias{SetGlobalDefaultFormatter}
\title{Get and set the default formatting operation}

\description{ Get or set the default formatting operation for a
  favir.data.frame object }

\usage{
DefaultFormatter(fdf)
DefaultFormatter(fdf) <- value
SetGlobalDefaultFormatter(formatter)
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{value}{A function to set as the default formatter}
  \item{formatter}{A function to set as the global default formatter}
}


\details{When an object of class favir.data.frame is converted to latex,
  the data, which probably has type numeric, must be converted from the
  internal R representation to a string.  The system must decide how
  many decimal places to show, whether a comma should separate hundreds
  from thousands, etc.  It does this using formatter functions, which
  are functions of a single argument that return the string
  representation of the argument.

  Assuming no field-level formatting is set, the default formatter is
  applied to each entry and the result is passed on to latex.  A field's
  FieldFormatter will override the default formatter for that particular
  field.

  \code{SetGlobalDefaultFormatter} sets the global default formatter
  that will apply to all tables.  Using this may break the formatting of
  other's code which assumes the default formatter.
  
}

\seealso{
  \code{\link{FieldFormatters}}.
  \code{\link{FavirDF}}.
  \code{\link{MakeFormatter}}.
}

\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10))
print(fdf)
DefaultFormatter(fdf) <- MakeFormatter(digits=5)
print(fdf)
}
