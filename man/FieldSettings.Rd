\name{FieldSettings}
\alias{FieldSettings}
\alias{FieldSettings<-}
\title{Get or set attributes for a particular field}

\description{ Get or set general formatting attributes for a particular
field in a favir.data.frame object }

\usage{
FieldSettings(fdf, field)
FieldSettings(fdf, field) <- value
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{field}{The name of a field/column}
  \item{value}{A list of attributes to set (set below)}
}


\details{ The heading and formatting for a favir.data.frame field can be
  controlled using the \code{FieldHeadings} and \code{FieldFormatters}
  functions.  However, it may be more convenient to specify the heading
  and/or formatting for a particular field instead of specifying all of
  them at once.

  The \code{FieldSettings} function allows this.  It accepts or returns
  a list with two possible components:
  \describe{
    \item{heading}{The descriptive title of the field}
    \item{formatter}{The formatting function to use on the field's data}
    }
  
}

\seealso{
  \code{\link{FieldHeadings}},
  \code{\link{FieldFormatters}},
  \code{\link{FavirDF}}.
}

\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10))
FieldSettings(fdf, "b")
FieldSettings(fdf, "b") <- list(heading="Example Header", formatter=formatters$percent)
fdf
}
