\name{FieldHeadings}
\alias{FieldHeadings}
\alias{FieldHeadings<-}
\title{Get and set printable field names}

\description{ Get or set the printable field names for a
  favir.data.frame object }

\usage{
FieldHeadings(fdf)
FieldHeadings(fdf) <- value
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{value}{List field headings to use}
}


\details{ For convenience, data frame field names may be short strings
  like \code{olep} instead of descriptive titles like \dQuote{On-Level
  Earned Premium}.  This function allows for the more descriptive titles
  to be printed.
}

\seealso{
  \code{\link{FieldSettings}},
  \code{\link{GroupHeading}},
  \code{\link{FavirDF}}.
}

\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10))
print(fdf)
FieldHeadings(fdf) <- list(a="Policies In-Force", b="Earned Premium")
FieldHeadings(fdf)
print(fdf)
}
