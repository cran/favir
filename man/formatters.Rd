\name{formatters}
\alias{formatters}
\title{Several common formatting functinons pre-packaged}

\description{ \code{formatters} is a list of common formatting functions
  that can be used with \code{\link{FieldFormatters}} or
  \code{\link{FieldSettings}}.
  }

\usage{
formatters
}

\details{Each component of the \code{formatters} list is a function that
  formats numbers into strings.  The basic prefixes are \samp{comma},
  \samp{flat}, \samp{space}, and \samp{percent}.  Each can then have an
  option digit between 0 and 3 indicating how many decimal places to
  show.  See the examples below.

}

\seealso{
  \code{\link{FieldSettings}},
  \code{\link{FieldFormatters}},
  \code{\link{FavirDF}}.
}

\examples{
formatters$percent(0.12345)
formatters$percent3(0.12345)
formatters$flat(123456)
formatters$comma(123456)
formatters$space3(123456)

\dontshow{
Assert(all(formatters$percent(0.12345) == "$12.3$"))
Assert(all(formatters$percent3(0.12345) == "$12.345$"))
Assert(all(formatters$flat(123456) == "$123456$"))
Assert(all(formatters$comma(123456) == "$123,456$"))
Assert(all(formatters$space3(123456) == "$123 456.000$"))
}
}
