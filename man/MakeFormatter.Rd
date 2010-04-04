\name{MakeFormatter}
\alias{MakeFormatter}
\title{Create and return a formatting function}

\description{ Create and return a function to format numbers }

\usage{
MakeFormatter(digits=0, format="f", big.mark=",", math.mode=TRUE, ...)
}

\arguments{
  \item{digits}{Number of displayed digits to right of decimal point}
  \item{format}{Normal vs scientific notation}
  \item{big.mark}{Separator between groups of digits}
  \item{math.mode}{Whether latex math mode is enabled}
  \item{...}{Other arguments to pass through to formatC}
}


\details{MakeFormatter returns a function which accepts a numeric type
  and returns a string representation of that number.  It mostly a
  wrapper around \code{\link{formatC}} which has sensible defaults for
  actuarial work.  The arguments are summarized briefly below, but see
  the documentation for \code{formatC} for more information.

  The \code{format} argument defaults to "f" for traditional (xxx.yyy)
  notation.  "e" is for scientific notation (n.de+n).

  \code{big.mark} separates thousands, millions, etc.  Set it to "" for
  no separator.

  If \code{math.mode} is TRUE, the entry will be displayed in latex math
  mode.  This should probably be set to false for strings.
}

\seealso{
  \code{\link{FieldSettings}},
  \code{\link{FieldFormatters}},
  \code{\link{FavirDF}}.
}

\examples{
x <- 12345.6789
MakeFormatter()(x)
MakeFormatter(digits=3)(x)
MakeFormatter(digits=1, big.mark="")(x)
\dontshow{
Assert(all(MakeFormatter()(x) == "$12,346$"))
Assert(all(MakeFormatter(digits=3)(x) == "$12,345.679$"))
Assert(all(MakeFormatter(digits=1, big.mark="", math.mode=FALSE)(x) == "12345.7"))
}
}
