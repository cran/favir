\name{LookupDF}
\alias{LookupDF}
\title{Select a single row from a data frame}

\description{ With extensive error checking, return a single row from a
  data frame. }

\usage{
LookupDF(df, ..., allow.NULL=FALSE)
}

\arguments{
  \item{df}{The data frame to look in}
  \item{...}{The fields to look in}
  \item{allow.NULL}{Whether to allow empty searches}
}

\details{This is a safe way to look for a row in a data frame.  Usually
  when querying a data frame, too many or not enough results will be
  returned or perhaps a field name will be misspelled.  With LookupDF,
  that will trigger an error.

  
  }

\examples{
df <- data.frame(a=1:5, b=11:15, c=21:25)
result <- LookupDF(df, a=3, b=13)
result

\dontshow{
stopifnot(all(names(result) == c("a", "b", "c")))
stopifnot(result$a == 3 && result$b == 13 && result$c == 23)
}

df2 <- data.frame(a=1:5, b=rep(0, 5))
\dontrun{
LookupDF(df2, b=0)
LookupDF(df2, a=-1)
}
stopifnot(identical(NULL, LookupDF(df2, a=-1, allow.NULL=TRUE)))

}
