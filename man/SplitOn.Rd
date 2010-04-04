\name{\%SplitOn\%}
\alias{\%SplitOn\%}
\title{Split a character type at the given substring}

\description{ This function splits a character vector of length 1 into a
  list of strings. }

\usage{
string \%SplitOn\% substring
}

\arguments{
  \item{string}{A single string to separate into components}
  \item{substring}{The string that separates the components}
}

\details{\code{substring} will not be included in the output.
  Importantly, the string can be recovered exactly from the output of
  \code{SplitOn} by gluing the pieces together with \code{substring}
  separating.
}


\examples{
comps <- "hello there" \%SplitOn\% "ll"
comps
Assert(all(comps == c("he", "o there")))

comps2 <- "hello therel" \%SplitOn\% "l"
comps2
Assert(all(comps2 == c("he", "", "o there", "")))
Assert(all("hello therel" == paste(comps2, collapse="l")))
}
