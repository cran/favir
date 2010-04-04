\name{RowColors}
\alias{RowColors}
\alias{RowColors<-}
\alias{DefaultRowColoring}
\title{Get or set row coloring}

\description{ Get or set the colors of each row in a favir.data.frame object }

\usage{
RowColors(fdf)
RowColors(fdf) <- value
DefaultRowColoring(df, primary="white", secondary="colorM1")
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{value}{Character vector of row colors to use}
  \item{df}{A data frame}
  \item{primary}{The primary row color}
  \item{secondary}{The secondary row color}
}


\details{ By default, favir.data.frame objects alternate colors for
  groups of rows to add readability.  The exact coloring of each row can
  be queried and set with \code{RowColors}.

  \code{DefaultRowColoring} returns the default vector of colors to use
  for a given data frame.
}

\seealso{
  \code{\link{FavirDF}}.
}

\examples{
df <- data.frame(a=1:10, b=11:20)
DefaultRowColoring(df)
fdf <- FavirDF(df)
RowColors(fdf)
RowColors(fdf) <- rep("white", nrow(fdf))
RowColors(fdf)
}
