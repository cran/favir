\name{IncludeGraph}
\alias{IncludeGraph}
\title{Include a ggplot graph in a Sweave paper}

\description{
  \code{IncludeGraph} is a convenient way to place a ggplot graph inside
  a FAViR latex paper produced with Sweave.  It handles several of the
  details such as size and caption.
}

\usage{
IncludeGraph(graph, caption="", label=NULL, width=12.5, height=12.5, filename=NULL)
}

\arguments{
  \item{graph}{The ggplot graph to include}
  \item{caption}{The Latex caption to display under the graph}
  \item{label}{The Latex label that can be used to refer to the graph}
  \item{width}{The width of the graph in centimeters}
  \item{height}{The height of the graph in centimeters}
  \item{filename}{The filename to use for the graph, or NULL}
}

\details{If no filename is provided, \code{IncludeGraph} will try to
  pick a sensible one.

  By default graphs are stored in the \samp{favir-graphs} subdirectory
  of the current directory, so this directory must be writable.
}
  
\seealso{
  \code{\link{IncludeGrid}},
  \code{\link{FavirDF}}.
}

\examples{
\dontrun{
example.graph <- qplot(1:10, (1:10)^2)
IncludeGraph(example.graph, caption="Example Graph", label="exgraph")
}
}

