\name{IncludeGrid}
\alias{IncludeGrid}
\title{Include a grid of ggplot graphs in a Sweave paper}

\description{
  \code{IncludeGrid} is a convenient way to place several ggplot graphs
  in a uniform grid inside a FAViR latex paper produced with Sweave.  It
  organizes the viewports automatically.
}

\usage{
IncludeGrid(graph.list, ...)
}

\arguments{
  \item{graph.list}{The graph specification (see below)}
  \item{...}{caption, label, width, or height (see below)}
}

\details{\code{graph.list} must be a list of ggplot graphs, where each
  name is of the form \samp{X.Y} where \samp{X} is the row number and
  \samp{Y} is the column number.  The total dimensions of the grid will
  be determined from this list.  Each graph in the grid will have the
  same dimensions (height and width).
  
  \code{IncludeGrid} takes the same caption, label, width, and height
  arguments as \code{IncludeGraph}.
}
  
\seealso{
  \code{\link{IncludeGraph}},
  \code{\link{FavirDF}}.
}

\examples{
\dontrun{
example.graph1 <- qplot(1:10, 1:10)
example.graph1 <- qplot(1:10, (1:10)^2)
# Produce two plots side by side
IncludeGraph(list("1.1"=example.graph1, "1.2"=example.graph2), caption="Example Graph", label="exgraph")
}
}

