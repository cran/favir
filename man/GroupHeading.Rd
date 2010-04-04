\name{GroupHeading}
\alias{GroupHeading}
\alias{GroupHeading<-}
\title{Query or set the heading for a field group}

\description{ Query or set the descriptive heading for a group of fields
  in a favir.data.frame object }

\usage{
GroupHeading(fdf, group.name)
GroupHeading(fdf, group.name) <- value
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{group.name}{The name of a field group}
  \item{value}{The descriptive heading to set}
}

\details{See \code{\link{FieldGroup}} for background on groups of
  fields, and \code{\link{FieldHeadings}} for descriptive field headings.
  
  In latex form, group headings will be centered above the field group
  and separated by a horizontal line.  Fields must be contiguous in
  order to have a heading set.
  
}

\seealso{
  \code{\link{FavirDF}}.
  \code{\link{FieldGroup}}.
  \code{\link{FieldHeadings}}.
}


\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10, c=11:15))
FieldGroup(fdf, "first2") <- 1:2
GroupHeading(fdf, "first2") <- "The first two fields"
fdf
}
