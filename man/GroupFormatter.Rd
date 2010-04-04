\name{GroupFormatter}
\alias{GroupFormatter}
\alias{GroupFormatter<-}
\alias{GetGlobalGroupFormatter}
\alias{SetGlobalGroupFormatter}
\title{Query or set formatter for a group of fields}

\description{ Query or set formatter for a group of fields in a
  favir.data.frame object }

\usage{
GroupFormatter(fdf, group.name)
GroupFormatter(fdf, group.name) <- value
GetGlobalGroupFormatter(group.name)
SetGlobalGroupFormatter(group.name, formatter)
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{group.name}{The name of a field group}
  \item{value}{The formatting function to use on that field group}
  \item{formatter}{The global formatting function for that group}
}

\details{See \code{\link{FieldGroup}} for background on groups of
  fields, and \code{\link{FieldFormatters}} for formatting functions.
  
  \code{GroupFormatter} sets the formatting function for the given group
  of fields.

  This is similar to setting the FieldFormatter for each field in the
  group.  However, there is a hierarchical relationship---if an
  individual field's formatter is set, it will override the group
  formatting.
  
  Global field groups are field groups that span more than one data
  frame.  For instance, a paper may contain many tables that have
  various types of premium as one of the fields.  If a "premium" group
  were declared, it could set formatting for all those fields at the
  same time.

  \code{GetGlobalGroupFormatter} and
  \code{SetGlobalGroupFormatter} query and establish global field
  groups.
  
}

\seealso{
  \code{\link{FavirDF}},
  \code{\link{FieldGroup}},
  \code{\link{FieldFormatters}}.
}


\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10, c=11:15))
FieldGroup(fdf, "first2") <- 1:2
GroupFormatter(fdf, "first2")
GroupFormatter(fdf, "first2") <- formatters$percent
fdf

SetGlobalGroupFormatter("last1", formatters$comma2)
GetGlobalGroupFormatter("last1")
FieldGroup(fdf, "last1") <- "c"
fdf
}
