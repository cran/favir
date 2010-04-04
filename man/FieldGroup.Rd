\name{FieldGroup}
\alias{FieldGroup}
\alias{FieldGroup<-}
\alias{ListFieldGroups}
\alias{ParentFieldGroups}
\title{Query and declare a group of fields}

\description{ Query and declare a group of fields of a favir.data.frame
  object }

\usage{
FieldGroup(fdf, group.name, as.names=TRUE)
FieldGroup(fdf, group.name) <- value
ListFieldGroups(fdf)
ParentFieldGroups(fdf, field)
}

\arguments{
  \item{fdf}{A favir.data.frame object}
  \item{group.name}{The name of a field group}
  \item{as.names}{TRUE to return field names, FALSE to return field indicies}
  \item{value}{A vector either of field names or indicies}
  \item{field}{The name of a single field}
}


\details{If several fields (i.e. columns) in a favir.data.frame object
  hold similar items, they may share formatting.  For instance, perhaps
  three fields all contain premium, so all should be comma-separated.
  These functions allow those fields to be grouped together, so
  formatting may be specified for all of them at once.
  
  A field group is just a set of fields.  Using the these functions
  don't affect formatting immediately; they just group the field
  together so formatting may be specified later on the group.

  \code{FieldGroup} gets and sets a group, specifying which
  fields are in that group.

  \code{ListFieldGroups} returns a vector of all existing field group
  names.

  \code{ParentFieldGroups} returns all the field group names that
  contain the given field.  As a field may be a member of more than one
  group, the results of this function may have length > 1.
}

\seealso{
  \code{\link{FavirDF}}.
  \code{\link{GroupFormatter}}.
  \code{\link{GroupHeading}}.
}


\examples{
fdf <- FavirDF(data.frame(a=1:5, b=6:10))
fdf
FieldGroup(fdf, "all") <- c("a", "b")
FieldGroup(fdf, "all")

FieldGroup(fdf, "only a") <- 1
ListFieldGroups(fdf)
ParentFieldGroups(fdf, "a")
ParentFieldGroups(fdf, "b")

\dontshow{
stopifnot(identical(FieldGroup(fdf, "all"), c("a", "b")))
stopifnot(identical(ParentFieldGroups(fdf, "b"), "all"))
stopifnot(identical(sort(ParentFieldGroups(fdf, "a")), c("all", "only a")))
}

}
