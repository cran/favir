\name{FavirLoadModule}
\alias{FavirLoadModule}
\alias{$.favir.environment}
\alias{with.favir.environment}
\title{Create and use favir environments}

\description{
  Create a new environment, source the given file in it, and return the
  environment.
}

\usage{
FavirLoadModule(filename)
}

\arguments{
  \item{filename}{The filename of an R source file.}
}

\details{If every FAViR paper used the same namespace, the potential for
  conflicts would rise.  FavirLoadModule is a simple function that
  allows a source code file to be loaded in its own environment, where
  conflicts are not possible.  Objects from the environment can be
  accessed using the \code{$} operator.}

\examples{
# Create a new file with the single statement "a <- 5" in it
filename <- "example-file.R"
\dontshow{
stopifnot(file.access(filename, 0) == -1)
}
write("a <- 5", filename)

# Load it into the environment called favir.env, and read a back
FavirEnv <- FavirLoadModule(filename)
FavirEnv$a
with(FavirEnv, ls())

\dontshow{
stopifnot(FavirEnv$a == 5)
}

# tidy up
unlink(filename)
}
