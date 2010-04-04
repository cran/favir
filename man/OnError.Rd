\name{OnError}
\alias{OnError}
\title{Print traceback and dump debugging frames to a file}

\description{ This may be an appropriate debugging function for
  non-interactive use. }

\usage{
OnError()
}

\details{Frames will be dumped into a file called "last.dump.rda" in the
  current directory.  This can be read with the debugger() function.
  For interactive use, options(error=recover) may be more appropriate.
}

\examples{
\dontrun{
options(error=OnError)
aoestnuh # Cause an error
debugger(load("last.dump.rda"))
}
}
