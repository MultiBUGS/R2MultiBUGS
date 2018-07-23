\name{bugs.update.settings}
\alias{bugs.update.settings}
\title{Adjusting MultiBUGS registry}
\description{Adjusts MultiBUGS registry - intended for internal use}
\usage{
bugs.update.settings(n.burnin, bugs.directory)
}
\arguments{
  \item{n.burnin}{length of burn in}
  \item{bugs.directory}{directory that contains the \pkg{MultiBUGS} executable}
}

\details{

Alter the adaptive phases in the \pkg{MultiBUGS} updaters (in the
\file{BUGS/System/Rsrc/Registry.odc} file, where BUGS is map of
installed \pkg{MultiBUGS}) so that all adaptive updating is done during
the burnin stage. Otherwise \pkg{MultiBUGS} will not report results if the
burnin is less than a minimum value such as 500 or 4000 (depending on
the model). Write permission on above mentioned file is needed for this!

}

\value{Nothing, but as a side effect, the registry file
  \file{BUGS/System/Rsrc/Registry.odc} is modified}

\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{internal}
\keyword{IO}
\keyword{file}
