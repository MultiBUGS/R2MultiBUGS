\name{bugs.run}
\alias{bugs.run}
\title{Calling OpenBUGS}

\description{
  Calls \pkg{OpenBUGS} and runs it with \file{script.txt} - intended for
  internal use}

\usage{
bugs.run(n.burnin, OpenBUGS.pgm, debug=FALSE, 
         useWINE=FALSE, WINE=NULL,
         newWINE=TRUE, WINEPATH=NULL)
}
\arguments{
  \item{n.burnin}{length of burn in}
  \item{OpenBUGS.pgm}{Full path to the \pkg{OpenBUGS} executable or shell script}
  \item{debug}{When debug=T, the OpenBUGS gui is displayed during execution.  there is no effect with linux execution}
  \item{useWINE}{as in \code{\link{bugs}} meta function}
  \item{WINE}{as in \code{\link{bugs}} meta function}
  \item{newWINE}{as in \code{\link{bugs}} meta function}
  \item{WINEPATH}{as in \code{\link{bugs}} meta function}
}

\value{Nothing, but has side effects as documented in
  \code{\link{bugs.update.settings}} and calls \pkg{OpenBUGS}.}

\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{internal}
