\name{bugs.plot}
\alias{bugs.plot.summary}
\alias{bugs.plot.inferences}
\title{Plotting summary information - intended for internal use only}
\description{Plotting summary information - intended for internal use}
\usage{
bugs.plot.summary(sims, ...)
bugs.plot.inferences(sims, display.parallel, ...)
}
\arguments{
  \item{sims}{an object of class `bugs', see \code{\link{bugs}} for
    details}
  \item{display.parallel}{display parallel intervals in both halves of
    the summary plots; this is a convergence-monitoring tool and is not
    necessary once you have approximate convergence (default is
    \code{FALSE})}
  \item{...}{further arguments to be passed to low-level plot functions}
}

\details{
  \code{bugs.plot.summary} (left hand side of plot) and
  \code{bugs.plot.inferences} (right hand side of plot).
}

\value{Does not return anything, but prints and plots as side-effects.}

\seealso{The main function to be called by the user is \code{plot}, see
  \code{\link{plot.bugs}} for details.}

\keyword{internal}
