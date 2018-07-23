\name{bugs.sims}
\alias{bugs.sims}
\title{MultiBUGS output reader}

\description{Reads simulations from \pkg{MultiBUGS} into \R,
  formats them, monitors convergence, performs convergence checks, and
  computes medians and quantiles - intended for internal use.}

\usage{
bugs.sims(parameters.to.save, n.chains, n.iter, n.burnin, n.thin,
    DIC = TRUE)
}
\arguments{
  \item{parameters.to.save}{parameters that should be monitored}
  \item{n.chains}{number of Markov chains}
  \item{n.iter}{number of total iterations (including burn in)}
  \item{n.burnin}{length of burn in}
  \item{n.thin}{size of thinning parameter}
  \item{DIC}{calculation of DIC}
}
\value{Returns the same values as \code{\link{bugs}}.}
\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{internal}
\keyword{IO}
\keyword{file}
