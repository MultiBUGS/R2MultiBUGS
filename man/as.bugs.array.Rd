\name{as.bugs.array}
\alias{as.bugs.array}
\title{Convert to bugs object}

\description{Function converting results from Markov chain simulations,
  that might not be from BUGS, to bugs object. Used mainly to display
  results with \code{\link{plot.bugs}}.
}

\usage{
as.bugs.array(sims.array, model.file=NULL, program=NULL,
    DIC=FALSE, DICOutput=NULL, n.iter=NULL, n.burnin=0, n.thin=1)
}

\arguments{
  \item{sims.array}{3-way array of simulation output, with dimensions
    n.keep, n.chains, and length of combined parameter vector.}
  \item{model.file}{file containing the model written in \pkg{OpenBUGS} code}
  \item{program}{the program used}
  \item{DIC}{logical; whether DIC should be calculated, see also
    argument \code{DICOutput} and details}
  \item{DICOutput}{DIC value}
  \item{n.iter}{number of total iterations per chain used for generating
    \code{sims.array}}
  \item{n.burnin}{length of burn in, i.e. number of iterations to
    discarded at the beginning for generating \code{sims.array}}
  \item{n.thin}{thinning rate, a positive integer, used for generating
    \code{sims.array}}
}

\details{

This function takes a 3-way array of simulations and makes it into a
\code{\link{bugs}} object that can be conveniently displayed using
\code{print} and \code{plot} and accessed using \code{attach.bugs}.  If
the third dimension of sims() has names, the resulting bugs object will
respect that naming convention. For example, if the parameter names are
\dQuote{alpha[1]}, \dQuote{alpha[2]}, ..., \dQuote{alpha[8]},
\dQuote{mu}, \dQuote{tau}, then \code{as.bugs.array} will know that
alpha is a vector of length 8, and mu and tau are scalar parameters.
These will all be plotted appropriately by \code{plot} and attached
appropriately by \code{attach.bugs}.

If \code{DIC=TRUE} then DIC can be either already passed to argument
\code{DICOutput} or calculated
from deviance values in \code{sims.array}.

}

\value{A \code{\link{bugs}} object is returned}

\author{Jouni Kerman, \email{kerman@stat.columbia.edu} with modification
  by Andrew Gelman, \cr
  \email{gelman@stat.columbia.edu}, packaged by Uwe
  Ligges, \email{ligges@statistik.tu-dortmund.de}.}

\seealso{\code{\link{bugs}}}
\keyword{interface}
\keyword{manip}
