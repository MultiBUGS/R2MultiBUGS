\name{monitor}
\alias{monitor}
\alias{conv.par}
\title{Special summary statistics}

\description{Special summary statistics of the OpenBUGS output.}

\usage{
monitor(a, n.chains = dim(a)[2], trans = NULL, keep.all = FALSE, Rupper.keep = FALSE)
conv.par(x, n.chains, Rupper.keep = TRUE)
}
\arguments{
  \item{a}{a \code{n * m * k} array: \code{m} sequences of length
    \code{n}, \code{k} variables measured}
  \item{n.chains}{number of Markov chains}
  \item{trans}{a vector of length \code{k}: "" if no transformation, or
    "log" or "logit" (If \code{trans} is \code{NULL}, it will be set to
    "log" for parameters that are all-positive and 0 otherwise.)}
  \item{keep.all}{if \code{FALSE} (default), first half of \code{a} will
    be discarded}
  \item{Rupper.keep}{if \code{FALSE}, don't return \code{Rupper}}
  \item{x}{for internal use only}
}

\details{\code{conv.par} is intended for internal use only.}

\value{
  for \code{monitor}:
    \item{output}{list of "mean","sd", quantiles
      ("2.5\%","25\%","50\%","75\%","97.5\%"), "Rhat" if
      \code{n.chains>1}, "Rupper" if \code{(Rupper.keep == TRUE) &&
      (n.chains > 1)}, and "n.eff" if \code{n.chains > 1}}
  for \code{conv.par} a list with elements:
    \item{quantiles}{emipirical quantiles of simulated sequences}
    \item{confshrink}{estimated potential scale reduction (that would be
      achieved by continuing simulations forever) has two components: an
      estimate and an approx. 97.5\% upper bound}
    \item{n.eff}{effective sample size: \code{m*n*min(sigma.hat^2/B,1)}.
      This is a crude measure of sample size because it relies on the
      between variance, \code{B}, which can only be estimated with m
      degrees of freedom.}
}

\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{internal}

