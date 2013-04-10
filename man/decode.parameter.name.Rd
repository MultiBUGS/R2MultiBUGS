\name{decode.parameter.name}
\alias{decode.parameter.name}
\title{Decodes OpenBUGS parameter names}

\description{Decodes \pkg{OpenBUGS} parameter names (e.g.,
  \code{beta[3,14]} becomes \code{beta} with 2 indexes: 3 and 14) for
  use by the \code{\link{bugs.sims}} function - intended for internal
  use}

\usage{
decode.parameter.name(a)
}
\arguments{
  \item{a}{one element of the first column of \file{codaIndex.txt}}
}
\value{
list with elements:
  \item{root}{name of parameter, e.g. \code{beta}}
  \item{dimension}{number of \code{indexes}, e.g. 2}
  \item{indexes}{indexes, e.g. 3 and 14}
given \code{a == "beta[3, 14]"}
}
\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{internal}
