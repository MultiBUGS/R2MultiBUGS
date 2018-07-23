\name{bugs.inits}
\alias{bugs.inits}
\title{Writing input for MultiBUGS }

\description{Write files \file{inits1.txt}, \file{inits2.txt}, etc., in
  the working directory for \pkg{MultiBUGS} to read }

\usage{
bugs.inits(inits, n.chains, digits,
    inits.files = paste("inits", 1:n.chains, ".txt", sep = ""))
}
\arguments{
  \item{inits}{a list with \code{n.chains} elements; each element of the
    list is itself a list of starting values for the \pkg{MultiBUGS} model,
    \emph{or} a function creating (possibly random) initial values}
  \item{n.chains}{number of Markov chains}
  \item{digits}{number of significant digits used for \pkg{MultiBUGS}
    input, see \code{\link{formatC}}}
  \item{inits.files}{name for the inits files R write the inits into.}
}

\value{Vector of names of \code{inits.files}; as a side effect, the inits files \file{inits*.txt}
    are written}

\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{IO}
\keyword{file}
