\name{read.bugs}
\alias{read.bugs}
\title{Read output files in CODA format}

\description{This function reads Markov Chain Monte Carlo output in the
  CODA format produced by \pkg{OpenBUGS} and returns an object of class
  \code{\link[coda]{mcmc.list}} for further output analysis using the
  \pkg{coda} package.}

\usage{
read.bugs(codafiles, ...)
}

\arguments{
  \item{codafiles}{character vector of filenames (e.g. returned from
    \code{\link{bugs}} in call such as \code{bugs(....., codaPkg=TRUE,
    .....)}). Each of the files contains coda output for one chain
    produced by \pkg{OpenBUGS}, the \emph{directory} name of the corresponding
    file \file{CODAindex.txt} is extracted from the first element of
    \code{codafiles}.}
  \item{...}{further arguments to be passed to
    \code{\link[coda]{read.coda}}}
}

% TODO
% \details{
%  \code{\link{bugs}} (actually it is \code{\link{bugs.script}}) writes
%  BUGS script in such a way that iteration numbers in resulting CODA
%  files correspond to iteration numbers before burnin and thinning.
% }

\seealso{\code{\link{bugs}}, \code{\link[coda]{read.coda}},
  \code{\link[coda]{mcmc.list}}}

\keyword{IO}
\keyword{file}
