\name{plot.bugs}
\alias{plot.bugs}
\title{Plotting a bugs object}
\description{Plotting a \code{bugs} object}
\usage{
\method{plot}{bugs}(x, display.parallel = FALSE, ...)
}
\arguments{
  \item{x}{an object of class `bugs', see \code{\link{bugs}} for details}
  \item{display.parallel}{display parallel intervals in both halves of
    the summary plots; this is a convergence-monitoring tool and is not
    necessary once you have approximate convergence (default is
    \code{FALSE})}
  \item{...}{further arguments to \code{\link{plot}}}
}
\seealso{\code{\link{bugs}}}
\keyword{hplot}
