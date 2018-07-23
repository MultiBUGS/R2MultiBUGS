\name{bugs.log}
\alias{bugs.log}
\title{Read data from MultiBUGS logfile}
\description{Read data such as summary statistics and DIC information
  from the \pkg{MultiBUGS} logfile}
\usage{
bugs.log(file)
}
\arguments{
  \item{file}{Location of the \pkg{MultiBUGS} logfile}
}
\value{
  A list with components:
  \item{stats}{A matrix containing summary statistics for each saved
    parameter. Comparable to the information in the element
    \code{summary} of a bugs object as returned by \code{\link{bugs}}.}
  \item{DIC}{A matrix containing the DIC statistics as returned from
    \pkg{MultiBUGS}.}
}
\details{Returns the MultiBUGS summary statistics and DIC extracted directly
from the log file.
}
\seealso{The main function that generates the log file is \code{\link{bugs}}.}
\author{Jouni Kerman}
\keyword{IO}
\keyword{file}
