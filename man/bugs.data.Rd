\name{bugs.data}
\alias{bugs.data}
\title{Writing input for MultiBUGS}

\description{Write file for \pkg{MultiBUGS} to read.} 

\usage{
bugs.data(data, dir = getwd(), digits = 5, data.file = "data.txt")
}
\arguments{
  \item{data}{either a named list (names corresponding to variable names
    in the \code{model.file}) of the data for the \pkg{MultiBUGS} model,
    \emph{or} a vector or list of the names of the data objects used by
    the model}
  \item{dir}{the directory to write the file \file{data.txt} to}
  \item{digits}{number of significant digits used for \pkg{MultiBUGS}
    input, see \code{\link{formatC}}}
  \item{data.file}{name for the file R writes the data into.}
}

\value{The name of \code{data.file} is returned and as a side effect, 
    the data file is written}

\seealso{The main function to be called by the user is \code{\link{bugs}}.}
\keyword{file}
\keyword{IO}
