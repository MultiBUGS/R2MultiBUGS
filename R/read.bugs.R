#' Read output files in CODA format
#'
#' This function reads Markov Chain Monte Carlo output in the CODA format
#' produced by \pkg{MultiBUGS} and returns an object of class
#' \code{\link[coda]{mcmc.list}} for further output analysis using the
#' \pkg{coda} package.
#'
#' @param codafiles character vector of filenames (e.g. returned from
#' \code{\link{bugs}} in call such as \code{bugs(....., codaPkg=TRUE, .....)}).
#' Each of the files contains coda output for one chain produced by
#' \pkg{MultiBUGS}, the \emph{directory} name of the corresponding file
#' \file{CODAindex.txt} is extracted from the first element of
#' \code{codafiles}.
#' @param ... further arguments to be passed to \code{\link[coda]{read.coda}}
#' @seealso \code{\link{bugs}}, \code{\link[coda]{read.coda}},
#' \code{\link[coda]{mcmc.list}}
#' @keywords IO file
#' @export read.bugs
read.bugs <- function(codafiles, ...){
  mcmc.list(lapply(codafiles,
                   read.coda,
                   index.file = file.path(dirname(codafiles[1]),
                                          "CODAindex.txt"),
                   ...))
}
