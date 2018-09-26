#' Running MultiBUGS from R
#'
#' \pkg{R2MultiBUGS} Call a \pkg{BUGS} model, summarize inferences and
#' convergence in a table and graph, and save the simulations in arrays for
#' easy access in \code{R}. The main command is \code{\link{bugs}}.
#'
#' The following are sources of information on \pkg{R2MultiBUGS} package:
#' \tabular{ll}{
#'   DESCRIPTION file\tab \code{library(help="R2MultiBUGS")}\cr
#'   \tab \cr
#'   This file\tab        \code{package?R2MultiBUGS}\cr
#'   \tab \cr
#'   Some help files\tab  \code{\link{bugs}}\cr
#'   \tab                 \code{\link{write.model}}\cr
#'   \tab                 \code{\link{print.bugs}}\cr
#'   \tab                 \code{\link{plot.bugs}}\cr
#' }
#'
#' @name R2MultiBUGS-package
#' @aliases R2MultiBUGS-package R2MultiBUGS
#' @docType package
#' @keywords package
#' @importFrom coda mcmc mcmc.list as.mcmc.list read.coda
#' @importFrom boot logit
#' @importFrom graphics layout lines mtext par plot points strheight strwidth text
#' @importFrom stats median qf quantile sd var
#' @importFrom utils compareVersion flush.console menu read.table
#' @importFrom parallel detectCores
NULL

#' 8 schools analysis
#'
#' 8 schools analysis
#'
#' @name schools
#' @docType data
#' @format A data frame with 8 observations on the following 3 variables.
#' \describe{ \item{school}{See Source.} \item{estimate}{See Source.}
#' \item{sd}{See Source.} }
#' @source Rubin, D.B. (1981): Estimation in Parallel Randomized Experiments.
#' \emph{Journal of Educational Statistics} 6(4), 377-400.
#'
#' Section 5.5 of Gelman, A., Carlin, J.B., Stern, H.S., Rubin, D.B. (2003):
#' \emph{Bayesian Data Analysis}, 2nd edition, CRC Press.
#' @keywords datasets
NULL
