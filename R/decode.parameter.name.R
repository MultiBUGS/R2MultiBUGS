#' Decodes MultiBUGS parameter names
#' 
#' Decodes \pkg{MultiBUGS} parameter names (e.g., \code{beta[3,14]} becomes
#' \code{beta} with 2 indexes: 3 and 14) for use by the \code{\link{bugs.sims}}
#' function - intended for internal use
#' 
#' 
#' @param a one element of the first column of \file{codaIndex.txt}
#' @return list with elements: \item{root}{name of parameter, e.g. \code{beta}}
#' \item{dimension}{number of \code{indexes}, e.g. 2} \item{indexes}{indexes,
#' e.g. 3 and 14} given \code{a == "beta[3, 14]"}
#' @seealso The main function to be called by the user is \code{\link{bugs}}.
#' @keywords internal
"decode.parameter.name" <-
function (a){
#
# Decodes Bugs parameter names
#   (e.g., "beta[3,14]" becomes "beta" with 2 indexes:  3 and 14)
# for use by the bugs.sim() function
#
  left.bracket <- regexpr ("[[]", a)
  if (left.bracket==-1){
    root <- a
    dimension <- 0
    indexes <- NA
  }
  else {
    root <- substring (a, 1, left.bracket-1)
    right.bracket <- regexpr ("[]]", a)
    a <- substring (a, left.bracket+1, right.bracket-1)
    indexes <- as.numeric(unlist(strsplit(a, ",")))
    dimension <- length(indexes)
  }
  list(root=root, dimension=dimension, indexes=indexes)
}
