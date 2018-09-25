fround <- function(x, digits){
  format(round(x, digits), nsmall=digits)
}

#' Printing a bugs object
#'
#' Printing a \code{bugs} object
#'
#' @param x an object of class `bugs', see \code{\link{bugs}} for details
#' @param digits.summary rounding for tabular output on the console (default is
#' to round to 1 decimal place)
#' @param ... further arguments to \code{\link{print}}
#' @seealso \code{\link{bugs}}
#' @keywords print
#' @export
print.bugs <- function(x, digits.summary = 1, ...){
  if(!is.null(x$model.file)){
    cat("Inference for Bugs model at \"", x$model.file, "\", ", sep="")
  }
  if(!is.null(x$program)){
    cat("fit using ", x$program, ",", sep="")
  }
  cat("\nCurrent: ", x$n.chains, " chains, each with ", x$n.iter,
      " iterations (first ", x$n.burnin, " discarded)", sep = "")
  if (x$n.thin > 1){
    cat(", n.thin =", x$n.thin)
  }
  cat("\nCumulative: n.sims =", x$n.sims, "iterations saved\n")
  print(round(x$summary, digits.summary), ...)

  if (x$n.chains > 1){
    cat("\nFor each parameter, n.eff is a crude measure",
        "of effective sample size,")
    cat("\nand Rhat is the potential scale reduction factor ",
        "(at convergence, Rhat=1).\n")
  }

  if(x$isDIC){
    msgDICRule <-
      ifelse(x$DICbyR,
             "(using the rule, pD = var(deviance)/2)", ## Gelman tweak
             "(using the rule, pD = Dbar-Dhat)")       ## BUGS
    cat(paste("\nDIC info ", msgDICRule, "\n", sep=""))
    if (length(x$DIC) == 1){
      cat("pD =",
          fround(x$pD, digits.summary),
          "and DIC =",
          fround(x$DIC, digits.summary))
    } else if (length(x$DIC)>1){
      print(round(x$DIC, digits.summary))
    }
    cat("\nDIC is an estimate of expected predictive error ",
        "(lower deviance is better).\n")
  }
  invisible(x)
}
