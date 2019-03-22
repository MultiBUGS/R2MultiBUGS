#' Compare MultiBUGS/R2MultiBUGS execution to results supplied with MultiBUGS
#'
#' A selected subset of the examples from the MultiBUGS manual is executed and
#' compared to results supplied with the package to validate installation.
#'
#' Operation system support:
#' \itemize{
#' \item \pkg{MS Windows} Yes
#' \item \pkg{Linux, intel processors} Yes, but GUI display and graphics not
#'   available.
#' \item \pkg{Mac OS X} and \pkg{Unix} Wine emulation via \code{useWINE=TRUE}
#' }
#'
#' If \code{useWINE=TRUE} is used, all paths (such as \code{working.directory}
#' and \code{model.file}, must be given in native (Unix) style, but
#' \code{MultiBUGS.pgm} can be given in Windows path style (e.g.
#' \dQuote{c:/Program Files/MultiBUGS/}) or native (Unix) style \cr (e.g.
#' \dQuote{/path/to/wine/folder/dosdevices/c:/Program
#' Files/MultiBUGS/MultiBUGS321/MultiBUGS.exe}).
#'
#' @param test.models A character list of models to validate.
#' @param n.workers The number of workers to use in the MCMC simulation.
#' @param report How to report the result of the validation. If \code{'text'}
#' then the outcome is reported to the console. If \code{'appveyor'} the result
#' is reported to the \code{appveyor} command line API, and R will be quit
#' after each test model.
#' @param MultiBUGS.pgm See \code{\link{bugs}}.
#' @param useWINE logical; attempt to use the Wine emulator to run
#' \pkg{MultiBUGS}. Default is \code{FALSE}. If WINE is used, the arguments
#' \code{MultiBUGS.pgm} and \code{working.directory} must be given in form of
#' Linux paths rather than Windows paths (if not \code{NULL}).
#' @param WINE Character, path to \file{wine} binary file, it is tried hard (by
#' a guess and the utilities \code{which} and \code{locate}) to get the
#' information automatically if not given.
#' @param newWINE Use new versions of Wine that have \file{winepath} utility
#' @param WINEPATH Character, path to \file{winepath} binary file, it is tried
#' hard (by a guess and the utilities \code{which} and \code{locate}) to get
#' the information automatically if not given.
#' @param ... Other arguments passed through to \code{bugs}
#' @return No data returned.  Prints match/no match result to console for each
#' example.
#' @author Neal Thomas based on BRugs examples created by Chris Jackaon.
#' @seealso \code{\link{bugs}}
#' @references Gelman, A., Carlin, J.B., Stern, H.S., Rubin, D.B. (2003):
#' \emph{Bayesian Data Analysis}, 2nd edition, CRC Press.
#'
#' Sturtz, S., Ligges, U., Gelman, A. (2005): R2WinBUGS: A Package for Running
#' WinBUGS from R.  \emph{Journal of Statistical Software} 12(3), 1-16.
#' @keywords interface models
#' @export validateInstallMultiBUGS
validateInstallMultiBUGS <- function(test.models = c("Air", "Asia", "Beetles",
                                                     "BiRats", "Camel",
                                                     "Dugongs", "Dyes", "Equiv",
                                                     "Eyes", "Line",
                                                     "OtreesMVN", "Rats",
                                                     "Stacks", "Surgical",
                                                     "Surgicalrand"),
                                     n.workers = 2,
                                     report = "text",
                                     MultiBUGS.pgm = NULL,
                                     useWINE = FALSE,
                                     WINE = NULL,
                                     newWINE = TRUE,
                                     WINEPATH = NULL,
                                     ...){
  ## Selected examples which take a few seconds in total to run

  if (is.null(MultiBUGS.pgm)){
    MultiBUGS.pgm <- findMultiBUGS()
    if (.Platform$OS.type == "windows" | useWINE == TRUE){
      MultiBUGS.pgm <- file.path(MultiBUGS.pgm, "MultiBUGS.exe")
    }
  } else if (MultiBUGS.pgm == "MultiBUGS"){
    MultiBUGS.pgm <- Sys.which("MultiBUGS")
  }

  if (!file.exists(MultiBUGS.pgm)){
    stop("Cannot find the MultiBUGS program")
  }

  any_failed <- FALSE
  if (report == "text"){
    report_fun <- function(fit,
                           true,
                           matched,
                           model,
                           milliseconds,
                           working.directory){
      if (matched){
        message(paste("Results matched for example",
                      model,
                      "\n",
                      sep = " "))
      } else {
        message(paste("Results did not match for example",
                      model,
                      "\n",
                      sep = " "))
      }
    }
  } else if (report == "appveyor"){
    report_fun <- function(fit,
                           true,
                           matched,
                           model,
                           milliseconds,
                           working.directory){
      if (matched){
        message(paste("Results matched for example",
                      model,
                      "\n",
                      sep = " "))
      } else {
        message(paste("Results did not match for example",
                      model,
                      "\n",
                      sep = " "))
      }
      outcome <- ifelse(matched, "Passed", "Failed")
      model <- paste0(model, " (", n.workers, " workers)")
      log <- readLines(file.path(working.directory, "log.txt"))
      fit <- c("\nResults obtained:\n",
               utils::capture.output(print(fit)),
               "\n",
               utils::capture.output(dput(fit)))
      true <- c("\nReference results:\n",
                utils::capture.output(print(true)),
                "\n",
                utils::capture.output(dput(true)))
      stdout <- paste(paste(log, collapse = "\n"),
                      paste(fit, collapse = "\n"),
                      paste(true, collapse = "\n"),
                      sep = "\n\n==============\n\n")
      system(paste("appveyor AddTest",
                   "-Framework", "R2MultiBUGS",
                   "-Filename", shQuote(model),
                   "-Duration", milliseconds,
                   "-Name", shQuote(model),
                   "-Outcome", outcome,
                   "-StdOut", shQuote(stdout)))
    }
  }

  test.params <-
    list(Air = c("X", "theta"),
         Asia = c("bronchitis", "either", "lung.cancer"),
         Beetles = c("alpha", "beta", "rhat"),
         BiRats = c("mu.beta", "sigma"),
         Camel = c("Sigma2", "rho", "tau"),
         Dugongs = c("U3", "alpha", "beta", "gamma", "sigma"),
         Dyes = c("sigma2.btw", "sigma2.with", "theta"),
         Equiv = c("equiv", "mu", "phi", "pi", "sigma1", "sigma2", "theta"),
         Eyes = c("P", "lambda", "sigma"),
         Line = c("alpha", "beta", "sigma"),
         OtreesMVN = c("mu", "sigma", "sigmaC"),
         Rats = c("alpha0", "beta.c", "sigma"),
         Stacks = c("b", "b0", "outlier", "sigma"),
         Surgical = "p",
         Surgicalrand = c("p", "pop.mean", "sigma"))

  ### Test for posterior means within 1 percent of previously
  ### saved values

  res.true <-
    dget(file = system.file("validateInstallMultiBUGS/validMultiBUGSResults.R",
                            package = "R2MultiBUGS"))

  message("The version of MultiBUGS on your computer is",
          "being compared to validation\n",
          "results created using MultiBUGS version 1.0\n")

  for (model in test.models){
    test.modelfile <- paste0(model, "model.txt")
    test.datafile <- paste0(model, "data.txt")
    test.inits <- paste0(model, "inits.txt")
    test.pattern <- paste0("^", model, ".*\\.txt$")
    working.directory <- tempdir()

    exfiles <- dir(system.file("validateInstallMultiBUGS",
                               package = "R2MultiBUGS"),
                   pattern = test.pattern,
                   full.names = TRUE)
    ok <- file.copy(exfiles, tempdir())
    start <- proc.time()
    fit <- bugs(data = test.datafile,
                inits = test.inits,
                parameters.to.save = test.params[[model]],
                model.file = test.modelfile,
                n.burnin = 5000,
                n.iter = 20000,
                n.thin = 1,
                n.workers = n.workers,
                n.chains = 1,
                DIC = FALSE,
                codaPkg = TRUE,
                working.directory = working.directory,
                MultiBUGS.pgm = MultiBUGS.pgm,
                ...)
    milliseconds <- round((proc.time() - start)["elapsed"] * 1000)
    fit <- summary(read.bugs(fit, quiet = TRUE))

    mean.fit <- fit[[model]][["statistics"]][, "Mean"]
    mc.fit <- fit[[model]][["statistics"]][, "Time-series SE"]
    upper.fit <- mean.fit + 2 * mc.fit
    lower.fit <- mean.fit - 2 * mc.fit
    mean.true <- res.true[["statistics"]][, "Mean"]
    matched <- all(mean.true > lower.fit & mean.true < upper.fit)
    if (!matched){
      any_failed <- TRUE
    }
    report_fun(fit = fit,
               true = res.true[[model]],
               matched = matched,
               model = model,
               milliseconds = milliseconds,
               working.directory = working.directory)
    flush.console()
  }
  if (report == "appveyor"){
    exit_status <- ifelse(any_failed, 1, 0)
    q(status = exit_status)
  }
  invisible()
}
