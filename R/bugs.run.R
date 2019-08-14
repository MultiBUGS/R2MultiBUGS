#' Calling MultiBUGS
#'
#' Calls \pkg{MultiBUGS} and runs it with \file{script.txt} - intended for
#' internal use
#'
#' @param n.burnin length of burn in
#' @param MultiBUGS.pgm Full path to the \pkg{MultiBUGS} executable or shell
#' script
#' @param debug When debug=T, the MultiBUGS gui is displayed during execution.
#' there is no effect with linux execution
#' @param useWINE as in \code{\link{bugs}} meta function
#' @param WINE as in \code{\link{bugs}} meta function
#' @param newWINE as in \code{\link{bugs}} meta function
#' @param WINEPATH as in \code{\link{bugs}} meta function
#' @return Nothing, but has side effects as documented in
#' \code{\link{bugs.update.settings}} and calls \pkg{MultiBUGS}.
#' @seealso The main function to be called by the user is \code{\link{bugs}}.
#' @keywords internal
bugs.run <- function(n.burnin,
                     MultiBUGS.pgm,
                     debug = FALSE,
                     useWINE = FALSE,
                     WINE = NULL,
                     newWINE = TRUE,
                     WINEPATH = NULL){
  ## Update the lengths of the adaptive phases in the Bugs
  ## updaters try(bugs.update.settings(n.burnin,
  ## bugs.directory)) ## Return the lengths of the adaptive
  ## phases to their original settings .fileCopy <- file.copy
  ## on.exit(try(.fileCopy(file.path(bugs.directory,
  ## 'System/Rsrc/Registry_Rsave.odc'),
  ## file.path(bugs.directory, 'System/Rsrc/Registry.odc'),
  ## overwrite=TRUE)))
  ## Call Bugs and have it run with script.txt
  if (.Platform$OS.type == "windows" || useWINE){
    bugsCall <- paste("\"",
                      MultiBUGS.pgm,
                      "\" /PAR \"",
                      native2win(file.path(getwd(), "script.txt"),
                                 useWINE = useWINE,
                                 newWINE = newWINE,
                                 WINEPATH = WINEPATH),
                      "\"",
                      sep = "")
    if (!debug){
      bugsCall <- paste(bugsCall, " /HEADLESS", sep = "")
    }
    if (useWINE){
      bugsCall <- paste(WINE, bugsCall)
    }
  } else {
    bugsCall <- paste(MultiBUGS.pgm,
                      "/PAR",
                      "script.txt")
  }
  if ((.Platform$OS.type == "windows" || useWINE) && debug){
    temp <- system(bugsCall, invisible = FALSE)
  } else {
    temp <- system(bugsCall)
  }

  if (temp == -1){
    stop("Error in bugs.run().")
  }
}
