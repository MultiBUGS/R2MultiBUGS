#' Adjusting MultiBUGS registry
#' 
#' Adjusts MultiBUGS registry - intended for internal use
#' 
#' 
#' Alter the adaptive phases in the \pkg{MultiBUGS} updaters (in the
#' \file{BUGS/System/Rsrc/Registry.odc} file, where BUGS is map of installed
#' \pkg{MultiBUGS}) so that all adaptive updating is done during the burnin
#' stage. Otherwise \pkg{MultiBUGS} will not report results if the burnin is
#' less than a minimum value such as 500 or 4000 (depending on the model).
#' Write permission on above mentioned file is needed for this!
#' 
#' @param n.burnin length of burn in
#' @param bugs.directory directory that contains the \pkg{MultiBUGS} executable
#' @return Nothing, but as a side effect, the registry file
#' \file{BUGS/System/Rsrc/Registry.odc} is modified
#' @seealso The main function to be called by the user is \code{\link{bugs}}.
#' @keywords internal IO file
"bugs.update.settings" <-
  function (n.burnin, bugs.directory)
{
    .fileCopy <- file.copy
    .regexpr <- function(...) regexpr(..., useBytes = TRUE)
    .sub <- function(...) sub(..., useBytes = TRUE)
    .writeBin <- 
        if(getRversion() >= "2.10") 
            function(...) writeBin(..., useBytes = TRUE)
        else 
            writeBin
  
  
  char.burnin <- as.character(n.burnin - 1)
  registry <- readBin(file.path(bugs.directory, "System/Rsrc/Registry.odc"),
                      "character", 400, size=1, endian="little")
  locale <- Sys.getlocale("LC_CTYPE")
  Sys.setlocale("LC_CTYPE", "C")
  .fileCopy(file.path(bugs.directory, "System/Rsrc/Registry.odc"),
            file.path(bugs.directory, "System/Rsrc/Registry_Rsave.odc"),
            overwrite = TRUE)
  info <- registry[.regexpr("Int", registry, fixed = TRUE) > 0]
  while(.regexpr("\r", info) > 0) {
    newline <- .regexpr("\r", info)
    info <- substring(info, newline + 1)
    line <- substring(info, 1, .regexpr("\r", info) - 1)
    if(.regexpr("AdaptivePhase", line) > 0) {
      numpos <- .regexpr("Int", line, fixed=TRUE) + 4
      num <- substring(line, numpos)
      if(as.numeric(num) > n.burnin) {
        blanks <- rep(" ", nchar(num, type="chars") - nchar(char.burnin, type="chars"))
        num.new <- paste(paste(blanks, collapse=""), char.burnin, sep="")
        line.new <- .sub(num, num.new, line)
        registry <- .sub(line, line.new, registry)
      }
    }
  }
  Sys.setlocale("LC_CTYPE", locale)
  .writeBin(registry,
           file.path(bugs.directory, "System/Rsrc/Registry.odc"), endian="little")
}
