bugs.check.coda <- function(){
  ## !!!! need to check these conditions Stop and print an error
  ## message if Bugs did not run correctly
  tmp <- scan("CODAchain1.txt", character(), quiet = TRUE, sep = "\n")
  tmp <- tmp[1:min(100, length(tmp))]
  if (length(grep("MultiBUGS did not run correctly", tmp)) > 0){
    logfile <- file.path(getwd(), "log.txt")
    if (file.exists(logfile)){
      logtext <- paste(readLines(con = logfile), collapse = "\n")
      stop(paste0("MultiBUGS did not run correctly\n",
                  "Log file:\n",
                  "=================\n",
                  logtext,
                  "\n================="))
    } else {
      stop(paste("MultiBUGS did not run correctly.",
                 "No log file was produced.",
                 "It might help to try again with 'debug=TRUE' to figure out",
                 "what went wrong within MultiBUGS."))
    }
  }
}
