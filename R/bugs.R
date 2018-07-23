"bugs" <-
function(data, inits, parameters.to.save, n.iter, model.file="model.txt",
    n.chains=3, n.burnin=floor(n.iter / 2), n.thin=1,
    n.workers = detectCores() - 1,
    saveExec=FALSE,restart=FALSE,
    debug=FALSE, DIC=TRUE, digits=5, codaPkg=FALSE,
    MultiBUGS.pgm=NULL,
    working.directory=NULL,
    clearWD=FALSE, useWINE=FALSE, WINE=NULL,
    newWINE=TRUE, WINEPATH=NULL, bugs.seed=1, summary.only=FALSE,
    save.history=(.Platform$OS.type == "windows" | useWINE==TRUE), 
    over.relax = FALSE)
{


if(is.null(MultiBUGS.pgm)){
    MultiBUGS.pgm <- findMultiBUGS()
    if(.Platform$OS.type == "windows" || useWINE)
        MultiBUGS.pgm <- file.path(MultiBUGS.pgm, "MultiBUGS.exe")
} else if(MultiBUGS.pgm == "MultiBUGS")
    MultiBUGS.pgm <- Sys.which("MultiBUGS")

if(!file.exists(MultiBUGS.pgm))
    stop("Cannot find the MultiBUGS program") 

  ## Is MultiBUGS.pgm defined in Windows (where second character is :
  ## i.e. C:\Program...) or Unix style path?
  if(useWINE && (substr(MultiBUGS.pgm, 2, 2) == ":")) {
    MultiBUGS.pgm <- win2native(MultiBUGS.pgm, newWINE=newWINE, WINEPATH=WINEPATH)
  }

  ### check options for unix/linux
  if(.Platform$OS.type != "windows" && !useWINE){
    if(debug)stop("The debug option is not available with linux/unix")
    if(save.history)("History plots (save.history) are not available with linux/unix")
  }

  if(! bugs.seed %in% 1:14)
    stop("MultiBUGS seed must be integer in 1:14")

  if(!is.function(model.file) && 
     length(grep("\\.bug", tolower(model.file))))stop("model.file must be renamed with .txt rather than .bug")

  if(is.null(working.directory) && (saveExec || restart))stop("The working directory must be specified when saveExec or restart is TRUE")

  if(!is.null(working.directory)) {
    working.directory <- path.expand(working.directory)
    savedWD <- getwd()
    setwd(working.directory)
    on.exit(setwd(savedWD))
  }

  ## Checking number of inits, which is NOT saved here:
  if(!missing(inits) && !is.function(inits) && !is.null(inits) && (length(inits) != n.chains))
    stop("Number of initialized chains (length(inits)) != n.chains")

  ## Wine
  if(useWINE) {
    ## Attempt to find wine and winepath
    if(is.null(WINE)) WINE <- findUnixBinary(x="wine")
    if(is.null(WINEPATH)) WINEPATH <- findUnixBinary(x="winepath")
  }

  ## Move to working drirectory or temporary directory when NULL
  inTempDir <- FALSE
  if(is.null(working.directory)) {
    working.directory <- tempdir()
    if(useWINE){
        ## Some tweaks for wine (particularly required for Mac OS)
        working.directory <- gsub("//", "/", working.directory)
        Sys.chmod(working.directory, mode="770")
        on.exit(Sys.chmod(working.directory, mode="700"), add = TRUE)
    }
    savedWD <- getwd()
    setwd(working.directory)
    on.exit(setwd(savedWD), add = TRUE)
    inTempDir <- TRUE
  }

  ## model.file is not a file name but a model function
  if(is.function(model.file)){
      temp <- tempfile("model")
      temp <- paste(temp, "txt", sep=".")
      write.model(model.file, con=temp, digits=digits)
      model.file <- gsub("\\\\", "/", temp)
  }
  if(inTempDir && basename(model.file) == model.file)
    try(file.copy(file.path(savedWD, model.file), model.file, overwrite = TRUE))
  if(!file.exists(model.file))
    stop(paste(model.file, "does not exist."))
  if(file.info(model.file)$isdir)
    stop(paste(model.file, "is a directory, but a file is required."))
  if (!(length(data) == 1 && is.vector(data) && is.character(data) && 
       (regexpr("\\.txt$", data) > 0))) {
    bugs.data.file <- bugs.data(data, dir = getwd(), digits)
  } else {
    if(inTempDir && all(basename(data) == data))
        try(file.copy(file.path(savedWD, data), data, overwrite = TRUE))
    if(!file.exists(data))
        stop("File", data, "does not exist.")
    bugs.data.file <- data
  }

  if (is.character(inits)) {
    if(inTempDir && all(basename(inits) == inits))
        try(file.copy(file.path(savedWD, inits), inits, overwrite = TRUE))
    if (!all(file.exists(inits))) {
        stop("One or more inits files are missing")
    }
    if (length(inits)!=n.chains) {
        stop("Need one inits file for each chain")
    }
    bugs.inits.files <- inits
  } else {
    if (!is.function(inits) && !is.null(inits) &&  (length(inits) != n.chains)) {
        stop("Number of initialized chains (length(inits)) != n.chains")
    }
    bugs.inits.files <- bugs.inits(inits, n.chains, digits)
  }

  if(DIC) parameters.to.save <- c(parameters.to.save, "deviance")
  ## Model files must have extension ".txt"
  if(!length(grep("\\.txt$", tolower(model.file)))) {
    new.model.file <- paste(basename(model.file), ".txt", sep="")
    if(!is.null(working.directory)) new.model.file <- file.path(working.directory, new.model.file)
    file.copy(model.file, new.model.file, overwrite=TRUE)
    on.exit(try(file.remove(new.model.file)), add=TRUE)
  } else {
    new.model.file <- model.file
  }

  ## Create a filename model.file + .bug
  model.file.bug<-gsub('\\.txt','.bug',basename(new.model.file))

  if(restart && !file.exists(model.file.bug))stop("The .bug restart file was not found in the working directory")

  if(useWINE){
        ## Some tweaks for wine (particularly required for Mac OS)
        new.model.file <- gsub("//", "/", new.model.file)
  }
  bugs.script(parameters.to.save, n.chains, n.iter, n.burnin, n.thin,
              n.workers,
              saveExec,restart,model.file.bug,
              new.model.file, debug=debug, is.inits=!is.null(inits),
              DIC=DIC, useWINE=useWINE, newWINE=newWINE,
              WINEPATH=WINEPATH, bugs.seed=bugs.seed, 
              summary.only=summary.only, save.history=save.history, 
              bugs.data.file = bugs.data.file, 
              bugs.inits.files = bugs.inits.files, over.relax = over.relax)
  bugs.run(n.burnin, MultiBUGS.pgm, debug=debug, WINE=WINE, useWINE=useWINE,
           newWINE=newWINE, WINEPATH=WINEPATH)
  if(codaPkg)
    return(file.path(getwd(), paste("CODAchain", 1:n.chains, ".txt", sep="")))
  if (summary.only) {
    return(bugs.log("log.txt"))
  }

  sims <- c(bugs.sims(parameters.to.save, n.chains, n.iter, n.burnin,
                      n.thin, DIC),
            model.file=model.file)
  if(clearWD) {
    file.remove(c(bugs.data.file, "log.odc", "log.txt", "CODAIndex.txt",
                  bugs.inits.files, "script.txt",
                  paste("CODAchain", 1:n.chains, ".txt", sep="")))
  }
  class(sims) <- "bugs"
  sims
}
