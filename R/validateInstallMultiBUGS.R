validateInstallMultiBUGS <- function(
    test.models = c("Air", "Asia", "Beetles", "BiRats", "Camel",
                    "Dugongs", "Dyes", "Equiv", "Eyes",
                    "Line", "OtreesMVN", "Rats", "Stacks",
                    "Surgical", "Surgicalrand"),
    report="text",
    MultiBUGS.pgm=NULL,
    useWINE=FALSE, WINE=NULL,
    newWINE=TRUE, WINEPATH=NULL,
    ...
    )
{
## Selected examples which take a few seconds in total to run

if(is.null(MultiBUGS.pgm)){
    MultiBUGS.pgm <- findMultiBUGS()
    if(.Platform$OS.type == "windows" | useWINE==TRUE)
        MultiBUGS.pgm <- file.path(MultiBUGS.pgm, "MultiBUGS.exe")
} else if(MultiBUGS.pgm == "MultiBUGS")
    MultiBUGS.pgm <- Sys.which("MultiBUGS")

if(!file.exists(MultiBUGS.pgm))
    stop("Cannot find the MultiBUGS program")

if(report == "text"){
  report <- function(matched, model, milliseconds){
    if (matched){
      message(paste('Results matched for example', model, '\n', sep=' '))
    } else {
      message(paste('Results did not match for example', model, '\n', sep=' '))
    }
  }
} else if (report == "appveyor") {
  report <- function(matched, model, milliseconds){
    outcome <- ifelse(matched, "Passed", "Failed")
    exit_status <- ifelse(matched, 0, 1)
    system(paste("appveyor AddTest",
                 "-Framework", "R2MultiBUGS",
                 "-Filename", model,
                 "-Duration", milliseconds,
                 "-Name", model,
                 "-Outcome", outcome))
    q(status = exit_status)
  }
}

test.params <- list(Air = c("X", "theta"),
                    Asia = c("bronchitis", "either", "lung.cancer"),
                    Beetles = c("alpha", "beta", "rhat"),
                    BiRats = c("mu.beta", "sigma"),
                    Camel = c("Sigma2", "rho", "tau"),
                    Dugongs = c("U3","alpha", "beta", "gamma", "sigma"),
                    Dyes = c("sigma2.btw", "sigma2.with", "theta"),
                    Equiv = c("equiv", "mu", "phi", "pi","sigma1", "sigma2", "theta"),
                    Eyes = c("P", "lambda", "sigma"),
                    Line = c("alpha", "beta", "sigma"),
                    OtreesMVN = c("mu","sigma", "sigmaC"),
                    Rats = c("alpha0", "beta.c", "sigma"),
                    Stacks = c("b", "b0", "outlier", "sigma"),
                    Surgical = "p",
                    Surgicalrand = c("p","pop.mean", "sigma")
                    )

### Test for posterior means within 1 percent of previously saved values

res.true <- dget(file = system.file("validateInstallMultiBUGS/validMultiBUGSResults.R", package="R2MultiBUGS") )

message("The version of MultiBUGS on your computer is being compared to validation\n",
     "results created using MultiBUGS version 3.2.1\n")

for (model in test.models) {
    test.modelfile <- paste0(model, "model.txt")
    test.datafile <- paste0(model, "data.txt")
    test.inits <- paste0(model, "inits.txt")
    test.pattern <- paste0("^", model, ".*\\.txt$")

    exfiles <- dir(system.file("validateInstallMultiBUGS", package="R2MultiBUGS"), pattern=test.pattern, full.names=TRUE)
    ok <- file.copy(exfiles, tempdir())
    start <- proc.time()
    fit <- round(bugs(data=test.datafile, inits=test.inits,
              parameters.to.save=test.params[[model]],model.file=test.modelfile,
              n.burnin=5000, n.iter=20000, n.thin=1, n.chains=1, DIC=FALSE,
              working.directory=tempdir(),
              MultiBUGS.pgm=MultiBUGS.pgm, ...)$summary, 5)
    milliseconds <- (proc.time() - start)["elapsed"] * 1000
    matched <- isTRUE(all.equal(fit, res.true[[model]], tol=1e-2))
    report(matched, model, milliseconds)
    flush.console()
}
    invisible()
}

