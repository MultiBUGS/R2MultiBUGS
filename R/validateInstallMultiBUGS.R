validateInstallMultiBUGS <- function(
    MultiBUGS.pgm=NULL,
    useWINE=FALSE, WINE=NULL,
    newWINE=TRUE, WINEPATH=NULL
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

test.models <- c("Air", "Asia", "Beetles", "BiRats", "Camel",
                 "Dugongs", "Dyes", "Equiv", "Eyes",
                 "Line", "OtreesMVN", "Rats", "Stacks",
                 "Surgical", "Surgicalrand")

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
                    Stacks = c("b", "b0", "outlier[21]","outlier[3]", "outlier[4]", "sigma"),
                    Surgical = "p",
                    Surgicalrand = c("p","pop.mean", "sigma")
                    )

test.modelfile <- paste(test.models,"model.txt",sep="")
test.datafile <- paste(test.models,"data.txt",sep="")
test.inits <- paste(test.models,"inits.txt",sep="")
test.pattern <- paste("^", test.models, ".*\\.txt$", sep="")

### Test for posterior means within 1 percent of previously saved values

res.true <- dget(file = system.file("validateInstallMultiBUGS/validMultiBUGSResults.R", package="R2MultiBUGS") )

message("The version of MultiBUGS on your computer is being compared to validation\n",
     "results created using MultiBUGS version 3.2.1\n")

for (i in seq(along=test.models)) {
    exfiles <- dir(system.file("validateInstallMultiBUGS", package="R2MultiBUGS"), pattern=test.pattern[i], full.names=TRUE)
    ok <- file.copy(exfiles, tempdir())
    fit <- round(bugs(data=test.datafile[i], inits=test.inits[i],
              parameters.to.save=test.params[[test.models[i]]],model.file=test.modelfile[i], 
              n.burnin=5000, n.iter=20000, n.thin=1, n.chains=1, DIC=FALSE, 
              working.directory=tempdir(),
              MultiBUGS.pgm=MultiBUGS.pgm)$summary, 5)
    if(isTRUE(all.equal(fit, res.true[[i]], tol=1e-2))){
        message(paste('Results matched for example', test.models[[i]], '\n', sep=' '))
    } else{
        message(paste('Results did not match for example',test.models[[i]], '\n', sep=' '))
    }
    flush.console()
}
    invisible()
}
                    