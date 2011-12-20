library(R2OpenBUGS)

## Selected examples which take a few seconds in total to run

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

test.modelfile = paste(test.models,"model.txt",sep="")
test.modelfile<-file.path('C:/Program Files/OpenBUGS/OpenBUGS321/Examples',test.modelfile)
test.datafile = paste(test.models,"data.txt",sep="")
test.datafile<-file.path('C:/Program Files/OpenBUGS/OpenBUGS321/Examples',test.datafile)
test.inits = paste(test.models,"inits.txt",sep="")
test.inits<-file.path('C:/Program Files/OpenBUGS/OpenBUGS321/Examples',test.inits)

### Test for posterior means within 1 percent of previously saved values

res.true <- vector(mode="list",length=length(test.modelfile))
for (i in seq(along=test.models)) {
    res.true[[i]]<-round(bugs(data=test.datafile[i], inits=test.inits[i],
              para=test.params[[test.models[i]]],model.file=test.modelfile[i], 
              n.burnin=5000, n.iter=20000, n.thin=1, n.chains=1, DIC=FALSE)$summary,5)

}

dput(res.true,'validOpenBUGSResults.R')



                    

