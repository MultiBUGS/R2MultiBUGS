# R2MultiBUGS <a href="https://www.multibugs.org"><img src="https://www.multibugs.org/images/logo.svg" align="right" width = 130></a>

R2MultiBUGS is an R interface to [MultiBUGS](https://www.multibugs.org).
MultiBUGS is a parallel implementation of the BUGS modelling framework.

R2MultiBUGS is a port of [R2OpenBUGS](https://cran.r-project.org/package=R2OpenBUGS) for MultiBUGS.
R2OpenBUGS was 
> originally written as R2WinBUGS by Andrew Gelman; changes and packaged by Sibylle Sturtz and Uwe Ligges. With considerable contributions by Gregor Gorjanc and Jouni Kerman. Adapted to R2OpenBUGS from R2WinBUGS by Neal Thomas.''

## Installation

R2MultiBUGS requires MultiBUGS, which currently works only on Windows. (A Linux port is under preparation.)
See the main [MultiBUGS website](https://multibugs.github.io) for installation instructions for MultiBUGS.

The current development version of R2MultiBUGS can be installed in R using

``` r
# install.packages("devtools")
devtools::install_github("MultiBUGS/R2MultiBUGS")
```

## Usage

The main functions in R2MultiBUGS are the same as R2OpenBUGS, except you specify the number of workers to be used via the `n.workers` argument to `bugs()`

``` r
# An example model file is given in:
model.file <- system.file(package='R2MultiBUGS', 'model', 'schools.txt')
# Let's take a look:
#file.show(model.file)

# Some example data (see ?schools for details):
data(schools)
schools

J <- nrow(schools)
y <- schools$estimate
sigma.y <- schools$sd
data <- list ('J', 'y', 'sigma.y')
inits <- function(){
    list(theta=rnorm(J, 0, 100), mu.theta=rnorm(1, 0, 100),
         sigma.theta=runif(1, 0, 100))
}
## or alternatively something like:
# inits <- list(
#   list(theta=rnorm(J, 0, 90), mu.theta=rnorm(1, 0, 90),
#        sigma.theta=runif(1, 0, 90)),
#   list(theta=rnorm(J, 0, 100), mu.theta=rnorm(1, 0, 100),
#        sigma.theta=runif(1, 0, 100))
#   list(theta=rnorm(J, 0, 110), mu.theta=rnorm(1, 0, 110),
#        sigma.theta=runif(1, 0, 110)))

parameters <- c('theta', 'mu.theta', 'sigma.theta')

## You may need to specify 'MultiBUGS.pgm'
## also you need write access in the working directory:
schools.sim <- bugs(data, inits, parameters, model.file,
    n.chains=3, n.workers = 2, n.iter=5000)
print(schools.sim)
plot(schools.sim)
```