\name{bugs}
\alias{bugs}
\title{Run OpenBUGS from R}

\description{The \code{bugs} function takes data and starting values as
  input.  It automatically writes a \pkg{OpenBUGS} script, calls the model, and
  saves the simulations for easy access in \R.}

\usage{
bugs(data, inits, parameters.to.save, n.iter, model.file="model.txt",
    n.chains=3, n.burnin=floor(n.iter / 2), n.thin=1, 
    saveExec=FALSE,restart=FALSE,
    debug=FALSE, DIC=TRUE, digits=5, codaPkg=FALSE,
    OpenBUGS.pgm=NULL,
    working.directory=NULL,
    clearWD=FALSE, useWINE=FALSE, WINE=NULL,
    newWINE=TRUE, WINEPATH=NULL, bugs.seed=1, summary.only=FALSE,
    save.history=(.Platform$OS.type == "windows" | useWINE==TRUE), 
    over.relax = FALSE) 
}

\arguments{
  \item{data}{either a named list (names corresponding to variable names
    in the \code{model.file}) of the data for the \pkg{OpenBUGS} model,
    \emph{or} a vector or list of the names of the data objects used by
    the model. If \code{data} is a one element character vector (such as \code{"data.txt"}), 
    it is assumed that data have already been written to the working directory into that file, 
    e.g. by the function \code{\link{bugs.data}}.}
  \item{inits}{a list with \code{n.chains} elements; each element of the
    list is itself a list of starting values for the \pkg{OpenBUGS} model,
    \emph{or} a function creating (possibly random) initial values.
    Alternatively, if \code{inits=NULL}, initial values are generated
    by \pkg{OpenBUGS}. If \code{inits} is a character vector with \code{n.chains} elements, 
    it is assumed that inits have already been written to the working directory into those files, 
    e.g. by the function \code{\link{bugs.inits}}.}
  \item{parameters.to.save}{character vector of the names of the
    parameters to save which should be monitored}
  \item{model.file}{File containing the model written in \pkg{OpenBUGS} code.  The 
    extension must be \file{.txt}.  The default location is given by \code{working.directory}.
    The old convention allowing model.file to be named \file{.bug} has been eliminated
    because the new \pkg{OpenBUGS} feature that allows the program image
    to be saved and later restarted uses the .bug extension for the saved
    images.  
    Alternatively, \code{model.file} can be an R function that contains a BUGS model that is written to a 
    temporary model file (see \code{\link{tempfile}}) using \code{\link{write.model}}.}
  \item{n.chains}{number of Markov chains (default: 3)}
  \item{n.iter}{number of total iterations per chain (including burn in;
    default: 2000)}
  \item{n.burnin}{length of burn in, i.e. number of iterations to
    discard at the beginning. Default is \code{n.iter/2}, that is,
    discarding the first half of the simulations.}
  \item{n.thin}{Thinning rate. Must be a positive integer. The default is 
    \code{n.thin} = 1. The thinning is implemented in the OpenBUGS update phase, so
    thinned samples are never stored, and they are not counted in \code{n.burnin} or
    \code{n.iter}.  Setting \code{n.thin}=2, doubles the number of iterations OpenBUGS 
    performs, but does not change \code{n.iter} or \code{n.burnin}.  Thinning implemented
    in this manner is not captured in summaries created by packages such as \pkg{coda}.}
  \item{saveExec}{If TRUE, a re-startable image of the OpenBUGS execution is
  saved with \code{basename (model.file)} and extension .bug in the working
  directory, which must be specified.  The .bug files can be large, so 
  users should monitor them carefully and remove them when not needed.
}
  \item{restart}{If TRUE, execution resumes with the final status from
  the previous execution stored in the .bug file in the working directory.
  If \code{n.burnin=0},additional iterations are performed and all iterations since
  the previous burnin are used (including those from past executions).  If
 \code{n.burnin>0}, a new burnin is performed, and the previous iterations are 
  discarded, but execution continues from the status at the end of the previous
  execution.  When \code{restart=TRUE}, only \code{n.burnin}, \code{n.iter}, 
  and \code{saveExec} inputs 
  should be changed from the call creating the .bug file, otherwise 
  failed or erratic results may be produced.  Note the default has \code{n.burnin>0}.
}
  \item{debug}{if \code{FALSE} (default), \pkg{OpenBUGS} is closed automatically
    when the script has finished running, otherwise \pkg{OpenBUGS} remains open
    for further investigation.  The debug option is not available for linux execution.}
  \item{DIC}{logical; if \code{TRUE} (default), compute deviance, pD,
    and DIC.  The results are extracted directly
    from the \pkg{OpenBUGS} log, which uses the rule \code{pD =
    Dbar - Dhat}.  If extraction fails or 
    if there are less iterations than required for the
    adaptive phase, the rule \code{pD=var(deviance) / 2} 
    is computed in R.  See \code{\link{bugs.log}} for more 
    information on extracting results from the log file.}
  \item{digits}{number of significant digits used for \pkg{OpenBUGS} input, see
    \code{\link{formatC}}}
  \item{codaPkg}{logical; if \code{FALSE} (default) a \code{bugs} object
    is returned, if \code{TRUE} file names of \pkg{OpenBUGS} output are
    returned for easy access by the \pkg{coda} package through function
    \code{\link{read.bugs}}.
    A \code{bugs} object can be converted to an \code{mcmc.list} object as 
    used by the \pkg{coda} package with the method \code{\link[coda]{as.mcmc.list}} 
    (for which a method is provided by R2OpenBUGS).}
  \item{OpenBUGS.pgm}{ For Windows or WINE execution, the full path 
    to the OpenBUGS
    executable.  For linux execution, the full path to the 
    OpenBUGS executable or shell script (the path to the shell
    script is not
    required if the OpenBUGS shell script is in the user's PATH variable).  
    If \code{NULL} (unset) and the environment variable 
    \code{OpenBUGS_PATH} is set the latter will be used as the default. 
    If \code{NULL} (unset), the environment variable 
    \code{OpenBUGS_PATH} is unset and the global option 
    \code{R2OpenBUGS.pgm} is not \code{NULL} the latter will be 
    used as the default.
    If none of the former are set and OS is Windows, the 
    most recent OpenBUGS version 
    registered in the Windows registry  will be used as the default.  
    For other operating systems, the location is guessed by \code{Sys.which("OpenBUGS")}.
    }
  \item{working.directory}{sets working directory during execution of
    this function; \pkg{OpenBUGS}' input and output will be stored in this
    directory; if \code{NULL}, a temporary working directory via
    \code{\link{tempdir}} is used.}
  \item{clearWD}{logical; indicating whether the files \file{data.txt},
    \file{inits[1:n.chains].txt}, \file{log.odc}, \file{codaIndex.txt},
    and \file{coda[1:nchains].txt} should be removed after \pkg{OpenBUGS} has
    finished.  If set to \code{TRUE}, this argument is only respected if
    \code{codaPkg=FALSE}.}
  \item{useWINE}{logical; attempt to use the Wine emulator to run
    \pkg{OpenBUGS}. Default is \code{FALSE}. If WINE is used, the arguments \code{OpenBUGS.pgm} and \code{working.directory} must be given in form of Linux paths
    rather than Windows paths (if not \code{NULL}).}
  \item{WINE}{Character, path to \file{wine} binary file, it is
    tried hard (by a guess and the utilities \code{which} and \code{locate}) 
    to get the information automatically if not given.}
  \item{newWINE}{Use new versions of Wine that have \file{winepath}
    utility}
  \item{WINEPATH}{Character, path to \file{winepath} binary file, it is
    tried hard (by a guess and the utilities \code{which} and \code{locate}) 
    to get the information automatically if not given.}
  \item{bugs.seed}{Random seed for \pkg{OpenBUGS}.  Must be an integer between 1-14.  Seed 
    specification changed between WinBUGS and OpenBUGS;  see the OpenBUGS documentation for 
    details.}
  \item{summary.only}{If \code{TRUE}, only a parameter summary for very quick analyses is given, 
    temporary created files are not removed in that case.}
  \item{save.history}{If \code{TRUE} (the default), trace plots are generated at the end.}
  \item{over.relax}{If \code{TRUE}, over-relaxed form of MCMC is used if available from OpenBUGS.}
}

\details{
  To run:
  \enumerate{
    \item Write a \pkg{BUGS} model in an ASCII file (hint: use
      \code{\link{write.model}}).
    \item Go into \R.
    \item Prepare the inputs for the \code{bugs} function and run it (see
      Example section).
    \item An \pkg{OpenBUGS} window will pop up and \R will freeze
    up. The model will now run in \pkg{OpenBUGS}. It might take awhile. You
    will see things happening in the Log window within \pkg{OpenBUGS}. When
    \pkg{OpenBUGS} is done, its window will close and \R will work
    again.
    \item If an error message appears, re-run with \code{debug=TRUE}.
  }

  BUGS version support:
  \itemize{
    \item{\pkg{OpenBUGS} }{>=3.2.1}
  }

  Operation system support:
  \itemize{
    \item{\pkg{MS Windows}}{ no problem}
    \item{\pkg{Linux, intel processors}}{ GUI display and graphics not available.}
    \item{\pkg{Mac OS X} and \pkg{Unix} in
      general}{ possible with Wine emulation via \code{useWINE=TRUE}}
  }

  If \code{useWINE=TRUE} is used, all paths (such as
  \code{working.directory} and \code{model.file}, must be given in
  native (Unix) style, but \code{OpenBUGS.pgm} can be given in
  Windows path style (e.g. \dQuote{c:/Program Files/OpenBUGS/}) or
  native (Unix) style \cr
  (e.g. \dQuote{/path/to/wine/folder/dosdevices/c:/Program
  Files/OpenBUGS/OpenBUGS321/OpenBUGS.exe}).  

}

\value{
  If \code{codaPkg=TRUE} the returned values are the names
  of coda output files written by \pkg{OpenBUGS} containing
  the Markov Chain Monte Carlo output in the CODA format.
  This is useful for direct access with \code{\link{read.bugs}}.

  If \code{codaPkg=FALSE}, the following values are returned:
  \item{n.chains}{see Section \sQuote{Arguments}}
  \item{n.iter}{see Section \sQuote{Arguments}}
  \item{n.burnin}{see Section \sQuote{Arguments}}
  \item{n.thin}{see Section \sQuote{Arguments}}
  \item{n.keep}{number of iterations kept per chain (equal to
    \code{(n.iter-n.burnin) / n.thin})}
  \item{n.sims}{number of posterior simulations (equal to
    \code{n.chains * n.keep})}
  \item{sims.array}{3-way array of simulation output, with dimensions
    n.keep, n.chains, and length of combined parameter vector}
  \item{sims.list}{list of simulated parameters:
    for each scalar parameter, a vector of length n.sims
    for each vector parameter, a 2-way array of simulations,
    for each matrix parameter, a 3-way array of simulations, etc.
    (for convenience, the \code{n.keep*n.chains} simulations in
    sims.matrix and sims.list (but NOT sims.array) have been randomly
    permuted)}
  \item{sims.matrix}{matrix of simulation output, with
    \code{n.chains*n.keep} rows and one column for each element of
    each saved parameter (for convenience, the \code{n.keep*n.chains}
    simulations in sims.matrix and sims.list (but NOT sims.array) have
    been randomly permuted)}
  \item{summary}{summary statistics and convergence information for
    each saved parameter.}
  \item{mean}{a list of the estimated parameter means}
  \item{sd}{a list of the estimated parameter standard deviations}
  \item{median}{a list of the estimated parameter medians}
  \item{root.short}{names of argument \code{parameters.to.save} and
    \dQuote{deviance}}
  \item{long.short}{indexes; programming stuff}
  \item{dimension.short}{dimension of \code{indexes.short}}
  \item{indexes.short}{indexes of \code{root.short}}
  \item{last.values}{list of simulations from the most recent
    iteration; they can be used as starting points if you wish to run
    \pkg{OpenBUGS} for further iterations}
  \item{pD}{an estimate of the effective number of parameters, for
    calculations see the section \dQuote{Arguments}.}
  \item{DIC}{\code{mean(deviance) + pD}}
}

\references{
    Gelman, A., Carlin, J.B., Stern, H.S., Rubin, D.B. (2003):
    \emph{Bayesian Data Analysis}, 2nd edition, CRC Press.

     Sturtz, S., Ligges, U., Gelman, A. (2005):
     R2WinBUGS: A Package for Running WinBUGS from R.
     \emph{Journal of Statistical Software} 12(3), 1-16.
}

\author{Andrew Gelman, \email{gelman@stat.columbia.edu},
  \url{http:/www.stat.columbia.edu/~gelman/bugsR/}; modifications and
  packaged by Sibylle Sturtz, \email{sturtz@statistik.tu-dortmund.de},
  Uwe Ligges, and Neal Thomas}

\seealso{\code{\link{print.bugs}}, \code{\link{plot.bugs}}, as well as
  \pkg{coda} and \pkg{BRugs} packages}

\examples{
# An example model file is given in:
model.file <- system.file(package="R2OpenBUGS", "model", "schools.txt")
# Let's take a look:
file.show(model.file)

# Some example data (see ?schools for details):
data(schools)
schools

J <- nrow(schools)
y <- schools$estimate
sigma.y <- schools$sd
data <- list ("J", "y", "sigma.y")
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

parameters <- c("theta", "mu.theta", "sigma.theta")

\dontrun{
## You may need to specify "OpenBUGS.pgm"
## also you need write access in the working directory:
schools.sim <- bugs(data, inits, parameters, model.file,
    n.chains=3, n.iter=5000)
print(schools.sim)
plot(schools.sim)
}
}

\keyword{interface}
\keyword{models}
