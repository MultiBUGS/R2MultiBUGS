\name{write.model}
\alias{write.model}
\title{Creating a MultiBUGS model file}
\description{Convert \R function to a \pkg{MultiBUGS} model file}
\usage{
write.model(model, con = "model.bug", digits = 5)
}

\arguments{
  \item{model}{\R function containing the BUGS model in the BUGS
    model language, for minor differences see Section Details.}
  \item{con}{passed to \code{\link{writeLines}} which actually writes
    the model file}
  \item{digits}{number of significant digits used for \pkg{MultiBUGS}
    input, see \code{\link{formatC}}}
}

\value{Nothing, but as a side effect, the model file is written}

\details{

BUGS models follow closely S syntax. It is therefore possible to write
most BUGS models as \R functions.

As a difference, BUGS syntax allows truncation specification like this:
\code{dnorm(...) I(...)}  but this is illegal in \R. To
overcome this incompatibility, use dummy operator \code{\%_\%} before
\code{I(...)}: \code{dnorm(...) \%_\% I(...)}. The dummy operator
\code{\%_\%} will be removed before the BUGS code is saved.

}

\author{original idea by Jouni Kerman, modified by Uwe Ligges}
\examples{
## Same "schoolsmodel" that is used in the examples in ?bugs:
schoolsmodel <- function(){
    for (j in 1:J){
        y[j] ~ dnorm (theta[j], tau.y[j])
        theta[j] ~ dnorm (mu.theta, tau.theta)
        tau.y[j] <- pow(sigma.y[j], -2)
    }
    mu.theta ~ dnorm (0.0, 1.0E-6)
    tau.theta <- pow(sigma.theta, -2)
    sigma.theta ~ dunif (0, 1000)
}

    ## some temporary filename:
    filename <- file.path(tempdir(), "schoolsmodel.bug")

## write model file:
write.model(schoolsmodel, filename)
## and let's take a look:
file.show(filename)
}
\seealso{\code{\link{bugs}}}
\keyword{IO}
\keyword{file}
\concept{model}
