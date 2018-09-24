#' Convert function to a MultiBUGS model file
#' 
#' BUGS models follow closely S syntax. It is therefore possible to write most
#' BUGS models as functions.
#' 
#' As a difference, BUGS syntax allows truncation specification like this:
#' \code{dnorm(...) I(...)} but this is illegal in . To overcome this
#' incompatibility, use dummy operator \code{\%_\%} before \code{I(...)}:
#' \code{dnorm(...) \%_\% I(...)}. The dummy operator \code{\%_\%} will be removed
#' before the BUGS code is saved.
#' 
#' @param model function containing the BUGS model in the BUGS model language,
#' for minor differences see Section Details.
#' @param con passed to \code{\link{writeLines}} which actually writes the
#' model file
#' @param digits number of significant digits used for \pkg{MultiBUGS} input,
#' see \code{\link{formatC}}
#' @return Nothing, but as a side effect, the model file is written
#' @author original idea by Jouni Kerman, modified by Uwe Ligges
#' @seealso \code{\link{bugs}}
#' @keywords IO file
#' @examples
#' 
#' ## Same "schoolsmodel" that is used in the examples in ?bugs:
#' schoolsmodel <- function(){
#'     for (j in 1:J){
#'         y[j] ~ dnorm (theta[j], tau.y[j])
#'         theta[j] ~ dnorm (mu.theta, tau.theta)
#'         tau.y[j] <- pow(sigma.y[j], -2)
#'     }
#'     mu.theta ~ dnorm (0.0, 1.0E-6)
#'     tau.theta <- pow(sigma.theta, -2)
#'     sigma.theta ~ dunif (0, 1000)
#' }
#' 
#'     ## some temporary filename:
#'     filename <- file.path(tempdir(), "schoolsmodel.bug")
#' 
#' ## write model file:
#' write.model(schoolsmodel, filename)
#' ## and let's take a look:
#' file.show(filename)
#' 
#' @export write.model
write.model <- function(model, con = "model.bug", digits = 5)
{
  model.text <- c("model", replaceScientificNotationR(body(model), digits = digits))
  # "[\+\-]?\d*\.?[Ee]?[\+\-]?\d*"
  model.text <- gsub("%_%", "", model.text)
  writeLines(model.text, con = con)
}

replaceScientificNotationR <- function(bmodel, digits = 5){
    env <- new.env()
    assign("rSNRidCounter", 0, envir=env)
    replaceID <- function(bmodel, env, digits = 5){
        for(i in seq_along(bmodel)){
            if(length(bmodel[[i]]) == 1){
                if(as.character(bmodel[[i]]) %in% c(":", "[", "[[")) return(bmodel)
                if((typeof(bmodel[[i]]) %in% c("double", "integer")) && ((abs(bmodel[[i]]) < 1e-3) || (abs(bmodel[[i]]) > 1e+4))){
                    counter <- get("rSNRidCounter", envir=env) + 1
                    assign("rSNRidCounter", counter, envir=env)
                    id <- paste("rSNRid", counter, sep="")
                    assign(id, formatC(bmodel[[i]], digits=digits, format="E"), envir=env)
                    bmodel[[i]] <- id
                }
            } else {
                bmodel[[i]] <- replaceID(bmodel[[i]], env, digits = digits)
            }
        }
        bmodel
    }
    bmodel <- deparse(replaceID(bmodel, env, digits = digits), control = NULL)
    for(i in ls(env)){
        bmodel <- gsub(paste('"', i, '"', sep=''), get(i, envir=env), bmodel, fixed=TRUE)
    }
    bmodel
}
