#' Attach / detach elements of (bugs) objects to search path
#'
#' The database is attached/detached to the search path. See
#' \code{\link{attach}} for details.
#'
#' While \code{attach.all} attaches all elements of an object \code{x} to a
#' database called \code{name}, \code{attach.bugs} attaches all elements of
#' \code{x$sims.list} to the database \code{bugs.sims} itself making use of
#' \code{attach.all}.
#'
#' \code{detach.all} and \code{detach.bugs} are removing the databases
#' mentioned above.
#'
#' \code{attach.all} also attaches \code{n.sims} (the number of simulations
#' saved from the MCMC runs) to the database.
#'
#' Each scalar parameter in the model is attached as vectors of length
#' \code{n.sims}, each vector is attached as a 2-way array (with first
#' dimension equal to \code{n.sims}), each matrix is attached as a 3-way array,
#' and so forth.
#'
#' @param x An object, which must be of class \code{bugs} for
#' \code{attach.bugs}.
#' @param overwrite If \code{TRUE}, objects with identical names in the
#' Workspace (.GlobalEnv) that are masking objects in the database to be
#' attached will be deleted. If \code{NA} (the default) and an interactive
#' session is running, a dialog box asks the user whether masking objects
#' should be deleted.  In non-interactive mode, behaviour is identical to
#' \code{overwrite=FALSE}, i.e. nothing will be deleted.
#' @param name The name of the environment where \code{x} will be attached /
#' which will be detached.
#' @return \code{attach.all} and \code{attach.bugs} invisibly return the
#' \code{\link{environment}}(s).
#'
#' \code{detach.all} and \code{detach.bugs} detach the \code{environment}(s)
#' named \code{name} created by \code{attach.all}.
#' @note Without detaching, do not use \code{attach.all} or \code{attach.bugs}
#' on another (\code{bugs}) object, because instead of the given name, an
#' object called \code{name} is attached. Therefore strange things may happen
#' \ldots{}
#' @seealso \code{\link{bugs}}, \code{\link{attach}}, \code{\link{detach}}
#' @keywords data
#' @examples
#'
#' # An example model file is given in:
#' model.file <- system.file('model', 'schools.txt', package='R2MultiBUGS')
#' # Some example data (see ?schools for details):
#' data(schools)
#' J <- nrow(schools)
#' y <- schools$estimate
#' sigma.y <- schools$sd
#' data <- list ('J', 'y', 'sigma.y')
#' inits <- function(){
#'     list(theta = rnorm(J, 0, 100), mu.theta = rnorm(1, 0, 100),
#'         sigma.theta = runif(1, 0, 100))
#' }
#' parameters <- c('theta', 'mu.theta', 'sigma.theta')
#' \dontrun{
#' ## See ?bugs if the following fails:
#' schools.sim <- bugs(data, inits, parameters, model.file,
#'     n.chains = 3, n.iter = 1000,
#'     working.directory = NULL)
#'
#' # Do some inferential summaries
#' attach.bugs(schools.sim)
#' # posterior probability that the coaching program in school A
#' # is better than in school C:
#' print(mean(theta[,1] > theta[,3]))
#' # 50% posterior interval for the difference between school A's
#' # and school C's program:
#' print(quantile(theta[,1] - theta[,3], c(.25, .75)))
#' plot(theta[,1], theta[,3])
#' detach.bugs()
#' }
#'
#' @aliases attach.all detach.all attach.bugs detach.bugs
#' @export attach.all
attach.all <- function(x, overwrite = NA, name = "attach.all"){
  rem <- names(x) %in% ls(.GlobalEnv)
  if (!any(rem)){
    overwrite <- FALSE
  }
  rem <- names(x)[rem]
  if (is.na(overwrite)){
    question <- paste(
      "The following objects in .GlobalEnv will mask\n",
      "objects in the attached database:\n",
      paste(rem, collapse = ", "),
      "\nRemove these objects from .GlobalEnv?",
      sep = "")
    if (interactive()){
      if (.Platform$OS.type == "windows"){
        overwrite <- "YES" == utils::winDialog(type = "yesno", question)
      } else {
        overwrite <- 1 == menu(c("YES", "NO"),
                               graphics = FALSE,
                               title = question)
      }
    } else overwrite <- FALSE
  }
  if (overwrite){
    remove(list = rem, envir = .GlobalEnv)
  }
  attach(x, name = name)
}

#' @export
attach.bugs <- function(x, overwrite = NA){
  if (class(x) != "bugs"){
    stop("attach.all() requires a bugs object.")
  }
  if ("bugs.sims" %in% search()){
    detach("bugs.sims")
  }
  x$sims.list$n.sims <- x$n.sims  # put n.sims into sims.list for convenience
  r2 <- attach.all(x$sims.list, overwrite = overwrite, name = "bugs.sims")
  invisible(r2)
}

#' @export
detach.all <- function(name = "attach.all"){
  do.call("detach", list(name = name))
}

#' @export
detach.bugs <- function(){
  detach.all("bugs.sims")
}
