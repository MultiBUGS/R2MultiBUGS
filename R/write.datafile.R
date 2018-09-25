#' Write data for MultiBUGS
#'
#' Write data in files that can be read by \pkg{MultiBUGS} - intended for
#' internal use
#'
#' @aliases write.datafile formatdata
#' @param datalist a \emph{list} to be written into an appropriate structure
#' @param towhere the name of the file which the data are to be written to
#' @param fill see \code{\link{cat}}, defaults to \code{TRUE}
#' @return \item{datalist.tofile}{A structure appropriate to be read in by
#' \pkg{MultiBUGS}.}
#' @seealso The main function to be called by the user is \code{\link{bugs}}.
#' @keywords internal
write.datafile <- function(datalist, towhere, fill = TRUE){
  if (!is.list(datalist) || is.data.frame(datalist)){
    stop("First argument to write.datafile must be a list.")
  }
  cat(formatdata(datalist), file = towhere, fill = fill)
}
