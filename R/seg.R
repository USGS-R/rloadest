#' Load Model
#'
#' Support function for building a segmented rating curve load model. Required 
#'in the formula in \code{segLoadReg} to define the segmented model.
#'
#' @param x the data to segment. Missing values are permitted and result 
#'corresponing in missing values in output.
#' @param N the number of breaks in the segmented model.
#'
#' @return The data in \code{x} with attributes to build the segmented model.
#'
#' @export
seg <- function(x, N) {
  if(missing(N))
    stop("The number of segment breaks must be specified")
  attr(x, "N") <- N
  return(x)
}
