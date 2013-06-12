#' Utility Function for Safe Prediction
#' 
#' A utility to help \code{\link{model.frame.default}} create the right matrices 
#'when predicting from models with \code{center} term. Used only internally.
#' 
#' @param var a variable.
#' @param call the term in the formula, as a call.
#' @return A replacement for \code{call} for the prediction variable.
#' @S3method makepredictcall center
#' @method makepredictcall center
makepredictcall.center <- function(var, call) {
  if (as.character(call)[1L] != "center") 
    return(call)
  call$center <- attr(var, "xstar")
  return(call)
}
