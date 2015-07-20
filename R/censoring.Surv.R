#' Describe Censoring
#'
#' Gets the type of censoring ("none," "left," "multiple") for an object.
#'
#'
#' @param x the object to get the type of censoring. For an object of class
#'"Surv," the type must be "interval."
#' @return A character string "none," "left," or "multiple" describing the type
#'of censoring present in the object.
#' @note This function is mostly used within other functions to determine the
#''best' technique to use for analysis.
#' @keywords censored attribute
#' @examples
#'\dontrun{
#'library(survival)
#'censoring(Surv(2.3, 2.3, type="interval2"))
#'}
#' @export
#' @method censoring Surv
censoring.Surv <- function(x) {
  if(attr(x, "type") != "interval")
    stop("Only interval type censoring supported")
  # Strip NAs from x
  x <- x[!is.na(x)]
  if(all(x[, 3] == 1)) {
    return("none")
  } else if(any(x[, 3] %in% c(0,3))) {
    return("multiple")
  }
  return("left")
}

