#' Center Time
#' 
#' Internal support function for rloadest that computes
#'the adjustment to time.
#'
#' @param x the calibration date data.
#' @param round either a numeric value indicating the number
#'of decimal places, or a list containing a value indicating
#'the number of decimal places. If \code{NULL}, then do no round.
#'
#' @return A vector of length 2 containing the base (reference)
#'year and the centering time correction value. The user value
#'for centered time would be sum.
#'
loadestTadj <- function(x, round=options("round.time")) {
  ## Coding history:
  ##    2013May31 DLLorenz Original version from S+ library
  ##
  x <- dectime(x)
  Tadj <- c(0,0)
  Tadj[1L] <- floor(min(x, na.rm=TRUE))
  meant <- mean(x, na.rm=TRUE)
  Tadj[2L] <- mean(x - Tadj[1], na.rm=TRUE) + 
    sum((x - meant)^3) / 2 / sum((x - meant)^2)
  retval <- Tadj
  if(is.list(round))
    round <- round[[1L]]
  if(!is.null(round)) {
    retval <- round(retval, round)
  }
  return(retval)
}
