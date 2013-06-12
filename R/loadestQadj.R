#' Center Flow
#' 
#' Internal support function for rloadest that computes
#'the adjustment to flow.
#'
#' @param x the calibration flow data.
#' @param round either a numeric value indicating the number
#'of decimal places, or a list containing a value indicating
#'the number of decimal places. If \code{NULL}, then do no round.
#'
#' @return The centering value for flow.
#'
loadestQadj <- function(x, round=options("round.flow")) {
  ## Coding history:
  ##    2013May31 DLLorenz Original version from S+ library
  ##
  Q <- log(x)
  meanQ <- mean(Q, na.rm=T)
  Qstar <- meanQ + sum((Q - meanQ)^3) / 2 / sum((Q - meanQ)^2)
  if(is.na(Qstar)) Qstar <- meanQ # Allow for constant flow
  retval <- exp(Qstar)
  if(is.list(round))
    round <- round[[1L]]
  if(!is.null(round)) {
    retval <- round(retval, round)
  }
  return(retval)
}
