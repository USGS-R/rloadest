#'Extract Log-Likelihood
#'
#'Compute the log-likelihood statistics for a load regression. 
#'
#'@param object the output from \code{loadReg}.
#'@param \dots further arguments passed to or from other methods.
#'@return An object of class "logLik" containing the log-likelihood and
#'the attributes "df" (degrees of freedom) and "nobs" (number of observations).
#'@seealso \code{\link{loadReg}},
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'logLik(app1.lr)
#'@S3method logLik loadReg
#'@method logLik loadReg
logLik.loadReg <- function(object, ...) {
  ## Coding history:
  ##    2012Sep25 DLLorenz Original Coding
  ##    2012Dec31 DLLorenz Roxygenized
  ##    2012Dec31          This version
  ##
  ll <- object$lfit$LLR
  attr(ll, "df") <- object$lfit$NPAR + 1
  attr(ll, "nobs") <- object$lfit$NOBSC
  class(ll) <- "logLik"
  return(ll)
}
