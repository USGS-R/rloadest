#' Extract Model Coefficients
#'
#' Extract the model coefficients from a load regression.
#'
#' @param object the output from \code{loadReg}.
#' @param summary include standard errors and other information?
#' @param which string indicating which coefficients to return;
#'"load" returns the load model and "concentration" returns the
#'concentration model coefficients.
#' @param \dots further arguments passed to or from other methods.
#' @return Either a names vector of the coefficients, if \code{summary} is
#'\code{FALSE} or a matrix of the coefficients, their standard errors,
#'z-scores, and attained p-values, if \code{summary} is \code{TRUE}.
#' @note The attained p-values are computed from the log-likelihood test for
#'AMLE regression and from a Wald chi-square test for MLE regression.
#' @seealso \code{\link{loadReg}},
#' @keywords regression
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'# Extract the coefficients
#'coef(app1.lr)
#' @S3method coef loadReg
#' @method coef loadReg
coef.loadReg <- function(object, summary=FALSE, which="load", ...) {
  ## Coding history:
  ##    2013Jun20 DLLorenz Original Coding
  ##
  which <- match.arg(which, c("load", "concentration"))
  if(which == "load")
    object <- object$lfit
  else
    object <- object$cfit
  return(coef(object, summary=summary))
}
