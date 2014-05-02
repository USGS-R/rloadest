#'Akaike's An Information Criterion with Correction
#'
#'Compute Akaike's An Information Criterion with Correction (AICc) for
#'for finite sample sizes. 
#'
#' @param object the output from \code{loadReg}, or any object that has a
#'\code{logLik} method.
#' @return A numeric value corresponding to the AICc of \code{object}.
#' @note The penalty that \code{AIC} applies for adding explanatory variables 
#'is biased low when the number of samples is small. As a result, models with
#'small smaple sizes can be overfitted. \code{AICc} can be used to identify more
#'parsimonious models.
#' @seealso \code{\link{loadReg}},
#' @references Hurvitch, C.M. and Tsai, C.L., 1989, Regression and time series
#'model selection in small samples: Biometrika, v. 76, no. 2, p. 297--307.
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'AICc(app1.lr)
#' @export
AICc <- function(object) {
  ## Coding history:
  ##    2014Jan17 DLLorenz Original Coding
  ##
  ## Copy from AIC.default
  ll <- if ("stats4" %in% loadedNamespaces()) 
    stats4:::logLik
  else logLik
  ## Compute the AIC
  lls <- ll(object)
  k <- attr(lls, "df")
  retval <- -2 * as.numeric(lls) + 2 * k
  ## Compute the corrected value
  n <- attr(lls, "nobs")
  retval <- retval + 2*k*(k + 1)/(n - k - 1)
  return(retval)
}
