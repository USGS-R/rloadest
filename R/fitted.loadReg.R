#'Extract Model Fitted Values
#'
#'Extract the fitted values of a load regression.
#'
#' @param object an object of class "loadReg"---output from \code{loadReg}
#' @param suppress.na.action logical, suppress the effects of the
#'\code{na.action} in the call to \code{loadReg} and return only the fitted
#'values corresponding to the fitted data.
#' @param which a character string indicating the type of fitted values. Must be
#'either "load" or "concentration."
#' @param \dots further arguments passed to or from other methods.
#' @return The fitted values from the regression. Note that these are not back-
#'transformed but are in natrual log units.
#' @seealso \code{\link{loadReg}}
#' @keywords regression
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'# Extract the fitted values
#'fitted(app1.lr)
#' @export
#' @method fitted loadReg
fitted.loadReg <- function(object, suppress.na.action=FALSE, which="load", ...) {
  ## Coding history:
  ##    2013Jun21 DLLorenz Original Coding
  ##
  which <- match.arg(which, c("load", "concentration"))
  if(which == "load")
    object <- object$lfit
  else
    object <- object$cfit
  return(fitted(object, suppress.na.action=suppress.na.action))
}
