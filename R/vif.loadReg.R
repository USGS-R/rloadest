#'Variance Inflation Factors
#'
#'Computes the variance inflation factor (Helsel and Hirsch, 2002) for each
#'variable in a load regression.
#'
#' @param model an object of class "loadReg"---output from \code{loadReg}.
#' @param \dots further arguments passed to or from other methods.
#' @return  further arguments passed to or from other methods.
#' @seealso \code{\link{loadReg}}
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'vif(app1.lr, app1.calib)
#' @keywords regression
#' @import USGSwsStats
#' @export
#' @method vif loadReg
vif.loadReg <- function(model, ...) {
  ## Coding history:
  ##    2013Jun21 DLLorenz Initial Coding
  ##
  return(vif(model$lfit))
}
