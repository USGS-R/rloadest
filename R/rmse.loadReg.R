#'Root-Mean-Squared Error
#'
#'Compute the root-mean-squared error (RMSE) of the difference between
#'observed values and the fitted values
#'
#'@param x the output from \code{loadReg}.
#'@param \dots further arguments passed to or from other methods.
#'@return The estimated root-mean-squared error, also know as the residual
#'standard error.
#'@seealso \code{\link{loadReg}},
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'rmse(app1.lr, app1.calib)
#'@S3method rmse loadReg
#'@method rmse loadReg
rmse.loadReg <- function(x, ...) {
  ## Coding history:
  ##    2013Jun21 DLLorenz Original Coding
  ##
  return(rmse(x$lfit))
}
