#'Root-Mean-Squared Error
#'
#'Compute the root-mean-squared error (RMSE) of the difference between
#'observed values and the fitted values for the load or concentration model.
#'The RMSEs will be the same unless the log of flow is not an 
#'explanatory variable.
#'
#' @param x the output from \code{loadReg}.
#' @param model the type of model, must be either "load" or "concentration."
#' @param \dots not used, required for other methods.
#' @return The estimated root-mean-squared error, also know as the residual
#'standard error.
#' @seealso \code{\link{loadReg}},
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'rmse(app1.lr)
#' @import USGSwsStats
#' @export
#' @method rmse loadReg
rmse.loadReg <- function(x, model=c("load", "concentration"), ...) {
  ## Coding history:
  ##    2013Jun21 DLLorenz Original Coding
  ##
  model <- match.arg(model)
  object <- if(model == "load") x$lfit else x$cfit
  return(rmse(object))
}
