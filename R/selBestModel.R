#' Load Estimation
#'
#' Select the "best" predefined rating-curve (regression) model for 
#' river load estimation.
#'
#' @param constituent a character string giving the name of the response
#'variable for which loads are to be computed.
#' @param data the data to search for the variables in \code{formula}.
#' @param subset an expression to select a subset of the data.
#' @param na.action what to do with missing values.
#' @param flow character string indicating the name of the 
#'flow column.
#' @param dates character string indicating the name of the 
#'date column.
#' @param flow.units character string describing the flow unit.
#' @param conc.units character string describing the concentration 
#'unit.
#' @param load.units character string describing the load unit.
#' @param time.step character string describing the time step of 
#'the calibration data.
#' @param station character string description of the station.
#'
#' @return An object of class "loadReg."
#' @seealso \code{\link{censReg}}
#' @references will need some.
#' @keywords regression censored loads
#' @examples
#'# Use the data from application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- selBestModel("Phosphorus", data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'# Extract the fitted values
#'print(app1.lr)
#'@export
selBestModel <- function(constituent, data, subset, na.action, flow, dates,
                         flow.units="cfs", conc.units="", load.units="kg", 
                         time.step="day", station="") {
  ## Coding history:
  ##    2013May31 DLLorenz Original Coding
  ##
  ## Modify to make call to loadReg
  cl <- match.call()
  cl[[1L]] <- as.name("loadReg")
  names(cl)[2L] <- "formula"
  cl[[2L]] <- as.formula(paste(constituent, " ~ model(1)", sep=""))
  retval <- model <- eval(cl)
  model.eval <- model$model.eval
  # Append AICc
  model.eval$AICc <- AICc(model)
  AICbest <- model.eval$AIC
  ## Supress multiple warnings
  warn <- options("warn")
  options(warn=-1L)
  for(model.no in seq(2L, 9L)) {
    cl[[2L]] <- as.formula(paste(constituent, " ~ model(", model.no, ")", sep=""))
    model <- eval(cl)
    model.tmp <- model$model.eval
    model.tmp$AICc <- AICc(model)
    model.eval <- rbind(model.eval, model.tmp)
    if(model$model.eval$AIC < AICbest) {
      AICbest <- model$model.eval$AIC
      retval <- model
    }
  }
  options(warn)
  ## Replace model.eval with complete list
  retval$model.eval <- model.eval
  return(retval)
}
