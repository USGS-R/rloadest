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
#' @param criterion the criterion to use for subset selection, must be 
#'one of "AIC," "SPCC," or "AICc."
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
                         time.step="day", station="", 
                         criterion=c("AIC", "SPCC", "AICc")) {
  ##
  criterion <- match.arg(criterion)
  ##
  ## Modify to make call to loadReg
  cl <- match.call()
  cl[[1L]] <- as.name("loadReg")
  names(cl)[2L] <- "formula"
  cl[[2L]] <- as.formula(paste(constituent, " ~ model(1)", sep=""))
  cl$criterion <- NULL
  retval <- model <- eval(cl)
  model.eval <- model$model.eval
  # Append AICc
  model.eval$AICc <- AICc(model)
  AICbest <- model.eval[[criterion]]
  ## Supress multiple warnings
  warn <- options("warn")
  options(warn=-1L)
  for(model.no in seq(2L, 9L)) {
    cl[[2L]] <- as.formula(paste(constituent, " ~ model(", model.no, ")", sep=""))
    model <- eval(cl)
    model.tmp <- model$model.eval
    model.tmp$AICc <- AICc(model)
    model.eval <- rbind(model.eval, model.tmp)
    if(model.tmp[[criterion]] < AICbest) {
      AICbest <- model.tmp[[criterion]]
      retval <- model
    }
  }
  options(warn)
  ## But warn if fewer N than 70
  if(model$lfit$NOBSC < 70L)
    warning("Selected model may be over fit: only ", 
            model$lfit$NOBSC, " observations.")
  ## Replace model.eval with complete list
  retval$model.eval <- model.eval
  return(retval)
}
