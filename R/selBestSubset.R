#' @title Load Estimation
#' 
#' @description Select the "best" subset of a user-defined rating-curve (regression)
#'model for rver load estimation.
#'
#' @param formula a formula describing the regression model. See \code{\link{loadReg}}
#'for details.
#' @param min.formula a formula containing the minimum variables to use in the final
#'model. The default is to only force the intercept term, whihc will normally be
#'acceptable. In some rare cases, the log of flow may be needed.
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
#' @param criterion the criterion to use for subset selection, must be either "AIC"
#'or "SPPC."
#'
#' @seealso \code{\link{censReg}}, \code{\link[stats]{step}}
#' @return An object of class "loadReg."
#' @note The printed output of the model inlcudes the \code{anova} component from 
#'\code{step}. That table summarizes the step wise selection and the criterion used
#'for each step. The statistics possibly represent a smaller  sample size than used
#'for the final model because the \code{step} function requires a data set with no
#'missing values. If missing values are found a warning is printed.
#' @importFrom stats step
#' @keywords regression censored loads
#' @export
selBestSubset <- function(formula, min.formula=~1, data, subset, na.action, flow, 
													dates, flow.units = "cfs", conc.units = "", load.units = "kg",
													time.step = "day", station = "", criterion=c("AIC", "SPPC")) {
	# First extract the call to construct a data frame for the model
	criterion <- match.arg(criterion)
	m <- call <- match.call()
	m[[1L]] <- as.name("get_all_vars")
	m$min.formula <- m$flow <- m$dates <- m$flow.units <- m$conc.units <- NULL
	m$load.units <- m$time.step <- m$station <- m$criterion <- NULL
	m$na.action <- NULL # get_all_vars returns all data, so not needed (creates error)
	df <- eval(m)
	Q <- data[[flow]] # Retain to compute the load later on
	Misses <- sapply(df, function(x) sum(is.na(x)))
	if(any(Misses > 0)) {
		Misses <- paste(names(Misses), Misses, sep=": ", collapse=", ")
		Nstart <- nrow(df)
		df <- na.omit(df)
		# subset the flow:
		Q <- Q[-attr(df, "na.action")]
		Nend <- nrow(df)
		warning(c("Missing values found\n", Misses, "\n",
							Nend, " rows remaining from original ", Nstart, sep=""))
	}
	# OK, now df is a data set with no missing values that will work in step.
	# Convert the Y column to lcens or mcens and flux
	if (class(df[[1L]]) == "qw") {
		if (conc.units == "") {
			conc.units <- unique(df[[1L]]@reporting.units)
			conc.units <- conc.units[!is.na(conc.units)]
			if (length(conc.units) == 0L) 
				conc.units <- ""
			else if (length(conc.units) > 1L) {
				lens <- nchar(conc.units)
				conc.units <- conc.units[which(lens > 1L)[1L]]
			}
			conc.units <- strsplit(conc.units, " ")[[1L]][1L]
		}
	}
	if (conc.units == "") {
		warning("Concentration units assumed to be mg/L")
		conc.units <- "mg/L"
	}
	CF <- loadConvFactor(flow.units, conc.units, load.units)
	if(censoring(df[[1L]]) == "multiple") {
		df[[1L]] <- as.mcens(df[[1L]]) * CF * Q
	} else
		df[[1L]] <- as.lcens(df[[1L]]) * CF * Q
	# Build the model and run stepwise
	m <- call
	m[[1L]] <- as.name("censReg")
	m$min.formula <- m$flow <- m$dates <- m$flow.units <- m$conc.units <- NULL
	m$load.units <- m$time.step <- m$station <- m$criterion <- NULL
	m$data <- df
	m$dist <- "lognormal"
	k <- if(criterion == "AIC") 2 else log(nrow(df))
	m <- eval(m)
	best <- step(m, scope=min.formula, direction="both", trace=0, k=k)
	# retain the step wise model selections
	model.eval <- best$anova
	names(model.eval)[[6L]] <- criterion # Fix this to represent what was really done
	# No build the load model
	m <- call
	m[[1L]] <- as.name("loadReg")
	m$min.formula <- m$criterion <- NULL
	m$formula <- best$call$formula
	retval <- eval(m)
	retval$model.eval <- model.eval
	return(retval)
}
 
