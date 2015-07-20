#' Load Estimation
#'
#' Build a segmented rating-curve (regression) model for river load estimation.
#'
#' The left-hand side of the formula can be any numeric variable, just 
#'as with \code{lm} or a variable of class "lcens," "mcens," "qw," or "Surv."
#'The response variable must be a column in \code{data}; it cannot be constructed using
#'\code{as.lcens}, \code{as.mcens}, or \code{Surv}. The initial segmented model is
#'based on uncensored data---simple substitution of 1/2 the reporting limit is used
#'for left-censored values and multiply censored values cause the analysis to fail.
#'
#'The first term of right-hand side must be defined by the \code{seg} function
#'with the number of segments. The first term may be followed by any number of 
#'additional terms. The final model will place the segmeted term in the last postition
#'and \code{seg} will be replaced by the proper call to \code{segment}.
#'
#' @param formula a formula describing the regression model. See \bold{Details}.
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
#' @param trace if logical, then if \code{TRUE} print a short summary of the
#'segmented fit. Otherwise a character string and the segmented model is saved
#'as that object name.
#'
#' @return An object of class "loadReg."
#' @seealso \code{\link{censReg}}, \code{link{seg}}, \code{\link{segment}}
#' @references will need some.
#' @keywords regression censored loads
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'print(app1.lr)
#' @import segmented
#' @export
segLoadReg <- function(formula, data, subset, na.action, flow, dates,
                    flow.units="cfs", conc.units="", load.units="kg", 
                    time.step="day", station="",trace=TRUE) {
	## Coding history:
	##    2014Jun02 DLLorenz Original Coding
	##
	## Trap model number specification
	PredMod <- terms(formula, "seg", data = data)
	indSeg <- attr(PredMod, "specials")$seg
	if(is.null(indSeg)) {
		stop("You must include the seg function to indicate which variable is segmented")
	} else if(indSeg != 2)
		stop("The segmented term must be the first one on the right-hand side of the formula")
	time.step <- match.arg(time.step, 
												 c("instantaneous", "1 hour", "2 hours",
												 	"3 hours", "4 hours", "6 hours",
												 	"8 hours", "12 hours", "day"))
	call <- match.call()
	m <- match.call(expand.dots = FALSE)
	## remove components not needed for model.frame
	m$flow <- m$dates  <- m$flow.units <- m$conc.units <- NULL
	m$load.units <- m$time.step <- m$station <- m$trace <- NULL
	mkeep <- m
	m[[1L]] <- as.name("model.frame")
	# Extract the column to segment
	m <- eval(m, parent.frame())
  cm1 <- censoring(m[[1L]])
	if(inherits(m[[1L]], "qw")) {
	  m[[1L]] <- as.numeric(m[[1L]]) # Will fail on multiple censoring
	} else if(inherits(m[[1L]], "mcens")) {
    if(cm1 == "none") {
      m[[1L]] <- m[[1L]]@.data[, 1L]
    } else if(cm1 == "left") {
      m[[1L]] <- ifelse(m[[1L]]@censor.codes == -1L, m[[1L]]@.data[, 1L]/2, m[[1L]]@.data[, 1L])
      warning("Simple substitution used for left-censored values")
    } else {
      stop("multiple censoring not supported")
    }
	} else if(inherits(m[[1L]], "lcens")) {
	  if(cm1 == "none") {
	    m[[1L]] <- m[[1L]]@.data[, 1L]
	  } else {
	    m[[1L]] <- ifelse(m[[1L]]@censor.codes, m[[1L]]@.data[, 1L]/2, m[[1L]]@.data[, 1L])
	    warning("Simple substitution used for left-censored values")
	  }
	} else if(inherits(m[[1L]], "Surv")) {
	  if(cm1 == "none") {
	    m[[1L]] <- m[[1L]]@.data[, 1L]
	  } else if(cm1 == "left") {
	    m[[1L]] <- ifelse(m[[1L]][, 3L] == 2, m[[1L]][, 1L]/2, m[[1L]][, 1L])
	    warning("Simple substitution used for left-censored values")
	  } else {
	    stop("multiple censoring not supported")
	  }
	} # Anything else must be purely numeric
	segVar <- m[[2L]]
	segTrm <- names(m)[2L]
	segNam <- make.names(names(m)[2L])
	segNam <- gsub(".", "", segNam, fixed=TRUE)
	segDat <- cbind(segVar, m)
	names(segDat)[1L] <- segNam
	# transform response
	respTrm <- names(m)[1L]
	segDat[[respTrm]] <- log(segDat[[respTrm]])
	# Construct lm model to find breaks
	m <- mkeep
	m$data <- segDat
	m[[1L]] <- as.name("lm")
	segN <- attr(segVar, "N")
	segBrk <- quantile(segVar, probs=(seq(segN)/(segN+1)))
  # Modify formula with single name rather than what was called
  form <- format(m$formula)
  form <- gsub(segTrm, segNam, form, fixed=TRUE)
	m[[2]] <- as.formula(form) # this should be the term in the formula
	m <- eval(m, parent.frame()) # The linear regression model
    lmAIC <- AIC(m)
	# OK, try segmented
	m <- segmented(m, seg.Z=as.formula(paste("~", segNam)),
								 psi=segBrk)
    # Deal with the segmented model
    if(is.logical(trace)) {
      if(trace) {
        cat("Segmented regression results:\n AIC:\n")
        print(c(lm=lmAIC, sm=AIC(m)), digits=6)
        cat("Breakpoints (psi):\n")
        psi <- m$psi
        row.names(psi) <- substring(row.names(psi), 1, 4)
        print(signif(psi, 4))
      }
    } else {
      assign(trace, m, pos=1L)
      cat("Segmented model saved as ", trace, "\n", sep="")
    }
	# Construct new call using segmented and psi replacing seg and N
    segBrk <- signif(m$psi[, 2L], 4)
    segBrk <- paste(segBrk, collapse=",")
    call[[1L]] <- as.name("loadReg")
    form <- as.formula(call$formula)
    segLstCom <- gregexpr(",", segTrm)[[1L]]
    segLstCom <- segLstCom[length(segLstCom)] - 1
	newTrm <- paste("segment", substring(segTrm, 4, segLstCom), 
           ", c(", segBrk, "))", sep="")
    form <- update(form, as.formula(paste("~. - ", segTrm, " + ", newTrm, sep="")))
    call$formula <- form
    call$trace <- NULL
    return(eval(call))
}
