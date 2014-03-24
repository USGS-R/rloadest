#' Load Estimation
#'
#' Build a rating-curve (regression) model for river load estimation.
#'
#' The left-hand side of the formula may be any numeric variable, just as with
#'\code{lm} or a variable of class "lcens," "mcens," or "qw."\cr
#'For un- or left-censored data, AMLE is used unless weights are specified in
#'the model, then MLE is used, through a call to survreg. For any other 
#'censored data, MLE is used.
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
#'
#' @return An object of class "loadReg."
#' @seealso \code{\link{censReg}}
#' @references will need some.
#' @keywords regression censored loads
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'print(app1.lr)
#'
#'@export
loadReg <- function(formula, data, subset, na.action, flow, dates,
                    flow.units="cfs", conc.units="", load.units="kg", 
                    time.step="day", station="") {
  ## Coding history:
  ##    2013May31 DLLorenz Original Coding
  ##
  ## Trap model number specification
  PredMod <- terms(formula, "model", data = data)
  indMod <- attr(PredMod, "specials")$model
  time.step <- match.arg(time.step, 
                         c("instantaneous", "1 hour", "2 hours",
                         "3 hours", "4 hours", "6 hours",
                         "8 hours", "12 hours", "day"))
  call <- match.call()
  m <- match.call(expand.dots = FALSE)
  ## remove components not needed for model.frame
  m$flow <- m$dates  <- m$flow.units <- m$conc.units <- NULL
  m$load.units <- m$time.step <- m$station <- NULL
  m[[1L]] <- as.name("model.frame")
  if(is.null(indMod)) { # User-defined model
    model.no <- 99L
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    xvars <- as.character(attr(Terms, "variables"))[-1L] # drop list
    Y <- model.extract(m, "response")
    X <- model.matrix(Terms, m)
    if((yvar <- attr(Terms, "response")) > 0L) {
      ynam <- xvars[yvar]
      xvars <- xvars[ - yvar]
    }
    if(length(xvars) > 0L) {
      xlevels <- lapply(m[xvars], levels)
      xlevels <- xlevels[!unlist(lapply(xlevels, is.null))]
      if(length(xlevels) == 0L)
        xlevels <- NULL
    }
    else xlevels <- NULL
    na.message <- attr(m, "na.message")
    saved.na.action <- attr(m, "na.action")
    if(missing(subset)) 
      subset <- seq(nrow(data))
    else
      subset <- eval(substitute(subset), data)
    Flow <- data[subset, flow]
    Time <- data[subset, dates]
    if(!is.null(saved.na.action)) {
      Flow <- Flow[-saved.na.action]
      Time <- Time[-saved.na.action]
    }
    PoR <- range(Time)
    Qadj <- loadestQadj(Flow)
    Tadj <- loadestTadj(Time)
  } else { # predefined model
    ## In fill the needed parameters from the call
    model.no <- m[["formula"]][[3L]][[2L]]
    m[["formula"]][[3L]][[3L]] <- data
    m[["formula"]][[3L]][[4L]] <- flow
    m[["formula"]][[3L]][[5L]] <- dates
    m <- eval(m, parent.frame())
    Terms <- attr(m, "terms")
    xvars <- as.character(attr(Terms, "variables"))[-1L] # drop list
    if(length(xvars) > 2L) # Should actually generate an error before here
      stop("If model is used, then no other explanatory variables can be specified")
    Y <- model.extract(m, "response")
    subset <- m[[2L]]
    if((yvar <- attr(Terms, "response")) > 0L) {
      ynam <- xvars[yvar]
    }
    na.message <- attr(m, "na.message")
    saved.na.action <- attr(m, "na.action")
    xlevels <- NULL
    ## Construct the model matrix
    Flow <- data[subset, flow]
    Time <- data[subset, dates]
    PoR <- range(Time)
    if(class(time)[1] == "Date")
      Time <- Time + 0.5
    Qadj <- loadestQadj(Flow)
    Tadj <- loadestTadj(Time)
    X <- setXLDat(data[subset,], flow, dates, Qadj, Tadj, model.no)
    xvars <- colnames(X)[-1L]
  }
  if(class(Y) == "numeric")
    Y <- as.lcens(Y)
  else if(class(Y) == "qw") {
    ## If conc.units are not specified, then try to get from object
    if(conc.units == "") {
      conc.units <- unique(Y@reporting.units)
      conc.units <- conc.units[!is.na(conc.units)]
      if(length(conc.units) == 0L)
        conc.units <- ""
      else if(length(conc.units) > 1L) {
        ## Select the first one with a length greater than 1
        lens <- nchar(conc.units)
        conc.units <- conc.unis[which(lens > 1L)[1L]]
      }
      ## Now make sure that as ... gets dropped
      conc.units <- strsplit(conc.units, " ")[[1L]][1L]
    }
    ## Required to fix the inability of model extraction to select all required parts
    if(!is.null(saved.na.action)) {
      Y@reporting.level <- Y@reporting.level[-saved.na.action]
      Y@remark.codes <- Y@remark.codes[-saved.na.action]
    }
    if(censoring(Y) == "multiple")
      Y <- as.mcens(Y)
    else
      Y <- as.lcens(Y)
  }
  else if(class(Y) == "lcens" && !is.null(saved.na.action))
    Y@censor.codes <- Y@censor.codes[-saved.na.action]
  ## Logic checks for time.step and class of Time
  if(class(Time)[1L] != "Date" && time.step == "day")
    warning("For time.step = \"day,\" the class of dates should be \"Date\"")
  else if(class(Time)[1L] == "Date" && time.step != "day")
    warning("For time.step less than \"day,\" the class of dates should be \"POXIXt\"")
  if(time.step == "day") { # Check that no duplicate days and appropriate gap
    Time <- as.Date(Time)
    Tdifs <- c(999L, diff(as.integer(Time)))
    if(any(Tdifs == 0L)) {
      Dys <- as.character(unique(Time[Tdifs == 0L]))
      Dys <- paste(Dys, collapse="\n")
      stop("Duplicated days not permitted for time.step = \"day\"\n", Dys)
    }
    Tdys <- min(Tdifs)
    if(Tdys < 7)
      warning("The minimum spacing between daily loads is ", Tdys,
              " days. The time between observations should be at least ",
              " 7 days to avoid autocorrelation issues.")
  } else { # Need unit checks too
    Tdys <- difftime(Time, shiftData(Time), units="days")
    Tdys <- min(Tdys, na.rm=TRUE)
    if(Tdys < 7)
      warning("The minimum spacing between daily loads is ", Tdys,
              " days. The time between observations should be at least ",
              " 7 days to avoid autocorrelation issues.")
  }
  ## OK, construct the concentration fit
  if(class(Y) == "lcens") {
    cfit <- censReg_AMLE.fit(Y, X, "lognormal")
    fit1 <- censReg_AMLE.fit(Y, X[,1L, drop=FALSE], "lognormal")
  } else {
    cfit <- censReg_MLE.fit(Y, X, rep(1, length(Y)), "lognormal")
    fit1 <- censReg_AMLE.fit(Y, X[,1L, drop=FALSE], 
                             rep(1, length(Y)), "lognormal")
  }
  cfit$LLR1 <- fit1$LLR
  cfit$call <- call
  cfit$terms <- Terms
  cfit$na.action <- saved.na.action
  cfit$na.message <- na.message
  cfit$xlevels <- xlevels
  class(cfit) <- "censReg"
  ## Not the load model fit
  ## Check on concentration units
  if(conc.units == "") {
    warning("Concentration units assumed to be mg/L")
    conc.units <- "mg/L"
  }
  CF <- loadConvFactor(flow.units, conc.units, load.units)
  Y <- Y * CF * Flow
  if(class(Y) == "lcens") {
    lfit <- censReg_AMLE.fit(Y, X, "lognormal")
    fit1 <- censReg_AMLE.fit(Y, X[,1L, drop=FALSE], "lognormal")
    method <- "AMLE"
  } else {
    lfit <- censReg_MLE.fit(Y, X, rep(1, length(Y)), "lognormal")
    fit1 <- censReg_MLE.fit(Y, X[,1L, drop=FALSE], 
                             rep(1, length(Y)), "lognormal")
    method <- "MLE"
  }
  lfit$LLR1 <- fit1$LLR
  lfit$call <- call
  lfit$terms <- Terms
  lfit$na.action <- saved.na.action
  lfit$na.message <- na.message
  lfit$xlevels <- xlevels
  class(lfit) <- "censReg"
  ## Construct the evaluation data frame
  MEC <- data.frame(model=model.no, AIC=lfit$AIC, SPCC=lfit$SPPC)
  retval <- list(station=station, constituent=ynam, 
                 flow=flow, dates=dates, Qadj=Qadj,
                 Tadj=Tadj, model.no=model.no, model.eval=MEC,
                 method=method, conc.units=conc.units, Time=Time,
                 Sum.flow=summary(Flow), flow.units=flow.units,
                 load.units=load.units, PoR=PoR, xvars=xvars,
                 lfit=lfit, cfit=cfit, time.step=time.step)
  class(retval) <- "loadReg"
  return(retval)
}
