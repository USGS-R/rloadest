#' Predict Concentrations
#'
#' Estimate concentrations from a rating-curve model from \code{loadReg}
#'for a new data frame.
#'
#' The time frame specified by \code{by} must be either "unit" or
#'"day."
#'
#' If \code{allow.incomplete} is \code{TRUE}, then concentrations will be 
#'computed based on all nonmissing values, otherwise missing values
#'\code{NAs} will be returned. For this application, missing values 
#'includes \code{NAs} and incomplete days. For prediction by "day" when 
#'there are variable number of unit values per day, \code{allow.incomplete}
#'must be set to \code{TRUE}.
#'
#' The term confidence interval is used here as in the original 
#'documentation for LOADEST, but the values that are reported are 
#'the prediction intervals, computed from the SEP.
#'
#' @param fit the output from \code{loadReg}.
#' @param newdata a data frame of the prediction variables. MIssing values
#'are not permitted in any column in \code{newdata}. Observations with
#'missing values \code{NAs} must be removed before prediction. Columns that
#'are not needed for prediction that contain missing values can be removed
#'before removing all rows with missing values. The maximum number of rows
#'permitted in \code{newdata} is 176000.
#' @param by the time frame for estimates. See \code{Details}.
#' @param allow.incomplete compute loads for periods withing
#'missing values or incomplete record? See \code{Details}.
#' @param conf.int the confidence interval to compute for concentrations.
#'See \code{Details}.
#' @return A data frame containing the concnetration estimates.
#' @seealso \code{\link{loadReg}},
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'predConc(app1.lr, app1.calib)
#' @useDynLib rloadest estlday
#' @import lubridate
#' @export
predConc <- function(fit, newdata, by="day", 
                     allow.incomplete=FALSE, conf.int=0.95) {
  
  ## By options and other preliminary code
  if(is.null(fit$cfit)) {
    stop("model cannot predict concentration values")
  }
    
  if(nrow(newdata) > 176000L) {
    stop("newdata has too many rows, the size limit is 176000")
  }
  ByOpt <- c("unit", "day")
  match.arg(by, ByOpt)
  Qadj <- fit$Qadj
  Tadj <- fit$Tadj
  flow <- fit$flow
  dates <- fit$dates
  model.no <- fit$model.no
  time.step <- fit$time.step
  gps.nday <- 1L
  ## Aggregate newdata by the time.step
  if(class(newdata[[dates]])[1L] == "Date") {
    if(time.step != "day")
      stop("Time steps of ", time.step, "require POSIXt dates for the estimation data")
    gps <- format(newdata[[dates]])
  } else if(time.step != "instantaneous") {
    ## Construct grouping time steps of the unit value data
    if(time.step == "day") {
      ## Extract the day part
      gps <- format(newdata[[dates]], "%Y-%m-%d")
    } else {
      ## Required to force hours to match local time rather than
      ## conversion to GMT, which offsets the local time
      gps <- as.POSIXlt(as.character(newdata[[dates]]), tz="GMT")
      N.gps <- length(gps)
      gps$sec <- gps$min <- rep(0, N.gps)
      ## OK, now aggregate the hours
      hts <- as.numeric(strsplit(time.step, split=" ")[[1L]][1L])
      gps$hour <- (gps$hour %/% hts) * hts
      gps <- format(gps, "%Y-%m-%d %H")
      ## Set the number of obs per day
      gps.nday <- as.integer(24.0001/hts) # Just in case
    }
    ## Aggregate newdata and reset timezone info
    ntz <- attr(newdata[[dates]], "tzone")
    newdata <- aggregate(newdata, list(time.step=gps), mean)
    attr(newdata[[dates]], "tzone") <- ntz
  } else { # Must be instantaneous
    gps <- format(newdata[[dates]])
  }
  if(model.no == 99L) {
    Terms <- delete.response(fit$cfit$terms)
    m <- model.frame(Terms, newdata, na.action = na.pass)
    model.inp <- model.matrix(Terms, m)
  } else {
    model.inp <- setXLDat(newdata, flow, dates, Qadj, Tadj, model.no)
  }
  if(any(is.na(model.inp)))
    stop("Prediction data contains missing values, remove before prediction")
  ## Construct the structure for aggregating the data
  ## Deal with byn == 0 directly in the last estimation section
  checkDays <- list() # Create empty list for day checking
  byn <- pmatch(by, ByOpt, nomatch=0L)
  if(byn == 3L) { # by month
    ## Get the expected number of days for each month
    mn <- month(newdata[[dates]])
    yr <- year(newdata[[dates]])
    dyinper <- daysInMonth(mn, yr)
    ## Create the categories
    by <- "_by_"
    Period <- paste(month.name[mn], yr, sep=" ")
    newdata[[by]] <- factor(Period, levels=unique(Period)) # Force date order
    checkDays <- tapply(dyinper, Period, function(x) x[1L])
  } else if(byn == 4L) { # by water year
    yr <- waterYear(newdata[[dates]], numeric=TRUE)
    dyinper <- 365L + leap_year(yr)
    by <- "_by_"
    Period <- paste("WY", yr, sep=" ")
    newdata[[by]] <- Period
    checkDays <- tapply(dyinper, Period, function(x) x[1L])
  } else if(byn == 5L) { # by calendar year
    yr <- year(newdata[[dates]])
    dyinper <- 365L + leap_year(yr)
    by <- "_by_"
    Period <- paste("CY", yr, sep=" ")
    newdata[[by]] <- Period
    checkDays <- tapply(dyinper, Period, function(x) x[1L])
  } else if(byn == 0L) {
    ## No way to check the correct number of days a priori
    ## Check to see that it is in newdata
    if(!(by %in% names(newdata)))
      stop(by, " not found in the estimation dataset")
  } else
    by <- ByOpt[byn]
  if(by == "unit") {
    ## Only 1 obs per unit
    KDays <- seq(nrow(model.inp))
    ## Exclude NAs and presumably 0 flows
    KDays <- KDays[is.finite(rowSums(model.inp))]
    NDIM <- as.integer(length(KDays))
    out.data <- .Fortran("estlday",
                         NPAR=fit$cfit$NPAR,
                         PARMLE=fit$cfit$PARMLE,
                         BIAS=fit$cfit$BIAS,
                         CV_IN=fit$cfit$CV,
                         SBIAS=fit$cfit$SBIAS,
                         SCV_IN=fit$cfit$SCV,
                         npred=as.integer(length(KDays)),
                         xlpred=model.inp[KDays,],
                         NDAYS=NDIM,
                         KDAY2=as.integer(KDays),
                         load=double(NDIM),
                         loadvar=double(NDIM),
                         loadlow=double(NDIM),
                         loadup=double(NDIM),
                         loadsep=double(NDIM),
                         IERR=integer(1L))
    Conc <- Std.Err <- SEP <- L95 <- U95 <- rep(NA_real_, nrow(model.inp))
    if(out.data$IERR > 0) {
      warning(" *** Error (",out.data$IERR,") occurred in processing daily data. ***\n",sep="")
      retval <- data.frame(Date=newdata[[dates]], Flow=Flow,
                           Conc=Conc, Std.Err=Std.Err, SEP=SEP,
                           L95=L95, U95=U95)
    } else {
      ## OK, we've had a successful run, pack the data into a data frame
      Conc[KDays] <- out.data$load
      Std.Err[KDays] <- sqrt(out.data$loadvar)
      SEP[KDays] <- out.data$loadsep
      ## Convert to correct conf.int
      Names <- paste(c("L", "U"), round(conf.int*100, 0), sep="")
      DF <- fit$cfit$NOBSC - fit$cfit$NPAR
      B <- sqrt(log(1 + (SEP[KDays]/Conc[KDays])^2))
      A <- log(Conc[KDays]) - 0.5*B^2
      qci <- qt(1 - (1 - conf.int)/2, DF)
      L95[KDays] <- exp(A - qci*B)
      U95[KDays] <- exp(A + qci*B)
      Flow <- newdata[[flow]]
      ## convert NAs to 0 for 0 flow
      ZDays <- which(Flow == 0)
      if(length(ZDays)) {
        Conc[ZDays] <- 0
        Std.Err[ZDays] <- 0
        SEP[ZDays] <- 0
        L95[ZDays] <- 0
        U95[ZDays] <- 0
      }
      retval <- data.frame(Date=newdata[[dates]], Flow=Flow,
                           Conc=Conc, Std.Err=Std.Err, SEP=SEP,
                           L95=L95, U95=U95)
    }
    names(retval)[6L:7L] <- Names
  } else if(by == "day") {
    ## One or more obs per day
    ## KDate are the dates of the days
    ## KDays are the indexes to days
    ## Kin are the indexes to the good model inputs
    ## Kdy are the indexes to the days of model inputs
    ## KinAll are the indexes to all days
    ##
    ## Preserve flow for later
    Flow <- newdata[[flow]]
    if(time.step == "day") {
      KDate <- as.Date(as.POSIXlt(newdata[[dates]]))
      KDays <- seq(nrow(model.inp))
      KinAll <- KDays
      ## Exclude NAs and presumably 0 flows
      KDays <- KDays[is.finite(rowSums(model.inp))]
      Kin <- Kdy <- KDays
      NDIM <- as.integer(length(KDays))
    } else {
      Kin <- seq(nrow(model.inp))
      Kin <- Kin[is.finite(rowSums(model.inp))]
      KDate <- as.Date(as.POSIXlt(newdata[[dates]]))
      Kdy <- as.integer(KDate)
      Ktbl <- table(Kdy)
      if(length(unique(Ktbl)) > 1L && !allow.incomplete) {
        warning("Variable observations per day, either set the allow.incomplete argument to TRUE or use the resampleUVdata function to construct a uniform series")
      }
      Kdy <- rep(seq(1, length(Ktbl)), Ktbl) # Create the index to days in input
      KDate <- unique(KDate)
      KinAll <- unique(Kdy)
      ## Make it daily flow, Flow0 indicates a partial 0 flow
      Flow0 <- tapply(Flow, Kdy, min) 
      Flow <- tapply(Flow, Kdy, mean)
      Flow0 <- ifelse(Flow == 0, 1, Flow0)
      Kdy <- Kdy[Kin]
      ## Make a count of Kins for each day
      Kintmp <- table(Kdy)
      KinK <- rep(0L, along=KinAll)
      KinK[as.integer(names(Kintmp))] <- Kintmp
      ## OK, process rest of indexes
      KDays <- unique(Kdy)
      NDIM <- as.integer(length(KDays))
    }
    out.data <- .Fortran("estlday",
                         NPAR=fit$cfit$NPAR,
                         PARMLE=fit$cfit$PARMLE,
                         BIAS=fit$cfit$BIAS,
                         CV_IN=fit$cfit$CV,
                         SBIAS=fit$cfit$SBIAS,
                         SCV_IN=fit$cfit$SCV,
                         npred=as.integer(length(Kdy)),
                         xlpred=model.inp[Kin,],
                         NDAYS=NDIM,
                         KDAY2=as.integer(Kdy),
                         load=double(NDIM),
                         loadvar=double(NDIM),
                         loadlow=double(NDIM),
                         loadup=double(NDIM),
                         loadsep=double(NDIM),
                         IERR=integer(1L))
    Conc <- Std.Err <- SEP <- L95 <- U95 <- rep(NA_real_, length(KinAll))
    if(out.data$IERR > 0) {
      warning(" *** Error (",out.data$IERR,") occurred in processing daily data. ***\n",sep="")
      retval <- data.frame(Date=KDate, Flow=Flow,
                           Conc=Conc, Std.Err=Std.Err, SEP=SEP,
                           L95=L95, U95=U95)
    } else {
      ## OK, we've had a successful run, pack the data into a data frame
      Conc[KDays] <- out.data$load
      Std.Err[KDays] <- sqrt(out.data$loadvar)
      SEP[KDays] <- out.data$loadsep
      ## Convert to correct conf.int
      Names <- paste(c("L", "U"), round(conf.int*100, 0), sep="")
      DF <- fit$cfit$NOBSC - fit$cfit$NPAR
      B <- sqrt(log(1 + (SEP[KDays]/Conc[KDays])^2))
      A <- log(Conc[KDays]) - 0.5*B^2
      qci <- qt(1 - (1 - conf.int)/2, DF)
      L95[KDays] <- exp(A - qci*B)
      U95[KDays] <- exp(A + qci*B)
      ## Convert NAs to 0 for 0 flow
      ZDays <- which(Flow == 0)
      if(length(ZDays)) {
        Conc[ZDays] <- 0
        Std.Err[ZDays] <- 0
        SEP[ZDays] <- 0
        L95[ZDays] <- 0
        U95[ZDays] <- 0
      }
      ## Repair partial 0 flow days and process incomplete days
      if(gps.nday > 1L)
        for(i in KinAll) {
          if(Flow0[i] == 0) {
            ## The correction factor to make it a true daily mean
            CorrFact <- KinK[i]/gps.nday
            Conc[i] <- Conc[i] * CorrFact
            Std.Err[i] <- Std.Err[i] * CorrFact
            SEP[i] <- SEP[i] * CorrFact
            L95[i] <- L95[i] * CorrFact
            U95[i] <- U95[i] * CorrFact
          } else if(KinK[i] != gps.nday && !allow.incomplete) {
            Conc[i] <- NA_real_
            Std.Err[i] <- NA_real_
            SEP[i] <- NA_real_
            L95[i] <- NA_real_
            U95[i] <- NA_real_
          }
        } # End of if too
      retval <- data.frame(Date=KDate, Flow=Flow,
                           Conc=Conc, Std.Err=Std.Err, SEP=SEP,
                           L95=L95, U95=U95)
    }
    names(retval)[6L:7L] <- Names
  } 
  rownames(retval) <- NULL
  return(retval)
}
