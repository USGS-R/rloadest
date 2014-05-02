#' Predict Loads
#'
#' Estimate loads from a rating-curve model from \code{loadReg}
#'for a new data frame, aggregating the loads by specified time 
#'periods.
#'
#' The time frame specified by \code{by} can be "unit,"
#'"day," "month," "water year," "calendar year," "total," or 
#'the name of a column in \code{newdata} that can be used to 
#'group the data.
#'
#' If \code{allow.incomplete} is \code{TRUE}, then loads will be 
#'computed based on all nonmissing values, otherwise missing values
#'\code{NAs} will be returned. For this application, missing values 
#'includes \code{NAs} and gaps in the record, except for \code{by} 
#'set to "total" or user defined groups where missing values only
#'includes \code{NAs}.
#'
#' The term confidence interval is used here as in the original 
#'documentation for LOADEST, but the values that are reported are 
#'the prediction intervals, computed from the SEP.
#'
#' @param fit the output from \code{loadReg}.
#' @param newdata a data frame of the prediction variables.
#' @param by the time frame for estimates. See \code{Details}.
#' @param seopt a character string indicating how to comute the 
#'standard error of the aggregated load estimates, must be either
#'"exact" or "approximate." Only the first letter is necessary.
#' @param allow.incomplete compute loads for periods withing
#'missing values or incomplete record? See \code{Details}.
#' @param conf.int the confidence interval to compute for loads
#'computed by "day" or "unit." The confidence interval is fixed at 
#'0.95 for any other value for \code{by}. See \code{Details}.
#' @param print print a report summary of the load estimate?
#' @return A data frame containing the load estimates.
#' @seealso \code{\link{loadReg}},
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'predLoad(app1.lr, app1.calib)
#' @useDynLib rloadest estlday
#' @useDynLib rloadest estltot
#' @export
predLoad <- function(fit, newdata, load.units=fit$load.units, by="total", 
                     seopt="exact", allow.incomplete=FALSE, 
                     conf.int=0.95, print=FALSE) {
  ## Coding history:
  ##    2013Jun06 DLLorenz Original Coding
  ##    2013Jul03 DLLorenz Added code to deal with unit values
  ##    2013Dec05 DLLorenz Bug fix
  ##
  ## By options and other preliminary code
  ByOpt <- c("unit", "day", "month", "water year", "calendar year", "total")
  load.units
  seopt <- match.arg(seopt, c("exact", "approximate"))
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
  lcf <- loadUnitConv(fit$load.units, load.units)
  if(model.no == 99L) {
    Terms <- delete.response(fit$lfit$terms)
    m <- model.frame(Terms, newdata, na.action = na.pass)
    model.inp <- model.matrix(Terms, m)
  } else {
    model.inp <- setXLDat(newdata, flow, dates, Qadj, Tadj, model.no)
  }
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
                         NPAR=fit$lfit$NPAR,
                         PARMLE=fit$lfit$PARMLE,
                         BIAS=fit$lfit$BIAS,
                         CV_IN=fit$lfit$CV,
                         SBIAS=fit$lfit$SBIAS,
                         SCV_IN=fit$lfit$SCV,
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
    Flux <- Std.Err <- SEP <- L95 <- U95 <- rep(NA_real_, nrow(model.inp))
    if(out.data$IERR > 0) {
      warning(" *** Error (",out.data$IERR,") occurred in processing daily data. ***\n",sep="")
      retval <- data.frame(Date=newdata[[dates]], Flow=Flow,
                           Flux=Flux, Std.Err=Std.Err, SEP=SEP,
                           L95=L95, U95=U95)
    } else {
      ## OK, we've had a successful run, pack the data into a data frame
      Flux[KDays] <- out.data$load * lcf
      Std.Err[KDays] <- sqrt(out.data$loadvar) * lcf
      SEP[KDays] <- out.data$loadsep * lcf
      ## Convert to correct conf.int
      Names <- paste(c("L", "U"), round(conf.int*100, 0), sep="")
      DF <- fit$lfit$NOBSC - fit$lfit$NPAR
      B <- sqrt(log(1 + (SEP[KDays]/Flux[KDays])^2))
      A <- log(Flux[KDays]) - 0.5*B^2
      qci <- qt(1 - (1 - conf.int)/2, DF)
      L95[KDays] <- exp(A - qci*B)
      U95[KDays] <- exp(A + qci*B)
      Flow <- newdata[[flow]]
      ## convert NAs to 0 for 0 flow
      ZDays <- which(Flow == 0)
      if(length(ZDays)) {
        Flux[ZDays] <- 0
        Std.Err[ZDays] <- 0
        SEP[ZDays] <- 0
        L95[ZDays] <- 0
        U95[ZDays] <- 0
      }
      retval <- data.frame(Date=newdata[[dates]], Flow=Flow,
                           Flux=Flux, Std.Err=Std.Err, SEP=SEP,
                           L95=L95, U95=U95)
    }
    names(retval)[6L:7L] <- Names
  } else if(by == "day") {
    ## One or more obs per day
    ## KDays are the indexes to days
    ## Kin are the indexes to the good model inputs
    ## Kdy are the indexes to the days
    ## KinAll are the indexes to all days
    ##
    ## Preserve flow for later
    Flow <- newdata[[flow]]
    if(time.step == "day") {
      KDate <- as.Date(newdata[[dates]])
      KDays <- seq(nrow(model.inp))
      KinAll <- KDays
      ## Exclude NAs and presumably 0 flows
      KDays <- KDays[is.finite(rowSums(model.inp))]
      Kin <- Kdy <- KDays
      NDIM <- as.integer(length(KDays))
    } else {
      Kin <- seq(nrow(model.inp))
      Kin <- Kin[is.finite(rowSums(model.inp))]
      KDate <- as.Date(newdata[[dates]])
      Kdy <- as.integer(KDate)
      KDate <- unique(KDate)
      Kdy <- Kdy - Kdy[1L] + 1L # make relative to first day (Index)
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
                         NPAR=fit$lfit$NPAR,
                         PARMLE=fit$lfit$PARMLE,
                         BIAS=fit$lfit$BIAS,
                         CV_IN=fit$lfit$CV,
                         SBIAS=fit$lfit$SBIAS,
                         SCV_IN=fit$lfit$SCV,
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
    Flux <- Std.Err <- SEP <- L95 <- U95 <- rep(NA_real_, length(KinAll))
    if(out.data$IERR > 0) {
      warning(" *** Error (",out.data$IERR,") occurred in processing daily data. ***\n",sep="")
      retval <- data.frame(Date=KDate, Flow=Flow,
                           Flux=Flux, Std.Err=Std.Err, SEP=SEP,
                           L95=L95, U95=U95)
    } else {
      ## OK, we've had a successful run, pack the data into a data frame
      Flux[KDays] <- out.data$load * lcf
      Std.Err[KDays] <- sqrt(out.data$loadvar) * lcf
      SEP[KDays] <- out.data$loadsep * lcf
      ## Convert to correct conf.int
      Names <- paste(c("L", "U"), round(conf.int*100, 0), sep="")
      DF <- fit$lfit$NOBSC - fit$lfit$NPAR
      B <- sqrt(log(1 + (SEP[KDays]/Flux[KDays])^2))
      A <- log(Flux[KDays]) - 0.5*B^2
      qci <- qt(1 - (1 - conf.int)/2, DF)
      L95[KDays] <- exp(A - qci*B)
      U95[KDays] <- exp(A + qci*B)
      ## Convert NAs to 0 for 0 flow
      ZDays <- which(Flow == 0)
      if(length(ZDays)) {
        Flux[ZDays] <- 0
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
            CorrFact <- KikK[i]/gps.nday
            Flux[i] <- Flux[i] * CorrFact
            Std.Err[i] <- Std.Err[i] * CorrFact
            SEP[i] <- SEP[i] * CorrFact
            L95[i] <- L95[i] * CorrFact
            U95[i] <- U95[i] * CorrFact
          } else if(KinK[i] != gps.nday && !allow.incomplete) {
            Flux[i] <- NA_real_
            Std.Err[i] <- NA_real_
            SEP[i] <- NA_real_
            L95[i] <- NA_real_
            U95[i] <- NA_real_
          }
        } # End of if too
      retval <- data.frame(Date=KDate, Flow=Flow,
                           Flux=Flux, Std.Err=Std.Err, SEP=SEP,
                           L95=L95, U95=U95)
    }
    names(retval)[6L:7L] <- Names
  } else if(by == "total") {
    ## For now, only 1 obs per day (assumed and hard coded)
    KDays <- seq(nrow(model.inp))
    Flow <- newdata[[flow]]
    Sum <- rowSums(model.inp) # Use to detect any missing values
    ## No reason to checkDays because total is total
    if(any(drop <- is.na(Sum) & Flow > 0) && !allow.incomplete) {
      warning("Missing values in newdata")
      ## Return all NAs
      retval <- data.frame(Period="total", Ndays=NA_integer_,
                           Flux=NA_real_, Std.Err=NA_real_, 
                           SEP=NA_real_, L95=NA_real_, U95=NA_real_)
      return(retval) # Skip print
    }
    if(any(drop)) {
      model.inp <- model.inp[!drop,]
      Flow <- Flow[!drop]
      KDays <- KDays[!drop]
    }
    Zdays <- which(Flow == 0)
    NDIM <- as.integer(length(KDays))
    if(length(Zdays))
      KDays <- KDays[-Zdays]
    out.data <- .Fortran("estltot",
                         NPAR=fit$lfit$NPAR,
                         PARMLE=fit$lfit$PARMLE,
                         BIAS=fit$lfit$BIAS,
                         CV_IN=fit$lfit$CV,
                         SBIAS=fit$lfit$SBIAS,
                         SCV_IN=fit$lfit$SCV,
                         npred=as.integer(length(KDays)),
                         xlpred=model.inp[KDays,],
                         NDAYS=NDIM,
                         N_DAY=gps.nday,
                         SDEXACT=seopt=="exact",
                         load=double(1L),
                         loadvar=double(1L),
                         loadlow=double(1L),
                         loadup=double(1L),
                         loadsep=double(1L),
                         IERR=integer(1L))
    if(out.data$IERR > 0) {
      warning(" *** Error (",out.data$IERR,") occurred in processing total load. ***\n",sep="")
      retval <- data.frame(Period="total", Ndays=NA_integer_,
                           Flux=NA_real_, Std.Err=NA_real_, 
                           SEP=NA_real_, L95=NA_real_, U95=NA_real_)
    } else {
      ## OK, we've had a successful run, pack the data into a data frame
      retval <- data.frame(Period="total", Ndays=NDIM,
                           Flux=out.data$load * lcf, 
                           Std.Err=sqrt(out.data$loadvar) * lcf, 
                           SEP=out.data$loadsep * lcf, 
                           L95=out.data$loadlow * lcf, 
                           U95=out.data$loadup * lcf)
    }
  } else { # Any other kind of aggregation
    Flow <- newdata[[flow]] # Needed for summary
    retval <- tapply(seq(nrow(model.inp)), newdata[[by]], FUN=function(period) {
      ## For now, only 1 obs per day (assumed and hard coded)
      KDays <- period
      Flow <- newdata[period, flow]
      Sum <- rowSums(model.inp[period,]) # Use to detect any missing values
      drop <- is.na(Sum) & Flow > 0
      ## Check for incomplete data
      OK <- TRUE
      if(!allow.incomplete) {
        NDays <- length(KDays) / gps.nday
        if(any(drop)) { # Any missing values?
          OK <- FALSE
          retby <- data.frame(Period="period", Ndays=NDays,
                               Flux=NA_real_, Std.Err=NA_real_, 
                               SEP=NA_real_, L95=NA_real_, U95=NA_real_)
        } else {
          ## Check for complete periods
          Period <- as.character(newdata[[by]][period[1L]])
          if(!is.null(targN <- checkDays[[Period]])) {
            if(targN != NDays) {
              OK <- FALSE
              retby <- data.frame(Period="period", Ndays=NDays,
                                   Flux=NA_real_, Std.Err=NA_real_, 
                                   SEP=NA_real_, L95=NA_real_, U95=NA_real_)
              
            }
          }
        }
      }
      if(OK) {
        if(any(drop)) {
          KDays <- KDays[!drop]
          model.inp <- model.inp[KDays,]
          Flow <- Flow[!drop]
        }
        Zdays <- which(Flow == 0)
        NDIM <- as.integer(length(KDays))
        if(length(Zdays))
          KDays <- KDays[-Zdays]
        out.data <- .Fortran("estltot",
                             NPAR=fit$lfit$NPAR,
                             PARMLE=fit$lfit$PARMLE,
                             BIAS=fit$lfit$BIAS,
                             CV_IN=fit$lfit$CV,
                             SBIAS=fit$lfit$SBIAS,
                             SCV_IN=fit$lfit$SCV,
                             npred=as.integer(length(KDays)),
                             xlpred=model.inp[KDays,],
                             NDAYS=NDIM,
                             N_DAY=gps.nday,
                             SDEXACT=seopt=="exact",
                             load=double(1L),
                             loadvar=double(1L),
                             loadlow=double(1L),
                             loadup=double(1L),
                             loadsep=double(1L),
                             IERR=integer(1L))
        if(out.data$IERR > 0) {
          warning(" *** Error (",out.data$IERR,") occurred in processing data. ***\n",sep="")
          retby <- data.frame(Period="period", Ndays=NA_integer_,
                               Flux=NA_real_, Std.Err=NA_real_, 
                               SEP=NA_real_, L95=NA_real_, U95=NA_real_)
        } else {
          ## OK, we've had a successful run, pack the data into a data frame
          retby <- data.frame(Period="period", Ndays=NDIM,
                               Flux=out.data$load * lcf, 
                               Std.Err=sqrt(out.data$loadvar) * lcf, 
                               SEP=out.data$loadsep * lcf, 
                               L95=out.data$loadlow * lcf, 
                               U95=out.data$loadup * lcf)
        }
      }
      return(retby)
    }
    ) # End of tapply
    Period <- names(retval)
    retval <- do.call(rbind, retval)
    retval$Period <- Period
  } # End of else any other aggregation
  rownames(retval) <- NULL
  if(print) {
    Date <- newdata[[dates]]
    cat("\n---------------------------------------------------------------------\n",
        "Constituent Output File Part IIa: Estimation (test for extrapolation)\n",
        " Load Estimates for ", as.character(min(Date)),
        " to ", as.character(max(Date)), "\n", 
        "---------------------------------------------------------------------\n\n",
        sep="")
    cat("Streamflow Summary Statistics\n",
        "-------------------------------------------\n\n", sep="")
    Qsum <- rbind(Cal.=fit$Sum.flow, Est.=summary(Flow))
    if(diff(range(Qsum[, ncol(Qsum)])) > 0)
      cat("WARNING: The maximum estimation data set steamflow exceeds the maximum\n",
          "calibration data set streamflow. Load estimates require extrapolation.\n\n",
          sep="")
    else
      cat("The maximum estimation data set streamflow does not exceed the maximum\n",
          "calibration data set streamflow. No extrapolation is required.\n\n",
          sep="")
    if(!(by %in% c("day", "unit"))) {
      cat("\n-------------------------------------------------------------\n",
          "Constituent Output File Part IIb: Estimation (Load Estimates)\n",
          " Load Estimates for ", as.character(min(Date)),
          " to ", as.character(max(Date)), "\n", 
          "---------------------------------------------------------------\n\n",
          sep="")
      cat("Flux Estimates, in ", load.units, "/d, using ",
          fit$lfit$method,
          "\n----------------------------------------\n\n", sep="")
      print(retval)
    } else
      cat("Estimates for daily or instantaneous values not printed\n\n")
    return(invisible(retval))
  } else
    return(retval)
}
