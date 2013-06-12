#' Predict Loads
#'
#' Predict loads from a rating-curve model from \code{loadReg}
#'for a new data frame, aggregating the loads by specified time 
#'periods.
#'
#' The time frame specified by \code{by} can be "unit,"
#'"day," "month," "water year," "calendar year," "total," or 
#'the name of a column in \code{newdata} that can be used to 
#'group the data.
#'
#' @param fit the output from \code{loadReg}.
#' @param newdata a data frame of the prediction variables.
#' @param by the time frame for estimates.
#' @param sd a character string indicating how to comute the 
#'standard deviation of the total load estimates
#' @param allow.incomplete compute loads for periods withing
#'missing values or incomplete record?
#' @param print print a report summary of the load estimate?
#' @return A data frame containing the load estimates.
#' @seealso \code{\link{loadReg}},
#' @useDynLib rloadest estlday
#' @useDynLib rloadest estltot
#' @export
predLoad <- function(fit, newdata, load.units=fit$load.units, by="day", sd="exact",
                     allow.incomplete=FALSE, print=FALSE) {
  ## Coding history:
  ##    2013Jun06 DLLorenz Original Coding
  ##
  ## By options:
  ByOpt <- c("unit", "day", "month", "water year", "calendar year", "total")
  ## Need to possibly aggregate newdata by the time.step
  ## Not included in this version
  load.units
  lcf <- loadUnitConv(fit$load.units, load.units)
  Qadj <- fit$Qadj
  Tadj <- fit$Tadj
  flow <- fit$flow
  dates <- fit$dates
  model.no <- fit$model.no
  if(model.no == 99L) {
    # Do this later
    stop("Sorry, no quite implemented yet")
  } else {
    model.inp <- setXLDat(newdata, flow, dates, Qadj, Tadj, model.no)
  }
  byn <- pmatch(by, ByOpt, nomatch=0L)
  ## Deal with byn == 0 directly in the last estimation section
  if(byn == 3L) { # by month
    by <- "_by_"
    Period <- paste(as.character(month(newdata[[dates]], label=TRUE, abbr=FALSE)),
                    year(newdata[[dates]]), sep=" ")
    newdata[[by]] <- factor(Period, levels=unique(Period)) # Force date order
  } else if(byn == 4L) { # by water year
    by <- "_by_"
    Period <- waterYear(newdata[[dates]])
    newdata[[by]] <- Period
  } else if(byn == 5L) { # by calendar year
    by <- "_by_"
    Period <- year(newdata[[dates]])
    newdata[[by]] <- Period
  } else
    by <- ByOpt[byn]
  if(by == "day") { # should also be the logic for "unit"
    ## For now, only 1 obs per day (assumed and hard coded)
    KDays <- seq(nrow(model.inp))
    ## Note below must be changed for model == 99
    KDays <- KDays[is.finite(model.inp[, 2L])] # Always lnQ
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
    if(out.data$IERR > 0)
      stop(" *** Error (",out.data$IERR,") occurred in processing daily data. ***\n",sep="")
    ## OK, we've had a successful run, pack the data into a data frame
    Flux <- Std.Err <- SEP <- L95 <- U95 <- rep(NA_real_, nrow(model.inp))
    Flux[KDays] <- out.data$load * lcf
    Std.Err[KDays] <- sqrt(out.data$loadvar) * lcf
    SEP[KDays] <- out.data$loadsep * lcf
    L95[KDays] <- out.data$loadlow * lcf
    U95[KDays] <- out.data$loadup * lcf
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
  } else if(by == "total") {
    ## For now, only 1 obs per day (assumed and hard coded)
    KDays <- seq(nrow(model.inp))
    Flow <- newdata[[flow]]
    Sum <- rowSums(model.inp) # Use to detect any missing values
    if(any(drop <- is.na(Sum)) && !allow.incomplete)
      stop("Missing values in newdata")
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
                         SDEXACT=sd=="exact",
                         load=double(1L),
                         loadvar=double(1L),
                         loadlow=double(1L),
                         loadup=double(1L),
                         loadsep=double(1L),
                         IERR=integer(1L))
    if(out.data$IERR > 0)
      stop(" *** Error (",out.data$IERR,") occurred in processing total load. ***\n",sep="")
    ## OK, we've had a successful run, pack the data into a data frame
    retval <- data.frame(Period="total", Ndays=NDIM,
                         Flux=out.data$load * lcf, 
                         Std.Err=sqrt(out.data$loadvar) * lcf, 
                         SEP=out.data$loadsep * lcf, 
                         L95=out.data$loadlow * lcf, 
                         U95=out.data$loadup * lcf)
  } else { # Any other kind of aggregation
    retval <- tapply(seq(nrow(model.inp)), newdata[[by]], FUN=function(period) {
      ## For now, only 1 obs per day (assumed and hard coded)
      KDays <- period
      Flow <- newdata[period, flow]
      Sum <- rowSums(model.inp[period,]) # Use to detect any missing values
      if(any(drop <- is.na(Sum)) && !allow.incomplete)
        stop("Missing values in newdata")
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
                           SDEXACT=sd=="exact",
                           load=double(1L),
                           loadvar=double(1L),
                           loadlow=double(1L),
                           loadup=double(1L),
                           loadsep=double(1L),
                           IERR=integer(1L))
      if(out.data$IERR > 0)
        stop(" *** Error (",out.data$IERR,") occurred in processing data. ***\n",sep="")
      ## OK, we've had a successful run, pack the data into a data frame
      retval <- data.frame(Period="period", Ndays=NDIM,
                           Flux=out.data$load * lcf, 
                           Std.Err=sqrt(out.data$loadvar) * lcf, 
                           SEP=out.data$loadsep * lcf, 
                           L95=out.data$loadlow * lcf, 
                           U95=out.data$loadup * lcf)
      
    })
    Period <- names(revtal)
    retval <- do.call(rbind, retval)
    retval$Period <- Period
  }
  if(print) {
    Date <- newdata[[dates]]
    cat("\n---------------------------------------------------------------------------------------------------------\n",
        "Constituent Output File Part IIa: Estimation (test for extrapolation)\n",
        " Load Estimates for ", as.character(min(Date)),
        " to ", as.character(max(Date)), "\n", 
        "---------------------------------------------------------------------------------------------------------\n\n",
        sep="")
    cat("Streamflow Summary Statistics\n",
        "-------------------------------------------?\n\n", sep="")
    Qsum <- rbind(Cal.=fit$Sum.flow, Est.=summary(Flow))
    print(Qsum)
    if(diff(range(Qsum[, ncol(Qsum)])) > 0)
      cat("WARNING: The maximum estimation data set steamflow exceeds the maximum\n",
          "calibration data set streamflow. Load estimates require extrapolation.\n\n",
          sep="")
    else
      cat("The maximum estimation data set streamflow does not exceed the maximum\n",
          "calibration data set streamflow. No extrapolation is required.\n\n",
          sep="")
    if(!(by %in% c("day", "unit"))) {
      cat("\n---------------------------------------------------------------------------------------------------------\n",
          "Constituent Output File Part IIb: Estimation (Load Estimates)\n",
          " Load Estimates for ", as.character(min(Date)),
          " to ", as.character(max(Date)), "\n", 
          "---------------------------------------------------------------------------------------------------------\n\n",
          sep="")
      cat("Load Estimates, in ", load.units, ", using ",
          fit$lfit$method,
          "\n------------------------------------\n\n", sep="")
      print(retval)
    } else
      cat("Load Estimates for daily or instantaneous values not printed\n\n")
  }
  invisible(retval)
}
