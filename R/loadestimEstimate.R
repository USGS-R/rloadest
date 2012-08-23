# dialog support functions for S-LOADEST.
#    Compute load estimates
#    for a single site and constituent.
#
# Coding history:
#    2004Jan29 DLLorenz Original version for USGS library
#    2004May18 DLLorenz Removed LA method
#    2004Oct29 DLLorenz Revised instantaneous estimates
#    2004Nov10 DLLorenz Added quadratic time
#    2005Jul14 DLLorenz LAD no instantaneous
#    2005Oct03 DLLorenz Bug fix to allow short prediction periods
#    2006Apr10 DLLorenz Added code to handle warning and error
#    2006Nov09 DLLorenz Bug fix in monthly and seasonal sections
#    2008Sep26 DLLorenz Bug fix in extracting Days
#    2010Sep24 DLLorenz Minor Tweak and added test for sequential data
#    2010Sep24          This version.
#
loadestimEstimate <- function(sname, data, preddata, loadest.model, exact=TRUE,
                              total=FALSE, totalfile="",
                              annual="None", annualfile="",
                              season="", seasonfile="",
                              monthly=FALSE, monthfile="", daily=FALSE, dailyfile="",
                              inst=FALSE, instfile="", conv.factor) {
  if(is.numeric(loadest.model)) {
    if(loadest.model[1] == -1) {
      cat("\n *** Model not calibrated. ***\n")
      return()
    }
    if(loadest.model[1] == 0) {
      cat("\n *** Model not selected. ***\n")
      return()
    }
    method <- c("AMLE", "MLE", "LAD")[loadest.model[2]]
    model <- loadest.model[1]
    flowtrans <- ""
    floworder <- ""
    lineartime <- 0
    seasonal <- ""
    period <- ""
    additional.terms <- ""
    diurnal <- FALSE
  } else {
    method <- loadest.model[8]
    model <- -1
    flowtrans <- loadest.model[1]
    floworder <- loadest.model[2]
    lineartime <- as.integer(loadest.model[3])
    seasonal <- loadest.model[4]
    period <- loadest.model[5]
    additional.terms <- loadest.model[6]
    diurnal <- as.logical(loadest.model[7])
  }
  ##
  sdopt <- as.integer(exact) + 2
  Qadj <- loadestQadj(data[[sname[4]]])
  Tadj <- loadestTadj(data[[sname[5]]])
  model.inp <- loadestSetXLDat(data, sname, model, Qadj, Tadj, flowtrans,
                               floworder, lineartime, seasonal, period,
                               additional.terms, diurnal)
  goodrows <- !is.na(rowSums(model.inp$xlcal))
  xlcal <- as.matrix(model.inp$xlcal[goodrows,])
  ylcal <- model.inp$ylcal[goodrows]
  yd <- model.inp$yd[goodrows]
  censflag <- model.inp$censflag[goodrows]
  NOBSC <- as.integer(sum(goodrows))
  NPAR <- as.integer(model.inp$NPAR)
  ## OK that takes care of the calibration data, now for the prediction data
  ## The input data should have DATES (dates), TIMES (character), FLOW (double)
  ## and any other additional explantory variable.
  ## Process the prediction data
  snamep <- c("", sname[4:7])
  model.inp <- loadestSetXLDat(preddata, snamep, model, Qadj, Tadj, flowtrans,
                               floworder, lineartime, seasonal, period,
                               additional.terms, diurnal)
  model.mat <- as.matrix(model.inp$xlcal)
  df.ttest <- -diff(dim(model.mat))
  dectime2 <- model.inp$dectime
  first.day <- dectime2[1]
  
  first.year <- as.double(as.numeric(format(preddata[[sname[6]]][1], format = "%Y")))
#   first.year <- as.double(as.character(years(preddata[[sname[6]]][1])))
  
  ## determine how many observations per day in the input data set.
  Days <- unique(preddata[[sname[6]]])
  cDays <- as.character(Days)
  NVD <- as.integer(length(preddata[[sname[6]]]) / length(cDays) )
  
  if(NVD > 1){
    cat(" Input prediction data has", NVD, "observations per day.\n")
  } else {
    cat(" Input prediction data has one observation per day.\n")
  }
    
  ## Test for sequential
  TestSeq <- unique(diff(as.double(preddata[[sname[6]]])))
  if(any(TestSeq < 0)) {
    cat("\nEstimation stopped, prediction data are not in increasing date order\n\n")
    return()
  }
  if((length(TestSeq) > 1 && NVD == 1) || (length(TestSeq) > 2 && NVD > 1)){
    cat("\nCaution, prediction data contains gaps in dates\n\n")
  }    
  cat("\n Method of load estimation is", method, "\n\n")
  method <- match(method, c("AMLE", "MLE", "LAD")) # convert to numeric
  ## check for no censored data if method is LAD
  if(method == 3 && sum(model.inp$censflag[goodrows]) != 0) {
    cat("\n\n*** Data contain censored values, cannot use LAD.\n\n")
    return()
  }
  ## Process the data
  if(total) {
    out.data <- .Fortran("estltot",
                         method=as.integer(method),
                         xlcal=xlcal,
                         ylcal=ylcal,
                         yd=yd,
                         censflag=censflag,
                         NOBSC=NOBSC,
                         NPAR=NPAR,
                         sdopt=as.integer(sdopt),
                         npred=as.integer(length(dectime2)),
                         xlpred=model.mat,
                         NVD=as.integer(NVD),
                         load=double(1),
                         loadvar=double(1),
                         loadlow=double(1),
                         loadup=double(1),
                         loadsep=double(1),
                         NDAYS=integer(1),
                         IERR=integer(1))
    if(out.data$IERR <= 0) { # data are OK
      out.data$total <- out.data$load * out.data$NDAYS
      if(method != 1) 
        out.data <- data.frame(Flux=out.data$load, Variance=out.data$loadvar,
                               NDAYS=out.data$NDAYS, Load=out.data$total)
      else
        out.data <- data.frame(Flux=out.data$load, Variance=out.data$loadvar,
                               Lower.95=out.data$loadlow, Upper.95=out.data$loadup,
                               SEP=out.data$loadsep, NDAYS=out.data$NDAYS,
                               Load=out.data$total)
      cat("\nTotal Load\n")
      print(out.data)
      if(totalfile != "") # save it
        assign(totalfile, out.data, where=1)
    }
    else
      cat(" *** Error (",out.data$IERR,") occurred in processing total data. ***\n",sep="")
  } # end of total data
  if(annual != "None") {
    if(annual == "Calendar")
      TYPE <- 1
    else # assume Water year
      TYPE <- 2
    NYEARS <- max(floor(length(dectime2)/NVD/365),1)
    out.data <- .Fortran("estlann",
                         method=as.integer(method),
                         xlcal=xlcal,
                         ylcal=ylcal,
                         yd=yd,
                         censflag=censflag,
                         NOBSC=NOBSC,
                         NPAR=NPAR,
                         sdopt=as.integer(sdopt),
                         npred=as.integer(length(dectime2)),
                         xlpred=model.mat,
                         dectime2=as.integer(dectime2),
                         NVD=as.integer(NVD),
                         YEARS=integer(NYEARS),
                         load=double(NYEARS),
                         loadvar=double(NYEARS),
                         loadlow=double(NYEARS),
                         loadup=double(NYEARS),
                         loadsep=double(NYEARS),
                         NDAYS=integer(NYEARS),
                         NYEARS=as.integer(NYEARS),
                         TYPE=as.integer(TYPE),
                         IERR=integer(1))
    if(out.data$IERR > 0)
      cat(" *** Error (",out.data$IERR,") occurred in processing annual data. ***\n",sep="")
    else {
      NYEARS <- out.data$NYEARS
      ## get the year of the first data
      if(annual == "Calendar") {
        if(first.day != 0)
          out.first <- first.year + 1 # must be because would miss the first days
        else
          out.first <- first.year
        names.rows <- paste("CalendarYearEnding", seq(out.first, by=1, length=NYEARS), sep=".")
      }
      else { # water year
        if(first.day > 274)
          out.first <- first.year + 2 # must be because would miss the first days
        else
          out.first <- first.year + 1 # to account for the ending year
        names.rows <- paste("WaterYearEnding", seq(out.first, by=1,length=NYEARS),sep=".")
      }
      if(method == 1) 
        out.data <- data.frame(Flux=out.data$load[1:NYEARS], Variance=out.data$loadvar[1:NYEARS],
                               Lower.95=out.data$loadlow[1:NYEARS], Upper.95=out.data$loadup[1:NYEARS],
                               SEP=out.data$loadsep[1:NYEARS], NDAYS=out.data$NDAYS[1:NYEARS],
                               row.names=names.rows)
      else
        out.data <- data.frame(Flux=out.data$load[1:NYEARS], Variance=out.data$loadvar[1:NYEARS],
                               NDAYS=out.data$NDAYS[1:NYEARS],
                               row.names=names.rows)
      out.data$Load <- out.data$Flux * out.data$NDAYS
      cat("\nAnnual Loads\n")
      print(out.data)
      if(annualfile != "") # save it
        assign(annualfile, data.frame(out.data), where=1)
    }
  } # end of annual data
  if(season != "" ) {
    season.names <- unlist(unpaste(season, sep=","))
    season.codes <- match(season.names, month.name)
    season.codes <- c(30,59,90,120,151,181,212,243,273,304,334,365)[season.codes]
    ## they must be sorted
    season.codes <- sort(season.codes)
    NYEARS <- max(ceiling(length(dectime2)/NVD/365),1)
    NSSN <- length(season.codes)
    NDIM <- NYEARS * NSSN
    out.data <- .Fortran("estlssn",
                         method=as.integer(method),
                         xlcal=xlcal,
                         ylcal=ylcal,
                         yd=yd,
                         censflag=censflag,
                         NOBSC=NOBSC,
                         NPAR=NPAR,
                         sdopt=as.integer(sdopt),
                         npred=as.integer(length(dectime2)),
                         xlpred=model.mat,
                         dectime2=as.integer(dectime2),
                         NVD=as.integer(NVD),
                         YEARS=integer(NDIM),
                         SSN=integer(NDIM),
                         load=double(NDIM),
                         loadvar=double(NDIM),
                         loadlow=double(NDIM),
                         loadup=double(NDIM),
                         loadsep=double(NDIM),
                         NDAYS=integer(NDIM),
                         NYEARS=as.integer(NYEARS),
                         NSSN=as.integer(NSSN),
                         SEASDEF=as.integer(season.codes),
                         IERR=integer(1))
    if(out.data$IERR > 0)
      cat(" *** Error (",out.data$IERR,") occurred in processing seasonal data. ***\n",sep="")
    else {
      ## first determine the beginning season and year
      out.start <- sum(first.day <= (season.codes+1))
      if(out.start == 1)
        year.first <- first.year + 1
      else
        year.first <- first.year
    ## make season names 
      names.rows <- paste("SeasonEnding", season.names[out.data$SSN],
                       out.data$YEARS - 1 + year.first, sep="")
      NSSN <- out.data$NSSN
      if(method == 1) 
        out.data <- data.frame(Flux=out.data$load[1:NSSN], Variance=out.data$loadvar[1:NSSN],
                               Lower.95=out.data$loadlow[1:NSSN], Upper.95=out.data$loadup[1:NSSN],
                               SEP=out.data$loadsep[1:NSSN], NDAYS=out.data$NDAYS[1:NSSN],
                               row.names=names.rows[1:NSSN])
      else
        out.data <- data.frame(Flux=out.data$load[1:NSSN], Variance=out.data$loadvar[1:NSSN],
                               NDAYS=out.data$NDAYS[1:NSSN],
                               row.names=names.rows[1:NSSN])
      out.data$Load <- out.data$Flux * out.data$NDAYS
      cat("\nSeasonal loads\n")
      print(out.data)
      if(seasonfile != "") # save it
        assign(seasonfile, data.frame(out.data), where=1)
    } # end of no error
  } # end of seasonal processing
  if(monthly) {
    NYEARS <- max(ceiling(length(dectime2)/NVD/365),1)
    NDIM <- NYEARS * 12
    out.data <- .Fortran("estlmon",
                         method=as.integer(method),
                         xlcal=xlcal,
                         ylcal=ylcal,
                         yd=yd,
                         censflag=censflag,
                         NOBSC=NOBSC,
                         NPAR=NPAR,
                         sdopt=as.integer(sdopt),
                         npred=as.integer(length(dectime2)),
                         xlpred=model.mat,
                         dectime2=as.integer(dectime2),
                         NVD=as.integer(NVD),
                         YEARS=integer(NDIM),
                         SSN=integer(NDIM),
                         load=double(NDIM),
                         loadvar=double(NDIM),
                         loadlow=double(NDIM),
                         loadup=double(NDIM),
                         loadsep=double(NDIM),
                         NDAYS=integer(NDIM),
                         NYEARS=as.integer(NYEARS),
                         IERR=integer(1))
    if(out.data$IERR > 0)
      cat(" *** Error (",out.data$IERR,") occurred in processing monthly data. ***\n",sep="")
    else {
      year.first <- first.year
      ## make month names 
      names.rows <- paste(month.name[out.data$SSN],
                       out.data$YEARS - 1 + year.first, sep="")
      NYEARS <- out.data$NYEARS
      if(method == 1) 
        out.data <- data.frame(Flux=out.data$load[1:NYEARS], Variance=out.data$loadvar[1:NYEARS],
                               Lower.95=out.data$loadlow[1:NYEARS], Upper.95=out.data$loadup[1:NYEARS],
                               SEP=out.data$loadsep[1:NYEARS], NDAYS=out.data$NDAYS[1:NYEARS],
                               row.names=names.rows[1:NYEARS])
      else
        out.data <- data.frame(Flux=out.data$load[1:NYEARS], Variance=out.data$loadvar[1:NYEARS],
                               NDAYS=out.data$NDAYS[1:NYEARS],
                               row.names=names.rows[1:NYEARS])
      out.data$Load <- out.data$Flux * out.data$NDAYS
      cat("\nMonthly loads\n")
      print(out.data)
      if(monthfile != "") # save it
        assign(monthfile, data.frame(out.data), where=1)
    } # end of no error
  } # end of monthly processing
  if(daily) {
    if(NVD > 1)
      FLOW <- tapply(preddata[[snamep[2]]],list(rep(cDays,each=NVD)), mean)
    else
      FLOW <- preddata[[snamep[2]]]
    NDIM <- length(dectime2)/NVD
    out.data <- .Fortran("estlday",
                         method=as.integer(method),
                         xlcal=xlcal,
                         ylcal=ylcal,
                         yd=yd,
                         censflag=censflag,
                         NOBSC=NOBSC,
                         NPAR=NPAR,
                         sdopt=as.integer(sdopt),
                         npred=as.integer(length(dectime2)),
                         xlpred=model.mat,
                         dectime2=as.integer(dectime2),
                         NVD=as.integer(NVD),
                         load=double(NDIM),
                         loadvar=double(NDIM),
                         loadlow=double(NDIM),
                         loadup=double(NDIM),
                         loadsep=double(NDIM),
                         NDAYS=as.integer(NDIM),
                         IERR=integer(1))
    if(out.data$IERR > 0)
      cat(" *** Error (",out.data$IERR,") occurred in processing daily data. ***\n",sep="")
    else {
      NDAYS <- out.data$NDAYS
      if(method == 1) 
        out.data <- data.frame(Flux=out.data$load[1:NDAYS], Variance=out.data$loadvar[1:NDAYS],
                               Lower.95=out.data$loadlow[1:NDAYS], Upper.95=out.data$loadup[1:NDAYS],
                               SEP=out.data$loadsep[1:NDAYS], FLOW=FLOW)
      else
        out.data <- data.frame(Flux=out.data$load[1:NDAYS], Variance=out.data$loadvar[1:NDAYS],
                               FLOW=FLOW)
        
      out.data$Conc <- as.double(out.data$Flux / conv.factor / FLOW) # strip labels
      
      out.data$DATES <- Days
      if(dailyfile != "") { # save the data
        assign(dailyfile, out.data, where=1)
        cat(" Daily estimates saved in ", dailyfile, ".\n", sep="")
###        out.attr <- list(df=df.ttest, conv.factor=conv.factor)
###        assign(paste(dailyfile, "attr", sep="."), out.attr, where=1)
      }
      else {
        cat("\nDaily estimates:\n")
        print(out.data)
      }
    } # end of no error
  } # end of daily processing
  if(inst && method != 3) {
    NDIM <- length(dectime2)
    out.data <- .Fortran("estlinst",
                         method=as.integer(method),
                         xlcal=xlcal,
                         ylcal=ylcal,
                         yd=yd,
                         censflag=censflag,
                         NOBSC=NOBSC,
                         NPAR=NPAR,
                         sdopt=as.integer(sdopt),
                         npred=as.integer(NDIM),
                         xlpred=model.mat,
                         load=double(NDIM),
                         loadvar=double(NDIM),
                         loadphi=double(NDIM),
                         xomega=double(NDIM),
                         xexpon=double(NDIM),
                         xloadvar=double(NDIM),
                         IERR=integer(1))
    if(out.data$IERR > 0)
      cat(" *** Error (",out.data$IERR,") occurred in processing instantaneous data. ***\n",sep="")
    else {
      out.data <- data.frame(DATES=preddata[[sname[6]]],
                             TIMES= preddata[[sname[7]]], FLOW=preddata[[sname[4]]],
                             log.median=out.data$load, stderrpred=out.data$loadvar,
                             phi=out.data$loadphi, 
                             omega=out.data$xomega, expon=out.data$xexpon,
                             loadvar=out.data$xloadvar)
      if(instfile != "") { # save the data
        assign(instfile, out.data, where=1)
        cat(" Instantaneous estimates saved in ", instfile, ".\n", sep="")
        out.attr <- list(df=df.ttest, conv.factor=conv.factor)
        assign(paste(instfile, "attr", sep="."), out.attr, where=1)
      }
      else {
        cat("\nInstantaneous estimates:\n")
        print(out.data)
      }
    } # end of no error
  } # end of instantaneous processing
  else
    if(method == 3)
      cat(" *** Error Instantaneous estimates cannot be made using the LAD method\n")
  return()
}
