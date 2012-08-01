# dialog support functions for S-LOADEST.
#    Build model from the Custom page
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004May18 DLLorenz Removed LA Method
#    2004Nov10 DLLorenz Modified for quadratic time
#    2005Oct07 DLLorenz Added printing of Qadj and Tadj
#    2010Jun15 DLLorenz Added return of model object
#    2010Jun15          This version.
#
loadestCalibrate <- function(sname, station, control, flowtrans, floworder,
                             lineartime, seasonal, period,
                             additional.terms = "<None>", diurnal,
                             method = "AMLE") {
  ## get the data and status files
  loadest.df <- get("loadest.df", where=1)
  loadest.status <- get("loadest.status", where=1)
  ## get or create the load.model file although this should exist!
  if(exists("loadest.load.model",where=1))
    loadest.load.model <- get("loadest.load.model",where=1)
  else {
    temp.stations <- get("loadest.stations", where=1)$stations
    temp.snames <- get("loadest.snames", where=1)$snames
    temp.df <- as.data.frame(cbind(rep(temp.stations,each=length(temp.snames)),rep(temp.snames,length(temp.stations))))
    ## create the by-object for the load model
    loadest.load.model <- by(temp.df,list(stations=temp.df[[1]], snames=temp.df[[2]]),FUN=function(x) -1)
  }
  ## some initial processing
  options(warn=-1) # suppress warning messages
  cat("	***  S-LOADEST Load models  ***\n")
  cat("\nConstituent: ", sname, "\n")
  pname <- paste("P", sname, sep="")
  rname <- paste("R", sname, sep="")
  if(station == "<ALL>") { # can't get all the stations
    cat("\n *** Select a single station ***\n")
    return()
  }
  loadest.temp <- loadest.df[station, sname][[1]]
  ## first determine if it should be analyzed
  if(loadest.status[station,sname][[1]] == "data OK" ||
     loadest.status[station,sname][[1]] == "calibrated" ) {
    cat(" Station:", station, "\n")
    ## check if the percentage of censored values exceeds the control value
    temp.ltv <- loadestCheckLtv(loadest.temp, pname, rname)
    if(temp.ltv$pct > control) {
      cat(" Percentage of censored values (", temp.ltv$pct, ") exceeds", control, "\n")
      if(temp.ltv$zeros > 0)
        cat("Zero values (nondetected values with unrecorded reporting limit) were found\n")
      cat(" No load model!.\n")
      ## change the model number in the load.model file
      loadest.load.model[station,sname][[1]] <- 0
    }
    ## determine the regression fit.
    
    else { # compose the specified model
      model <- character()
      if(lineartime == 1)
        model <- "DATES"
      if(lineartime == 2)
        model <- "DATES + DATES^2"
      if(seasonal == "sinusoidal")
        model <- c(model, "SEASON")
      if(seasonal == "period")
        model <- c(model, unlist(unpaste(period,sep=",")))
      if(additional.terms != "<None>")
        model <- c(model, unlist(unpaste(additional.terms)))
      if(diurnal)
        model <- c(mode, "TIMES")
      if(length(model) == 0)
        model <- -1
      if(seasonal != "period")
        period <- " "
      Qadj <- loadestQadj(loadest.temp$FLOW)
      Tadj <- loadestTadj(loadest.temp$DECTIME)
      model.inp <- loadestSetXLDat(loadest.temp,sname, -1, Qadj, Tadj,
                                   flowtrans, floworder, lineartime, seasonal,
                                   period, additional.terms, diurnal)
      ## need to see if breakpoint regression was selected and if it succeeded
      if(floworder == "one breakpoint" || floworder == "two breakpoints") {
        if(exists("loadest.breakpoint", where=1)) { # it was successful
          Bp <- get("loadest.breakpoint", where=1)
          Bp <- format(Bp, digits=8)
          floworder <- paste(c("piecewise", Bp), collapse=",")
          rm(loadest.breakpoint) # remove the evidence
        }
        else # it failed
          floworder <- "linear"
      }
      NPAR <- model.inp$NPAR
      goodrows <- !is.na(rowSums(model.inp$xlcal)) # need to keep track for printing and plotting
      ## print the header info
      loadestPrintHeader(goodrows, model.inp, loadest.temp$DATES,
                         loadest.temp$FLOW, Qadj, Tadj)
      ## now run the calibration for AMLE if requested
      if(method == "AMLE") {
        evaluat <- loadestEvalAMLE(goodrows, model.inp)
        cat("\nDiagnostic plots based on the AMLE method.\n")
        cat("\nThe user must evaluate the diagnostic plots to assess fit and choice of method for prediction\n")
        title.plot <- paste(station, ", ",sname, ", Model: Custom, Method: AMLE", sep="")
        par(tck=0.02, xaxs='s', yaxs='s')
        loadestPlotModel(loadest.temp[goodrows,], evaluat, model, title.plot,
                         model.inp$censflag[goodrows])
        name.graph <- guiGetGraphName(GraphSheet="", GraphNum=1)
        guiModify("GraphSheet", name = name.graph ,AutoPageMode = "Every Graph")
        ## change the model number in the load.model file
      } # end of AMLE analysis
      if(method == "MLE") {
        evaluat <- loadestEvalMLE(goodrows, model.inp)
        cat("\nNo diagnostic plots based on the MLE method are available.\n")
        cat("\nThe user should evaluate the diagnostic plots for the AMLE method\nto assess fit and choice of method for prediction\n")
        ## change the model number in the load.model file
      } # end of MLE analysis
      if(is.element(method, c("LAD"))) {
        NCENS = sum(model.inp$censflag[goodrows])
        if(NCENS == 0) {
          evaluat <- loadestEvalLAD(goodrows, model.inp)
          cat("\nDiagnostic plots based on the LAD method.\n")
          cat("\nThe user must evaluate the diagnostic plots to assess fit and choice of method for prediction\n")
          title.plot <- paste(station, ", ", sname, ", Model: Custom, Method: LAD", sep="")
          par(tck=0.02, xaxs='s', yaxs='s')
          loadestPlotModel(loadest.temp[goodrows,], evaluat, model, title.plot,
                         model.inp$censflag[goodrows])
        }
        else
          cat("\n\n*** Data contain censored values, cannot use LAD.\n\n")
        ## change the model number in the load.model file
      } # end of LAD analysis
      ## change the model number in the load.model file
      loadest.load.model[station, sname][[1]] <- c(flowtrans, floworder,
                                                   lineartime, seasonal, period,
                                                   additional.terms, diurnal, method)
    } # end of if..else section for checking control
    loadest.status[station, sname][[1]] <- "calibrated"
  } # end of if(status OK)
  ## save the data now
  assign("loadest.status", loadest.status, where=1)
  assign("loadest.load.model", loadest.load.model, where=1)
  loadest.status.list <- get("loadest.status.list", where=1)
  ## make sure that the length of the list is 7, if not assume that this step is being repeated intentionally
  if(length(loadest.status.list) == 5) {
    loadest.status.list <- c(loadest.status.list, "calibrated")
    assign("loadest.status.list", loadest.status.list, where = 1)
  }
  invisible()
  return(evaluat)
}
