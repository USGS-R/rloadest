# dialog support functions for S-LOADEST.
#    Build model from the Predefined/Auto page
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Jan29 DLLorenz Cleaned up code
#    2004May18 DLLorenz Removed LA method
#    2004Nov03 DLLorenz Check for censored values in LAD
#    2005Oct07 DLLorenz Added printing of Qadj and Tadj
#    2010Jun15 DLLorenz Added return of model object
#    2010Jun15          This version.
#
loadestBuildAuto <- function(sname, stations = "<ALL>", control = 80, model = "best model", method = "AMLE")
{
  method.save <- method
  model.name <- model
  ## get the data and status files
  loadest.df <- get("loadest.df", where=1)
  loadest.status <- get("loadest.status", where=1)
  ## get or create the load.model file
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
  first.loop <- T
  cat("	***  S-LOADEST Load models  ***\n")
  cat("\nConstituent: ", sname, "\n")
  pname <- paste("P", sname, sep="")
  rname <- paste("R", sname, sep="")
  if(stations == "<ALL>") # get all the stations if requested
    stations <- get("loadest.stations", where=1)$stations
  for(i in stations) {
    method <- method.save
    loadest.temp <- loadest.df[i, sname][[1]]
    model <- unlist(unpaste(model.name, sep = " "))[1] # get the model type
    ## first determine if it should be analyzed
    if(loadest.status[i,sname][[1]] == "data OK" ||
       loadest.status[i,sname][[1]] == "calibrated" ) {
      cat(" Station:", i, "\n")
      ## check if the percentage of censored values exceeds the control value
      temp.ltv <- loadestCheckLtv(loadest.temp, pname, rname)
      if(temp.ltv$pct > control) {
        cat(" Percentage of censored values (", temp.ltv$pct, ") exceeds", control, "\n")
        model <- "0"
        if(temp.ltv$zeros > 0)
          cat("Zero values (nondetected values with unrecorded reporting limit) were found\n")
      }
      ## determine the regression fit.
      ## to get exact agreement with the stand-alone version, we must use the
      ## routines in loadest to do the analysis, not survReg--we give up
      ## influence statistics.
      if(model == "best") {
        Qadj <- loadestQadj(loadest.temp$FLOW)
        Tadj <- loadestTadj(loadest.temp$DECTIME)
        cat("  Selecting 'best' model for station", i, "\n")
        model <- loadestSelBest(loadest.temp, sname, Qadj, Tadj)
        cat("\n  Model number", model, "selected.\n")
      } # end of "best model selection"
      
      if(model == "0") { # no load model
        cat(" No load model!.\n")
        ## change the model number in the fac.model file
        loadest.load.model[i,sname][[1]] <- 0
      }
      else { # any model from 1 to 9
        Qadj <- loadestQadj(loadest.temp$FLOW)
        Tadj <- loadestTadj(loadest.temp$DECTIME)
        model.inp <- loadestSetXLDat(loadest.temp,sname,as.double(model),Qadj,Tadj)
        goodrows <- !is.na(rowSums(model.inp$xlcal))
        ## print the header info
        loadestPrintHeader(goodrows, model.inp, loadest.temp$DATES,
                           loadest.temp$FLOW, Qadj, Tadj)
        ## now run the calibration for AMLE if requested
        if(method == "AMLE") {
          evaluat <- loadestEvalAMLE(goodrows, model.inp)
          cat("\nDiagnostic plots based on the AMLE method.\n")
          cat("\nThe user must evaluate the diagnostic plots to assess fit and choice of method for prediction\n")
          title.plot <- paste(i, ", ", sname, ", Model: ", model.name,", Method: AMLE", sep="")
          par(tck=0.02, xaxs='s', yaxs='s')
          loadestPlotModel(loadest.temp[goodrows,], evaluat, as.double(model), title.plot,
                           model.inp$censflag[goodrows])
          if(first.loop) { # get the graph and setup to force all new graphs to go onto separate pages
            name.graph <- guiGetGraphName(GraphSheet="", GraphNum=1)
            guiModify("GraphSheet", name = name.graph ,AutoPageMode = "Every Graph")
            first.loop <- F
          }
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
            title.plot <- paste(i, ", ", sname, ", Model: ", model.name,", Method: LAD", sep="")
            par(tck=0.02, xaxs='s', yaxs='s')
            loadestPlotModel(loadest.temp[goodrows,], evaluat, as.double(model), title.plot,
                             model.inp$censflag[goodrows])
            if(first.loop) { # get the graph and setup to force all new graphs to go onto separate pages
              name.graph <- guiGetGraphName(GraphSheet="", GraphNum=1)
              guiModify("GraphSheet", name = name.graph ,AutoPageMode = "Every Graph")
              first.loop <- F
            }
          }
          else
            cat("\n\n*** Data contain censored values, cannot use LAD.\n\n")
          ## change the model number in the load.model file
        } # end of LAD analysis
        method <- match(method, c("AMLE", "MLE", "LAD"))
        loadest.load.model[i,sname][[1]] <- c(as.double(model),method)
      } # end of if..else if..else section for creating model
      loadest.status[i,sname][[1]] <- "calibrated"
    } # end of if(status OK)
  } # end of loop
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
