# dialog support functions for S-LOADEST.
#    Build model from the Predefined/Auto page
#    for a single site and constituent.
#
# Coding history:
#    2004Jan29 DLLorenz Original version for USGS library
#    2004May18 DLLorenz Removed LA method
#    2004Nov03 DLLorenz Modified for no censored data
#    2005Oct07 DLLorenz Added printing of Qadj and Tadj
#    2005Dec02 DLLorenz Bug fix.
#    2005Dec02          This version.
#
loadestimBuildAuto <- function(loadest.temp, sname, model = "best model",
                               method = "AMLE", normal.plot = T,
                               sl.plot = F, partial.plot=T, savefile="")
{
  ## sname is remarks, load, detection, flow, dectime, dates
  model.name <- model
  ## some initial processing
  options(warn=-1) # suppress warning messages
  cat("	***  Load estimation model  ***\n")
  
#   model <- unlist(unpaste(model.name, sep = " "))[1] # get the model type  #Laura's edit....
  
  ## determine the regression fit.
  ## to get exact agreement with the stand-alone version, we must use the
  ## routines in loadest to do the analysis, not survReg--we give up
  ## influence statistics.
  if(model == "best") {
    Qadj <- loadestQadj(loadest.temp[[sname[4]]])
    Tadj <- loadestTadj(loadest.temp$DECTIME)
    cat("  Selecting 'best' model.\n")
    model <- loadestSelBest(loadest.temp, sname, Qadj, Tadj)
    cat("\n  Model number", model, "selected.\n")
  } # end of "best model selection"
  ##
  if(model == "0") {# no load model
    cat(" No load model!.\n")
  } else { # any model from 1 to 9
    Qadj <- loadestQadj(loadest.temp[[sname[4]]])
    Tadj <- loadestTadj(loadest.temp$DECTIME)
    model.inp <- loadestSetXLDat(loadest.temp,sname,as.double(model),Qadj,Tadj)
    goodrows <- !is.na(rowSums(model.inp$xlcal))
    ## print the header info
    loadestPrintHeader(goodrows, model.inp, loadest.temp[[sname[6]]],
                       loadest.temp[[sname[4]]], Qadj, Tadj)
    ## now run the calibration for AMLE if requested
    if(method == "AMLE") {
      evaluat <- loadestEvalAMLE(goodrows, model.inp)
      if(any(c(normal.plot, sl.plot, partial.plot))) {
        cat("\nDiagnostic plots based on the AMLE method.\n")
        cat("\nThe user must evaluate the diagnostic plots to assess fit and choice of method for prediction\n")
        title.plot <- paste(sname[2], ", Model: ", model.name,", Method: AMLE", sep="")
        par(tck=0.02, xaxs='s', yaxs='s')
        loadestimPlotModel(loadest.temp[goodrows,], evaluat, as.double(model),
                           title.plot, model.inp$censflag[goodrows], sname,
                           normal.plot, sl.plot, partial.plot)
      } # End of plot requests
    } # end of AMLE analysis
    if(method == "MLE") {
      evaluat <- loadestEvalMLE(goodrows, model.inp)
      cat("\nNo diagnostic plots based on the MLE method are available.\n")
      cat("\nThe user should evaluate the diagnostic plots for the AMLE method\nto assess fit and choice of method for prediction\n")
    } # end of MLE analysis
    if(method == "LAD") {
      NCENS = sum(model.inp$censflag[goodrows])
      if(NCENS == 0) {
        evaluat <- loadestEvalLAD(goodrows, model.inp)
        if(any(c(normal.plot, sl.plot, partial.plot))) {
          cat("\nDiagnostic plots based on the LAD method.\n")
          cat("\nThe user must evaluate the diagnostic plots to assess fit and choice of method for prediction\n")
          title.plot <- paste(sname[2], ", Model: ", model.name," Method: LAD", sep="")
          par(tck=0.02, xaxs='s', yaxs='s')
          loadestimPlotModel(loadest.temp[goodrows,], evaluat, as.double(model),
                             title.plot, model.inp$censflag[goodrows], sname,
                             normal.plot, sl.plot, partial.plot)
        } # End of plot requests
      } else {
        cat("\n\n*** Data contain censored values, cannot use LAD.\n\n")
      }       
    } # end of LAD analysis
  } # end of if..else if..else section for creating model
  ## save residuals to the savefile, if possible
  if(as.double(model) > 0 && method != "MLE" && savefile != "") {
    data <- get(savefile)
    data$Resid <- as.double(NA)
    data$Resid[goodrows] <- evaluat$resid
    assign(savefile, data, where=1)
  }
  invisible()
  return(c(as.double(model), match(method, c("AMLE", "MLE", "LAD"))))
}
