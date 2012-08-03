# dialog support functions for S-LOADEST.
#    Build model from the Custom page
#    for a single site and constituent.
#
# Coding history:
#    2004Jan29 DLLorenz Original version for USGS library
#    2004May05 DLLorenz Bug fix.
#    2004May18 DLLorenz Removed LA method
#    2004Nov03 DLLorenz Modified for no censored data
#    2005Oct07 DLLorenz Added printing of Qadj and Tadj
#    2005Oct07          This version.
#    2012Aug02 ldecicco Conversion to R

#' LOADEST Calibration 
#'
#' Calibration stuff for loeadest...
#' @param data string descriptive flow unit
#' @param sname string descriptive concentration unit
#' @param flowtrans string descriptive load unit
#' @param floworder string descriptive flow unit
#' @param lineartime string descriptive concentration unit
#' @param seasonal string descriptive load unit
#' @param period string defaults to ""
#' @param additional.terms string defaults to "<None>"
#' @param diurnal string descriptive load unit
#' @param method string defaults to "AMLE"
#' @param normal.plot logical defaults to FALSE
#' @param sl.plot logical defaults to FALSE
#' @param partial.plot logical defaults to FALSE
#' @param savefile string defaults to ""
#' @keywords unit conversions
#' @return conv.factor list conversion factor
#' @export
#' @examples
#' # These examples require an internet connection to run
#' loadest.conv.factor('cubic meter per second','milligrams per liter','pounds')
loadestimCalibrate <- function(data, sname, flowtrans, floworder,
                               lineartime, seasonal, period="",
                               additional.terms = "<None>", diurnal,
                               method = "AMLE",
                               normal.plot=F, sl.plot=F, partial.plot=F, savefile="") {
  options(warn=-1) # suppress warning messages
  cat("	***  Load estimation model  ***\n")
  model <- c(flowtrans, floworder,lineartime, seasonal, period,
             additional.terms, diurnal, method)
  Qadj <- loadestQadj(data[[sname[4]]])
  Tadj <- loadestTadj(data[[sname[5]]])
  model.inp <- loadestSetXLDat(data ,sname, -1, Qadj, Tadj,
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
  loadestPrintHeader(goodrows, model.inp, data[[sname[6]]],
                     data[[sname[4]]], Qadj, Tadj)
  ## now run the calibration for AMLE if requested
  if(method == "AMLE") {
    evaluat <- loadestEvalAMLE(goodrows, model.inp)
    if(any(c(normal.plot, sl.plot, partial.plot))) {
      cat("\nDiagnostic plots based on the AMLE method.\n")
      cat("\nThe user must evaluate the diagnostic plots to assess fit and choice of method for prediction\n")
      title.plot <- paste(sname[2], ", Model: Custom, Method: AMLE", sep="")
      par(tck=0.02, xaxs='s', yaxs='s')
      loadestimPlotModel(data[goodrows,], evaluat, model, title.plot,
                         model.inp$censflag[goodrows], sname,
                         normal.plot, sl.plot, partial.plot)
      name.graph <- guiGetGraphName(GraphSheet="", GraphNum=1)
      guiModify("GraphSheet", name = name.graph ,AutoPageMode = "Every Graph")
    }
  } # end of AMLE analysis
  if(method == "MLE") {
    evaluat <- loadestEvalMLE(goodrows, model.inp)
    if(any(c(normal.plot, sl.plot, partial.plot))) {
      cat("\nNo diagnostic plots based on the MLE method are available.\n")
      cat("\nThe user should evaluate the diagnostic plots for the AMLE method\nto assess fit and choice of method for prediction\n")
    }
  } # end of MLE analysis
  if(method == "LAD") {
    NCENS = sum(model.inp$censflag[goodrows])
    if(NCENS == 0) {
      evaluat <- loadestEvalLAD(goodrows, model.inp)
      if(any(c(normal.plot, sl.plot, partial.plot))) {
        cat("\nDiagnostic plots based on the LAD method.\n")
        cat("\nThe user must evaluate the diagnostic plots to assess fit and choice of method for prediction\n")
        title.plot <- paste(sname[2], ", Model: Custom, Method: LAD", sep="")
        par(tck=0.02, xaxs='s', yaxs='s')
        loadestimPlotModel(data[goodrows,], evaluat, model, title.plot,
                           model.inp$censflag[goodrows], sname,
                           normal.plot, sl.plot, partial.plot)
      }
      else
        cat("\n\n*** Data contain censored values, cannot use LAD.\n\n")
    }
  } # end of LAD analysis
  ## change the model number in the load.model file
  if(method != "MLE" && savefile != "") {
    data <- get(savefile)
    data$Resid <- as.double(NA)
    data$Resid[goodrows] <- evaluat$resid
    assign(savefile, data, where=1)
  }
  invisible()
  return(c(flowtrans, floworder,lineartime, seasonal, period,
           additional.terms, diurnal, method))
}
