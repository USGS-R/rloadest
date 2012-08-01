# dialog support functions for S-LOADEST.
#    Plot flow-adjusted concentrations against additional variables
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Jan28          This version.
#
loadestPlotAdditional <- function(station, constituentlist)
{
  if(missing(station)) {
    cat(" *** The station must be selected. ***\n")
    return(F)
  }
  if(!exists("loadest.expvars", where=1)) {
    cat(" *** No additional explanatory variables were selected. ***\n")
    return(F)
  }
  ## get the data file
  loadest.df <- get("loadest.df", where=1)
  ## get the list of constituents
  constituent.objs <- dimnames(loadest.df)$snames
  ## get the selected constituents, and create a vector of stations to choose
  if(missing(constituentlist))
    constituent.list <- constituent.objs
  else
    constituent.list <- unlist(unpaste(constituentlist,sep=','))
  constituents.sel <- charmatch(constituent.list, constituent.objs)
  ## get the additional explanatory variables
  loadest.expvars <- get ("loadest.expvars", where=1)
  loadest.expvars <- unlist(unpaste(loadest.expvars,sep=','))
  
  ## remove the graphsheet if it exists!
  guiClose("GraphSheet", "Loadest")
  icount <- 0
  for(i in constituents.sel) {
    temp.df <- loadest.df[station,i][[1]]
    sname <- constituent.objs[i]
    constituent.name <- paste("L", sname, sep="")
    constituent.remark <- paste("R", sname, sep="")
    ## for this, we need to look at the log(flow adjusted concentration)
    ## and we will assume that there are few or no censored values
    FAC <- lsfit(poly(log(temp.df$FLOW),2),log(temp.df[[constituent.name]]))$residuals
    for(var in loadest.expvars) {
    ## the concentration vs additional variables pages
      icount <- icount +1
      data.to.plot <- temp.df[var]
      data.to.plot[["FAC"]] <- FAC
      data.to.plot[[constituent.remark]] <- factor(temp.df[[constituent.remark]])
   
      guiPlot( PlotType = "Color", GraphSheet = "Loadest", Page="New",
              DataSetValues = data.to.plot, AxisType = "Linear")
      guiCreate( "MainTitle", Name = paste("Loadest$", icount, "$1", sep=""),
                Title = paste("Station",station),
                FillColor = "Transparent")
      guiModify( "YAxisTitle", Name = paste("Loadest$", icount, "$Axis2dY1$YAxisTitle", sep=""),
                Title = paste("Log-Flow Adjusted", sname, "Concentration"))
    ## add the loess smooth and legend
      guiModify( "LinePlot", Name = paste("Loadest$", icount, "$1", sep=""),
                LineStyle = "Solid",
                SmoothingType = "Least Squares",
                NumOutputPts = "Auto")
    
      guiCreate( "Legend", Name = paste("Loadest$", icount, "$1", sep=""),
                FillColor = "Bright White",
                RoundCorners = F)
    
      guiModify( "Legend", Name = paste("Loadest$", icount, "$1", sep=""),
                xPosition = "1.6",
                yPosition = "7.0",
                UseAxesUnits = F)
    } # end of variables loop
  } # end of constituents for loop
  
  invisible(T)
}
