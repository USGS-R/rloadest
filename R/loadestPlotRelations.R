# dialog support functions for S-LOADEST.
#    Plot Flux-flow relations
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Jan28          This version.
#
loadestPlotRelations <- function(station, constituentlist)
{
  if(missing(station)) {
    cat(" *** The station must be selected. ***\n")
    return()
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
  
  ## remove the graphsheet if it exists!
  guiClose("GraphSheet", "Loadest")
  icount <- 0
  for(i in constituents.sel) {
    temp.df <- loadest.df[station,i][[1]]
  
    ## the load vs flow page
    icount <- icount + 1
    sname <- constituent.objs[i]
    constituent.name <- paste("L", sname, sep="")
    constituent.remark <- paste("R", sname, sep="")
    data.to.plot <- temp.df[c("FLOW", constituent.name)]
    data.to.plot[[constituent.remark]] <- factor(temp.df[[constituent.remark]])
    guiPlot(PlotType = "Color", DataSetValues = data.to.plot,
            AxisType = "Log Log", Page = "New", GraphSheet="Loadest")
    guiCreate( "MainTitle", Name = paste("Loadest$", icount, "$1", sep=""),
              Title = paste("Station",station),
              FillColor = "Transparent")
    guiModify( "YAxisTitle", Name = paste("Loadest$", icount, "$Axis2dY1$YAxisTitle", sep=""),
              Title = paste(sname, "Flux"))
    ## add the loess smooth and legend
    guiModify( "LinePlot", Name = paste("Loadest$", icount, "$1", sep=""),
              LineStyle = "Solid",
              SmoothingType = "Loess",
              NumOutputPts = "Auto")
    
    guiCreate( "Legend", Name = paste("Loadest$", icount, "$1", sep=""),
              FillColor = "Bright White",
              RoundCorners = F)
    
    guiModify( "Legend", Name = paste("Loadest$", icount, "$1", sep=""),
              xPosition = "1.6",
              yPosition = "7.0",
              UseAxesUnits = F)
    
    ## the concentration vs month page
    icount <- icount +1
    temp.df$Month <- months(temp.df$DATES)
    constituent.name <- paste("P", sname, sep="")
    data.to.plot <- temp.df[c("Month", constituent.name)]
    data.to.plot[[constituent.remark]] <- factor(temp.df[[constituent.remark]])
   
    guiPlot( PlotType = "Color", GraphSheet = "Loadest", Page="New", DataSetValues = data.to.plot, AxisType = "Linear")
    guiCreate( "MainTitle", Name = paste("Loadest$", icount, "$1", sep=""),
              Title = paste("Station",station),
              FillColor = "Transparent")
    guiModify( "YAxisTitle", Name = paste("Loadest$", icount, "$Axis2dY1$YAxisTitle", sep=""),
              Title = paste(sname, "Concentration"))
    ## add the loess smooth and legend
    guiModify( "LinePlot", Name = paste("Loadest$", icount, "$1", sep=""),
              LineStyle = "Solid",
              SmoothingType = "Loess",
              NumOutputPts = "Auto")              
    
    guiCreate( "Legend", Name = paste("Loadest$", icount, "$1", sep=""),
              FillColor = "Bright White",
              RoundCorners = F)
    
    guiModify( "Legend", Name = paste("Loadest$", icount, "$1", sep=""),
              xPosition = "1.6",
              yPosition = "7.0",
              UseAxesUnits = F)
    
  } # end plotting for loop
  
  invisible()
}
