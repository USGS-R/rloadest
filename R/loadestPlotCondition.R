# dialog support functions for S-LOADEST.
#    Plot flux-flow relations conditioned by time
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Jan28          This version.
#
loadestPlotCondition <- function(station, constituentlist)
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
  guiClose("GraphSheet", "Loadest")
  icount <- 0
  for(i in constituents.sel) {
    temp.df <- loadest.df[station,i][[1]]
  
    ## the load vs flow conditioned by date page
    icount <- icount + 1
    sname <- constituent.objs[i]
    constituent.name <- paste("L", sname, sep="")
    data.to.plot <- temp.df[c("FLOW", constituent.name, "DATES")]
    guiPlot(PlotType = "Scatter", DataSetValues = data.to.plot,
            AxisType = "Log Log", Page = "New", GraphSheet="Loadest",
            NumConditioningVars = "1")
    ## insert the title
    guiCreate( "MainTitle", Name = paste("Loadest$", icount, "$1", sep=""),
              Title = paste("Station",station),
              FillColor = "Transparent")
    ## adjust the number of panels to the number of years
    Years <- as.integer(diff(range(as.double(data.to.plot$DATES)))/365 + .5)
    if(Years < 1)
      Years <- 1
    guiModify( "Graph2D", Name = paste("Loadest$", icount, sep=""),
              NumberOfPanels = format(Years),
              IntervalType = "Equal Ranges")
    ## change the axis title
    guiModify( "YAxisTitle", Name = paste("Loadest$", icount, "$Axis2dY1$YAxisTitle", sep=""),
              Title = paste(sname, "Flux"))
    ## add the loess smooth and legend
    guiModify( "LinePlot", Name = paste("Loadest$", icount, "$1", sep=""),
              LineStyle = "Solid", SmoothingType = "Loess", Span = "1",
              NumOutputPts = "Auto")    
    x.range <- format(range(data.to.plot$FLOW))
    y.range <- format(range(data.to.plot[[constituent.name]]))
    ## add a reference line
    guiCreate( "Line", Name = paste("Loadest$", icount, "$1", sep=""),
              LineStyle = "Solid",xStart = x.range[1], yStart = y.range[1],
              xEnd = x.range[2], yEnd = y.range[2], UseAxesUnits = T,
              LineWeight = "2")
  } # end plotting for loop
  
  invisible()
}
