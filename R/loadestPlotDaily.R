# dialog support functions for S-LOADEST.
#    plot daily flux estimates and optional data
#
# Coding history:
#    2004Dec10 DLLorenz original coding
#    2004Dec22 DLLorenz removed borders from areas
#    2006Jan19 DLLorenz Bug fix and added concentration plot
#    2006Mar07 DLLorenz Bug fix on concentration plot
#    2006Mar07          This version.
#
loadestPlotDaily <- function(file, observed=NULL, sname="", page=1) {
  if(missing(file)) {
    cat("\nNo data to plot.\n")
    return()
  }
  if(page == 1) {
    guiClose("GraphSheet", "Daily")
    graph <- 1
    if(!any(is.na(match(c("DATES","Lower.95","Upper.95"), names(file))))) {
      data.to.plot <- file[,c("DATES","Lower.95","Upper.95")]
      guiPlot( PlotType = "Area", DataSetValues = data.to.plot,
              GraphSheet = "Daily", Graph = "1")
      
      guiModify( "AreaPlot", Name = "Daily$1$1",
                FillDirection = "Inside data",
                MultipleYCols = "Individual",
                StartFillColor = "Transparent",
                EndFillColor = "User13")
      guiModify( "Area", Name = "Daily$1$1$Area1",
                BorderStyle = "none")
      guiModify( "Area", Name = "Daily$1$1$Area2",
                BorderStyle = "none")
      graph <- graph + 1
    }
    if(!any(is.na(match(c("DATES","Flux"), names(file))))) {
      data.to.plot <- file[,c("DATES","Flux")]
      guiPlot( PlotType = "Line", DataSetValues = data.to.plot,
              GraphSheet = "Daily", Graph = "1")
      
      guiModify( "LinePlot", Name = paste("Daily$1$", graph, sep=""),
                LineColor = "User2",
                LineWeight = "2")
      
      guiModify( "Axis2dY", Name = "Daily$1$Axis2dY1",
                AxisScaling = "Log")
      graph <- graph + 1
    }
    else {
      cat("\nInvalid data to plot.\n")
      return()
    }
    if(!is.null(observed)) {
      guiPlot( PlotType = "Scatter", DataSetValues = observed,
              GraphSheet = "Daily", Graph = "1")
      
      guiModify( "LinePlot", Name = paste("Daily$1$", graph, sep=""),
                SymbolStyle = "Circle, Solid",
                SymbolColor = "User3",
                LineStyle = "Dots",
                LineColor = "User3")
    }
    ## insert legend and Y-axis title
    guiCreate( "Legend", Name = "Daily$1$1",
              FillColor = "Bright White",
              RoundCorners = F)
    
    guiModify( "Legend", Name = "Daily$1$1",
              xPosition = "7",
              yPosition = "7",
              UseAxesUnits = F)
    guiModify( "YAxisTitle", Name = "Daily$1$Axis2dY1$YAxisTitle",
              Title = paste(sname, "Flux", sep=" "))
    return()
  } # end page == 1
    if(!any(is.na(match(c("DATES","Conc"), names(file))))) {
      data.to.plot <- file[,c("DATES","Conc")]
      guiPlot( PlotType = "Line", DataSetValues = data.to.plot,
              GraphSheet = "Daily", Graph = "1", Page="New")
      
      guiModify( "LinePlot", Name = "Daily$2$1",
                LineColor = "User2",
                LineWeight = "2")
      
    }
    else {
      cat("\nInvalid data to plot.\n")
      return()
    }
    if(!is.null(observed)) {
      guiPlot( PlotType = "Scatter", DataSetValues = observed,
              GraphSheet = "Daily", Graph = "2", Page=2)
      guiModify("LinePlot", Name = "Daily$2$2",
                SymbolStyle = "Circle, Solid")
    }
    ## insert legend and Y-axis title
    guiCreate( "Legend", Name = "Daily$2$1",
              FillColor = "Bright White",
              RoundCorners = F)
    
    guiModify( "Legend", Name = "Daily$2$1",
              xPosition = "7",
              yPosition = "7",
              UseAxesUnits = F)
    guiModify( "YAxisTitle", Name = "Daily$2$Axis2dY1$YAxisTitle",
              Title = paste(sname, "Concentration", sep=" "))
    return()
  
}
