# dialog support functions for S-LOADEST.
#    Print contents of loadest.load.model
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004May18 DLLorenz Removed LA method
#    2004May18          This version.
#
loadestPrintMethod <- function() {
  ## interpret and print the contents of loadest.load.model
  loadest.load.model <- get("loadest.load.model", where=1)
  cat("\n	***  S-LOADEST model selections.  ***\n")
  loadest.stations <- get("loadest.stations", where=1)$stations
  loadest.snames <- get("loadest.snames", where=1)$snames
  for(i in loadest.stations) {
    cat("Station:", i,"\n")
    for(j in loadest.snames) {
      cat("   Constituent:", j, "\n")
      model.list <- loadest.load.model[i,j][[1]]
      if(is.numeric(model.list)) {
        if(model.list[1] == -1) 
          cat("     No model selected\n")
        else {
          cat("     Predefined model", model.list[1], "selected, explanatory variables:\n")
          cat("     ", c("Ln(FLOW)", "Ln(FLOW) + Ln(FLOW)^2", "Ln(FLOW) + Time",
                         "Ln(FLOW + Sin(Time) + Cos(Time)", "Ln(FLOW) + Ln(FLOW)^2 + Time",
                         "Ln(FLOW) + Ln(FLOW)^2  + Sin(Time) + Cos(Time)",
                         "Ln(FLOW) + Time + Sin(Time) + Cos(Time)",
                         "Ln(FLOW) + Ln(FLOW)^2 + Time + Sin(Time) + Cos(Time)",
                         "Ln(FLOW) + Ln(FLOW)^2 + Time + Time^2 + Sin(Time) + Cos(Time)")[model.list[1]],
              "\n")
         cat("      Method is", c("AMLE", "LAD")[model.list[2]],"\n")
        }
      } # end of numeric
      else { #must be character
        cat("      Custom model selected, explanatory variables:\n")
        cat("      Flow transform is", model.list[1], "\n")
        if(substring(model.list[2],1,5) == "piece") {
          numbreak <- length(unlist(unpaste(model.list[2], sep=","))) - 1
          if(numbreak == 1)
            cat("      Flow term is piecewise linear with one breakpoint\n")
          else
            cat("      Flow term is piecewise linear with two breakpoints\n")
        }
        else
          cat("      Flow order is", model.list[2], "\n")
        if(model.list[3] == "TRUE")
          cat("      Linear time (Time)\n")
        if(model.list[4] == "sinusoidal")
          cat("      Sin(Time) + Cos(Time)\n")
        else if(model.list[4] == "period")
          cat("      Seasonal period:", model.list[5], "\n")
        if(model.list[6] != "<none>")
          cat("      Additional terms:", model.list[6], "\n")
        if(model.list[7] == "TRUE")
          cat("      Sin(Time of day) + Cos(Time of Day)\n")
        cat("      Method is", model.list[8], "\n")
      } # end of character
    } # end of constitnuent loop
  } # end of station loop
  invisible()
}
