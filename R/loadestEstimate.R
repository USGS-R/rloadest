# dialog support functions for S-LOADEST.
#    Compute load estimates
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Oct29 DLLorenz modified for loadest.conv.factor
#    2010Sep24 DLLorenz Tweaks
#    2010Sep24          This version.
# 
loadestEstimate <- function(station, sname, data, exact=T, total=F, totalfile="",
                             annual="None", annualfile="", season="", seasonfile="",
                             monthly=F, monthfile="", daily=F, dailyfile="",
                             inst=F, instfile="") {
  if(!any(total, annual != "None", season != "", monthly, daily, inst)) {
    cat(" *** No output loads selected, nothing to do. ***\n")
    return()
  }
  cat(" *** Estimate Loads ***\nStation:", station,"\nConstituent:", sname,"\n")
  loadest.snames <- get("loadest.snames", where=1)
  sname.conc <- loadest.snames[loadest.snames$snames == sname, 2]
  sname.load <- loadest.snames[loadest.snames$snames == sname, 3]
  cat("  Concentration units are in", sname.conc,", load units are in", sname.load, "\n")
  ## get the data and the load model
  loadest.df <- get("loadest.df", where=1)
  loadest.temp <- loadest.df[station, sname][[1]]
  loadest.model <- get("loadest.load.model", where=1)
  loadest.model <- loadest.model[station, sname][[1]]
  ## compute conversion factor
  flow.units <- get("loadest.flow.units", where=1)
  conv.factor <- loadest.conv.factor(flow.units, sname.conc, sname.load)
  ## The prediction data might need DECTIME if
  ## the model calls for any time variable.  So add it.
  Year <- month.day.year(data$DATES)$year
  Days <- as.double(data$DATES) - julian(1,1,Year)
  Days <- Days + as.double(substring(data$TIMES,1,2))/24 +
    as.double(substring(data$TIMES,3,4))/1440
  data$DECTIME <- Year + Days/(365 + as.double(leap.year(Year)))
  ## let loadestimEstimate do the work!
  sname <- c(paste(c("R", "L", "D"), sname, sep=""), "FLOW", "DECTIME", "DATES", "TIMES")
  loadestimEstimate(sname, loadest.temp, data, loadest.model, exact,
                    total, totalfile, annual, annualfile,
                    season, seasonfile, monthly, monthfile,
                    daily, dailyfile, inst, instfile, conv.factor)
                    
  return()
}
