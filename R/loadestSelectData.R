# dialog support functions for S-LOADEST.
#    function supporting menuSelectLoadest
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Nov10 DLLorenz Modified to save in different data frame
#    2004Dec10 DLLorenz Modified added data checks
#    2005Feb18 DLLorenz Bug Fix on data check
#    2005Feb18          This version.
#
loadestSelectData <- function(data.object, data.name) {
  ## five tests on the data
  ## first verify that STAID, DATES, TIMES, and FLOW are present
  SL.temp <- names(data.object)
  if(any(is.na(match(c("STAID", "DATES", "TIMES", "FLOW"),
                     SL.temp))))
    stop("At least one required column is missing.")
  ## second make sure that there are no spaces in the STAID column--
  ## that confuses some of the cb functions
  data.object$STAID <- strip.blanks(data.object$STAID)
  if(length(grep(" ", data.object$STAID)) > 0)
    stop("Spaces found in STAID. Fix and rerun.")
  ## third force all R columns to be character
  Rnames <- grep("^R", names(data.object))
  for(i in Rnames)
    data.object[[i]] <- as.character(data.object[[i]])
  ## fourth double check for matching P and R column names
  SL.snames <- data.frame(snames = as.character(substring(SL.temp[grep("^P",SL.temp)],2)), stringsAsFactors=F)
  Rnomatch <- which.na(match(paste("R", SL.snames$snames, sep=""), SL.temp))
  if(length(Rnomatch) > 0) {
    cat("\n Missmatched column names:", SL.snames$snames[Rnomatch], "\n", sep=" ")
    stop("Fix value and remark column names.")
  }
  ## fifth remove missing FLOW data
  data.object <- data.object[!is.na(data.object$FLOW),]
  assign("loadest.data.frame", data.object, where=1)
  ## extract the stations and constituents from the data file (data.object)
  ## and save them in their own separate files.
  SL.stations <- data.frame(stations = as.character(unique(data.object$STAID)),
                            stringsAsFactors=F) 
  ## add a column to the station data file to record how many constituents can be analyzed for the station
  count <- rep(0,dim(SL.stations)[1])
  ## convert data.object to a name and save it for the next step.
  SL.data <- data.name
  ## OK now do summary statistics on the data
  cat("\n	*** Summary statistics for the S-LOADEST data. ***\n")
  loop <- 0
  for(station in SL.stations$stations) {
    cat("\nStation: ", station, "\n")
    temp.df <- data.object[data.object$STAID==station,"FLOW"]
    cat("Statistics of the flow data\n")
    print(quantile(temp.df,probs=c(0,.1,.25,.5,.75,.9,1)))
    if(min(temp.df) <= 0)
      cat("\n****Caution zero flow values cannot be used in the calibration data\n\n")
    loop <- loop +1
    for(code in SL.snames$snames) {
      cat("\n   Constituent: ", code, "\n")
      pcode <- paste("P", code, sep="")
      rcode <- paste("R", code, sep="")
      columns <- c("DATES", rcode, pcode)
      temp.df <- data.object[data.object$STAID==station,columns]
      ## remove those observations where the P-value is NA
      temp.df <- temp.df[!is.na(temp.df[[pcode]]),]
      if(dim(temp.df)[1] == 0)
        cat("No data.\n")
      else {
        if(dim(temp.df)[1] < 20)
          cat("Insufficient data, only ", dim(temp.df)[1], " observations.\n")
        else {
          if(max(as.double(temp.df$DATES),na.rm=T) - min(as.double(temp.df$DATES),na.rm=T) < 335) # 1 year less 1/2 month on each end!
            cat("Record too short for analysis.\n Earliest: ",
                format(min(temp.df$DATES,na.rm=T)), ", latest: ",
                format(max(temp.df$DATES,na.rm=T)), "\n")
          else { # provide more complete summary of statistics
            cat(dim(temp.df)[1], " observations from ",
                format(min(temp.df$DATES,na.rm=T)), " to ",
                format(max(temp.df$DATES,na.rm=T)), "\n")
            num.lt <- sum(temp.df[[rcode]] == "<")
            num.zero <- sum(temp.df[[pcode]] == 0)
            cat(num.lt, " less than values and ", num.zero,
                "zero values (",
                round(100 * (num.lt + num.zero)/dim(temp.df)[1], digits=1),
                " %)\n")
            cat("Statistics of the raw concentration data\n")
            print(quantile(temp.df[[pcode]],probs=c(0,.1,.25,.5,.75,.9,1)))
            ## increment the counter
            count[loop] <- count[loop] + 1
          } # end of complete summary
        } # end of else (dim > 50)
      } # end of else (dim > 0)
    } # end of for station
  } # end of for code
  ## save the data
  SL.stations$count <- count
  ## add conc and load columns to the sname data frame
  nsnames <- dim(SL.snames)[1]
  SL.snames$conc <- character(nsnames)
  SL.snames$load <- character(nsnames)
  SL.snames$detlim <- numeric(nsnames)
  assign("loadest.data", SL.data, where=1)
  assign("loadest.stations", SL.stations, where=1)
  assign("loadest.snames", SL.snames, where=1)
  invisible()
}
