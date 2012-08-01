# dialog support functions for S-LOADEST.
#    function supporting menuCreateLoadest
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Oct29 DLLorenz modified for loadest.conv.factor--update header on Nov 10
#    2005Jul14 DLLorenz Updated date
#    2005Jul14          This version.
#
loadestCreateData <- function(data.object, flow.units) {
  ## the argument requires a data.frame, the same one specified in step 1.
  ## get the data that was created in step 1 and edited in step 2.
  ## extract the necessary data for each station and constituent in
  ## their respective files.
  ## also create the status object and the vector that contains
  ## the possible statuses.
  stations <- get("loadest.stations", where=1)$stations
  snames.data <- get("loadest.snames", where=1)
  ## check for completed step 2
  if(any(snames.data$conc == "") || any(snames.data$load == ""))
    stop("Not all constituents defined.")
  snames <- snames.data$snames
  temp.df <- as.data.frame(cbind(rep(stations,each=length(snames)),rep(snames,length(stations))))
  ## create the by-objects for data frames and status
  loadest.df <- by(temp.df,list(stations=temp.df[[1]], snames=temp.df[[2]]),FUN=function(x) as.data.frame(0))
  loadest.status <- by(temp.df,list(stations=temp.df[[1]], snames=temp.df[[2]]),FUN=function(x) "data OK")
  
  if(exists("loadest.expvars", where=1)) {
    loadest.expvars <- get("loadest.expvars", where=1)
    loadest.expvars <- unlist(unpaste(loadest.expvars, sep = ","))
  }
  else
    loadest.expvars <- ""
  
  ## populate the shell with data
  for(station in stations) {
    icode <- 0
    for(code in snames) {
      pcode <- paste("P", code, sep="")
      rcode <- paste("R", code, sep="")
      icode <- icode + 1
      columns <- c("DATES", "TIMES", "FLOW", rcode, pcode)
      if(loadest.expvars != "")
        columns <- c(columns, loadest.expvars)
      temp.df <- data.object[data.object$STAID==station,columns]
      ## remove those observations where the P-value. DATES or TIMES is NA
      temp.df <- temp.df[!is.na(temp.df[[pcode]]),]
      temp.df <- temp.df[!is.na(temp.df[["DATES"]]),]
      temp.df <- temp.df[!is.na(temp.df[["TIMES"]]),]
      ## add those columns needed for load estimation (DECTIME, Lcode, and Dcode)
      if(dim(temp.df)[1] > 0) {
        Year <- as.double(as.character(years(temp.df$DATES)))
        Days <- as.double(temp.df$DATES) - julian(1,1,Year)
        Days <- Days + as.double(substring(temp.df$TIMES,1,2))/24 +
          as.double(substring(temp.df$TIMES,3,4))/1440
        temp.df$DECTIME <- Year + Days/(365 + as.double(leap.year(Year)))
        ## Lcode is computed in terms of flux, load per day.
        lcode <- paste("L", code, sep="")
        conv.factor <- loadest.conv.factor(flow.units, snames.data$conc[icode],
                                           snames.data$load[icode])
        temp.df[[lcode]] <- temp.df[[pcode]] * temp.df$FLOW * conv.factor
        ## Dcode is the flux of the detection limit.
        dcode <- paste("D", code, sep="")
        if(snames.data$detlim[icode] > 0) # use the default value
          temp.df[[dcode]] <- snames.data$detlim[icode] * temp.df$FLOW * conv.factor
        else { # determine the detection limit, and allow multiple and no DLs
          num.cens <- sum(temp.df[[rcode]] == "<")
          if(num.cens == 0) { # determine the minimum value actual value should be irrelevant
            d.value <- min(temp.df[[pcode]])
            d.value <- 10^floor(log10(d.value))
          }
          else {
            d.value <- temp.df[[pcode]]
            d.current <- d.value[which(temp.df[[rcode]] == "<")[1]]
            for(i in 1:length(d.value)) { # replace with most recent detection limit
              if(temp.df[[rcode]][i] == "<")
                d.current <- d.value[i]
              d.value[i] <- d.current
            }
          }
          temp.df[[dcode]] <- d.value * temp.df$FLOW * conv.factor
        } # done with multiple detection limits
      }
      loadest.df[station, code][[1]] <- temp.df
      if(dim(temp.df)[1] == 0)
        loadest.status[station, code][[1]] <- "no data"
      else {
        if(dim(temp.df)[1] < 20)
          loadest.status[station, code][[1]] <- "insufficient data"
        else {
          if(max(as.double(temp.df$DATES)) - min(as.double(temp.df$DATES)) < 335)
            loadest.status[station, code][[1]] <- "short record"
        }
      } # end of else
    } # end of for code
  } # end of for station
  ## save the data
  assign("loadest.df", loadest.df, where=1)
  assign("loadest.status", loadest.status, where=1)
  assign("loadest.status.list", c("skip", "no data", "insufficient data", "short record", "data OK"), where = 1)
  assign("loadest.flow.units", flow.units, where=1)
  invisible()
}
