# GUI dialog functions (menu and callback) for S-LOADEST.
#    Load estimation for a single site and constituent.
#
# Coding history:
#    2004Jan29 DLLorenz Original version for USGS library
#    2004Oct29 DLLorenz modified for loadest.conv.factor
#    2004Nov16 DLLorenz modifed for quadratic time
#    2010Sep24 DLLorenz Bug fix for Date containing the time
#    2010Sep24          This version.
#    2012Aug07 ldecicco Conversion to R

#' Unit conversion function
#'
#' Converts units
#' @param flow.units string descriptive flow unit
#' @param conc.units string descriptive concentration unit
#' @param load.units string descriptive load unit
#' @keywords unit conversions
#' @return conv.factor number conversion factor
#' @export
#' @examples
#' loadest.conv.factor('cubic meter per second','milligrams per liter','pounds')
menuLoadEstimation <- function(data, constit, detect, conc, load, flow, units,
                               Date, Time, build, method, normal.plot, sl.plot,
                               partial.plot, models.predefined, flowtrans,
                               floworder, lineartime, seasonal, period="",
                               addnlterms, diurnal, preddata, exact, total,
                               totalfile, annual, annualfile, season,
                               seasonfile, monthly, monthfile, daily, dailyfile,
                               inst, instfile, savefile) {
  
  ## Error checks
  if(missing(constit)) 
    stop("The constituent term is missing.")
  if(missing(flow)) 
    stop("The flow term is missing.")
  if(missing(Date)) 
    stop("The Date/time term is missing.")
  ## Get the name of the columns for the consituent and remark codes
  if(!missing(constit)) {
    allvars <- names(data)
    # check to see if constit is in allvars (implies use or .rmk suffux)
    varindex <- is.element(constit, allvars)
    if(sum(varindex) == 1) {
      Pvar <- constit
      Rvar <- paste(constit, "[rR][mM][kK]", sep=".")
      Rvar <- sapply(Rvar,grep,text=allvars)
      Rvar <- allvars[Rvar]
    }
    else { # nope, use R, P prefix
      Pvar <- paste("P", constit, sep="")
      Rvar <- paste("R", constit, sep="")
    }
  } # end of processing costituent
  ## convert lineartime to numeric
  lineartime <- switch(lineartime, none = 0, linear = 1, quadratic = 2)
  ## Compute the Load
  Lvar <- paste("Load", constit, sep=".")
  conv.factor <- loadest.conv.factor(units, conc, load)
  data[[Lvar]] <- data[[Pvar]] * data[[flow]] * conv.factor
  ## remove those observations where the Load, DATES or TIMES is NA
  data <- data[!is.na(data[[Lvar]]),]
  data <- data[!is.na(data[[Date]]),]
  ## remove those observations where the Load is 0 (from 0 flow or 0 conc)
  if(min(data[[Lvar]]) <= 0) {
    cat("\n\n****Caution zero load values removed from analysis****\n\n")
    data <- data[data[[Lvar]] <= 0,]
  }
  if(!missing(Time)) 
    data <- data[!is.na(data[[Time]]),]
  else { # make time (Noon if Date is dates, 0 otherwise extract from Date
    if(is.dates(data[[Date]]))
      TIME <- "1200"
    else {
      TIME <- as.character(timeDate(julian=data[[Date]], format='%2H%2M'))
      if(all(TIME == '0000'))
        TIME <- "1200" # change to noon
    }
    Time <- "TIMEofDay.temp"
    data[[Time]] <- TIME
  }
  ## add DECTIME
  data[[Date]] <- floor(data[[Date]]) ## needed to fix for time, just in case
  Year <- month.day.year(data[[Date]])$year
  Days <- as.double(data[[Date]]) - julian(1,1,Year)
  Days <- Days + as.double(substring(data[[Time]],1,2))/24 +
    as.double(substring(data[[Time]],3,4))/1440
  data$DECTIME <- Year + Days/(365 + as.double(leap.year(Year)))
  ## Dvar is the flux of the detection limit.
  Dvar <- paste("Detect", constit, sep=".")
  if(missing(detect)) { # compute based on most recent value
    num.cens <- sum(data[[Rvar]] == "<")
    if(num.cens == 0) { # determine the minimum value actual value should be irrelevant
      d.value <- min(data[[Pvar]])
      d.value <- 10^floor(log10(d.value))
    }
    else {
      d.value <- data[[Pvar]]
      d.current <- d.value[which(data[[Rvar]] == "<")[1]]
      for(i in 1:length(d.value)) { # replace with most recent detection limit
        if(data[[Rvar]][i] == "<")
          d.current <- d.value[i]
        d.value[i] <- d.current
      }
    }
    data[[Dvar]] <- d.value * data[[flow]] * conv.factor
  } # end of missing
  else { # detect not missing
    if(is.na(as.double(detect))) { # it is a column, we hope
      data[[Dvar]] <- data[[detect]] * data[[flow]] * conv.factor
    }
    else { # it is a numeric constant
      d.value <- rep(as.double(detect), nrow(data))
      data[[Dvar]] <- d.value * data[[flow]] * conv.factor
    }
  } # end not missing
  ## Done with the initial data processing (computing loads and other 
  ## necessary data).  Thsi corresponds to creating the data object in
  ## S-LOADEST.
### Now, process the model building information
  temp.ltv <- loadestCheckLtv(data, Pvar, Rvar)
  if(temp.ltv$pct > 80) {
    cat(" Percentage of censored values (", temp.ltv$pct, ") exceeds 80.\n")
    if(temp.ltv$zeros > 0)
      cat("Zero values (nondetected values with unrecorded reporting limit) were found\n")
    return()
  }
  ## save the data if requested
  if(!missing(savefile) && savefile != "")
    assign(savefile, data, where=1)
  else
    savefile <- ""
  if(build == "Predefined") {
    sname <- c(Rvar, Lvar, Dvar, flow, "DECTIME", Date, Time)
    process <- loadestimBuildAuto(data, sname, models.predefined, method,
                                  normal.plot, sl.plot, partial.plot, savefile)
  }
  else { # build == "Custom"
    sname <- c(Rvar, Lvar, Dvar, flow, "DECTIME", Date, Time)
    process <- loadestimCalibrate(data, sname, flowtrans, floworder,
                                  lineartime, seasonal, period,
                                  addnlterms, diurnal, method,
                                  normal.plot, sl.plot, partial.plot, savefile)
  }
  if(!missing(preddata)) {
    if(missing(season)) 
      season <- ""
    if(!any(total, annual != "None", season != "", monthly, daily, inst)) {
      cat(" *** No output loads selected, nothing to predict. ***\n")
      return()
    }
    ## something to do, add the necessary columns
    if(missing(Time)) {
      if(is.dates(preddata[[Date]]))
        TIME <- "1200"
      else {
        TIME <- as.character(timeDate(julian=preddata[[Date]], format='%2H%2M'))
        if(all(TIME == '0000'))
          TIME <- "1200" # change to noon
      }
      Time <- "TIMEofDay.temp"
      preddata[[Time]] <- TIME
    }
    ## add DECTIME
    preddata[[Date]] <- floor(preddata[[Date]])
    Year <- month.day.year(preddata[[Date]])$year
    Days <- as.double(preddata[[Date]]) - julian(1,1,Year)
    Days <- Days + as.double(substring(preddata[[Time]],1,2))/24 +
      as.double(substring(preddata[[Time]],3,4))/1440
    preddata$DECTIME <- Year + Days/(365 + as.double(leap.year(Year)))
    
    if(missing(totalfile)) 
      totalfile <- ""
    if(missing(annualfile)) 
      annualfile <- ""
    if(missing(seasonfile)) 
      seasonfile <- ""
    if(missing(monthfile)) 
      monthfile <- ""
    if(missing(dailyfile)) 
      dailyfile <- ""
    if(missing(instfile)) 
      instfile <- ""
    cat("\n *** Estimate Loads for ", constit, " ***\n", sep='')
    loadestimEstimate(sname, data, preddata, process, exact=exact,
                      total=total, totalfile=totalfile,
                      annual=annual, annualfile=annualfile,
                      season=season, seasonfile=seasonfile,
                      monthly=monthly, monthfile=monthfile,
                      daily=daily, dailyfile=dailyfile,
                      inst=inst, instfile=instfile, conv.factor)
  }
  return()
}

backLoadEstimation <- function(data) {
  activeprop <- cbGetActiveProp(data)
  ## if initial message and we have a selected data.frame:
  if(cbIsInitDialogMessage(data)) {
    data <- cbSetEnableFlag(data, "USGS.loadestselmodel.build", T) # default off
    data <- cbSetEnableFlag(data, "USGS.loadestselmodel.models", T) # predefined
    data <- cbSetEnableFlag(data, "USGS.loadestselmodel.flowtrans", F) # custom
    data <- cbSetEnableFlag(data, "USGS.loadestselmodel.floworder", F) # custom
    data <- cbSetEnableFlag(data, "USGS.loadestselmodel.lineartime", F) # custom
    data <- cbSetEnableFlag(data, "USGS.loadestselmodel.seasonal", F) # custom
    data <- cbSetEnableFlag(data, "USGS.loadestselmodel.period", F) # custom
    data <- cbSetEnableFlag(data, "USGS.loadestselmodel.addnlterms", F) # custom
    data <- cbSetEnableFlag(data, "USGS.loadestselmodel.diurnal", F) # custom
    data <- cbSetEnableFlag(data, "USGS.loadestpredict.total", F) # predict
    data <- cbSetEnableFlag(data, "USGS.loadestpredict.annual", F) # predict
    data <- cbSetEnableFlag(data, "USGS.loadestpredict.season", F) # predict
    data <- cbSetEnableFlag(data, "USGS.loadestpredict.month", F) # predict
    data <- cbSetEnableFlag(data, "USGS.loadestpredict.daily", F) # predict
    data <- cbSetEnableFlag(data, "USGS.loadestpredict.inst", F) # predict
    data <- cbSetCurrValue(data, "USGS.loadestim.preddata", "") # remove default selection
    if(exists(cbGetCurrValue(data, "SPropDataFrameList"))) 
      activeprop <- "SPropDataFrameList"
  } # end of if init message
  if(activeprop == "SPropDataFrameList"){
    ## first reset all variables
    data <- cbSetOptionList(data, "USGS.loadestim.constit", "")
    data <- cbSetOptionList(data, "USGS.loadestim.detect", "")
    data <- cbSetOptionList(data, "USGS.loadestim.flow", "")
    data <- cbSetOptionList(data, "USGS.loadestim.date", "")
    data <- cbSetOptionList(data, "USGS.loadestim.time", "")
    data.name <- cbGetCurrValue(data, "SPropDataFrameList")
    if(exists(data.name)) {
      data.object <- eval(as.name(data.name), sys.parent())
      variables.x <- names(data.object)
      list.x <- paste(variables.x,collapse=",")
      # first see if P, R codes are used.
      Pcodes <- grep("^P", variables.x)
      if(length(Pcodes) > 0) { # some were found, now check on Rcodes
        Rcodes <- grep("^R", variables.x)
        if(length(Rcodes) > 0) { # some were found, now check if they have the same suffixes
          Pvars <- substring(variables.x[Pcodes],2)
          Rvars <- substring(variables.x[Rcodes],2)
          Vars <- intersect(Pvars, Rvars)
          if(length(Vars) > 0) { # they have some suffixes in common
            data <- cbSetOptionList(data, "USGS.loadestim.constit", paste(Vars,collapse=","))
            data <- cbSetCurrValue(data, "USGS.loadestim.constit", Vars[1])
          }
          else { # nothing in common, reset Pcodes
            Pcodes <- numeric(0)
          }
        } # end of Rcodes found
        else
          Pcodes <- numeric(0)
      } # end of length > 0
      if(length(Pcodes) == 0) { # P, R codes not found, look for .rmk
        Rcodes <- grep("\.[rR][mM][kK]$", variables.x)
        if(length(Rcodes) > 0) { # found some, now get the list
          Rvars <- variables.x[Rcodes]
          Vars <- substring(Rvars, 1, nchar(Rvars) - 4)
          data <- cbSetOptionList(data, "USGS.loadestim.constit",  paste(Vars,collapse=","))
          data <- cbSetCurrValue(data, "USGS.loadestim.constit", Vars[1])
        }
        else { # nothing found
          data <- cbSetOptionList(data, "USGS.loadestim.constit", "")
        }
      } # end of length == 0 (looking for constituent variables)
      ## now do detect, flow, date and time if possible
      if(length(Rcodes) == 0)
        return(data)
      Vars <- variables.x[unlist(lapply(data.object, is.numeric))]
      data <- cbSetOptionList(data, "USGS.loadestim.detect",
                              paste(Vars, collapse=","))
      data <- cbSetOptionList(data, "USGS.loadestim.flow",
                              paste(Vars, collapse=","))
      data <- cbSetOptionList(data, "USGS.loadestselmodel.addnlterms",
                              paste(c("<None>", Vars), collapse=","))
      Vars <- variables.x[unlist(lapply(data.object, is.dates)) | (unlist(lapply(data.object, class)) == "timeDate")]
      data <- cbSetOptionList(data, "USGS.loadestim.date", paste(Vars, collapse=","))
      Vars <- {variables.x[-Rcodes]}[unlist(lapply(data.object[,-Rcodes], is.character))]
      data <- cbSetOptionList(data, "USGS.loadestim.time", paste(Vars, collapse=","))
    } # end of if exists
  } # end of active prop data
  if(activeprop == "USGS.loadestselmodel.build") { # the models option changed
    if(cbGetCurrValue(data, "USGS.loadestselmodel.build") == "Predefined") {
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.models", T)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.flowtrans", F)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.floworder", F)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.lineartime", F)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.seasonal", F)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.period", F)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.addnlterms", F)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.diurnal", F)
    }
    else {
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.models", F)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.flowtrans", T)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.floworder", T)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.lineartime", T)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.seasonal", T)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.period", T)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.addnlterms", T)
      data <- cbSetEnableFlag(data, "USGS.loadestselmodel.diurnal", T)
    }
  } # end of activeprop models option
  if(cbGetCurrValue(data, "USGS.loadestselmodel.seasonal") == "period") {
    data <- cbSetEnableFlag(data, "USGS.loadestselmodel.period", T)
    data <- cbSetCurrValue(data, "USGS.loadestselmodel.floworder", "linear")
  }
  else
    data <- cbSetEnableFlag(data, "USGS.loadestselmodel.period", F)
  # now do the last page if the predict data set is selected
  data.name <- cbGetCurrValue(data, "USGS.loadestim.preddata")
  sname.name <- cbGetCurrValue(data, "USGS.loadestim.constit")
  if(data.name != "" && sname.name != "") {
    data <- cbSetEnableFlag(data, "USGS.loadestpredict.total", T)
    data <- cbSetEnableFlag(data, "USGS.loadestpredict.annual", T)
    data <- cbSetEnableFlag(data, "USGS.loadestpredict.season", T)
    data <- cbSetEnableFlag(data, "USGS.loadestpredict.month", T)
    data <- cbSetEnableFlag(data, "USGS.loadestpredict.daily", T)
    data <- cbSetEnableFlag(data, "USGS.loadestpredict.inst", T)
    if(cbGetCurrValue(data, "USGS.loadestpredict.total") == "T") {
      data <- cbSetEnableFlag(data, "USGS.loadestpredict.totalfile", T)
      if(cbGetActiveProp(data) == "USGS.loadestpredict.total" &&
         cbGetCurrValue(data, "USGS.loadestpredict.totalfile") == "")       
        data <- cbSetCurrValue(data, "USGS.loadestpredict.totalfile", paste(data.name, sname.name, "total", sep="."))
    }
    else {
      data <- cbSetCurrValue(data, "USGS.loadestpredict.totalfile", "")
      data <- cbSetEnableFlag(data, "USGS.loadestpredict.totalfile", F)
    }
    if(cbGetCurrValue(data, "USGS.loadestpredict.annual") != "None") {
      data <- cbSetEnableFlag(data, "USGS.loadestpredict.annualfile", T)
      if(cbGetActiveProp(data) == "USGS.loadestpredict.annual" &&
         cbGetCurrValue(data, "USGS.loadestpredict.annualfile") == "")       
        data <- cbSetCurrValue(data, "USGS.loadestpredict.annualfile", paste(data.name, sname.name, "annual", sep="."))
    }
    else {
      data <- cbSetCurrValue(data, "USGS.loadestpredict.annualfile", "")
      data <- cbSetEnableFlag(data, "USGS.loadestpredict.annualfile", F)
    }
    if(cbGetCurrValue(data, "USGS.loadestpredict.season") != "") {
      data <- cbSetEnableFlag(data, "USGS.loadestpredict.seasonfile", T)
      if(cbGetActiveProp(data) == "USGS.loadestpredict.season" &&
         cbGetCurrValue(data, "USGS.loadestpredict.seasonfile") == "")       
        data <- cbSetCurrValue(data, "USGS.loadestpredict.seasonfile", paste(data.name, sname.name, "seasonal", sep="."))
    }
    else {
      data <- cbSetCurrValue(data, "USGS.loadestpredict.seasonfile", "")
      data <- cbSetEnableFlag(data, "USGS.loadestpredict.seasonfile", F)
    }
    if(cbGetCurrValue(data, "USGS.loadestpredict.month") == "T") {
      data <- cbSetEnableFlag(data, "USGS.loadestpredict.monthfile", T)
      if(cbGetActiveProp(data) == "USGS.loadestpredict.month" &&
         cbGetCurrValue(data, "USGS.loadestpredict.monthfile") == "")       
        data <- cbSetCurrValue(data, "USGS.loadestpredict.monthfile", paste(data.name, sname.name, "monthly", sep="."))
    }
    else {
      data <- cbSetCurrValue(data, "USGS.loadestpredict.monthfile", "")
      data <- cbSetEnableFlag(data, "USGS.loadestpredict.monthfile", F)
    }
    if(cbGetCurrValue(data, "USGS.loadestpredict.daily") == "T") {
      data <- cbSetEnableFlag(data, "USGS.loadestpredict.dailyfile", T)
      if(cbGetActiveProp(data) == "USGS.loadestpredict.daily" &&
         cbGetCurrValue(data, "USGS.loadestpredict.dailyfile") == "")       
        data <- cbSetCurrValue(data, "USGS.loadestpredict.dailyfile", paste(data.name, sname.name, "daily", sep="."))
    }
    else {
      data <- cbSetCurrValue(data, "USGS.loadestpredict.dailyfile", "")
      data <- cbSetEnableFlag(data, "USGS.loadestpredict.dailyfile", F)
    }
    if(cbGetCurrValue(data, "USGS.loadestpredict.inst") == "T") {
      data <- cbSetEnableFlag(data, "USGS.loadestpredict.instfile", T)
      if(cbGetActiveProp(data) == "USGS.loadestpredict.inst" &&
         cbGetCurrValue(data, "USGS.loadestpredict.instfile") == "")       
        data <- cbSetCurrValue(data, "USGS.loadestpredict.instfile", paste(data.name, sname.name, "instantaneous", sep="."))
    }
    else {
      data <- cbSetCurrValue(data, "USGS.loadestpredict.instfile", "")
      data <- cbSetEnableFlag(data, "USGS.loadestpredict.instfile", F)
    }
  } # end of if predict data 
  data
}
