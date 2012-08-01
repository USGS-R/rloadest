# dialog support functions for S-LOADEST.
#    Plot model diagnostics
#    for a single site and constituent.
#
# Coding history:
#    2004Jan29 DLLorenz Original version for USGS library
#    2006Jan19 DLLorenz Added resid vs pred.
#    2006Aug04 DLLorenz Added fix to Streamflow plot, needed for concentration model
#    2009Apr01 DLLorenz Fixed Time plot
#    2009Apr01          This version.
#
loadestimPlotModel <- function(data, evaluat, model,
                               title.plot, Censored, sname,
                               normal.plot=T, sl.plot=T, partial.plot=T) {
  Residuals <- evaluat$resid
  if(normal.plot && !is.null(evaluat$scale)) {
    qqxy <- qqnorm(Residuals / evaluat$scale, ylab = "Normalized Residuals")
    points(qqxy$x[Censored == 0], qqxy$y[Censored == 0], col=4, pch=16)
    abline(0,1)
    title(title.plot)
  }
  if(sl.plot) {
    spread <- sqrt(abs(Residuals))
    location <- evaluat$ylcal - Residuals
    plot(location, spread, xlab="Log Fitted Values", ylab="Square Root Absolute Residuals")
    points(location[Censored == 0], spread[Censored == 0], col=4, pch=16)
    lines(lowess(location, spread, f=.99), col=2)
    title(title.plot)
  }
  if(!partial.plot) { # just return if partialplots are not wanted
    invisible()
    return()
  }
  ## plot 1 Residuals vs predicted
  location <- evaluat$ylcal - Residuals
  plot(location, Residuals, xlab="Log Fitted Values", ylab="Residuals")
  points(location[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
  lines(lowess(location, Residuals, f=.99), col=2)
  title(title.plot)
  
  ## plot 2 Residuals vs x (log of flow)
  Streamflow <- data[[sname[4]]]
  plot(Streamflow, Residuals, ylab = "Residuals", log="x")
  points(Streamflow[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
  abline(h = 0, lty = 2)
  ## the smooth should be based on log(X)
  ## must use try to allow the concentration model documented in the T&M report
  prediction <- try(loess.smooth(log(Streamflow), Residuals, span = 1, degree = 1))
  if(class(prediction) != "Error") {
    prediction$x <- exp(prediction$x)
    lines(prediction)
    title(title.plot)
  }
  
  if(is.numeric(model)) {
    ## plots based on what is in the model
    if(is.element(model,(c(3,5,7,8,9)))) { # has linear time term
      Time <- data[[sname[5]]]
      plot(Time, Residuals, ylab = "Residuals", xlab = "Decimal time")
      points(Time[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
      abline(h = 0, lty = 2)
      prediction <- loess.smooth(Time, Residuals, span = 1, degree = 1)
      lines(prediction)
      title(title.plot)
    }
  
    if(is.element(model,(c(4,6,7,8,9)))) { # has sinusoidal terms
      Time <- data[[sname[5]]] %% 1
      plot(Time, Residuals, ylab = "Residuals", xlab = "Proportion of year")
      points(Time[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
      abline(h = 0, lty = 2)
      prediction <- loess.smooth(Time, Residuals, span = 1, degree = 1)
      lines(prediction)
      title(title.plot)
    }
  } # end of option for numeric model
  else {  ## more plots, based on additional variables!
    if(model[3] != "0") { # has linear time term
      Time <- data[[sname[5]]]
      plot(Time, Residuals, ylab = "Residuals", xlab = "Decimal time")
      points(Time[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
      abline(h = 0, lty = 2)
      prediction <- loess.smooth(Time, Residuals, span = 1, degree = 1)
      lines(prediction)
      title(title.plot)
    } # end of DATES plot
    if(model[4] == "sinusoidal") { # has sinusoidal terms
      Time <- data[[sname[5]]] %% 1
      plot(Time, Residuals, ylab = "Residuals", xlab = "Porportion of year")
      points(Time[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
      abline(h = 0, lty = 2)
      prediction <- loess.smooth(Time, Residuals, span = 1, degree = 1)
      lines(prediction)
      title(title.plot)
    } # end of SEASON plots
    
    if(model[4] == "period") { # has period definition
      months.sample <- as.character(months(data[[sname[6]]], abb=F))
      months.target <- unlist(unpaste(model[5],sep=","))
      period <- is.element(months.sample, months.target)
      split.dat <- split(Residuals, period)
      names(split.dat) <- c("Outside", "Inside")
      boxplot(split.dat, ylab = "Residuals", xlab = "Period", style.bxp=bxp.usgs)
      title(title.plot)
    } # end of period plots
    if(model[7] == "TRUE") { # has sinusoidal terms
      Time <- as.double(substring(data[[sname[7]]],1,2))/24 +
        as.double(substring(data[[sname[7]]],3,4))/1440
      plot(Time, Residuals, ylab = "Residuals", xlab = "Proportion of day")
      points(Time[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
      abline(h = 0, lty = 2)
      prediction <- loess.smooth(Time, Residuals, span = 1, degree = 1)
      lines(prediction)
      title(title.plot)
    }
    ## plot by any of the additional explanatory variables
    if(model[6] != "<None>") {
      loadest.expvars <- unlist(unpaste(model[6], sep=","))
      ## Plot them
      for(var in loadest.expvars) {
        Var <- data[[var]]
        plot(Var, Residuals, ylab = "Residuals", xlab = var)
        points(Var[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
        abline(h = 0, lty = 2)
        good.var <- !is.na(Var) # protect against missing variables that are not in the regression
        prediction <- loess.smooth(Var[good.var], Residuals[good.var], span = 1, degree = 1)
        lines(prediction)
        title(title.plot)
      } # end of for var loop
    } # end of if additional variables loop
  } # end of user specified model
  invisible()
}
