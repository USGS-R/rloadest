# dialog support functions for S-LOADEST.
#    Plot model diagnostics
#
# Coding history:
#    2004Feb13 DLLorenz Original version for USGS library
#    2006Jan19 DLLorenz Added resid vs pred. and SL plot
#    2010Sep24 DLLorenz Force time and season if possible
#    2010Sep24          This version.
#
loadestPlotModel <- function(data, evaluat, model, title.plot, Censored)
{
  Residuals <- evaluat$resid
  ## plot 1, Normal QQplot of normalized Residuals if possible
  if(!is.null(evaluat$scale)) {
    qqxy <- qqnorm(Residuals / evaluat$scale, ylab = "Normalized Residuals")
    points(qqxy$x[Censored == 0], qqxy$y[Censored == 0], col=4, pch=16)
    abline(0,1)
    title(title.plot)
  }
  
  ## plot 2 SL plot
  spread <- sqrt(abs(Residuals))
  location <- evaluat$ylcal - Residuals
  plot(location, spread, xlab="Log Fitted Values", ylab="Square Root Absolute Residuals")
  points(location[Censored == 0], spread[Censored == 0], col=4, pch=16)
  lines(lowess(location, spread, f=.99), col=2)
  title(title.plot)

  ## plot 3 Residuals vs predicted
  plot(location, Residuals, xlab="Log Fitted Values", ylab="Residuals")
  points(location[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
  lines(lowess(location, Residuals, f=.99), col=2)
  title(title.plot)

  ## plot 4 Residuals vs x (log of flow)
  Streamflow <- data$FLOW
  plot(Streamflow, Residuals, ylab = "Residuals", log="x")
  points(Streamflow[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
  abline(h = 0, lty = 2)
  ## the smooth should be based on log(X)
  prediction <- loess.smooth(log(Streamflow), Residuals, span = 1, degree = 1)
  prediction$x <- exp(prediction$x)
  lines(prediction)
  title(title.plot)

  ## plots based on time and season
  if(!is.null(data$DECTIME)) { # has linear time term
    ## linear
    Time <- data$DECTIME
    plot(Time, Residuals, ylab = "Residuals", xlab = "Decimal time")
    points(Time[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
    abline(h = 0, lty = 2)
    prediction <- loess.smooth(Time, Residuals, span = 1, degree = 1)
    lines(prediction)
    title(title.plot)
    ## seasonal
    Time <- data$DECTIME %% 1
    plot(Time, Residuals, ylab = "Residuals", xlab = "Proportion of year")
    points(Time[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
    abline(h = 0, lty = 2)
    prediction <- loess.smooth(Time, Residuals, span = 1, degree = 1)
    lines(prediction)
    title(title.plot)
  }
  ## End of regular variables
  if(!is.numeric(model))  {  ## more plots, based on additional variables!
    if(any(is.element(month.name,model))) { # has period definition
      months.sample <- as.character(months(data$DATES, abb=F))
      months.target <- month.name[is.element(month.name,model)]
      period <- is.element(months.sample, months.target)
      split.dat <- split(Residuals, period)
      names(split.dat) <- c("Outside", "Inside")
      boxplot(split.dat, ylab = "Residuals", xlab = "Period", style.bxp=bxp.usgs)
      title(title.plot)
    } # end of period plots
    if(is.element("TIMES", model)) { # has sinusoidal terms
      Time <- as.double(substring(data$TIMES,1,2))/24 +
        as.double(substring(data$TIMES,3,4))/1440
      plot(Time, Residuals, ylab = "Residuals", xlab = "Proportion of day")
      points(Time[Censored == 0], Residuals[Censored == 0], col=4, pch=16)
      abline(h = 0, lty = 2)
      prediction <- loess.smooth(Time, Residuals, span = 1, degree = 1)
      lines(prediction)
      title(title.plot)
    }
    ## plot by any of the additional explanatory variables
    if(exists("loadest.expvars", where=1)) {
      loadest.expvars <- get("loadest.expvars", where=1)
      loadest.expvars <- unlist(unpaste(loadest.expvars, sep=","))
      ## for now, plot everything!
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
