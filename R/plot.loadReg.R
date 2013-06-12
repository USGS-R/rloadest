#'Diagnostic Plot 
#'
#'Plot rating-curve load model diagnostics.
#'
#'Seven  graphs can be produced by this function. If \code{which} is "All," then all plots are produced.
#'The argument \code{which} can also be the name of an explanatory variable so that a partial residual 
#'plot is created for a single variable. Or \code{which} can be any of a sequence of numbers from 1 thorugh 7.
#'Numeric values for \code{which}:
#'\enumerate{
#'\item Fitted with separate factor levels vs. Observed
#'\item Fitted vs. Residual
#'\item S-L plot
#'\item A correlogram if dates are available in the model or in the data set
#'\item Q-normal and Tukey boxplots for each factor level
#'\item Influence plot
#'\item Partial residual plots for each explanatory variable
#'}
#'
#'@param x an object of class "loadReg"---output from \code{loadReg}
#'@param which either "All" or any of a sequence from 1 to 7 indicating which plot, see \bold{Details}.
#'@param set.up set up the graphics page?
#'@param span the span to use for the loess smooth. Set to 0 to suppress.
#'@param \dots further arguments passed to or from other methods.
#'@return The object \code{x} is returned invisibly.
#'@seealso \code{\link{censReg}}
#'@keywords regression hplot
#'@S3method plot loadReg
#'@method plot loadReg
plot.loadReg <- function(x, which='All', set.up=TRUE, span=1.0, ...) {
  ## Coding history:
  ##    2013Jun04 DLLorenz Initial Coding from plot.summary.censReg
  ## 
  ## Identify which plots to do:
  ## 1 Fitted - Actual
  ## 2 Fitted - Residual
  ## 3 S-L
  ## 4 correlogram
  ## 5 Q - normal
  ## 6 Box plot of Observed and Expected
  ## 7 Partial residual plots
  ##
  ## Set up graphics page
  if(set.up)
    setGD("loadReg")
  ## Set up to do all plots
  doPlot <- rep(TRUE, 7L)
  do7 <- FALSE
  if(is.numeric(which)) {
    if(min(which) > 0) # select which to plot
      doPlot[seq(7L)[-which]] <- FALSE
    else # which not to plot
      doPlot[-which] <- FALSE
  }
  else if(is.character(which) && which[1L] != "All") {
    doPlot[1:6] <- FALSE
    xnames <- which
    do7 <- TRUE
  }
  ## Anything else produces all plots
  ## Final plot (7) residuals vs predictors
  ## Extract data needed later
  Fits <- x$lfit$XLCAL %*% x$lfit$PARAML[seq(x$lfit$NPAR)]
  Fits <- as.vector(Fits)
  Resids <- x$lfit$RESID
  Cens <- x$lfit$CENSFLAG
  if(doPlot[7L]) {
    xpred <- x$lfit$XLCAL[, -1L, drop=FALSE]
    xparm <- x$lfit$PARAML[-1L]
    names(xparm) <- c(dimnames(xpred)[[2L]], "Scale")
    if(!do7) # get all explanatory variable names
      xnames <- dimnames(xpred)[[2L]]
    ## Partial residual plots
    if(do7 || length(xnames) > 1L) {
      for(i in xnames) {
        presid <- Resids + xparm[i] * xpred[, i]
        if(length(unique(xpred[, i])) > 2L) { # drop factors, etc.
          xyPlot(xpred[, i], presid,
                 Plot=list(what="points", size=0.05),
                 xtitle=i, ytitle="Partial Residual",
                 margin=c(NA, NA, 1.5, .5))
          if(span > 0)
            addSmooth(xpred[, i], presid, family="sym", span=span)
          refLine(coefficients=c(0, xparm[i]),
                  Plot=list(what="lines", width="standard", type="dashed"))
          ## The p-value of the second order fit on the residuals almost matches
          ## the p-value of adding the second order term to the regression
          nl.p <- summary(lm(presid ~ poly(xpred[,i], 2L),
                             model=TRUE), FALSE)
          nl.p <- nl.p$coefficients[3L, 4L]
          addTitle(Main=paste("Second order polynomial test for linearity: p=",
                              round(nl.p, 4L), sep=""))
        } else {
          Grp <- format(xpred[, i])
          boxPlot(presid, group=Grp, Box=list(type="tukey"),
                  ytitle="Partial Residual", xtitle=i)
        }
      }
    }
  } # end of Partial residual plots
  ## Show a box plot to compare actual observed with estimated
  if(doPlot[6L]) {
    Obs <- exp(x$lfit$YLCAL)
    Observed <- ifelse(x$lfit$CENSFLAG, Obs/2, Obs)
    Estimated <- x$lfit$YPRED
    if(sum(x$lfit$CENSFLAG) > 0L)
      cap <- "Extended box plot (5-95); note: simple substitution used for censored observed values."
    else
      cap <- "Extended box plot (5-95); no censored observed values."
    boxPlot(Observed, Estimated, 
            Box=list(type="extended", show.counts = FALSE, truncated = c(5, 95)),
            ytitle="Load", yaxis.log=TRUE, caption=cap)
  } # end of box plot
  ## Q-normal of standardized residuals (H&H criterion 4)
  if(doPlot[5L]) {
    sresid <- Resids / sqrt(x$lfit$PARAML[x$lfit$NPAR+1])
    qqPlot(sresid, Plot=list(filled=FALSE),
           yaxis.log=FALSE, ylabels=7,
           ytitle="Standardized Residual",
           margin=c(NA, NA, 2.4, NA), mean=0, sd=1)
    ## Add the uncensored values in solid
    ord <- order(sresid)
    ## Cens == 0 for uncensored regardless if left only or multiply censored
    xt <- qnorm(ppoints(sresid, a=0.4))
    yt <- sresid[ord]
    ct <- Cens[ord] == 0
    addXY(xt[ct], yt[ct], Plot=list(what="points", filled=TRUE))
  }
  ## If possible, plot a correlogram--requires finding 1 datelike column in
  ## the data
  if(doPlot[4L])
    corGram(dectime(x$Time), Resids)
  ## Add details of call on regression model to next plots
  Mod <- format(x$lfit$call$formula)
  ## 3rd plot, S-L
  RSE <- rmse(x$lfit) # now the resid std. error
  if(doPlot[3L]) {
    Slres <- residuals(x$lfit, suppress.na.action=TRUE, type="S-L")
    xyPlot(Fits, Slres,
           Plot=list(what="points", size=0.05),
           xtitle="Fitted",
           ytitle=as.expression(substitute(sqrt(abs(YL)),
               list(YL = as.name("Residuals")))),
           margin=c(NA, NA, 2.4, NA))
    if(span > 0)
      addSmooth(Fits, Slres, family="sym", span=span)
    ## 0.82218 is the expected value of the sqrt(abs(x)) for a normal dist.:
    ## integrate(function(x) sqrt(abs(x))*dnorm(x), -Inf, Inf)
    refLine(horizontal=0.82218*sqrt(RSE), Plot=list(what="lines", width="standard", type="dashed"))
  } # end of S-L 
  ## 2nd plot response vs. fit
  if(doPlot[2L]) {
    xyPlot(Fits, Resids,
           Plot=list(what="points", size=0.05),
           xtitle="Fitted",
           ytitle="Residuals")
    if(span > 0)
      addSmooth(Fits, Resids, family="sym", span=span)
    refLine(horizontal=0, Plot=list(what="lines", width="standard", type="dashed"))
  }
  ## First plot is actual vs fitted, with regression details
  if(doPlot[1L]) {
    Act <- Fits + Resids
    Act2 <- Fits + residuals(x$lfit, suppress.na.action=TRUE, type="response")
    xyPlot(Fits, Act2,
           Plot=list(what="points", size=0.07, filled=FALSE),
           xtitle=paste("Fitted:", Mod, sep=" "),
           ytitle="Response")
    if(span > 0)
      addSmooth(Fits, Act, family="sym", span=span)
    refLine(coefficients=c(0,1), Plot=list(what="lines", width="standard", type="dashed"))
    ## Add data details
    if(x$method == "AMLE")
      status <- 1L + x$lfit$CENSFLAG
    else # method is MLE
      status <- x$lfit$survreg$y[, 3L]
    addXY(Fits[status == 1L], Act2[status == 1L],
      Plot=list(what="points", size=0.07, filled=TRUE))
    if(any(status == 0L)) # Greater thans
      for(i in which(status == 0L))
        refLine(vertical=Fits[i], yrange=c(Act2[i], NA))
    if(any(status == 0L)) # Less thans
      for(i in which(status == 2L))
        refLine(vertical=Fits[i], yrange=c(NA, Act2[i]))
    ## Leave interval as open circle
  }
  invisible(x)
}
