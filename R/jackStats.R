#' Jackknife Statistics
#'
#' Compute selected jackknife statistics for a rating-curve load-estimation model.
#'
#' @param fit an object of class "loadReg"---output from \code{loadReg}. Can also 
#'be an object of class "censReg."
#' @param which a character string indicating the "load" or
#'"concentration" model for an object of class "loadReg" or "censReg" for
#'an object of class "censReg."
#' @return An object of class "jackStats" containing these components:
#'coef, the table of coefficient estimates, the jackknife bias and standard errors\cr
#'coefficients, the jackknifed coefficients\cr
#'pctcens, the percentage of left-censored values. \cr
#'The PRESS statistic and individual jackknife differences are also returned
#'when the percentage of censoring is 0.
#' @note The \code{jackStats} function can only be used when the analysis is AMLE.
#'
#'Abdi and Williams (2010) describe the jackknife as refering to two related techniques: the first 
#'estimates the parameters, their bias and standard errors and the second evaluates the 
#'predictive performance of the model. The second technique is the PRESS statistic (Helsel
#'and Hirsch, 2002), but can only be used on uncensored data; it is computed by \code{jackStats}
#'when no data are censored. The first technique can be used to assess the coefficients of the
#'regression---the bias should be small and the jackknife standard errors should not be much
#'different from the standard errors reported for the regression. Efron and Tibshirani (1993)
#'suggest that the bias is small if the relative bias (biuas divided by the jackknife standard
#'error) is less than 0.25.
#' @seealso \code{\link{loadReg}}
#' @keywords utilities
#' @references
#' Abdi, H. and Williams, L.J., 2010, Jackknife, in encyclopedia of research design, 
#'Salkind, N.J., editor: Thousand Oaks, Calif., SAGE Publications, 1719 p.
#'
#'Efron, B. and Tibshirani, R.J., 1993, An introduction to the bootstrap: Boca Raton, 
#'Fla., Chapman and Hall/CRC, 436 p.
#'
#' Helsel, D.R. and Hirsch, R.M., 2002, Statistical methods in water resources: 
#'U.S. Geological Survey Techniques of Water-Resources Investigations, book 4, 
#'chap. A3, 522 p.
#'Salkind, 
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'jackStats(app1.lr)
#' @export
jackStats <- function(fit, which="load") {
  ## Compute some stats
  which <- match.arg(which, c("load", "concentration", "censReg"))
  if(which == "load") {
    # Verify AMLE
    if(fit$method != "AMLE") {
      stop("The analysis method must be AMLE")
    }
    # initial stuff
    NPAR <- fit$lfit$NPAR
    NOBS <- fit$lfit$NOBSC
    # Get the repsonse, X, and Yeff
    Y <- as.lcens(exp(fit$lfit$YLCAL), exp(fit$lfit$YD), fit$lfit$CENSFLAG)
    X <- fit$lfit$XLCAL
    Yeff <- fit$lfit$YLCAL
    ## The code below computes an effective value for left-censored values
    ## by computing the expected value from the prediction
    ## provided if the method is ever published. No plans for now
    #Yeff[fit$lfit$CENSFLAG] <- fit$lfit$XLCAL[fit$lfit$CENSFLAG, ,drop=FALSE] %*% 
    #  fit$lfit$PARAML[seq(NPAR)] + fit$lfit$RESID[fit$lfit$CENSFLAG]
    #
    # Other Info
    dist <- "lognormal"
    parms <- fit$lfit$PARAML[seq(fit$lfit$NPAR)]
    parnames <- colnames(fit$lfit$XLCAL)
  } else if(which == "concentration") {
    # Verify AMLE
    if(fit$method != "AMLE") {
      stop("The analysis method must be AMLE")
    }
    # initial stuff
    NPAR <- fit$cfit$NPAR
    NOBS <- fit$cfit$NOBSC
    # Get the repsonse, X, and Yeff
    Y <- as.lcens(exp(fit$cfit$YLCAL), exp(fit$cfit$YD), fit$cfit$CENSFLAG)
    X <- fit$cfit$XLCAL
    Yeff <- fit$cfit$YLCAL
    ## See comment above
    #Yeff[fit$cfit$CENSFLAG] <- fit$cfit$XLCAL[fit$cfit$CENSFLAG, ,drop=FALSE] %*% 
    #  fit$cfit$PARAML[seq(NPAR)] + fit$cfit$RESID[fit$cfit$CENSFLAG]
    #
    # Other Info
    dist <- "lognormal"
    parms <- fit$cfit$PARAML[seq(fit$cfit$NPAR)]
    parnames <- colnames(fit$cfit$XLCAL)
  } else { # must be censReg
    # Verify AMLE
    if(fit$method != "AMLE") {
      stop("The analysis method must be AMLE")
    }
    # initial stuff
    NPAR <- fit$NPAR
    NOBS <- fit$NOBSC
    dist <- fit$dist
    # Get the repsonse, X, and Yeff
	if(dist == "lognormal") {
      Y <- as.lcens(exp(fit$YLCAL), exp(fit$YD), fit$CENSFLAG)
	} else {
	  Y <- as.lcens(fit$YLCAL, fit$YD, fit$CENSFLAG)
	}
    X <- fit$XLCAL
    Yeff <- fit$YLCAL
	## See comment above
    #Yeff[fit$CENSFLAG] <- fit$XLCAL[fit$CENSFLAG, ,drop=FALSE] %*% 
    #  fit$PARAML[seq(NPAR)] + fit$RESID[fit$CENSFLAG]
    #
    # Other Info
    parms <- fit$PARAML[seq(fit$NPAR)]
    parnames <- colnames(fit$XLCAL)
  }
  # do it
  # set up res and coeff storage
  pre <- numeric(NOBS)
  coeff <- matrix(0, nrow=NOBS, ncol=NPAR)
  for(i in seq(NOBS)) {
    tmp <- censReg_AMLE.fit(Y[-i], X[-i,], dist)
    coeff[i,] <- tmp$PARAML[-(NPAR + 1L)]
    pre[i] <- Yeff[i] - coeff[i,, drop=FALSE] %*% t(X[i,,drop=FALSE])
  }
  # Compute the jackknife bias and variance of coeffs
  out <- (NOBS - 1)*(rep(parms, each=NOBS) - coeff)
  bias <- -1/NOBS*colSums(out)
  var <- 1/(NOBS*(NOBS-1)) * (colSums(out^2) - NOBS * bias^2)
  coef <- cbind(est=parms, bias=bias, stderr=sqrt(var))
  rownames(coef) <- parnames
  retval <- list(coef=coef, coefficients=coeff,
                 pctcens=pctCens(Y))
  # Include press if no censoring
  if(retval$pctcens == 0) {
    retval$press <- sum(pre^2)
    retval$pre <- pre
  }
  class(retval) <- "jackStats"
  return(retval)
}
