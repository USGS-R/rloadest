# dialog support functions for S-LOADEST.
#    Print the summary of a regression analysis
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004May05 DLLorenz Minor bug fix
#    2004May18 DLLorenz Removed LA method
#    2006Apr10 DLLorenz Modified for new warning codes
#    2006Apr10          This version.
#
loadestPrintEval <- function(evaluat, method) {
  ## print the results of the input data and model evaluation.
  censored.vec <- evaluat$censorflag
  names.params <- c("Intercept", dimnames(evaluat$xlcal)[[2]][-1])
  ## report the results for AMLE
  if(method == "AMLE") {
    cat("\n Adjusted Maximum Likelihood Estimate of the selected model.\n")
    if(evaluat$IERR < 0)
      cat("\n\n**** Warning excessive censoring, code: ", evaluat$IERR,
          "\n\n", sep="")
    params <- evaluat$parms[1:evaluat$NPAR]
    parmsd <- evaluat$stddev[1:evaluat$NPAR]
    parmpv <- evaluat$pval[1:evaluat$NPAR]
    params <- cbind(params, parmsd, parmpv)
    dimnames(params) <- list(names.params, c("Coefficient", "Std. Dev.", "P-value"))
    print(params)
    cat("\nEstimated residual variance = ", signif(evaluat$parms[evaluat$NPAR + 1], 4), "\n", sep="")
    cat("R-squared of the regression =", signif(evaluat$rsq, 4), "\n")
    cat("Serial correlation of the residuals =", signif(evaluat$scorr, 4), "\n")
    cat("\n Covariance/correlation of regression coefficients\n")
    names.params <- c(names.params,"Scale")
    dimnames(evaluat$cov) <- list(names.params,names.params)
    std <- sqrt(diag(evaluat$cov))
    evaluat$cov[lower.tri(evaluat$cov)] <- evaluat$cov[lower.tri(evaluat$cov)] /
      outer(std,std)[lower.tri(evaluat$cov)]
    print(evaluat$cov)
    cat("\n* NOTE: the following tests are merely informative, the user must evaluate the model more fully\n")
    cat("\nTurnbull-Weiss normality test statistic = ",
        signif(evaluat$llraml, 4), ", P-value = ", signif(evaluat$plevaml, 4),
        ", DF = ", signif(evaluat$df, 4), "\n", sep="")
    Crit <- round(loadestGrubbsBeck(length(evaluat$resid)), 3)
    cat("\nGrubbs & Beck 2-sided outlier test criteria at 5% significance\n")
    cat(-Crit, Crit, "\nRange of normalized residuals\n", sep=" ")
    evaluat$scale <- sqrt(evaluat$parms[evaluat$NPAR + 1])
    Range <- range(evaluat$resid) / evaluat$scale
    cat(round(Range,3), "\n", sep=" ")
    if(max(abs(Range)) > Crit)
      evaluat$outlier <- T
    else
      evaluat$outlier <- F
    return(evaluat)
  }
  ## report results for MLE
  if(method == "MLE") {
    cat("\n Maximum Likelihood Estimate of the selected model.\n")
    if(evaluat$IERR < 0)
      cat("\n\n**** Warning excessive censoring, code: ", evaluat$IERR,
          "\n\n", sep="")
    params <- evaluat$parms[1:evaluat$NPAR]
    names(params) <- names.params
    print(params)
    cat("\nEstimated residual variance = ",
        signif(evaluat$parms[evaluat$NPAR + 1], 4), "\n", sep="")
    cat("\n* NOTE: the following test is merely informative, the user must evaluate the model more fully\n")
    cat("\nTurnbull-Weiss normality test statistic = ",
        signif(evaluat$llraml, 4), ", P-value = ", signif(evaluat$plevaml, 4),
        ", DF = ", signif(evaluat$df, 4), "\n", sep="")
    return(evaluat)
  }
  ## report results for LAD, the last remaining option
  cat("\n Least Absolute Deviation Estimate of the selected model.\n")
  params <- evaluat$parms[1:evaluat$NPAR]
  names(params) <- names.params
  print(params)
  cat("\nSerial correlation of the residuals =",signif( evaluat$scorr, 4), "\n")
  cat("Bias correction factor = ", signif(evaluat$bicor, 4), "\n", sep="")
  invisible(evaluat)
}
