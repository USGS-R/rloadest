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
    
    if(evaluat$IERR < 0) cat("\n\n**** Warning excessive censoring, code: ", evaluat$IERR,"\n\n", sep="")
    
    params <- evaluat$PARAML[1:evaluat$NPAR]
    parmsd <- evaluat$STDDEV[1:evaluat$NPAR]
    parmpv <- evaluat$PVAL[1:evaluat$NPAR]
    params <- cbind(params, parmsd, parmpv)
    
    dimnames(params) <- list(NULL, c("Coefficient", "Std. Dev.", "P-value"))
#     dimnames(params) <- list(names.params, c("Coefficient", "Std. Dev.", "P-value"))
    
    print(params)
    cat("\nEstimated residual variance = ", signif(evaluat$PARAML[evaluat$NPAR + 1], 4), "\n", sep="")
    cat("R-squared of the regression =", signif(evaluat$RSQ, 4), "\n")
    cat("Serial correlation of the residuals =", signif(evaluat$SCORR, 4), "\n")
    cat("\n COVariance/correlation of regression coefficients\n")
    names.params <- c(names.params,"Scale")
    
#     dimnames(evaluat$COV) <- list(names.params,names.params)
    
    std <- sqrt(diag(evaluat$COV))
    evaluat$COV[lower.tri(evaluat$COV)] <- evaluat$COV[lower.tri(evaluat$COV)] /
      outer(std,std)[lower.tri(evaluat$COV)]
    print(evaluat$COV)
    cat("\n* NOTE: the following tests are merely informative, the user must evaluate the model more fully\n")
    cat("\nTurnbull-Weiss normality test statistic = ",
        signif(evaluat$LLRAML, 4), ", P-value = ", signif(evaluat$PLEVAML, 4),
        ", DF = ", signif(evaluat$DF, 4), "\n", sep="")
    Crit <- round(loadestGrubbsBeck(length(evaluat$RESID)), 3)
    cat("\nGrubbs & Beck 2-sided outlier test criteria at 5% significance\n")
    cat(-Crit, Crit, "\nRange of normalized residuals\n", sep=" ")
    evaluat$SCALE <- sqrt(evaluat$PARAML[evaluat$NPAR + 1])
    Range <- range(evaluat$RESID) / evaluat$SCALE
    cat(round(Range,3), "\n", sep=" ")
    if(max(abs(Range)) > Crit){
      evaluat$outlier <- TRUE
    } else {
      evaluat$outlier <- FALSE
    }      
    return(evaluat)
  }
  ## report results for MLE
  if(method == "MLE") {
    cat("\n Maximum Likelihood Estimate of the selected model.\n")
    if(evaluat$IERR < 0)
      cat("\n\n**** Warning excessive censoring, code: ", evaluat$IERR,
          "\n\n", sep="")
    params <- evaluat$PARMLE[1:evaluat$NPAR]
    names(params) <- names.params
    print(params)
    cat("\nEstimated residual variance = ",
        signif(evaluat$PARMLE[evaluat$NPAR + 1], 4), "\n", sep="")
    cat("\n* NOTE: the following test is merely informative, the user must evaluate the model more fully\n")
    cat("\nTurnbull-Weiss normality test statistic = ",
        signif(evaluat$LLRAML, 4), ", P-value = ", signif(evaluat$PLEVAML, 4),
        ", DF = ", signif(evaluat$DF, 4), "\n", sep="")
    return(evaluat)
  }
  ## report results for LAD, the last remaining option
  cat("\n Least Absolute Deviation Estimate of the selected model.\n")
  params <- evaluat$parms[1:evaluat$NPAR]
  names(params) <- names.params
  print(params)
  cat("\nSerial correlation of the residuals =",signif( evaluat$SCORR, 4), "\n")
  cat("Bias correction factor = ", signif(evaluat$BIAS, 4), "\n", sep="")
  invisible(evaluat)
}
