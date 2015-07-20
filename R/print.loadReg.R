#' Print Results
#'
#' Print the results of an load rating-curve regression.
#'
#' @param x an object of class "loadReg"---output from \code{loadReg}.
#' @param digits the number of significant digits to print.
#' @param brief print the brief output? See \bold{Note}.
#' @param load.only print only the load model and not concentration model results.
#' @param \dots further arguments passed to or from other methods.
#' @return The object \code{x} is returned invisibly.
#' @note The printed output replicates the output described in Runkel (2004) and
#'includes a short section summarizing the data, the load model and coefficients,
#'regression statistics, and comparison of observed and estimated loads. If
#'\code{load.only} is set to \code{FALSE}, then similar output is generated for the
#'concetration model. If \code{brief} is \code{FALSE}, then additional descriptions
#'of selected sections of the output are produced.
#'
#'If the estimation method is "MLE," then the estimated loads used in the comparison
#'to observed loads are approximate becuase they are estimated using MLE, rather than
#'AMLE, wihch is used for \code{predLoad} and \code{predConc}. The bias is very small
#'when the residual varaince is less than 0.5, but can be large when the residual
#'variance is greater than 1.
#'
#' @seealso \code{\link{loadReg}}
#' @keywords utilities
#' @references
#'Runkel, R.L., Crawford, C.G., and Cohn, T.A., 2004, Load estimator (LOADEST): 
#'A FORTRAN program for estimating constituent loads in streams and rivers: 
#'U.S. Geological Survey Techniques and Methods book 4, chap. A5, 69 p.
#' @export
#' @method print loadReg
print.loadReg <- function(x, digits=4, brief=TRUE, load.only=brief, ...) {
  ##
  ## Explanations of model terms
  Explan <- c(lnQ="ln(Q) - center of ln(Q)",
             lnQ2="ln(Q) - center of ln(Q))^2",
             DECTIME="decimal time - center of decimal time",
             DECTIME2="(decimal time - center of decimal time)^2",
             sin.DECTIME="sine(2 * pi * decimal time)",
             cos.DECTIME="cosine(2 * pi * decimal time)")
  ## Print the results of the input data and model evaluation.
  ## to nearly match the output from LOADEST if brief is FALSE
  load.only
  ## Catch errors/warnings
  if(x$lfit$IERR == -201L) {
    warning("Excessive censoring, greater than 80%")
  } else if(x$lfit$IERR == -202L) {
    warning("Excessive censoring, fewer than 3 uncensored values for each parameter")
  } else if(x$lfit$IERR == 1L) {
    stop("Too many parameters")
  } else if(x$lfit$IERR == 2L) {
    stop("Too many observations")
  } else if(x$lfit$IERR == 201L) {
    stop("Excessive censoring, greater than 90%")
  } else if(x$lfit$IERR == 202L) {
    stop("Excessive censoring, fewer than 1.5 uncensored values for each parameter")
  } else if(x$lfit$IERR == 203L) {
    stop("Variance of uncensored values is 0")
  } else if(x$lfit$IERR > 0L) {
    stop("\nFatal error in censReg, error code: ", x$IERR, "\n")
  }
  if(brief)
    cat("*** Load Estimation ***\n\n")
  else {
    cat("                      LOADEST\n",
        "       A Program to Estimate Constituent Loads\n",
        "U.S. Geological Survey, Version for R 0.1 (June, 2013)\n",
        "------------------------------------------------------\n\n",
        sep="")
  }
  cat("Station: ", x$station, "\n", sep="")
  cat("Constituent: ", x$constituent, "\n\n", sep="")
  ## Load output
  if(!brief)
    cat("----------------------------------------------------------------------\n",
        "    Constituent Output File Part Ia: Calibration (Load Regression)\n",
        "----------------------------------------------------------------------\n\n",
        sep="")
  # CENSFLAG is logical for AMLE, integer for MLE, this protects
  Ncen <- sum(x$lfit$CENSFLAG != 0)
  cat("           Number of Observations: ", x$lfit$NOBSC, "\n",
      "Number of Uncensored Observations: ",
      x$lfit$NOBSC - Ncen, "\n",
      "           Center of Decimal Time: ", 
      round(sum(x$Tadj), digits), "\n",
      "                  Center of ln(Q): ",
      round(log(x$Qadj), digits), "\n",
      "                 Period of record: ",
      as.character(x$PoR[1L]), " to ", as.character(x$PoR[2L]), 
      "\n\n", sep="")
  if(!brief || nrow(x$model.eval) > 1) { # Capture best model selection
    cat("Model Evaluation Criteria Based on ", x$method, " Results\n",
        "-----------------------------------------------\n\n", sep="")
    print(x$model.eval, digits=digits)
    cat("Model # ", x$model.no, " selected\n\n", sep="")
  }
  cat("Selected Load Model:\n--------------------\n\n")
  dput(x$lfit$call$formula)
  if(!brief) {
    cat("\nwhere:\n   ", deparse(x$lfit$call$formula[[2L]]),
        " is the constituent load in log(", x$load.units, "/d)\n",
        sep="")
    if(x$model.no != 99) {
      cat("and model ", x$model.no, " has these variables:\n", sep="")
      for(i in x$xvars)
        cat("   ", i, " is ", Explan[i], "\n", sep="")
    }
  }
  cat("\nModel coefficients:\n")
  ctab <- coef(x$lfit, summary=TRUE)
  ## Round last column of table to eliminate scientific notation
  ctab[,4L] <- round(ctab[, 4L], 4L)
  print(ctab, digits=digits)
  ## Summary stats
  if(Ncen == 0L) {
    RSQ <- signif(x$lfit$RSQ, digits)
    RSQtxt <- "R-squared: "
  } else {
    RSQ <- 100*signif(1 - exp((x$lfit$LLR1 - x$lfit$LLR)*2/x$lfit$NOBSC), digits)
    RSQtxt <- "Generalized R-squared: "
  }
  G2 <- signif(2*(x$lfit$LLR - x$lfit$LLR1), digits)
  pval <- 1 - pchisq(G2, x$lfit$NPAR - 1)
  ## Format the attained p-value, last one prevents scientific printing
  if(pval < 0.0001)
    pval <- "<0.0001"
  else
    pval <- format(round(pval, 4), scientific=5)
  ## Compute the PPCC
  Res <- residuals(x$lfit, type="working", suppress.na.action=TRUE)
  if(x$method == "AMLE") {
    ppcc <- censPPCC.test(as.lcens(Res, censor.codes=x$lfit$CENSFLAG))
    cat("\n", x$method, " Regression Statistics\n",
        "Residual variance: ", 
        signif(x$lfit$PARAML[x$lfit$NPAR + 1L], digits), "\n",
        RSQtxt, RSQ, " percent\n",
        "G-squared: ", G2, " on ", x$lfit$NPAR - 1L, 
        " degrees of freedom\n",
        "P-value: ", pval, "\n",
        "Prob. Plot Corr. Coeff. (PPCC):\n", 
        "  r = ", round(ppcc$statistic, digits), "\n",
        "  p-value = ", round(ppcc$p.value, digits), "\n",
        "Serial Correlation of Residuals: ", 
        round(x$lfit$SCORR, digits), "\n\n",
        sep="")
  } else { # ppcc test cannot be computed for interval or right-censored 
    cat("\n", x$method, " Regression Statistics\n",
        "Residual variance: ", 
        signif(x$lfit$PARAML[x$lfit$NPAR + 1L], digits), "\n",
        RSQtxt, RSQ, " percent\n",
        "G-squared: ", G2, " on ", x$lfit$NPAR - 1L, 
        " degrees of freedom\n",
        "P-value: ", pval, "\n",
        "Prob. Plot Corr. Coeff. (PPCC) cannot be computed for method MLE\n", 
        "Serial Correlation of Residuals not computed for method MLE\n", 
        sep="")
  }
  if(!brief && x$lfit$NPAR > 2L) { # Print the correlation matrixes
    cat("Correlation Between Explanatory Variables\n",
        "-----------------------------------------\n\n", sep="")
    cormat <- format(round(cor(x$lfit$XLCAL[,-1]), digits))
    cormat[upper.tri(cormat, diag=TRUE)] <- ""
    print(cormat[seq(2L,x$lfit$NPAR -1L), seq(1L, x$lfit$NPAR - 2L)],
          quote=FALSE)
    cat("\nCorrelation Between Variable Coefficients\n",
        "-----------------------------------------\n\n", sep="")
    cormat <- format(round(cov2cor(x$lfit$COV), digits))
    dimnames(cormat) <- list(c("Icept", x$xvars, "scale"),
                             c("Icept", x$xvars, "scale"))
    cormat[upper.tri(cormat, diag=TRUE)] <- ""
    print(cormat[seq(3L,x$lfit$NPAR), seq(2L, x$lfit$NPAR - 1L)],
          quote=FALSE)
    cat("\n")
  }
  if(x$lfit$NPAR > 2L) { # Print the VIFs
    cat("Variance Inflation Factors:\n")
    vifs <- as.matrix(vif(x$lfit))
    colnames(vifs) <- "VIF"
    print(vifs, digits=digits)
  }
  if(x$method == "AMLE") {
    cat("\nComparison of Observed and Estimated Loads\n",
        "------------------------------------------\n", sep="")
  } else { # method is MLE
    cat("\nComparison of Observed and Approximate Estimated Loads\n",
        "------------------------------------------------------\n", sep="")
  }
  if(!brief)
    cat("  The summary statistics and bias diagnostics presented below are based\n",
        "on a comparison of observed and estimated loads for all dates/times within\n",
        "the calibration data set. Although this comparison does not directly\n",
        "address errors in load estimation for unsampled dates/times, large\n",
        "discrepancies between observed and estimated loads are indicative of a\n",
        "poor model fit. Additional details and warnings are provided below.\n\n",
        "Note: The comparison that follows uses a concentration equal to 1/2 the\n",
        "detection limit when an observation is censored. The summary stats and\n",
        "bias diagnostics are therefore slightly inaccurate for censored datasets.\n\n",
        sep="")
  ## Compute the stats
  Stats <- loadStats(x)
  cat("      Summary Stats: Loads in ", x$load.units, "/d\n",
      "---------------------------------------------\n", sep="")
  print(Stats$outSum)
  ## Print the bias diagnostics
  cat("\nBias Diagnostics\n",
      "----------------\n",
      " Bp: ", signif(Stats$outBias[1L], digits), " percent\n",
      "PLR: ", signif(Stats$outBias[2L], digits), "\n",
      "  E: ", signif(Stats$outBias[3L], digits), "\n\n", sep="")
  PLR <- Stats$outBias[2L]
  E <- Stats$outBias[3L]
  if(!brief) {
    cat("where:\n",
        "   Bp Load Bias in Percent\n",
        "      Positive (negative) values indicate over (under) estimation.\n",
        "      ***The model should not be used when the + or - bias exceeds 25%***\n",
        "  PLR Partial Load Ratio\n",
        "      Sum of estimated loads divided by sum of observed loads.\n",
        "      Values greater than 1 indicate over estimation.\n",
        "      Values less than 1 indicate under estimation.\n",
        "    E Nash Sutcliffe Efficiency Index\n",
        "      E ranges from -infinity to 1.0\n",
        "      E = 1; a perfect fit to observed data.\n",
        "      E = 0; model estimates are as accurate as the mean of observed data.\n",
        "      E < 0; the observed mean is a better estimate than the model estimates.\n\n",
        sep="")
    if(abs(PLR - 1) > .25) # Warn user
      cat(".-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--.\n",
          "|                W A R N I N G ! !             |\n",
          "|                    ______                    |\n",
          "|                 .-\"      \"-.                 |\n",
          "|                /            \\                |\n",
          "|    _          |              |          _    |\n",
          "|   ( \\         |,  .-.  .-.  ,|         / )   |\n",
          "|    > \"=._     | )(__/  \\__)( |     _.=\" <    |\n",
          "|   (_/\"=._\"=._ |/     /\\     \\| _.=\"_.=\"\\_)   |\n",
          "|          \"=._\"(_     ^^     _)\"_.=\"          |\n",
          "|              \"=\\__|IIIIII|__/=\"              |\n",
          "|             _.=\"| \\IIIIII/ |\"=._             |\n",
          "|   _     _.=\"_.=\"\\          /\"=._\"=._     _   |\n",
          "|  ( \\_.=\"_.=\"     '--------'     \"=._\"=._/ )  |\n",
          "|   > _.=\"                            \"=._ <   |\n",
          "|  (_/     jgs                            \\_)  |\n",
          "|                W A R N I N G ! !             |\n",
          "|                                              |\n",
          ".=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-.\n\n",
          "IMPORTANT WARNING:\n\n",
          "Load Bias (Bp) Exceeds + or - 25%\n",
          "THE CALIBRATED MODEL SHOULD NOT BE USED FOR LOAD ESTIMATION\n\n",
          sep="")
    if(E < 0) # Again, warn
      cat(".-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--.\n",
          "|                W A R N I N G ! !             |\n",
          "|                    ______                    |\n",
          "|                 .-\"      \"-.                 |\n",
          "|                /            \\                |\n",
          "|    _          |              |          _    |\n",
          "|   ( \\         |,  .-.  .-.  ,|         / )   |\n",
          "|    > \"=._     | )(__/  \\__)( |     _.=\" <    |\n",
          "|   (_/\"=._\"=._ |/     /\\     \\| _.=\"_.=\"\\_)   |\n",
          "|          \"=._\"(_     ^^     _)\"_.=\"          |\n",
          "|              \"=\\__|IIIIII|__/=\"              |\n",
          "|             _.=\"| \\IIIIII/ |\"=._             |\n",
          "|   _     _.=\"_.=\"\\          /\"=._\"=._     _   |\n",
          "|  ( \\_.=\"_.=\"     '--------'     \"=._\"=._/ )  |\n",
          "|   > _.=\"                            \"=._ <   |\n",
          "|  (_/     jgs                            \\_)  |\n",
          "|                W A R N I N G ! !             |\n",
          "|                                              |\n",
          ".=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-.\n\n",
          "IMPORTANT WARNING:\n\n",
          "Nash Sutcliffe Efficiency Index (E) is less than zero.\n",
          "The observed mean is a better estimate than the model estimates.\n",
          "THE CALIBRATED MODEL SHOULD NOT BE USED FOR LOAD ESTIMATION\n\n",
          sep="")
    cat("NOTE: Additional information on model calibration is included in the\n",
        "      residual diagnostic plots. users should conduct a thorough\n",
        "      residuals analysis. Example residual plots are shown in figures\n",
        "      7, 8, 9, and 17 of the LOADEST documentation (Runkel et al., 2004).\n\n",
        sep="")
  }
  ## Concentration output
  if(!load.only && !is.null(x$cfit)) {
    if(!brief)
      cat("--------------------------------------------------------------------------\n",
          "    Constituent Output File Part Ia: Calibration (Concentration Regression)\n",
          "--------------------------------------------------------------------------\n\n",
          sep="")
    Ncen <- sum(x$cfit$CENSFLAG)
    if(!brief) { # Capture best model selection
      cat("Model # ", x$model.no, " selected\n\n", sep="")
    }
    cat("Selected Concentration Model:\n-----------------------------\n\n")
    dput(x$cfit$call$formula)
    if(!brief) {
      cat("\nwhere:\n   ", deparse(x$cfit$call$formula[[2L]]),
          " is the constituent concentration in log(", x$conc.units, ")\n",
          sep="")
      if(x$model.no != 99) {
        cat("and model ", x$model.no, " has these variables:\n", sep="")
        for(i in x$xvars)
          cat("   ", i, " is ", Explan[i], "\n", sep="")
      }
    }
    cat("\nModel coefficients:\n")
    ctab <- coef(x$cfit, summary=TRUE)
    ## Round last column of table to eliminate scientific notation
    ctab[,4L] <- round(ctab[, 4L], 4L)
    print(ctab, digits=digits)
    ## Summary stats
    if(Ncen == 0L) {
      RSQ <- signif(x$cfit$RSQ, digits)
      RSQtxt <- "R-squared: "
    } else {
      RSQ <- 100*signif(1 - exp(x$cfit$LLR1 - x$cfit$LLR)^(2/x$cfit$NOBSC), digits)
      RSQtxt <- "Generalized R-squared: "
    }
    G2 <- signif(2*(x$cfit$LLR - x$cfit$LLR1), digits)
    pval <- 1 - pchisq(G2, x$cfit$NPAR - 1)
    ## Format the attained p-value, last one prevents scientific printing
    if(pval < 0.0001)
      pval <- "<0.0001"
    else
      pval <- format(round(pval, 4), scientific=5)
    ## Compute the PPCC
    Res <- residuals(x$cfit, type="response")
    if(x$method == "AMLE") {
      ppcc <- censPPCC.test(as.lcens(Res, censor.codes=x$cfit$CENSFLAG))
      cat("\n", x$method, " Regression Statistics\n",
          "Residual variance: ", 
          signif(x$cfit$PARAML[x$cfit$NPAR + 1L], digits), "\n",
          RSQtxt, RSQ, " percent\n",
          "G-squared: ", G2, " on ", x$cfit$NPAR - 1L, 
          " degrees of freedom\n",
          "P-value: ", pval, "\n",
          "Prob. Plot Corr. Coeff. (PPCC):\n", 
          "  r = ", round(ppcc$statistic, digits), "\n",
          "  p-value = ", round(ppcc$p.value, digits), "\n",
          "Serial Correlation of Residuals: ", 
          round(x$cfit$SCORR, digits), "\n\n",
          sep="")
    } else {
      cat("\n", x$method, " Regression Statistics\n",
          "Residual variance: ", 
          signif(x$cfit$PARAML[x$cfit$NPAR + 1L], digits), "\n",
          RSQtxt, RSQ, " percent\n",
          "G-squared: ", G2, " on ", x$cfit$NPAR - 1L, 
          " degrees of freedom\n",
          "P-value: ", pval, "\n",
          "Prob. Plot Corr. Coeff. (PPCC) cannot be computed for method MLE\n", 
          "Serial Correlation of Residuals not be computed for method MLE\n", 
          sep="")
    }
    if(x$method == "AMLE") {
      cat("\nComparison of Observed and Estimated Concentrations\n",
          "---------------------------------------------------\n", sep="")
    } else {
      cat("\nComparison of Observed and Approximate Estimated Concentrations\n",
          "---------------------------------------------------------------\n", sep="")
    }
    if(!brief)
      cat("  The summary statistics and bias diagnostics presented below are based\n",
          "on a comparison of observed and estimated concentrations for all dates/times\n",
          "within the calibration data set. Although this comparison does not directly\n",
          "address errors in concentration estimation for unsampled dates/times, large\n",
          "discrepancies between observed and estimated concentrations are indicative of\n",
          "a poor model fit. Additional details and warnings are provided below.\n\n",
          "Note: The comparison that follows uses a concentration equal to 1/2 the\n",
          "detection limit when an observation is censored. The summary stats and\n",
          "bias diagnostics are therefore slightly inaccurate for censored datasets.\n\n",
          sep="")
    ## Compute the stats
    Stats <- loadStats(x, "concentration")
    cat("      Summary Stats: Concentrations in ", x$conc.units, "\n",
        "----------------------------------------------\n", sep="")
    print(Stats$outSum)
    ## Now the bias diagnostics
    cat("\nBias Diagnostics\n",
        "----------------\n",
        " Bp: ", signif(Stats$outBias[1L], digits), " percent\n",
        "PCR: ", signif(Stats$outBias[2L], digits), "\n",
        "  E: ", signif(Stats$outBias[3L], digits), "\n\n", sep="")
    PCR <- Stats$outBias[2L]
    E <- Stats$outBias[3L]
    if(!brief) {
      cat("where:\n",
          "   Bp Concentration Bias in Percent\n",
          "      Positive (negative) values indicate over (under) estimation.\n",
          "      ***The model should not be used when the + or - bias exceeds 25%***\n",
          "  PCR Partial Concentration Ratio\n",
          "      Sum of estimated concentrations divided by sum of observed concentrations.\n",
          "      Values greater than 1 indicate over estimation.\n",
          "      Values less than 1 indicate under estimation.\n",
          "    E Nash Sutcliffe Efficiency Index\n",
          "      E ranges from -infinity to 1.0\n",
          "      E = 1; a perfect fit to observed data.\n",
          "      E = 0; model estimates are as accurate as the mean of observed data.\n",
          "      E < 0; the observed mean is a better estimate than the model estimates.\n\n",
          sep="")
      if(abs(PCR - 1) > .25) # Warn user
        cat(".-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--.\n",
            "|                W A R N I N G ! !             |\n",
            "|                    ______                    |\n",
            "|                 .-\"      \"-.                 |\n",
            "|                /            \\                |\n",
            "|    _          |              |          _    |\n",
            "|   ( \\         |,  .-.  .-.  ,|         / )   |\n",
            "|    > \"=._     | )(__/  \\__)( |     _.=\" <    |\n",
            "|   (_/\"=._\"=._ |/     /\\     \\| _.=\"_.=\"\\_)   |\n",
            "|          \"=._\"(_     ^^     _)\"_.=\"          |\n",
            "|              \"=\\__|IIIIII|__/=\"              |\n",
            "|             _.=\"| \\IIIIII/ |\"=._             |\n",
            "|   _     _.=\"_.=\"\\          /\"=._\"=._     _   |\n",
            "|  ( \\_.=\"_.=\"     '--------'     \"=._\"=._/ )  |\n",
            "|   > _.=\"                            \"=._ <   |\n",
            "|  (_/     jgs                            \\_)  |\n",
            "|                W A R N I N G ! !             |\n",
            "|                                              |\n",
            ".=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-.\n\n",
            "IMPORTANT WARNING:\n\n",
            "Concentration Bias (Bp) Exceeds + or - 25%\n",
            "THE CALIBRATED MODEL SHOULD NOT BE USED FOR CONCENTRATION ESTIMATION\n\n",
            sep="")
      if(E < 0) # Again, warn
        cat(".-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=--.\n",
            "|                W A R N I N G ! !             |\n",
            "|                    ______                    |\n",
            "|                 .-\"      \"-.                 |\n",
            "|                /            \\                |\n",
            "|    _          |              |          _    |\n",
            "|   ( \\         |,  .-.  .-.  ,|         / )   |\n",
            "|    > \"=._     | )(__/  \\__)( |     _.=\" <    |\n",
            "|   (_/\"=._\"=._ |/     /\\     \\| _.=\"_.=\"\\_)   |\n",
            "|          \"=._\"(_     ^^     _)\"_.=\"          |\n",
            "|              \"=\\__|IIIIII|__/=\"              |\n",
            "|             _.=\"| \\IIIIII/ |\"=._             |\n",
            "|   _     _.=\"_.=\"\\          /\"=._\"=._     _   |\n",
            "|  ( \\_.=\"_.=\"     '--------'     \"=._\"=._/ )  |\n",
            "|   > _.=\"                            \"=._ <   |\n",
            "|  (_/     jgs                            \\_)  |\n",
            "|                W A R N I N G ! !             |\n",
            "|                                              |\n",
            ".=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-.\n\n",
            "IMPORTANT WARNING:\n\n",
            "Nash Sutcliffe Efficiency Index (E) is less than zero.\n",
            "The observed mean is a better estimate than the model estimates.\n",
            "THE CALIBRATED MODEL SHOULD NOT BE USED FOR CONCENTRATION ESTIMATION\n\n",
            sep="")
      cat("NOTE: Additional information on model calibration is included in the\n",
          "      residual diagnostic plots. users should conduct a thorough\n",
          "      residuals analysis. Example residual plots are shown in figures\n",
          "      7, 8, 9, and 17 of the LOADEST documentation (Runkel et al., 2004).\n\n",
          sep="")
    }
  } # End of not load.only section
  invisible(x)
}
