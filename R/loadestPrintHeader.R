# internal support function for S-LOADEST.
#    Print preliminary regression info
#
# Coding history:
#    2004Jan29 DLLorenz Original version for USGS library
#    2005Oct07 DLLorenz Added printing of Qadj and Tadj
#    2006Jan19 DLLorenz Added printing of VIFs
#    2006Jan19          This version.
#
loadestPrintHeader <- function(goodrows, model.inp, Dates, Flow, Qadj, Tadj) {
  cat("\nTotal number of observations: ", sum(goodrows),
      ", number of censored obs: ", sum(model.inp$censflag[goodrows]), ".\n", sep="")
  cat("\nCalibration data period of record: ",
      format(min(Dates)),
      " to ", format(max(Dates)), ".\n", sep="")
  ## Tadj has 2 components, the base year and and adjustment to the center
  cat("Central value of time in dectime format: ", round(sum(Tadj),2), ".\n",
      sep="")
  cat("\nCalibration data streamflow summary statistics:\n")
  print(summary(Flow))
  cat("Central value of flow: ", signif(Qadj,4), ".\n",
      sep="")
  Cor <- cor(model.inp$xlcal[,-1])
  if(length(Cor) == 1)
    return() # no need to print this information
  cat("\nCorrelation between explanatory variables\n")
  print(Cor)
  cat("\nVariance inflation factors\n")
  Vif <- diag(solve(Cor))
  names(Vif) <- dimnames(Cor)[[1]]
  Vif <- as.matrix(Vif)
  dimnames(Vif)[[2]] <- "VIF"
  print(Vif)
  return()
}
