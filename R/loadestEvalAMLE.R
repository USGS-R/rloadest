# internal support function for S-LOADEST.
#    Evaluate the specified AMLE model
#
# Coding history:
#    2004Jan29 DLLorenz Original version for USGS library
#    2006Feb16 DLLorenz Changed Error Message.
#    2006Feb16          This version.
#
loadestEvalAMLE <- function(goodrows, model.inp,dist="normal") {
  NPAR <- model.inp$NPAR

  ## change to censReg_AMLE.fit...
  if(dist == "lognormal")
    model.inp$yd[goodrows] <- log(model.inp$yd[goodrows])
  evaluat <- .Fortran("evalaml",
                      NOBSC = as.integer(sum(goodrows)),
                      NPAR = as.integer(NPAR),
                      XLCAL =as.matrix(model.inp$xlcal[goodrows,]),
                      YLCAL = model.inp$ylcal[goodrows],
                      YD = model.inp$yd[goodrows],
                      CENSFLAG = model.inp$censflag[goodrows],
                      PARMLE = double(NPAR + 1),
                      PARAML = double(NPAR + 1),
                      BIAS = double(NPAR + 1),
                      CV = matrix(0., NPAR + 1, NPAR + 1),
                      SBIAS = double(NPAR + 1),
                      SCV = matrix(0., NPAR + 1, NPAR + 1),
                      STDDEV = double(NPAR + 1),
                      PVAL = double(NPAR + 1),
                      COV = matrix(0., NPAR + 1, NPAR + 1),
                      RESID = double(sum(goodrows)),
                      RSQ = double(1),
                      LLR = double(2),
                      SCORR = double(1),
                      LLRAML = double(1),
                      PLEVAML = double(1),
                      DF = integer(1),
                      LogNorm = dist == "lognormal",
                      YPRED = double(as.integer(sum(goodrows))),
                      AIC = double(1),
                      SPPC = double(1),
                      IERR = integer(1) )
  if(evaluat$IERR > 0) 
    stop(paste("Error Code:", evaluat$IERR))
  evaluat <- loadestPrintEval(evaluat, method="AMLE")
  evaluat$dist <- dist
  evaluat$method <- "AMLE"
  return(evaluat)
}
