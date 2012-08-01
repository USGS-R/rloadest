# internal support function for S-LOADEST.
#    Evaluate the specified AMLE model
#
# Coding history:
#    2004Jan29 DLLorenz Original version for USGS library
#    2006Feb16 DLLorenz Changed Error Message.
#    2006Feb16          This version.
#
loadestEvalAMLE <- function(goodrows, model.inp) {
  NPAR <- model.inp$NPAR
  evaluat <- .Fortran("evalaml",
                      xlcal =as.matrix(model.inp$xlcal[goodrows,]),
                      ylcal = model.inp$ylcal[goodrows],
                      yd = model.inp$yd[goodrows],
                      censflag = model.inp$censflag[goodrows],
                      NOBSC = as.integer(sum(goodrows)),
                      NPAR = as.integer(NPAR),
                      parms = double(NPAR + 1),
                      stddev = double(NPAR + 1),
                      pval = double(NPAR + 1),
                      cov = matrix(0., NPAR + 1, NPAR + 1),
                      resid = double(sum(goodrows)),
                      rsq = double(1),
                      llr = double(2),
                      pllr = double(1),
                      scorr = double(1),
                      llraml = double(1),
                      plevaml = double(1),
                      df = integer(1),
                      IERR = integer(1) )
  if(evaluat$IERR > 0) 
    stop(paste("Error Code:", evaluat$IERR))
  evaluat <- loadestPrintEval(evaluat, method="AMLE")
  return(evaluat)
}
