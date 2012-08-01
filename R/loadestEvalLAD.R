# internal support function for S-LOADEST.
#    Evaluate the specified LAD model
#
# Coding history:
#    2004Jan29 DLLorenz Original version for USGS library
#    2006Feb16 DLLorenz Changed Error Message.
#    2006Feb16          This version.
#
loadestEvalLAD <- function(goodrows, model.inp) {
  NPAR <- model.inp$NPAR
  NCENS = sum(model.inp$censflag[goodrows])
  if(NCENS == 0) 
    evaluat <- .Fortran("evallad",
                        xlcal =as.matrix(model.inp$xlcal[goodrows,]),
                        ylcal = model.inp$ylcal[goodrows],
                        NOBSC = as.integer(sum(goodrows)),
                        NPAR = as.integer(NPAR),
                        parms = double(NPAR + 1),
                        resid = double(sum(goodrows)),
                        scorr = double(1),
                        bicor = double(1),
                        IERR = integer(1) )
  else
    evaluat <- list(IERR=11)
  if(evaluat$IERR > 0) 
    stop(paste("Error Code:", evaluat$IERR))
  evaluat <- loadestPrintEval(evaluat, method="LAD")
  return(evaluat)
}
