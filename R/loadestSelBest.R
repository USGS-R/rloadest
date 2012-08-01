# internal support function for S-LOADEST.
#    Select the "best" predefined model
#
# Coding history:
#    2004Jan29 DLLorenz Original version for USGS library
#    2006Feb16 DLLorenz Changed Error Message.
#    2006Feb16          This version.
#
loadestSelBest <- function(loadest.temp, sname, Qadj, Tadj) {
  calibr <- list(aic=numeric(9), sppc=numeric(9))
  for(j in 1:9) {
    model.inp <- loadestSetXLDat(loadest.temp,sname,j,Qadj,Tadj)
    NPAR <- model.inp$NPAR
    model.out <- .Fortran("calibr",
                          xlcal = as.matrix(model.inp$xlcal),
                          ylcal = model.inp$ylcal,
                          yd = model.inp$yd,
                          censflag = model.inp$censflag,
                          NOBSC = as.integer(length(model.inp$ylcal)),
                          NPAR = as.integer(NPAR),
                          paraml = double(NPAR+1),
                          parmle = double(NPAR+1),
                          LLK = double(1),
                          AIC = double(1),
                          SPPC = double(1),
                          IERR = integer(1))
    if(model.out$IERR > 0)
      stop(paste('Error Code:', model.out$IERR))
    calibr$aic[j] <- model.out$AIC
    calibr$sppc[j] <- model.out$SPPC
  } # end of for j
  calibr$rank.aic <- rank(calibr$aic)
  calibr$rank.sppc <- rank(-calibr$sppc)
  print(data.frame(calibr))
  model <- as.character(which(calibr$rank.aic == 1))
  return(model)
}
