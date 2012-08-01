# internal support function for S-LOADEST.
#    Compute the adjustment to flow
#
# Coding history:
#    2004Jan29 DLLorenz Original version for USGS library
#    2004Oct29 DLLorenz Correctly compute Q*
#    2005Oct03 DLLorenz allow for constant flow
#    2005Dec05 DLLorenz allow missing values in flow (removed later)
#    2005Dec05         This version.
#
loadestQadj <- function(x) {
  Q <- log(x)
  meanQ <- mean(Q, na.rm=T)
  Qstar <- meanQ + sum((Q - meanQ)^3) / 2 / sum((Q - meanQ)^2)
  if(is.na(Qstar)) Qstar <- meanQ # allow for constant flow
  return(exp(Qstar))
}
