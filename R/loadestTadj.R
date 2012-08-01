# internal support function for S-LOADEST.
#    Compute the adjustment to time
#
# Coding history:
#    2004Jan29 DLLorenz Original version for USGS library
#    2004Oct29 DLLorenz Correctly compute t*
#    2005Dec02 DLLorenz Allow NAs (removed later)
#    2005Dec02          This version.
#
loadestTadj <- function(x) {
  ## two values are needed, the first produces a reference year (for sin and
  ## cos transforms), the second corrects to centered time
  Tadj <- c(0,0)
  Tadj[1] <- floor(min(x, na.rm=T))
  meant <- mean(x,na.rm=T)
  Tadj[2] <- mean(x - Tadj[1], na.rm=T) + sum((x - meant)^3) / 2 / sum((x - meant)^2)
  return(Tadj)
}
