# dialog support functions for S-LOADEST.
#    Compute limits for the Grubbs & Beck 
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2008Apr24 DLLorenz Modified to use the actual distribution
#    2004Jan28          This version.
#
loadestGrubbsBeck <- function(N) {
  ## apply the 5% two-tailed test to normalized residuals
  if(N < 3)
    return(NA)
  G <- qt(.05/(2*N), N-2)
  return((N-1)/sqrt(N)*sqrt(G^2/(N-2+G^2)))
}
