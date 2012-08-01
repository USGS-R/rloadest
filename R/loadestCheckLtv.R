# dialog support functions for S-LOADEST.
#    Compute the percentage of censored values and the levels of censoring
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Jan28          This version.
#
loadestCheckLtv <- function(data, pname, rname) {
  rvalues <- data[rname]
  pvalues <- data[pname]
  levels <- unique(pvalues[rvalues == '<'])
  ## now remove the missing values
  rvalues <- rvalues[!is.na(pvalues)]
  nvalues <- length(rvalues) # the total number of non-missing observations
  ## also need to include the zero values in the percent
  zvalues <- sum(pvalues == 0, na.rm=T)
  pct <- 100 * (length(rvalues[rvalues == '<']) + zvalues) / nvalues
  zvalues <- 100 * zvalues / nvalues
  list(pct=pct, levels=levels, zeros=zvalues)
}
