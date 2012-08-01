# dialog support functions for S-LOADEST.
#    Print contents of loadest.status
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Oct07 DLLorenz added drop=F (does not retain names)
#    2006Jan20 DLLorenz Fixed sname retention and print error
#                       if loadest.status not found
#    2006Jan20          This version.
#
loadestPrintStatus <- function() {
  ## simple, just print the contents of loadest.status
  if(!exists("loadest.status", where=1)) {
    cat("\nS-LOADEST structures not available\n\n")
    return()
  }
  loadest.status <- get("loadest.status", where=1)
  cat("\n	***  S-LOADEST Processing status.  ***\n")
  loadest.stations <- get("loadest.stations", where=1)$stations
  snames <- dimnames(loadest.status)$snames
  for(i in loadest.stations) {
    cat("\nStation: ",i,"\n")
    status <- unlist(loadest.status[i,, drop=F])
    names(status) <- snames
    print(status)
  }
  invisible()
}
