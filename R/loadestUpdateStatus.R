# dialog support functions for S-LOADEST.
#    Update loadest.status
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Jan28          This version.
#
loadestUpdateStatus <- function(station,sname,status) {
  ## update status of station and sname
  ## all arguments must be character
  loadest.status <- get("loadest.status", where=1)
  loadest.status[station,sname][[1]] <- status
  assign("loadest.status", loadest.status, where=1)
  print(paste("Updated status of", sname, "for", station, "to", status))
  invisible()
}
