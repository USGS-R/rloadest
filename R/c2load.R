#' Loads
#'
#' Convert concentration and flow to load (flux).
#'
#' @param conc the concentration data missing values are permitted
#'and result in missing values in the output.
#' @param flow the flow data missing values are permitted
#'and result in missing values in the output.
#' @param flow.units character string describing the flow unit.
#' @param conc.units character string describing the concentration 
#'unit.
#' @param load.units character string describing the load unit.
#' @param ignore.censoring logical, see \bold{Value}.
#' @return If \code{ignore.censoring} is \code{TRUE}, the default,
#'then return a vector of numeric values with censored values replaced
#'by 1/2 the detection limit. Otherwise, return a vector that retains
#'the censoring---if \code{conc} is numeric, then uncensored; if
#'\code{conc} is of class "qw," then the returned data would be of
#'class "lcens" or "mcens."
#' @seealso \code{\link{loadReg}}
#' @references will need some.
#' @keywords regression censored loads
#' @examples
#'# These calls return the conversion factors
#'c2load(1, 1, conc.units="mg/L")
#'c2load(1, 1, conc.units="mg/L", load.units="tons")
#'
#'@export
c2load <- function(conc, flow, flow.units="cfs", conc.units="", 
                   load.units="kg", ignore.censoring=TRUE) {
  ## Coding history:
  ##    2013Jul26 DLLorenz Original Coding
  ##
  ## Get conc.units and deal with ignore.censoring
  if(class(conc) == "qw") {
    ## If conc.units are not specified, then try to get from object
    if(conc.units == "") {
      conc.units <- unique(Y@reporting.units)
      conc.units <- conc.units[!is.na(conc.units)]
      if(length(conc.units) == 0L)
        conc.units <- ""
      else if(length(conc.units) > 1L) {
        ## Select the first one with a length greater than 1
        lens <- nchar(conc.units)
        conc.units <- conc.unis[which(lens > 1L)[1L]]
      }
      ## Now make sure that as ... gets dropped
      conc.units <- strsplit(conc.units, " ")[[1L]][1L]
    }
    if(ignore.censoring) {
      conc <- as.numeric(conc)
    } else if(censoring(conc) == "multiple") {
      conc <- as.mcens(conc)
    } else
      conc <- as.lcens(conc)
  } else if(conc.units == "") {
    warning("Concentration units assumed to be mg/L")
    conc.units <- "mg/L"
  } 
  CF <- loadConvFactor(flow.units, conc.units, load.units)
  return(conc * flow * CF)
}
