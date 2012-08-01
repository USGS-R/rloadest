#    Compute load conversion factor
#
# Coding history:
#    2004Oct29 DLLorenz Original version for USGS library
#    2004Oct29          This version.
#    2012Aug01 ldecicco Conversion to R
#

#' Unit conversion function
#'
#' Imports data from NWIS web service. This function gets the data from here: \url{http://qwwebservices.usgs.gov/}
#' @param flow.units string descriptive flow unit
#' @param conc.units string descriptive concentration unit
#' @param load.units string descriptive load unit
#' @keywords unit conversions
#' @return conv.factor number conversion factor
#' @export
#' @examples
#' # These examples require an internet connection to run
#' loadest.conv.factor('cubic meter per second','milligrams per liter','pounds')
loadest.conv.factor <- function(flow.units, conc.units, load.units) {
  if(flow.units == "cubic meter per second")
    conv.factor <- 86400000 # liters per day
  else # assume that the flow is cubic feet per second
    conv.factor <- 28.317 * 86400 # liters per day
  ## conversion from mg/L to output
  if(is.null(load.factor <- switch(load.units,
                                      "pounds" = 2.204623e-6,
                                      "tons" = 1.102311e-9,
                                      "milligrams" = 1,
                                      "grams" = 1.0e-3,
                                      "kilograms" = 1.0e-6,
                                      "metric tons" = 1.0e-9,
                                      "million colonies" = 1.0e-6)))
    stop("\nInvalid load units specified")
  conv.factor <- conv.factor * load.factor
  ## now convert from mg/L to other
  if(is.null(conc.factor <- switch(conc.units,
                                      "milligrams per liter" = 1,
                                      "micrograms per liter" = .001,
                                      "nanograms per liter" = .000001,
                                      "colonies per 100mL" = 10)))
    stop("\nInvalid concentration units specified")
  conv.factor <- conv.factor * conc.factor
  return(conv.factor)
}
