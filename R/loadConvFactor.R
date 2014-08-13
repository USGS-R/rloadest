#' Unit Conversion
#'
#' Computes the conversion factor to compute the flux units 
#'(load units) from the concentration and flow units
#'
#' @param flow.units character string describing the flow unit. The only valid values are
#'"cubic meter per second," "cms," "cubic feet per second," or "cfs."
#' @param conc.units character string describing the concentration unit. The valid units
#'are "mg/l," "mg/L," "ug/l," "ug/L," "ng/l," "ng/L," "milligrams per liter,"
#'"micrograms per liter," "nanograms per liter," "col/100mL," "col/dL," or
#'"colonies per 100mL."
#' @param load.units character string describing the load unit. The valid values are
#'"pounds," "tons," "mg," "milligrams," "grams," "g," "kilograms," "kg," 
#'"metric tons," "Mg," or "million colonies."
#' @keywords unit conversions
#' @return The conversion factor.
#' @examples
#' loadConvFactor("cubic meter per second","milligrams per liter","pounds")
#' @export
loadConvFactor <- function(flow.units, conc.units, load.units) {
  ## Coding history:
  ##    2004Oct29 DLLorenz Original version for S+ library
  ##    2012Aug01 ldecicco Conversion to R
  ##    2013May31 DLLorenz Added abbreviations and renamed to loadConvFactor
  ##
  if(flow.units %in% c("cubic meter per second", "cms")) {
    conv.factor <- 86400000 # liters per day
  } else if(flow.units %in% c("cubic feet per second", "cfs")) {
    conv.factor <- 28.317 * 86400 # liters per day
  } else
    stop("Invalid flow units: ", flow.units)
  ## conversion from mg/L to output
  if(is.null(load.factor <- switch(load.units,
                                   "pounds" = 2.204623e-6,
                                   "tons" = 1.102311e-9,
                                   "mg" = 1,
                                   "milligrams" = 1,
                                   "grams" = 1.0e-3,
                                   "g" = 1.0e-3,
                                   "kilograms" = 1.0e-6,
                                   "kg" = 1.0e-6,
                                   "metric tons" = 1.0e-9,
                                   "Mg" = 1.0e-9,
                                   "million colonies" = 1.0e-6)))
    stop("\nInvalid load units specified")
  conv.factor <- conv.factor * load.factor
  ## now convert from mg/L to other
  if(is.null(conc.factor <- switch(conc.units,
                                   "mg/l" = 1,
                                   "mg/L" = 1,
                                   "ug/l" = .001,
                                   "ug/L" = .001,
                                   "ng/l" = .000001,
                                   "ng/L" = .000001,
                                   "milligrams per liter" = 1,
                                   "micrograms per liter" = .001,
                                   "nanograms per liter" = .000001,
                                   "col/100mL" = 10,
                                   "col/dL" = 10,
                                   "colonies per 100mL" = 10)))
    stop("\nInvalid concentration units specified")
  conv.factor <- conv.factor * conc.factor
  return(conv.factor)
}
