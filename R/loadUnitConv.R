#' Unit Conversion
#'
#' Computes the factor to convert from between load units.
#'
#' @param from character string describing the load units to
#'convert from.
#' @param to character string describing the load units to 
#'convert to.
#' @keywords unit conversions
#' @return The conversion factor.
#' @examples
#' loadUnitConv("kilograms", "tons")
#' @export
loadUnitConv <- function(from, to) {
  ## Coding history:
  ##    2013Jun12 DLLorenz Original Coding
  ##
  if(is.null(to <- switch(to,
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
  stop("\nInvalid 'to' load units specified")
  if(is.null(from <- switch(from,
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
  stop("\nInvalid 'from' load units specified")
  return(to/from)
}
