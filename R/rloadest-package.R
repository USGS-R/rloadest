#' LOADEST functions for R
#'
#' \tabular{ll}{
#' Package: \tab rloadest\cr
#' Type: \tab Package\cr
#' Version: \tab 0.4.2\cr
#' Date: \tab 2015-07-20\cr
#' License: \tab CC0\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' This package is intended to replicate and extend the LOADEST program for 
#'estimating constituent loads in streams and rivers. Some subtle differences 
#'between the output for LOADEST and rloadest include:
#'
#'The least absolute deviation (LAD) method is not supported in rloadest.
#'
#'LOADEST uses centered time when computing the sine and cosine terms in model
#'numbers 4, 6, 7, 8, and 9, but the functions in rloadest use the actual decimal
#'time so that the seasonality can more easily be assessed by the user.
#'
#'The order of the terms in the predefined models is different between LOADEST
#'and the rloadest functions.
#'
#'The printed output of the model descriptions from rloadest matches the format 
#'the most users of R would recognize from other linear model output rather then
#'the printed output from LOADEST.
#'
#'Furthermore, the model building capability in the rloadest functions make easier
#'to explore other forms of rating-curve models than LOADEST.
#'
#' @name rloadest-package
#' @docType package
#' @author Dave Lorenz \email{lorenz@@usgs.gov}
#' @keywords load estimation
NULL
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.")
}
