#' LOADEST functions for R
#'
#' \tabular{ll}{
#' Package: \tab rloadest\cr
#' Type: \tab Package\cr
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
#' @keywords load estimation
NULL
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This information is preliminary or provisional and
is subject to revision. It is being provided to meet
the need for timely best science. The information
has not received final approval by the U.S. Geological
Survey (USGS) and is provided on the condition that
neither the USGS nor the U.S. Government shall be held
liable for any damages resulting from the authorized
or unauthorized use of the information.

****Orphaned Package****
This package is looking for a new maintainer. For more information, 
see: https://owi.usgs.gov/R/packages.html#orphan")
}
