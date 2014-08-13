#' Centered Linear Terms
#'
#' Computes centered values. Used primarily in a linear regression formula.
#'
#'
#' @param x a numeric vector for which to compute the centered values. Missing values
#' are permitted and result in corresponding missing values in the output.
#' @param center an optional value to use for the center of \code{x}.
#' @note The centering value by default is computed by the method described
#'in Cohn and others (1992).
#' @return The centered value of \code{x}.
#' @seealso \code{\link[USGSwsBase]{quadratic}}, \code{\link{scale}}
#' @references Cohn, T.A., Caulder, D.L., Gilroy, E.J., Zynjuk, L.D., and
#'Summers, R.M., 1992, The validity of a simple statistical model for
#'estimating fluvial constituent loads---An empirical study involving nutrient
#'loads entering Chesapeake Bay: Water Resources research, v. 28, no. 5, p.
#'937--942.
#' @keywords manip
#' @examples
#'# trivial ceneterd values from 1 to 10
#'center(seq(10))
#' @export
center <- function (x, center = NULL) {
  ## Coding history:
  ##    2013Jun12 DLLorenz Original coding
  ##
  if (is.null(center)) {
    meanx <- mean(x, na.rm = T)
    xstar <- meanx + sum((x - meanx)^3, na.rm = T)/2/sum((x - 
                                                            meanx)^2, na.rm = T)
    if (is.na(xstar)) 
      xstar <- meanx
    xr <- log10(diff(range(x, na.rm = T)))
  }
  else xstar <- center
  retval <- x - xstar
  attr(retval, "xstar") <- xstar
  class(retval) <- "center"
  return(retval)
}
