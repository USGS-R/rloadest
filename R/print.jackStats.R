#' Print Results
#'
#' Print the results of a jackknife analysis of a censored regression.
#'
#' @param x an object of class "jackStats"---output from \code{jackStats}.
#' @param digits the number of significant digits to print.
#' @param \dots further arguments passed to or from other methods.
#' @return The object \code{x} is returned invisibly.
#' @note The printed output includes the original original estimate,
#'the jackknife bias and standard error and the relative bias for
#'each parameter in the regression model.
#'
#' @seealso \code{\link{loadReg}}
#' @keywords utilities
#' @export
#' @method print jackStats
print.jackStats <- function(x, digits=4, ...) {
  ##
  if(x$pctcens == 0) {
    cat("jackknife estimates:\n\n",
        "PRESS: ", signif(x$press, digits), "\nCoefficients:\n", sep="")
  } else {
    cat("jackknife estimates:\n\nCoefficients:\n", sep="")
  }
  coef <- x$coef
  # compute the relative bias
  coef <- cbind(coef, abs(coef[,2L]/coef[,3L]))
  colnames(coef) <- c("Estimate", "Bias", "Std. Error", "Rel. Bias")
  print(coef, digits=digits)
  invisible(x)
}
