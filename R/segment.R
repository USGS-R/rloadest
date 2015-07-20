#' Segmented Model
#'
#' Computes the basis for a segmented model. Used primarily in a linear 
#'regression or load model.
#'
#' @param x the data to segment. Missing values are permitted and result 
#'corresponding in missing values in output.
#' @param psi a numeric vector of the breakpoints.
#'
#' @return A matrix contining a column named X of the data in \code{x} and
#'paired U and P columns for each of the breaks in \code{psi} that form the 
#'basis for that segment.
#'
#' @export
segment <- function(x, psi) {
	# Set up matrix for segmented regression
	N <- length(psi)
	u <- matrix(rep(x, N), ncol=N)
	# Sweep out the values of psi, and set min to 0
	u <- sweep(u, 2L, psi)
	u <- apply(u, 2, function(x) pmax(0,x))
	colnames(u) <- paste("U", seq(N), sep="")
	p <- sign(u)
	colnames(p) <- paste("P", seq(N), sep="")
	return(cbind(X=x, u, p))
}