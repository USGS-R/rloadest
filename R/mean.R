#' Arithmetic Mean
#'
#' Method functions to compute the "mean" for factors and characters. These
#'funcitons are intended primarily as support functions when aggregating 
#'unit values data in \code{predLoad}.
#'
#' @aliases mean.factor mean.character
#' @param x an object of class "factor" or "character."
#' @param \dots further arguments passed to or from other methods.
#'
#' @return If all values are identical, then the unique value, otherwise
#'the missing value (\code{NA}).
#'
#' @rdname mean
#' @export
#' @method mean factor
mean.factor <- function(x, ...) {
  z <- unique(x)
  return(if(length(z) == 1L) z else NA)
}

#' @rdname mean
#' @method mean character
#' @export
mean.character <- function(x, ...) {
  z <- unique(x)
  return(if(length(z) == 1L) z else NA)
}
