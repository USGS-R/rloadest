#' Nash Sutcliffe
#'
#' Compute the Nash-Sutcliffe efficiency rating for model estiamtes.
#'
#' @param obs a vector of the observed values.
#' @param est a vector of the model estiamted values. Each value must 
#'pair with each value in \code{obs}.
#' @param na.rm remove missing values from \code{obs} and \code{est} before
#'computing the Nash-Sutcliffe efficiency rating.
#' @return A single numeric value representing the Nash-Sutcliffe efficiency 
#'rating for the observed and estiamted data.
#' @keywords utilities
#' @export
nashSutcliffe <- function(obs, est, na.rm=TRUE) {
  E <- 1 - sum((obs - est)^2, na.rm=na.rm)/
    sum((obs-mean(obs,na.rm=na.rm))^2, na.rm=na.rm)
  return(E)
}
