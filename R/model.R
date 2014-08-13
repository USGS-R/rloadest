#' Load Model
#'
#' Support function for building a predefined rating curve load model.
#'
#'
#' @param model.no the model number.
#' @param data the dataset.
#' @param flow character string indicating the name of the 
#'flow column.
#' @param time character string indicating the name of the 
#'date/time column.
#'
#' @return Row number to select from \code{data} to build a predefined model.
#'
#' @export
model <- function(model.no, data, flow, time) {
  notok <- is.na(data[[flow]]) | is.na(data[[time]])
  retval <- seq(along=notok)
  retval[notok] <- NA
  return(matrix(retval, ncol=1L))
}
