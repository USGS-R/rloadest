#' Daily Mean
#'
#' Compute daily mean water-quality values for data frames containing 
#'water-quality data.
#'
#' @param x a data frame containing water-quality data.
#' @param dates the name of the sample date column.
#' @param times the name of the sample time column.
#'
#' @return A data frame like \code{x} but with the means for each 
#'column by day and the sample time set to "1200."
#'
#' @export
dailyAg <- function(x, dates="sample_dt", times="sample_tm") {
  Days <- as.character(as.Date(x[[dates]]))
  retval <- lapply(x, function(Col) {
    retc <- tapply(Col, Days, mean, simplify=FALSE)
    retc <- do.call(c, retc)  
    return(retc)})
  retval$stringsAsFactors <- FALSE
  retval <- do.call(data.frame, retval)
  ## Fix times
  retval[[times]] <- "1200"
  return(retval)
}
