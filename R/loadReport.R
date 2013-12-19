#' Create Load Report
#' 
#' Create a 2-page pdf file report of a rating-curve load model. The 
#'report contains the text output and 6 diagnostic plots.
#'
#' @param x the load model.
#' @param file the output file base name; the .pdf suffix 
#'is appended to make the actual file name. if missing, then the
#'name of \code{x} is used as the base name.
#' @return The actual file name is returned invisibly.
#' @export
loadReport <- function(x, file) {
  ## Coding history:
  ##    2013Jul29 DLLorenz Original version from S+ library
  ##
  if(missing(file))
    file <- deparse(substitute(x))
  retval <- setPDF(basename=file)
  plot.new()
  ## Draw the text
  par(mar=c(0,0,0,0), usr=c(0,1,0,1))
  txt <- capture.output(x)
  text(0, 1, paste(txt, collapse="\n"), family="mono", adj=c(0,1))
  ## 6 diagnostic plots
  AA.lo <- setLayout(num.cols=2L, num.rows=3L)
  for(i in seq(6)) {
    setGraph(i, AA.lo)
    plot(x, which=i, set.up=FALSE)
  }
  ## All done, close the graph
  dev.off(retval[[1]])
  invisible(paste(retval[[2]], ".pdf", sep=""))
}
