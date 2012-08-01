# dialog support functions for S-LOADEST.
#    Process the instantaneous estimates
#
# Coding history:
#    2004Jan28 DLLorenz Original version for USGS library
#    2004Oct29 DLLorenz Modified Upper and lower estimates
#    2004Oct29          This version.
#
loadestProcessInst <- function(file, pred.interval, exceed.crit) {
  if(missing(file))
    return()
  data <- get(file, where=1)
  data.attr <- get(paste(file, "attr", sep="."), where=1)
  out.data <- data[c("DATES", "TIMES", "FLOW")]
  ## compute the flux and concentration
  out.data["Flux"] <- exp(data$log.median)*data$phi
  out.data["Conc"] <- out.data$Flux/out.data$FLOW/data.attr$conv.factor
  ## set up for prediction interval and probability of exceeding the criterion
  eta <- data$log.median
  sigma <- data$stderrpred
  if(!missing(pred.interval)) {
    pred.interval <- (1 - pred.interval)/2
    upper <- eta + qt(1 - pred.interval, data.attr$df) * sigma
    lower <- eta + qt(pred.interval, data.attr$df) * sigma
    out.data["Upper"] <- exp(upper)/out.data$FLOW/data.attr$conv.factor
    out.data["Lower"] <- exp(lower)/out.data$FLOW/data.attr$conv.factor
  }
  if(!missing(exceed.crit)) {
    exceed.crit <- log(exceed.crit*out.data$FLOW*data.attr$conv.factor)
    out.data["Prob.exceed"] <- pt((eta - exceed.crit) / sigma, data.attr$df)
  }
  return(out.data)
}
