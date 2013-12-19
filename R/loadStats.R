#' Summary Statistics
#'
#' Compute some summary statistics for a rating-curve load-estimation model.
#'
#' @param fit an object of class "loadReg"---output from \code{loadReg}.
#' @param which a character string indicating the "load" or
#'"concentration" model.
#' @return A list containing \code{outSum}, selected summary statistics;
#'of the observed and estimated values and \code{outBias}, the bias 
#'statistics.
#' @seealso \code{\link{loadReg}}
#' @keywords utilities
#' @examples
#'# From application 1 in the vignettes
#'data(app1.calib)
#'app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, 
#'  flow = "FLOW", dates = "DATES", conc.units="mg/L",
#'  station="Illinois River at Marseilles, Ill.")
#'loadStats(app1.lr)
#' @export
loadStats <- function(fit, which="load") {
  ## Compute some stats
  which <- match.arg(which, c("load", "concentration"))
  if(which == "load")
    fit <- fit$lfit
  else
    fit <- fit$cfit
  Obs <- exp(fit$YLCAL)
  Obs <- ifelse(fit$CENSFLAG, Obs/2, Obs)
  Est <- fit$YPRED
  Pest <- quantile(Est, probs=c(0, .25, .5, .75, .9, .95, 1.0), type=2L)
  Pobs <- quantile(Obs, probs=c(0, .25, .5, .75, .9, .95, 1.0), type=2L)
  outSum <- rbind(Est=round(signif(Pest, 3), 2), 
                  Obs=round(signif(Pobs, 3), 2))
  colnames(outSum)[c(1L, 7L)] <- c("Min", "Max")
  ## Compute the bias diagnostics
  PLR <- sum(Est)/sum(Obs)
  E <- nashSutcliffe(Obs, Est)
  Bp <- 100*(PLR - 1)
  outBias <- c(Bp, PLR, E)
  if(which == "load")
    names(outBias) <- c("Bp", "PLR", "E")
  else
    names(outBias) <- c("Bp", "PCR", "E")
  return(list(outSum=outSum, outBias=outBias))
}
