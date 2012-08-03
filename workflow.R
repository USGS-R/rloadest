library(USGSqw)

setMethod("as.lcens", signature(values="numeric", detlim="missing", censor.codes="logical"),
          function(values, detlim, censor.codes) {
            ## Based on the DLIMIT function in LOADEST:
            ##   Here's the approach that SA LOADEST uses to impute detection limits.
            ## 1. If there are no censored values, then the detection limit is 1x10^-25.
            ## 2. If there are censored values, then the data are assumed to be in
            ##    sequential order and detection limit is set as the value of the
            ##    most recent censored value, backfilling from the first censored
            ##    observation to the first observation in the dataset. If any uncensored
            ##    concentration is less than the detection limit of the most recent
            ##    censored value, then the detection limit is set to 1x10^-25.
            ##
            if(any(censor.codes)) {
              dl <- values[which(censor.codes)[1]] # this is the first in the sequence
              detlim <- rep(dl, length(values))
              for(i in seq(along=values)) {
                if(is.na(values[i]))
                  detlim[i] <- NA
                else if(censor.codes[i]) { # assume that never NA if get to here
                  dl <- values[i]
                  detlim[i] <- dl
                }
                else # uncensored
                  detlim[i] <- if(dl <= values[i]) dl else 1.e-25
              }
            }
            else
              detlim <- rep(1.e-25, length(values))
            mat <- cbind(values=values, detlim=detlim)
            retval <- new("lcens", mat, censor.codes=censor.codes)
            return(retval) }
          )



# Yd is a vector of water quality data
Yd <- c(2.7,3.98,1.79,9.28,46.18,64.56,27.19,11.49,347.18,133.60,6.8,11.95,11.19,5.77,39.39,4.38,7.0,1.81,96.35,1.53,22.36,14.25,4.28,9.05,23.07,96.89,5.97,5.62,78.33,13.55,17.25,41)

# Qd is a vector of flow data that corrolates to the water quality data
# Qd should be centered flow?
Qd <- c(1.98,1.15,1.85,1.95,3.68,3.17,1.32,5.56,3.39,3.67,2.62,1.69,3.11,1.61,4.0,1.19,1.93,1.48,2.77,1.19,2.1,2.45,1.3,1.65,3.46,3.38,3.76,2.22,3.16,1.15,1.77,1.58)

with(DF, censReg_AMLE.fit(as.lcens(Yd), cbind(1, Qd), "lognormal"))$PARAML #Uncensored data
lm(log(Yd) ~ Qd, data=DF) #coefs agree
with(DF, censReg_AMLE.fit(as.lcens(Yd, 10), cbind(1, Qd), "lognormal"))$PARAML # ~ 44% censored


# QdEST is the flow data we want to get estimated loads for
# I think this should also be centered?
QdEST <- seq(0.5,4,0.5)
timeEST <- c(1:8)
XPRED <- do.call(cbind,list(timeEST,QdEST))


fitReturn <- censReg_AMLE.fit(as.lcens(Yd, 10), cbind(1, Qd), "lognormal")
fitReturn
predReturn <- censReg_AMLE.pred(fitReturn, XPRED)

