library(USGSqw)
library(dataRetrieval)

source("D:/LADData/RCode/gitLOADEST/R/loadestConvFactor.R")
source("D:/LADData/RCode/gitLOADEST/R/loadestCheckLtv.R")
source("D:/LADData/RCode/gitLOADEST/R/loadestQadj.R")
source("D:/LADData/RCode/gitLOADEST/R/loadestTadj.R")
source("D:/LADData/RCode/gitLOADEST/R/loadestSetXLDat.R")
source("D:/LADData/RCode/gitLOADEST/R/loadestPrintHeader.R")
source("D:/LADData/RCode/gitLOADEST/R/loadestEvalAMLE.R")
source("D:/LADData/RCode/gitLOADEST/R/loadestPrintEval.R")
source("D:/LADData/RCode/gitLOADEST/R/loadestGrubbsBeck.R")

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

# Create a model with Sample Data:
sta <- "01491000"
param <- "00631"
StartDate <- "1979-09-01"
EndDate <- "2011-09-30"

QWData <- getQWData(sta,param,StartDate,EndDate,interactive=FALSE)
rmk <- QWData$qualifier.00631
Sample <- getSampleData(sta,param,StartDate,EndDate,interactive=FALSE)

Daily<-getDVData(sta,"00060",StartDate,EndDate,interactive=FALSE)

Sample<-mergeReport()

INFO<-getMetaData(sta,param,interactive=FALSE)

data <- data.frame(Sample$DecYear)
colnames(data) <- c("DECTIME")
preddata <- data.frame(Daily$DecYear)
colnames(preddata) <- c("DECTIME")

# constit <- INFO$constitAbbrev  # Something like this would be better....
constit <- "N"



data[[constit]] <- Sample$ConcAve
data[[paste(constit,"rmk",sep=".")]] <- rmk
data[["Q"]] <- Sample$Q
data[["Date"]] <- Sample$Date
data[["Time"]] <- rep("1200", nrow(Sample))

preddata[["Q"]] <- Daily$Q
preddata[["Date"]] <- Daily$Date
preddata[["Time"]] <- rep("1200", nrow(Daily))

rm(Sample,Daily,QWData,StartDate,EndDate,param,sta)

constit <- "N"
flow <- "Q"
Date <- "Date"
Time <- "Time"
lineartime <- "none"
savefile <- ""
build <- "Predefined"
models.predefined <- 9
method <- "AMLE"
conc <- "milligrams per liter"
load <- "tons"
units <- "cubic meter per second"
conv.factor <- loadest.conv.factor(units, conc, load)

allvars <- names(data)
varindex <- is.element(constit, allvars)
if(sum(varindex) == 1) {
  Pvar <- constit
  Rvar <- paste(constit, "[rR][mM][kK]", sep=".")
  #       Rvar <- sapply(Rvar,grep,text=allvars)
  Rvar <- grep(Rvar,allvars)
  Rvar <- allvars[Rvar]
} else { # nope, use R, P prefix
  Pvar <- paste("P", constit, sep="")
  Rvar <- paste("R", constit, sep="")
}


lineartime <- switch(lineartime, none = 0, linear = 1, quadratic = 2)
## Compute the Load
Lvar <- paste("Load", constit, sep=".")
data[[Lvar]] <- data[[Pvar]] * data[[flow]] * conv.factor
## remove those observations where the Load, DATES or TIMES is NA
data <- data[!is.na(data[[Lvar]]),]
data <- data[!is.na(data[[Date]]),]
## remove those observations where the Load is 0 (from 0 flow or 0 conc)
if(min(data[[Lvar]]) <= 0) {
  cat("\n\n****Caution zero load values removed from analysis****\n\n")
  data <- data[data[[Lvar]] <= 0,]
}

data <- data[!is.na(data[[Time]]),]

if (!("DECTIME" %in% colnames(data))){
  data[[Date]] <- floor(data[[Date]]) ## needed to fix for time, just in case
  
  Year <- month.day.year(data[[Date]])$year
  Days <- as.double(data[[Date]]) - julian(1,1,Year)
  Days <- Days + as.double(substring(data[[Time]],1,2))/24 +
    as.double(substring(data[[Time]],3,4))/1440
  
  data$DECTIME <- Year + Days/(365 + as.double(leap.year(Year)))    
}

## Dvar is the flux of the detection limit.
Dvar <- paste("Detect", constit, sep=".")

num.cens <- sum(data[[Rvar]] == "<")
if(num.cens == 0) { # determine the minimum value actual value should be irrelevant
  d.value <- min(data[[Pvar]])
  d.value <- 10^floor(log10(d.value))
} else {
  d.value <- data[[Pvar]]
  d.current <- d.value[which(data[[Rvar]] == "<")[1]]
  for(i in 1:length(d.value)) { # replace with most recent detection limit
    if(data[[Rvar]][i] == "<")
      d.current <- d.value[i]
    d.value[i] <- d.current
  }
}
data[[Dvar]] <- d.value * data[[flow]] * conv.factor

temp.ltv <- loadestCheckLtv(data, Pvar, Rvar) # Compute the percentage of censored values and the levels of censoring
if(temp.ltv$pct > 80) {
  cat(" Percentage of censored values (", temp.ltv$pct, ") exceeds 80.\n")
  if(temp.ltv$zeros > 0)
    cat("Zero values (nondetected values with unrecorded reporting limit) were found\n")
  return()
}

sname <- c(Rvar, Lvar, Dvar, flow, "DECTIME", Date, Time)

Qadj <- loadestQadj(data[[sname[4]]])
Tadj <- loadestTadj(data$DECTIME)
model.inp <- loadestSetXLDat(data,sname,as.double(models.predefined),Qadj,Tadj)
goodrows <- !is.na(rowSums(model.inp$xlcal))
loadestPrintHeader(goodrows, model.inp, data[[sname[6]]],
                   data[[sname[4]]], Qadj, Tadj)
  
evaluat <- loadestEvalAMLE(goodrows, model.inp)

evaluat2 <- censReg_AMLE.fit(as.lcens(model.inp$ylcal), model.inp$xlcal) 
evaluat2 <- loadestPrintEval(evaluat2, method="AMLE")

evaluat3 <- censReg_AMLE.fit(as.lcens(model.inp$ylcal), model.inp$xlcal,"lognormal") # Figure this out...
evaluat3 <- loadestPrintEval(evaluat3, method="AMLE")

flowtrans <- ""
floworder <- ""
lineartime <- 0
seasonal <- ""
period <- ""
additional.terms <- ""
diurnal <- FALSE
exact<- TRUE
snamep <- c("", sname[4:7])
model.inp <- loadestSetXLDat(preddata, snamep, models.predefined, Qadj, Tadj, flowtrans,
                             floworder, lineartime, seasonal, period,
                             additional.terms, diurnal)

predictedReturn <- censReg_AMLE.pred(evaluat, model.inp$xlcal)
predictedReturn2 <- censReg_AMLE.pred(evaluat2, model.inp$xlcal)
# predictedReturn3 <- censReg_AMLE.pred(evaluat3, model.inp$xlcal)  #Hangs up the computer, need to quit RStudio to stop it

#######################################################################################
# For fun:
#######################################################################################
plot(preddata$Date,predictedReturn$ESTIM,
     xlab="Date",ylab="",type="l",col="blue")
axis(2,col="blue",col.axis="blue")
mtext("ESTIM",side=2,line=3,col="blue")
text(as.Date("1980-12-15"),2,"ESTIM",col="blue")
text(as.Date("1980-12-18"),1,"Discharge",col="red")
par(new=T)
plot(preddata$Date, preddata$Q,pch=15,  xlab="", ylab="", axes=F, 
     type="l", col="red", ylim=c(0,150))
axis(4, col="red",col.axis="red")
mtext("Discharge",side=4,line=3,col="red")
mtext(INFO$station.nm, side=3,line=1)


#######################################################################################
# For fun zoomed in some random section:
#######################################################################################
startYear <- 1992
endYear <- 1997
xlimit <- c(as.Date(paste(startYear,"-01-01",sep="")),as.Date(paste(endYear,"-01-01",sep="")))
datesLabel <- ISOdate(startYear:endYear, 1, 1) # first of every year

par(mfcol=c(1,1), mar=c(4,4,1,4), oma=c(2,1,3,1),xpd=F, mgp = c(3, 1, 0))
plot(preddata$Date,predictedReturn$ESTIM,
     xlab="Date", ylab="", type="l",col="blue",xlim=xlimit)
axis(2,col="blue",col.axis="blue")
mtext("ESTIM",side=2,line=3,col="blue")
par(new=T)
plot(preddata$Date, preddata$Q,pch=15,  xlab="", ylab="", axes=F, xlim=xlimit,
     type="l", col="red", ylim=c(0,150))
axis(4, col="red",col.axis="red")
axis(1, at=as.Date(datesLabel), labels=format(datesLabel, "%Y"))
mtext("Discharge",side=4,line=3,col="red")
mtext(INFO$station.nm, side=3,line=1)
#######################################################################################
# Raw data:
#######################################################################################

par(mfcol=c(1,1), mar=c(4,4,1,4), oma=c(2,1,3,1),xpd=F, mgp = c(3, 1, 0))
plot(data$Date,data$N,
     xlab="Date", ylab="",col="blue",xlim=xlimit,type="b")
axis(2,col="blue",col.axis="blue")
mtext(INFO$param.nm,side=2,line=3,col="blue")
par(new=T)
plot(data$Date, data$Q,pch=15,  xlab="", ylab="", axes=F, xlim=xlimit,
     type="b", col="red", ylim=c(0,150))
axis(4, col="red",col.axis="red")
axis(1, at=as.Date(datesLabel), labels=format(datesLabel, "%Y"))
mtext("Discharge",side=4,line=3,col="red")
mtext(INFO$station.nm, side=3,line=1)

#######################################################################################
# Raw data vs. estimated data:
#######################################################################################

par(mfcol=c(1,1), mar=c(4,4,1,4), oma=c(2,1,3,1),xpd=F, mgp = c(3, 1, 0))
plot(preddata$Date,predictedReturn$ESTIM,
     xlab="Date", ylab="",col="blue",xlim=xlimit,type="l")
axis(2,col="blue",col.axis="blue")
mtext("ESTIM",side=2,line=3,col="blue")
par(new=T)
plot(data$Date,data$N,pch=15,  xlab="", ylab="", axes=F, xlim=xlimit,
     type="b", col="red")
axis(4, col="red",col.axis="red")
axis(1, at=as.Date(datesLabel), labels=format(datesLabel, "%Y"))
mtext(INFO$param.nm,side=4,line=3,col="red")
mtext(INFO$station.nm, side=3,line=1)