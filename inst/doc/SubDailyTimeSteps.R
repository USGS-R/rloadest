### R code from vignette source 'SubDailyTimeSteps.Rnw'

###################################################
### code chunk number 1: SubDailyTimeSteps.Rnw:10-14
###################################################
library(knitr)
opts_chunk$set(
concordance=TRUE
)


###################################################
### code chunk number 2: SubDailyTimeSteps.Rnw:35-59
###################################################
# Load the necessary packages and the data
library(rloadest)
library(dataRetrieval)
# Get the QW data
Brushy <- "02207400"
# Parameter code 00665 is whole-water phosphorus
# Parameter code 71123 is mean streamflow for peak flow
# Parameter code 00060 is the daily mean streamflow
# Parameter code 00061 is the measured streamflow
# 71123 will be used as the flow for peak flows and 00061
# for base flow or 00060 if 00061 is missing.
BrushyQW <- importNWISqw("02207400",
  params=c("00625", "72123", "00060", "00061"),
  begin.date="2004-10-02", end.date="2013-09-30")
# Convert the separate columns of dates and times to a single column
# Uses functions from smwrBase
BrushyQW <- transform(BrushyQW,
  StartDateTime = setTZ(sample_dt + as.timeDay(sample_tm), tzone_cd, force.stz=TRUE),
  EndDateTime = setTZ(sample_end_dt + as.timeDay(sample_end_tm), tzone_cd, force.stz=TRUE))
# A few rows of data:
head(BrushyQW)
# Subset to remove the missing values in Kjeldahl_WW.N.00625
# Note that this works only because we are interested in Kjeldahl_WW.N.00625
BrushyQW <- subset(BrushyQW, !is.na(Kjeldahl_WW.N.00625))


###################################################
### code chunk number 3: SubDailyTimeSteps.Rnw:67-82
###################################################
# Compute the median peak duration.
with(BrushyQW, median(EndDateTime - StartDateTime, na.rm=T))
# Compute the sample date and time
BrushyQW <- transform(BrushyQW,
  dateTime=ifelse(is.na(EndDateTime), StartDateTime,
    StartDateTime + (EndDateTime - StartDateTime)/2))
# Need to convert to POSIXct. Note that the original data were
# recorded in standard time only, so the correct time zone is
# "America/Jamaica," which preserves the correct time offset.
BrushyQW <- transform(BrushyQW, dateTime=as.POSIXct(dateTime,
  origin="1970-01-01", tz="America/Jamaica"))
# Now the flow, coalesce is in smwrBase
BrushyQW <- transform(BrushyQW,
  Flow=ifelse(!is.na(EndDateTime), DischargeMeanStorm_cfs,
  coalesce(InstDischarge_cfs, Discharge_cfs)))


###################################################
### code chunk number 4: SubDailyTimeSteps.Rnw:90-94
###################################################
# Find the "best" predefined model
Brushy.lreg <- selBestModel("Kjeldahl_WW.N.00625", BrushyQW, flow="Flow",
  dates="dateTime", time.step="6 hour", station=Brushy)
print(Brushy.lreg)


###################################################
### code chunk number 5: SubDailyTimeSteps.Rnw:99-103
###################################################
# Set up for graph in vignette
setSweave("graph01", 6, 6)
plot(Brushy.lreg, which = 1, set.up=FALSE)
dev.off()


###################################################
### code chunk number 6: SubDailyTimeSteps.Rnw:111-115
###################################################
# Set up for graph in vignette
setSweave("graph02", 6, 6)
plot(Brushy.lreg, which = 4, set.up=FALSE)
dev.off()


###################################################
### code chunk number 7: SubDailyTimeSteps.Rnw:123-127
###################################################
# Set up for graph in vignette
setSweave("graph03", 6, 6)
plot(Brushy.lreg, which = 5, set.up=FALSE)
dev.off()


###################################################
### code chunk number 8: SubDailyTimeSteps.Rnw:137-139
###################################################
# Jackknife statistics
jackStats(Brushy.lreg)


###################################################
### code chunk number 9: SubDailyTimeSteps.Rnw:147-162
###################################################
# Get the data for May, 2013
BrushyQ <- readNWISuv(Brushy, "00060", startDate="2013-05-01",
  endDate="2013-05-31", tz="America/New_York")
# Rename the Flow column, must be done manually as renameNWISColumns
# appends _Inst
# Detect the Flow column position based on the expectation
# that it will be numeric
flowColumn <- seq(4, 6, 1)[
                which.min(lapply(BrushyQ[,4:6],
                  function(x) sum(is.na(as.numeric(x)))))]
names(BrushyQ)[flowColumn] <- "Flow"
# Show the gap in the record:
BrushyQ[2030:2040,]
# predict the load
predLoad(Brushy.lreg, BrushyQ, by="month")


