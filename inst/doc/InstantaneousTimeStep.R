### R code from vignette source 'InstantaneousTimeStep.Rnw'

###################################################
### code chunk number 1: InstantaneousTimeStep.Rnw:21-45
###################################################
# Load the necessary packages and the data
library(rloadest)
library(dataRetrieval)
# What unit values are available?
subset(whatNWISdata("04027000", "uv"),
  select=c("parm_cd", "srsname", "begin_date", "end_date"))
# Get the QW data
BadQW <- importNWISqw("04027000", "00940", 
  begin.date="2011-04-01", end.date="2014-09-30")
# Merge data and time and set timezone (2 steps)
BadQW <- transform(BadQW, dateTime=sample_dt + as.timeDay(sample_tm))
BadQW <- transform(BadQW, dateTime=setTZ(dateTime, tzone_cd))
# Now the Unit values data
BadUV <- readNWISuv("04027000", c("00060", "00095", "00300", "63680"),
  startDate="2011-04-01", endDate="2014-09-30", tz="America/Chicago")
BadUV <- renameNWISColumns(BadUV)
names(BadUV)
# Strip _Inst off column names
names(BadUV) <- sub("_Inst", "", names(BadUV))
# Merge the data
BadData <- mergeNearest(BadQW, "dateTime", right=BadUV, dates.right="dateTime",
  max.diff="4 hours")
# Rename the left-hand dateTime column
names(BadData)[9] <- "dateTime"


###################################################
### code chunk number 2: InstantaneousTimeStep.Rnw:53-55
###################################################
# Print the number of missing values in each column
sapply(BadData, function(col) sum(is.na(col)))


###################################################
### code chunk number 3: InstantaneousTimeStep.Rnw:60-67
###################################################
# Create the and print the candidate model.
BadChloride.lr <- selBestSubset(Chloride ~ log(Flow) + fourier(dateTime) + 
  log(SpecCond) + log(DO) + log(Turb), data=BadData,
  flow="Flow", dates="dateTime", time.step="instantaneous", 
  station="Bad River near Odanah", criterion="SPCC")

print(BadChloride.lr)


###################################################
### code chunk number 4: InstantaneousTimeStep.Rnw:74-78
###################################################
# Plot the overall fit, choose plot number 2.
setSweave("graph01", 6, 6)
plot(BadChloride.lr, which = 2, set.up=FALSE)
dev.off()


###################################################
### code chunk number 5: InstantaneousTimeStep.Rnw:87-91
###################################################
# Plot the residual Q-normal graph.
setSweave("graph02", 6, 6)
plot(BadChloride.lr, which = 5, set.up=FALSE)
dev.off()


###################################################
### code chunk number 6: InstantaneousTimeStep.Rnw:100-104
###################################################
# Plot the residual Q-normal graph.
setSweave("graph03", 6, 6)
plot(BadChloride.lr, which = "log(Turb)", set.up=FALSE)
dev.off()


###################################################
### code chunk number 7: InstantaneousTimeStep.Rnw:113-120
###################################################
# Create the and print the revised model.
BadChloride.lr <- loadReg(Chloride ~ log(Flow) + fourier(dateTime) + 
  log(SpecCond) + Turb, data=BadData,
  flow="Flow", dates="dateTime", time.step="instantaneous", 
  station="Bad River near Odanah")

print(BadChloride.lr, load.only=FALSE)


###################################################
### code chunk number 8: InstantaneousTimeStep.Rnw:128-132
###################################################
# Plot the overall fit, choose plot number 2.
setSweave("graph04", 6, 6)
plot(BadChloride.lr, which = 2, set.up=FALSE)
dev.off()


###################################################
### code chunk number 9: InstantaneousTimeStep.Rnw:141-145
###################################################
# Plot the S-L grpah.
setSweave("graph05", 6, 6)
plot(BadChloride.lr, which = 3, set.up=FALSE)
dev.off()


###################################################
### code chunk number 10: InstantaneousTimeStep.Rnw:154-158
###################################################
# Plot the residual Q-normal graph.
setSweave("graph06", 6, 6)
plot(BadChloride.lr, which = 5, set.up=FALSE)
dev.off()


###################################################
### code chunk number 11: InstantaneousTimeStep.Rnw:168-172
###################################################
# Plot the residual Q-normal graph.
setSweave("graph07", 6, 6)
plot(BadChloride.lr, which = "Turb", set.up=FALSE)
dev.off()


###################################################
### code chunk number 12: InstantaneousTimeStep.Rnw:186-202
###################################################
# Extract one day from the UV data
Bad063014 <- subset(BadUV, as.Date(as.POSIXlt(dateTime)) == "2014-06-30")
# Remove the unecessary surrogates from the data set.
# This reduces the likelihood of missing values in the dataset
Bad063014 <- Bad063014[, c("dateTime", "Flow", "SpecCond", "Turb")]
# Simple check
any(is.na(Bad063014))
# Estimate concetrations
Bad063014.est <- predConc(BadChloride.lr, Bad063014, by="unit")
# Display the first and last few rows.
head(Bad063014.est)
tail(Bad063014.est)
# The daily mean concentration can also be easily estimated
predConc(BadChloride.lr, Bad063014, by="day")
# Compare to the mean of the unit values:
with(Bad063014.est, mean(Conc))


###################################################
### code chunk number 13: InstantaneousTimeStep.Rnw:214-241
###################################################
# Extract one month from the UV data, done in two steps
Bad0714 <- subset(BadUV, as.Date(as.POSIXlt(dateTime)) >= "2014-07-01")
Bad0714 <- subset(Bad0714, as.Date(as.POSIXlt(dateTime)) <= "2014-07-31")
# Remove the unecessary surrogates from the data set.
# This reduces the likelihood of missing values in the dataset
Bad0714 <- Bad0714[, c("dateTime", "Flow", "SpecCond", "Turb")]
# Simple check on each column, how many in each column?
sapply(Bad0714, function(x) sum(is.na(x)))
# Fix each column, using the defaults of fillMissing
Bad0714$SpecCond <- fillMissing(Bad0714$SpecCond)
Bad0714$Turb <- fillMissing(Bad0714$Turb)
# Verify filled values
sapply(Bad0714, function(x) sum(is.na(x)))
# Estimate daily loads
Bad0714.day <- predLoad(BadChloride.lr, Bad0714, by="day")
# Display the first and last few rows.
head(Bad0714.day)
tail(Bad0714.day)
# And the month
Bad0714.mon <- predLoad(BadChloride.lr, Bad0714, by="month")
Bad0714.mon
# Compare to the results using the approximate standard error:
# For long periods, the processing time to the exact seopt can be very large
# and may be desireable to use the approximation.
predLoad(BadChloride.lr, Bad0714, by="month", seopt="app")
# Compare to the mean of the daily values:
with(Bad0714.day, mean(Flux))


