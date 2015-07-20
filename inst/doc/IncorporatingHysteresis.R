### R code from vignette source 'IncorporatingHysteresis.Rnw'

###################################################
### code chunk number 1: IncorporatingHysteresis.Rnw:21-32
###################################################
# Load the necessary packages and the data
library(rloadest)
library(dataRetrieval)
# Get the QW data
Boyer <- "06609500"
BoyerQW <- importNWISqw(Boyer, "00631", 
  begin.date="2003-10-01", end.date="2012-09-30")
# Now the Daily values data
BoyerQ <- readNWISdv(Boyer, "00060", startDate="2003-10-01", 
  endDate="2012-09-30")
BoyerQ <- renameNWISColumns(BoyerQ)


###################################################
### code chunk number 2: IncorporatingHysteresis.Rnw:40-53
###################################################
# Compute the hysteresis metrics.
BoyerQ <- transform(BoyerQ,
  dQ1 = hysteresis(log(Flow), 1),
  dQ3 = hysteresis(log(Flow), 3))
# Rename the date column in QW so that the data can be merged
names(BoyerQW)[2] <- "Date"
# Merge the data
BoyerData <- mergeQ(BoyerQW, FLOW=c("Flow", "dQ1", "dQ3"), 
  DATES="Date", Qdata=BoyerQ, Plot=F)
# Create the initial model
Boyer.lr <- selBestModel("NO2PlusNO3.N", BoyerData,
  flow="Flow", dates="Date", station=Boyer)
print(Boyer.lr)


###################################################
### code chunk number 3: IncorporatingHysteresis.Rnw:58-72
###################################################
# residuals and hysteresis
setSweave("graph01", 6, 8)
AA.lo <- setLayout(num.rows=2)
setGraph(1, AA.lo)
AA.pl <- xyPlot(BoyerData$dQ1, residuals(Boyer.lr),
  ytitle="Residuals", xtitle="1-day lag hysteresis",
  xaxis.range=c(-1,3))
addSLR(AA.pl)
setGraph(2, AA.lo)
AA.pl <- xyPlot(BoyerData$dQ3, residuals(Boyer.lr),
  ytitle="Residuals", xtitle="3-day lag hysteresis",
  xaxis.range=c(-1,3))
addSLR(AA.pl)
dev.off()


###################################################
### code chunk number 4: IncorporatingHysteresis.Rnw:85-90
###################################################
# Construct the model
Boyer.lr <- loadReg(NO2PlusNO3.N ~ quadratic(log(Flow)) +
  quadratic(dectime(Date)) + fourier(Date) + dQ1, data=BoyerData,
  flow="Flow", dates="Date", station=Boyer)
print(Boyer.lr)


###################################################
### code chunk number 5: IncorporatingHysteresis.Rnw:95-99
###################################################
# Plot the overall fit, choose "fourier(Date)cos(k=1)"
setSweave("graph02", 6, 6)
plot(Boyer.lr, which="fourier(Date)cos(k=1)", set.up=FALSE)
dev.off()


###################################################
### code chunk number 6: IncorporatingHysteresis.Rnw:108-113
###################################################
# Construct the revised model
Boyer.lr <- loadReg(NO2PlusNO3.N ~ quadratic(log(Flow)) +
  dectime(Date) + fourier(Date, 2) + dQ1, data=BoyerData,
  flow="Flow", dates="Date", station=Boyer)
print(Boyer.lr)


###################################################
### code chunk number 7: IncorporatingHysteresis.Rnw:118-122
###################################################
# Plot the residual Q-normal graph.
setSweave("graph03", 6, 6)
plot(Boyer.lr, which = "fourier(Date, 2)cos(k=1)", set.up=FALSE)
dev.off()


###################################################
### code chunk number 8: IncorporatingHysteresis.Rnw:131-135
###################################################
# Plot the overall fit, choose plot number 2.
setSweave("graph04", 6, 6)
plot(Boyer.lr, which = 4, set.up=FALSE)
dev.off()


###################################################
### code chunk number 9: IncorporatingHysteresis.Rnw:144-148
###################################################
# Plot the S-L grpah.
setSweave("graph05", 6, 6)
plot(Boyer.lr, which = 3, set.up=FALSE)
dev.off()


###################################################
### code chunk number 10: IncorporatingHysteresis.Rnw:157-161
###################################################
# Plot the residual Q-normal graph.
setSweave("graph06", 6, 6)
plot(Boyer.lr, which = 5, set.up=FALSE)
dev.off()


