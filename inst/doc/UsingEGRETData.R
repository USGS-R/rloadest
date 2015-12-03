### R code from vignette source 'UsingEGRETData.Rnw'

###################################################
### code chunk number 1: UsingEGRETData.Rnw:23-30
###################################################
# Load the necessary packages and the data
library(survival) # required for Surv
library(rloadest)
library(EGRET)
# Get the QW and daily flow data
Chop.QW <- Choptank_eList$Sample
Chop.Q <- Choptank_eList$Daily


###################################################
### code chunk number 2: UsingEGRETData.Rnw:39-43
###################################################
# Compute the 7-parameter model.
Chop.lr <- loadReg(Surv(ConcLow, ConcHigh, type="interval2") ~ model(9),
  data=Chop.QW, flow="Q", dates="Date", conc.units="mg/L",
  flow.units="cms", station="Choptank")


###################################################
### code chunk number 3: UsingEGRETData.Rnw:48-52
###################################################
# Plot the overall fit
setSweave("graph01", 6, 6)
plot(Chop.lr, which=1, set.up=FALSE)
dev.off()


###################################################
### code chunk number 4: UsingEGRETData.Rnw:60-79
###################################################
# Plot the explanatory variable fits
setSweave("graph02", 6, 9)
AA.lo <- setLayout(num.rows=3, num.cols=2)
# Flow and flow squared
setGraph(1, AA.lo)
plot(Chop.lr, which="lnQ", set.up=FALSE)
setGraph(2, AA.lo)
plot(Chop.lr, which="lnQ2", set.up=FALSE)
# Time and time squared
setGraph(3, AA.lo)
plot(Chop.lr, which="DECTIME", set.up=FALSE)
setGraph(4, AA.lo)
plot(Chop.lr, which="DECTIME2", set.up=FALSE)
# Seasonality
setGraph(5, AA.lo)
plot(Chop.lr, which="sin.DECTIME", set.up=FALSE)
setGraph(6, AA.lo)
plot(Chop.lr, which="cos.DECTIME", set.up=FALSE)
dev.off()


###################################################
### code chunk number 5: UsingEGRETData.Rnw:88-93
###################################################
# Plot tconcentration and flow
setSweave("graph03", 6, 6)
# Use the average concentration (only one censored value)
with(Chop.QW, xyPlot(Q, ConcAve, yaxis.log=TRUE, xaxis.log=TRUE))
dev.off()


###################################################
### code chunk number 6: UsingEGRETData.Rnw:105-119
###################################################
# Compute the breakpoint--the seg term must be the first term on 
# the right-hand side.
Chop.lr <- segLoadReg(ConcAve ~ seg(LogQ, 1) + DecYear + 
    fourier(DecYear, 2),
  data=Chop.QW, flow="Q", dates="Date", conc.units="mg/L",
  flow.units="cms", station="Choptank")
# From the printed output, the breakpoint is 1.994 in natural log units, 
# about 7.4 cms
# Compute and print the final model
Chop.lr <- loadReg(Surv(ConcLow, ConcHigh, type="interval2") ~ 
    segment(LogQ, 1.994) + DecYear + fourier(DecYear, 2),
  data=Chop.QW, flow="Q", dates="Date", conc.units="mg/L",
  flow.units="cms", station="Choptank")
print(Chop.lr)


###################################################
### code chunk number 7: UsingEGRETData.Rnw:126-143
###################################################
# Plot the explanatory variable fits
setSweave("graph04", 6, 9)
AA.lo <- setLayout(num.rows=3, num.cols=2)
# Segmented flow
setGraph(1, AA.lo)
plot(Chop.lr, which="segment(LogQ, 1.994)X", set.up=FALSE)
setGraph(2, AA.lo)
plot(Chop.lr, which="segment(LogQ, 1.994)U1", set.up=FALSE)
# Time 
setGraph(3, AA.lo)
plot(Chop.lr, which="DecYear", set.up=FALSE)
# Seasonality
setGraph(5, AA.lo)
plot(Chop.lr, which="fourier(DecYear, 2)sin(k=2)", set.up=FALSE)
setGraph(6, AA.lo)
plot(Chop.lr, which="fourier(DecYear, 2)cos(k=2)", set.up=FALSE)
dev.off()


###################################################
### code chunk number 8: UsingEGRETData.Rnw:157-171
###################################################
# Compute the mean residual and flow by water year
MeanRes <- tapply(residuals(Chop.lr), waterYear(Chop.QW$Date), mean)
MeanQ <- with(Chop.Q, tapply(LogQ, waterYear(Date), mean))
# Get the years and convert the means to scaled vectors (for plotting)
MeanWY <- as.integer(names(MeanQ))
MeanRes <- as.vector(scale(MeanRes))
MeanQ <- as.vector(scale(MeanQ))
# Plot them
setSweave("graph05", 6, 6)
AA.pl <- timePlot(MeanWY, MeanRes, Plot=list(what="overlaid"),
  yaxis.range=c(-2.5, 2.5))
addXY(MeanWY, MeanQ, Plot=list(what="overlaid", color="red"),
  current=AA.pl)
dev.off()


###################################################
### code chunk number 9: UsingEGRETData.Rnw:180-195
###################################################
# Retrieve the flow data , beginning 1978-10-01, and compute log flowe in cms
Chop.ExQ <- renameNWISColumns(readNWISdv(
  "01491000", "00060", "1978-10-01", "2011-09-30"))
Chop.ExQ$LogQ <- log(Chop.ExQ$Flow/35.31467)
# Compute the Dependencies
Chop.ExQ <- transform(Chop.ExQ, 
  Dep3mo=movingAve(LogQ, 91, pos="trailing"),
  Dep6mo=movingAve(LogQ, 182, pos="trailing"),
  Dep9mo=movingAve(LogQ, 273, pos="trailing"),
  Dep12mo=movingAve(LogQ, 365, pos="trailing"))
# Merge the dependencies into the calibration dataset
Chop.ExQW <- mergeQ(Chop.QW, FLOW=c("Dep3mo", "Dep6mo", "Dep9mo", "Dep12mo"),
  DATES="Date", Qdata=Chop.ExQ, Plot=FALSE)
# Which has the largest correlation?
cor(residuals(Chop.lr), Chop.ExQW[c("Dep3mo", "Dep6mo", "Dep9mo", "Dep12mo")])


###################################################
### code chunk number 10: UsingEGRETData.Rnw:200-207
###################################################
# Compute the Hysterisis and merge into the new calibration data set
Chop.ExQ <- transform(Chop.ExQ, Hy1=hysteresis(LogQ, 1))
# Merge into the calibration dataset
Chop.ExQW <- mergeQ(Chop.ExQW, FLOW="Hy1",
  DATES="Date", Qdata=Chop.ExQ, Plot=FALSE)
# Is it correlated with the residuals?
cor(residuals(Chop.lr), Chop.ExQW$Hy1)


###################################################
### code chunk number 11: UsingEGRETData.Rnw:253-269
###################################################
# Compute the extended load regression exluding time
Chop.lrEx <- loadReg(Surv(ConcLow, ConcHigh, type="interval2") ~ 
    segment(LogQ, 1.994) + 
    fourier(DecYear, 2) + Hy1 + Dep3mo,
  data=Chop.ExQW, flow="Q", dates="Date", conc.units="mg/L",
  flow.units="cms", station="Choptank")
# Use the functions in smwrGraphs to easily add the smooth line
# Plot the residuals over time (decimal year) 
# zooming in to the bulk of the residuals
setSweave("graph06", 6, 6)
AA.pl <- xyPlot(Chop.QW$DecYear, residuals(Chop.lrEx),
  xtitle="Decimal Time", ytitle="Partial Residual",
  yaxis.range=c(-1,1))
# Add a smmothed line, setting family to "gaussian"--better for regression
addSmooth(AA.pl, family="gaussian")
dev.off()


###################################################
### code chunk number 12: UsingEGRETData.Rnw:279-286
###################################################
# Compute and print the Extended model
Chop.lrEx <- loadReg(Surv(ConcLow, ConcHigh, type="interval2") ~ 
    segment(LogQ, 1.994) + trends(DecYear, c(1991, 1998)) + 
    fourier(DecYear, 2) + Hy1 + Dep3mo,
  data=Chop.ExQW, flow="Q", dates="Date", conc.units="mg/L",
  flow.units="cms", station="Choptank")
print(Chop.lrEx)


###################################################
### code chunk number 13: UsingEGRETData.Rnw:294-321
###################################################
# Compute the WRTDS residuals and the water year
Chop.QW <- transform(Chop.QW, Res=log(ConcAve) - yHat,
                     WY=waterYear(Date, numeric=FALSE))
# Graph the residuals
setSweave("graph07", 6, 9)
AA.lo <- setLayout(num.rows=3, shared.x=1)
# The WRTDS residuals over time
AA.mr <- setGraph(1, AA.lo)
with(Chop.QW, boxPlot(Res, group=WY, Box=list(show.counts=FALSE),
  yaxis.range=c(-1,1), xlabels.rotate=TRUE, margin=AA.mr))
refLine(horizontal=0)
addTitle(Main="WRTDS", Position="inside", Bold=FALSE)
# Modified residuals over time
AA.mr <- setGraph(2, AA.lo)
with(Chop.QW, boxPlot(residuals(Chop.lr), group=WY, 
  Box=list(show.counts=FALSE),
  yaxis.range=c(-1,1), xlabels.rotate=TRUE, margin=AA.mr))
refLine(horizontal=0)
addTitle(Main="Modified", Position="inside", Bold=FALSE)
# Extended residuals over time
AA.mr <- setGraph(3, AA.lo)
with(Chop.QW, boxPlot(residuals(Chop.lrEx), group=WY, 
  Box=list(show.counts=FALSE),
  yaxis.range=c(-1,1), xlabels.rotate=TRUE, margin=AA.mr))
refLine(horizontal=0)
addTitle(Main="Extended", Position="inside", Bold=FALSE)
dev.off()


