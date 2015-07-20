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


