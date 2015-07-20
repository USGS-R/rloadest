### R code from vignette source 'app5.Rnw'

###################################################
### code chunk number 1: app5.Rnw:24-28
###################################################
# Load the rloadest package and the data
library(rloadest)
data(app5.calib)
head(app5.calib)


###################################################
### code chunk number 2: app5.Rnw:38-43
###################################################
# Create the and print load model.
app5.lr <- loadReg(Alkalinity ~ log(FLOW) + log(SC), data = app5.calib, 
                   flow = "FLOW", dates = "DATES", conc.units="mg/L",
                   station="Arkansas River at Halstead, Ks.")
app5.lr


###################################################
### code chunk number 3: app5.Rnw:53-57
###################################################
# setSweave is required for the vignette.
setSweave("app5_01", 5, 5)
plot(app5.lr, which=1, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 4: app5.Rnw:59-61
###################################################
cat("\\includegraphics{app5_01.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 5: app5.Rnw:69-73
###################################################
# setSweave is required for the vignette.
setSweave("app5_02", 5, 5)
plot(app5.lr, which=3, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 6: app5.Rnw:75-77
###################################################
cat("\\includegraphics{app5_02.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 7: app5.Rnw:85-89
###################################################
# setSweave is required for the vignette.
setSweave("app5_03", 5, 5)
plot(app5.lr, which=4, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 8: app5.Rnw:91-93
###################################################
cat("\\includegraphics{app5_03.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 9: app5.Rnw:101-105
###################################################
# setSweave is required for the vignette.
setSweave("app5_04", 5, 5)
plot(app5.lr, which=5, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 10: app5.Rnw:107-109
###################################################
cat("\\includegraphics{app5_04.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 11: app5.Rnw:118-122
###################################################
# setSweave is required for the vignette.
setSweave("app5_05", 5, 5)
plot(app5.lr, which="log(FLOW)", span=0.5, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 12: app5.Rnw:124-126
###################################################
cat("\\includegraphics{app5_05.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 13: app5.Rnw:136-146
###################################################
# Create the revised load model and plot the correlogram.
app5.lrR1 <- loadReg(Alkalinity ~ quadratic(log(FLOW)) + log(SC),
                     data = app5.calib, 
                     flow = "FLOW", dates = "DATES", conc.units="mg/L",
                     station="Arkansas River at Halstead, Ks.")

# setSweave is required for the vignette.
setSweave("app5_06", 5, 5)
plot(app5.lrR1, which=4, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 14: app5.Rnw:148-150
###################################################
cat("\\includegraphics{app5_06.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 15: app5.Rnw:162-170
###################################################
# setSweave is required for the vignette.
setSweave("app5_07", 5, 5)
AA.pl <- with(app5.calib, xyPlot(FLOW, Alkalinity, yaxis.log=TRUE,
                                 xaxis.log=TRUE))
with(subset(app5.calib, DATES > "1998-01-01" & DATES < "1998-05-01"), 
     addXY(FLOW, Alkalinity,Plot=list(what="points", color="red"),
           current=AA.pl))
graphics.off()


###################################################
### code chunk number 16: app5.Rnw:172-174
###################################################
cat("\\includegraphics{app5_07.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 17: app5.Rnw:184-195
###################################################
# Create, print the revised load model and plot the correlogram.
app5.lrR2 <- loadReg(Alkalinity ~ quadratic(log(FLOW)) + log(SC) +
                       fourier(DATES),
                     data = app5.calib, subset=DATES < "1998-01-01",
                     flow = "FLOW", dates = "DATES", conc.units="mg/L",
                     station="Arkansas River at Halstead, Ks.")
app5.lrR2
# setSweave is required for the vignette.
setSweave("app5_08", 5, 5)
plot(app5.lrR2, which=4, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 18: app5.Rnw:197-199
###################################################
cat("\\includegraphics{app5_08.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 19: app5.Rnw:208-212
###################################################
# setSweave is required for the vignette.
setSweave("app5_09", 5, 5)
plot(app5.lrR2, which=1, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 20: app5.Rnw:214-216
###################################################
cat("\\includegraphics{app5_09.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 21: app5.Rnw:224-228
###################################################
# setSweave is required for the vignette.
setSweave("app5_10", 5, 5)
plot(app5.lrR2, which=3, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 22: app5.Rnw:230-232
###################################################
cat("\\includegraphics{app5_10.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 23: app5.Rnw:240-244
###################################################
# setSweave is required for the vignette.
setSweave("app5_11", 5, 5)
plot(app5.lrR2, which=5, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 24: app5.Rnw:246-248
###################################################
cat("\\includegraphics{app5_11.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 25: app5.Rnw:260-285
###################################################
# Get the estimation data
data(app5.est)
# Predict daily loads for 1999
app5.ld <- predLoad(app5.lrR2, app5.est, by="day", 
                    load.units="pounds")
# Get the 1999 sample data and merge to get daily flows and compute 
# load, note that the units must match what was selected for
# estimation! The mergeQ function is in the USGSwsBase package;
# the default names for dates and flow agree with the current datasets.
data(app5.1999)
app5.1999 <- mergeQ(app5.1999, Qdata=app5.est, Plot=FALSE)
app5.1999$Load <- with(app5.1999, c2load(Alkalinity, FLOW, 
                                          conc.units="mg/L",
                                          load.units="pounds"))
# Create the graph
setSweave("app5_12", 5, 5)
AA.pl <- with(app5.ld, timePlot(Date, Flux, 
  Plot=list(name="Daily load estimate", what="overlaid",
            size=0.03),
  ytitle="Alkanity Load, in pounds per day"))
AA.pl <- with(app5.1999, addXY(DATES, Load, 
  Plot=list(name="Observed load", what="points"),
  current=AA.pl))
addExplanation(AA.pl, where="ur", title="")
graphics.off()


###################################################
### code chunk number 26: app5.Rnw:287-289
###################################################
cat("\\includegraphics{app5_12.pdf}\n")
cat("\\paragraph{}\n")


