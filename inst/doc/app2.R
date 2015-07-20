### R code from vignette source 'app2.Rnw'

###################################################
### code chunk number 1: app2.Rnw:29-38
###################################################
# Load the rloadest package and the data
library(rloadest)
data(app2.calib)
head(app2.calib)
# Plot the seasonal pattern of Atrazine
# setSweave is required for the vignette.
setSweave("app2_01", 5, 5)
with(app2.calib, seasonPlot(DATES, Atrazine, yaxis.log=TRUE))
graphics.off()


###################################################
### code chunk number 2: app2.Rnw:40-42
###################################################
cat("\\includegraphics{app2_01.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 3: app2.Rnw:51-54
###################################################
# Add Period to the calibration data.
app2.calib <- transform(app2.calib, Period=seasons(DATES, 
                        breaks=c("Apr", "Jul")))


###################################################
### code chunk number 4: app2.Rnw:61-69
###################################################
# Create and print the load model.
app2.lr <- loadReg(Atrazine ~ Period*center(log(FLOW)), data = app2.calib,
                   flow = "FLOW", dates = "DATES", conc.units="ug/L", 
                   load.units="pounds",
                   station="St.Joseph River near Newville, Ind.")
# Warnings are not printed in the vignette
warnings()
app2.lr


###################################################
### code chunk number 5: app2.Rnw:77-81
###################################################
# Load the estimation data and add Period
data(app2.est)
app2.est <- transform(app2.est, Period=seasons(DATES, 
                      breaks=c("Apr", "Jul")))


###################################################
### code chunk number 6: app2.Rnw:94-96
###################################################
predLoad(app2.lr, newdata = app2.est, by="total",
         print=TRUE)


###################################################
### code chunk number 7: app2.Rnw:100-102
###################################################
app2.est <- transform(app2.est, Season=seasons(DATES, 
                      breaks=c("Jan", "Apr", "Jul", "Oct")))


###################################################
### code chunk number 8: app2.Rnw:106-108
###################################################
app2.seas <- predLoad(app2.lr, newdata = app2.est, by="Season")
app2.seas


###################################################
### code chunk number 9: app2.Rnw:114-118
###################################################
# setSweave is required for the vignette.
setSweave("app2_02", 5, 5)
plot(app2.lr, which=1, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 10: app2.Rnw:120-122
###################################################
cat("\\includegraphics{app2_02.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 11: app2.Rnw:129-133
###################################################
# setSweave is required for the vignette.
setSweave("app2_03", 5, 5)
plot(app2.lr, which=3, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 12: app2.Rnw:135-137
###################################################
cat("\\includegraphics{app2_03.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 13: app2.Rnw:144-148
###################################################
# setSweave is required for the vignette.
setSweave("app2_04", 5, 5)
plot(app2.lr, which=4, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 14: app2.Rnw:150-152
###################################################
cat("\\includegraphics{app2_04.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 15: app2.Rnw:159-163
###################################################
# setSweave is required for the vignette.
setSweave("app2_05", 5, 5)
plot(app2.lr, which=5, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 16: app2.Rnw:165-167
###################################################
cat("\\includegraphics{app2_05.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 17: app2.Rnw:181-185
###################################################
# Create the limited regression model.
app2.lm <- lm(log(Atrazine) ~ center(log(FLOW)), data = app2.calib)
app2.sp <- seasonalPeak(dectime(app2.calib$DATES), residuals(app2.lm))
app2.sp


###################################################
### code chunk number 18: app2.Rnw:190-197
###################################################
# Show the plot for this example
setSweave("app2_06", 5, 5)
confirm(app2.sp, plot.only=TRUE)
graphics.off()
# Confirm the seasonalPeak analysis for a single peak.
app2.sp <- confirm(app2.sp, all=TRUE)
app2.sp


###################################################
### code chunk number 19: app2.Rnw:199-201
###################################################
cat("\\includegraphics{app2_06.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 20: app2.Rnw:208-213
###################################################
# Add Dectime.
app2.calib <- transform(app2.calib, Dectime=dectime(DATES))
# Find the best model
selBestWave(log(Atrazine) ~ center(log(FLOW)), data = app2.calib,
            "Dectime", app2.sp, exhaustive=TRUE)


###################################################
### code chunk number 21: app2.Rnw:218-226
###################################################
# Create and print the seasonal-wave load model.
app2.lrsw <- loadReg(Atrazine ~ center(log(FLOW)) +
                       seasonalWave(Dectime, 0.45, 2, 3), 
                     data = app2.calib, flow = "FLOW", 
                     dates = "DATES", conc.units="ug/L", 
                     load.units="pounds",
                     station="St.Joseph River near Newville, Ind.")
app2.lrsw


###################################################
### code chunk number 22: app2.Rnw:231-237
###################################################
# Use colorPlot to show the relation by season
setSweave("app2_07", 5, 5)
AA.pl <- with(app2.calib, colorPlot(FLOW, Atrazine, color=Period, 
        yaxis.log=TRUE, xaxis.log=TRUE))
addExplanation(AA.pl, "ul", title="Period")
graphics.off()


###################################################
### code chunk number 23: app2.Rnw:239-241
###################################################
cat("\\includegraphics{app2_07.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 24: app2.Rnw:248-250
###################################################
subset(app2.calib, Period=="Season Ending Jul" & Atrazine < .7)
subset(app2.calib, Period=="Season Ending Apr" & Atrazine > .7)


