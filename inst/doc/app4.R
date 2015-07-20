### R code from vignette source 'app4.Rnw'

###################################################
### code chunk number 1: app4.Rnw:23-27
###################################################
# Load the rloadest package and the data
library(rloadest)
data(app4.calib)
head(app4.calib)


###################################################
### code chunk number 2: app4.Rnw:32-35
###################################################
# Convert Buty and Alach to class "qw"
app4.calib <- convert2qw(app4.calib, scheme="partial")
head(app4.calib)


###################################################
### code chunk number 3: app4.Rnw:43-51
###################################################
# Create the "best" load model.
app4.lr <- selBestModel("Buty", data = app4.calib, flow = "FLOW", 
                   dates = "DATES", conc.units="ug/L",
                   station="White River at Hazleton, Ind.")
# Print the warning in the vignette
warnings()
# Print the results
app4.lr


###################################################
### code chunk number 4: app4.Rnw:64-68
###################################################
# setSweave is required for the vignette.
setSweave("app4_01", 5, 5)
plot(app4.lr, which=1, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 5: app4.Rnw:70-72
###################################################
cat("\\includegraphics{app4_01.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 6: app4.Rnw:80-84
###################################################
# setSweave is required for the vignette.
setSweave("app4_02", 5, 5)
plot(app4.lr, which=4, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 7: app4.Rnw:86-88
###################################################
cat("\\includegraphics{app4_02.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 8: app4.Rnw:96-100
###################################################
# setSweave is required for the vignette.
setSweave("app4_03", 5, 5)
plot(app4.lr, which=5, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 9: app4.Rnw:102-104
###################################################
cat("\\includegraphics{app4_03.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 10: app4.Rnw:112-116
###################################################
# setSweave is required for the vignette.
setSweave("app4_04", 5, 5)
plot(app4.lr, which=6, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 11: app4.Rnw:118-120
###################################################
cat("\\includegraphics{app4_04.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 12: app4.Rnw:134-139
###################################################
# Create the limited regression model.
app4.cr <- censReg(Buty ~ center(log(FLOW)) + dectime(DATES), 
                   data = app4.calib, dist="lognormal")
app4.sp <- seasonalPeak(dectime(app4.calib$DATES), residuals(app4.cr))
app4.sp


###################################################
### code chunk number 13: app4.Rnw:144-151
###################################################
# Show the plot for this example
setSweave("app4_05", 5, 5)
confirm(app4.sp, plot.only=TRUE)
graphics.off()
# Confirm the seasonalPeak analysis for a single peak.
app4.sp <- confirm(app4.sp, all=TRUE)
app4.sp


###################################################
### code chunk number 14: app4.Rnw:153-155
###################################################
cat("\\includegraphics{app4_05.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 15: app4.Rnw:162-169
###################################################
# Add Dectime.
app4.calib <- transform(app4.calib, Dectime=dectime(DATES))
# Find the best model
selBestWave(Buty ~ center(log(FLOW)) + dectime(DATES), 
            data = app4.calib,
            "Dectime", app4.sp, exhaustive=TRUE, Regression=censReg,
            dist="lognormal")


###################################################
### code chunk number 16: app4.Rnw:174-182
###################################################
# Create and print the seasonal-wave load model.
# Note that we can use Dectime directly in this model
app4.lrsw <- loadReg(Buty ~ center(log(FLOW)) + Dectime +
                     seasonalWave(Dectime, 0.393, 1, 1), 
                     data = app4.calib, flow = "FLOW", 
                     dates = "DATES", conc.units="ug/L", 
                     station="White River at Hazleton, Ind.")
app4.lrsw


###################################################
### code chunk number 17: app4.Rnw:190-194
###################################################
# setSweave is required for the vignette.
setSweave("app4_06", 5, 5)
plot(app4.lrsw, which=1, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 18: app4.Rnw:196-198
###################################################
cat("\\includegraphics{app4_06.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 19: app4.Rnw:206-210
###################################################
# setSweave is required for the vignette.
setSweave("app4_07", 5, 5)
plot(app4.lrsw, which=4, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 20: app4.Rnw:212-214
###################################################
cat("\\includegraphics{app4_07.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 21: app4.Rnw:222-226
###################################################
# setSweave is required for the vignette.
setSweave("app4_08", 5, 5)
plot(app4.lrsw, which=5, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 22: app4.Rnw:228-230
###################################################
cat("\\includegraphics{app4_08.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 23: app4.Rnw:238-242
###################################################
# setSweave is required for the vignette.
setSweave("app4_09", 5, 5)
plot(app4.lrsw, which=6, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 24: app4.Rnw:244-246
###################################################
cat("\\includegraphics{app4_09.pdf}\n")
cat("\\paragraph{}\n")


