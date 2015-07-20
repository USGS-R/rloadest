### R code from vignette source 'app1.Rnw'

###################################################
### code chunk number 1: app1.Rnw:23-27
###################################################
# Load the rloadest package and the data
library(rloadest)
data(app1.calib)
head(app1.calib)


###################################################
### code chunk number 2: app1.Rnw:37-41
###################################################
# Create the load model.
app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib, flow = "FLOW", 
                   dates = "DATES", conc.units="mg/L",
                   station="Illinois River at Marseilles, Ill.")


###################################################
### code chunk number 3: app1.Rnw:49-50
###################################################
print(app1.lr, brief=FALSE, load.only=FALSE)


###################################################
### code chunk number 4: app1.Rnw:78-80
###################################################
predLoad(app1.lr, newdata = app1.calib, load.units="tons", by="total",
         print=TRUE)


###################################################
### code chunk number 5: app1.Rnw:89-93
###################################################
# setSweave is required for the vignette.
setSweave("app1_01", 5, 5)
plot(app1.lr, which=1, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 6: app1.Rnw:95-97
###################################################
cat("\\includegraphics{app1_01.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 7: app1.Rnw:104-108
###################################################
# setSweave is required for the vignette.
setSweave("app1_02", 5, 5)
plot(app1.lr, which=2, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 8: app1.Rnw:110-112
###################################################
cat("\\includegraphics{app1_02.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 9: app1.Rnw:119-123
###################################################
# setSweave is required for the vignette.
setSweave("app1_03", 5, 5)
plot(app1.lr, which=3, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 10: app1.Rnw:125-127
###################################################
cat("\\includegraphics{app1_03.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 11: app1.Rnw:134-138
###################################################
# setSweave is required for the vignette.
setSweave("app1_04", 5, 5)
plot(app1.lr, which=4, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 12: app1.Rnw:140-142
###################################################
cat("\\includegraphics{app1_04.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 13: app1.Rnw:149-153
###################################################
# setSweave is required for the vignette.
setSweave("app1_05", 5, 5)
plot(app1.lr, which=5, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 14: app1.Rnw:155-157
###################################################
cat("\\includegraphics{app1_05.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 15: app1.Rnw:164-168
###################################################
# setSweave is required for the vignette.
setSweave("app1_06", 5, 5)
plot(app1.lr, which=6, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 16: app1.Rnw:170-172
###################################################
cat("\\includegraphics{app1_06.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 17: app1.Rnw:181-190
###################################################
# Create and print the revised load model.
app1.lr7 <- loadReg(Phosphorus ~ model(7), data = app1.calib, flow = "FLOW", 
                   dates = "DATES", conc.units="mg/L",
                   station="Illinois River at Marseilles, Ill.")
print(app1.lr7)
# setSweave is required for the vignette.
setSweave("app1_07", 5, 5)
plot(app1.lr7, which=4, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 18: app1.Rnw:192-194
###################################################
cat("\\includegraphics{app1_07.pdf}\n")
cat("\\paragraph{}\n")


