### R code from vignette source 'app3.Rnw'

###################################################
### code chunk number 1: app3.Rnw:39-47
###################################################
# Load the rloadest package, which requires the USGSwsQW and
# other packages that contain the necessary functions
library(dataRetrieval)
library(rloadest)
app3.qw <- importNWISqw("01646580", params="00660", 
    begin.date="2001-10-01", end.date="2010-09-30")
app3.flow <- renameNWISColumns(readNWISdv("01646502", "00060",
    startDate="2001-10-01", endDate="2010-09-30"))


###################################################
### code chunk number 2: app3.Rnw:52-62
###################################################
# There are duplicated samples in this dataset. Print them
subset(app3.qw, sample_dt %in% 
  app3.qw[duplicated(app3.qw$sample_dt), "sample_dt"])
# Remove the duplicates
app3.qw <- subset(app3.qw, !duplicated(sample_dt))
# Now change the date column name and merge
names(app3.qw)[2] <- "Date"
# Supress the plot in this merge
app3.calib <- mergeQ(app3.qw, FLOW="Flow", DATES="Date", 
                     Qdata=app3.flow, Plot=FALSE)


###################################################
### code chunk number 3: app3.Rnw:70-75
###################################################
# Create and print the load model.
app3.lr <- loadReg(OrthoPhosphate.PO4 ~ model(9), data = app3.calib, 
  flow = "Flow", dates = "Date",
  station="Potomac River at Chain Bridge, at Washington, DC")
print(app3.lr)


###################################################
### code chunk number 4: app3.Rnw:84-88
###################################################
# setSweave is required for the vignette.
setSweave("app3_01", 5, 5)
plot(app3.lr, which=1, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 5: app3.Rnw:90-92
###################################################
cat("\\includegraphics{app3_01.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 6: app3.Rnw:99-103
###################################################
# setSweave is required for the vignette.
setSweave("app3_02", 5, 5)
plot(app3.lr, which=3, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 7: app3.Rnw:105-107
###################################################
cat("\\includegraphics{app3_02.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 8: app3.Rnw:114-118
###################################################
# setSweave is required for the vignette.
setSweave("app3_03", 5, 5)
plot(app3.lr, which=4, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 9: app3.Rnw:120-122
###################################################
cat("\\includegraphics{app3_03.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 10: app3.Rnw:129-133
###################################################
# setSweave is required for the vignette.
setSweave("app3_04", 5, 5)
plot(app3.lr, which=5, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 11: app3.Rnw:135-137
###################################################
cat("\\includegraphics{app3_04.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 12: app3.Rnw:144-148
###################################################
# setSweave is required for the vignette.
setSweave("app3_05", 5, 5)
plot(app3.lr, which="DECTIME", set.up=FALSE)
graphics.off()


###################################################
### code chunk number 13: app3.Rnw:150-152
###################################################
cat("\\includegraphics{app3_05.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 14: app3.Rnw:161-168
###################################################
# setSweave is required for the vignette.
setSweave("app3_06", 5, 5)
timePlot(app3.calib$Date, residuals(app3.lr),
         Plot=list(what="points"), ytitle="Residuals")
refLine(horizontal=0)
refLine(vertical=as.Date("2001-10-01") + years(0:9))
graphics.off()


###################################################
### code chunk number 15: app3.Rnw:170-172
###################################################
cat("\\includegraphics{app3_06.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 16: app3.Rnw:179-181
###################################################
mean(app3.flow$Flow)
with(app3.flow, tapply(Flow, waterYear(Date), mean))


###################################################
### code chunk number 17: app3.Rnw:195-201
###################################################
app3.anom <- renameNWISColumns(readNWISdv("01646502", "00060",
  startDate="1999-10-01", endDate="2010-09-30"))
app3.anom <- cbind(app3.anom, anomalies(log(app3.anom$Flow), 
  a1yr=365))
# The head would show missing values for a1yr and HFV
tail(app3.anom)


###################################################
### code chunk number 18: app3.Rnw:206-210
###################################################
# Supress the plot in this merge and overwrite app3.calib
app3.calib <- mergeQ(app3.qw, FLOW=c("Flow", "a1yr", "HFV"),
                     DATES="Date", 
                     Qdata=app3.anom, Plot=FALSE)


###################################################
### code chunk number 19: app3.Rnw:215-221
###################################################
app3.lra <- loadReg(OrthoPhosphate.PO4 ~ a1yr + HFV + dectime(Date)
                    + fourier(Date),
                    data = app3.calib, 
                   flow = "Flow", dates = "Date",
                   station="Potomac River at Chain Bridge, at Washington, DC")
print(app3.lra)


###################################################
### code chunk number 20: app3.Rnw:229-233
###################################################
# setSweave is required for the vignette.
setSweave("app3_07", 5, 5)
plot(app3.lra, which=1, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 21: app3.Rnw:235-237
###################################################
cat("\\includegraphics{app3_07.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 22: app3.Rnw:244-248
###################################################
# setSweave is required for the vignette.
setSweave("app3_08", 5, 5)
plot(app3.lra, which=3, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 23: app3.Rnw:250-252
###################################################
cat("\\includegraphics{app3_08.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 24: app3.Rnw:259-263
###################################################
# setSweave is required for the vignette.
setSweave("app3_09", 5, 5)
plot(app3.lra, which=4, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 25: app3.Rnw:265-267
###################################################
cat("\\includegraphics{app3_09.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 26: app3.Rnw:274-278
###################################################
# setSweave is required for the vignette.
setSweave("app3_10", 5, 5)
plot(app3.lra, which=5, set.up=FALSE)
graphics.off()


###################################################
### code chunk number 27: app3.Rnw:280-282
###################################################
cat("\\includegraphics{app3_10.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 28: app3.Rnw:289-293
###################################################
# setSweave is required for the vignette.
setSweave("app3_11", 5, 5)
plot(app3.lra, which="dectime(Date)", set.up=FALSE)
graphics.off()


###################################################
### code chunk number 29: app3.Rnw:295-297
###################################################
cat("\\includegraphics{app3_11.pdf}\n")
cat("\\paragraph{}\n")


###################################################
### code chunk number 30: app3.Rnw:307-310
###################################################
app3.est <- subset(app3.anom, Date > as.Date("2001-09-30"))
predLoad(app3.lra, newdata = app3.est, by="water year",
         print=TRUE)


