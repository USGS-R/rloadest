### R code from vignette source 'app6.Rnw'

###################################################
### code chunk number 1: app6.Rnw:21-25
###################################################
# Load the rloadest package and the data
library(rloadest)
data(app5.calib)
head(app5.calib)


###################################################
### code chunk number 2: app6.Rnw:33-40
###################################################
# Create the and print load model with concentration.
app6.lr <- loadReg(Alkalinity ~ quadratic(log(FLOW)) + log(SC) +
                       fourier(DATES),
                     data = app5.calib, subset=DATES < "1998-01-01",
                     flow = "FLOW", dates = "DATES", conc.units="mg/L",
                     station="Arkansas River at Halstead, Ks.")
print(app6.lr, load.only=FALSE)


###################################################
### code chunk number 3: app6.Rnw:45-50
###################################################
# Get the estimation data
data(app5.est)
# Predict daily concentrations
app6.cd <- predConc(app6.lr, app5.est, by="day") 
head(app6.cd)


###################################################
### code chunk number 4: app6.Rnw:61-67
###################################################
# Create synthetic flow values 
app6.calib <- transform(app5.calib, Sflow=1/c2load(1, 1, 
   conc.units="mg/L"))
app6.est <- transform(app5.est, Sflow=1/c2load(1, 1, 
   conc.units="mg/L"))
head(app6.calib)


###################################################
### code chunk number 5: app6.Rnw:72-79
###################################################
# Create the and print load model.
app6.lrTWM <- loadReg(Alkalinity ~ quadratic(log(FLOW)) + log(SC) +
                       fourier(DATES),
                     data = app6.calib, subset=DATES < "1998-01-01",
                     flow = "Sflow", dates = "DATES", conc.units="mg/L",
                     station="Arkansas River at Halstead, Ks.")
print(app6.lrTWM)


###################################################
### code chunk number 6: app6.Rnw:84-89
###################################################
# Predict monthly TWM concentrations using the \textt{predLoad} function.
app6.TWM <- predLoad(app6.lrTWM, app6.est, by="month") 
# Change the name of the Flux column to Conc
names(app6.TWM)[3] <- "Conc"
app6.TWM


###################################################
### code chunk number 7: app6.Rnw:94-95
###################################################
with(app6.cd, tapply(Conc, month(Date, label=TRUE), mean))


###################################################
### code chunk number 8: app6.Rnw:104-112
###################################################
# Compute the monthly fluxes. 
app6.FWM <- predLoad(app6.lr, app6.est, by="month")
# Compute the mean flows
app6.FWM$Flow <- as.vector(with(app6.est, tapply(FLOW, month(DATES), mean)))
# Compute the FWM concentration
app6.FWM <- transform(app6.FWM, FWMC=Flux/Flow/
                        c2load(1, 1, conc.units="mg/L"))
app6.FWM


