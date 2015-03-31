# Use app1 (model7) from the vignette
library(rloadest)
data(app1.calib)
# construct the model, see the vignette for app1 for details
app1.lr7 <- loadReg(Phosphorus ~ model(7), data = app1.calib, flow = "FLOW",
  dates = "DATES", conc.units="mg/L",
  station="Illinois River at Marseilles, Ill.")
# construct the grids
FLOW <- seq(3000, 60000, length.out=21)
DATES75 <- seq(as.Date("1975-01-01"), as.Date("1976-01-01"), length.out=24)
Tmp75 <- expand.grid(DATES=DATES75, FLOW=FLOW)
Tmp75$C <- predConc(app1.lr7, Tmp75)$Conc
Tmp75$L <- predLoad(app1.lr7, Tmp75, by="day")$Flux
Z.conc75 <- matrix(Tmp75$C, nrow=24)
Z.load75 <- matrix(Tmp75$L, nrow=24)

DATES84 <- seq(as.Date("1984-01-01"), as.Date("1985-01-01"), length.out=24)
Tmp84 <- expand.grid(DATES=DATES84, FLOW=FLOW)
Tmp84$C <- predConc(app1.lr7, Tmp84)$Conc
Tmp84$L <- predLoad(app1.lr7, Tmp84, by="day")$Flux
Z.conc84 <- matrix(Tmp84$C, nrow=24)
Z.load84 <- matrix(Tmp84$L, nrow=24)

# The range in concentration should be from .24 to .92
AA.lev <- seq(.24, .92, by=.02)
# the maximum load is 84,000+, so set range to 100,000
# Construct the 1975 load/concentration surface
# The shape of the surface indicates the potential loading given
# The date and flow. The color of the surface indicates the 
# potential concentration given the date and flow.
preSurface(DATES75, FLOW, Z.load75, zaxis.range = c(0, 100000),
 batch="I") -> AA75.pre
# I selected
# Construct the 1984 load/concentration surface
preSurface(DATES84, FLOW, Z.load84, zaxis.range = c(0, 100000),
 batch="I") -> AA84.pre
# I selected
# Proceed:
#set the page to landscape
setPDF("land", basename="Change_Load")
setLayout(width=c(4.5,4.5), height=5.5, explanation=list(right=1.2)) -> AA.lo
setGraph(1, AA.lo)
surfacePlot(pre=AA75.pre, z.color=Z.conc75, 
						Surface=list(name="Concentration", levels=AA.lev),
						xtitle="1975", ytitle="Streamflow", ztitle="Load") -> AA75.pl
addCaption("Change in Phosphorus Loading in the Illinois River at Marseilles, Ill. from 1975 to 1984")
# Construct the 1984 load/concentration surface
setGraph(2, AA.lo)
surfacePlot(pre=AA84.pre, z.color=Z.conc84, 
						Surface=list(name="Concentration", levels=AA.lev),
						xtitle="1984", ytitle="Streamflow", ztitle="Load") -> AA84.pl
# The 
setGraph("explanation", AA.lo)
addExplanation(AA84.pl)
dev.off()

