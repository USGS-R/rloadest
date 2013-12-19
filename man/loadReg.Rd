\name{loadReg}
\alias{loadReg}
\title{Load Estimation}
\usage{
  loadReg(formula, data, subset, na.action, flow, dates,
    flow.units = "cfs", conc.units = "", load.units = "kg",
    time.step = "day", station = "")
}
\arguments{
  \item{formula}{a formula describing the regression model.
  See \bold{Details}.}

  \item{data}{the data to search for the variables in
  \code{formula}.}

  \item{subset}{an expression to select a subset of the
  data.}

  \item{na.action}{what to do with missing values.}

  \item{flow}{character string indicating the name of the
  flow column.}

  \item{dates}{character string indicating the name of the
  date column.}

  \item{flow.units}{character string describing the flow
  unit.}

  \item{conc.units}{character string describing the
  concentration unit.}

  \item{load.units}{character string describing the load
  unit.}

  \item{time.step}{character string describing the time
  step of the calibration data.}

  \item{station}{character string description of the
  station.}
}
\value{
  An object of class "loadReg."
}
\description{
  Build a rating-curve (regression) model for river load
  estimation.
}
\details{
  The left-hand side of the formula may be any numeric
  variable, just as with \code{lm} or a variable of class
  "lcens," "mcens," or "qw."\cr For un- or left-censored
  data, AMLE is used unless weights are specified in the
  model, then MLE is used, through a call to survreg. For
  any other censored data, MLE is used.
}
\examples{
# From application 1 in the vignettes
data(app1.calib)
app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib,
 flow = "FLOW", dates = "DATES", conc.units="mg/L",
 station="Illinois River at Marseilles, Ill.")
print(app1.lr)
}
\references{
  will need some.
}
\seealso{
  \code{\link{censReg}}
}
\keyword{censored}
\keyword{loads}
\keyword{regression}

