\name{selBestModel}
\alias{selBestModel}
\title{Load Estimation}
\usage{
selBestModel(constituent, data, subset, na.action, flow, dates,
  flow.units = "cfs", conc.units = "", load.units = "kg",
  time.step = "day", station = "")
}
\arguments{
  \item{constituent}{a character string giving the name of
  the response variable for which loads are to be
  computed.}

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
Select the "best" predefined rating-curve (regression)
model for river load estimation.
}
\examples{
# Use the data from application 1 in the vignettes
data(app1.calib)
app1.lr <- selBestModel("Phosphorus", data = app1.calib,
 flow = "FLOW", dates = "DATES", conc.units="mg/L",
 station="Illinois River at Marseilles, Ill.")
# Extract the fitted values
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

