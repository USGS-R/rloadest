\name{app5.calib}
\alias{app5.calib}
\docType{data}
\title{
app5.calib Data
}
\description{
  Little Arkansas River near Halstead, Kansas (Station Number 07143672)
}
\usage{app5.calib}
\format{
Data frame with 103 rows and 5 columns\cr
\tabular{lll}{
 Name \tab Type \tab Description\cr
 \cr
 DATES \tab Date \tab Date of daily value\cr
 TIMES \tab character \tab Time that sample was actually taken\cr
 FLOW \tab numeric \tab Daily mean streamflow\cr
 SC \tab numeric \tab Daily mean specific conductance\cr
 Alkalinity \tab numeric \tab Daily mean alkalinity (assumed)\cr
 }
}
\source{
Example calibration dataset from LOADEST
}
\references{
Runkel, R.G., Crawford, C.G., and Cohn, T.A., 2004, Load Estimator (LOADEST):
a FORTRAN program for estimating constituent loads in streams and rivers:
U.S. Geological Survey Techniques and Methods Book 4, Chapter A5, 69 p.
}
\examples{
\dontrun{
data(app5.calib)
# Plot concentration vs. specific conductance
with(app5.calib, plot(SC, Alkalinity, log="xy"))
}}
\keyword{datasets}
