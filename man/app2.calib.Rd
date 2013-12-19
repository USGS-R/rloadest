\name{app2.calib}
\alias{app2.calib}
\docType{data}
\title{
app2.calib Data
}
\description{
  St.Joseph River near Newville, IN (Station # 04178000)
}
\usage{app2.calib}
\format{
Data frame with 32 rows and 4 columns\cr
\tabular{lll}{
 Name \tab Type \tab Description\cr
 \cr
 DATES \tab Date \tab Date of daily value\cr
 TIMES \tab integer \tab Time that sample was actually taken\cr
 FLOW \tab numeric \tab Daily mean streamflow\cr
 Atrazine \tab numeric \tab Daily mean atrazine concentration (assumed)\cr
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
data(app2.calib)
# Plot concentration vs. flow
with(app2.calib, plot(FLOW, Atrazine, log="xy"))
}}
\keyword{datasets}
