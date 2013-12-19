\name{app1.calib}
\alias{app1.calib}
\docType{data}
\title{
app1.calib Data
}
\description{
  Illinois River at Marseilles, Illinois (Helsel & Hirsch, 2002)
}
\usage{app1.calib}
\format{
Data frame with 96 rows and 4 columns\cr
\tabular{lll}{
 Name \tab Type \tab Description\cr
 \cr
 DATES \tab Date \tab Date of daily value\cr
 TIMES \tab character \tab Time corresponding to noon of daily value\cr
 FLOW \tab numeric \tab Daily mean streamflow\cr
 Phosphorus \tab numeric \tab Daily mean phosphorus concentration (assumed)\cr
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
data(app1.calib)
# Plot concentration vs. flow
with(app1.calib, plot(FLOW, Phosphorus, log="xy"))
}}
\keyword{datasets}
