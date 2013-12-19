\name{app4.calib}
\alias{app4.calib}
\docType{data}
\title{
app4.calib Data
}
\description{
  White River at Hazleton, Ind. (Station Number 03374100)
}
\usage{app4.calib}
\format{
Data frame with 45 rows and 9 columns\cr
\tabular{lll}{
 Name \tab Type \tab Description\cr
 \cr
 DATES \tab Date \tab Date of daily value\cr
 TIMES \tab character \tab Time that sample was actually taken\cr
 FLOW \tab numeric \tab Daily mean streamflow\cr
 Buty.rmk \tab character \tab Remark code for butylate concentration\cr
 Buty \tab numeric \tab Daily mean butylate concentration (assumed)\cr
 Atra \tab numeric \tab Daily mean atrazine concentration (assumed)\cr
 Alach.rmk \tab character \tab Remark code for alachlor concentration\cr
 Alach \tab numeric \tab Daily mean alachlor concentration (assumed)\cr
 SuspSed \tab numeric \tab Daily mean suspended sediment concentration (assumed)\cr
 }
}
\source{
Obtained from Charlie Crawford, 5 July 2001
}
\references{
Runkel, R.G., Crawford, C.G., and Cohn, T.A., 2004, Load Estimator (LOADEST):
a FORTRAN program for estimating constituent loads in streams and rivers:
U.S. Geological Survey Techniques and Methods Book 4, Chapter A5, 69 p.
}
\examples{
\dontrun{
data(app4.calib)
# Plot atrazine concentration vs. flow
with(app4.calib, plot(FLOW, Atra, log="xy"))
}}
\keyword{datasets}
