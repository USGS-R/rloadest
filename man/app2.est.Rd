\name{app2.est}
\alias{app2.est}
\docType{data}
\title{
app2.est Data
}
\description{
  St.Joseph River near Newville, IN (Station # 04178000)
}
\usage{app2.est}
\format{
Data frame with 730 rows and 3 columns\cr
\tabular{lll}{
 Name \tab Type \tab Description\cr
 \cr
 DATES \tab Date \tab Date of daily value\cr
 TIMES \tab integer \tab Time corresponding to noon of daily value\cr
 FLOW \tab numeric \tab Daily mean streamflow\cr
 }
}
\source{
Example estimation dataset from LOADEST
}
\references{
Runkel, R.G., Crawford, C.G., and Cohn, T.A., 2004, Load Estimator (LOADEST):
a FORTRAN program for estimating constituent loads in streams and rivers:
U.S. Geological Survey Techniques and Methods Book 4, Chapter A5, 69 p.
}
\examples{
\dontrun{
data(app2.est)
summary(app2.est)
}}
\keyword{datasets}
