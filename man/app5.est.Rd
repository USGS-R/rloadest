\name{app5.est}
\alias{app5.est}
\docType{data}
\title{
app5.est Data
}
\description{
  Little Arkansas River near Halstead, Kansas (Station Number 07143672)
}
\usage{app5.est}
\format{
Data frame with 358 rows and 4 columns\cr
\tabular{lll}{
 Name \tab Type \tab Description\cr
 \cr
 DATES \tab Date \tab Date of daily value\cr
 TIMES \tab character \tab ime corresponding to noon of daily value\cr
 FLOW \tab numeric \tab Daily mean streamflow\cr
 SC \tab numeric \tab Daily mean specific conductance\cr
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
data(app5.est)
summary(app5.est)
}}
\keyword{datasets}
