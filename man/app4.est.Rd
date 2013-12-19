\name{app4.est}
\alias{app4.est}
\docType{data}
\title{
app4.est Data
}
\description{
  White River at Hazleton, Ind. (Station Number 03374100)
}
\usage{app4.est}
\format{
Data frame with 730 rows and 3 columns\cr
\tabular{lll}{
 Name \tab Type \tab Description\cr
 \cr
 DATES \tab Date \tab Date of daily value\cr
 TIMES \tab character \tab Time corresponding to noon of daily value\cr
 FLOW \tab numeric \tab Daily mean streamflow\cr
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
data(app4.est)
summary(app4.est)
}}
\keyword{datasets}
