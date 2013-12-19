\name{app5.1999}
\alias{app5.1999}
\docType{data}
\title{
app5.1999 Data
}
\description{
  Little Arkansas River near Halstead, Kansas (Station Number 07143672)
}
\usage{app5.1999}
\format{
Data frame with 13 rows and 3 columns\cr
\tabular{lll}{
 Name \tab Type \tab Description\cr
 \cr
 DATES \tab Date \tab Date of daily value\cr
 TIMES \tab character \tab Time that sample was actually taken\cr
 Alkalinity \tab numeric \tab Daily mean alkalinity (assumed)\cr
 }
}
\source{
Retrieved from NWISweb on July 26, 2013 from URL:
\link{"http://nwis.waterdata.usgs.gov/ks/nwis/qwdata"}
}
\examples{
data(app5.1999)
head(app5.1999)
}
\keyword{datasets}
