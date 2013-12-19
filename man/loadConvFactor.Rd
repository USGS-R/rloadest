\name{loadConvFactor}
\alias{loadConvFactor}
\title{Unit Conversion}
\usage{
  loadConvFactor(flow.units, conc.units, load.units)
}
\arguments{
  \item{flow.units}{character string describing the flow
  unit}

  \item{conc.units}{character string describing the
  concentration unit}

  \item{load.units}{character string describing the load
  unit}
}
\value{
  The conversion factor.
}
\description{
  Computes the conversion factor to compute the flux units
  (load units) from the concentration and flow units
}
\examples{
loadConvFactor("cubic meter per second","milligrams per liter","pounds")
}
\keyword{conversions}
\keyword{unit}

