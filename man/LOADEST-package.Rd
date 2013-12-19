\docType{package}
\name{LOADEST-package}
\alias{LOADEST-package}
\title{LOADEST functions for R}
\description{
  \tabular{ll}{ Package: \tab LOADEST\cr Type: \tab
  Package\cr Version: \tab 0.2\cr Date: \tab 2013-07-16\cr
  License: \tab GPL 2\cr LazyLoad: \tab yes\cr }
}
\details{
  This package is intended to replicate and extend the
  LOADEST program for estimating constituent loads in
  streams and rivers. Some subtle differences between the
  output for LOADEST and rloadest include:

  LOADEST uses centered time when computing the sine and
  cosine terms in model numbers 4, 6, 7, 8, and 9, but the
  functions in rloadest use the actual decimal time so that
  the seasonality can more easily be assessed by the user.

  The order of the terms in the predefined models is
  different between LOADEST and the rloadest functions.

  The printed output of the model descriptions from
  rloadest matches the format the most users of R would
  recognize from other linear model output rather then the
  printed output from LOADEST.

  Furhtermore, the model building capability in the
  rloadest functions make easier to explore other forms of
  rating-curve models than LOADEST.
}
\author{
  Dave Lorenz \email{lorenz@usgs.gov}
}
\keyword{estimation}
\keyword{load}

