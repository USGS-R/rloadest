\name{logLik.loadReg}
\alias{logLik.loadReg}
\title{Extract Log-Likelihood}
\usage{
\method{logLik}{loadReg}(object, ...)
}
\arguments{
  \item{object}{the output from \code{loadReg}.}

  \item{\dots}{further arguments passed to or from other
  methods.}
}
\value{
An object of class "logLik" containing the log-likelihood
and the attributes "df" (degrees of freedom) and "nobs"
(number of observations).
}
\description{
Compute the log-likelihood statistics for a load
regression.
}
\examples{
# From application 1 in the vignettes
data(app1.calib)
app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib,
 flow = "FLOW", dates = "DATES", conc.units="mg/L",
 station="Illinois River at Marseilles, Ill.")
logLik(app1.lr)
}
\seealso{
\code{\link{loadReg}},
}

