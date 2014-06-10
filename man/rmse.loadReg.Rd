\name{rmse.loadReg}
\alias{rmse.loadReg}
\title{Root-Mean-Squared Error}
\usage{
\method{rmse}{loadReg}(x, ...)
}
\arguments{
  \item{x}{the output from \code{loadReg}.}

  \item{\dots}{further arguments passed to or from other
  methods.}
}
\value{
The estimated root-mean-squared error, also know as the
residual standard error.
}
\description{
Compute the root-mean-squared error (RMSE) of the
difference between observed values and the fitted values
}
\examples{
# From application 1 in the vignettes
data(app1.calib)
app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib,
 flow = "FLOW", dates = "DATES", conc.units="mg/L",
 station="Illinois River at Marseilles, Ill.")
rmse(app1.lr, app1.calib)
}
\seealso{
\code{\link{loadReg}},
}

