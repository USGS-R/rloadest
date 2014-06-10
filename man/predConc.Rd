\name{predConc}
\alias{predConc}
\title{Predict Concentrations}
\usage{
predConc(fit, newdata, by = "day", allow.incomplete = FALSE,
  conf.int = 0.95)
}
\arguments{
  \item{fit}{the output from \code{loadReg}.}

  \item{newdata}{a data frame of the prediction variables.}

  \item{by}{the time frame for estimates. See
  \code{Details}.}

  \item{allow.incomplete}{compute loads for periods withing
  missing values or incomplete record? See \code{Details}.}

  \item{conf.int}{the confidence interval to compute for
  concentrations. See \code{Details}.}
}
\value{
A data frame containing the concnetration estimates.
}
\description{
Estimate concentrations from a rating-curve model from
\code{loadReg} for a new data frame.
}
\details{
The time frame specified by \code{by} must be either "unit"
or "day."

If \code{allow.incomplete} is \code{TRUE}, then
concentrations will be computed based on all nonmissing
values, otherwise missing values \code{NAs} will be
returned. For this application, missing values includes
\code{NAs} and incomplete days.

The term confidence interval is used here as in the
original documentation for LOADEST, but the values that are
reported are the prediction intervals, computed from the
SEP.
}
\examples{
# From application 1 in the vignettes
data(app1.calib)
app1.lr <- loadReg(Phosphorus ~ model(1), data = app1.calib,
 flow = "FLOW", dates = "DATES", conc.units="mg/L",
 station="Illinois River at Marseilles, Ill.")
predConc(app1.lr, app1.calib)
}
\seealso{
\code{\link{loadReg}},
}

