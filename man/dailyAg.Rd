\name{dailyAg}
\alias{dailyAg}
\title{Daily Mean}
\usage{
  dailyAg(x, dates = "sample_dt", times = "sample_tm")
}
\arguments{
  \item{x}{a data frame contining water-quality data.}

  \item{dates}{the name of the sample date column.}

  \item{times}{the name of the sample time column.}
}
\value{
  A data frame like \code{x] but with the means for each
  column by day and the sample time set to "1200."
}
\description{
  Compute the daily mean for data frames containing
  water-quality data.
}
