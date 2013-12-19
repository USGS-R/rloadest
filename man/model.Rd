\name{model}
\alias{model}
\title{Load Model}
\usage{
  model(model.no, data, flow, time)
}
\arguments{
  \item{model.no}{the model number.}

  \item{data}{the dataset.}

  \item{flow}{character string indicating the name of the
  flow column.}

  \item{dates}{character string indicating the name of the
  date column.}
}
\value{
  Row number to select from \code{data} to build a
  predefined model.
}
\description{
  Support function for building a predefined rating curve
  load model.
}

