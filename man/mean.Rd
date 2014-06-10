\name{mean.factor}
\alias{mean.character}
\alias{mean.factor}
\title{Arithmetic Mean}
\usage{
\method{mean}{factor}(x, ...)

\method{mean}{character}(x, ...)
}
\arguments{
  \item{x}{an object of class "factor" or "character."}

  \item{\dots}{further arguments passed to or from other
  methods.}
}
\value{
If all values are identical, then the unique value,
otherwise the missing value (\code{NA}).
}
\description{
Method functions to compute the "mean" for factors and
characters. These funcitons are intended primarily as
support functions when aggregating unit values data in
\code{predLoad}.
}

