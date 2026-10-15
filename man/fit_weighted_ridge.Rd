\name{fit_weighted_ridge}
\alias{fit_weighted_ridge}
\title{Weighted ridge regression with an unpenalised intercept}
\description{Weighted ridge regression with an unpenalised intercept.}
\usage{
fit_weighted_ridge(x,
  y,
  w,
  alpha = 0)
}
\arguments{
  \item{x}{Numeric predictor matrix.}
  \item{y}{Numeric response vector.}
  \item{w}{Numeric vector of sample weights.}
  \item{alpha}{Ridge penalty; alpha = 0 is weighted least squares.}
}
\value{A list with intercept, coef and fitted (fitted values).}
\keyword{models}
