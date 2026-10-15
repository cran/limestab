\name{predict.optilime_gp}
\alias{predict.optilime_gp}
\title{Predict method for optilime_gp objects}
\description{Predict method for optilime_gp objects.}
\usage{
predict.optilime_gp(object,
  newdata,
  return_std = FALSE,
  ...)
}
\arguments{
  \item{object}{An optilime_gp object returned by fit_gp().}
  \item{newdata}{Numeric vector of new points.}
  \item{return_std}{If TRUE, also return the predictive standard deviation.}
  \item{...}{Unused; included for S3 consistency.}
}
\value{A numeric vector of means, or a list with mean and sd when return_std = TRUE.}
\keyword{models}
