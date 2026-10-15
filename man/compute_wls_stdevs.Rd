\name{compute_wls_stdevs}
\alias{compute_wls_stdevs}
\title{Standard deviations of weighted ridge coefficients}
\description{Standard deviations of weighted ridge coefficients.}
\usage{
compute_wls_stdevs(x,
  y,
  weights,
  alpha)
}
\arguments{
  \item{x}{Numeric predictor matrix used by the local model.}
  \item{y}{Numeric response vector.}
  \item{weights}{Sample weights.}
  \item{alpha}{Ridge penalty used when fitting the local model.}
}
\value{A numeric vector of approximate coefficient standard deviations.}
\keyword{models}
