\name{fit_gp}
\alias{fit_gp}
\title{Fit a 1-D Gaussian process (Matern 3/2 kernel)}
\description{Fit a 1-D Gaussian process (Matern 3/2 kernel).}
\usage{
fit_gp(x,
  y,
  alpha = 1e-6)
}
\arguments{
  \item{x}{Numeric vector of training inputs (kernel widths already evaluated).}
  \item{y}{Numeric vector of observed values (the loss at each x).}
  \item{alpha}{Observation noise variance added to the kernel diagonal.}
}
\value{An object of class optilime_gp.}
\keyword{models}
