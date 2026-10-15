\name{lime_kernel}
\alias{lime_kernel}
\title{LIME exponential kernel}
\description{LIME exponential kernel.}
\usage{
lime_kernel(distances,
  kernel_width)
}
\arguments{
  \item{distances}{Numeric vector of distances to the instance being explained.}
  \item{kernel_width}{Positive numeric kernel width.}
}
\value{Numeric vector of proximity weights, sqrt(exp(-distances^2 / kernel_width^2)).}
\keyword{models}
