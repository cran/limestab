\name{compute_vsi}
\alias{compute_vsi}
\title{Variable Stability Index (VSI)}
\description{Variable Stability Index (VSI).}
\usage{
compute_vsi(confidence_intervals,
  index_verbose = FALSE)
}
\arguments{
  \item{confidence_intervals}{List of named confidence-interval lists, one element per repeated LIME run.}
  \item{index_verbose}{If TRUE, print the overall percentage.}
}
\value{A single numeric value between 0 and 100.}
\details{For a pair of runs with different numbers of selected variables, the overlap is normalised by the smaller selected set, avoiding dependence on the first run's feature count.}
\keyword{models}
