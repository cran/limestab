\name{compute_csi}
\alias{compute_csi}
\title{Coefficient Stability Index (CSI)}
\description{Coefficient Stability Index (CSI).}
\usage{
compute_csi(confidence_intervals,
  index_verbose = FALSE)
}
\arguments{
  \item{confidence_intervals}{List of named confidence-interval lists, one element per repeated LIME run.}
  \item{index_verbose}{If TRUE, print the per-feature overlap percentage.}
}
\value{A single numeric value between 0 and 100.}
\keyword{models}
