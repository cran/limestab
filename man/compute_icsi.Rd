\name{compute_icsi}
\alias{compute_icsi}
\title{Interval-Based Coefficient Stability Index (ICSI)}
\description{Interval-Based Coefficient Stability Index (ICSI).}
\usage{
compute_icsi(B,
  train_ratio = 0.8,
  eps = 1e-12)
}
\arguments{
  \item{B}{Coefficient matrix (runs x features).}
  \item{train_ratio}{Share of runs used to build the reference interval.}
  \item{eps}{Small constant used to avoid division by zero.}
}
\value{A named list of coverage results (ICSI_MAD, ICSI_rMAD, ICSI_IQR, ICSI_sMAD), each with coverage_per_feature and overall_coverage.}
\keyword{models}
