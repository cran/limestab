\name{compute_cov_metrics}
\alias{compute_cov_metrics}
\title{Coefficient-of-variation metrics across repeated runs}
\description{Coefficient-of-variation metrics across repeated runs.}
\usage{
compute_cov_metrics(run_rankings,
  topk = NULL,
  eps = 1e-12,
  missing_value = 0)
}
\arguments{
  \item{run_rankings}{List of data frames (feature, beta), one per run.}
  \item{topk}{If not NULL, keep only the topk features with the largest median abs(beta).}
  \item{eps}{Small constant used to avoid division by zero.}
  \item{missing_value}{Value used for a feature absent from a given run.}
}
\value{A list with cov_results (per feature) and the averages cov_unscaled, cov_scaled, cov_iqr, median_sd.}
\keyword{models}
