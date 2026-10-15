\name{compute_mrms}
\alias{compute_mrms}
\title{Median Rank Matching Stability of features across repeated runs}
\description{Median Rank Matching Stability of features across repeated runs.}
\usage{
compute_mrms(run_rankings,
  k = 5)
}
\arguments{
  \item{run_rankings}{List of data frames (feature, beta), one per run.}
  \item{k}{Number of top features used for the explanation-based top-k score.}
}
\value{A list with per_feature, mrms_all and mrms_topk. Scores are percentages.}
\details{Features are ranked by decreasing absolute coefficient. Tied absolute coefficients receive the average of the rank positions they occupy. A missing feature is not assigned an artificial rank; it contributes zero to the matching score for that run. MRMS-All averages the resulting binary matches over all features and runs.}
\keyword{models}
