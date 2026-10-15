\name{lime_confidence_intervals}
\alias{lime_confidence_intervals}
\title{Confidence intervals for a LIME-style explanation}
\description{Confidence intervals for a LIME-style explanation.}
\usage{
lime_confidence_intervals(explanation)
}
\arguments{
  \item{explanation}{The list returned by lime_explain_instance() or baylime_explain_instance().}
}
\value{A named list of c(lower, upper) intervals, or NULL if uncertainty cannot be estimated.}
\details{For Bayesian BayLIME fits, intervals are built from the posterior coefficient covariance. For LIME, ALIME and non-Bayesian BayLIME fits, approximate weighted-ridge standard errors are used.}
\keyword{models}
