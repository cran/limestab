\name{select_features}
\alias{select_features}
\title{Select the features used by a LIME-style local model}
\description{Select the features used by a LIME-style local model.}
\usage{
select_features(x,
  y,
  w,
  num_features,
  method = "auto")
}
\arguments{
  \item{x}{Numeric predictor matrix.}
  \item{y}{Numeric response vector.}
  \item{w}{Sample weights.}
  \item{num_features}{Number of features to select.}
  \item{method}{One of "auto", "forward_selection", "highest_weights" or "none".}
}
\value{An integer vector of selected column indices of x.}
\keyword{models}
