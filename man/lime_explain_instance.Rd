\name{lime_explain_instance}
\alias{lime_explain_instance}
\title{Explain a single instance with LIME}
\description{Explain a single instance with LIME.}
\usage{
lime_explain_instance(explainer,
  data_row,
  predict_fn,
  label = 2,
  num_features = 5,
  num_samples = 5000,
  seed = NULL)
}
\arguments{
  \item{explainer}{A lime_explainer object created by lime_explainer().}
  \item{data_row}{The instance to explain, in the original feature space.}
  \item{predict_fn}{Prediction function; takes a matrix and returns a numeric vector or a matrix of class probabilities.}
  \item{label}{Column of predict_fn's output to explain, when it returns a matrix.}
  \item{num_features}{Number of features to keep in the local model.}
  \item{num_samples}{Number of perturbed samples to generate.}
  \item{seed}{Optional seed; if NULL, the global RNG stream continues (repeated calls give different neighbourhoods).}
}
\value{A list describing the local explanation (coefficients, local_exp, score, local_pred, and the data needed for confidence intervals).}
\keyword{models}
