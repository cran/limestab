\name{alime_explain_instance}
\alias{alime_explain_instance}
\title{Explain a single instance with ALIME}
\description{Explain a single instance with ALIME.}
\usage{
alime_explain_instance(explainer,
  data_row,
  predict_fn,
  label = 2,
  num_features = 5,
  num_samples = 10000,
  seed = NULL)
}
\arguments{
  \item{explainer}{An alime_explainer object.}
  \item{data_row}{The instance to explain.}
  \item{predict_fn}{Prediction function.}
  \item{label}{Column of predict_fn's output to explain.}
  \item{num_features}{Number of features kept in the local model.}
  \item{num_samples}{Number of points perturbed before keeping the nearest n_neighbors.}
  \item{seed}{Optional seed for reproducibility.}
}
\value{A list with the same structure as lime_explain_instance().}
\keyword{models}
