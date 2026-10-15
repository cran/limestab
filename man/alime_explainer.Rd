\name{alime_explainer}
\alias{alime_explainer}
\title{Create an ALIME explainer}
\description{Create an ALIME explainer.}
\usage{
alime_explainer(training_data,
  feature_names = NULL,
  n_neighbors = 300,
  feature_selection = "forward_selection",
  penalty = 1,
  standardize = TRUE,
  embed_fn = NULL,
  random_state = NULL)
}
\arguments{
  \item{training_data}{Numeric training-data matrix.}
  \item{feature_names}{Optional character vector of feature names.}
  \item{n_neighbors}{Number of nearest perturbed points kept for the local model.}
  \item{feature_selection}{Feature-selection method.}
  \item{penalty}{Ridge penalty of the local model.}
  \item{standardize}{If TRUE, measure ALIME neighbourhood distances in the standardised feature space.}
  \item{embed_fn}{Optional embedding function applied before measuring distances.}
  \item{random_state}{Optional default seed.}
}
\value{An object of class alime_explainer.}
\keyword{models}
