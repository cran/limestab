\name{baylime_explainer}
\alias{baylime_explainer}
\title{Create a BayLIME explainer}
\description{Create a BayLIME explainer.}
\usage{
baylime_explainer(training_data,
  feature_names = NULL,
  kernel_width = NULL,
  feature_selection = "auto",
  sample_around_instance = FALSE,
  model_regressor = c(
    "Bay_info_prior",
    "non_Bay",
    "Bay_non_info_prior",
    "BayesianRidge_inf_prior_fit_alpha"
  ),
  prior_alpha = 1,
  prior_lambda = 1000,
  random_state = NULL)
}
\arguments{
  \item{training_data}{Numeric training-data matrix.}
  \item{feature_names}{Optional character vector of feature names.}
  \item{kernel_width}{Optional kernel width.}
  \item{feature_selection}{Feature-selection method.}
  \item{sample_around_instance}{If TRUE, perturbations are centred on the instance.}
  \item{model_regressor}{One of "Bay_info_prior", "non_Bay", "Bay_non_info_prior" or "BayesianRidge_inf_prior_fit_alpha".}
  \item{prior_alpha}{Informative prior on the noise precision (used by "Bay_info_prior").}
  \item{prior_lambda}{Informative prior on the weight precision (used by "Bay_info_prior" and "BayesianRidge_inf_prior_fit_alpha").}
  \item{random_state}{Optional default seed.}
}
\value{An object of class baylime_explainer.}
\keyword{models}
