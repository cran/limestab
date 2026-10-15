\name{lime_explainer}
\alias{lime_explainer}
\title{Create a LIME explainer}
\description{Create a LIME explainer.}
\usage{
lime_explainer(training_data,
  feature_names = NULL,
  kernel_width = NULL,
  feature_selection = "auto",
  sample_around_instance = FALSE,
  penalty = 1,
  random_state = NULL)
}
\arguments{
  \item{training_data}{Numeric training-data matrix used to centre and scale features.}
  \item{feature_names}{Optional character vector of feature names.}
  \item{kernel_width}{Optional kernel width; defaults to default_kernel_width().}
  \item{feature_selection}{Feature-selection method passed to select_features().}
  \item{sample_around_instance}{If TRUE, perturbations are centred on the instance.}
  \item{penalty}{Ridge penalty of the local model (0 = weighted least squares).}
  \item{random_state}{Optional default seed used by lime_check_stability().}
}
\value{An object of class lime_explainer.}
\keyword{models}
