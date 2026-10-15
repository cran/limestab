\name{baylime_explain_instance}
\alias{baylime_explain_instance}
\title{Explain a single instance with BayLIME}
\description{Explain a single instance with BayLIME.}
\usage{
baylime_explain_instance(explainer,
  data_row,
  predict_fn,
  label = 2,
  num_features = 5,
  num_samples = 5000,
  seed = NULL)
}
\arguments{
  \item{explainer}{A baylime_explainer object.}
  \item{data_row}{The instance to explain.}
  \item{predict_fn}{Prediction function.}
  \item{label}{Column of predict_fn's output to explain.}
  \item{num_features}{Number of features kept in the local model.}
  \item{num_samples}{Number of perturbed samples.}
  \item{seed}{Optional seed for reproducibility.}
}
\value{A list with the same structure as lime_explain_instance(), plus local_std, coef_var, posterior_alpha and posterior_lambda.}
\keyword{models}
