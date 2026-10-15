\name{optilime}
\alias{optilime}
\title{OptiLIME: choose the LIME kernel width by Bayesian optimisation}
\description{OptiLIME: choose the LIME kernel width by Bayesian optimisation.}
\usage{
optilime(explainer,
  data_row,
  predict_fn,
  bounds = c(0.1, 3.0),
  n_iters = 30,
  n_pre_samples = 10,
  label = 2,
  num_features = 5,
  num_samples = 5000,
  max_r2 = 0.9,
  epsilon = NULL,
  seed = 42,
  verbose = FALSE)
}
\arguments{
  \item{explainer}{A lime_explainer object.}
  \item{data_row}{The instance to explain.}
  \item{predict_fn}{Prediction function.}
  \item{bounds}{c(lower, upper) search bounds for the kernel width.}
  \item{n_iters}{Number of Bayesian-optimisation iterations.}
  \item{n_pre_samples}{Number of random initial kernel widths.}
  \item{label}{Column of predict_fn's output to explain.}
  \item{num_features}{Number of features kept in the local model.}
  \item{num_samples}{Number of perturbed samples per explanation.}
  \item{max_r2}{Target local R-squared passed to optilime_score().}
  \item{epsilon}{Optional tolerance passed to optilime_score().}
  \item{seed}{Seed used for every evaluation, so kernel widths are compared on identical perturbations.}
  \item{verbose}{If TRUE, print progress.}
}
\value{A list with best_width, best_loss, r2, explanation, explainer and optimization.}
\keyword{models}
