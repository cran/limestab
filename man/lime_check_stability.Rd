\name{lime_check_stability}
\alias{lime_check_stability}
\title{Repeat an explanation and compute every stability metric}
\description{Repeat an explanation and compute every stability metric.}
\usage{
lime_check_stability(explainer,
  data_row,
  predict_fn,
  label = 2,
  num_features = 5,
  num_samples = 5000,
  n_calls = 10,
  index_verbose = FALSE,
  topk = 5,
  train_ratio = 0.8,
  seed = explainer$random_state,
  explain_fn = lime_explain_instance)
}
\arguments{
  \item{explainer}{An explainer object (from lime_explainer(), baylime_explainer() or alime_explainer()).}
  \item{data_row}{The instance to explain.}
  \item{predict_fn}{Prediction function passed on to explain_fn.}
  \item{label}{Column of predict_fn's output to explain.}
  \item{num_features}{Number of features kept in each individual explanation.}
  \item{num_samples}{Number of perturbed samples per explanation.}
  \item{n_calls}{Number of repeated explanations.}
  \item{index_verbose}{Passed on to compute_csi() / compute_vsi().}
  \item{topk}{Number of top features used for the top-k metrics.}
  \item{train_ratio}{Share of runs used as the reference set for compute_icsi().}
  \item{seed}{Seed for the whole procedure; the global RNG state is restored afterwards.}
  \item{explain_fn}{Function used to explain one instance, e.g. lime_explain_instance, baylime_explain_instance or alime_explain_instance.}
}
\value{A list with csi, vsi, mrms_all, mrms_topk, PCSI_MAD/PCSI_sMAD/PCSI_IQR/PCSI_MSD, interval_results (ICSI) and the raw run_rankings / confidence_intervals.}
\keyword{models}
