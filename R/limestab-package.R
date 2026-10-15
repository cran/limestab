# limestab-package.R

#' limestab: Stable LIME, OptiLIME, BayLIME and ALIME
#'
#' Four related methods for locally explaining black-box model predictions
#' on tabular data, plus a common set of stability diagnostics for repeated
#' explanations of the same instance.
#'
#' @section Explainers:
#' \itemize{
#'   \item `lime_explainer()` / `lime_explain_instance()` -- standard LIME.
#'   \item `optilime()` -- LIME with the kernel width chosen automatically
#'     by Bayesian optimisation so the local model reaches a target
#'     adherence (R-squared).
#'   \item `baylime_explainer()` / `baylime_explain_instance()` -- LIME
#'     whose local model is a Bayesian ridge regression, allowing an
#'     informative prior on the coefficients.
#'   \item `alime_explainer()` / `alime_explain_instance()` -- a
#'     nearest-neighbour local model fit on the closest perturbed points to
#'     the instance.
#' }
#'
#' @section Stability diagnostics:
#' `lime_check_stability()` repeats an explanation several times and
#' computes the Coefficient Stability Index (`compute_csi()`), the Variable
#' Stability Index (`compute_vsi()`), median-rank stability
#' (`compute_mrms()`), coefficient-of-variation based indices
#' (`compute_cov_metrics()`, `compute_pcsi()`) and interval coverage
#' stability (`compute_icsi()`).
#'
#' @keywords internal
"_PACKAGE"
