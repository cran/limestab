# optilime.R

#' OptiLIME objective (reflected local R-squared)
#' @param explanation The list returned by `lime_explain_instance()`.
#' @param max_r2 Target local R-squared; scores above it are penalised symmetrically.
#' @param epsilon Optional tolerance used to flag and penalise degenerate explanations.
#' @return A single numeric value to be maximised by `optilime()`.
#' @export
optilime_score <- function(explanation, max_r2 = 0.9, epsilon = NULL) {
  if (!is.null(epsilon)) {
    in_interval <- function(x, middle, dev) abs(x - middle) <= abs(dev)
    if (in_interval(explanation$intercept, explanation$local_pred, epsilon) &&
        in_interval(explanation$score, 1, 10 * epsilon)) {
      return(0)
    }
    return(explanation$score)
  }
  r2 <- explanation$score
  if (r2 > max_r2) max_r2 - (r2 - max_r2) else r2
}

#Build the objective function kernel width
make_optilime_loss <- function(explainer, data_row, predict_fn,
                               label = 2, num_features = 5, num_samples = 5000,
                               max_r2 = 0.9, epsilon = NULL, seed = 42) {
  function(kernel_width) {
    ex <- explainer
    ex$kernel_width <- as.numeric(kernel_width)[1]
    explanation <- tryCatch(
      lime_explain_instance(ex, data_row, predict_fn, label,
                            num_features, num_samples, seed = seed),
      error = function(e) NULL
    )
    if (is.null(explanation)) return(0)
    optilime_score(explanation, max_r2, epsilon)
  }
}
#' OptiLIME: choose the LIME kernel width by Bayesian optimisation
#' @param explainer A `lime_explainer` object.
#' @param data_row The instance to explain.
#' @param predict_fn Prediction function.
#' @param bounds `c(lower, upper)` search bounds for the kernel width.
#' @param n_iters Number of Bayesian-optimisation iterations.
#' @param n_pre_samples Number of random initial kernel widths.
#' @param label Column of `predict_fn`'s output to explain.
#' @param num_features Number of features kept in the local model.
#' @param num_samples Number of perturbed samples per explanation.
#' @param max_r2 Target local R-squared passed to `optilime_score()`.
#' @param epsilon Optional tolerance passed to `optilime_score()`.
#' @param seed Seed used for every evaluation, so kernel widths are compared on identical perturbations.
#' @param verbose If `TRUE`, print progress.
#' @return A list with `best_width`, `best_loss`, `r2`, `explanation`, `explainer` and `optimization`.
#' @export
optilime <- function(explainer, data_row, predict_fn,
                     bounds = c(0.1, 3.0), n_iters = 30, n_pre_samples = 10,
                     label = 2, num_features = 5, num_samples = 5000,
                     max_r2 = 0.9, epsilon = NULL, seed = 42, verbose = FALSE) {
  loss_fn <- make_optilime_loss(explainer, data_row, predict_fn, label,
                                num_features, num_samples, max_r2, epsilon, seed)
  bo <- bayesian_optimisation(n_iters, loss_fn, bounds, n_pre_samples,
                              random_state = seed, verbose = verbose)

  best_i <- which.max(bo$y_sample)
  best_width <- bo$x_sample[best_i]

  best_explainer <- explainer
  best_explainer$kernel_width <- best_width
  explanation <- lime_explain_instance(best_explainer, data_row, predict_fn, label,
                                       num_features, num_samples, seed = seed)

  list(
    best_width = best_width,
    best_loss = bo$y_sample[best_i],
    r2 = explanation$score,
    explanation = explanation,
    explainer = best_explainer,
    optimization = bo
  )
}
