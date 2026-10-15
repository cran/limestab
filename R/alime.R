# alime.R

#' Create an ALIME explainer
#' @param training_data Numeric training-data matrix.
#' @param feature_names Optional character vector of feature names.
#' @param n_neighbors Number of nearest perturbed points kept for the local model.
#' @param feature_selection Feature-selection method.
#' @param penalty Ridge penalty of the local model.
#' @param standardize If `TRUE`, measure ALIME neighbourhood distances in the standardised feature space.
#' @param embed_fn Optional embedding function applied before measuring distances.
#' @param random_state Optional default seed.
#' @return An object of class `alime_explainer`.
#' @export
alime_explainer <- function(training_data,
                            feature_names = NULL,
                            n_neighbors = 300,
                            feature_selection = "forward_selection",
                            penalty = 1,
                            standardize = TRUE,
                            embed_fn = NULL,
                            random_state = NULL) {
  base <- lime_explainer(training_data, feature_names)

  structure(
    list(
      mean = base$mean,
      scale = base$scale,
      feature_names = base$feature_names,
      n_neighbors = n_neighbors,
      feature_selection = feature_selection,
      penalty = penalty,
      standardize = standardize,
      embed_fn = embed_fn,
      random_state = random_state
    ),
    class = "alime_explainer"
  )
}
#' Explain a single instance with ALIME
#' @param explainer An `alime_explainer` object.
#' @param data_row The instance to explain.
#' @param predict_fn Prediction function.
#' @param label Column of `predict_fn`'s output to explain.
#' @param num_features Number of features kept in the local model.
#' @param num_samples Number of points perturbed before keeping the nearest `n_neighbors`.
#' @param seed Optional seed for reproducibility.
#' @return A list with the same structure as `lime_explain_instance()`.
#' @export
alime_explain_instance <- function(explainer,
                                   data_row,
                                   predict_fn,
                                   label = 2,
                                   num_features = 5,
                                   num_samples = 10000,
                                   seed = NULL) {
  run <- function() {
    p <- length(explainer$mean)
    x0 <- as.numeric(data_row)
    x0_scaled <- (x0 - explainer$mean) / explainer$scale

    #Perturbations around the training distribution
    scaled <- matrix(stats::rnorm(num_samples * p), nrow = num_samples, ncol = p)
    original <- unscale_data(scaled, explainer$mean, explainer$scale)

    space <- if (explainer$standardize) scaled else original
    x0_space <- if (explainer$standardize) x0_scaled else x0

    emb <- space
    emb_x0 <- matrix(x0_space, nrow = 1)
    if (!is.null(explainer$embed_fn)) {
      emb <- as.matrix(explainer$embed_fn(space))
      emb_x0 <- as.matrix(explainer$embed_fn(emb_x0))
    }
    distances <- sqrt(rowSums(sweep(emb, 2, as.numeric(emb_x0), "-")^2))

    k <- min(explainer$n_neighbors, num_samples)
    idx <- order(distances)[seq_len(k)]
    local_x <- space[idx, , drop = FALSE]
    local_orig <- original[idx, , drop = FALSE]
    colnames(local_orig) <- explainer$feature_names

    #Black-box labels
    yss <- predict_fn(local_orig)
    y <- if (is.matrix(yss)) as.numeric(yss[, label]) else as.numeric(yss)
    weights <- exp(-distances[idx])

    #Weighted Ridge on the selected features
    method <- explainer$feature_selection
    if (method == "auto") {
      method <- if (num_features <= 6) "forward_selection" else "highest_weights"
    }
    used <- if (method == "highest_weights") {
      fit0 <- fit_weighted_ridge(local_x, y, weights, alpha = 0.01)
      order(abs(fit0$coef * x0_space), decreasing = TRUE)[seq_len(min(num_features, p))]
    } else {
      select_features(local_x, y, weights, num_features, method)
    }

    x_used <- local_x[, used, drop = FALSE]
    fit <- fit_weighted_ridge(x_used, y, weights, alpha = explainer$penalty)

    inst <- matrix(x0, nrow = 1, dimnames = list(NULL, explainer$feature_names))
    inst_pred <- predict_fn(inst)
    inst_pred <- if (is.matrix(inst_pred)) inst_pred[1, label] else inst_pred[1]

    ord <- order(abs(fit$coef), decreasing = TRUE)
    local_exp <- data.frame(
      feature_idx = used[ord],
      feature = explainer$feature_names[used[ord]],
      beta = fit$coef[ord],
      stringsAsFactors = FALSE
    )

    list(
      intercept = fit$intercept,
      coef = fit$coef,
      used_features = used,
      feature_names = explainer$feature_names[used],
      local_exp = local_exp,
      score = weighted_r2(y, fit$fitted, weights),
      local_pred = fit$intercept + sum(fit$coef * x0_space[used]),
      predicted_value = as.numeric(inst_pred),
      X = x_used,
      weights = weights,
      true_labels = y,
      alpha = explainer$penalty,
      distances = distances[idx]
    )
  }

  if (!is.null(seed)) with_local_seed(seed, run()) else run()
}