# lime.R

#' Create a LIME explainer
#' @param training_data Numeric training-data matrix used to centre and scale features.
#' @param feature_names Optional character vector of feature names.
#' @param kernel_width Optional kernel width; defaults to `default_kernel_width()`.
#' @param feature_selection Feature-selection method passed to `select_features()`.
#' @param sample_around_instance If `TRUE`, perturbations are centred on the instance.
#' @param penalty Ridge penalty of the local model (0 = weighted least squares).
#' @param random_state Optional default seed used by `lime_check_stability()`.
#' @return An object of class `lime_explainer`.
#' @export
lime_explainer <- function(training_data,
                           feature_names = NULL,
                           kernel_width = NULL,
                           feature_selection = "auto",
                           sample_around_instance = FALSE,
                           penalty = 1,
                           random_state = NULL) {
  training_data <- as.matrix(training_data)
  p <- ncol(training_data)

  if (is.null(feature_names)) {
    feature_names <- if (!is.null(colnames(training_data))) {
      colnames(training_data)
    } else {
      paste0("feature_", seq_len(p))
    }
  }

  center <- colMeans(training_data)
  scale <- sqrt(colMeans(sweep(training_data, 2, center)^2))
  scale[scale == 0] <- 1

  if (is.null(kernel_width)) kernel_width <- default_kernel_width(p)

  structure(
    list(
      mean = center,
      scale = scale,
      feature_names = feature_names,
      kernel_width = kernel_width,
      feature_selection = feature_selection,
      sample_around_instance = sample_around_instance,
      penalty = penalty,
      random_state = random_state
    ),
    class = "lime_explainer"
  )
}
#' Explain a single instance with LIME
#' @param explainer A `lime_explainer` object created by `lime_explainer()`.
#' @param data_row The instance to explain, in the original feature space.
#' @param predict_fn Prediction function; takes a matrix and returns a numeric vector or a matrix of class probabilities.
#' @param label Column of `predict_fn`'s output to explain, when it returns a matrix.
#' @param num_features Number of features to keep in the local model.
#' @param num_samples Number of perturbed samples to generate.
#' @param seed Optional seed; if `NULL`, the global RNG stream continues (repeated calls give different neighbourhoods).
#' @return A list describing the local explanation (coefficients, `local_exp`, `score`, `local_pred`, and the data needed for confidence intervals).
#' @export
lime_explain_instance <- function(explainer,
                                  data_row,
                                  predict_fn,
                                  label = 2,
                                  num_features = 5,
                                  num_samples = 5000,
                                  seed = NULL) {
  run <- function() {
    x0 <- as.numeric(data_row)
    x0_scaled <- (x0 - explainer$mean) / explainer$scale

    #Neighbourhood + distances
    scaled <- generate_perturbations(x0_scaled, num_samples,
                                     explainer$sample_around_instance)
    distances <- calculate_distances(scaled, x0_scaled)

    #Black-box predictions on the original feature space
    original <- unscale_data(scaled, explainer$mean, explainer$scale)
    original[1, ] <- x0
    colnames(original) <- explainer$feature_names
    yss <- predict_fn(original)
    y <- if (is.matrix(yss)) as.numeric(yss[, label]) else as.numeric(yss)

    #Kernel weights
    weights <- lime_kernel(distances, explainer$kernel_width)

    #Local model on standardised data
    used <- select_features(scaled, y, weights, num_features,
                            explainer$feature_selection)
    x_used <- scaled[, used, drop = FALSE]
    fit <- fit_weighted_ridge(x_used, y, weights, alpha = explainer$penalty)

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
      local_pred = fit$intercept + sum(fit$coef * x_used[1, ]),
      predicted_value = y[1],
      X = x_used,
      weights = weights,
      true_labels = y,
      alpha = explainer$penalty,
      distances = distances,
      kernel_width = explainer$kernel_width
    )
  }

  if (!is.null(seed)) with_local_seed(seed, run()) else run()
}
