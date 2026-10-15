# baylime.R

pop_var <- function(v) mean((v - mean(v))^2)
#' Bayesian ridge regression (Tipping 2001 / MacKay 1992)
#' @param x Numeric predictor matrix.
#' @param y Numeric response vector.
#' @param w Sample weights.
#' @param alpha_init Initial (or fixed) noise precision; `NULL` uses `1 / var(y)`.
#' @param lambda_init Initial (or fixed) weight precision; `NULL` uses 1.
#' @param fit_alpha Whether the noise precision is re-estimated from data.
#' @param fit_lambda Whether the weight precision is re-estimated from data.
#' @param max_iter Maximum number of update iterations.
#' @param tol Convergence tolerance on the coefficients.
#' @param alpha_1 Gamma-prior shape hyperparameter for alpha.
#' @param alpha_2 Gamma-prior rate hyperparameter for alpha.
#' @param lambda_1 Gamma-prior shape hyperparameter for lambda.
#' @param lambda_2 Gamma-prior rate hyperparameter for lambda.
#' @param weighted_centering If `TRUE`, centre `x`/`y` with weighted means; if `FALSE`, use unweighted means.
#' @return A list with `intercept`, `coef`, `sigma` (posterior covariance), `alpha` and `lambda`.
#' @export
bayesian_ridge_fit <- function(x, y, w,
                               alpha_init = NULL, lambda_init = NULL,
                               fit_alpha = TRUE, fit_lambda = TRUE,
                               max_iter = 1000, tol = 1e-4,
                               alpha_1 = 1e-6, alpha_2 = 1e-6,
                               lambda_1 = 1e-6, lambda_2 = 1e-6,
                               weighted_centering = FALSE) {
  x <- as.matrix(x)
  n <- nrow(x)
  p <- ncol(x)
  if (n <= p) stop("bayesian_ridge_fit needs more samples than features.")

  if (weighted_centering) {
    x_off <- colSums(w * x) / sum(w)
    y_off <- sum(w * y) / sum(w)
  } else {
    x_off <- colMeans(x)
    y_off <- mean(y)
  }
  sw <- sqrt(w)
  xr <- sweep(x, 2, x_off) * sw
  yr <- (y - y_off) * sw

  eps <- .Machine$double.eps
  alpha <- if (is.null(alpha_init)) 1 / (pop_var(yr) + eps) else alpha_init
  lambda <- if (is.null(lambda_init)) 1 else lambda_init

  sv <- svd(xr)
  eig <- sv$d^2
  V <- sv$v
  xty <- crossprod(xr, yr)

  update_coef <- function(alpha, lambda) {
    coef <- as.numeric(V %*% (crossprod(V, xty) / (eig + lambda / alpha)))
    list(coef = coef, rmse = sum((yr - xr %*% coef)^2))
  }

  n_iter <- if (fit_alpha || fit_lambda) max_iter else 0
  coef_old <- NULL
  for (iter in seq_len(n_iter)) {
    cu <- update_coef(alpha, lambda)
    gamma <- sum((alpha * eig) / (lambda + alpha * eig))
    if (fit_lambda) lambda <- (gamma + 2 * lambda_1) / (sum(cu$coef^2) + 2 * lambda_2)
    if (fit_alpha) alpha <- (n - gamma + 2 * alpha_1) / (cu$rmse + 2 * alpha_2)
    if (iter != 1 && sum(abs(coef_old - cu$coef)) < tol) break
    coef_old <- cu$coef
  }

  coef <- update_coef(alpha, lambda)$coef
  sigma <- (V %*% (t(V) / (eig + lambda / alpha))) / alpha

  list(intercept = y_off - sum(x_off * coef), coef = coef, sigma = sigma,
       alpha = alpha, lambda = lambda)
}

baylime_fit_local <- function(x, y, w, model_regressor, prior_alpha, prior_lambda) {
  if (model_regressor == "non_Bay") {
    fit <- fit_weighted_ridge(x, y, w, alpha = 1)
    return(list(intercept = fit$intercept, coef = fit$coef, fitted = fit$fitted,
                sigma = NULL, alpha_post = NA_real_, lambda_post = NA_real_,
                penalty = 1))
  }

  fit <- switch(
    model_regressor,
    Bay_non_info_prior = bayesian_ridge_fit(
      x, y, w, fit_alpha = TRUE, fit_lambda = TRUE,
      max_iter = 1000, tol = 1e-4, weighted_centering = TRUE),
    Bay_info_prior = bayesian_ridge_fit(
      x, y, w, alpha_init = prior_alpha, lambda_init = prior_lambda,
      fit_alpha = FALSE, fit_lambda = FALSE),
    BayesianRidge_inf_prior_fit_alpha = bayesian_ridge_fit(
      x, y, w, lambda_init = prior_lambda,
      fit_alpha = TRUE, fit_lambda = FALSE, max_iter = 1000, tol = 1e-4)
  )

  list(intercept = fit$intercept, coef = fit$coef,
       fitted = as.numeric(fit$intercept + x %*% fit$coef),
       sigma = fit$sigma, alpha_post = fit$alpha, lambda_post = fit$lambda,
       penalty = fit$lambda / fit$alpha)
}

#' Create a BayLIME explainer
#' @param training_data Numeric training-data matrix.
#' @param feature_names Optional character vector of feature names.
#' @param kernel_width Optional kernel width.
#' @param feature_selection Feature-selection method.
#' @param sample_around_instance If `TRUE`, perturbations are centred on the instance.
#' @param model_regressor One of "Bay_info_prior", "non_Bay", "Bay_non_info_prior" or "BayesianRidge_inf_prior_fit_alpha".
#' @param prior_alpha Informative prior on the noise precision (used by "Bay_info_prior").
#' @param prior_lambda Informative prior on the weight precision (used by "Bay_info_prior" and "BayesianRidge_inf_prior_fit_alpha").
#' @param random_state Optional default seed.
#' @return An object of class `baylime_explainer`.
#' @usage
#' baylime_explainer(
#'   training_data,
#'   feature_names = NULL,
#'   kernel_width = NULL,
#'   feature_selection = "auto",
#'   sample_around_instance = FALSE,
#'   model_regressor = c(
#'     "Bay_info_prior",
#'     "non_Bay",
#'     "Bay_non_info_prior",
#'     "BayesianRidge_inf_prior_fit_alpha"
#'   ),
#'   prior_alpha = 1,
#'   prior_lambda = 1000,
#'   random_state = NULL
#' )
#' @export
baylime_explainer <- function(training_data,
                              feature_names = NULL,
                              kernel_width = NULL,
                              feature_selection = "auto",
                              sample_around_instance = FALSE,
                              model_regressor = c("Bay_info_prior", "non_Bay",
                                                  "Bay_non_info_prior",
                                                  "BayesianRidge_inf_prior_fit_alpha"),
                              prior_alpha = 1,
                              prior_lambda = 1000,
                              random_state = NULL) {
  ex <- lime_explainer(training_data, feature_names, kernel_width,
                       feature_selection, sample_around_instance,
                       penalty = 1, random_state = random_state)
  ex$model_regressor <- match.arg(model_regressor)
  ex$prior_alpha <- prior_alpha
  ex$prior_lambda <- prior_lambda
  class(ex) <- c("baylime_explainer", class(ex))
  ex
}
#' Explain a single instance with BayLIME
#' @param explainer A `baylime_explainer` object.
#' @param data_row The instance to explain.
#' @param predict_fn Prediction function.
#' @param label Column of `predict_fn`'s output to explain.
#' @param num_features Number of features kept in the local model.
#' @param num_samples Number of perturbed samples.
#' @param seed Optional seed for reproducibility.
#' @return A list with the same structure as `lime_explain_instance()`, plus `local_std`, `coef_var`, `posterior_alpha` and `posterior_lambda`.
#' @export
baylime_explain_instance <- function(explainer,
                                     data_row,
                                     predict_fn,
                                     label = 2,
                                     num_features = 5,
                                     num_samples = 5000,
                                     seed = NULL) {
  run <- function() {
    x0 <- as.numeric(data_row)
    x0_scaled <- (x0 - explainer$mean) / explainer$scale

    scaled <- generate_perturbations(x0_scaled, num_samples,
                                     explainer$sample_around_instance)
    distances <- calculate_distances(scaled, x0_scaled)

    original <- unscale_data(scaled, explainer$mean, explainer$scale)
    original[1, ] <- x0
    colnames(original) <- explainer$feature_names
    yss <- predict_fn(original)
    y <- if (is.matrix(yss)) as.numeric(yss[, label]) else as.numeric(yss)

    weights <- lime_kernel(distances, explainer$kernel_width)

    used <- select_features(scaled, y, weights, num_features,
                            explainer$feature_selection)
    x_used <- scaled[, used, drop = FALSE]
    fit <- baylime_fit_local(x_used, y, weights, explainer$model_regressor,
                             explainer$prior_alpha, explainer$prior_lambda)

    x0_used <- x0_scaled[used]
    local_pred <- fit$intercept + sum(fit$coef * x0_used)
    local_std <- if (is.null(fit$sigma)) {
      0
    } else {
      sqrt(drop(t(x0_used) %*% fit$sigma %*% x0_used) + 1 / fit$alpha_post)
    }
    coef_var <- if (is.null(fit$sigma)) rep(0, length(used)) else diag(fit$sigma)

    ord <- order(abs(fit$coef), decreasing = TRUE)
    local_exp <- data.frame(
      feature_idx = used[ord],
      feature = explainer$feature_names[used[ord]],
      beta = fit$coef[ord],
      variance = coef_var[ord],
      stringsAsFactors = FALSE
    )

    list(
      intercept = fit$intercept,
      coef = fit$coef,
      used_features = used,
      feature_names = explainer$feature_names[used],
      local_exp = local_exp,
      score = weighted_r2(y, fit$fitted, weights),
      local_pred = local_pred,
      local_std = local_std,
      predicted_value = y[1],
      X = x_used,
      weights = weights,
      true_labels = y,
      alpha = fit$penalty,
      bayesian = !is.null(fit$sigma),
      coef_var = coef_var,
      posterior_alpha = fit$alpha_post,
      posterior_lambda = fit$lambda_post,
      distances = distances,
      kernel_width = explainer$kernel_width
    )
  }

  if (!is.null(seed)) with_local_seed(seed, run()) else run()
}