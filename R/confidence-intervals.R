# confidence-intervals.R

#' Standard deviations of weighted ridge coefficients
#' @param x Numeric predictor matrix used by the local model.
#' @param y Numeric response vector.
#' @param weights Sample weights.
#' @param alpha Ridge penalty used when fitting the local model.
#' @return A numeric vector of approximate coefficient standard deviations.
#' @export
compute_wls_stdevs <- function(x, y, weights, alpha) {
  x <- as.matrix(x)
  n <- nrow(x)
  p <- ncol(x)
  if (n <= p + 1L) stop("Not enough observations to estimate coefficient uncertainty.")
  if (length(y) != n || length(weights) != n) stop("x, y and weights have incompatible lengths.")
  if (any(!is.finite(weights)) || any(weights < 0)) stop("weights must be finite and non-negative.")

  fit <- stats::lm.wfit(cbind(1, x), y, weights)
  weighted_resid <- sqrt(weights) * fit$residuals
  # Intercept plus p coefficient parameters are fitted.  For ridge fits this is
  # a conservative approximation to the residual degrees of freedom.
  df_resid <- n - p - 1L
  sigma2 <- sum(weighted_resid^2) / df_resid

  xtwx <- crossprod(x, x * weights)
  partial <- solve(xtwx + alpha * diag(p))
  variance_matrix <- sigma2 * partial %*% xtwx %*% t(partial)
  sqrt(pmax(diag(variance_matrix), 0))
}

#' Robust standard deviation from repeated coefficients
#' @param beta_list Matrix of coefficients, one row per repeated explanation.
#' @return A numeric vector, `1.4826 * MAD` for each column.
#' @export
compute_mad_stdevs <- function(beta_list) {
  betas <- as.matrix(beta_list)
  1.4826 * apply(betas, 2, function(v) stats::median(abs(v - stats::median(v))))
}

build_confidence_intervals <- function(means, st_devs, feature_names) {
  conf_int <- list()
  for (i in seq_along(feature_names)) {
    conf_int[[feature_names[i]]] <- c(means[i] - 1.96 * st_devs[i],
                                      means[i] + 1.96 * st_devs[i])
  }
  conf_int
}

#' Confidence intervals for a LIME-style explanation
#' @param explanation The list returned by `lime_explain_instance()` or `baylime_explain_instance()`.
#' @return A named list of `c(lower, upper)` intervals, or `NULL` if uncertainty cannot be estimated.
#' @details For Bayesian BayLIME fits, intervals are built from the posterior coefficient covariance.
#'   For LIME, ALIME and non-Bayesian BayLIME fits, approximate weighted-ridge standard errors are used.
#' @export
lime_confidence_intervals <- function(explanation) {
  if (isTRUE(explanation$bayesian) && !is.null(explanation$coef_var)) {
    if (length(explanation$coef_var) != length(explanation$coef) ||
        any(!is.finite(explanation$coef_var)) || any(explanation$coef_var < 0)) {
      return(NULL)
    }
    stdevs <- sqrt(pmax(explanation$coef_var, 0))
  } else {
    stdevs <- tryCatch(
      compute_wls_stdevs(explanation$X, explanation$true_labels,
                         explanation$weights, explanation$alpha),
      error = function(e) NULL
    )
  }
  if (is.null(stdevs)) return(NULL)
  build_confidence_intervals(explanation$coef, stdevs, explanation$feature_names)
}
