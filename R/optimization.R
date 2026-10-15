# optimization.R

# Gaussian process
matern32_kernel <- function(x1, x2, length_scale) {
  r <- abs(outer(x1, x2, "-")) * sqrt(3) / length_scale
  (1 + r) * exp(-r)
}

gp_log_marginal_likelihood <- function(log_ls, x, y, alpha) {
  K <- matern32_kernel(x, x, exp(log_ls)) + diag(alpha, length(x))
  L <- tryCatch(chol(K), error = function(e) NULL)
  if (is.null(L)) return(-Inf)
  z <- backsolve(L, forwardsolve(t(L), y))
  -0.5 * sum(y * z) - sum(log(diag(L))) - 0.5 * length(x) * log(2 * pi)
}

#' Fit a 1-D Gaussian process (Matern 3/2 kernel)
#' @param x Numeric vector of training inputs (kernel widths already evaluated).
#' @param y Numeric vector of observed values (the loss at each `x`).
#' @param alpha Observation noise variance added to the kernel diagonal.
#' @return An object of class `optilime_gp`.
#' @export
fit_gp <- function(x, y, alpha = 1e-6) {
  x <- as.numeric(x)
  y <- as.numeric(y)

  neg_lml <- function(l) {
    v <- -gp_log_marginal_likelihood(l, x, y, alpha)
    if (is.finite(v)) v else 1e10
  }
  ls <- tryCatch(
    exp(stats::optim(par = 0, fn = neg_lml, method = "L-BFGS-B",
              lower = log(1e-5), upper = log(1e5))$par),
    error = function(e) 1
  )

  K <- matern32_kernel(x, x, ls) + diag(alpha, length(x))
  L <- tryCatch(chol(K), error = function(e) chol(K + diag(1e-8, length(x))))

  structure(
    list(x = x, y = y, ls = ls, alpha = alpha, L = L,
         K_inv_y = backsolve(L, forwardsolve(t(L), y))),
    class = "optilime_gp"
  )
}

#' Predict method for `optilime_gp` objects
#' @param object An `optilime_gp` object returned by `fit_gp()`.
#' @param newdata Numeric vector of new points.
#' @param return_std If `TRUE`, also return the predictive standard deviation.
#' @param ... Unused; included for S3 consistency.
#' @return A numeric vector of means, or a list with `mean` and `sd` when `return_std = TRUE`.
#' @export
predict.optilime_gp <- function(object, newdata, return_std = FALSE, ...) {
  Ks <- matern32_kernel(as.numeric(newdata), object$x, object$ls)
  mu <- as.numeric(Ks %*% object$K_inv_y)
  if (!return_std) return(mu)

  v <- backsolve(object$L, forwardsolve(t(object$L), t(Ks)))
  var <- 1 - colSums(t(Ks) * v)
  list(mean = mu, sd = sqrt(pmax(var, 0)))
}
# Acquisition function
expected_improvement <- function(x, y_sample, gp, xi = 0.01) {
  pr <- stats::predict(gp, x, return_std = TRUE)
  mu <- pr$mean
  sigma <- pr$sd
  best <- max(y_sample)

  imp <- mu - best - xi
  z <- ifelse(sigma > 0, imp / sigma, 0)
  ei <- imp * stats::pnorm(z) + sigma * stats::dnorm(z)
  ei[sigma == 0] <- 0
  ei
}

propose_location <- function(y_sample, gp, bounds, n_restarts = 25) {
  best_val <- Inf
  best_x <- NA_real_

  for (x0 in stats::runif(n_restarts, bounds[1], bounds[2])) {
    res <- tryCatch(
      stats::optim(par = x0,
            fn = function(x) -expected_improvement(x, y_sample, gp),
            method = "L-BFGS-B", lower = bounds[1], upper = bounds[2]),
      error = function(e) NULL
    )
    if (!is.null(res) && is.finite(res$value) && res$value < best_val) {
      best_val <- res$value
      best_x <- res$par
    }
  }
  if (is.na(best_x)) best_x <- stats::runif(1, bounds[1], bounds[2])
  best_x
}

# Bayesian optimisation (maximisation)

#' Bayesian optimisation of a scalar objective
#' @param n_iters Number of Bayesian-optimisation iterations after the initial design.
#' @param sample_loss Function of one scalar returning the value to maximise.
#' @param bounds `c(lower, upper)` search bounds.
#' @param n_pre_samples Number of random initial evaluations.
#' @param random_state Seed for the whole procedure.
#' @param verbose If `TRUE`, print progress.
#' @return A list with `x_sample` and `y_sample`, every point evaluated.
#' @export
bayesian_optimisation <- function(n_iters, sample_loss, bounds,
                                  n_pre_samples = 5, random_state = 42,
                                  verbose = FALSE) {
  with_local_seed(random_state, {
    x_sample <- stats::runif(n_pre_samples, bounds[1], bounds[2])
    y_sample <- vapply(x_sample, sample_loss, numeric(1))

    for (i in seq_len(n_iters)) {
      gp <- fit_gp(x_sample, y_sample)
      next_x <- propose_location(y_sample, gp, bounds)
      next_y <- sample_loss(next_x)
      x_sample <- c(x_sample, next_x)
      y_sample <- c(y_sample, next_y)
      if (verbose) {
        cat(sprintf("iter %2d  kernel width = %.4f  loss = %.4f\n", i, next_x, next_y))
      }
    }
    list(x_sample = x_sample, y_sample = y_sample)
  })
}
