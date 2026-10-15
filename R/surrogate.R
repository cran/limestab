# surrogate.R

#Weighted R^2
#' Weighted R-squared
#' @param y Observed response.
#' @param fitted Fitted values from the local model.
#' @param w Sample weights.
#' @return A single numeric value between (typically) 0 and 1.
#' @export
weighted_r2 <- function(y, fitted, w) {
  ybar <- sum(w * y) / sum(w)
  ss_res <- sum(w * (y - fitted)^2)
  ss_tot <- sum(w * (y - ybar)^2)
  if (!is.finite(ss_tot) || ss_tot == 0) {
    return(if (is.finite(ss_res) && ss_res == 0) 1 else 0)
  }
  1 - ss_res / ss_tot
}

#Weighted Ridge regression with an unpenalised intercept.
#' Weighted ridge regression with an unpenalised intercept
#' @param x Numeric predictor matrix.
#' @param y Numeric response vector.
#' @param w Numeric vector of sample weights.
#' @param alpha Ridge penalty; `alpha = 0` is weighted least squares.
#' @return A list with `intercept`, `coef` and `fitted` (fitted values).
#' @export
fit_weighted_ridge <- function(x, y, w, alpha = 0) {
  x <- as.matrix(x)
  n <- nrow(x)
  p <- ncol(x)
  if (length(y) != n || length(w) != n) stop("x, y and w have incompatible lengths.")
  if (any(!is.finite(w)) || any(w < 0) || sum(w) <= 0) stop("w must contain finite non-negative weights with a positive sum.")
  if (length(alpha) != 1L || !is.finite(alpha) || alpha < 0) stop("alpha must be a non-negative finite number.")

  if (p == 0) {
    b0 <- sum(w * y) / sum(w)
    return(list(intercept = b0, coef = numeric(0), fitted = rep(b0, n)))
  }

  if (alpha == 0) {
    fit <- stats::lm.wfit(cbind(1, x), y, w)
    beta <- fit$coefficients
    beta[is.na(beta)] <- 0
  } else {
    xm <- colSums(w * x) / sum(w)
    ym <- sum(w * y) / sum(w)
    xc <- sweep(x, 2, xm)
    yc <- y - ym
    A <- crossprod(xc, xc * w) + alpha * diag(p)
    b <- as.numeric(tryCatch(
      solve(A, crossprod(xc, w * yc)),
      error = function(e) qr.solve(A, crossprod(xc, w * yc))
    ))
    beta <- c(ym - sum(xm * b), b)
  }

  list(
    intercept = unname(beta[1]),
    coef = as.numeric(beta[-1]),
    fitted = as.numeric(cbind(1, x) %*% beta)
  )
}

#Forward selection
forward_selection <- function(x, y, w, num_features) {
  p <- ncol(x)
  used <- integer(0)
  for (k in seq_len(min(num_features, p))) {
    candidates <- setdiff(seq_len(p), used)
    best <- candidates[1]
    best_score <- -1e8
    for (j in candidates) {
      fit <- fit_weighted_ridge(x[, c(used, j), drop = FALSE], y, w, alpha = 0)
      score <- weighted_r2(y, fit$fitted, w)
      if (score > best_score) {
        best <- j
        best_score <- score
      }
    }
    used <- c(used, best)
  }
  used
}

#Select the features used by the local model.
#' Select the features used by a LIME-style local model
#' @param x Numeric predictor matrix.
#' @param y Numeric response vector.
#' @param w Sample weights.
#' @param num_features Number of features to select.
#' @param method One of "auto", "forward_selection", "highest_weights" or "none".
#' @return An integer vector of selected column indices of `x`.
#' @export
select_features <- function(x, y, w, num_features, method = "auto") {
  p <- ncol(x)
  if (method == "none") return(seq_len(p))
  if (method == "auto") {
    method <- if (num_features <= 6) "forward_selection" else "highest_weights"
  }
  if (method == "forward_selection") {
    return(forward_selection(x, y, w, num_features))
  }
  if (method == "highest_weights") {
    fit <- fit_weighted_ridge(x, y, w, alpha = 0.01)
    ord <- order(abs(fit$coef * x[1, ]), decreasing = TRUE)
    return(ord[seq_len(min(num_features, p))])
  }
  stop("Unsupported feature_selection '", method,
       "'. Use auto, forward_selection, highest_weights or none.")
}
