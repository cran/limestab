# cov.R

#' Coefficient-of-variation metrics across repeated runs
#' @param run_rankings List of data frames (`feature`, `beta`), one per run.
#' @param topk If not `NULL`, keep only the `topk` features with the largest median `abs(beta)`.
#' @param eps Small constant used to avoid division by zero.
#' @param missing_value Value used for a feature absent from a given run.
#' @return A list with `cov_results` (per feature) and the averages `cov_unscaled`, `cov_scaled`, `cov_iqr`, `median_sd`.
#' @export
compute_cov_metrics <- function(run_rankings, topk = NULL, eps = 1e-12,
                                missing_value = 0) {
  if (length(run_rankings) == 0) {
    return(list(cov_results = list(), cov_unscaled = NaN, cov_scaled = NaN,
                cov_iqr = NaN, median_sd = NaN))
  }

  features <- select_top_features(run_rankings, topk)
  B <- build_beta_matrix(run_rankings, features, missing_value)
  p <- ncol(B)

  m <- apply(B, 2, stats::median)
  mad <- apply(B, 2, function(v) stats::median(abs(v - stats::median(v))))

  q <- matrix(apply(B, 2, stats::quantile, probs = c(0.25, 0.75), names = FALSE, type = 7),
              nrow = 2)
  iqr <- q[2, ] - q[1, ]
  iqr_scaled <- iqr / 1.349

  q75_abs <- apply(abs(B), 2, stats::quantile, probs = 0.75, names = FALSE, type = 7)
  b <- ifelse(q75_abs > eps, 1 / q75_abs, NA_real_)

  cov_unscaled <- pmin(abs(mad / (m + eps)), 1)
  cov_scaled   <- pmin(abs((b * mad) / (m + eps)), 1)
  cov_iqr      <- pmin(abs(iqr_scaled / (m + eps)), 1)

  # Median-based standard deviation
  median_sd <- rep(NA_real_, p)
  for (j in seq_len(p)) {
    v <- B[, j]
    v <- v[!is.na(v)]
    if (length(v) > 1) median_sd[j] <- sqrt(sum((v - m[j])^2) / (length(v) - 1))
  }

  cov_results <- list()
  for (j in seq_len(p)) {
    cov_results[[features[j]]] <- list(
      median_beta = m[[j]], mad = mad[[j]], median_sd = median_sd[j],
      iqr = iqr[j], iqr_scaled = iqr_scaled[j],
      cov_unscaled = cov_unscaled[[j]], cov_scaled = cov_scaled[[j]],
      cov_iqr = cov_iqr[[j]]
    )
  }

  list(
    cov_results = cov_results,
    cov_unscaled = mean(cov_unscaled, na.rm = TRUE),
    cov_scaled = mean(cov_scaled, na.rm = TRUE),
    cov_iqr = mean(cov_iqr, na.rm = TRUE),
    median_sd = mean(median_sd, na.rm = TRUE)
  )
}
