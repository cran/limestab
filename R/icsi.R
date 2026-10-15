# icsi.R

#' Interval-Based Coefficient Stability Index (ICSI)
#' @param B Coefficient matrix (runs x features).
#' @param train_ratio Share of runs used to build the reference interval.
#' @param eps Small constant used to avoid division by zero.
#' @return A named list of coverage results (`ICSI_MAD`, `ICSI_rMAD`, `ICSI_IQR`, `ICSI_sMAD`), each with `coverage_per_feature` and `overall_coverage`.
#' @export
compute_icsi <- function(B, train_ratio = 0.8, eps = 1e-12) {
  n_runs <- nrow(B)
  n_train <- floor(n_runs * train_ratio)
  if (n_train < 1 || n_train >= n_runs) return(list())

  idx <- sample.int(n_runs)
  B_train <- B[idx[seq_len(n_train)], , drop = FALSE]
  B_test  <- B[idx[-seq_len(n_train)], , drop = FALSE]

  m <- apply(B_train, 2, stats::median)
  mad <- apply(B_train, 2, function(v) stats::median(abs(v - stats::median(v))))

  coverage <- function(lo, hi) {
    inside <- (t(B_test) >= (lo - eps)) & (t(B_test) <= (hi + eps))
    list(coverage_per_feature = rowMeans(inside) * 100,
         overall_coverage = mean(inside) * 100)
  }

  results <- list()
  results$ICSI_MAD <- coverage(m - 3 * mad, m + 3 * mad)

  d_rmad <- 1.4826 * mad
  results$ICSI_rMAD <- coverage(m - d_rmad, m + d_rmad)

  q <- matrix(apply(B_train, 2, stats::quantile, probs = c(0.25, 0.75), names = FALSE),
              nrow = 2)
  d_iqr <- (q[2, ] - q[1, ]) / 1.349
  results$ICSI_IQR <- coverage(m - d_iqr, m + d_iqr)

  q75_abs <- apply(abs(B_train), 2, stats::quantile, probs = 0.75, names = FALSE)
  b <- ifelse(q75_abs > eps, 1 / q75_abs, 0)
  d_smad <- b * mad
  results$ICSI_sMAD <- coverage(m - 3 * d_smad, m + 3 * d_smad)

  results
}
