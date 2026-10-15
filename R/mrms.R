# mrms.R

rank_one_run <- function(run, features) {
  out <- rep(NA_real_, length(features))
  names(out) <- features
  if (is.null(run) || nrow(run) == 0L) return(out)
  if (!all(c("feature", "beta") %in% names(run))) {
    stop("Each run must contain 'feature' and 'beta' columns.")
  }
  keep <- !duplicated(run$feature) & run$feature %in% features
  rr <- run[keep, , drop = FALSE]
  if (nrow(rr) == 0L) return(out)
  # Rank by decreasing absolute coefficient.  Average ranks are assigned to ties,
  # matching the operational definition used in the manuscript.
  rk <- rank(-abs(rr$beta), ties.method = "average", na.last = "keep")
  out[rr$feature] <- rk
  out
}

#' Median Rank Matching Stability of features across repeated runs
#' @param run_rankings List of data frames (`feature`, `beta`), one per run.
#' @param k Number of top features used for the explanation-based top-k score.
#' @return A list with `per_feature`, `mrms_all` and `mrms_topk`. Scores are percentages.
#' @details Features are ranked by decreasing absolute coefficient. Tied absolute
#'   coefficients receive the average of the rank positions they occupy. A missing
#'   feature is not assigned an artificial rank; it contributes zero to the matching
#'   score for that run. MRMS-All averages the resulting binary matches over all
#'   features and runs.
#' @export
compute_mrms <- function(run_rankings, k = 5) {
  if (!is.list(run_rankings) || length(run_rankings) == 0L) {
    return(list(per_feature = list(), mrms_all = NaN, mrms_topk = NaN))
  }
  if (length(k) != 1L || !is.finite(k) || k < 1) stop("k must be a positive number.")

  feats <- all_run_features(run_rankings)
  if (length(feats) == 0L) {
    return(list(per_feature = list(), mrms_all = NaN, mrms_topk = NaN))
  }
  rank_matrix <- t(vapply(run_rankings, rank_one_run, numeric(length(feats)), features = feats))
  colnames(rank_matrix) <- feats

  med <- apply(rank_matrix, 2, stats::median, na.rm = TRUE)
  med[!is.finite(med)] <- NA_real_
  lower <- med - 0.5
  upper <- med + 0.5

  score_matrix <- matrix(0, nrow = nrow(rank_matrix), ncol = ncol(rank_matrix),
                         dimnames = dimnames(rank_matrix))
  for (j in seq_along(feats)) {
    # Missing features remain zero rather than receiving an artificial rank.
    if (is.finite(med[j])) {
      present <- is.finite(rank_matrix[, j])
      score_matrix[present, j] <- as.numeric(rank_matrix[present, j] >= lower[j] &
                                              rank_matrix[present, j] <= upper[j])
    }
  }

  per_feature <- lapply(seq_along(feats), function(j) {
    list(median_rank = med[j], lower = lower[j], upper = upper[j],
         stability_score = mean(score_matrix[, j]) * 100)
  })
  names(per_feature) <- feats

  mrms_all <- mean(score_matrix) * 100

  topk_values <- numeric(0)
  kk <- min(as.integer(k), length(feats))
  for (i in seq_len(nrow(rank_matrix))) {
    present <- which(is.finite(rank_matrix[i, ]))
    if (!length(present)) next
    ord <- present[order(rank_matrix[i, present], feats[present])]
    chosen <- ord[seq_len(min(kk, length(ord)))]
    topk_values <- c(topk_values, mean(score_matrix[i, chosen]) * 100)
  }
  mrms_topk <- if (length(topk_values)) mean(topk_values) else NaN

  list(per_feature = per_feature, mrms_all = mrms_all, mrms_topk = mrms_topk)
}
