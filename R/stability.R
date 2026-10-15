# stability.R

all_run_features <- function(run_rankings) {
  sort(unique(unlist(lapply(run_rankings, function(r) r$feature))), method = "radix")
}

select_top_features <- function(run_rankings, topk = NULL) {
  all_feats <- all_run_features(run_rankings)
  if (is.null(topk)) return(all_feats)
  med_abs <- vapply(all_feats, function(f) {
    vals <- unlist(lapply(run_rankings, function(r) abs(r$beta[r$feature == f])))
    if (length(vals)) stats::median(vals) else 0
  }, numeric(1))
  all_feats[order(-med_abs)][seq_len(min(topk, length(all_feats)))]
}

build_beta_matrix <- function(run_rankings, features, missing_value = 0) {
  B <- matrix(missing_value, nrow = length(run_rankings), ncol = length(features),
              dimnames = list(NULL, features))
  for (r in seq_along(run_rankings)) {
    run <- run_rankings[[r]]
    keep <- run$feature %in% features
    B[r, run$feature[keep]] <- run$beta[keep]
  }
  B
}

#' Repeat an explanation and compute every stability metric
#' @param explainer An explainer object (from `lime_explainer()`, `baylime_explainer()` or `alime_explainer()`).
#' @param data_row The instance to explain.
#' @param predict_fn Prediction function passed on to `explain_fn`.
#' @param label Column of `predict_fn`'s output to explain.
#' @param num_features Number of features kept in each individual explanation.
#' @param num_samples Number of perturbed samples per explanation.
#' @param n_calls Number of repeated explanations.
#' @param index_verbose Passed on to `compute_csi()` / `compute_vsi()`.
#' @param topk Number of top features used for the top-k metrics.
#' @param train_ratio Share of runs used as the reference set for `compute_icsi()`.
#' @param seed Seed for the whole procedure; the global RNG state is restored afterwards.
#' @param explain_fn Function used to explain one instance, e.g. `lime_explain_instance`, `baylime_explain_instance` or `alime_explain_instance`.
#' @return A list with `csi`, `vsi`, `mrms_all`, `mrms_topk`, `PCSI_MAD`/`PCSI_sMAD`/`PCSI_IQR`/`PCSI_MSD`, `interval_results` (ICSI) and the raw `run_rankings` / `confidence_intervals`.
#' @export
lime_check_stability <- function(explainer,
                                 data_row,
                                 predict_fn,
                                 label = 2,
                                 num_features = 5,
                                 num_samples = 5000,
                                 n_calls = 10,
                                 index_verbose = FALSE,
                                 topk = 5,
                                 train_ratio = 0.8,
                                 seed = explainer$random_state,
                                 explain_fn = lime_explain_instance) {
  body <- function() {
    if (length(n_calls) != 1L || !is.finite(n_calls) || n_calls < 2) stop("n_calls must be at least 2.")
    if (length(num_features) != 1L || !is.finite(num_features) || num_features < 1) stop("num_features must be positive.")
    if (length(num_samples) != 1L || !is.finite(num_samples) || num_samples < 2) stop("num_samples must be at least 2.")
    if (length(topk) != 1L || !is.finite(topk) || topk < 1) stop("topk must be positive.")
    if (length(train_ratio) != 1L || !is.finite(train_ratio) || train_ratio <= 0 || train_ratio >= 1) {
      stop("train_ratio must be strictly between 0 and 1.")
    }
    p <- length(explainer$feature_names)
    confidence_intervals <- list()
    run_rankings <- vector("list", n_calls)

    for (i in seq_len(n_calls)) {
      expl <- explain_fn(explainer, data_row, predict_fn, label,
                         num_features, num_samples)
      ci <- lime_confidence_intervals(expl)
      if (length(ci) > 0) {
        confidence_intervals[[length(confidence_intervals) + 1]] <- ci
      }

      expl_full <- explain_fn(explainer, data_row, predict_fn, label,
                              p, num_samples)
      run_rankings[[i]] <- expl_full$local_exp[, c("feature", "beta")]
    }

    # MRMS
    mrms <- compute_mrms(run_rankings, k = num_features)

    # CSI and VSI
    reps <- length(confidence_intervals)
    if (reps < 2) {
      message("Cannot compute CSI/VSI: the kernel width and Ridge penalty were too small. ",
              "No sampled unit got a meaningful weight in more than one repetition.")
      csi <- NaN
      vsi <- NaN
    } else {
      if (reps != n_calls) {
        message("Some repetitions were not meaningful (no unit had a meaningful weight). ",
                "Indices were computed on ", reps, " repetitions.")
      }
      n_feat <- vapply(confidence_intervals, length, integer(1))
      if (floor(stats::median(n_feat)) != min(n_feat) && min(n_feat) != max(n_feat)) {
        message("Warning: LIME variable selection returned a different number of ",
                "variables across explanations; CSI/VSI may be inaccurate.")
      }
      csi <- compute_csi(confidence_intervals, index_verbose)
      vsi <- compute_vsi(confidence_intervals, index_verbose)
    }

    # CoV / PCSI and ICSI on the coefficient matrix
    cov <- compute_cov_metrics(run_rankings, topk = topk)
    pcsi <- compute_pcsi(cov)
    selected <- select_top_features(run_rankings, topk)
    B <- build_beta_matrix(run_rankings, selected, 0)
    icsi <- compute_icsi(B, train_ratio = train_ratio)

    list(
      csi = csi,
      vsi = vsi,
      mrms_per_feature = mrms$per_feature,
      mrms_all = mrms$mrms_all,
      mrms_topk = mrms$mrms_topk,
      cov_per_feature = cov$cov_results,
      PCSI_MAD = pcsi$PCSI_MAD,
      PCSI_sMAD = pcsi$PCSI_sMAD,
      PCSI_IQR = pcsi$PCSI_IQR,
      PCSI_MSD = pcsi$PCSI_MSD,
      repetitions_used = reps,
      run_rankings = run_rankings,
      confidence_intervals = confidence_intervals,
      n_calls = n_calls,
      num_features = num_features,
      topk_used = topk,
      interval_results = icsi,
      interval_feature_names = selected
    )
  }

  if (!is.null(seed)) with_local_seed(seed, body()) else body()
}