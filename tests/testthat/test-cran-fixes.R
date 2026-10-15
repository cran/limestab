test_that("CSI treats touching closed intervals as overlapping", {
  ci <- list(
    list(a = c(0, 1)),
    list(a = c(1, 2))
  )
  expect_equal(compute_csi(ci), 100)
})

test_that("PCSI metrics stay on the 0-1 scale", {
  cov <- list(cov_unscaled = 0.2, cov_scaled = 0.4,
              cov_iqr = 0.3, median_sd = 2.5)
  pcsi <- compute_pcsi(cov)
  expect_true(all(unlist(pcsi) >= 0 & unlist(pcsi) <= 1))
  expect_equal(pcsi$PCSI_MSD, 0)
})

test_that("MRMS uses average ranks for tied absolute coefficients", {
  runs <- list(
    data.frame(feature = c("a", "b", "c"), beta = c(2, -2, 1)),
    data.frame(feature = c("a", "b", "c"), beta = c(-2, 2, 1))
  )
  out <- compute_mrms(runs, k = 3)
  expect_equal(unname(out$per_feature$a$median_rank), 1.5)
  expect_equal(unname(out$per_feature$b$median_rank), 1.5)
  expect_equal(out$mrms_all, 100)
})

test_that("Bayesian confidence intervals use posterior coefficient variance", {
  explanation <- list(
    bayesian = TRUE,
    coef = c(1, -1),
    coef_var = c(0.04, 0.09),
    feature_names = c("a", "b")
  )
  ci <- lime_confidence_intervals(explanation)
  expect_equal(ci$a, c(1 - 1.96 * 0.2, 1 + 1.96 * 0.2))
  expect_equal(ci$b, c(-1 - 1.96 * 0.3, -1 + 1.96 * 0.3))
})

test_that("kernel width validation rejects non-positive values", {
  expect_error(lime_kernel(c(0, 1), 0), "positive")
  expect_error(lime_kernel(c(0, 1), -1), "positive")
})

test_that("VSI remains bounded when runs select different numbers of features", {
  ci <- list(
    list(a = c(0, 1), b = c(0, 1), c = c(0, 1)),
    list(a = c(0, 1), b = c(0, 1))
  )
  vsi <- compute_vsi(ci)
  expect_true(is.finite(vsi))
  expect_true(vsi >= 0 && vsi <= 100)
})

test_that("ICSI returns percentage coverages on valid train/test splits", {
  set.seed(10)
  B <- matrix(rnorm(40), nrow = 10, ncol = 4)
  out <- compute_icsi(B, train_ratio = 0.8)
  expect_true(all(c("ICSI_MAD", "ICSI_rMAD", "ICSI_IQR", "ICSI_sMAD") %in% names(out)))
  vals <- vapply(out, function(x) x$overall_coverage, numeric(1))
  expect_true(all(vals >= 0 & vals <= 100))
})

test_that("BayLIME and ALIME produce finite local explanations", {
  set.seed(11)
  x <- matrix(rnorm(240), 80, 3, dimnames = list(NULL, c("a", "b", "c")))
  predict_fn <- function(m) 1.5 * m[, "a"] - 0.7 * m[, "b"]

  bex <- baylime_explainer(x, colnames(x), model_regressor = "Bay_info_prior",
                           prior_alpha = 1, prior_lambda = 10, random_state = 11)
  bfit <- baylime_explain_instance(bex, x[1, ], predict_fn,
                                   num_features = 3, num_samples = 300, seed = 11)
  expect_true(all(is.finite(bfit$coef)))
  expect_true(isTRUE(bfit$bayesian))
  expect_true(all(bfit$coef_var >= 0))

  aex <- alime_explainer(x, colnames(x), n_neighbors = 50, random_state = 11)
  afit <- alime_explain_instance(aex, x[1, ], predict_fn,
                                 num_features = 3, num_samples = 300, seed = 11)
  expect_true(all(is.finite(afit$coef)))
  expect_true(is.finite(afit$score))
})

test_that("OptiLIME completes a small optimisation", {
  set.seed(12)
  x <- matrix(rnorm(180), 60, 3, dimnames = list(NULL, c("a", "b", "c")))
  predict_fn <- function(m) m[, "a"] - 0.5 * m[, "b"]
  ex <- lime_explainer(x, colnames(x), feature_selection = "none", random_state = 12)
  out <- optilime(ex, x[1, ], predict_fn, bounds = c(0.2, 1.5),
                  n_iters = 2, n_pre_samples = 2, num_features = 3,
                  num_samples = 200, seed = 12)
  expect_true(is.finite(out$best_width))
  expect_true(out$best_width >= 0.2 && out$best_width <= 1.5)
})
