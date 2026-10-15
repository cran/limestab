test_that("lime_explain_instance recovers a known linear signal", {
  set.seed(1)
  n <- 300
  x <- matrix(rnorm(n * 3), n, 3, dimnames = list(NULL, c("a", "b", "c")))
  y <- 2 * x[, 1] - 1 * x[, 2] + 0.05 * rnorm(n)
  predict_fn <- function(m) 2 * m[, "a"] - 1 * m[, "b"]

  ex <- lime_explainer(x, feature_names = c("a", "b", "c"),
                        feature_selection = "none", penalty = 0.01,
                        random_state = 1)
  expl <- lime_explain_instance(ex, x[1, ], predict_fn,
                                 num_features = 3, num_samples = 2000, seed = 1)

  expect_true(expl$score > 0.9)
  top_feature <- expl$local_exp$feature[1]
  expect_true(top_feature %in% c("a", "b"))
})

test_that("lime_kernel is 1 at distance 0 and decreasing", {
  d <- c(0, 1, 2, 5)
  w <- lime_kernel(d, kernel_width = 1)
  expect_equal(w[1], 1)
  expect_true(all(diff(w) < 0))
})

test_that("fit_weighted_ridge matches lm for uniform weights and alpha = 0", {
  set.seed(2)
  x <- matrix(rnorm(200), 100, 2)
  y <- 1 + x %*% c(0.5, -0.3) + rnorm(100, sd = 0.01)
  w <- rep(1, 100)

  fit <- fit_weighted_ridge(x, y, w, alpha = 0)
  ref <- lm(y ~ x)

  expect_equal(unname(fit$intercept), unname(coef(ref)[1]), tolerance = 1e-6)
  expect_equal(unname(fit$coef), unname(coef(ref)[-1]), tolerance = 1e-6)
})

test_that("lime_check_stability returns metrics on a 0-100 / 0-1 scale", {
  set.seed(3)
  n <- 300
  x <- matrix(rnorm(n * 3), n, 3, dimnames = list(NULL, c("a", "b", "c")))
  predict_fn <- function(m) 2 * m[, "a"] - 1 * m[, "b"]

  ex <- lime_explainer(x, feature_names = c("a", "b", "c"),
                        feature_selection = "none", penalty = 1,
                        random_state = 42)
  stab <- lime_check_stability(ex, x[1, ], predict_fn,
                                num_features = 3, num_samples = 1000,
                                n_calls = 5, topk = 3, seed = 42)

  expect_true(is.numeric(stab$vsi))
  expect_true(stab$vsi >= 0 && stab$vsi <= 100)
  expect_true(stab$PCSI_MAD <= 1)
})
