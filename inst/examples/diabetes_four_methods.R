# ==============================================================================
# diabetes_four_methods.R
# Compare LIME, OptiLIME, BayLIME and ALIME on one instance of the Pima
# Indians Diabetes data set, using the same stability metrics.
#
# Run with:
#   source(system.file("examples", "diabetes_four_methods.R", package = "limestab"))
# (requires the xgboost package: install.packages("xgboost"))
# ==============================================================================

library(limestab)
if (!requireNamespace("xgboost", quietly = TRUE)) {
  stop("This example needs the xgboost package: install.packages(\"xgboost\")")
}

data_path <- system.file("extdata", "diabetes.csv", package = "limestab")

df <- utils::read.csv(data_path)
y <- df$Outcome
X <- as.matrix(df[, setdiff(names(df), "Outcome")])
feature_names <- colnames(X)
for (j in seq_len(ncol(X))) X[is.na(X[, j]), j] <- mean(X[, j], na.rm = TRUE)

set.seed(42)
test_idx <- sample.int(nrow(X), ceiling(0.2 * nrow(X)))
X_train <- X[-test_idx, ]; y_train <- y[-test_idx]
X_test  <- X[test_idx, ]

valid_idx <- sample.int(nrow(X_train), ceiling(0.2 * nrow(X_train)))
X_strain <- X_train[-valid_idx, ]; y_strain <- y_train[-valid_idx]

model <- xgboost::xgb.train(
  params = list(objective = "binary:logistic", max_depth = 4, eta = 0.05,
                subsample = 0.8, colsample_bytree = 0.8, lambda = 1, seed = 42),
  data = xgboost::xgb.DMatrix(X_strain, label = y_strain),
  nrounds = 300, verbose = 0
)

predict_fn <- function(m) {
  m <- as.matrix(m)
  colnames(m) <- feature_names
  as.numeric(stats::predict(model, m))
}

test_instance <- X_test[4, ]

# ---- Explainers --------------------------------------------------------------
lime_ex <- lime_explainer(X_strain, feature_names,
                           feature_selection = "forward_selection",
                           sample_around_instance = TRUE,
                           penalty = 1, random_state = 42)

opt <- optilime(
  lime_explainer(X_strain, feature_names,
                 feature_selection = "forward_selection",
                 sample_around_instance = TRUE,
                 penalty = 0, random_state = 42),
  test_instance, predict_fn,
  bounds = c(0.1, 3.0), n_iters = 30, n_pre_samples = 10,
  num_features = 5, num_samples = 5000, max_r2 = 0.9, seed = 42
)
cat(sprintf("OptiLIME kernel width: %.4f (R2 = %.4f)\n", opt$best_width, opt$r2))

bay_ex <- baylime_explainer(X_strain, feature_names,
                             kernel_width = opt$best_width,
                             feature_selection = "forward_selection",
                             sample_around_instance = TRUE,
                             model_regressor = "Bay_info_prior",
                             prior_alpha = 1, prior_lambda = 1000,
                             random_state = 42)

alime_ex <- alime_explainer(X_strain, feature_names,
                             n_neighbors = 300,
                             feature_selection = "forward_selection",
                             penalty = 1, standardize = TRUE,
                             random_state = 42)

# ---- Stability metrics --------------------------------------------------------
# The package returns CSI, VSI, MRMS and ICSI as percentages (0-100), whereas
# PCSI metrics are returned on a 0-1 scale.  The table below converts percentage
# metrics to 0-1 so all displayed columns share the same scale.
run_stability <- function(explainer, explain_fn, num_samples) {
  lime_check_stability(explainer, test_instance, predict_fn,
                        num_features = 5, num_samples = num_samples,
                        n_calls = 10, topk = 5, seed = 42,
                        explain_fn = explain_fn)
}

metric_row <- function(method, stab) {
  ir <- stab$interval_results
  data.frame(
    Method = method,
    CSI = stab$csi / 100, VSI = stab$vsi / 100,
    MRMS_All = stab$mrms_all / 100, MRMS_TopK = stab$mrms_topk / 100,
    PCSI_MAD = stab$PCSI_MAD, PCSI_sMAD = stab$PCSI_sMAD,
    PCSI_IQR = stab$PCSI_IQR, PCSI_MSD = stab$PCSI_MSD,
    ICSI_MAD = ir$ICSI_MAD$overall_coverage / 100,
    ICSI_rMAD = ir$ICSI_rMAD$overall_coverage / 100,
    ICSI_IQR = ir$ICSI_IQR$overall_coverage / 100,
    ICSI_sMAD = ir$ICSI_sMAD$overall_coverage / 100
  )
}

results <- rbind(
  metric_row("LIME",     run_stability(lime_ex, lime_explain_instance, 5000)),
  metric_row("OptiLIME", run_stability(opt$explainer, lime_explain_instance, 5000)),
  metric_row("BayLIME",  run_stability(bay_ex, baylime_explain_instance, 5000)),
  metric_row("ALIME",    run_stability(alime_ex, alime_explain_instance, 10000))
)
num_cols <- setdiff(names(results), "Method")
results[num_cols] <- lapply(results[num_cols], round, 4)

cat("\nStability metrics by method\n")
print(results, row.names = FALSE)
