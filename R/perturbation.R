# perturbation.R

with_local_seed <- function(seed, expr) {
  env <- globalenv()
  had_seed <- exists(".Random.seed", envir = env, inherits = FALSE)
  if (had_seed) old_seed <- get(".Random.seed", envir = env, inherits = FALSE)
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = env)
    } else if (exists(".Random.seed", envir = env, inherits = FALSE)) {
      rm(".Random.seed", envir = env)
    }
  }, add = TRUE)
  set.seed(seed)
  expr
}

scale_data <- function(x, center, scale) {
  sweep(sweep(as.matrix(x), 2, center, "-"), 2, scale, "/")
}

unscale_data <- function(scaled, center, scale) {
  sweep(sweep(as.matrix(scaled), 2, scale, "*"), 2, center, "+")
}

#' Generate a perturbed neighbourhood in standardised feature space
#' @param instance_scaled Standardised instance (numeric vector).
#' @param num_samples Number of perturbed samples to generate; row 1 is the instance itself.
#' @param sample_around_instance If `TRUE`, sample around the instance; otherwise around the origin (the training-data mean).
#' @return A numeric matrix (`num_samples` x `length(instance_scaled)`) in standardised space.
#' @export
generate_perturbations <- function(instance_scaled,
                                   num_samples = 5000,
                                   sample_around_instance = TRUE) {
  p <- length(instance_scaled)
  z <- matrix(stats::rnorm(num_samples * p), nrow = num_samples, ncol = p)
  scaled <- if (sample_around_instance) sweep(z, 2, instance_scaled, "+") else z
  scaled[1, ] <- instance_scaled
  scaled
}

#' Euclidean distance of every row to an instance
#' @param scaled_data Numeric matrix in standardised space.
#' @param instance_scaled Standardised instance to measure distances from.
#' @return A numeric vector of Euclidean distances.
#' @export
calculate_distances <- function(scaled_data, instance_scaled) {
  sqrt(rowSums(sweep(as.matrix(scaled_data), 2, instance_scaled, "-")^2))
}
