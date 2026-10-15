# kernel.R

#LIME kernel: sqrt(exp(-d^2 / width^2)).
#' LIME exponential kernel
#' @param distances Numeric vector of distances to the instance being explained.
#' @param kernel_width Positive numeric kernel width.
#' @return Numeric vector of proximity weights, `sqrt(exp(-distances^2 / kernel_width^2))`.
#' @export
lime_kernel <- function(distances, kernel_width) {
  if (length(kernel_width) != 1L || !is.finite(kernel_width) || kernel_width <= 0) {
    stop("kernel_width must be a single positive finite number.")
  }
  sqrt(exp(-(distances^2) / kernel_width^2))
}

#Default LIME kernel width for tabular data: 0.75 * sqrt(number of features).
#' Default LIME kernel width for tabular data
#' @param n_features Number of features in the data set.
#' @return A single numeric value, `0.75 * sqrt(n_features)`.
#' @export
default_kernel_width <- function(n_features) {
  if (length(n_features) != 1L || !is.finite(n_features) || n_features <= 0) {
    stop("n_features must be a single positive finite number.")
  }
  sqrt(n_features) * 0.75
}
