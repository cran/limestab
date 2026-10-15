# csi.R

intervals_overlap <- function(i1, i2) i1[1] <= i2[2] && i2[1] <= i1[2]
#' Coefficient Stability Index (CSI)
#' @param confidence_intervals List of named confidence-interval lists, one element per repeated LIME run.
#' @param index_verbose If `TRUE`, print the per-feature overlap percentage.
#' @return A single numeric value between 0 and 100.
#' @export
compute_csi <- function(confidence_intervals, index_verbose = FALSE) {
  features <- unique(unlist(lapply(confidence_intervals, names)))

  partial_indices <- numeric(0)
  for (feat in features) {
    ints <- Filter(Negate(is.null),
                   lapply(confidence_intervals, function(ci) ci[[feat]]))
    if (length(ints) < 2) next

    pairs <- utils::combn(length(ints), 2)
    overlaps <- apply(pairs, 2, function(pr) {
      intervals_overlap(ints[[pr[1]]], ints[[pr[2]]])
    })
    frac <- round(mean(overlaps) * 100, 2)
    partial_indices <- c(partial_indices, frac)

    if (index_verbose) {
      cat(sprintf("Percentage of overlapping confidence intervals, variable %s: %s%%\n",
                  feat, frac))
    }
  }

  round(mean(partial_indices), 2)
}