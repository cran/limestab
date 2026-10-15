# vsi.R

#' Variable Stability Index (VSI)
#' @param confidence_intervals List of named confidence-interval lists, one element per repeated LIME run.
#' @param index_verbose If `TRUE`, print the overall percentage.
#' @return A single numeric value between 0 and 100.
#' @details For a pair of runs with different numbers of selected variables, the
#'   overlap is normalised by the smaller selected set, avoiding dependence on the
#'   first run's feature count.
#' @export
compute_vsi <- function(confidence_intervals, index_verbose = FALSE) {
  if (!is.list(confidence_intervals) || length(confidence_intervals) < 2L) return(NaN)
  selected <- lapply(confidence_intervals, names)
  pairs <- utils::combn(length(selected), 2)
  pair_scores <- apply(pairs, 2, function(pr) {
    a <- selected[[pr[1]]]
    b <- selected[[pr[2]]]
    denom <- min(length(a), length(b))
    if (denom == 0L) return(NA_real_)
    length(intersect(a, b)) / denom
  })
  vsi <- round(mean(pair_scores, na.rm = TRUE) * 100, 2)
  if (index_verbose) {
    cat(sprintf("Percentage same variables across repeated LIME calls: %s%%\n", vsi))
  }
  vsi
}
