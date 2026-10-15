# pcsi.R


# PCSI - Point-Based Coefficient Stability Index derived from the CoV metrics.
#' PCSI stability indices
#' @param cov The list returned by `compute_cov_metrics()`.
#' @return A list with `PCSI_MAD`, `PCSI_sMAD`, `PCSI_IQR` and `PCSI_MSD`.
#' @export
compute_pcsi <- function(cov) {
  clamp01 <- function(x) pmin(1, pmax(0, x))
  list(
    PCSI_MAD  = clamp01(1 - cov$cov_unscaled),
    PCSI_sMAD = clamp01(1 - cov$cov_scaled),
    PCSI_IQR  = clamp01(1 - cov$cov_iqr),
    PCSI_MSD  = clamp01(1 - cov$median_sd)
  )
}
