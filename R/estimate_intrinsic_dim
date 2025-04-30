#' estimate_intrinsic_dim
#'
#' Estimate the intrinsic dimensionality of a latent time series representation.
#'
#' @param latent A numeric matrix where rows are observations and columns are features.
#' @param k The number of nearest neighbors to use (default = 10).
#'
#' @return A list with estimated intrinsic dimension and local profile (per point).
#' @export

estimate_intrinsic_dim <- function(latent, k = 10) {
  if (!requireNamespace("FNN", quietly = TRUE)) stop("Please install the FNN package for nearest neighbor estimation.")
  if (!is.matrix(latent)) latent <- as.matrix(latent)

  library(FNN)
  n <- nrow(latent)
  dists <- FNN::get.knn(latent, k = k)$nn.dist

  log_ratios <- log(dists[, k] / dists[, 1])
  id_local <- (k - 2) / rowSums(log_ratios)
  id_global <- mean(id_local, na.rm = TRUE)

  return(list(global_dim = id_global, local_dims = id_local))
}
