#' compare_latent_models
#'
#' Compare two latent representations of time series using geometric similarity measures.
#' Measures structural divergence using average pairwise distances or projection-based similarity.
#'
#' @param latent1 A numeric matrix of latent vectors (model 1).
#' @param latent2 A numeric matrix of latent vectors (model 2). Must be same dimension.
#' @param method Distance method: \"cosine\", \"euclidean\", or \"procrustes\".
#' @param align Whether to align via Procrustes before comparing (default = FALSE).
#'
#' @return A list with similarity score, method used, and optional aligned matrices.
#' @export

compare_latent_models <- function(latent1, latent2, method = "cosine", align = FALSE) {
  if (!is.matrix(latent1)) latent1 <- as.matrix(latent1)
  if (!is.matrix(latent2)) latent2 <- as.matrix(latent2)

  if (!all(dim(latent1) == dim(latent2))) {
    stop("Latent matrices must have the same dimensions.")
  }

  if (align || method == "procrustes") {
    if (!requireNamespace("vegan", quietly = TRUE)) stop("Install the 'vegan' package for Procrustes alignment.")
    proc <- vegan::procrustes(latent1, latent2, scale = TRUE)
    latent1 <- proc$Yrot
    latent2 <- proc$X
  }

  # Compute similarity score
  if (method == "cosine") {
    cosine_sim <- function(a, b) sum(a * b) / (sqrt(sum(a^2)) * sqrt(sum(b^2)) + 1e-8)
    scores <- mapply(cosine_sim, split(latent1, row(latent1)), split(latent2, row(latent2)))
    score <- mean(scores, na.rm = TRUE)  # higher = more similar
  } else if (method == "euclidean") {
    diffs <- latent1 - latent2
    dists <- sqrt(rowSums(diffs^2))
    score <- mean(dists, na.rm = TRUE)  # lower = more similar
  } else if (method == "procrustes") {
    score <- proc$ss  # sum of squared errors (lower is better)
  } else {
    stop("Unsupported method. Choose from 'cosine', 'euclidean', or 'procrustes'.")
  }

  return(list(
    score = score,
    method = method,
    aligned = if (align) list(latent1 = latent1, latent2 = latent2) else NULL
  ))
}
