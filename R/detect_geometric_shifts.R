#' detect_geometric_shifts
#'
#' Detect geometric regime shifts in a latent representation of a time series.
#' Based on changes in local angles, vector norms, or trajectory curvature.
#'
#' @param latent A numeric matrix of latent representations (from learn_structure).
#' @param method Character: "angle", "curvature", or "norm_diff". Default = "angle".
#' @param threshold Numeric value for detecting significant changes.
#' @param window Integer: rolling window size for smoothing (optional).
#'
#' @return A list with shift_indices, scores, and method used.
#' @export

detect_geometric_shifts <- function(latent, method = "angle", threshold = NULL, window = 5) {
  if (!is.matrix(latent)) latent <- as.matrix(latent)
  n <- nrow(latent)
  p <- ncol(latent)

  if (n < 3) stop("Need at least 3 time points to compute geometry.")

  # Normalize each row to unit vector
  normalize_rows <- function(x) x / sqrt(rowSums(x^2) + 1e-8)
  latent_norm <- normalize_rows(latent)

  score <- rep(NA, n)

  if (method == "angle") {
    # Compute cosine angle change between consecutive vectors
    for (i in 2:(n - 1)) {
      v1 <- latent_norm[i, ] - latent_norm[i - 1, ]
      v2 <- latent_norm[i + 1, ] - latent_norm[i, ]
      angle <- acos(sum(v1 * v2) / (sqrt(sum(v1^2)) * sqrt(sum(v2^2)) + 1e-8))
      score[i] <- angle
    }
  } else if (method == "curvature") {
    for (i in 2:(n - 1)) {
      a <- latent[i - 1, ]
      b <- latent[i, ]
      c <- latent[i + 1, ]
      ab <- b - a
      bc <- c - b
      curvature <- sqrt(sum((bc - ab)^2)) / sqrt(sum(ab^2))
      score[i] <- curvature
    }
  } else if (method == "norm_diff") {
    for (i in 2:n) {
      score[i] <- sqrt(sum((latent[i, ] - latent[i - 1, ])^2))
    }
  } else {
    stop("Unsupported method. Use 'angle', 'curvature', or 'norm_diff'.")
  }

  # Smooth score (optional)
  if (!is.null(window) && window > 1) {
    score <- stats::filter(score, rep(1 / window, window), sides = 2)
  }

  # Thresholding
  if (is.null(threshold)) {
    threshold <- quantile(score, 0.95, na.rm = TRUE)
  }
  shifts <- which(score > threshold)

  return(list(
    shift_indices = shifts,
    score = score,
    method = method,
    threshold = threshold
  ))
}
