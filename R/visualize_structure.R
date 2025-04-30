#' visualize_structure
#'
#' Visualize latent time series structure in 2D or 3D using UMAP, t-SNE, or PCA.
#'
#' @param latent A numeric matrix of latent representations (rows = time).
#' @param method Dimensionality reduction method: \"umap\", \"tsne\", or \"pca\". Default = \"umap\".
#' @param color_by Optional vector for coloring points (e.g., dimension, regime score).
#' @param n_components Integer: 2 or 3 for output dimensions. Default = 2.
#' @param return_data If TRUE, return the projection data instead of plotting. Default = FALSE.
#'
#' @return A ggplot object (if n_components = 2) or plotly object (if n_components = 3),
#'         or the reduced data.frame if return_data = TRUE.
#' @export

visualize_structure <- function(latent, method = "umap", color_by = NULL,
                                n_components = 2, return_data = FALSE) {
  if (!is.matrix(latent)) latent <- as.matrix(latent)
  stopifnot(n_components %in% c(2, 3))

  if (method == "umap") {
    if (!requireNamespace("uwot", quietly = TRUE)) stop("Install 'uwot' for UMAP.")
    coords <- uwot::umap(latent, n_components = n_components)
  } else if (method == "tsne") {
    if (!requireNamespace("Rtsne", quietly = TRUE)) stop("Install 'Rtsne' for t-SNE.")
    coords <- Rtsne::Rtsne(latent, dims = n_components, check_duplicates = FALSE)$Y
  } else if (method == "pca") {
    coords <- prcomp(latent, center = TRUE, scale. = TRUE)$x[, 1:n_components, drop = FALSE]
  } else {
    stop("Unsupported method. Choose from 'umap', 'tsne', or 'pca'.")
  }

  df <- as.data.frame(coords)
  colnames(df) <- paste0("V", 1:n_components)
  df$time <- seq_len(nrow(df))

  if (!is.null(color_by)) {
    if (length(color_by) != nrow(df)) stop("color_by must have the same length as latent rows.")
    df$color <- color_by
  }

  if (return_data) return(df)

  if (!requireNamespace("ggplot2", quietly = TRUE)) stop("Install 'ggplot2' for plotting.")
  library(ggplot2)

  if (n_components == 2) {
    p <- ggplot(df, aes(x = V1, y = V2, color = color)) +
      geom_point(size = 1.5) +
      labs(x = "Latent 1", y = "Latent 2") +
      theme_minimal()
    return(p)
  }

  if (n_components == 3) {
    if (!requireNamespace("plotly", quietly = TRUE)) stop("Install 'plotly' for 3D plotting.")
    return(plotly::plot_ly(df, x = ~V1, y = ~V2, z = ~V3, color = ~color, type = "scatter3d", mode = "markers"))
  }
}
