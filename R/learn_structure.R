#' learn_structure
#'
#' Learn a latent representation of a univariate or multivariate time series
#' using an unsupervised ML-based encoder (autoencoder or contrastive method).
#'
#' @param data A data.frame or data.table containing the time series (columns may be multivariate).
#' @param method A string indicating the method to use: "autoencoder" or "tscc".
#' @param window An integer specifying the size of each rolling window.
#' @param normalize Logical. If TRUE, standardizes each series before learning.
#' @param ... Additional arguments passed to internal encoder methods.
#'
#' @return A matrix of latent representations (rows = time windows, columns = features).
#' @export

learn_structure <- function(data, method = "autoencoder", window = 30, normalize = TRUE, ...) {
  if (!requireNamespace("torch", quietly = TRUE)) stop("The 'torch' package is required.")
  if (!requireNamespace("R6", quietly = TRUE)) stop("The 'R6' package is required.")

  library(torch)
  library(R6)

  if (!is.data.frame(data)) stop("Input must be a data.frame or data.table")
  ts_matrix <- as.matrix(data)
  if (normalize) ts_matrix <- scale(ts_matrix)

  n <- nrow(ts_matrix)
  d <- ncol(ts_matrix)
  n_windows <- n - window + 1
  X <- array(0, dim = c(n_windows, window, d))

  for (i in 1:n_windows) {
    X[i, , ] <- ts_matrix[i:(i + window - 1), , drop = FALSE]
  }

  # Flatten to feed into encoder: [batch, window * d]
  X_input <- torch_tensor(array_reshape(X, c(n_windows, window * d)), dtype = torch_float())

  # Define autoencoder model
  if (method == "autoencoder") {
    encoder <- nn_module(
      initialize = function(input_dim, hidden_dim = 32) {
        self$fc1 <- nn_linear(input_dim, hidden_dim)
        self$fc2 <- nn_linear(hidden_dim, hidden_dim)
      },
      forward = function(x) {
        x %>% self$fc1() %>% nnf_relu() %>% self$fc2()
      }
    )

    input_dim <- window * d
    model <- encoder(input_dim = input_dim, ...)

    optimizer <- optim_adam(model$parameters, lr = 0.01)
    criterion <- nn_mse_loss()

    for (epoch in 1:100) {
      model$train()
      optimizer$zero_grad()
      output <- model(X_input)
      loss <- criterion(output, X_input)
      loss$backward()
      optimizer$step()
      if (epoch %% 20 == 0) cat("Epoch", epoch, "Loss:", loss$item(), "\n")
    }

    # Extract encoded features
    model$eval()
    latent <- as_array(model(X_input))
    return(latent)
  }

  stop("Only method = 'autoencoder' is currently supported.")
}
