# ```structurelearn``` (An R Package for Latent Structure Analysis)
`structurelearn` is an R package for latent structure analysis. It analyzes the latent structure of time series data using geometric diagnostics and ML-based representation.

It provides tools to estimate intrinsic dimensionality, detect structural regime changes, and extract unsupervised representations of time-evolving systems. You can characterize memory depth, geometric complexity, and latent dynamics. This is suitable for high-dimensional and nonlinear generative processes. 


## Overview

The package supports:

- Unsupervised representation learning for time series (e.g., autoencoder-based, contrastive learning)
- Intrinsic dimensionality estimation of dynamic signals
- Detection of regime changes based on geometric structure (e.g., curvature, tangent variability)
- Comparative diagnostics between time series via structural divergence
- Visualization of latent geometry and local complexity


## Installation

```r
devtools::install_github("MengqiLi0000/structurelearn")
```

## Core Functions
```learn_structure()```
Constructs an unsupervised latent representation of a univariate or multivariate time series using either reconstruction- or contrast-based encoding.
```r
latent <- learn_structure(data, method = "autoencoder", window = 50)
```

```estimate_intrinsic_dim()```
Estimates the local or global intrinsic dimensionality of the latent time series using neighborhood-based methods (e.g., Levina-Bickel MLE).
```r
id <- estimate_intrinsic_dim(latent)
```

```detect_geometric_shifts()```
Identifies structural regime changes by analyzing variation in curvature, local rank, or divergence in latent space.
```r
shifts <- detect_geometric_shifts(latent)
```

```compare_latent_models()```
Quantifies structural differences between two time series using divergence in learned representations.
```r
div <- compare_latent_models(latent1, latent2)
```

```div <- compare_latent_models(latent1, latent2)```
Visualizes latent trajectories using dimensionality reduction (UMAP/t-SNE) or curvature-based coloring.
```r
visualize_structure(latent, color_by = "dim")
```

## Dependencies
R >= 4.0

Optional: ```torch```, ```reticulate```, ```FNN```, ```Rtsne```,```umap```, ```Rcpp```
