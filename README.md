# ssmooth

<!-- badges: start -->
 [![R-CMD-check](https://github.com/Biodiversity-Futures-Lab/ssmooth/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Biodiversity-Futures-Lab/ssmooth/actions/workflows/R-CMD-check.yaml)
 [![format-check](https://github.com/Biodiversity-Futures-Lab/ssmooth/actions/workflows/format-check.yaml/badge.svg)](https://github.com/Biodiversity-Futures-Lab/ssmooth/actions/workflows/format-check.yaml)
  <!-- badges: end -->

Run time series smoothing on `terra::SpatRaster` objects. Applies a simple smoothing algorithm to each pixels' time series.

## Installation

Install from this GitHub repo via:

```r
# install.packages("pqk")
pak::pkg_install("Biodiversity-Futures-Lab/ssmooth")
```

## Usage

The main function of interest is `SmoothRasterTS`, which applies a smoother over the raster time-series:

```r
# library(ssmooth)
r <- terra::rast(nrow = 2, ncol = 2, nlyrs = 10)
terra::values(r) <- rep(1:4, each = 10)
SmoothRasterTS(r, "mean")
```

We assume that the layers are temporally ordered (oldest -> newest).