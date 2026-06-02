# ssmooth

<!-- badges: start -->
 [![CRAN](https://www.r-pkg.org/badges/version/ssmooth)](https://CRAN.R-project.org/package=ssmooth)
 [![R-CMD-check](https://github.com/Biodiversity-Futures-Lab/ssmooth/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Biodiversity-Futures-Lab/ssmooth/actions/workflows/R-CMD-check.yaml)
 [![format-check](https://github.com/Biodiversity-Futures-Lab/ssmooth/actions/workflows/format-check.yaml/badge.svg)](https://github.com/Biodiversity-Futures-Lab/ssmooth/actions/workflows/format-check.yaml)
  <!-- badges: end -->

Run time series smoothing on `terra::SpatRaster` objects. Applies a simple smoothing algorithm to each pixels' time series. Currently supports simple moving average, weighted moving average, and exponential smoothing.

## Installation

Install from CRAN via:

```r
install.packages("ssmooth")
```

Alternatively, install the development version from this GitHub repo via:

```r
# install.packages("pak")
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

We assume that the layers are temporally ordered (oldest -> newest): if time information is detected in the layers (`terra::time()`), then smoothing will be applied in temporal order. If no time information is found then smoothing will be applied across layers in the order given.

As well, we also provide a lower-level function `SmoothTS` that applies smoothing to a single numeric vector, which is what `SmoothRasterTS` uses under the hood:

```r
# library(ssmooth)
x <- runif(10)
SmoothTS(x, "mean", n = 3)
```