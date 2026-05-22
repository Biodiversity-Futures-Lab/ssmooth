#' Smooth Time Series Data
#'
#' Applies a smoothing operation to a numeric vector (time series data) using
#' one of three methods: mean (moving average), weighted moving average, or
#' exponential moving average.
#'
#' @param x Numeric vector. The time series data to be smoothed.
#' @param method Character. Smoothing method to use. Options are \code{"mean"},
#'   \code{"weighted"}, or \code{"exponential"}. Default is
#'   \code{"exponential"}.
#' @param n Integer. Number of points for the moving average (used in "mean" and
#'   "weighted" methods). Default is 3.
#' @param weights Numeric vector. Weights for the weighted moving average (used
#'   in "weighted" method). If NULL, equal weights are used. Default is NULL.
#' @param alpha Numeric. Smoothing factor for exponential moving average (used
#'   in "exponential" method). Default is 0.3.
#' @param n_init Integer. Number of initial points to use for the first
#'   smoothed value in the exponential moving average (used in "exponential"
#'   method). Default is 5.
#' @return Numeric vector of smoothed values.
#'
#' @export
SmoothTS <- function(
  x,
  method = "exponential",
  n = 3,
  weights = NULL,
  alpha = 0.3,
  n_init = 5
) {
  if (method == "mean") {
    return(simple_ma(x, n))
  } else if (method == "weighted") {
    if (is.null(weights)) {
      weights <- rep(1, n)
    }
    return(weighted_ma(x, weights))
  } else if (method == "exponential") {
    return(exponential_ma(x, alpha, n_init))
  } else {
    stop("Given method not recognised")
  }
}

#' Smooth Raster Time Series
#'
#' Applies a smoothing operation to each pixel's time series in a multi-layer
#' raster using one of three methods: mean (moving average), weighted moving
#' average, or exponential moving average.
#'
#' Assumes that the input raster has multiple layers, where each layer
#' represents a time point in the series. The function applies the specified
#' smoothing method to the time series of each pixel across the layers and
#' returns a new raster with the smoothed time series for each pixel.
#'
#' @param rast SpatRaster. A multi-layer raster object where each layer
#'   represents a time point in the series.
#' @param ... Additional arguments passed to the `SmoothTS` function, such as
#'   `method`, `n`, `weights`, `alpha`, and `n_init`.
#' @return SpatRaster. A multi-layer raster object with smoothed time series for
#'   each pixel.
#'
#' @export
SmoothRasterTS <- function(rast, ...) {
  return(terra::app(rast, SmoothTS, ..., cores = 1))
}
