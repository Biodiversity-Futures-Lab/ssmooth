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
#' @examples
#' x <- 1:10
#' SmoothTS(x, method = "mean", n = 3)
#' SmoothTS(x, method = "weighted", n = 3, weights = c(0.1, 0.5, 0.4))
#' SmoothTS(x, method = "exponential", alpha = 0.5, n_init = 3)
#'
#' @export
SmoothTS <- function(
  x,
  method = "exponential",
  n = 3,
  weights = rep(1, 3),
  alpha = 0.3,
  n_init = 5
) {
  if (!is.numeric(x)) {
    stop("Input x must be a numeric vector")
  }

  if (!method %in% c("mean", "weighted", "exponential")) {
    stop("Method must be one of 'mean', 'weighted', or 'exponential'")
  }

  if (method == "mean") {
    if (n <= 0 || n > length(x)) {
      stop("n must be a positive integer <= length(x)")
    }

    return(simple_ma(x, n))
  } else if (method == "weighted") {
    if (!is.numeric(weights)) {
      stop("weights must be a numeric vector")
    }

    if (length(weights) > length(x)) {
      stop("Length of weights cannot be greater than the length of x")
    }

    return(weighted_ma(x, weights))
  } else if (method == "exponential") {
    if (alpha <= 0 || alpha >= 1) {
      stop("alpha must be between 0 and 1 for exponential moving average")
    }

    if (n_init <= 0 || n_init > length(x)) {
      stop(
        "n_init must be a positive integer <= length(x)"
      )
    }
    return(exponential_ma(x, alpha, n_init))
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
#' @param cores Integer. Number of CPU cores to use for parallel processing.
#'   Default is 1. Don't change this unless you know what you're doing.
#' @param filename Character. Optional filename for writing the output raster.
#'   If provided, the output raster will be written to this file.
#' @param overwrite Logical. If TRUE and `filename` is provided, allows
#'   overwriting the existing file; optional.
#' @param wopt List. Optional list of additional arguments to pass to
#'   `terra::writeRaster`.
#' @return SpatRaster. A multi-layer raster object with smoothed time series for
#'   each pixel.
#'
#' @examples
#'
#' rast <- terra::rast(nrow = 1, ncol = 2, nlyrs = 5)
#' terra::values(rast) <- matrix(rep(1:5, each = 2), nrow = 2)
#' out <- SmoothRasterTS(rast, method = "mean", n = 3)
#'
#' @export
SmoothRasterTS <- function(
  rast,
  ...,
  cores = 1L,
  filename = "",
  overwrite = FALSE,
  wopt = list()
) {
  if (!inherits(rast, "SpatRaster")) {
    stop("Input must be a SpatRaster object")
  }

  dates <- terra::time(rast)
  if (any(is.na(dates))) {
    warning(
      "Input raster has incomplete dates: smoothing will be applied across layers in order"
    )
  } else {
    # Ensure dates are in the correct order
    rast <- rast[[order(dates)]]
    dates <- dates[order(dates)]
  }

  # 'app' applies the function over the layers of the raster
  result <- terra::app(
    rast,
    SmoothTS,
    ...,
    cores = cores,
    filename = filename,
    overwrite = overwrite,
    wopt = wopt
  )

  # Propagate time information to the output raster
  terra::time(result) <- dates
  return(result)
}
