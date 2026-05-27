# Helper to generate a simple SpatRaster with predictable values
MakeTestRaster <- function(nrow = 2, ncol = 2, nlayers = 5) {
  mat_list <- lapply(
    1:nlayers,
    function(i) {
      return(terra::rast(matrix(i, nrow, ncol)))
    }
  )
  rast <- terra::rast(mat_list)
  names(rast) <- paste0("layer", seq_len(nlayers))
  return(rast)
}

# Helper: extract values as a matrix (pixels x layers)
RasterToMatrix <- function(rast) {
  return(as.matrix(t(terra::values(rast))))
}

test_that("SmoothRasterTS works with mean (moving average)", {
  rast <- MakeTestRaster(nrow = 1, ncol = 2, nlayers = 5)

  # All time series are c(1,2,3,4,5)
  expect_warning(
    out <- SmoothRasterTS(rast, method = "mean", n = 3),
    "Input raster has incomplete dates: smoothing will be applied across layers in order"
  )
  expect_equal(
    terra::values(out),
    matrix(rep(c(2, 2, 3, 4, 4), each = 2), nrow = 2),
    ignore_attr = TRUE,
    tolerance = 1e-8
  )
})

test_that("SmoothRasterTS works with weighted moving average", {
  rast <- MakeTestRaster(nrow = 1, ncol = 1, nlayers = 5)
  weights <- c(0.2, 0.3, 0.5)
  expect_warning(
    out <- SmoothRasterTS(rast, method = "weighted", n = 3, weights = weights),
    "Input raster has incomplete dates: smoothing will be applied across layers in order"
  )
  mat <- terra::values(out)
  expect_equal(
    mat,
    c(1.9, 2.3, 3.3, 4.3, 3.8),
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
})

test_that("SmoothRasterTS works with exponential moving average", {
  rast <- MakeTestRaster(nrow = 1, ncol = 1, nlayers = 5)
  alpha <- 0.5
  n_init <- 2
  expect_warning(
    out <- SmoothRasterTS(
      rast,
      method = "exponential",
      alpha = alpha,
      n_init = n_init
    ),
    "Input raster has incomplete dates: smoothing will be applied across layers in order"
  )
  mat <- terra::values(out)

  # First value: mean(1,2), then recursively
  expected <- numeric(5)
  expected[1] <- mean(c(1, 2))
  for (i in 2:5) {
    expected[i] <- alpha * i + (1 - alpha) * expected[i - 1]
  }
  expect_equal(mat, expected, ignore_attr = TRUE, tolerance = 1e-8)
})

test_that("SmoothRasterTS errors for incorrect input datatype", {
  df <- data.frame(a = 1:5, b = 6:10)
  expect_error(
    suppressWarnings(SmoothRasterTS(df, method = "mean", n = 3)),
    "Input must be a SpatRaster object"
  )
})

test_that("SmoothRasterTS errors for invalid method", {
  rast <- MakeTestRaster()
  expect_error(
    suppressWarnings(SmoothRasterTS(rast, method = "foobar")),
    "Method must be one of 'mean', 'weighted', or 'exponential'"
  )
})

test_that("SmoothRasterTS works for multi-pixel raster", {
  rast <- MakeTestRaster(nrow = 2, ncol = 2, nlayers = 4)
  expect_warning(
    out <- SmoothRasterTS(rast, method = "mean", n = 2),
    "Input raster has incomplete dates: smoothing will be applied across layers in order"
  )
  mat <- terra::values(out)
  expected <- matrix(
    rep(c(1.75, 1.5, 2.5, 3.5), each = 4),
    nrow = 4
  )

  expect_equal(
    mat,
    expected,
    tolerance = 1e-8,
    ignore_attr = TRUE
  )
})

test_that("SmoothRasterTS preserves raster dimensions and layer count", {
  rast <- MakeTestRaster(nrow = 3, ncol = 2, nlayers = 6)
  expect_warning(
    out <- SmoothRasterTS(rast, method = "mean", n = 2),
    "Input raster has incomplete dates: smoothing will be applied across layers in order"
  )
  expect_equal(dim(out), dim(rast))
})

test_that("SmoothRasterTS propagates NA correctly", {
  rast <- terra::rast(list(
    terra::rast(matrix(c(NA, 2), 1, 2)),
    terra::rast(matrix(c(3, NA), 1, 2)),
    terra::rast(matrix(c(4, 5), 1, 2))
  ))
  expect_warning(
    out <- SmoothRasterTS(rast, method = "mean", n = 2),
    "Input raster has incomplete dates: smoothing will be applied across layers in order"
  )
  mat <- terra::values(out)

  # If NA present, moving average should result in NA for that window
  expect_true(any(is.na(mat)))
})

test_that("SmoothRasterTS deals with time information", {
  # Create a raster with time information
  rast <- MakeTestRaster(nrow = 1, ncol = 1, nlayers = 5)
  terra::time(rast) <- as.Date("2020-01-01") + 0:4

  out <- SmoothRasterTS(rast, method = "mean", n = 2)
  expect_equal(terra::time(out), terra::time(rast))
})

test_that("SmoothRasterTS correctly reorders layers", {
  # Create a raster with time information in reverse order
  rast <- MakeTestRaster(nrow = 1, ncol = 1, nlayers = 5)
  terra::time(rast) <- as.Date("2020-01-01") + 4:0

  out <- SmoothRasterTS(rast, method = "mean", n = 2)
  expect_equal(terra::time(out), sort(terra::time(rast)))
})

test_that("SmoothRasterTS deals with years", {
  # Create a raster with time information as years
  rast <- MakeTestRaster(nrow = 1, ncol = 1, nlayers = 5)
  terra::time(rast) <- 2000:2004

  out <- SmoothRasterTS(rast, method = "mean", n = 2)
  expect_equal(terra::time(out), terra::time(rast))
})

test_that("SmoothRasterTS deals with years in different order", {
  # Create a raster with time information as years in reverse order
  rast <- MakeTestRaster(nrow = 1, ncol = 1, nlayers = 5)
  terra::time(rast) <- 2004:2000

  out <- SmoothRasterTS(rast, method = "mean", n = 2)
  expect_equal(terra::time(out), sort(terra::time(rast)))
})
