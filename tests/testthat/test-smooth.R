test_that("SmoothTS computes mean moving average", {
  x <- 1:5
  result <- SmoothTS(x, method = "mean", n = 3)
  expected <- simple_ma(x, 3)
  expect_equal(as.numeric(result), as.numeric(expected))
})

test_that("SmoothTS computes weighted moving average with custom weights", {
  x <- 1:5
  weights <- c(0.1, 0.6, 0.3)
  result <- SmoothTS(x, method = "weighted", n = 3, weights = weights)
  expected <- weighted_ma(x, weights)
  expect_equal(as.numeric(result), as.numeric(expected))
})

test_that("SmoothTS computes weighted moving average with default weights", {
  x <- 1:5
  result <- SmoothTS(x, method = "weighted", n = 3)
  expected <- weighted_ma(x, c(1, 1, 1))
  expect_equal(as.numeric(result), as.numeric(expected))
})

test_that("SmoothTS computes exponential moving average", {
  x <- c(10, 20, 20, 30, 40)
  alpha <- 0.5
  result <- SmoothTS(x, method = "exponential", alpha = alpha, n_init = 5)
  expected <- exponential_ma(x, alpha, 5)
  expect_equal(as.numeric(result), as.numeric(expected))
})

test_that("SmoothTS computes exponential moving average with small n_init", {
  x <- c(10, 20, 20, 30, 40, 50, 60)
  alpha <- 0.5
  result <- SmoothTS(x, method = "exponential", alpha = alpha, n_init = 3)
  expected <- exponential_ma(x, alpha, 3)
  expect_equal(as.numeric(result), as.numeric(expected))
})

test_that("SmoothTS works with NA values in mean method", {
  x <- c(1, NA, 3, 4, 5)
  result <- SmoothTS(x, method = "mean", n = 3)
  expected <- c(NA, NA, NA, 4, 12.25 / 3)
  expect_equal(as.numeric(result), as.numeric(expected))
})

test_that("SmoothTS works with NA values in weighted method", {
  x <- c(1, NA, 3, 4, 5)
  weights <- c(0.1, 0.2, 0.1)
  result <- SmoothTS(x, method = "weighted", n = 3, weights = weights)
  expected <- c(NA, NA, NA, 4, 4.3125)
  expect_equal(as.numeric(result), as.numeric(expected))
})

test_that("SmoothTS works with NA values in exponential method", {
  x <- c(10, NA, 20, 30, 40)
  alpha <- 0.5
  result <- SmoothTS(x, method = "exponential", alpha = alpha, n_init = 1)
  expected <- c(10, NA, NA, NA, NA)
  expect_equal(as.numeric(result), as.numeric(expected))
})

test_that("SmoothTS correctly windows NA values", {
  x <- c(1:99, NA, 101:200)
  result <- SmoothTS(x, method = "mean", n = 3)
  expected <- simple_ma(x, 3)
  expect_equal(sum(is.na(result)), 3)
  expect_equal(as.numeric(result), as.numeric(expected))
})

test_that("SmoothTS correctly windows NA values", {
  x <- c(1:99, NA, 101:200)
  result <- SmoothTS(x, method = "mean", n = 5)
  expected <- simple_ma(x, 5)
  print(expected)
  expect_equal(sum(is.na(result)), 5)
  expect_equal(as.numeric(result), as.numeric(expected))
})

test_that("SmoothTS correctly handles first NA values in exponential method", {
  x <- c(NA, NA, 10, 20, 30)
  alpha <- 0.5
  result <- SmoothTS(x, method = "exponential", alpha = alpha, n_init = 3)
  expected <- rep(NA, 5)
  expect_equal(as.numeric(result), as.numeric(expected))
})

test_that("SmoothTS returns NA for all NA input", {
  x <- c(NA, NA, NA)
  result <- SmoothTS(x, method = "mean", n = 3)
  expect_equal(result, x)
})

test_that("SmoothTS returns NA for all NA input in weighted method", {
  x <- c(NA, NA, NA)
  result <- SmoothTS(x, method = "weighted", n = 3, weights = c(0.1, 0.5, 0.4))
  expect_equal(result, x)
})

test_that("SmoothTS returns NA for all NA input in exponential method", {
  x <- c(NA, NA, NA)
  result <- SmoothTS(x, method = "exponential", alpha = 0.5, n_init = 3)
  expect_equal(result, x)
})

test_that("SmoothTS throws error for non-numeric input", {
  x <- c("a", "b", "c")
  expect_error(
    SmoothTS(x, method = "mean", n = 3),
    "Input x must be a numeric vector"
  )
})

test_that("SmoothTS throws error for invalid method", {
  x <- 1:5
  expect_error(
    SmoothTS(x, method = "not_a_method"),
    "Method must be one of 'mean', 'weighted', or 'exponential'"
  )
})

test_that("SmoothTS throws error for invalid n in mean method", {
  x <- 1:5
  expect_error(
    SmoothTS(x, method = "mean", n = 0),
    "n must be a positive integer <= length(x)",
    fixed = TRUE
  )
  expect_error(
    SmoothTS(x, method = "mean", n = 6),
    "n must be a positive integer <= length(x)",
    fixed = TRUE
  )
})

test_that("SmoothTS throws error for non-numeric weights", {
  x <- 1:5
  expect_error(
    SmoothTS(x, method = "weighted", n = 3, weights = c("a", "b", "c")),
    "weights must be a numeric vector"
  )
})

test_that("SmoothTS throws error for weights longer than x", {
  x <- 1:5
  expect_error(
    SmoothTS(x, method = "weighted", n = 3, weights = 1:6),
    "Length of weights cannot be greater than the length of x"
  )
})

test_that("SmoothTS throws error for non-positive weights", {
  x <- 1:5
  expect_error(
    SmoothTS(x, method = "weighted", n = 3, weights = c(0.1, -0.5, 0.4)),
    "Weights have to be positive"
  )
})

test_that("SmoothTS throws error for invalid alpha", {
  x <- 1:5
  expect_error(
    SmoothTS(x, method = "exponential", alpha = -0.1, n_init = 3),
    "alpha must be between 0 and 1 for exponential moving average"
  )
  expect_error(
    SmoothTS(x, method = "exponential", alpha = 1.5, n_init = 3),
    "alpha must be between 0 and 1 for exponential moving average"
  )
})

test_that("SmoothTS throws error for invalid n_init", {
  x <- 1:5
  expect_error(
    SmoothTS(x, method = "exponential", alpha = 0.5, n_init = 0),
    "n_init must be a positive integer <= length(x)",
    fixed = TRUE
  )
  expect_error(
    SmoothTS(x, method = "exponential", alpha = 0.5, n_init = 6),
    "n_init must be a positive integer <= length(x)",
    fixed = TRUE
  )
})
