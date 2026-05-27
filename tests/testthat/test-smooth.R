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

test_that("SmoothTS throws error for invalid method", {
  x <- 1:5
  expect_error(
    SmoothTS(x, method = "not_a_method"),
    "Method must be one of 'mean', 'weighted', or 'exponential'"
  )
})
