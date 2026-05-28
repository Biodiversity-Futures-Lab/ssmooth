#include <Rcpp.h>

#include <numeric>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector exponential_ma(const NumericVector& x, double alpha, int n_init) {
  // Note that we need n_init <= x.size() to compute the initial mean
  int nx = x.size();
  NumericVector out(nx);
  double mean_first = 0.0;
  for (int i = 0; i < n_init; ++i) {
    if (NumericVector::is_na(x[i])) {
      mean_first = NA_REAL;
      break;
    }
    mean_first += x[i];
  }

  // Normalize the mean if it is not NA
  if (!NumericVector::is_na(mean_first)) {
    mean_first /= n_init;
  }

  // Initialize with mean of first few elements
  out[0] = mean_first;
  for (int i = 1; i < nx; ++i) {
    if (NumericVector::is_na(x[i]) || NumericVector::is_na(out[i - 1])) {
      out[i] = NA_REAL;
      for (int j = i + 1; j < nx; ++j) {
        out[j] = NA_REAL;
      }
      break;
    } else {
      out[i] = alpha * x[i] + (1 - alpha) * out[i - 1];
    }
  }

  return out;
}

// [[Rcpp::export]]
NumericVector simple_ma(const NumericVector& x, int n) {
  int nx = x.size();
  int nx_pad = n / 2;  // Use integer division

  // Compute mean: if NA present, use mean of non-NA values for padding
  // There will be at least one non-NA value since we guard against all-NA
  // input in the R wrapper
  double x_mean = mean(na_omit(x));

  // Pad input with mean
  NumericVector x_pad(nx + 2 * nx_pad, x_mean);
  for (int i = 0; i < nx; ++i) {
    x_pad[i + nx_pad] = x[i];
  }

  NumericVector out(nx, 0.0);

  // If NA is present in the initial window, then the first output is NA
  // However, we can still compute the moving average for subsequent windows if
  // they do not contain NA
  for (int i = 0; i < n; ++i) {
    if (NumericVector::is_na(x_pad[i])) {
      out[0] = NA_REAL;
      break;
    } else {
      out[0] += x_pad[i];
    }
  }

  if (!NumericVector::is_na(out[0])) {
    out[0] /= n;
  }

  // Use running sum for the moving average
  // If any NA is encountered in the window, the result is NA
  double window_sum = 0.0;
  bool has_na = false;
  for (int i = 1; i < nx; ++i) {
    window_sum = 0.0;
    has_na = false;

    for (int j = 0; j < n; ++j) {
      if (NumericVector::is_na(x_pad[i + j])) {
        has_na = true;
        break;
      } else {
        window_sum += x_pad[i + j];
      }
    }

    // Set the output
    if (has_na) {
      out[i] = NA_REAL;
    } else {
      out[i] = window_sum / n;
    }
  }

  return out;
}

// [[Rcpp::export]]
NumericVector weighted_ma(const NumericVector& x, const NumericVector& w) {
  int n = w.size();
  int nx = x.size();
  int nx_pad = n / 2;

  // Compute mean: if NA present, use mean of non-NA values for padding
  // There will be at least one non-NA value since we guard against all-NA
  // input in the R wrapper
  double x_mean = mean(na_omit(x));

  // Pad input with mean
  NumericVector x_pad(nx + 2 * nx_pad, x_mean);
  for (int i = 0; i < nx; ++i) {
    x_pad[i + nx_pad] = x[i];
  }

  // Compute weighted moving average
  NumericVector out(nx, 0.0);
  double acc = 0.0;
  bool has_na = false;
  double w_sum = std::accumulate(w.begin(), w.end(), 0.0);
  for (int i = 0; i < nx; ++i) {
    acc = 0.0;
    has_na = false;
    for (int j = 0; j < n; ++j) {
      if (NumericVector::is_na(x_pad[i + j])) {
        has_na = true;
        break;
      } else {
        acc += x_pad[i + j] * w[j];
      }
    }

    if (has_na) {
      out[i] = NA_REAL;
    } else {
      out[i] = acc / w_sum;
    }
  }

  return out;
}
