#include <numeric>
#include <Rcpp.h>

using namespace Rcpp;

// [[Rcpp::export]]
NumericVector exponential_ma(const NumericVector &x, double alpha, int n_init)
{
  int nx = x.size();
  NumericVector out(nx);
  double mean_first = 0.0;
  for (int i = 0; i < n_init; ++i)
  {
    mean_first += x[i];
  }
  mean_first /= n_init;

  // Initialize with mean of first few elements
  out[0] = mean_first;
  for (int i = 1; i < nx; ++i)
  {
    out[i] = alpha * x[i] + (1 - alpha) * out[i - 1];
  }

  return out;
}

// [[Rcpp::export]]
NumericVector simple_ma(const NumericVector &x, int n)
{
  int nx = x.size();
  int nx_pad = n / 2; // Use integer division

  // Compute mean from the vector
  double x_mean = mean(x);

  // Pad input with mean
  NumericVector x_pad(nx + 2 * nx_pad);
  for (int i = 0; i < nx_pad; ++i)
    x_pad[i] = x_mean;
  for (int i = 0; i < nx; ++i)
    x_pad[i + nx_pad] = x[i];
  for (int i = nx + nx_pad; i < nx + 2 * nx_pad; ++i)
    x_pad[i] = x_mean;

  NumericVector out(nx);

  // Compute initial window sum
  double window_sum = 0.0;
  for (int i = 0; i < n; ++i)
  {
    window_sum += x_pad[i];
  }

  out[0] = window_sum / n;

  // Use running sum for the moving average
  for (int i = 1; i < nx; ++i)
  {
    window_sum += x_pad[i + n - 1] - x_pad[i - 1];
    out[i] = window_sum / n;
  }

  return out;
}

// [[Rcpp::export]]
NumericVector weighted_ma(const NumericVector &x, const NumericVector &w)
{
  int n = w.size();
  int nx = x.size();
  int nx_pad = n / 2;

  // Compute mean for padding from specified indices
  double x_mean = mean(x);

  // Pad input with mean
  NumericVector x_pad(nx + 2 * nx_pad, x_mean);
  for (int i = 0; i < nx; ++i)
  {
    x_pad[i + nx_pad] = x[i];
  }

  NumericVector out(nx);

  double w_sum = std::accumulate(w.begin(), w.end(), 0.0);

  for (int i = 0; i < nx; ++i)
  {
    double acc = 0.0;
    for (int j = 0; j < n; ++j)
    {
      acc += x_pad[i + j] * w[j];
    }
    out[i] = acc / w_sum;
  }

  return out;
}
