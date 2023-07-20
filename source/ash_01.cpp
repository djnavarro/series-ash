#include "cpp11.hpp"
#include "Rmath.h"
#include <vector>
#include <cpp11/list.hpp>

using namespace cpp11;
namespace writable = cpp11::writable;

[[cpp11::register]]
writable::doubles_matrix deform(
    std::vector<double> old_x,
    std::vector<double> old_y,
    double stdev
  ) {

  // utility variables
  int n = old_x.size();
  int ind = 0;
  double x_insert = 0;
  double y_insert = 0;
  double sd = 0;
  writable::doubles_matrix out(2 * n - 1, 2);

  // vectors for the new co-ordinate values
  std::vector<double> new_x {};
  std::vector<double> new_y {};

  for(int i = 0; i < (n - 1); ++i) {

    // append the current point
    out(ind, 0) = old_x[i];
    out(ind, 1) = old_y[i];
    ind++;

    // standard deviation depends on the length of the segment
    sd = stdev * (
      std::abs(old_x[i] - old_x[i + 1]) +
      std::abs(old_y[i] - old_y[i + 1])
    ) / 2;

    // generate insertion points
    x_insert = Rf_rnorm(0, sd) + (old_x[i] + old_x[i + 1]) / 2;
    y_insert = Rf_rnorm(0, sd) + (old_y[i] + old_y[i + 1]) / 2;

    // append the inserted point
    out(ind, 0) = x_insert;
    out(ind, 1) = y_insert;
    ind++;
  }

  // don't forget final point
  out(ind, 0) = old_x[n - 1];
  out(ind, 1) = old_y[n - 1];

  return out;
}
