#include <cpp11.hpp>
#include <Rmath.h>
#include <vector>
#include <cpp11/list.hpp>

using namespace cpp11;
namespace writable = cpp11::writable;

[[cpp11::register]]
writable::doubles_matrix deform(
    std::vector<double> old_x,
    std::vector<double> old_y,
    double stdev,
    int depth
) {

  // utility variables
  int n = old_x.size();
  int ntot = 1 + (n - 1) * pow(2, depth);
  int npt = n;
  double x_insert = 0;
  double y_insert = 0;
  double sd = 0;
  double x_diff = 0;
  double y_diff = 0;
  double x_mid = 0;
  double y_mid = 0;
  int ind = 0;
  int step_size = 0;
  int prev = 0;
  int next = 0;

  writable::doubles_matrix out(ntot, 5);

  // copy the initial points
  step_size = pow(2, depth);
  for(int i = 0; i < npt; ++i) {
    out(ind, 0) = old_x[i]; // x-coordinate
    out(ind, 1) = old_y[i]; // y-coordinate
    out(ind, 2) = 0;        // current depth
    out(ind, 3) = ind;      // position in polygon
    out(ind, 4) = stdev;    // current standard deviation
    ind = ind + step_size;
  }

  for(int d = 0; d < depth; ++d) {

    npt = n * pow(2, d);
    step_size = pow(2, depth - d);
    ind = step_size /2;
    stdev = stdev * .8;

    for(int i = 0; i < npt; ++i) {

      // indices
      prev = ind - step_size/2;
      next = ind + step_size/2;

      if(next <= ntot) {

        // mid points for the segment
        x_mid = (out(prev, 0) + out(next, 0)) / 2;
        y_mid = (out(prev, 1) + out(next, 1)) / 2;

        // length of the segment along each dimension
        x_diff = std::abs(out(prev, 0) - out(next, 0));
        y_diff = std::abs(out(prev, 1) - out(next, 1));

        // standard deviation depends on the length of the segment
        sd = stdev * (x_diff + y_diff) / 2;
        if(sd > .1) {
          sd = .1;
        }

        // generate insertion points
        x_insert = Rf_rnorm(0, sd) + x_mid;
        y_insert = Rf_rnorm(0, sd) + y_mid;

        // append the inserted point
        out(ind, 0) = x_insert;
        out(ind, 1) = y_insert;
        out(ind, 2) = d + 1;
        out(ind, 3) = ind;      // position in polygon
        out(ind, 4) = sd;       // current standard deviation
        ind = ind + step_size;
      }
    }
  }

  return out;
}
