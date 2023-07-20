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
    std::vector<double> drift_x,
    std::vector<double> drift_y,
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
  double sd_x = 0;
  double sd_y = 0;
  double x_diff = 0;
  double y_diff = 0;
  double x_mid = 0;
  double y_mid = 0;
  double x_mid_drift = 0;
  double y_mid_drift = 0;
  int ind = 0;
  int step_size = 0;
  int prev = 0;
  int next = 0;

  writable::doubles_matrix out(ntot, 7);

  // copy the initial points
  step_size = pow(2, depth);
  for(int i = 0; i < npt; ++i) {
    out(ind, 0) = old_x[i];   // x-coordinate
    out(ind, 1) = old_y[i];   // y-coordinate
    out(ind, 2) = 0;          // current depth
    out(ind, 3) = ind;        // position in polygon
    out(ind, 4) = stdev;      // current standard deviation
    out(ind, 5) = drift_x[i]; // drift along x-coord
    out(ind, 6) = drift_y[i]; // drift along y-coord
    ind = ind + step_size;
  }

  // fill in all internal points
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

        // drift at the midpoint
        x_mid_drift = (out(prev, 5) + out(next, 5)) / 2;
        y_mid_drift = (out(prev, 6) + out(next, 6)) / 2;

        // length of the segment along each dimension
        x_diff = std::abs(out(prev, 0) - out(next, 0));
        y_diff = std::abs(out(prev, 1) - out(next, 1));

        // standard deviation depends on the length of the segment
        sd = stdev * (x_diff + y_diff) / 2;
        sd_x = sd * x_mid_drift;
        sd_y = sd * y_mid_drift;
        if(sd_x > .1) {
          sd_x = .1;
        }
        if(sd_y > .1) {
          sd_y = .1;
        }

        // generate insertion points
        x_insert = Rf_rnorm(0, sd_x) + x_mid;
        y_insert = Rf_rnorm(0, sd_y) + y_mid;

        // append the inserted point
        out(ind, 0) = x_insert;
        out(ind, 1) = y_insert;
        out(ind, 2) = d + 1;
        out(ind, 3) = ind;      // position in polygon
        out(ind, 4) = sd;       // current standard deviation
        out(ind, 5) = x_mid_drift;
        out(ind, 6) = y_mid_drift;
        ind = ind + step_size;
      }
    }
  }

  return out;
}
