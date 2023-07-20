#include "cpp11.hpp"
#include "Rmath.h"
#include <vector>
#include <cpp11/list.hpp>

using namespace cpp11;
namespace writable = cpp11::writable;

// writable::doubles_matrix

[[cpp11::register]]
int deform(
    std::vector<double> old_x,
    std::vector<double> old_y,
    double sigma
) {

  // utility variables
  int npt = 0; // number of points in "old" array
  int ind = 0;            // current index into "new" array
  double x_insert = 0;    // x-coord for to-be-inserted point
  double y_insert = 0;    // y-coord for to-be-inserted point
  double seg_len = 0;     // length of a line segment
  double x_diff = 0;      // length of a line segment along x-axis
  double y_diff = 0;      // length of a line segment along y-axis
  double x_mid = 0;
  double y_mid = 0;

  // vectors for the new co-ordinate values
  std::vector<double> new_x {};
  std::vector<double> new_y {};

  // vectors for the drift rates and variability
//  std::vector<double> drift_mean {};
//  std::vector<double> drift_sd {};
//  for(int i = 0; i < npt; ++i) {
 //   drift_mean[i] = 0;
  //  drift_sd[i] = sigma;
  //}

  //int depth = 1;

  // iterate until recursion depth reached
//  for(int  = 0; d < depth; ++d) {

    // set up for new loop
    npt = old_x.size();
    ind = 0;

    // iterate until all displacements/insertions done
    for(int i = 0; i < (npt - 1); ++i) {

      // midpoint of the segment
      x_mid = (old_x[i] + old_x[i + 1]) / 2;
      y_mid = (old_y[i] + old_y[i + 1]) / 2;

      // length of the segment
      //x_diff = std::abs(old_x[i] - old_x[i + 1]); // x-axis difference
      //y_diff = std::abs(old_y[i] - old_y[i + 1]); // y-axis difference
      //seg_len = sqrt(x_diff * x_diff + y_diff * y_diff); // euclidean distance

      // generate insertion points
      x_insert = Rf_rnorm(0, sigma) + x_mid; // + drift_mean[i]
      y_insert = Rf_rnorm(0, sigma) + y_mid; // + drift_mean[i]

      // append the current point
      new_x[ind] = old_x[i]; // + drift_mean[i];
      new_y[ind] = old_y[i]; // + drift_mean[i];
      ind++;

      // append the inserted point
      new_x[ind] = x_insert;
      new_y[ind] = y_insert;
      ind++;
    }

    // don't forget final point
    //new_x[ind] = old_x[npt - 1]; // + drift_mean[npt - 1];
    //new_y[ind] = old_y[npt - 1]; // + drift_mean[npt - 1];

    // update state by swapping new with old
    //old_x.swap(new_x);
    //old_y.swap(new_y);

    // update point count
    //npt = old_x.size();

//  }

  // prepare data for return
  // writable::doubles_matrix out(npt, 2);
  //for(int i = 0; i < npt; ++i) {
  //  out(i, 0) = old_x[i];
  //  out(i, 1) = old_y[i];
  //}

  return npt;
}


