#include <Rcpp.h>
using namespace Rcpp;


int which_min(const NumericVector x, const int start, const int end) {
  // this function find the index of the minimum element in a vector
  if(start == end) return(start);
  double minval = x[start];
  int minind=start;
  // loop search for the minimum value
  for(int i = start + 1; i <= end; i++) {
    if(x[i] < minval) {
      minval = x[i];
      minind = i;
    }
  }
  return(minind);
}


double find_min(const NumericVector x, int start, int end) {
  // this function find the index of the minimum element in a vector
  double minval = x[start];
  // loop search for the minimum value
  for(int i=start+1; i<=end; i++) {
    if(x[i] < minval) {
      minval = x[i];
    }
  }
  return(minval);
}


int which_max(const NumericVector x, const int start, const int end) {
  // this function find the index of the maximum element in a vector
  if(start == end) return(start);
  double maxval = x[start];
  int maxind=start;
  // loop search for the minimum value
  for(int i = start+1; i <= end; i++) {
    if(x[i] > maxval) {
      maxval = x[i];
      maxind = i;
    }
  }
  return(maxind);
}


double find_max(const NumericVector x, const int start, const int end) {
  // this function find the index of the minimum element in a vector
  double maxval = x[start];
  // loop search for the minimum value
  for(int i=start+1; i<=end; i++) {
    if(x[i] > maxval) {
      maxval = x[i];
    }
  }
  return(maxval);
}


// parameters of the filter rule

double t_bull = 15;
double t_bear = 15;

//' Sets the paramters of the filtering algorithm
//'
//' This function sets the paramters of the filtering algorithm of Lunde and Timmermann (2004)
//'
//' @param tr_bull threshold to idenitfy a Bull state (in percentages)
//' @param tr_bear threshold to idenitfy a Bear state (in percentages)
//' @return None
//' @usage setpar_filtering_alg(tr_bull, tr_bear)
//' @references
//' Lunde, A. and Timmermann, A. (2004). Duration Dependence in Stock Prices: An Analysis
//' of Bull and Bear Markets. Journal of Business and Economic Statistics, 22 (3), 253-273.
//' @export
// [[Rcpp::export]]

void setpar_filtering_alg(double tr_bull = 30, double tr_bear = 20) {
  t_bull = tr_bull;
  t_bear = tr_bear;
}

//' Runs the filtering algorithm to identify Bull and Bear states
//'
//' This function implements the filtering algorithm of Lunde and Timmermann (2004) to identify Bull and Bear states
//'
//' @param index vector containing the stock price index
//' @return A logical vector that contains TRUE for Bull states and FALSE for Bear states
//' @note Be aware that the states in the beginning and in the end of \code{"index"} are not properly defined
//' @usage run_filtering_alg(index)
//' @references
//' Lunde, A. and Timmermann, A. (2004). Duration Dependence in Stock Prices: An Analysis
//' of Bull and Bear Markets. Journal of Business and Economic Statistics, 22 (3), 253-273.
//' @export
// [[Rcpp::export]]

LogicalVector run_filtering_alg(const NumericVector index) {

  // initial definitions
  int nobs = index.size();
  int enter = 0, exit = 0, ind = 0;
  double maxval = 0.0, minval = 0.0, change = 0.0;
  bool instocks = true;
  LogicalVector bull(nobs);

  // main loop
  for(int i = 0; i < nobs; i++) {
    if(i == 0) {
      // first element, assume the bull state
      enter = 0;
      bull[i] = true;
      instocks = true;
    } else {
      // any other element
      if(instocks == true) {
        // check whether we need to exit
        maxval = find_max(index, enter, i);
        change = (maxval - index[i]) / maxval*100;
        if (change > t_bear) {
          exit = i;
          instocks = false;
          // now find the BEAR period
          ind = which_max(index, enter, i);
          for(int j=ind+1; j<=i; j++) bull[j] = false;
        } else bull[i] = true;
      } else {
        // check whether we need to enter
        minval = find_min(index, exit, i);
        change = (index[i] - minval) / minval*100;
        if (change > t_bull) {
          instocks = true;
          enter = i;
          // now find the BULL period
          ind = which_min(index, exit, i);
          for(int j=ind+1; j<=i; j++) bull[j] = true;
        } else bull[i] = false;
      }
    }
  }
  if(bull[1]==false) bull[0]=false; // if the first phase is Bear
  return(bull);
}

// parameters of the dating rule

int roll_window_size = 3;  // the half-size of the window to find minima and maxima, 3 by default
int margin_size = 6;      // the size of the left and right margin, 6 by default
int min_cycle_length = 16; // the minimum full cycle length, 16 by default
double max_change = 20;  // the change (in percentages) in "index" that invalidates the minimum cycle length, 20 by default
int min_phase_length = 4; // the minimum phase (bull or bear) length, 4 by default

double find_min(const std::vector<double> & x) {
  // this function find the index of the minimum element in a vector
  int n = x.size();
  double minval = x[0];
  if(n > 1) {
    // loop search for the minimum value
    for(int i = 1; i < n; i++) {
      if(x[i] < minval) {
        minval = x[i];
      }
    }
  }
  return(minval);
}

int which_min(const std::vector<double> & x) {
  // this function find the index of the minimum element in a vector
  int n = x.size();
  double minval = x[0];
  int minind=0;
  if(n > 1) {
    // loop search for the minimum value
    for(int i = 1; i < n; i++) {
      if(x[i] < minval) {
        minval = x[i];
        minind = i;
      }
    }
  }
  return(minind);
}

double find_max(const std::vector<double> & x) {
  // this function find the index of the minimum element in a vector
  int n = x.size();
  double maxval = x[0];
  if(n > 1) {
    // loop search for the minimum value
    for(int i = 1; i < n; i++) {
      if(x[i] > maxval) {
        maxval = x[i];
      }
    }
  }
  return(maxval);
}

int which_max(const std::vector<double> & x) {
  // this function find the index of the minimum element in a vector
  int n = x.size();
  double maxval = x[0];
  int maxind=0;
  if(n > 1) {
    // loop search for the minimum value
    for(int i = 1; i < n; i++) {
      if(x[i] > maxval) {
        maxval = x[i];
        maxind = i;
      }
    }
  }
  return(maxind);
}

int first_true(const LogicalVector & x) {
  int n = x.size();
  int first = 0;
  for(int i = 0; i < n; i++) {
    if(x[i]==true) {
      first = i;
      break;
    }
  }
  return(first);
}

int last_true(const LogicalVector & x) {
  int n = x.size();
  int last = n-1;
  for(int i = n-1; i >= 0; i--) {
    if(x[i]==true) {
      last = i;
      break;
    }
  }
  return(last);
}

void eliminate_min(LogicalVector & local_min, const int start, const int end,
                   const NumericVector & index, const int position) {

  // this function eleminates multiple minima in "local_min" from "start" till "end"
  // in the end, only the lowest minimum remains
  // local_min is a boolean vector that containst the positions of local minima
  // index is a vector that contains values

  // find all minima from "start" till "end"
  std::vector<double> minima(0);
  std::vector<int> ind_minima(0);
  int i = 0;
  for(i = start; i <= end; i++) {
    if(local_min[i]==true) {
      minima.push_back(index[i]);
      ind_minima.push_back(i);
    }
  }
  // proceed with the counting the number of minima
  int nMin = minima.size();
  // elimination
  if(nMin > 0) {
    if(find_min(minima)>index[position]) {
      // eliminate all of them
      for(i = start; i <= end; i++) local_min[i] = false;
    } else {
      // keep only the lowest of all minima
      int ind_min = which_min(minima);
      for(i = 0; i < nMin; i++) if(i != ind_min) local_min[ind_minima[i]] = false;
    }
  }

}

void eliminate_max(LogicalVector & local_max, const int start, const int end,
                   const NumericVector & index, const int position) {

  // this function eleminates multiple maxima in "local_max" from "start" till "end"
  // in the end, only the largest maximum remains
  // local_max is a boolean vector that containst the positions of maxima
  // index is a vector that contains values

  // find all maxima from "start" till "end"
  std::vector<double> maxima(0);
  std::vector<int> ind_maxima(0);
  int i = 0;
  for(i = start; i <= end; i++) {
    if(local_max[i]==true) {
      maxima.push_back(index[i]);
      ind_maxima.push_back(i);
    }
  }
  // proceed with the counting the number of maxima
  int nMax = maxima.size();
  // elimination
  if(nMax > 0) {
    if(find_max(maxima)<index[position]) {
      // eliminate all of them
      for(i = start; i <= end; i++) local_max[i] = false;
    } else {
      // keep only the largest of all maxima
      int ind_max = which_max(maxima);
      for(i = 0; i < nMax; i++) if(i != ind_max) local_max[ind_maxima[i]] = false;
    }
  }

}

void eliminate_multiple_extr(const LogicalVector & major, LogicalVector & minor,
                             const NumericVector & index, bool bMin) {
  // this function eliminates multiple extrema in "minor" boolean vector
  // between two extrema in "major" boolean vector

  int n = major.size();
  bool first = false;
  int ind_first = 0;
  int ind_extr = 0;

  for(int i = 0; i < n; i++) {
    if( (major[i]==true) && (first==false) ) {
      // this is the very first extremum
      first = true;
      ind_first = i;
      continue;
    }
    if(( major[i]==true) && (first==true) ) {
      // this is the subsequent extremum in "major"
      // elimination of multiple extrema within ind_first and i
      // start with the search for extrema
      std::vector<double> extrema(0);
      std::vector<int> ind_extrema(0);
      for(int j=ind_first; j <= i; j++) {
        if(minor[j]==true) {
          extrema.push_back(index[j]);
          ind_extrema.push_back(j);
        }
      }
      // proceed with the counting the number of extrema
      int nExtr = extrema.size();
      // eliminate multiple extrema
      if(nExtr > 1) {
        if(bMin == true) {
          // we eliminate minima
          ind_extr = which_min(extrema);
        } else {
          // we eliminate maxima
          ind_extr = which_max(extrema);
        }
        // the elimination
        for(int s = 0; s < nExtr; s++)
          if(s != ind_extr) minor[ind_extrema[s]] = false;
      }
      // end of current elimination, reset the index of first extrema
      ind_first = i;
    }
  }
}

LogicalVector get_bull(LogicalVector & local_min, LogicalVector & local_max) {
  // this function construct the vector that contains TRUE during Bull markets

  int n = local_min.size();

  LogicalVector bull = LogicalVector(n,false);

  // identify bull and bear markets
  bool first_ext_found = false; // first extremum, either local.min or local.max, FALSE if not found yet
  bool isBull = false;
  for(int i = 0; i < n; i++) {

    // find the first local min or max
    if( (first_ext_found==false) & (local_min[i]==true) ) {
      // beginning is BEAR, next is BULL
      first_ext_found = true;
      isBull = true;
      continue;
    }
    if( (first_ext_found==false) & (local_max[i]==true) ) {
      first_ext_found = true;
      // beginning is BULL, next BEAR
      for(int j = 0; j <= i; j++) bull[j] = true; // set all previous points as BULL
      isBull = false;
      continue;
    }

    // if the first extremum is found
    if(first_ext_found==true) {
      if(isBull==true) {
        bull[i] = true;
        // this is Bull period
        if(local_max[i]==true) isBull  = false; // Next period BEAR
      } else {
        // this is Bear period
        bull[i] = false;
        if(local_min[i]==true) isBull = true; // Next period BULL
      }
    }
  }

  return(bull);
}

void eliminate_multiple_mm(const NumericVector & index,
                           LogicalVector & local_min,
                           LogicalVector & local_max) {

  // THIS PROCEDURE ELIMINATE MULTIPLE MAXIMA AND MINIMA IN THE BEGINNING, IN THE END
  // MULTIPLE MINIMA WITHIN TWO MAXIMA, MULTIPLE MAXIMA WITHIN TWO MINIMA

  int n = index.size();

  //**********************************************************
  // FIRST PART, eliminating multiple maxima and minima in the
  // beginning and in the end
  //**********************************************************

  //========================================================
  // eliminating multiple minima and maxima in the beginning

  // find the first min and max
  int first_max = first_true(local_max);
  int first_min = first_true(local_min);

  // eliminate multiple minima in the beginning
  eliminate_min(local_min, 0, first_max, index, 0);

  // eliminate multiple maxima in the beginning
  eliminate_max(local_max, 0, first_min, index, 0);

  //========================================================
  // eliminating multiple minima and maxima in the end

  // find the first min and max
  int last_max = last_true(local_max);
  int last_min = last_true(local_min);

  // eliminate multiple minima in the end
  eliminate_min(local_min, last_max, n-1, index, n-1);

  // eliminate multiple maxima in the end
  eliminate_max(local_max, last_min, n-1, index, n-1);

  //**********************************************************
  // SECOND PART, Eliminate multipe max within two minima and
  // multiple minima within two maxima
  //**********************************************************

  // eliminate multiple minima within two maxima
  eliminate_multiple_extr(local_max, local_min, index, true);

  // eliminate multiple maxima within two minima
  eliminate_multiple_extr(local_min, local_max, index, false);
}


void eliminate_consecutive_extr(LogicalVector & local_extr,
                                const NumericVector & index, bool bMin) {
  // this function eliminates consecutive extrema in a logical vector
  // only the last extrema in a series is kept

  int n = local_extr.size();
  bool multiple_extr = false;
  int first_extr = 0;
  int end = 0;
  int ind_extr = 0;

  for(int i = 1; i < n; i++) {
    if( ( (local_extr[i-1]==false) && (local_extr[i]==true) ) )  {
      // mark the start of a new extremum (probably series)
      first_extr = i;
      continue;
    }
    if( (local_extr[i-1]==true) && (local_extr[i]==true) ) {
      // mark the existence of multiple extrema
      multiple_extr = true;
      if(i == 1) first_extr = i - 1;
      if(i != (n-1)) continue; // continue if not the end of the vector
    }
    // check whether the end of a sequence of extrema or the end of vector
    if( ( (local_extr[i]==false) || (i==(n-1)) ) && (multiple_extr==true) ) {
      // this is the end of the sequence
      multiple_extr = false; // reset
      if( (i==(n-1)) && (local_extr[i]==true) ) end = n - 1; else end = i - 1; // depending on whether it is the end or not
      if(bMin == true) {
        // we eliminate minima
        ind_extr = which_min(index, first_extr, end);
      } else {
        // we eliminate maxima
        ind_extr = which_max(index, first_extr, end);
      }
      // eleminate all extrema within "first_extr" and "end"
      for(int j = first_extr; j <= end; j++) local_extr[j]=false;
      // keep only the one of them
      local_extr[ind_extr]=true;
    }
  }

}

void forth_censoring(const NumericVector & index,
                     LogicalVector & local_min,
                     LogicalVector & local_max) {
  // implements the fourth censoring operation:
  // eliminate the cycles whose duration is less than "min_cycle_length"
  // except when change > "max_change"

  int n = index.size();
  int cycle_length = 0;
  double change_1 = 0.0;
  double change_2 = 0.0;
  double change   = 0.0;

  bool Elimination = true;
  while(Elimination) {

    bool first_max = false;
    bool first_min = false;
    std::vector<double> cycle_points(0); // point values
    std::vector<double> cycle_ind(0);    // indices of points

    for(int i = 0; i < n; i++) {
      // find the first point, identify it as max or min
      if((first_max==false) && (first_min==false) && (local_min[i]==true)) {
        // first point is min
        first_min = true;
        cycle_points.push_back(index[i]);
        cycle_ind.push_back(i);
        continue;
      }
      if((first_max==false) && (first_min==false) && (local_max[i]== true)) {
        // first point is max
        first_max = true;
        cycle_points.push_back(index[i]);
        cycle_ind.push_back(i);
        continue;
      }

      if(first_max == true) {
        if(local_min[i] == true) {
          // add the next min
          cycle_points.push_back(index[i]);
          cycle_ind.push_back(i);
          continue;
        }
        if(local_max[i] == true) {
          // this is the end of the current cycle that begins with MAX
          cycle_points.push_back(index[i]);
          cycle_ind.push_back(i);
          // we need to check if we need to eliminate the first point
          cycle_length = cycle_ind[2] - cycle_ind[0]; // compute the cycle length
          change_1 = fabs( (cycle_points[1]-cycle_points[0])/cycle_points[0] ) * 100;
          change_2 = fabs( (cycle_points[2]-cycle_points[1])/cycle_points[1] ) * 100;
          change = fmax(change_1, change_2);
          if( (cycle_length < min_cycle_length) && (change < max_change) ) {
            // need to eliminate the cycle
            local_max[cycle_ind[0]] = false;
            // sensor the rest of the points
            eliminate_multiple_mm(index, local_min, local_max);
            // go out of the for loop and begin again
            break;
          } else {
            // do not need to eliminate, proceed further
            // now the next cycle begins with MIN
            first_max = false;
            first_min = true;
            // delete the first cycle point
            cycle_points.erase(cycle_points.begin());
            cycle_ind.erase(cycle_ind.begin());
            continue;
          }
        }
      }

      if(first_min == true) {
        if(local_max[i] == true) {
          // add the next max
          cycle_points.push_back(index[i]);
          cycle_ind.push_back(i);
          continue;
        }
        if(local_min[i] == true) {
          // this is the end of the current cycle that begins with MIN
          cycle_points.push_back(index[i]);
          cycle_ind.push_back(i);
          // we need to check if we need to eliminate the first point
          cycle_length = cycle_ind[2] - cycle_ind[0];
          change_1 = fabs( (cycle_points[1]-cycle_points[0])/cycle_points[0] ) * 100;
          change_2 = fabs( (cycle_points[2]-cycle_points[1])/cycle_points[1] ) * 100;
          change = fmax(change_1, change_2);
          if( (cycle_length < min_cycle_length) && (change < max_change)) {
            // need to eliminate the first point
            local_min[cycle_ind[0]] = false;
            // sensor the rest of the points
            eliminate_multiple_mm(index, local_min, local_max);
            // go out of the for loop and begin again
            break;
          } else {
            // do not need to eliminate, proceed further
            // now the next cycle begins with MAX
            first_max = true;
            first_min = false;
            // delete the first cycle point
            cycle_points.erase(cycle_points.begin());
            cycle_ind.erase(cycle_ind.begin());
            continue;
          }
        }
      }

      // if we go to the very end then there were no eliminations
      if(i==(n-1)) Elimination = false; // this ends the FOURTH SENSORING OPERATION

    } // end for-loop

  }

}

//' Sets the paramters of the dating algorithm
//'
//' This function sets the paramters of the dating algorithm of Bry and Boschan (1971)
//'
//' @param t_window the half-size of the rolling window to find minima and maxima
//' @param t_censor the size of the left and right margin (for the cencoring operation)
//' @param t_phase the minimum phase (bull or bear) length
//' @param t_cycle the minimum full cycle length
//' @param max_chng the change (in percentages) in the \code{"index"} that invalidates the minimum phase length rule
//' @return None
//' @note All parameters but \code{"max_chng"} are given in a number of observations.
//' For example, if data are at the monthly frequency, \code{"t_cycle=16"} defines that the minimum cycle length should be 16 months.
//' @usage setpar_dating_alg(t_window, t_censor, t_phase, t_cycle, max_chng)
//' @references
//' Bry, G. and Boschan, C. (1971). Cyclical Analysis of Time Series: Selected Procedures and
//' Computer Programs. NBER.
//'
//' Pagan, A. R. and Sossounov, K. A. (2003). A Simple Framework for Analysing Bull and Bear
//' Markets. Journal of Applied Econometrics, 18 (1), 23-46.
//'
//' Gonzalez, L., Powell, J. G., Shi, J., and Wilson, A. (2005). Two Centuries of Bull and Bear
//' Market Cycles. International Review of Economics and Finance, 14 (4), 469-486.
//' @export
// [[Rcpp::export]]

void setpar_dating_alg(int t_window = 8, int t_censor = 6, int t_phase = 4, int t_cycle = 16, double max_chng = 20) {
  roll_window_size = t_window;
  margin_size = t_censor;
  min_cycle_length = t_cycle;
  max_change = max_chng;
  min_phase_length = t_phase;
}


//' Runs the dating algorithm to identify Bull and Bear states
//'
//' This function implements the dating algorithm of Bry and Boschan (1971) to identify Bull and Bear states
//'
//' @param index vector containing the stock price index
//' @return A logical vector that contains TRUE for Bull states and FALSE for Bear states
//' @note Be aware that the states in the beginning and in the end of \code{"index"} are not properly defined.
//' The users are advised to always visually check the correctness of the result
//' (during Bull states the prices should generally increase, during the Bear states decrease).
//' @usage run_dating_alg(index)
//' @references
//' Bry, G. and Boschan, C. (1971). Cyclical Analysis of Time Series: Selected Procedures and
//' Computer Programs. NBER.
//'
//' Pagan, A. R. and Sossounov, K. A. (2003). A Simple Framework for Analysing Bull and Bear
//' Markets. Journal of Applied Econometrics, 18 (1), 23-46.
//'
//' Gonzalez, L., Powell, J. G., Shi, J., and Wilson, A. (2005). Two Centuries of Bull and Bear
//' Market Cycles. International Review of Economics and Finance, 14 (4), 469-486.
//' @export
// [[Rcpp::export]]


LogicalVector run_dating_alg(const NumericVector index) {

  // This is the main function that finds the Bull and Bear states
  // "index" is the vector of stock price index

  int k = roll_window_size;
  int phase_length = 0;
  int n = index.size();
  int i = 0; // to use in for-loops
  int ind = 0;
  double change = 0.0;

  // create vectors that will contain the positions of the local minima and maxima
  LogicalVector local_min = LogicalVector(n,false);
  LogicalVector local_max = LogicalVector(n,false);

  //********************************************************
  // FIRST RUN, identify min and max in a rolling window

  for(i = k; i < n-k; i++) {
    int start = i - k;
    int end = i + k;
    // find local max in the rolling window
    ind = which_max(index, start, end);
    // if the center of the window is the maximum in the window
    if(ind == i) local_max[i] = true;
    // find local min in the rolling window
    ind = which_min(index, start, end);
    // if the center of the window is the minimum in the window
    if(ind == i) local_min[i] = true;
  }

  //********************************************************
  // SECOND RUN, elimination of consecutive minima and maxima
  // need to find a sequence, choose only one point among all

  // eliminate consecutive maxima
  eliminate_consecutive_extr(local_max, index, false);

  // eliminate consecutive minima
  eliminate_consecutive_extr(local_min, index, true);

  //********************************************************
  // FIRST CENSORING OPERATION
  // eliminating extrema within "margin_size" of in the beginning and the end

  int w = margin_size;
  for(i = 0; i < w; i++) {
    local_min[i] = false;
    local_max[i] = false;
  }
  for(i = n-w; i < n; i++) {
    local_min[i] = false;
    local_max[i] = false;
  }

  //********************************************************
  // SECOND CENSORING OPERATION
  // eliminate multiple maxima in the beginnig, end, and within two minima
  // eliminate multiple minima in the beginnig, end, and within two maxima

  eliminate_multiple_mm(index, local_min, local_max);

  //********************************************************
  // THIRD CENSORING OPERATION
  // eliminate phases whose duration is less than "min_phase_length"
  // except when change > "max_change"

  bool first_max = false;
  bool first_min = false;
  int ind_max = 0;
  int ind_min = 0;

  bool Elimination = true;
  while(Elimination) {

    first_max = false;
    first_min = false;

    for(i = 0; i < n; i++) {
      // find the first point, identify it as max or min
      if((first_max==false) && (first_min==false) && (local_min[i]==true)) {
        first_min = true;
        ind_min   = i;
        continue;
      }
      if((first_max==false) && (first_min==false) && (local_max[i]== true)) {
        first_max = true;
        ind_max   = i;
        continue;
      }
      if((first_max==true) && (local_min[i]==true)) {
        // find out if we need to eleminate this local min
        change = fabs( (index[ind_max]-index[i])/index[ind_max]) * 100;
        phase_length = i - ind_max;
        if( (phase_length < min_phase_length) && (change < max_change) ) {
          // we need to eliminate this local min
          local_min[i] = false;
          // sensor the rest of the points
          eliminate_multiple_mm(index, local_min, local_max);
          // go out of the for loop and begin again
          break;
        } else {
          first_max = false;
          first_min = true;
          ind_min   = i;
        }
      }
      if((first_min==true) && (local_max[i]==true)) {
        // find out if we need to eleminate this local max
        change = fabs( (index[ind_min]-index[i])/index[ind_min]) * 100;
        phase_length = i - ind_min;
        if( (phase_length < min_phase_length) && (change < max_change) ) {
          // we need to eliminate this local max
          local_max[i] = false;
          // sensor the rest of the points
          eliminate_multiple_mm(index, local_min, local_max);
          // go out of the for loop and begin again
          break;
        } else {
          first_min = false;
          first_max = true;
          ind_max   = i;
        }
      }

      // if we go to the very end then there were no eliminations
      if(i==(n-1)) Elimination = false; // this ends the THIRD SENSORING OPERATION
    }
  }

  //********************************************************
  // FINAL FOURTH CENSORING OPERATION
  // eliminate the cycles whose duration is less than "min_cycle_length"
  // except when change > "max_change"

  forth_censoring(index, local_min, local_max);

  // in the end, create the vector that contains TRUE for Bull states
  LogicalVector bull = get_bull(local_min, local_max);

  return(bull);
}


//' @useDynLib bbdetection

