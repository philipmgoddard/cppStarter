#include <Rcpp.h>
using namespace Rcpp;

//' allRcpp
//' @description c++ version of base all(). Missing values not accepted.
//' @param x a logical vector
// [[Rcpp::export]]
bool allRcpp(LogicalVector x) {
  int n = x.size();

  for(int i = 0; i < n; i++ ) {
    if(!x[i]) return false;
  }
  return true;
}

//' cumprodRcpp
//' @description c++ version of base cumprod(). Missing values not accepted.
//' @param x a numeric vector
// [[Rcpp::export]]
NumericVector cumprodRcpp(NumericVector x){
  int n = x.size();
  NumericVector out(n);

  out[0] = x[0];
  for(int i = 1; i < n; i++) {
    out[i] = out[i - 1] * x[i];
  }
  return out;
}

//' cumminRcpp
//' @description c++ version of base cummin(). Missing values not accepted.
//' @param x a numeric vector
// [[Rcpp::export]]
NumericVector cumminRcpp(NumericVector x){
  int n = x.size();
  NumericVector out(n);

  out[0] = x[0];
  for(int i = 1; i < n; i++) {
    if(x[i] < out[i-1]) {
      out[i] = x[i];
    } else {
      out[i] = out[i-1];
    }
  }
  return out;
}

//' cummaxRcpp
//' @description c++ version of base cummax(). Missing values not accepted.
//' @param x a numeric vector
// [[Rcpp::export]]
NumericVector cummaxRcpp(NumericVector x){
  int n = x.size();
  NumericVector out(n);

  out[0] = x[0];
  for(int i = 1; i < n; i++) {
    if(x[i] > out[i-1]) {
      out[i] = x[i];
    } else {
      out[i] = out[i-1];
    }
  }
  return out;
}

//' diffRcpp
//' @description c++ version of base diff(). Missing values not accepted.
//' @param x a numeric vector
//' @param lag the specified lag
// [[Rcpp::export]]
NumericVector diffRcpp(NumericVector x, int lag = 1){
  int n = x.size() - 1 - (lag - 1);
  NumericVector out(n);

  for(int i = 0; i < n ; i++) {
    out[i] = x[i + lag] - x[i];
  }
  return out;
}

//' varRcpp
//' @description c++ version of base var(). Uses a 'two-pass' approach
//' @param x a numeric vector
// [[Rcpp::export]]
double varRcpp(NumericVector x) {
  int n = x.size();
  double tmp = 0.0;
  double mean;

  // do with two passes:
  // first calculate the mean
  for(int i = 0; i < n; i++) {
    tmp += x[i];
  }
  mean = tmp / n;

  tmp = 0.0;
  // now calculate the variance
  for(int i = 0; i < n; i++){
    tmp += pow((x[i] - mean), 2);
  }

  return (tmp / (n - 1));

}

/////////////////////////////////////////////////
/////////////////////////////////////////////////
// below functions can accept missing values in input

//' rangeRcpp
//' @description c++ version of base range(), can deal with na's
//' @param x a numeric vector
//' @param na_rm boolean, should NA's be considered?
// [[Rcpp::export]]
NumericVector rangeRcpp(NumericVector x, bool na_rm = false){
  int n = x.size();
  NumericVector out(2);
  out[0] = x[0];
  out[1] = x[0];

  for(int i = 0; i < n ; i++) {
    if(na_rm && NumericVector::is_na(x[i])){
      continue;
    }
    if(NumericVector::is_na(x[i])){
      return NumericVector::create(NA_REAL, NA_REAL);
    }
    if(x[i] < out[0]){
      out[0] = x[i];
    } else if(x[i] > out[1]) {
      out[1] = x[i];
    }
  }
  return out;
}

//' minRcpp
//' @description c++ version of base min(). can deal with missing values
//' @param x a numeric vector
//' @param na_rm boolean, should NA's be considered?
// [[Rcpp::export]]
double minRcpp(NumericVector x, bool na_rm = false) {
  int n = x.size();
  int start = 1;
  double tmp = x[0];

  // initiate- need position of first non NA
  // otherwise will fail if x[0] is NA
  if(na_rm){
    if(NumericVector::is_na(x[0]) && n == 1) {
      return INFINITY;
    }
    for(int i = 0; i < n; i++){
      if(!NumericVector::is_na(x[i])){
        start = i;
        tmp = x[i];
        break;
      }
    }
  } else {
    // if zeroth element is NA and na_rm = false, return NA
    if(NumericVector::is_na(x[0])) {
      return NA_REAL;
    }
  }

  // now use tmp to hold current min, loop through all values
  for(int i = start; i < n; i++){
    if(na_rm && NumericVector::is_na(x[i])){
      continue;
    }
    if(NumericVector::is_na(x[i])){
      return NA_REAL;
    }
    if(x[i] < tmp) tmp = x[i];
  }
  return tmp;
}

//' maxRcpp
//' @description c++ version of base max(). can deal with missing values
//' @param x a numeric vector
//' @param na_rm boolean, should NA's be considered?
// [[Rcpp::export]]
double maxRcpp(NumericVector x, bool na_rm = false) {
  int n = x.size();
  int start = 1;
  double tmp = x[0];

  // initiate- need position of first non NA
  // otherwise will fail eg consider if x[0] is NA
  if(na_rm){
    if(NumericVector::is_na(x[0]) && n == 1) {
      return -INFINITY;
    }
    for(int i = 0; i < n; i++){
      if(!NumericVector::is_na(x[i])){
        start = i;
        tmp = x[i];
        break;
      }
    }
  } else {
    // if zeroth element is NA and na_rm = false, return NA
    if(NumericVector::is_na(x[0])) {
      return NA_REAL;
    }
  }

  // now use tmp to hold current max, loop through all values
  // if hit NA and na_rm = true, continue
  // if hit NA and na_rm is false return NA
  for(int i = start; i < n; i++){
    if(na_rm && NumericVector::is_na(x[i])){
      continue;
    }
    if(NumericVector::is_na(x[i])){
      return NA_REAL;
    }
    if(x[i] > tmp) tmp = x[i];
  }
  return tmp;
}

//' maxRcpp2
//' @description c++ version of base max(). Different implementation.
//'   can deal with missing values. Its not as fast as maxRcpp()
//' @param x a numeric vector
//' @param na_rm boolean, should NA's be considered?
// [[Rcpp::export]]
double maxRcpp2(NumericVector x, bool na_rm = false) {
  int n = x.size();
  int m = sum(!is_na(x));
  double tmp;
  NumericVector x2(m);

  if(!na_rm) {
    if(any(is_na(x))) {
      return NA_REAL;
    }
  }
  x2 = x[!is_na(x)];

  tmp = x2[0];
  for(int i = 1; i < n; i++){
    if(x2[i] > tmp) tmp = x2[i];
  }
  return tmp;
}
