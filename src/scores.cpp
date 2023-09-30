#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]

// univariate scoring rules

// [[Rcpp::export]]
arma::rowvec crps_cpp(arma::rowvec y, arma::mat x){

  int m = x.n_rows;
  int d = x.n_cols;
  arma::rowvec out(m);
  arma::rowvec exy(m);
  arma::rowvec exx(m);

  for (int k = 1; k < (m+1); k++) {

    arma::rowvec xtmp = x.row(k-1);
    exy(k-1) = sum(abs(xtmp - y(k-1)));

    for (int i = 1; i < (d+1); i++) {
      for (int j = i; j < (d+1); j++) {
        exx(k-1) += 2*abs(xtmp(i-1) - xtmp(j-1));
      }
    }

  }


  out = exy/d - exx/(2*pow(d, 2));

  return out;

}

// [[Rcpp::export]]
arma::rowvec crps_sml_cpp(arma::rowvec y, arma::mat x){

  int m = x.n_rows;
  int d = x.n_cols;
  arma::rowvec out(m);
  arma::rowvec exy(m);
  arma::rowvec exx(m);

  for (int k = 1; k < (m+1); k++) {

    arma::rowvec xtmp = x.row(k-1);
    exy(k-1) = sum(abs(xtmp - y(k-1)));

    for (int i = 1; i < (d+1); i++) {
      for (int j = i; j < (d+1); j++) {
        exx(k-1) += 2*abs(xtmp(i-1) - xtmp(j-1));
      }
    }

  }


  out = exy/d - exx/(2*d*(d-1));

  return out;

}

// [[Rcpp::export]]
arma::rowvec crps_mc_cpp(arma::rowvec y, arma::mat x){

  int m = x.n_rows;
  int d = x.n_cols;
  arma::rowvec out(m);
  arma::rowvec exy(m);
  arma::rowvec exx(m);

  for (int k = 1; k < (m+1); k++) {

    arma::rowvec xtmp = x.row(k-1);
    exy(k-1) = sum(abs(xtmp - y(k-1)));

    for (int i = 1; i < d; i++) {
      exx(k-1) += abs(xtmp(i-1) - xtmp(i));
    }

  }


  out = exy/d - exx/(2*(d-1));

  return out;

}

// multivariate scoring rules

// [[Rcpp::export]]
double euclnorm_cpp(arma::rowvec x){
  double out = sqrt(sum(square(x)));
  return(out);
}

// [[Rcpp::export]]
arma::rowvec es_cpp(arma::mat y, arma::cube x){

  int m = y.n_rows;
  int n = x.slice(0).n_rows;
  arma::rowvec s1(m);
  arma::rowvec s2(m);
  arma::rowvec out(m);

  for (int k = 1; k < (m+1); k++) {

    arma::mat xtmp = x.slice(k-1);

    for (int i = 1; i < (n+1); i++) {
      s1(k-1) += euclnorm_cpp(xtmp.row(i-1) - y.row(k-1));

      for (int j = i; j < (n+1); j++) {
        s2(k-1) += 2*euclnorm_cpp(xtmp.row(i-1) - xtmp.row(j-1));
      }
    }

  }

  out = s1/n - s2 /(2*pow(n, 2));

  return out;

}

// [[Rcpp::export]]
arma::rowvec es_mc_cpp(arma::mat y, arma::cube x){

  int m = y.n_rows;
  int n = x.slice(0).n_rows;
  arma::rowvec s1(m);
  arma::rowvec s2(m);
  arma::rowvec out(m);

  for (int k = 1; k < (m+1); k++) {

    arma::mat xtmp = x.slice(k-1);

    for (int i = 1; i < n; i++) {
      s1(k-1) += euclnorm_cpp(xtmp.row(i-1) - y.row(k-1));
      s2(k-1) += euclnorm_cpp(xtmp.row(i-1) - xtmp.row(i));
    }

    // add last value (to avoid two loops)
    s1(k-1) += euclnorm_cpp(xtmp.row(n-1) - y.row(k-1));


  }

  out = s1/n - s2 /(2*(n-1));

  return out;

}

// [[Rcpp::export]]
arma::rowvec vs_cpp(arma::mat y, arma::cube x, arma::mat w, double p){

  int m = y.n_rows;
  int d = y.n_cols;
  double vy = 0;
  double vx = 0;
  arma::rowvec out(m);

  for (int l = 1; l < (m+1); l++) {

    // access l-th entry of y and x
    arma::mat xtmp = x.slice(l-1);
    arma::rowvec ytmp = y.row(l-1);

    for (int i = 1; i < (d+1); i++) {
      for (int j = i; j < (d+1); j++) {
        vy = pow(abs(ytmp(i-1)-ytmp(j-1)), p);
        vx = mean(pow(abs(xtmp.col(i-1) - xtmp.col(j-1)), p));
        out(l-1) += 2*w(i-1,j-1)*pow(vy - vx, 2);
      }
    }

  }

  return out;
}

// [[Rcpp::export]]
arma::rowvec ds_cpp(arma::cube x){

  int m = x.n_slices;
  int d = x.slice(0).n_cols;
  int n = x.slice(0).n_rows;
  arma::rowvec y(d);
  arma::mat z(d,d);
  arma::rowvec out(m);

  for (int i = 1; i < (m+1); i++) {

    arma::mat xtmp = x.slice(i-1);

    y = sqrt(n)*mean(xtmp, 0);

    z = (xtmp.t()*xtmp - y.t() * y)/(n-1);
    out(i-1) = det(z);

  }


  return out;
}

// slower than C++ code for l1median_VaZh in pcaPP; therefore not exported and used
arma::rowvec step_l1median_cpp(arma::rowvec y, arma::mat X, double zero_tol) {

  // *Weighted* L1-median (weights eta := 1)
  //
  // Vardi, Y. and Zhang, C.-H.  (2000).
  // The multivariate $L_1$-median and associated data depth,
  // \emph{Proc. National Academy of Science} {\bf 97}(4): 1423--1426.

  int ind;
  int d = X.n_cols;
  int n = X.n_rows;
  arma::mat Xtmp = X;
  arma::rowvec out(d);
  arma::rowvec y_di(n);

  Xtmp.each_row() -= y; // X - y by row
  y_di = sqrt(sum(pow(Xtmp, 2),1)).t(); // compute euclidean norm by row
  ind = y_di.index_min(); // get index with minimum value

  if (y_di(ind) < zero_tol * median(y_di)) {

    double r;
    arma::rowvec R(d);
    arma::rowvec xmax(2);
    arma::rowvec xmin(2);

    y_di = 1/y_di;
    y_di.shed_col(ind);

    Xtmp.shed_row(ind);
    R = sum(Xtmp.each_col() % y_di.t(), 0);
    X.shed_row(ind);
    out = sum(X.each_col() % y_di.t(), 0)/sum(y_di);

    R = pow(R, 2);
    r = 1/sqrt(sum(R));
    xmax(0) = 0;
    xmax(1) = 1-r;
    xmin = 1-xmax;
    out = xmax.max() * out + xmin.min() * y;

  } else {

    y_di = 1/y_di;
    out = sum(X.each_col() % y_di.t(), 0)/sum(y_di);

  }

  return out;

}
arma::mat l1median_cpp(arma::cube X, double maxit, double tol, double zero_tol) {

  int m = X.n_slices;
  int d = X.slice(0).n_cols;
  arma::rowvec y(d);
  arma::rowvec ynew(d);
  arma::mat out(m, d);

  for (int k = 1; k < (m+1); k++) {

    arma::mat Xtmp = X.slice(k-1);

    int iter = 0;
    double diff = std::numeric_limits<int>::max();

    // initialize median
    y = median(Xtmp, 0);

    while(diff > tol) {

      if (iter == maxit) {
        Rcpp::stop("maxit reached without convergence!");
      }

      ynew = step_l1median_cpp(y, Xtmp, zero_tol);
      diff = sqrt(sum(pow(y-ynew, 2)));
      iter = iter+1;
      y = ynew;

    }

    out.row(k-1) = y;

  }

  return out;

}


