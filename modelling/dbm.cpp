#include "Rcpp.h"

// [[Rcpp::export]]
int fibonacci(const int x) {
   if (x < 2) return(x);
   return (fibonacci(x - 1)) + fibonacci(x - 2);
}

// [[Rcpp::export]]
Rcpp::DataFrame fsim(Rcpp::NumericVector x, const Rcpp::NumericVector y, 
		     const Rcpp::NumericMatrix P, const Rcpp::NumericVector a, const Rcpp::NumericMatrix B, const Rcpp::NumericVector tau, const double ymin, const int d, const int t0){

  Rcpp::NumericVector yhat(y.length());
  Rcpp::NumericMatrix U(y.length(),P.ncol());
  // Rcpp::Rcout << "U dim " << U.nrow() << " " << U.ncol() << std::endl;
  // Rcpp::Rcout << "P dim " << P.nrow() << " " << P.ncol() << std::endl;
  // Rcpp::Rcout << "tau " << tau.length() << std::endl;
  
  for(int ii=0; ii < t0-1; ++ii){
    //    Rcpp::Rcout << ii << std::endl;
    yhat[ii] = y[ii];
    if( std::isnan(y[ii]) ){
      for(int jj=0; jj < U.ncol(); ++jj){
	//Rcpp::Rcout << "jj " << jj << std::endl;
	U(ii,jj) = pow(ymin,tau[jj])*P(ii,jj);
      }
    }else{
      for(int jj=0; jj < U.ncol(); ++jj){
	//Rcpp::Rcout << "jj " << jj << std::endl;
	U(ii,jj) = pow(y[ii],tau[jj])*P(ii,jj);
      }
    }
  }
  //Rcpp::Rcout << "main loop" << std::endl;
  for(int ii=t0-1; ii < yhat.length(); ++ii){
    //Rcpp::Rcout << ii << std::endl;
    x = a*x;
    for(int jj=0; jj < U.ncol(); ++jj){
      x = x + B(Rcpp::_,jj)*U(ii-d,jj);
    }
    yhat[ii] = sum(x) + ymin;
    //Rcpp::Rcout << ii << " " << yhat[ii] << " " << pow(yhat[ii],tau) << " " << p[ii] << std::endl;
    for(int jj=0; jj < U.ncol(); ++jj){
      U(ii,jj) = pow(yhat[ii],tau[jj])*P(ii,jj);
    }
    // u[ii] = pow(yhat[ii],tau)*p[ii];
  }
  
  return Rcpp::DataFrame::create( Rcpp::Named("x") = yhat,         // simple assign
				  Rcpp::Named("u") = U);
}
  
    
