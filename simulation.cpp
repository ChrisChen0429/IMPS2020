#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export()]]
int integral_creater(Rcpp::NumericVector RA,
                     Rcpp::NumericVector RT,
                     Rcpp::NumericVector item_RA_param,
                     Rcpp::NumericVector item_RT_param,
                     Rcpp::NumericVector person_RA_param_vec,
                     Rcpp::NumericVector person_RT_param_vec){
  int integral=1;
  Rcpp::NumericVector RT_scale = item_RT_param-person_RT_param_vec;
  
  Rcpp::NumericVector RA_p;
  Rcpp::NumericVector RT_p;
  
  for (int i; i<RA.length();i++){
    RA_p[i] =  R::pnorm(person_RA_param_vec[i] - item_RA_param[i],0,1,1,0);
    RT_p[i] =  R::dlnorm(item_RT_param[i] - person_RT_param_vec[i],0,1/1.875,0);
  }
  
  
  integral = arma::prod(pow(RA_p,RA) * pow(1-RA_p,1-RA))*arma::prod(RT_p);
  
  return integral;
  
}