library(parallel)
library(Rcpp)

sourceCpp(code='
#include <RcppArmadillo.h>

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export()]]
arma::mat mvrnormArma(int n, arma::vec mu, arma::mat sigma) {
  int ncols = sigma.n_cols;
  arma::mat Y = arma::randn(n, ncols);
  return arma::repmat(mu, 1, n).t() + Y * arma::chol(sigma);
}
')


sourceCpp(code='
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export()]]
double integral_creater(Rcpp::NumericVector RA,
                     Rcpp::NumericVector RT,
                     Rcpp::NumericVector ){
  
  Rcpp::NumericVector RA_p;
  Rcpp::NumericVector RT_p;
  
  double integral=1.0;
  for (int i=0; i<RA.length();i++){
    RA_p[i] =  R::pnorm(person_RA_param_vec[i] - item_RA_param[i],0,1,1,0);
    RT_p[i] =  R::dlnorm(item_RT_param[i] - person_RT_param_vec[i],0,1/1.875,0);
    
    integral *= ifelse(RA==1,RA_p[i],1-RA_p[i]) * RT_p[i];
  }
  
  return integral;
}')

PV_RT <- function(p_index){
  file_name <- paste(c("person_",as.character(p_index),".rds"), 
                     sep = "",collapse = "")
  jags_data <- readRDS(file = file_name)
  file_name <- paste(c("parameter_",as.character(p_index),".rds"), 
                     sep = "",collapse = "")
  jags_paramer <- readRDS(file = file_name)
  item_param=jags_paramer$item_parm
  gamma_RA=jags_paramer$gamma_RA
  gamma_RT=jags_paramer$gamma_RT
  #person_param=jags_paramer$person_param
  person_sigma=jags_paramer$person_sigma
  item_sigma=jags_paramer$item_sigma
  
  M <- 5000
  I <- jags_data$I
  person_param <- array(NA,dim = c(M,I,2))
  for (i in 1:I){
    person_mu <- c(jags_paramer$proficiency_mu[i],jags_paramer$speed_mu[i])
    person_param[,i,] <- mvrnormArma(M,person_mu,person_sigma)
  }
  
  II <- jags_data$II
  JJ <- jags_data$JJ
  integral <- matrix(NA,nrow = M,ncol = I)
  
  
  for (i in 1:I){
      RA <- jags_data$RA[which(II==i)]
      RT <- jags_data$RT[which(II==i)]  
      item_diff <- item_param[JJ[which(II==i)],1]
      item_speed <- item_param[JJ[which(II==i)],2]
      for (m in 1:M){  
        RA_p <- pnorm(person_param[m,i,1]-item_diff,0,1)
        RT_p <- plnorm(item_speed-person_param[m,i,1],0,1/1.875)
        integral <- prod(ifelse(RA==1,RA_p,1-RA_p))*prod(RT_p)
        
    }
  }
  
  #for (m in 1:M){
  #  for (i in 1:I){
  #    RA <- jags_data$RA[which(II==i)]
  #    RT <- jags_data$RT[which(II==i)]  
  #    integral[m,i] <- integral_creater(RA,RT,item_param[JJ[which(II==i)],1],
  #                                      item_param[JJ[which(II==i)],2],
  #                                      rep(person_param[m,i,1],length(RA)),
  #                                      rep(person_param[m,i,2],length(RA)))
  #  }
  #}
  
  for (i in 1:I){
    RA <- jags_data$RA[which(II==i)]
    RT <- jags_data$RT[which(II==i)]
    for (m in 1:M){
      RA_p <- pnorm(person_param[m,i,1] - item_param[JJ[which(II==i)],1])
      integral[m,i] <- prod(RA_p^RA * (1-RA_p)^(1-RA)) * 
                          prod(dlnorm(x = RT, meanlog = item_param[JJ[which(II==i)],2] - person_param[m,i,2],
                                      sdlog = 1/1.875))
    }
  }
  
  mean_integral <- apply(integral,2,mean)
  
  q_mi <- matrix(NA,nrow = M,ncol = I)
  for (i in 1:I){
    for (m in 1:M){
      q_mi[m,i] <- integral[m,i]  / mean_integral[i]
    }
    q_mi[1:M,i] <- q_mi[1:M,i] / sum(q_mi[1:M,i])
  }
  
  n_pv = 5
  plausible_values <- array(NA,dim = c(n_pv,I,2))
  for (i in 1:I){
    index <- sample(x = 1:M,size = n_pv,replace = TRUE,prob = q_mi[1:M,i])
    plausible_values[1:n_pv,i,1:2] <- person_param[index,i, 1:2]
  }
  
  file_name <- paste(c("PV_person_",as.character(p_index),".rds"), 
                     sep = "",collapse = "")
  saveRDS(plausible_values, file = file_name)
  
}

p_index <- 1:3
numCores <- 3

mclapply(p_index,PV_RT,mc.cores = numCores)
