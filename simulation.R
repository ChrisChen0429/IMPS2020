library(lqmm)
library(Rcpp)
set.seed(3356)
## simiulation 
gamma_RA_list <- list()
gamma_RA_list[[1]] <- c(-1,1,1)
gamma_RA_list[[2]] <- c(0,1,-1)
gamma_RA_list[[3]] <- c(-1,1,1)
gamma_RT_list <- list()
gamma_RT_list[[1]] <- c(-3,-0.5,-0.5)
gamma_RT_list[[2]] <- c(-3.5,-0.5,0.5)
gamma_RT_list[[3]] <- c(-4,0.5,0.5)
I <- 4000  # number of task-takers
K <- 3     # number of context variable
x <- matrix(NA,nrow = I,ncol = K)
x[,1] <- rep(1,4000)
x[,2] <- c(rep(0,2000),rep(1,2000))
x[,3] <- rep(c(rep(0,1000),rep(1,1000)),2)

## matrix sampling 
### eight block design with each task-taker take four block
### (ABCD) (BCDE) (CDEF) (DEFG) (EFGH) (FGHA) (GHAB) (HABC)
### each block have 7 items and each task-taker take 28 items
II <- rep(1:I,each=28)
JJ <- c()
J <- 56 
block_list <- list()
block_list[[1]] <- 1:28
block_list[[2]] <- 8:35
block_list[[3]] <- 15:42
block_list[[4]] <- 22:49
block_list[[5]] <- 29:56
block_list[[6]] <- c(36:56,1:7)
block_list[[7]] <- c(43:56,1:14)
block_list[[8]] <- c(50:56,1:21)
for (i in 1:500){
    for (j in 1:8){
        JJ <- c(JJ,block_list[[j]])
    }
}







sourceCpp(code="#include <RcppArmadillo.h>
// [[Rcpp::plugins('cpp11')]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export()]]
arma::vec innerproduct(arma::mat& X, arma::colvec& gamma_RA){
  int I = X.n_rows;
  arma::vec proficiency_mu(I);
  proficiency_mu = X * gamma_RA;
  return proficiency_mu;
}
")

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

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export()]]
Rcpp::NumericVector RA_generator(Rcpp::NumericVector person_param,
                       Rcpp::NumericVector item_parm,
                       Rcpp::NumericVector II,
                       Rcpp::NumericVector JJ){
  Rcpp::NumericVector RA(II.length());
  for (int n = 0; n <II.length(); n++){
    RT[n] = exp(R::rnorm(item_parm[JJ[n]] - person_param[II[n]],1/1.875));
  }
  return RT;
}
')

sourceCpp(code='
#include <RcppArmadillo.h>

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export()]]
Rcpp::NumericVector RT_generator(Rcpp::NumericVector person_param,
                       Rcpp::NumericVector item_parm,
                       Rcpp::NumericVector II,
                       Rcpp::NumericVector JJ){
  Rcpp::NumericVector RT(II.length());
  for (int n = 0; n <II.length(); n++){
    RT[n] = exp(R::rnorm(item_parm[JJ[n]] - person_param[II[n]],1/1.875));
  }
  return RT;
}
')

sourceCpp(code='
#include <RcppArmadillo.h>

// [[Rcpp::plugins("cpp11")]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::export()]]
Rcpp::NumericVector RA_generator(Rcpp::NumericVector person_param,
                                 Rcpp::NumericVector item_parm,
                                 Rcpp::NumericVector II,
                                 Rcpp::NumericVector JJ){
  Rcpp::NumericVector RA(II.length());
  Rcpp::NumericVector prob(II.length());
  for (int n = 0; n <II.length(); n++){
    prob[n] = arma::normcdf(item_parm[JJ[n]] - person_param[II[n]]);
  }
  RA = ifelse(prob<=0.5,0,1);
  return RA;
}')


for (p_index in 1:3){
        person_cor <- c(-0.5,0,0.5)[p_index]
        item_cor <- 0.5
        person_cov <- person_cor * sqrt(0.25)
        person_sigma <- matrix(c(1,person_cov,person_cov,0.25),2,2)
        ## personal parameters
        gamma_RA <- gamma_RA_list[[p_index]]
        gamma_RT <- gamma_RT_list[[p_index]]
            
        proficiency_mu <- as.vector(innerproduct(x,gamma_RA))
        speed_mu <- as.vector(innerproduct(x,gamma_RT))
        
        person_param <- matrix(NA,nrow=I, ncol=2)
        for (i in 1:I){
            person_param[i,] <- mvrnormArma(1, c(proficiency_mu[i],speed_mu[i]), person_sigma)
        }
         
        
        item_cov <- item_cor * sqrt(0.14)
        item_sigma <- matrix(c(1,item_cov,item_cov,0.14),2,2)
        item_parm <- mvrnormArma(J, c(0,0), item_sigma)
            

        parameter_data <- list(gamma_RA=gamma_RA,gamma_RT=gamma_RT,
            proficiency_mu=proficiency_mu,speed_mu=speed_mu,
            person_sigma=person_sigma,person_param=person_param,
            item_sigma=item_sigma,item_parm=item_parm)
        file_name <- paste(c("parameter_",as.character(p_index),".rds"), 
                             sep = "",collapse = "")
        saveRDS(parameter_data, file = file_name)


        #### generate the item response
        RA <- RA_generator(person_param[,1],item_parm[,1],II,JJ)
        RT <- RT_generator(person_param[,2],item_parm[,2],II,JJ)

        jags_data <- list(I = I,J = J,K = ncol(x),
            N = length(II),II = II,JJ = JJ,
            RT = RT,RA = RA,x = x,
            Omega = matrix(c(1,0,0,1),ncol = 2,nrow = 2))
        file_name <- paste(c("person_",as.character(p_index),".rds"), 
                             sep = "",collapse = "")
        saveRDS(jags_data, file = file_name)
}

