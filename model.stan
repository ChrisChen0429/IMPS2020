// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N;
  int<lower=1> I;
  int<lower=1> J;
  int<lower=1> K;
  int<lower=1,upper=I> II[N];
  int<lower=1,upper=J> JJ[N];
  real<lower=0> RT[N];
  int<lower=0,upper=1> RA[N];
  matrix<lower=0>[I,K] x;
  matrix<lower=0,upper=1>[2,2] Omega;
}

parameters {
  matrix[I,2] person_param;
  matrix[J,2] item_param;
  matrix[2,2] person_sigma;
  matrix[2,2] item_sigma;
  vector[K] gamma_RA;
  vector[K] gamma_RT;
}
transformed parameters{
  matrix[I,2] person_mu;
  matrix[J,2] item_mu;
  person_mu[1:I,1] = x[1:I,1:K]* gamma_RA[1:K];
  person_mu[1:I,2] = x[1:I,1:K]* gamma_RT[1:K];
  
}

model {
  person_sigma ~ inv_wishart(2,Omega);
  item_sigma ~ inv_wishart(2,Omega);
  for (i in 1:I){
    person_param[i,1:2] ~ multi_normal(person_mu[i,1:2],person_sigma);  
  }
  for (j in 1:J){
    item_param[j,1:2] ~ multi_normal([0,0],item_sigma);  
  }
  RA ~ bernoulli(Phi_approx(person_param[II,1] - item_param[JJ,1]));  
  RT ~ lognormal(item_param[JJ,2] - person_param[II,2],1/1.875);  
}

