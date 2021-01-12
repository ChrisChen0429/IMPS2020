// The input data is a vector 'y' of length 'N'.
data {
  int<lower=1> N;
  int<lower=1> I;
  int<lower=1> J;
  int<lower=1> K;
  int<lower=1,upper=I> II[N];
  int<lower=1,upper=J> JJ[N];
  int<lower=0,upper=1> RA[N];
  matrix<lower=0>[I,K] x;
}

parameters {
  vector[I] person_param;
  vector[J] item_param;
  vector[K] gamma_RA;
  real<lower=0> sigma_beta;
  real<lower=0> sigma_theta;
}
transformed parameters{
  vector[I] person_mu;
  person_mu[1:I] = x[1:I,1:K]* gamma_RA[1:K];
}

model {
  for (i in 1:I){
    person_param[i] ~ normal(person_mu[i],sigma_theta);  
  }
  item_param ~ normal(0, sigma_beta);
  RA ~ bernoulli(Phi_approx(person_param[II] - item_param[JJ]));  
}

