library(parallel)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)

PV_RT <- function(p_index){
  file_name <- paste(c("person_",as.character(p_index),".rds"), 
                     sep = "",collapse = "")
  jags_data <- readRDS(file = file_name)
  file_name <- paste(c("parameter_",as.character(p_index),".rds"), 
                     sep = "",collapse = "")
  jags_paramer <- readRDS(file = file_name)
  inits <- function(){list("gamma_RA"=jags_paramer$gamma_RA,
     "gamma_RT"=jags_paramer$gamma_RT,
     "person_param"=jags_paramer$person_param,
     "person_sigma"=jags_paramer$person_sigma,
     "item_param"=jags_paramer$item_parm,
     "item_sigma"=jags_paramer$item_sigma)}
  variable.names <-  c('person_param')
  
  
  stan_result <- stan(file = 'model.stan',
  data = jags_data,
  init = inits,
  pars = variable.names,
  #iter = 20000,
  chains = 3)
  file_name <- paste(c("samples_person_",as.character(p_index),".rds"), 
                     sep = "",collapse = "")
  saveRDS(stan_result, file = file_name)
  
  #simulations <- extract(stan_result,permuted = TRUE)
  #item_param <- round(apply(simulations[["item_param"]][25000:30000,,],c(2,3),median),3)
  #gamma_RA <- round(apply(simulations[["gamma_RA"]][25000:30000,],2,median),3)
  #gamma_RT <- round(apply(simulations[["gamma_RT"]][25000:30000,],2,median),3)
  #person_sigma <- round(apply(simulations[["person_sigma"]][25000:30000,,], c(2,3), median),3)
  #item_sigma <- round(apply(simulations[["item_sigma"]][25000:30000,,], c(2,3), median),3)
}

p_index <- 1:3
numCores <- 3

mclapply(p_index,PV_RT,mc.cores = numCores)
