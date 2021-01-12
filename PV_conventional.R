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
    variable.names <-  c('sigma_theta','item_param','person_mu')
    #samples <- stan(file = 'model_conventional.stan',
    #                data = jags_data,
    #                chains = 3)
    
    file_name <- paste(c("con_sample_person",as.character(p_index),".rds"), 
                       sep = "",collapse = "")
    # saveRDS(samples, file = file_name)
    samples <- readRDS(file = file_name)
    simulations <- extract(samples,permuted = TRUE)
    person_mu <- round(apply(simulations[["person_mu"]][1500:3000,],2,mean),3)
    item_param <- round(apply(simulations[["item_param"]][1500:3000,],2,mean),3)
    sigma_theta <- round(mean(simulations[["sigma_theta"]][1500:3000]),3)
    
    
    M <- 2000
    I <- jags_data$I
    person_param <- matrix(NA,nrow = M,ncol = I)
    for (i in 1:I){
        person_param[1:M,i] <- rnorm(n = M,mean = person_mu[i], sd = sigma_theta)
    }
    
    II <- jags_data$II
    JJ <- jags_data$JJ
    integral_RA <- matrix(NA,nrow = M,ncol = I)
    for (i in 1:I){
        for (m in 1:M){
            RA <- jags_data$RA[which(II==i)]
            RA_p <- pnorm(person_param[m,i] - item_param[JJ[which(II==i)]])
            integral_RA[m,i] <- prod(RA_p^RA * (1-RA_p)^(1-RA))
        }
    }
    
    mean_integral_RA <- apply(integral_RA,2,mean)
    q_mi_RA <- matrix(NA,nrow = M,ncol = I)
    for (i in 1:I){
        for (m in 1:M){
            q_mi_RA[m,i] <- integral_RA[m,i]  / mean_integral_RA[i]
        }
        q_mi_RA[1:M,i] <- q_mi_RA[1:M,i] / sum(q_mi_RA[1:M,i])
    }
    
    n_pv = 5
    plausible_values <- matrix(NA,nrow = n_pv, ncol = I)
    for (i in 1:I){
        index <- sample(x = 1:M,size = n_pv,replace = TRUE,prob = q_mi_RA[1:M,i])
        plausible_values[1:n_pv,i] <- person_param[index,i]
    }
    
    file_name <- paste(c("con_PV_person_",as.character(p_index),".rds"), 
                       sep = "",collapse = "")
    saveRDS(plausible_values, file = file_name)
    
    
}

p_index <- 1:3
numCores <- 3

mclapply(p_index,PV_RT,mc.cores = numCores)





set.seed(3356)
data <- data.frame(
    engagement = 0,
    useful = 0,
    prior_experience = 0
)
for (i in 1:20){
    data <- rbind(data,round(runif(3,0,100)))
    data <- rbind(data,round(runif(3,0,100)))
}
data <- data[2:nrow(data),]

