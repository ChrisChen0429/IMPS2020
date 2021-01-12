library(rstan)
library(coda)

## load jags data
K = 3
I = 4000
p_index = 1
i_index = 1
x <- matrix(NA,nrow = I,ncol = K)
x[,1] <- rep(1,4000)
x[,2] <- c(rep(0,2000),rep(1,2000))
x[,3] <- rep(c(rep(0,1000),rep(1,1000)),2)
person_index_1 <- which(x[,2]==0)
person_index_2 <- which(x[,2]==1)
person_index_3 <- which(x[,3]==0)
person_index_4 <- which(x[,3]==1)


######## overall mean and variance ########
p_index = 3
file_name <- paste(c("parameter_",as.character(p_index),".rds"), 
                   sep = "",collapse = "")
parameter_data <- readRDS(file = file_name)
print(round(apply(parameter_data$person_param,2,mean),3))
print(round(apply(parameter_data$person_param,2,var),3))


file_name <- paste(c("PV_person_",as.character(p_index),".rds"), 
                   sep = "",collapse = "")
plausible_value = readRDS(file_name)

pv_mean = c()
pv_variance = c()
for (pv in 1:5){
    print(round(c(mean(plausible_value[pv,,1]),var(plausible_value[pv,,1])),3))
    pv_mean = c(pv_mean, mean(plausible_value[pv,,1]))
    pv_variance = c(pv_variance ,var(plausible_value[pv,,1]) )
}
mean(pv_mean)
mean(pv_variance)


pv_mean = c()
pv_variance = c()
for (pv in 1:5){
    print(round(c(mean(plausible_value[pv,1:4000,2]),var(plausible_value[pv,1:4000,2])),3))
    pv_mean = c(pv_mean, mean(plausible_value[pv,,2]))
    pv_variance = c(pv_variance ,var(plausible_value[pv,,2]) )
}
mean(pv_mean)
mean(pv_variance)


file_name <- paste(c("con_PV_person_",as.character(p_index),".rds"), 
                   sep = "",collapse = "")
plausible_value = readRDS(file_name)

pv_mean = c()
pv_variance = c()
for (pv in 1:5){
    print(round(c(mean(plausible_value[pv,]),var(plausible_value[pv,])),3))
    pv_mean = c(pv_mean, mean(plausible_value[pv,]))
    pv_variance = c(pv_variance ,var(plausible_value[pv,]) )
}
mean(pv_mean)
mean(pv_variance)



file_name <- paste(c("samples_person_",as.character(p_index),".rds"), 
                   sep = "",collapse = "")
stan_result <- readRDS(file = file_name)
simulations <- extract(stan_result,permuted = TRUE)
person_param <- round(apply(simulations[["person_param"]],c(2,3),mean),3)

apply(person_param,2,mean)
apply(person_param,2,var)







######## group mean ########
# true value
result <- c(0,0)
for (p_index in 1:3){
        file_name <- paste(c("parameter_",as.character(p_index),".rds"), 
                                sep = "",collapse = "")
        parameter_data <- readRDS(file = file_name)

        
        result <- rbind(result,round(apply(parameter_data$person_param[person_index_1,1:2],2,mean),3))
        result <- rbind(result,round(apply(parameter_data$person_param[person_index_2,1:2],2,mean),3))
        result <- rbind(result,round(apply(parameter_data$person_param[person_index_3,1:2],2,mean),3))
        result <- rbind(result,round(apply(parameter_data$person_param[person_index_4,1:2],2,mean),3))
}


write.csv(result,'text.csv')


## plausible value generation with response time
result <- c(0,0)
L =5
for (p_index in 1:3){
        file_name <- paste(c("PV_person_",as.character(p_index),".rds"), 
                           sep = "",collapse = "")
        plausible_value = readRDS(file_name)
        case_summary <- array(NA,dim=c(4,L,2))
        for (l in 1:L){
            case_summary[1,l,1:2] <- apply(plausible_value[l,person_index_1,1:2],2,mean)
            case_summary[2,l,1:2] <- apply(plausible_value[l,person_index_2,1:2],2,mean)
            case_summary[3,l,1:2] <- apply(plausible_value[l,person_index_3,1:2],2,mean)
            case_summary[4,l,1:2] <- apply(plausible_value[l,person_index_4,1:2],2,mean)
        }
        for (k in 1:4){
            r_star <- c(mean(case_summary[k,1:L,1]),mean(case_summary[k,1:L,2]))
            result <- rbind(result,round(r_star,3))
        }
}


write.csv(result,'text.csv')

## plausible value generation without response time
result <- c(0,0)
L =5
for (p_index in 1:3){
    file_name <- paste(c("con_PV_person_",as.character(p_index),".rds"), 
                       sep = "",collapse = "")
    plausible_value = readRDS(file_name)
    case_summary <- matrix(NA,nrow = 4,ncol = L)
    for (l in 1:L){
        case_summary[1,l] <- mean(plausible_value[l,person_index_1])
        case_summary[2,l] <- mean(plausible_value[l,person_index_2])
        case_summary[3,l] <- mean(plausible_value[l,person_index_3])
        case_summary[4,l] <- mean(plausible_value[l,person_index_4])
    }
    for (k in 1:4){
        r_star <- c(mean(case_summary[k,1:L]))
        result <- rbind(result,round(r_star,3))
    }
}


write.csv(result,'text.csv')







result <- c(0,0)
for (p_index in 1:3){
    file_name <- paste(c("samples_person_",as.character(p_index),".rds"), 
                       sep = "",collapse = "")
    stan_result <- readRDS(file = file_name)
    simulations <- extract(stan_result,permuted = TRUE)
    person_param <- round(apply(simulations[["person_param"]],c(2,3),mean),3)
    
    case_summary <- matrix(NA,nrow=4,ncol = 2)
    case_summary[1,1:2] <- apply(person_param[person_index_1,1:2],2,mean)
    case_summary[2,1:2] <- apply(person_param[person_index_2,1:2],2,mean)
    case_summary[3,1:2] <- apply(person_param[person_index_3,1:2],2,mean)
    case_summary[4,1:2] <- apply(person_param[person_index_4,1:2],2,mean)
    result <- rbind(result,case_summary)
}


write.csv(result,'text.csv')







p_index = 3
file_name <- paste(c("parameter_",as.character(p_index),".rds"), 
                   sep = "",collapse = "")
parameter_data <- readRDS(file = file_name)

round(mean(sort(parameter_data$person_param[1:4000,1])[200:201]),3)
round(mean(sort(parameter_data$person_param[1:4000,1])[400:401]),3)
round(mean(sort(parameter_data$person_param[1:4000,1])[1000:1001]),3)
round(mean(sort(parameter_data$person_param[1:4000,1])[2000:2001]),3)
round(mean(sort(parameter_data$person_param[1:4000,1])[3000:3001]),3)
round(mean(sort(parameter_data$person_param[1:4000,1])[3600:3601]),3)
round(mean(sort(parameter_data$person_param[1:4000,1])[3800:3801]),3)


round(mean(sort(parameter_data$person_param[1:4000,2])[200:201]),3)
round(mean(sort(parameter_data$person_param[1:4000,2])[400:401]),3)
round(mean(sort(parameter_data$person_param[1:4000,2])[1000:1001]),3)
round(mean(sort(parameter_data$person_param[1:4000,2])[2000:2001]),3)
round(mean(sort(parameter_data$person_param[1:4000,2])[3000:3001]),3)
round(mean(sort(parameter_data$person_param[1:4000,2])[3600:3601]),3)
round(mean(sort(parameter_data$person_param[1:4000,2])[3800:3801]),3)



file_name <- paste(c("samples_person_",as.character(p_index),".rds"), 
                   sep = "",collapse = "")
stan_result <- readRDS(file = file_name)
simulations <- extract(stan_result,permuted = TRUE)
person_param <- round(apply(simulations[["person_param"]],c(2,3),mean),3)

round(mean(sort(person_param[1:4000,1])[200:201]),3)
round(mean(sort(person_param[1:4000,1])[400:401]),3)
round(mean(sort(person_param[1:4000,1])[1000:1001]),3)
round(mean(sort(person_param[1:4000,1])[2000:2001]),3)
round(mean(sort(person_param[1:4000,1])[3000:3001]),3)
round(mean(sort(person_param[1:4000,1])[3600:3601]),3)
round(mean(sort(person_param[1:4000,1])[3800:3801]),3)


round(mean(sort(person_param[1:4000,2])[200:201]),3)
round(mean(sort(person_param[1:4000,2])[400:401]),3)
round(mean(sort(person_param[1:4000,2])[1000:1001]),3)
round(mean(sort(person_param[1:4000,2])[2000:2001]),3)
round(mean(sort(person_param[1:4000,2])[3000:3001]),3)
round(mean(sort(person_param[1:4000,2])[3600:3601]),3)
round(mean(sort(person_param[1:4000,2])[3800:3801]),3)



file_name <- paste(c("PV_person_",as.character(p_index),".rds"), 
                   sep = "",collapse = "")
plausible_value = readRDS(file_name)


result <- matrix(NA,nrow = 5,ncol=7)
for (pv in 1:5){
    result[pv,1] <- round(mean(sort(plausible_value[pv,,1])[200:201]),3)
    result[pv,2] <- round(mean(sort(plausible_value[pv,,1])[400:401]),3)
    result[pv,3] <- round(mean(sort(plausible_value[pv,,1])[1000:1001]),3)
    result[pv,4] <- round(mean(sort(plausible_value[pv,,1])[2000:2001]),3)
    result[pv,5] <- round(mean(sort(plausible_value[pv,,1])[3000:3001]),3)
    result[pv,6] <- round(mean(sort(plausible_value[pv,,1])[3600:3601]),3)
    result[pv,7] <- round(mean(sort(plausible_value[pv,,1])[3800:3801]),3)
}
write.csv(result,'text.csv')


result <- matrix(NA,nrow = 5,ncol=7)
for (pv in 1:5){
    result[pv,1] <- round(mean(sort(plausible_value[pv,,2])[200:201]),3)
    result[pv,2] <- round(mean(sort(plausible_value[pv,,2])[400:401]),3)
    result[pv,3] <- round(mean(sort(plausible_value[pv,,2])[1000:1001]),3)
    result[pv,4] <- round(mean(sort(plausible_value[pv,,2])[2000:2001]),3)
    result[pv,5] <- round(mean(sort(plausible_value[pv,,2])[3000:3001]),3)
    result[pv,6] <- round(mean(sort(plausible_value[pv,,2])[3600:3601]),3)
    result[pv,7] <- round(mean(sort(plausible_value[pv,,2])[3800:3801]),3)
}
write.csv(result,'text.csv')





file_name <- paste(c("con_PV_person_",as.character(p_index),".rds"), 
                   sep = "",collapse = "")
plausible_value = readRDS(file_name)


result <- matrix(NA,nrow = 5,ncol=7)
for (pv in 1:5){
    result[pv,1] <- round(mean(sort(plausible_value[pv,])[200:201]),3)
    result[pv,2] <- round(mean(sort(plausible_value[pv,])[400:401]),3)
    result[pv,3] <- round(mean(sort(plausible_value[pv,])[1000:1001]),3)
    result[pv,4] <- round(mean(sort(plausible_value[pv,])[2000:2001]),3)
    result[pv,5] <- round(mean(sort(plausible_value[pv,])[3000:3001]),3)
    result[pv,6] <- round(mean(sort(plausible_value[pv,])[3600:3601]),3)
    result[pv,7] <- round(mean(sort(plausible_value[pv,])[3800:3801]),3)
}
write.csv(result,'text.csv')

















library(ggplot2)
file_name <- paste(c("PV_person_",as.character(p_index),".rds"), 
                   sep = "",collapse = "")
plausible_value = readRDS(file_name)
file_name <- paste(c("con_PV_person_",as.character(p_index),".rds"), 
                   sep = "",collapse = "")
conventional_plausible_value = readRDS(file_name)

average_plausible_value <- c()
for (i in 1:5){
    average_plausible_value <- rbind(average_plausible_value, sort(plausible_value[i,,1]))
}
average_plausible_value <- apply(average_plausible_value, 2,mean)

average_conventional_plausible_value <- c()
for (i in 1:5){
    average_conventional_plausible_value <- rbind(average_conventional_plausible_value, 
                                                  sort(conventional_plausible_value[i,]))
}
average_conventional_plausible_value <- apply(average_conventional_plausible_value, 2,mean)

person_param <- round(apply(simulations[["person_param"]],c(2,3),mean),3)
EAP <- person_param[,1]
df <- data.frame(plausible.value=c(parameter_data$person_param[1:4000,1],
                                   plausible_value[1,,1],
                                   average_plausible_value,
                                   average_conventional_plausible_value,
                                   EAP = EAP),
                 type = c(rep("True",4000),
                          rep("PV1",4000),
                          rep("EAP",4000),
                          rep("Aveage PVs",4000),
                          rep("Aveage PVs with RT",4000)))


P1 <- ggplot() + 
    geom_density(data = df[df$type!="Aveage PVs" & df$type!="True" & df$type!="Aveage PVs with RT",],
                 mapping = aes(x=plausible.value,color=type),size=1,alpha=0.4)+ 
    geom_density(data=df[df$type=="Aveage PVs" | df$type=="True" | df$type=="Aveage PVs with RT",],
                 mapping = aes(x=plausible.value,color=type),size =1, alpha =0.4)+ 
    scale_fill_grey() +
    theme_minimal()+
    theme(axis.text=element_text(size=20,face="bold"),
          axis.title=element_text(size=20,face="bold"),
          legend.text =element_text(size=20),
          legend.title =element_text(size=20),
          legend.key.size = unit(1, "cm"))+
    labs(x ="Estimated Values", y = "Density")












file_name <- paste(c("PV_person_",as.character(p_index),".rds"), 
                   sep = "",collapse = "")
plausible_value = readRDS(file_name)
file_name <- paste(c("con_PV_person_",as.character(p_index),".rds"), 
                   sep = "",collapse = "")
conventional_plausible_value = readRDS(file_name)

average_plausible_value <- c()
for (i in 1:5){
    average_plausible_value <- rbind(average_plausible_value, sort(plausible_value[i,,2]))
}
average_plausible_value <- apply(average_plausible_value, 2,mean)

person_param <- round(apply(simulations[["person_param"]],c(2,3),mean),3)
EAP <- person_param[,2]
df <- data.frame(plausible.value=c(parameter_data$person_param[1:4000,2],
                                   plausible_value[1,,2],
                                   average_plausible_value,
                                   EAP = EAP),
                 type = c(rep("True",4000),
                          rep("PV1",4000),
                          rep("EAP",4000),
                          rep("Aveage PVs with RT",4000)))
P2 <- ggplot() + 
    geom_density(data = df[df$type!="Aveage PVs with RT" & df$type!="True",],
                 mapping = aes(x=plausible.value,color=type),size= 1,alpha=0.4)+ 
    geom_density(data=df[df$type=="Aveage PVs with RT" | df$type=="True",],
                 mapping = aes(x=plausible.value,color=type),size = 1, alpha =0.4)+ 
    scale_fill_grey() +
    theme_minimal()+
    theme(axis.text=element_text(size=20,face="bold"),
          axis.title=element_text(size=20,face="bold"),
          legend.text =element_text(size=20),
          legend.title =element_text(size=20),
          legend.key.size = unit(1, "cm"))+
labs(x ="Estimated Values", y = "Density")

library(ggpubr)

ggarrange(P1, P2,ncol=2, nrow=1, common.legend = TRUE, legend="bottom")




