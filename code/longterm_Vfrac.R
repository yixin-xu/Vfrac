library(tidyverse)
library(readxl)
library(dplyr)
library(survival)


# read excel file (PSA_results is sarash's inc Q and inc C)
setwd('C:/Users/yx18392/OneDrive - University of Bristol/Desktop/Vfrac/')
shortterm_raw = read.csv('shortterm table.csv')
PSA_results = read_excel('PSA inc C&Q.xlsx')
shortterm=shortterm_raw%>% drop_na(36)

# set sample size and random data frame
n_samples = 1000
random_QALYs = data.frame(matrix(NA, nrow = length(shortterm$xvfracid), ncol = n_samples))
random_costs = data.frame(matrix(NA, nrow = length(shortterm$xvfracid), ncol = n_samples))

# set bootstrap
bootstrap_samples <- matrix(NA, nrow = length(shortterm$xvfracid), ncol = n_samples)
for(i in 1:dim(bootstrap_samples)[1]) {
  bootstrap_samples[i, ] <- sample(1:length(shortterm$xvfracid), size = n_samples)
}


# If vfyncode and XVfracscreen are both yes, long-term costs and QALYs are short-term costs and QALYs add PSA results, else long-term costs and QALYs are short-term costs and QALYs
# xvfracscreen: does vfrac recommend xray
# If vfrac=1 and fracture=0
# Use short term follow-up only
# If vfrac=1 and fracture = 1
# Use short term follow-up and PSA 
# If vfrac=0 
# Use baseline

### QALYs
for(i in 1:nrow(shortterm)){
  
  if( shortterm$xvfracscreen[i] == T ) {
    if (shortterm$vfyncode[i] == T){
      random_QALYs[i,1:n_samples] = 
      shortterm$feq5d_score[bootstrap_samples[i, ]]*0.25 + PSA_results[bootstrap_samples[i, ],2]
    } else { random_QALYs[i,1:n_samples] = shortterm$feq5d_score[bootstrap_samples[i, ]]*0.25}
  }
    else {random_QALYs[i,1:n_samples] = shortterm$beq5d_score[bootstrap_samples[i, ]]*0.25}
}
    

### costs
for(i in 1:nrow(shortterm)){
  
  if( shortterm$xvfracscreen[i] == T ) {
    if (shortterm$vfyncode[i] == T){
      random_costs[i,1:n_samples] = 
        shortterm$fcosts[bootstrap_samples[i, ]] + PSA_results[bootstrap_samples[i, ],3]
    } else { random_costs[i,1:n_samples] = shortterm$fcosts[bootstrap_samples[i, ]]}
  }
  else {random_costs[i,1:n_samples] = shortterm$bcosts[bootstrap_samples[i, ]]}
}
  


# calculate the mean of costs and QALYs for each column
qalys_mean = data.frame(colMeans(random_QALYs, na.rm = TRUE))
costs_mean = data.frame(colMeans(random_costs, na.rm = TRUE))

#calculate the total mean
long_term_qalys_mean = colMeans(qalys_mean)
long_term_costs_mean = colMeans(costs_mean)

# Calculate 95% confidence intervals for QALYs
quantile(qalys_mean$colMeans.random_QALYs..na.rm...TRUE.,.025)
quantile(qalys_mean$colMeans.random_QALYs..na.rm...TRUE.,.975)

# Calculate 95% confidence intervals for QALYs
quantile(costs_mean$colMeans.random_costs..na.rm...TRUE.,.025)
quantile(costs_mean$colMeans.random_costs..na.rm...TRUE.,.975)


###### NET BENEFIT 20000*IQ-IC
nb_vfrac = 2000*qalys_mean - costs_mean
nb_vfrac_table = data.frame(nb_vfrac)
mean_nb_vfrac = mean(nb_vfrac_table$colMeans.random_QALYs..na.rm...TRUE.)

nb_standad_of_care_table = read.csv('nb_standad_of_care.csv')
mean_nb_standad_of_care = mean(nb_standad_of_care_table$Total_samples_QALYs_mean...2000...Total_samples_costs_mean)
nb_standad_of_care = unlist(nb_standad_of_care_table[,2])


####### EVPI
EVPI = max(max(nb_vfrac), max(nb_standad_of_care))-max(mean_nb_vfrac, mean_nb_standad_of_care)

write.csv(boot_qalys_mean, 'qalys_mean.csv')
write.csv(boot_costs_mean, 'costs_mean.csv')
write.csv(nb_vfrac_table, 'nb_vfrac.csv')



