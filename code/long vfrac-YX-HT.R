library(tidyverse)
library(readxl)
library(dplyr)
library(survival)


patient_infor_raw = read.csv('results/qalys and costs.csv')
PSA_results = as.data.frame(read_excel('data/PSA mean costs and qalys.xlsx'))
#shortterm = read.csv('results/shortterm_vfrac.csv')
patient_infor = patient_infor_raw%>% drop_na(36)



# set sample size and random data frame
n_samples = 1000
random_QALYs = data.frame(matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples))
random_costs = data.frame(matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples))

# set bootstrap
bootstrap_samples <- matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples)
for(i in 1:dim(bootstrap_samples)[1]) {
  bootstrap_samples[i, ] <- sample(1:length(patient_infor$xvfracid), size = n_samples)
}


# If vfyncode and XVfracscreen are both yes, long-term costs and QALYs are short-term costs and QALYs add PSA results, else long-term costs and QALYs are short-term costs and QALYs
# xvfracscreen: does vfrac recommend xray
# If vfrac=1 and fracture=0
# Use short term follow-up 
# If vfrac=1 and fracture = 1
# Use short term follow-up and PSA alendronate data
# If vfrac=0 and fracture=0
# Use baseline
# If vfrac=0 and fracture=1
# Use baseline and add PSA no treatment data

### QALYs
for(i in 1:nrow(patient_infor)){
  print(paste("Calculating for patient",i, "/",nrow(patient_infor)))
  
  if( patient_infor$xvfracscreen[i] == T ) {
    # Referred for screening by vfrac
    if (patient_infor$vfyncode[i] == "Yes"){
      # Have a fracture then get treatment
      random_QALYs[i,1:n_samples] = 
        patient_infor$feq5d_score[bootstrap_samples[i, ]]*0.25 + PSA_results[bootstrap_samples[i, ],5]
    } else { 
      # No fracture so no additional costs
      random_QALYs[i,1:n_samples] = 
        patient_infor$feq5d_score[bootstrap_samples[i, ]]*0.25 
      }
  } else {
    # Not referred for screening by vfrac
    if (patient_infor$vfyncode[i] == T) {
      # Have a fracture but don't get treatment so add no treatment QALYs
      random_QALYs[i,1:n_samples] = 
        patient_infor$beq5d_score[bootstrap_samples[i, ]]*0.25 + PSA_results[bootstrap_samples[i, ],3]
    } else {
      # No fracture so no additional costs
      random_QALYs[i,1:n_samples] = patient_infor$beq5d_score[bootstrap_samples[i, ]]*0.25
    }
  }
}

### costs
for(i in 1:nrow(patient_infor)){
  print(paste("Calculating for patient",i, "/",nrow(patient_infor)))
  
  if( patient_infor$xvfracscreen[i] == T ) {
    # Referred for screening by vfrac
    if (patient_infor$vfyncode[i] == "Yes"){
      # Has a fracture and diagnosed so treatment cost
      random_costs[i,1:n_samples] = 
        patient_infor$fcosts[bootstrap_samples[i, ]] + PSA_results[bootstrap_samples[i, ],4]
    } else { 
      # No fracture so no additional cost
      random_costs[i,1:n_samples] = 
        patient_infor$fcosts[bootstrap_samples[i, ]]
      }
  } else {
    if (patient_infor$vfyncode[i] == T){
      # Has a fracture but not diagnosed so add no treatment costs
      random_costs[i,1:n_samples] = 
        patient_infor$bcosts[bootstrap_samples[i, ]] + PSA_results[bootstrap_samples[i, ],2] 
    } else {
      # No fracture so no additional cost
      random_costs[i,1:n_samples] = patient_infor$bcosts[bootstrap_samples[i, ]]  
    }
  }
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
nb_vfrac = 2000*random_QALYs - random_costs
mean_nb_vfrac = data.frame(colMeans(nb_vfrac,na.rm = TRUE))
mean = colMeans(mean_nb_vfrac)
quantile(mean_nb_vfrac$colMeans.nb_vfrac..na.rm...TRUE.,.025)
quantile(mean_nb_vfrac$colMeans.nb_vfrac..na.rm...TRUE.,.975)

write.csv(nb_vfrac, 'results/nb_vfrac.csv')
write.csv(costs_mean, 'results/longterm_vfrac_costs.csv')
write.csv(qalys_mean, 'results/longterm_vfrac_qalys.csv')
