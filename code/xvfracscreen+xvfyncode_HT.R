library(tidyverse)
library(readxl)
library(dplyr)
library(boot)

# read excel file (PSA_results is sarash's inc Q and inc C)
setwd('C:/Users/yx18392/OneDrive - University of Bristol/Desktop/Vfrac/')
shortterm = read.csv('shortterm table.csv')
PSA_results = read_excel('PSA inc C&Q.xlsx')

# vfyncode: Does this person have an osteoporotic vertebral fracture
# change "Yes" and "No" to 1 and 0
shortterm$vfyncode[shortterm$vfyncode == "Yes"] = 1
shortterm$vfyncode[shortterm$vfyncode == "No"] = 0

# If vfyncode and XVfracscreen are both yes, long-term costs and QALYs are short-term costs and QALYs add PSA results, else long-term costs and QALYs are short-term costs and QALYs
# xvfracscreen: does vfrac recommend xray

for(i in 1:nrow(shortterm)){
  # If vfrac=1 and fracture=0
  # Use short term follow-up only
  # If vfrac=1 and fracture = 1
  # Use short term follow-up and PSA 
  # If vfrac=0 
  # Use baseline
    
  if(shortterm$vfyncode[i] == 1 & shortterm$xvfracscreen[i] == 1){
      shortterm$long_term_qalys[i] = shortterm$feq5d_score[i]*0.25 + PSA_results[i,2]} else {
        # Use baseline QALYS
        shortterm$long_term_qalys[i] = shortterm$beq5d_score[i]*0.25
        }
    
  if(shortterm$vfyncode[i] == 1 & shortterm$xvfracscreen[i] == 1){
    shortterm$long_term_costs[i] = shortterm$fcosts[i] + PSA_results[i,3]} else {
      # Use baseline costs
      shortterm$long_term_costs[i] = shortterm$bcosts[i]
    }
  }

# seperate the long-term costs and QALYs results as new data frame
long_term_qalys = data.frame(shortterm$long_term_qalys)
long_term_costs = data.frame(shortterm$long_term_costs)

## I AM NOT SURE ON THIS BOOTSTRAPPING SECTION
# Define a function
a = function(data, indices){
  d = data[indices,]
  fit = (data = d)
  return(data)
}

# bootstrapping with 1000 replications 
boot_qalys = boot(data = long_term_qalys, statistic = a, R = 1000)
boot_costs = boot(data = long_term_costs, statistic = a, R = 1000)

# set 1000 replications data as two new table 
boot_qalys2 = data.frame(boot_qalys[2])
boot_costs2 = data.frame(boot_costs[2])

# calculate the mean of costs and QALYs for each column
boot_qalys_mean = rowMeans(boot_qalys2, na.rm = TRUE)
boot_costs_mean = rowMeans(boot_costs2, na.rm = TRUE)

#calculate the total mean
long_term_qalys_mean = mean(boot_qalys_mean)
long_term_costs_mean = mean(boot_costs_mean)

# Calculate 95% confidence intervals for QALYs
quantile(boot_qalys_mean,.025)
quantile(boot_qalys_mean,.975)

# Calculate 95% confidence intervals for QALYs
quantile(boot_costs_mean,.025)
quantile(boot_costs_mean,.975)

boot_qalys_mean = data.frame(boot_qalys_mean)
boot_costs_mean = data.frame(boot_costs_mean)

write.csv(boot_qalys_mean, 'boot_qalys_mean.csv')
write.csv(boot_costs_mean, 'boot_costs_mean.csv')
write.csv(boot_qalys2, 'qalys1000after_bootstrapping.csv')
write.csv(boot_costs2, 'costs1000after_bootstrapping.csv')
