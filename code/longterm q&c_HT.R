library(tidyverse)
library(readxl)
library(dplyr)

# Have separate data, code, and results directories when you set up a GitHub repository.


# read excel file (T5 is sarash's inc Q and inc C)
setwd('C:/Users/yx18392/OneDrive - University of Bristol/Desktop/Vfrac/')
shortterm_raw = read.csv('shortterm table.csv')
PSA_results = read_excel('PSA inc C&Q.xlsx')
# Drop rows without mrbackpainbas variable
shotterm=shortterm_raw%>% drop_na(69) # Change shortterm to shortterm_raw then T4 is shortterm

# Filter to only patients who have a consultation for back pain during 
# the 3 months before enrolment in the study
# mrbackpainbas = Did the individual have a consultation for back pain during the 3 months before enrolment in this study? 
# These will be randomly sampled.
gp_xvfracid = shotterm$xvfracid[shotterm$mrbackpainbas == 1]

# change "Yes" and "No" to 1 and 0
shotterm$vfyncode[shotterm$vfyncode == "Yes"] = 1 
shotterm$vfyncode[shotterm$vfyncode == "No"] = 0

# Patient IDs
# The final dataframe will have one row for each patient and each column
# corresponds to each of 1000 sampled values.
random_QALYs = data.frame(gp_xvfracid)
random_costs = data.frame(gp_xvfracid)

# Yixin to change so that number of samples (currently 1000) can vary 
n_samples = 1000

# Use "guesstimate" based on stakeholder work for proportion referred 
# Used a normal distribution ranging from 0.10 to 0.30 to represent uncertainty

prop_referred <- rnorm(n_samples, 0.20, sd = 0.05)
# Cut off below 0 and above 1
prop_referred[prop_referred < 0] <- 0
prop_referred[prop_referred > 1] <- 1

# Generate 1000 random samples assuming prop_referred are referred for x-ray
for(i in 1:n_samples){
  
  # Random sample referred for x-ray
  randomsample = sample(gp_xvfracid, size = length(gp_xvfracid) * prop_referred[i], replace = F)
  for(a in gp_xvfracid){
    # If referred use follow-up EQ5D and cost and add random inc Q and inc C
    if(a %in% randomsample& shotterm$vfyncode == 1){
      random_QALYs[match(a,gp_xvfracid),i+1] = shotterm$feq5d_score[shotterm$xvfracid == a]*0.25 + PSA_results[i,"inc Q"]} 
    
    if(a %in% randomsample& shotterm$vfyncode == 1){
      random_costs[match(a,gp_xvfracid),i+1] = shotterm$fcosts[shotterm$xvfracid == a] + PSA_results[i, "inc C"]} 
  }
}


# Add the IDs to the EQ5D data
random_QALYs$xvfracid = random_QALYs$gp_xvfracid

leftjoin_qalys = left_join(shotterm, random_QALYs, by ='xvfracid')

# Combine IDs with sampled eq5d
Total_samples_QALYs = data.frame(leftjoin_qalys[, 2], leftjoin_qalys[, 75:1074])

# Create a matrix of indices to the patients who did not have a GP consultation in previous 3 months
# Each row corresponds to a patient and each column is a randomly sampled other patient for use in each bootstrap sample
bootstrap_samples <- matrix(NA, nrow = length(Total_samples_QALYs$leftjoin_qalys...2.), ncol = n_samples)
for(i in 1:dim(bootstrap_samples)[1]) {
  bootstrap_samples[i, ] <- sample(1:sum(!is.element(shotterm$xvfracid, random_QALYs$gp_xvfracid)), size = n_samples)
}


# Loop through the patients
for(i in Total_samples_QALYs$leftjoin_qalys...2.){
  # If i was one of the patients with a GP referal
  if(i %in% random_QALYs$gp_xvfracid == FALSE){
    # the first loop only random the situation when mrbackpainbas=1, so here just add the eq5d_score for patient with mrbackpainbas=0
    # replace NA in Total_samples_QALYs with baseline QALYs
    # Choosing the bootstrap sample corresponding to patient i
    Total_samples_QALYs[match(i,Total_samples_QALYs$leftjoin_qalys...2.),2:n_samples+1] = 
      shotterm$beq5d_score[bootstrap_samples[which(Total_samples_QALYs$leftjoin_qalys...2.==i), ]]*0.25}
  
  
  # What you'll want is to sample the baseline QALYs randomly
  # If you used the code below it would use different random samples for costs and QALYs
  # sample(T4$beq5d_score[!is.element(T4$xvfracid, random_QALYs$gp_xvfracid)] * 0.25, size = 1000, replace = TRUE)
  
}

# change NA data to baseline QALYs
nalist = c(which(rowSums(is.na(Total_samples_QALYs)) > 0))
for(a in nalist){
  Total_samples_QALYs[a,is.na(Total_samples_QALYs[a,])] =  shotterm$beq5d_score[a]*0.25
}

# Add the IDs to the costs data
random_costs$xvfracid = random_costs$gp_xvfracid
leftjoin_costs = left_join(shotterm, random_costs, by ='xvfracid')
# Combine IDs with sampled costs
Total_samples_costs = data.frame(leftjoin_costs[, 2], leftjoin_costs[, 75:1074])

# Loop through the patients
for(i in Total_samples_costs$leftjoin_costs...2.){
  # If i was one of the patients with a GP referal
  if(i %in% random_costs$gp_xvfracid == FALSE){
    # replace NA in Total_samples_costs with bcosts
    # Choosing the bootstrap sample corresponding to patient i
    Total_samples_costs[match(i,Total_samples_costs$leftjoin_costs...2.),2:n_samples+1] = 
      shotterm$bcosts[bootstrap_samples[which(Total_samples_costs$leftjoin_costs...2.==i), ]]}
  
  
  
  # Randomly sample 1000 costs for these patients
}

# change NA data to baseline costs
nalist = c(which(rowSums(is.na(Total_samples_costs)) > 0))
for(a in nalist){
  Total_samples_costs[a,is.na(Total_samples_costs[a,])] =  shotterm$bcosts[a]
}

# calculate the mean QALYs of each dataset and add into Total_samples_eq5d as a new row
Total_samples_QALYs_mean = colMeans(Total_samples_QALYs, na.rm = TRUE)
Total_samples_QALYs = rbind(Total_samples_QALYs, Total_samples_QALYs_mean)

# calculate the mean costs of each dataset and add into Total_samples_eq5d as a new row
Total_samples_costs_mean = colMeans(Total_samples_costs, na.rm = TRUE)
Total_samples_costs = rbind(Total_samples_costs, Total_samples_costs_mean)


# List mean QALYs and costs separately
Total_samples_QALYs2 = Total_samples_QALYs[1418,2:n_samples+1]
Total_samples_costs2 = Total_samples_costs[1418,2:n_samples+1]

# Calculate the final mean of QALYs and costs
Mean_QALYs = rowMeans(Total_samples_QALYs2)
Mean_costs = rowMeans(Total_samples_costs2)

# Calculate 95% confidence intervals for QALYs
quantile(Total_samples_QALYs2,.025, na.rm = TRUE)
quantile(Total_samples_QALYs2,.975, na.rm = TRUE)

# Calculate 95% confidence intervals for costs
quantile(Total_samples_costs2,.025)
quantile(Total_samples_costs2,.975)

# Calculate the mean of 1000 mean QALYs and costs
Total_samples_QALYs2$mean = rowMeans(Total_samples_QALYs2)
Total_samples_costs2$mean = rowMeans(Total_samples_costs2)


write.csv(Total_samples_QALYs, 'Total_samples_QALYs.csv')
write.csv(Total_samples_costs, 'Total_samples_costs.csv')
write.csv(Total_samples_QALYs2, 'Total_samples_QALYs2.csv')
write.csv(Total_samples_costs2, 'Total_samples_costs2.csv') 