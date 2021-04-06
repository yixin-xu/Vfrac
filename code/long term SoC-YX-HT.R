### Estimate the costs, QALYs and net benefit of the long term standard of care. 
### Step 3, after running the "qalys and costs", "shortterm standard" and "shortterm vfrac" code

library(tidyverse)
library(readxl)
library(dplyr)

patient_infor_raw = read.csv('results/qalys and costs.csv')
PSA_results = as.data.frame(read_excel('data/PSA mean costs and qalys.xlsx'))
patient_infor=patient_infor_raw%>% drop_na(69) 


# set sample size and random data frame
# The final dataframe will have one row for each patient and each column
n_samples = 1000
random_QALYs = data.frame(matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples))
random_costs = data.frame(matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples))
random_QALYs = cbind(patient_infor$xvfracid, random_QALYs)
random_costs = cbind(patient_infor$xvfracid, random_costs)
# Give rows meaningful names
rownames(random_QALYs) <- rownames(random_costs) <- patient_infor$xvfracid


# Filter to only patients who have a consultation for back pain during 
# mrbackpainbas = Did the individual have a consultation for back pain during the 3 months before enrolment in this study? 
# These will be randomly sampled.
gp_xvfracid = patient_infor$xvfracid[patient_infor$mrbackpainbas == 1]
# And identify those without GP consultation
notgp_xvfracid <- patient_infor$xvfracid[patient_infor$mrbackpainbas == 0]


# Use "guesstimate" based on stakeholder work for proportion referred 
# Used a normal distribution ranging from 0.10 to 0.30 to represent uncertainty
prop_referred <- rnorm(n_samples, 0.20, sd = 0.05)
# Cut off below 0 and above 1
prop_referred[prop_referred < 0] <- 0
prop_referred[prop_referred > 1] <- 1


# set bootstrap for short term - these are resamples of patient IDs
bootstrap_samples <- matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples)
for(i in 1:dim(bootstrap_samples)[1]) {
  bootstrap_samples[i, ] <- sample(1:length(patient_infor$xvfracid), size = n_samples)
}
rownames(bootstrap_samples) <- patient_infor$xvfracid

# set bootstrap for long term - these are resamples of PSA samples
bootstrap_samples_longterm <- matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples)
for(i in 1:dim(bootstrap_samples_longterm)[1]) {
  bootstrap_samples_longterm[i, ] <- sample(1:n_samples, size = n_samples)
}
rownames(bootstrap_samples_longterm) <- patient_infor$xvfracid

# If vfyncode and mrbackpains are both yes, long-term costs and QALYs are short-term costs and QALYs add PSA results, else long-term costs and QALYs are short-term costs and QALYs
# Mrbackpains: whether have an gp consultation
# Ramdom select some patient had an x-ray after gp consultaion 
# If vfrac=1 and did not be selected to have an x-ray
# Use short term follow-up 
# If vfrac=1 and selected to have an x-ray
# Use short term follow-up and PSA alendronate data
# If vfrac=0 and did not be selected to have an x-ray
# Use baseline
# If vfrac=0 and selected to have an x-ray
# Use baseline and add PSA no treatment data

### QALYs and costs for patients had gp consultation

for(i in 1:n_samples){
  print(i)
  # Random sample referred for x-ray
  randomsample = sample(gp_xvfracid, size = length(gp_xvfracid) * prop_referred[i], replace = F)
  for(a in gp_xvfracid){
    # QALYs
    if(a %in% randomsample){
      # Had an x-ray
      if(patient_infor$vfyncode[patient_infor$xvfracid == a] == "Yes"){
        # Have a fracture then get treatment
        random_QALYs[toString(a), i+1] = patient_infor$feq5d_score[patient_infor$xvfracid == a]*0.25 + PSA_results[i,5]} 
      else{
        # No fracture so no additional qalys
        random_QALYs[toString(a), i+1] = patient_infor$feq5d_score[patient_infor$xvfracid == a]*0.25}
    }
    else if(patient_infor$vfyncode[patient_infor$xvfracid == a] == "Yes"){
      # Have a fracture but don't get treatment so add no treatment QALYs
      random_QALYs[toString(a),i+1] = patient_infor$beq5d_score[patient_infor$xvfracid == a]*0.25 + PSA_results[i,3]
    } else{
      # No fracture so no additional qalys
      random_QALYs[toString(a),i+1] = patient_infor$beq5d_score[patient_infor$xvfracid == a]*0.25}

   # Costs
   if(a %in% randomsample){
     # Had an x-ray
    if(patient_infor$vfyncode[patient_infor$xvfracid == a] == "Yes"){
      # Have a fracture then get treatment
      random_costs[toString(a),i+1] = patient_infor$fcosts[patient_infor$xvfracid == a]+ PSA_results[i,4]} 
    else{
      # No fracture so no additional costs
      random_costs[toString(a),i+1] = patient_infor$fcosts[patient_infor$xvfracid == a]}
  }
  else if(patient_infor$vfyncode[patient_infor$xvfracid == a] == "Yes"){
    # Have a fracture but don't get treatment so add no treatment costs
    random_costs[toString(a),i+1] = patient_infor$bcosts[patient_infor$xvfracid == a] + PSA_results[i,2]
  } else{
    # No fracture so no additional costs
    random_costs[toString(a),i+1] = patient_infor$bcosts[patient_infor$xvfracid == a]}
}
}

# QALYs and costs for patients who did not have gp consultation

for(i in 1:nrow(patient_infor)){
  print(paste("Calculating for patient",i, "/",nrow(patient_infor)))
  
  # Patients who did not have consultation
  if(patient_infor$mrbackpainbas[i] == 0 ){
    # Do not have an x-ray
    if (patient_infor$vfyncode[i] == "Yes") {
      # Have a fracture but don't get treatment so add no treatment QALYs and costs
      random_QALYs[i,2:(n_samples+1)] = 
        patient_infor$beq5d_score[bootstrap_samples[i, ]]*0.25 + PSA_results[bootstrap_samples_longterm[i, ],3]
      random_costs[i,2:(n_samples+1)] = 
        patient_infor$bcosts[bootstrap_samples[i, ]] + PSA_results[bootstrap_samples_longterm[i, ],2]
      
    } else {
      # No fracture so no additional QALYs or costs
      random_QALYs[i,2:(n_samples+1)] = patient_infor$beq5d_score[bootstrap_samples[i, ]]*0.25
      random_costs[i,2:(n_samples+1)] = patient_infor$bcosts[bootstrap_samples[i, ]]
    }
  }
}

# calculate the mean QALYs and costs of each dataset 
random_QALYs_mean = colMeans(random_QALYs[,2:1001], na.rm = TRUE)
random_costs_mean = colMeans(random_costs[,2:1001], na.rm = TRUE)


# Calculate the final mean of QALYs and costs
Mean_QALYs = mean(random_QALYs_mean)
Mean_costs = mean(random_costs_mean)

# Calculate 95% confidence intervals for QALYs
quantile(random_QALYs_mean,.025, na.rm = TRUE)
quantile(random_QALYs_mean,.975, na.rm = TRUE)

# Calculate 95% confidence intervals for costs
quantile(random_costs_mean,.025)
quantile(random_costs_mean,.975)

### NET BENEFIT
nb_standard_of_care_raw = data.frame(random_QALYs*2000 - random_costs)
nb_standard_of_care = nb_standard_of_care_raw[,2:1001]
nb_mean_col = colMeans(nb_standard_of_care, na.rm = TRUE)
mean(nb_mean_col)
quantile(nb_mean_col,.025)
quantile(nb_mean_col,.975)

write.csv(nb_standard_of_care, 'results/nb_standard_of_care.csv')
write.csv(random_QALYs_mean, 'results/longterm_standard_qalys.csv')
write.csv(random_costs_mean, 'results/longterm_standard_costs.csv')
