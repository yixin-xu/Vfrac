library(tidyverse)
library(readxl)
library(dplyr)

patient_infor_raw = read.csv('results/qalys and costs.csv')
PSA_results = read_excel('data/PSA mean costs and qalys.xlsx')
# Drop rows without mrbackpainbas variable
patient_infor=patient_infor_raw%>% drop_na(69) 

# Filter to only patients who have a consultation for back pain during 
# the 3 months before enrolment in the study
# mrbackpainbas = Did the individual have a consultation for back pain during the 3 months before enrolment in this study? 
# These will be randomly sampled.
gp_xvfracid = patient_infor$xvfracid[patient_infor$mrbackpainbas == 1]

# vfyncode = Does this person have an osteoporotic vertebral fracture
# change "Yes" and "No" to 1 and 0
patient_infor$vfyncode[patient_infor$vfyncode == "Yes"] = 1 # Had fracture
patient_infor$vfyncode[patient_infor$vfyncode == "No"] = 0 # Did not have fracture


n_samples = 1000

# Patient IDs
# The final dataframe will have one row for each patient and each column
# corresponds to each of 1000 sampled values.
random_QALYs = data.frame(gp_xvfracid)
random_costs = data.frame(gp_xvfracid)
# HT: Yixin, I pre-build these data frames to have n_samples columns. It makes the loops
# go faster (because they aren't assigning new CPU memory) and is more robust.
random_QALYs <- cbind(random_QALYs, matrix(nrow = length(gp_xvfracid), ncol = n_samples))
random_costs <- cbind(random_costs, matrix(nrow = length(gp_xvfracid), ncol = n_samples))



# Use "guesstimate" based on stakeholder work for proportion referred 
# Used a normal distribution ranging from 0.10 to 0.30 to represent uncertainty

prop_referred <- rnorm(n_samples, 0.20, sd = 0.05)
# Cut off below 0 and above 1
prop_referred[prop_referred < 0] <- 0
prop_referred[prop_referred > 1] <- 1

# Generate 1000 random samples assuming prop_referred are referred for x-ray
for(i in 1:n_samples){
  
  # Random sample referred for x-ray
  # Have to use those referred as otherwise follow-up costs
  randomsample = sample(gp_xvfracid, size = length(gp_xvfracid) * prop_referred[i], replace = F)
  for(a in gp_xvfracid){
    # If referred and had fracture use follow-up EQ5D and cost and add random inc Q and inc C
    # HT: I corrected the condition below so that it correctly checks if patient a had a fracture
    # YX: I add the situation that patient had an x-ray but not be diagnosed and change the incQ and incC to the mean costs and qalys
    if(a %in% randomsample & patient_infor$vfyncode[patient_infor$xvfracid == a] == 1){
      random_QALYs[match(a,gp_xvfracid),i+1] = patient_infor$feq5d_score[patient_infor$xvfracid == a]*0.25 + PSA_results[i,5]} 
    else if(a %in% randomsample & patient_infor$vfyncode[patient_infor$xvfracid == a] == 0){
      random_QALYs[match(a,gp_xvfracid),i+1] = patient_infor$feq5d_score[patient_infor$xvfracid == a]*0.25 + PSA_results[i,3]} 
    
    if(a %in% randomsample & patient_infor$vfyncode[patient_infor$xvfracid == a] == 1){
      random_costs[match(a,gp_xvfracid),i+1] = patient_infor$fcosts[patient_infor$xvfracid == a] + PSA_results[i, 4]} 
    else if(a %in% randomsample & patient_infor$vfyncode[patient_infor$xvfracid == a] == 0){
      random_costs[match(a,gp_xvfracid),i+1] = patient_infor$fcosts[patient_infor$xvfracid == a] + PSA_results[i, 2]} 
  }
}


# Add the IDs to the EQ5D data
random_QALYs$xvfracid = random_QALYs$gp_xvfracid

leftjoin_qalys = left_join(patient_infor, random_QALYs, by ='xvfracid')

# Combine IDs with sampled eq5d
# HT: Yixin, I changed the below to work with any number of n_samples
Total_samples_QALYs = data.frame(leftjoin_qalys[, 2], leftjoin_qalys[, 75:(74+n_samples)])

# Create a matrix of indices to the patients who did not have a GP consultation in previous 3 months
# Each row corresponds to a patient and each column is a randomly sampled other patient for use in each bootstrap sample
bootstrap_samples <- matrix(NA, nrow = length(Total_samples_QALYs$leftjoin_qalys...2.), ncol = n_samples)
for(i in 1:dim(bootstrap_samples)[1]) {
  bootstrap_samples[i, ] <- sample(1:sum(!is.element(patient_infor$xvfracid, random_QALYs$gp_xvfracid)), size = n_samples)
}


# Loop through the patients
for(i in Total_samples_QALYs$leftjoin_qalys...2.){
  # If i was one of the patients with a GP referal
  if(i %in% random_QALYs$gp_xvfracid == FALSE){
    # the first loop only random the situation when mrbackpainbas=1, so here just add the eq5d_score for patient with mrbackpainbas=0
    # replace NA in Total_samples_QALYs with baseline QALYs
    # Choosing the bootstrap sample corresponding to patient i
    # HT: Yixin, I changed the column range from 2:n_samples+1 to 2:(n_samples+1) as otherwise undercounts by 1
    Total_samples_QALYs[match(i,Total_samples_QALYs$leftjoin_qalys...2.),2:(n_samples+1)] = 
      patient_infor$beq5d_score[bootstrap_samples[which(Total_samples_QALYs$leftjoin_qalys...2.==i), ]]*0.25}
  
  
  # What you'll want is to sample the baseline QALYs randomly
  # If you used the code below it would use different random samples for costs and QALYs
  # sample(T4$beq5d_score[!is.element(T4$xvfracid, random_QALYs$gp_xvfracid)] * 0.25, size = 1000, replace = TRUE)
  
}
# YX: add bootstrap here 
# HT: Yixin, the line below needs to include a bootstrap as well.
# At the moment you're using the observed QALY for patient a but you need to instead randomly
# sample (with replacement these QALYs. Use the bootstrap_samples[a, ] so use:
#  Total_samples_QALYs[a,is.na(Total_samples_QALYs[a,])] =   patient_infor$beq5d_score[bootstrap_samples[a,is.na(Total_samples_QALYs[a, -1])]]
# And make this change in the costs as well
# change NA data to baseline QALYs
nalist = c(which(rowSums(is.na(Total_samples_QALYs)) > 0))
for(a in nalist){
  Total_samples_QALYs[a,is.na(Total_samples_QALYs[a,])] =   patient_infor$beq5d_score[bootstrap_samples[a,is.na(Total_samples_QALYs[a, -1])]]
}

# Add the IDs to the costs data
random_costs$xvfracid = random_costs$gp_xvfracid
leftjoin_costs = left_join(patient_infor, random_costs, by ='xvfracid')
# Combine IDs with sampled costs
Total_samples_costs = data.frame(leftjoin_costs[, 2], leftjoin_costs[, 75:1074])

# Loop through the patients
for(i in Total_samples_costs$leftjoin_costs...2.){
  # If i was one of the patients with a GP referal
  if(i %in% random_costs$gp_xvfracid == FALSE){
    # replace NA in Total_samples_costs with bcosts
    # Choosing the bootstrap sample corresponding to patient i
    Total_samples_costs[match(i,Total_samples_costs$leftjoin_costs...2.),2:n_samples+1] = 
      patient_infor$bcosts[bootstrap_samples[which(Total_samples_costs$leftjoin_costs...2.==i), ]]}
  
  
  
  # Randomly sample 1000 costs for these patients
}
# YX: add bootstrap here
# change NA data to baseline costs
nalist = c(which(rowSums(is.na(Total_samples_costs)) > 0))
for(a in nalist){
  Total_samples_costs[a,is.na(Total_samples_costs[a,])] =   patient_infor$bcosts[bootstrap_samples[a,is.na(Total_samples_costs[a, -1])]]
}

# calculate the mean QALYs of each dataset and add into Total_samples_eq5d as a new row
Total_samples_QALYs_mean = colMeans(Total_samples_QALYs, na.rm = TRUE)

# calculate the mean costs of each dataset and add into Total_samples_eq5d as a new row
Total_samples_costs_mean = colMeans(Total_samples_costs, na.rm = TRUE)


# Calculate the final mean of QALYs and costs
Mean_QALYs = mean(Total_samples_QALYs_mean)
Mean_costs = mean(Total_samples_costs_mean)

# Calculate 95% confidence intervals for QALYs
quantile(Total_samples_QALYs_mean,.025, na.rm = TRUE)
quantile(Total_samples_QALYs_mean,.975, na.rm = TRUE)

# Calculate 95% confidence intervals for costs
quantile(Total_samples_costs_mean,.025)
quantile(Total_samples_costs_mean,.975)

### NET BENEFIT
nb_standard_of_care_raw = data.frame(Total_samples_QALYs*2000 - Total_samples_costs)
nb_standard_of_care = nb_standard_of_care_raw[,2:1001]


write.csv(nb_standad_of_care, 'results/nb_standad_of_care.csv')
write.csv(Total_samples_QALYs_mean, 'results/longterm_standard_qalys.csv')
write.csv(Total_samples_costs_mean, 'results/longterm_standard_costs.csv')
