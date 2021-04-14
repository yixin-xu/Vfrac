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


gp_notdiagnosed = patient_infor[patient_infor$mrbackpainbas =="1"&patient_infor$vfyncode =="No",]
notgp_notdiagnosed = patient_infor[patient_infor$mrbackpainbas =="0"&patient_infor$vfyncode =="No",]
gp_diagnosed = patient_infor[patient_infor$mrbackpainbas =="1"&patient_infor$vfyncode =="Yes",]
notgp_diagnosed = patient_infor[patient_infor$mrbackpainbas =="0"&patient_infor$vfyncode =="Yes",]

# set bootstrap for different types of patients
# All have the same dimensions and same rownames
# Should have row for every patient and elements should be index of each specified type of patient
bootstrap_samples_gp_notdiagnosed <- bootstrap_samples_notgp_notdiagnosed <-
  bootstrap_samples_gp_diagnosed <- bootstrap_samples_notgp_diagnosed <-
    matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples)
for(i in 1:dim(bootstrap_samples_gp_notdiagnosed)[1]) {
  bootstrap_samples_gp_notdiagnosed[i, ] <- sample(which(is.element(patient_infor$xvfracid,gp_notdiagnosed$xvfracid)), size = n_samples, replace = TRUE)
  bootstrap_samples_notgp_notdiagnosed[i, ] <- sample(which(is.element(patient_infor$xvfracid,notgp_notdiagnosed$xvfracid)), size = n_samples, replace = TRUE)
  bootstrap_samples_gp_diagnosed[i, ] <- sample(which(is.element(patient_infor$xvfracid,gp_diagnosed$xvfracid)), size = n_samples, replace = TRUE)
  bootstrap_samples_notgp_diagnosed[i, ] <- sample(which(is.element(patient_infor$xvfracid,notgp_diagnosed$xvfracid)), size = n_samples, replace = TRUE)
}
rownames(bootstrap_samples_gp_notdiagnosed) <- rownames(bootstrap_samples_notgp_notdiagnosed) <-
  rownames(bootstrap_samples_gp_diagnosed) <- rownames(bootstrap_samples_notgp_diagnosed) <- 
  patient_infor$xvfracid

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

for(i in 1:1000){
  print(i)
  # Random sample referred for x-ray
  randomsample = sample(gp_xvfracid, size = length(gp_xvfracid) * prop_referred[i], replace = F)
  # Patients not referred for x-ray
  randomsample_notreferred <- gp_xvfracid[!is.element(gp_xvfracid, randomsample)]

  # Referred and have a fracture
  random_QALYs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(randomsample, toString))), i+1] <-
        patient_infor$feq5d_score[bootstrap_samples_gp_diagnosed[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],randomsample)), i]]*0.25  +
    PSA_results[bootstrap_samples_longterm[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(randomsample, toString))) ,i],5] 
  
  # Not referred and have a fracture
   random_QALYs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(randomsample_notreferred, toString))), i+1] <-
        patient_infor$beq5d_score[bootstrap_samples_gp_diagnosed[is.element(patient_infor$xvfracid, intersect(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],randomsample_notreferred)), i]]*0.25 +
         PSA_results[bootstrap_samples_longterm[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(randomsample_notreferred, toString))), i],3]

   # Referred but no fracture (i.e. not diagnosed in original dataset)
  random_QALYs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "No"], toString),lapply(randomsample, toString))), i+1] <-
    patient_infor$feq5d_score[bootstrap_samples_gp_notdiagnosed[is.element(patient_infor$xvfracid, intersect(patient_infor$xvfracid[patient_infor$vfyncode == "No"],randomsample)), i]]*0.25 
  # Not referred and no fracture
  random_QALYs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "No"], toString),lapply(randomsample_notreferred, toString))), i+1] <-
    patient_infor$beq5d_score[bootstrap_samples_gp_notdiagnosed[is.element(patient_infor$xvfracid, intersect(patient_infor$xvfracid[patient_infor$vfyncode == "No"],randomsample_notreferred)), i]]*0.25 
     
  # Referred and fracture
  random_costs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(randomsample, toString))), i+1] <-
       patient_infor$fcosts[bootstrap_samples_gp_diagnosed[is.element(patient_infor$xvfracid, intersect(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],randomsample)), i]] +
    PSA_results[bootstrap_samples_longterm[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(randomsample, toString))) ,i],4] 
  # Not referred and fracture
  random_costs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(randomsample_notreferred, toString))), i+1] <-
       patient_infor$bcosts[bootstrap_samples_gp_diagnosed[is.element(patient_infor$xvfracid, intersect(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],randomsample_notreferred)), i]] +
      PSA_results[bootstrap_samples_longterm[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(randomsample_notreferred, toString))), i],2]
  
  # Referred no fracture
  random_costs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "No"], toString),lapply(randomsample, toString))), i+1] <-
    patient_infor$fcosts[bootstrap_samples_gp_notdiagnosed[is.element(patient_infor$xvfracid, intersect(patient_infor$xvfracid[patient_infor$vfyncode == "No"],randomsample)), i]] 
  # Not referred and no fracture
  random_costs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "No"], toString),lapply(randomsample_notreferred, toString))), i+1] <-
    patient_infor$bcosts[bootstrap_samples_gp_notdiagnosed[is.element(patient_infor$xvfracid, intersect(patient_infor$xvfracid[patient_infor$vfyncode == "No"],randomsample_notreferred)), i]] 
}     


# QALYs and costs for patients who did not have gp consultation
# These matrix assignments didn't work so using the for loop below instead

#random_QALYs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(notgp_xvfracid, toString))), 2:1001] = 
#  patient_infor$beq5d_score[bootstrap_samples_notgp_diagnosed[unlist(lapply(notgp_xvfracid, toString)),]]*0.25 +
#  PSA_results[bootstrap_samples_longterm[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(notgp_xvfracid, toString))), ],3]
#random_costs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(notgp_xvfracid, toString))), 2:1001] = 
#  patient_infor$bcosts[bootstrap_samples_notgp_diagnosed[unlist(lapply(notgp_xvfracid, toString)),]]  +
#   PSA_results[bootstrap_samples_longterm[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(notgp_xvfracid, toString))), ],2]

#random_QALYs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "No"], toString),lapply(notgp_xvfracid, toString))), 2:1001] = 
#  patient_infor$beq5d_score[bootstrap_samples_notgp_notdiagnosed[unlist(lapply(notgp_xvfracid, toString)),]] 
#random_costs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "No"], toString),lapply(notgp_xvfracid, toString))), 2:1001] = 
#  patient_infor$bcosts[bootstrap_samples_notgp_notdiagnosed[unlist(lapply(notgp_xvfracid, toString)),]] 

#### QALYs for patients who did not have gp consultation
for(i in 1:nrow(patient_infor)){
  print(paste("Calculating for patient",i, "/",nrow(patient_infor)))
  
  if(patient_infor$mrbackpainbas[i] == 0){
    # Do not have an x-ray
    if (patient_infor$vfyncode[i] == "Yes") {
      # Have a fracture but don't get treatment so add no treatment QALYs
      random_QALYs[i,2:(n_samples+1)] = 
       patient_infor$beq5d_score[bootstrap_samples_notgp_diagnosed[i, ]]*0.25 + PSA_results[bootstrap_samples_longterm[i, ],3]
    } else {
      # No fracture so no additional costs
      random_QALYs[i,2:(n_samples+1)] = patient_infor$beq5d_score[bootstrap_samples_notgp_notdiagnosed[i, ]]*0.25
    }
  }
}

#### Costs for patients who did not have gp consultation

for(i in 1:nrow(patient_infor)){
  print(paste("Calculating for patient",i, "/",nrow(patient_infor)))
  
  if( patient_infor$mrbackpainbas[i] == 0 ) {
    # Do not have an x-ray
    if (patient_infor$vfyncode[i] == "Yes") {
      # Have a fracture but don't get treatment so add no treatment QALYs
      random_costs[i,2:(n_samples+1)] = 
        patient_infor$bcosts[bootstrap_samples_notgp_diagnosed[i, ]] + PSA_results[bootstrap_samples_longterm[i, ],2]
    } else {
      # No fracture so no additional costs
      random_costs[i,2:(n_samples+1)] = patient_infor$bcosts[bootstrap_samples_notgp_notdiagnosed[i, ]]
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
nb_standard_of_care_raw = data.frame(random_QALYs*20000 - random_costs)
nb_standard_of_care = nb_standard_of_care_raw[,2:1001]
nb_mean_col = colMeans(nb_standard_of_care, na.rm = TRUE)
mean(nb_mean_col)
quantile(nb_mean_col,.025)
quantile(nb_mean_col,.975)

write.csv(nb_standard_of_care, 'results/nb_standard_of_care.csv')
write.csv(random_QALYs_mean, 'results/longterm_standard_qalys.csv')
write.csv(random_costs_mean, 'results/longterm_standard_costs.csv')
