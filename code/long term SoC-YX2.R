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

for(i in 1:1000) {
  print(i)
  # Random sample referred for x-ray
  randomsample = sample(gp_xvfracid, size = length(gp_xvfracid) * prop_referred[i], replace = F)
  # Patients not referred for x-ray
  randomsample_notreferred <- gp_xvfracid[!is.element(gp_xvfracid, randomsample)]
  
  # Have a fracture then get treatment
  random_QALYs[unlist(intersect(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],lapply(randomsample, toString))), i+1] <-
        patient_infor$feq5d_score[bootstrap_samples[is.element(patient_infor$xvfracid, intersect(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],randomsample)), i]]*0.25 + PSA_results[bootstrap_samples_longterm[i, ],5]
  random_QALYs[unlist(intersect(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],lapply(randomsample_notreferred, toString))), i+1] <-
        patient_infor$beq5d_score[bootstrap_samples[is.element(patient_infor$xvfracid, intersect(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],randomsample_notreferred)), i]]*0.25 + PSA_results[bootstrap_samples_longterm[i, ],3]
         
} 

   


# else{
#        random_QALYs[unlist(lapply(randomsample, toString)), i+1] <-
#          patient_infor$feq5d_score[bootstrap_samples[is.element(a, randomsample), i]]*0.25 
#        random_QALYs[unlist(lapply(randomsample_notreferred, toString)), i+1] <-
#          patient_infor$beq5d_score[bootstrap_samples[is.element(a, randomsample_notreferred), i]]*0.25 
#      }
 # }
#}
        
    # Costs
#    if(a %in% randomsample){
      # Had an x-ray
 #     if(patient_infor$vfyncode[patient_infor$xvfracid == a] == "Yes"){
        # Have a fracture then get treatment
#        random_costs[toString(a),i+1] = patient_infor$fcosts[patient_infor$xvfracid == a]+ PSA_results[i,4]} 
 #     else{
        # No fracture so no additional costs
 #       random_costs[toString(a),i+1] = patient_infor$fcosts[patient_infor$xvfracid == a]}
 #   }
 #   else if(patient_infor$vfyncode[patient_infor$xvfracid == a] == "Yes"){
      # Have a fracture but don't get treatment so add no treatment costs
 #     random_costs[toString(a),i+1] = patient_infor$bcosts[patient_infor$xvfracid == a] + PSA_results[i,2]
#    } else{
      # No fracture so no additional costs
#      random_costs[toString(a),i+1] = patient_infor$bcosts[patient_infor$xvfracid == a]}
#  }
#}
