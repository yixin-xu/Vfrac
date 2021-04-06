### Estimate the costs and qalys of short term standard of care.
### Step 2, after running "qalys and costs" code


library(tidyverse)
library(readxl)

patient_infor_raw = read.csv('results/qalys and costs.csv')
# Drop rows without mrbackpainbas variable
patient_infor=patient_infor_raw%>% drop_na(69)

# Filter to only patients who have a consultation for back pain during 
# the 3 months before enrolment in the study
gp_xvfracid = patient_infor$xvfracid[patient_infor$mrbackpainbas == 1]

# Patient IDs
# The final dataframe will have one row for each patient and each column
# corresponds to each of 1000 sampled values.
random_eq5d = data.frame(gp_xvfracid)
random_costs = data.frame(gp_xvfracid)
# Prebuild the full matrix to reduce runtime (changing a dataframe is slow so the loop below is 
# slower without these two lines)
random_eq5d <- cbind(random_eq5d, matrix(nrow = dim(random_eq5d)[1], ncol = n_samples))
random_costs <- cbind(random_costs, matrix(nrow = dim(random_costs)[1], ncol = n_samples))
# Give rows meaningful names
rownames(random_eq5d) <- rownames(random_costs) <- gp_xvfracid

# Use xvfyncode to identify which patients have a fracture

# Proportion referred by GP for x-ray is a random variable
prop_referred <- rnorm(n_samples, mean = 0.20, sd = 0.05)

# set bootstrap
bootstrap_samples <- matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples)
for(i in 1:dim(bootstrap_samples)[1]) {
  bootstrap_samples[i, ] <- sample(1:length(patient_infor$xvfracid), size = n_samples)
}
rownames(bootstrap_samples) <- patient_infor$xvfracid


for(i in 1:1000){
  print(i)
  # Random sample referred for x-ray
  # Sampled without replacement because assigning costs to each actual patient 
  randomsample = sample(gp_xvfracid, size = round(prop_referred[i] * length(gp_xvfracid)), replace = FALSE) 
  # Patients not referred for x-ray
  randomsample_notreferred <- gp_xvfracid[!is.element(gp_xvfracid, randomsample)]
  
  # If referred use follow-up EQ5D and cost
  random_eq5d[unlist(lapply(randomsample, toString)), i+1] <-
    patient_infor$feq5d_score[is.element(patient_infor$xvfracid, randomsample)]
  random_costs[unlist(lapply(randomsample, toString)), i+1] <-
    patient_infor$fcosts[is.element(patient_infor$xvfracid, randomsample)]
  
  # If not referred use baseline EQ5D and cost
  
  random_eq5d[unlist(lapply(randomsample_notreferred, toString)), i+1] <-
    patient_infor$beq5d_score[is.element(patient_infor$xvfracid, randomsample_notreferred)]
  random_costs[unlist(lapply(randomsample_notreferred, toString)), i+1] <-
    patient_infor$bcosts[is.element(patient_infor$xvfracid, randomsample_notreferred)]
}

# Rename the IDs to match with patient_infor
colnames(random_eq5d)[colnames(random_eq5d) == "gp_xvfracid"] <- "xvfracid"

T6 = left_join(patient_infor, random_eq5d, by ='xvfracid')
# Combine IDs with sampled eq5d
# T7 is same as random_eq5d but with NA for patients without GP referral
T7 = data.frame(T6[, 2], T6[, 74:1073])

# Same for costs
random_costs$xvfracid = random_costs$gp_xvfracid
T8 = left_join(patient_infor, random_costs, by ='xvfracid')
T9 = data.frame(T8[, 2], T8[, 74:1073])

# Loop through the patients
# Note that T7$T6...2. is the same as T9$T8...2.
for(i in T7$T6...2.){
  # If i was one of the patients without a GP referal
  # Set to baseline EQ5D
  if(i %in% random_eq5d$gp_xvfracid == FALSE){
    T7[match(i,T7$T6...2.),2:1001] = patient_infor$beq5d_score[bootstrap_samples[toString(i),]]
    T9[match(i,T9$T8...2.),2:1001] = patient_infor$bcosts[bootstrap_samples[toString(i),]]
  }
}



Total_samples_qalys = T7[,2:1001]*0.25
Total_samples_costs = T9[,2:1001]
Total_samples_qalys[is.na(Total_samples_qalys)] = 0

Total_samples_qalys_mean = colMeans(Total_samples_qalys)
Total_samples_costs_mean = colMeans(Total_samples_costs)

shortterm_SoC_qalys_mean = mean(Total_samples_qalys_mean)
quantile(Total_samples_qalys_mean,.025)
quantile(Total_samples_qalys_mean,.975)

shortterm_SoC_costs_mean = mean(Total_samples_costs_mean)
quantile(Total_samples_costs_mean,.025)
quantile(Total_samples_costs_mean,.975)

shortterm_SoC = rbind(Total_samples_costs_mean,Total_samples_qalys_mean)



write.csv(Total_samples_qalys, 'results/shortterm-standard-qalys.csv')
write.csv(Total_samples_costs, 'results/shortterm-standard-costs.csv')
write.csv(shortterm_SoC, 'results/shortterm-standaed-mean C&Q')


