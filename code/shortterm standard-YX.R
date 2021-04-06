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

# Use xvfyncode to identify which patients have a fracture

# Proportion referred by GP for x-ray is a random variable
prop_referred <- rnorm(n_samples, mean = 0.20, sd = 0.05)

# Generate 1000 random samples assuming X% are referred for x-ray
for(i in 1:1000){
  # Random sample referred for x-ray
  randomsample = sample(gp_xvfracid, size = round(prop_referred[i] * length(gp_xvfracid)), replace = TRUE) # Should be replace = TRUE
  for (a in gp_xvfracid){
    # If referred use follow-up EQ5D and cost
    # If not referred use baseline
    if(a %in% randomsample){
      random_eq5d[match(a,gp_xvfracid),i+1] = patient_infor$feq5d_score[patient_infor$xvfracid == a]
    } else{random_eq5d[match(a,gp_xvfracid),i+1] = patient_infor$beq5d_score[patient_infor$xvfracid == a]}
    if(a %in% randomsample){
      random_costs[match(a,gp_xvfracid),i+1] = patient_infor$fcosts[patient_infor$xvfracid == a]
    } else{random_costs[match(a,gp_xvfracid),i+1] = patient_infor$bcosts[patient_infor$xvfracid == a]}
  }
}

# Add the IDs to the EQ5D data
random_eq5d$xvfracid = random_eq5d$gp_xvfracid

T6 = left_join(patient_infor, random_eq5d, by ='xvfracid')
# Combine IDs with sampled eq5d
T7 = data.frame(T6[, 2], T6[, 75:1074])

# Loop through the patients
for(i in T7$T6...2.){
  # If i was one of the patients with a GP referal
  if(i %in% random_eq5d$gp_xvfracid == FALSE){
    # Not sure why it's being set to the baseline score?
    T7[match(i,T7$T6...2.),2:1001] = patient_infor$beq5d_score[patient_infor$xvfracid == i]}
}

random_costs$xvfracid = random_costs$gp_xvfracid
T8 = left_join(patient_infor, random_costs, by ='xvfracid')
T9 = data.frame(T8[, 2], T8[, 75:1074])

for(i in T9$T8...2.){
  if(i %in% random_costs$gp_xvfracid == FALSE){
    T9[match(i,T9$T8...2.),2:1001] = patient_infor$bcosts[patient_infor$xvfracid == i]}
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


