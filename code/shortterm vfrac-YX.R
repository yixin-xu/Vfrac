### Estimate the costs and qalys of short term vfrac tool
### Step 2, running after "qalys and costs" code


library(readxl)
library(tidyverse)
# read excel file
patient_infor_raw = read.csv('results/qalys and costs.csv')

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


# Calculate costs and QALYs depending on whether they would be referred for x-ray by vfrac
for( i in 1:nrow(patient_infor)){
  if(patient_infor$xvfracscreen[i] == 0){
    random_QALYs[i,1:n_samples]  = patient_infor$beq5d_score[bootstrap_samples[i, ]]*0.25
    random_costs[i,1:n_samples] = patient_infor$bcosts[bootstrap_samples[i, ]]
    }
  if(patient_infor$xvfracscreen[i] == 1){
    random_QALYs[i,1:n_samples]  = patient_infor$feq5d_score[bootstrap_samples[i, ]]*0.25
    random_costs[i,1:n_samples] = patient_infor$fcosts[bootstrap_samples[i, ]]
    }

}

random_QALYs_mean = colMeans(random_QALYs, na.rm = TRUE)
random_costs_mean = colMeans(random_costs)

mean(random_QALYs_mean) 
quantile(random_QALYs_mean,.025)
quantile(random_QALYs_mean,.975)

mean(random_costs_mean)
quantile(random_costs_mean,.025)
quantile(random_costs_mean,.975)

shortterm_vfrac = rbind(random_costs_mean,random_QALYs_mean)

write.csv(random_QALYs, 'results/shortterm-vfrac-qalys.csv')
write.csv(random_costs, 'results/shortterm-vfrac-costs.csv')
write.csv(shortterm_vfrac, 'results/shortterm_vfrac.csv')
