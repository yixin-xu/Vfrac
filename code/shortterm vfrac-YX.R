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
rownames(random_QALYs) <- rownames(random_costs) <- patient_infor$xvfracid

xvfracscreen0 = patient_infor[patient_infor$xvfracscreen =="0",]
xvfracscreen1 = patient_infor[patient_infor$xvfracscreen =="1",]

# set bootstrap
bootstrap_samples_xvfracscreen0 <- matrix(NA, nrow = nrow(xvfracscreen0), ncol = n_samples)
for(i in 1:dim(bootstrap_samples_xvfracscreen0)[1]) {
  bootstrap_samples_xvfracscreen0[i, ] <- sample(1:nrow(xvfracscreen0), size = n_samples,replace = TRUE)
}
rownames(bootstrap_samples_xvfracscreen0) <- xvfracscreen0$xvfracid

bootstrap_samples_xvfracscreen1 <- matrix(NA, nrow = nrow(xvfracscreen1), ncol = n_samples)
for(i in 1:dim(bootstrap_samples_xvfracscreen1)[1]) {
  bootstrap_samples_xvfracscreen1[i, ] <- sample(1:nrow(xvfracscreen1), size = n_samples,replace = TRUE)
}
rownames(bootstrap_samples_xvfracscreen1) <- xvfracscreen1$xvfracid

for(i in 1:1000){
  print(i)
  
  # If not recommended an x-ray by vfrac use baseline EQ5D and cost
  # Use bootstrap sample i for each patient
  random_QALYs[unlist(lapply(xvfracscreen0$xvfracid, toString)), i] <-
    patient_infor$beq5d_score[bootstrap_samples_xvfracscreen0[is.element(xvfracscreen0$xvfracid, patient_infor$xvfracid), i]]*0.25
  random_costs[unlist(lapply(xvfracscreen0$xvfracid, toString)), i] <-
    patient_infor$bcosts[bootstrap_samples_xvfracscreen0[is.element(xvfracscreen0$xvfracid, patient_infor$xvfracid), i]]
  
  # If recommended an x-ray by vfrac use follow-up EQ5D and cost
  # Again use bootstrap sample i for each patient
  random_QALYs[unlist(lapply(xvfracscreen1$xvfracid, toString)), i+1] <-
    patient_infor$feq5d_score[bootstrap_samples_xvfracscreen1[is.element(xvfracscreen1$xvfracid, patient_infor$xvfracid), i]]*0.25
  random_costs[unlist(lapply(xvfracscreen1$xvfracid, toString)), i+1] <-
    patient_infor$fcosts[bootstrap_samples_xvfracscreen1[is.element(xvfracscreen1$xvfracid, patient_infor$xvfracid), i]]
}


random_QALYs_mean = colMeans(random_QALYs, na.rm = TRUE)
random_costs_mean = colMeans(random_costs, na.rm = TRUE)

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
