### Estimate the costs, QALYs and net benefit of the long term vfrac tool. 
### Step 3, after running the "qalys and costs", "shortterm standard" and "shortterm vfrac" code

library(tidyverse)
library(readxl)
library(dplyr)
library(survival)

patient_infor_raw = read.csv('results/qalys and costs.csv')
PSA_results = as.data.frame(read_excel('data/PSA mean costs and qalys.xlsx'))
#shortterm = read.csv('results/shortterm_vfrac.csv')
patient_infor = patient_infor_raw%>% drop_na(36)



# set sample size and random data frame
n_samples = 1000
random_QALYs = data.frame(matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples))
random_costs = data.frame(matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples))
rownames(random_QALYs) <- rownames(random_costs) <- patient_infor$xvfracid

recommended_notdiagnosed = patient_infor[patient_infor$xvfracscreen =="1"&patient_infor$vfyncode =="No",]
notrecommended_notdiagnosed = patient_infor[patient_infor$xvfracscreen =="0"&patient_infor$vfyncode =="No",]
recommended_diagnosed = patient_infor[patient_infor$xvfracscreen =="1"&patient_infor$vfyncode =="Yes",]
notrecommended_diagnosed = patient_infor[patient_infor$xvfracscreen =="0"&patient_infor$vfyncode =="Yes",]

# set bootstrap - thee are resamples of patient ID
bootstrap_samples_recommended_notdiagnosed <- bootstrap_samples_notrecommended_notdiagnosed <-
  bootstrap_samples_recommended_diagnosed <- bootstrap_samples_notrecommended_diagnosed <-
  matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples)
for(i in 1:dim(bootstrap_samples_recommended_notdiagnosed)[1]) {
  bootstrap_samples_recommended_notdiagnosed[i, ] <- sample(which(is.element(patient_infor$xvfracid,recommended_notdiagnosed$xvfracid)), size = n_samples, replace = TRUE)
  bootstrap_samples_notrecommended_notdiagnosed[i, ] <- sample(which(is.element(patient_infor$xvfracid,notrecommended_notdiagnosed$xvfracid)), size = n_samples, replace = TRUE)
  bootstrap_samples_recommended_diagnosed[i, ] <- sample(which(is.element(patient_infor$xvfracid,recommended_diagnosed$xvfracid)), size = n_samples, replace = TRUE)
  bootstrap_samples_notrecommended_diagnosed[i, ] <- sample(which(is.element(patient_infor$xvfracid,notrecommended_diagnosed$xvfracid)), size = n_samples, replace = TRUE)
}
rownames(bootstrap_samples_recommended_notdiagnosed) <- rownames(bootstrap_samples_notrecommended_notdiagnosed) <-
  rownames(bootstrap_samples_recommended_diagnosed) <- rownames(bootstrap_samples_notrecommended_diagnosed) <- 
  patient_infor$xvfracid

# set bootstrap for long term - these are resamples of PSA samples
bootstrap_samples_longterm <- matrix(NA, nrow = length(patient_infor$xvfracid), ncol = n_samples)
for(i in 1:dim(bootstrap_samples_longterm)[1]) {
  bootstrap_samples_longterm[i, ] <- sample(1:n_samples, size = n_samples)
}
rownames(bootstrap_samples_longterm) <- patient_infor$xvfracid


# If vfyncode and XVfracscreen are both yes, long-term costs and QALYs are short-term costs and QALYs add PSA results, else long-term costs and QALYs are short-term costs and QALYs
# xvfracscreen: does vfrac recommend xray
# If vfrac=1 and fracture=0
# Use short term follow-up 
# If vfrac=1 and fracture = 1
# Use short term follow-up and PSA alendronate data
# If vfrac=0 and fracture=0
# Use baseline
# If vfrac=0 and fracture=1
# Use baseline and add PSA no treatment data

  
  for(i in 1:1000){
    print(i)
    
    # If not recommended an x-ray by vfrac use baseline EQ5D and cost
    # Use bootstrap sample i for each patient
    random_QALYs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(patient_infor$xvfracid[patient_infor$xvfracscreen == "1"], toString))), i] <-
      patient_infor$feq5d_score[bootstrap_samples_recommended_diagnosed[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],patient_infor$xvfracid[patient_infor$xvfracscreen == "1"])), i]]*0.25  +
      PSA_results[bootstrap_samples_longterm[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(patient_infor$xvfracid[patient_infor$xvfracscreen == "1"], toString))) ,i],5] 
    random_costs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(patient_infor$xvfracid[patient_infor$xvfracscreen == "1"], toString))), i] <-
      patient_infor$fcosts[bootstrap_samples_recommended_diagnosed[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],patient_infor$xvfracid[patient_infor$xvfracscreen == "1"])), i]]+
      PSA_results[bootstrap_samples_longterm[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "Yes"], toString),lapply(patient_infor$xvfracid[patient_infor$xvfracscreen == "1"], toString))), i],4]
      
    random_QALYs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "No"], toString),lapply(patient_infor$xvfracid[patient_infor$xvfracscreen == "1"], toString))), i] <-
      patient_infor$feq5d_score[bootstrap_samples_recommended_notdiagnosed[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "No"],patient_infor$xvfracid[patient_infor$xvfracscreen == "1"])), i]]*0.25
    random_costs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "No"], toString),lapply(patient_infor$xvfracid[patient_infor$xvfracscreen == "1"], toString))), i] <-
      patient_infor$fcosts[bootstrap_samples_recommended_notdiagnosed[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "No"],patient_infor$xvfracid[patient_infor$xvfracscreen == "1"])), i]]
     
    random_QALYs[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],patient_infor$xvfracid[patient_infor$xvfracscreen == "0"])), i] <-
      patient_infor$beq5d_score[bootstrap_samples_notrecommended_diagnosed[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],patient_infor$xvfracid[patient_infor$xvfracscreen == "0"])),i]]*0.25+
      PSA_results[bootstrap_samples_longterm[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],patient_infor$xvfracid[patient_infor$xvfracscreen == "0"])), i],3]
   random_costs[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],patient_infor$xvfracid[patient_infor$xvfracscreen == "0"])), i] <-
     patient_infor$bcosts[bootstrap_samples_notrecommended_diagnosed[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],patient_infor$xvfracid[patient_infor$xvfracscreen == "0"])),i]]+
     PSA_results[bootstrap_samples_longterm[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "Yes"],patient_infor$xvfracid[patient_infor$xvfracscreen == "0"])), i],2]
    
    random_QALYs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "No"], toString),lapply(patient_infor$xvfracid[patient_infor$xvfracscreen == "0"], toString))), i] <-
      patient_infor$beq5d_score[bootstrap_samples_notrecommended_notdiagnosed[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "No"],patient_infor$xvfracid[patient_infor$xvfracscreen == "0"])), i]]*0.25
    random_costs[unlist(intersect(lapply(patient_infor$xvfracid[patient_infor$vfyncode == "No"], toString),lapply(patient_infor$xvfracid[patient_infor$xvfracscreen == "0"], toString))), i] <-
      patient_infor$bcosts[bootstrap_samples_notrecommended_notdiagnosed[is.element(patient_infor$xvfracid,intersect( patient_infor$xvfracid[patient_infor$vfyncode == "No"],patient_infor$xvfracid[patient_infor$xvfracscreen == "0"])), i]]
    
}
    
# calculate the mean of costs and QALYs for each column
qalys_mean = data.frame(colMeans(random_QALYs, na.rm = TRUE))
costs_mean = data.frame(colMeans(random_costs, na.rm = TRUE))

#calculate the total mean
long_term_qalys_mean = colMeans(qalys_mean)
long_term_costs_mean = colMeans(costs_mean)

# Calculate 95% confidence intervals for QALYs
quantile(qalys_mean$colMeans.random_QALYs..na.rm...TRUE.,.025)
quantile(qalys_mean$colMeans.random_QALYs..na.rm...TRUE.,.975)

# Calculate 95% confidence intervals for QALYs
quantile(costs_mean$colMeans.random_costs..na.rm...TRUE.,.025)
quantile(costs_mean$colMeans.random_costs..na.rm...TRUE.,.975)


###### NET BENEFIT 20000*IQ-IC
nb_vfrac = 20000*random_QALYs - random_costs
mean_nb_vfrac = data.frame(colMeans(nb_vfrac,na.rm = TRUE))
mean = colMeans(mean_nb_vfrac)
quantile(mean_nb_vfrac$colMeans.nb_vfrac..na.rm...TRUE.,.025)
quantile(mean_nb_vfrac$colMeans.nb_vfrac..na.rm...TRUE.,.975)

write.csv(nb_vfrac, 'results/nb_vfrac.csv')
write.csv(costs_mean, 'results/longterm_vfrac_costs.csv')
write.csv(qalys_mean, 'results/longterm_vfrac_qalys.csv')
