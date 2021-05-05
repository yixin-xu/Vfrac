### Estimate the costs and qalys of short term vfrac tool
### Step 2, running after "qalys and costs" code

library(readxl)

# read excel file
patient_infor_raw = read.csv('results/qalys and costs.csv')

patient_infor = subset(patient_infor_raw, !is.na(patient_infor_raw$xvfracscreen))


for( i in 1:nrow(patient_infor)){
  if(patient_infor$xvfracscreen[i] == 0){
    patient_infor$st_eq5d[i] = patient_infor$beq5d_score[i]}
  if(patient_infor$xvfracscreen[i] == 1){
    patient_infor$st_eq5d[i] = patient_infor$feq5d_score[i]}
  
}

for( i in 1:nrow(patient_infor)){
  if(patient_infor$xvfracscreen[i] == 0){
    patient_infor$st_costs[i] = patient_infor$bcosts[i]}
  if(patient_infor$xvfracscreen[i] == 1){
    patient_infor$st_costs[i] = patient_infor$fcosts[i]}
  
}

patient_infor$st_QALYs = patient_infor$st_eq5d*0.25

mean_costs = mean(patient_infor$st_costs)
mean_qalys = mean(patient_infor$st_QALYs, na.rm = TRUE)
quantile(mean_costs,na.rm = TRUE,.025)
quantile(mean_costs,na.rm = TRUE,.975)
quantile(mean_qalys,na.rm = TRUE,.025)
quantile(mean_qalys,na.rm = TRUE,.975)
