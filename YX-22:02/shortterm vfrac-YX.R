library(readxl)
library(tidyverse)
# read excel file
patient_infor_raw = read.csv('results/qalys and costs.csv')

patient_infor = patient_infor_raw%>% drop_na(36)



for( i in 1:nrow(patient_infor)){
  if(patient_infor$xvfracscreen[i] == 0){
    patient_infor$qalys[i] = patient_infor$beq5d_score[i]*0.25}
  if(patient_infor$xvfracscreen[i] == 1){
    patient_infor$qalys[i] = patient_infor$feq5d_score[i]*0.25}

}

for( i in 1:nrow(patient_infor)){
  if(patient_infor$xvfracscreen[i] == 0){
    patient_infor$costs[i] = patient_infor$bcosts[i]}
  if(patient_infor$xvfracscreen[i] == 1){
    patient_infor$costs[i] = patient_infor$fcosts[i]}
  
}


shortterm_vfrac = data.frame(patient_infor$qalys,patient_infor$costs)

write.csv(shortterm_vfrac, 'results/shortterm_vfrac.csv')
