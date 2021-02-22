library(readxl)
library(dplyr)
library(tidyverse)

# read excel file
patient_infor_raw = read_excel('data/patient information.xls')
patient_infor_raw2=patient_infor_raw%>% drop_na(25:29)
patient_infor=patient_infor_raw2%>% drop_na(38:42)
value_sets = read_excel('data/EuroQoL value sets 19Jan2019.xlsx')
per_cost = read_excel('data/Per cost of each therapy.xlsx')
drugs_cost = read_excel('data/average daily dose-Copy of Pain meds doses 13Feb2021 EC.xlsx')


################## QALYs

####### Baseline


#select columns relevant to baseline 5L Prolife
bProlife=patient_infor[, 25:29]

#combine these columns into 5L Prolife
bfiveL_profile = data.frame(matrix(0,nrow(bProlife),1))

for( i in 1:nrow(bProlife)){
  bfiveL_profile[i,1] = data.frame(paste(bProlife[i,1],bProlife[i,2],bProlife[i,3],
                                             bProlife[i,4],bProlife[i,5],sep = ""))
}

bfiveL_profile$`5L profile` = as.numeric(bfiveL_profile$matrix.0..nrow.bProlife...1.)
typeof(bfiveL_profile$`5L profile`)

#get the qalys of UK
align_two_table = left_join(bfiveL_profile,value_sets, by = '5L profile')
beq5d_score = align_two_table[, 10]
mode(beq5d_score)

patient_infor$beq5d_score = beq5d_score

####### follow-up 

#select columns relevant to follow-up 5L Prolife
fProlife=patient_infor[, 38:42]

#combine these columns into 5L Prolife
ffiveL_profile = data.frame(matrix(0,nrow(fProlife),1))

for( i in 1:nrow(bProlife)){
  ffiveL_profile[i,1] = data.frame(paste(fProlife[i,1],fProlife[i,2],fProlife[i,3],
                                         fProlife[i,4],fProlife[i,5],sep = ""))
}

ffiveL_profile$`5L profile` = as.numeric(ffiveL_profile$matrix.0..nrow.fProlife...1.)

typeof(ffiveL_profile$`5L profile`)

#get the qalys of UK
align_two_table = left_join(ffiveL_profile,value_sets, by = '5L profile')
feq5d_score = align_two_table[, 10]
mode(feq5d_score)

patient_infor$feq5d_score = feq5d_score

################## costs 31 43

# daily drugs dose
average_daily_durgs_costs = 0.362*drugs_cost[1,7]+0.121*drugs_cost[2,7]+0.087*drugs_cost[3,7]+0.087*drugs_cost[4,7]+0.071*drugs_cost[5,7]+0.061*drugs_cost[6,7]+0.053*drugs_cost[7,7]+0.051*drugs_cost[8,7]
#weekly drugs dose
#when 13painmedfreq = 0
weekly1 = 0
#when 13painmedfreq = 1 
weekly2 = average_daily_durgs_costs*3.5
#when 13painmedfreq >= 2
weekly3 = average_daily_durgs_costs*7


mergecell = data.frame(patient_infor[,4:13], patient_infor[,47:56])
mergecell[is.na(mergecell)] = 0

# calculate costs
costs = data.frame(matrix(0,1429,1))

for(i in 1:10){
  a = c(mergecell[,i] *as.numeric(per_cost[i,2]))
  costs = cbind(costs,a)
}

for(i in 1:10){
  a = c(mergecell[,10+i] *as.numeric(per_cost[i,2]))
  costs = cbind(costs,a)
}

bcosts = rowSums(costs[, 2:11])
fcosts = rowSums(costs[, 12:21])

# add baseline costs
patient_infor$bcosts = 0

for (i in 1:nrow(patient_infor)){
  if (is.na(patient_infor$b28painmedfreq[i])){
    patient_infor$bcosts[i] = bcosts[i]}
  else if (patient_infor$b28painmedfreq[i] == 0){
    patient_infor$bcosts[i] = bcosts[i]+weekly1}
  else if (patient_infor$b28painmedfreq[i] == 1){
    patient_infor$bcosts[i] = bcosts[i] + weekly2[1,1]}
  else if (patient_infor$b28painmedfreq[i] >= 2){
    patient_infor$bcosts[i] = bcosts[i] + weekly3[1,1]}
}

# add follow-up costs
patient_infor$fcosts = 0
# assuming 30 days a month
for (i in 1:nrow(patient_infor)){
  if (is.na(patient_infor$f13painmedfreq[i])){
    patient_infor$fcosts[i] = bcosts[i]}
  else if (patient_infor$f13painmedfreq[i] == 0){
    patient_infor$fcosts[i] = bcosts[i]+weekly1*90/7}
  else if (patient_infor$f13painmedfreq[i] == 1){
    patient_infor$fcosts[i] = bcosts[i] + weekly2[1,1]*90/7}
  else if (patient_infor$f13painmedfreq[i] >= 2){
    patient_infor$fcosts[i] = bcosts[i] + weekly3[1,1]*90/7}
}

write.csv(patient_infor, 'results/qalys and costs.csv')
