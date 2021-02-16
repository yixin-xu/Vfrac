library(readxl)
library(dplyr)
library(tidyverse)

# read excel file
setwd('C:/Users/yx18392/OneDrive - University of Bristol/Desktop/Vfrac/')
T1 = read_excel('1.xls')
T2=T1%>% drop_na(25:29)
T3=T2%>% drop_na(38:42)

#select columns relevant to 5L Prolife
bProlife=T3[, 25:29]


#combine these columns into 5L Prolife
bfiveL_profile = data.frame(matrix(0,nrow(bProlife),1))

for( i in 1:nrow(bProlife)){
  bfiveL_profile[i,1] = data.frame(paste(bProlife[i,1],bProlife[i,2],bProlife[i,3],
                                             bProlife[i,4],bProlife[i,5],sep = ""))
}

bfiveL_profile$`5L profile` = as.numeric(bfiveL_profile$matrix.0..nrow.bProlife...1.)
typeof(bfiveL_profile$`5L profile`)

# read EuroQoL value sets table
value_sets = read_excel('EuroQoL value sets 19Jan2019.xlsx')

#get the qalys of UK
align_two_table = left_join(bfiveL_profile,value_sets, by = '5L profile')
beq5d_score = align_two_table[, 10]
mode(beq5d_score)

T3$beq5d_score = beq5d_score

T2$cost_sum = costs$sum

write.csv(T2, 'T2.csv')
