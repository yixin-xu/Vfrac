library(readxl)

# read excel files
setwd('C:/Users/yx18392/OneDrive - University of Bristol/Desktop/Vfrac/')
T1 = read_excel('1.xls')
T2=T1%>% drop_na(25:29)
per_cost = read_excel('Per cost of each therapy.xlsx')

mergecell = data.frame(T2[,4:13], T2[,47:56])
mergecell[is.na(mergecell)] = 0

# calculate costs
costs = data.frame(matrix(0,1600,1))


for(i in 1:10){
  a = c(mergecell[,i] *as.numeric(per_cost[i,2]))
  costs = cbind(costs,a)
}

for(i in 1:10){
  a = c(mergecell[,10+i] *as.numeric(per_cost[i,2]))
  costs = cbind(costs,a)
}

costs$sum <- rowSums(costs[, 2:21])
