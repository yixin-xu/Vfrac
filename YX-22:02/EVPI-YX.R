library(readxl)
library(dplyr)

nb_standard = read.csv('results/nb_standard_of_care.csv')
nb_vfrac = read.csv('results/nb_vfrac.csv')

# calculate the total net benefit of qalys and costs
mean_nb_vfrac = mean(colMeans(nb_vfrac[,2:1001]))
mean_nb_standard = mean(colMeans(nb_standard[,2:1001]))

# select the max net benefit from each sample
nb_standard_max = data.frame(apply(nb_standard[,2:1001],2,max))
nb_vfrac_max = data.frame(apply(nb_vfrac[,2:1001],2,max))

####### EVPI
for (i in 1:nrow(nb_vfrac_max)){
  evpi_raw[i] = max(nb_vfrac_max[i,1],nb_standard_max[i,1])-max(mean_nb_vfrac, mean_nb_standard) 
}
  
EVPI = mean(evpi_raw)

