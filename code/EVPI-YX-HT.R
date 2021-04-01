library(readxl)
library(dplyr)

nb_standard = read.csv('results/nb_standard_of_care.csv')
nb_vfrac = read.csv('results/nb_vfrac.csv')

# calculate the total net benefit of qalys and costs
mean_nb_vfrac = mean(colMeans(nb_vfrac[,2:1001],na.rm = TRUE))
mean_nb_standard = mean(colMeans(nb_standard[,2:1001]))

# select the max net benefit from each sample
nb_standard_max = data.frame(apply(nb_standard[,2:1001],2,max))
nb_vfrac_max = data.frame(apply(nb_vfrac[,2:1001],2,max))

# HT: I corrected the above as you need the average for each sample over all patients
# You'll then compared vfrac and standard and take the maximum of the two means
nb_standard_sampled = colMeans(nb_standard[,2:1001])
nb_vfrac_sampled = colMeans(nb_vfrac[,2:1001],na.rm = TRUE)


####### EVPI
evpi_raw <- rep(NA, dim(nb_vfrac)[2]-1)
for (i in 1:nrow(nb_vfrac_max)){
  evpi_raw[i] = max(nb_vfrac_sampled[i],nb_standard_sampled[i])-max(mean_nb_vfrac, mean_nb_standard) 
}

EVPI = mean(evpi_raw)

# HT: Can also do it without the loop
# HT: Put in a matrix to avoid the loop
nb_sampled <- matrix(c(colMeans(nb_standard[,2:1001]), colMeans(nb_vfrac[,2:1001],na.rm = TRUE)), ncol = 2)
EVPI <- mean(apply(nb_sampled, c(1), max) - max(apply(nb_sampled, c(2), mean)))

# Population size (female age 65+)
base_population = 12390000*0.51

# Population for 10 years
discount = 0.035
population = sum(base_population * ((1 / (1 + discount)^(c(0:9)))))

# Total EVPI
total_EVPI = EVPI*population
