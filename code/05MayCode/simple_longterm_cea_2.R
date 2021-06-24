# Vfrac health economics
# Analysis of long-term costs and QALYs only
# Howard Thom 14April2021
# v2 24th June 2021 following discussions with Emma C and Sarah D

# All proportions derived from Vfrac dataset follow Dirichlet-distributions

library(readxl)
library(tidyverse)
library(BCEA)
library(DirichletReg)

set.seed(154523450)

treatment_names <- c("SoC", "Vfrac")
n_samples <- 1000

# Need to add correct values for these (e.g. £150 and -0.1)
cost_xray <- 72
disutility_xray <- 0

fracture_prevalence <- 0.12

# Results from long-term economic model
# These are for any patient with a fracture
PSA_results = as.data.frame(read_excel('data/PSA mean costs and qalys.xlsx'))

# Short term results from the Vfrac study
patient_infor_raw = read.csv('results/qalys and costs.csv')
patient_infor = patient_infor_raw%>% drop_na(36)

# Use "guesstimate" based on stakeholder work for proportion referred for x-ray by their GP
# Used a normal distribution ranging from 0.10 to 0.30 to represent uncertainty
prop_referred <- rnorm(n_samples, 0.20, sd = 0.05)
# Cut off below 0 and above 1
prop_referred[prop_referred < 0] <- 0
prop_referred[prop_referred > 1] <- 1

# Final analysis will be per patient with fracture
# To get this, we divide by the proportion with a fracture
n_with_fracture <-  sum(patient_infor$vfyncode == "Yes")
n_patients <- dim(patient_infor)[1]

prop_with_fracture <- rbeta(n_samples, n_with_fracture, n_patients)



# Need proportions with fracture and diagnosed 
# (either by vfrac or GP referral for x-ray) for both treatment options
# Those without a fracture have no impact on their costs and QALYs
# Their only cost and utility decrement will be from unnecessary x-ray

prop_frac_diagnosed <- prop_frac_undiagnosed <- prop_nofrac_referred <- matrix(NA, nrow = n_samples, ncol = 2)
colnames(prop_frac_diagnosed) <- colnames(prop_frac_undiagnosed) <- 
  colnames(prop_nofrac_referred) <- treatment_names


# For Vfrac (no uncertainty in proportion)
n_frac_diagnosed_temp <- sum(patient_infor$xvfracscreen =="1" & patient_infor$vfyncode == "Yes")
n_frac_undiagnosed_temp <- sum(patient_infor$xvfracscreen =="0" & patient_infor$vfyncode == "Yes")
n_nofrac_referred_temp <- sum(patient_infor$xvfracscreen =="1" & patient_infor$vfyncode == "No")
n_nofrac_notreferred_temp <- sum(patient_infor$xvfracscreen =="0" & patient_infor$vfyncode == "No")

# Sample from a Dirichlet
proportions_temp <- rdirichlet(n_samples, alpha = c(n_frac_diagnosed_temp, n_frac_undiagnosed_temp, n_nofrac_referred_temp, n_nofrac_notreferred_temp))
prop_frac_diagnosed[, "Vfrac"] <- proportions_temp[, 1] 
prop_frac_undiagnosed[, "Vfrac"] <- proportions_temp[, 2]
prop_nofrac_referred[, "Vfrac"] <-  proportions_temp[, 3] 

# For SoC assume everyone has a GP consultation but that proportion are referred for radiograph
# Same proportion applied to those with fractures as without
# Probability of fracture is independent of probability of radiograph referral
n_frac_temp <- sum(patient_infor$vfyncode == "Yes")
prop_frac <- rbeta(n_samples, n_frac_temp, n_patients - n_frac_temp)
prop_frac_diagnosed[, "SoC"] <- prop_frac * prop_referred
prop_frac_undiagnosed[, "SoC"] <- prop_frac * (1 - prop_referred)
prop_nofrac_referred[, "SoC"] <- (1 - prop_frac) * prop_referred


# Estimate long-term costs and QALYs
total_costs <- total_qalys <- matrix(NA, nrow = n_samples, ncol = 2)
colnames(total_costs) <- colnames(total_qalys) <- treatment_names 

# Vfrac
total_costs[, "Vfrac"] <- prop_frac_undiagnosed[, "Vfrac"] * PSA_results$`No treatment costs` +
  prop_frac_diagnosed[, "Vfrac"] * PSA_results$`Alendronate costs`
total_qalys[, "Vfrac"] <- prop_frac_undiagnosed[, "Vfrac"] * PSA_results$`No treatment qalys` +
  prop_frac_diagnosed[, "Vfrac"] * PSA_results$`Alendronate qalys`

# SoC
total_costs[, "SoC"] <- prop_frac_undiagnosed[, "SoC"] * PSA_results$`No treatment costs` +
  prop_frac_diagnosed[, "SoC"] * PSA_results$`Alendronate costs`
total_qalys[, "SoC"] <- prop_frac_undiagnosed[, "SoC"] * PSA_results$`No treatment qalys` +
  prop_frac_diagnosed[, "SoC"] * PSA_results$`Alendronate qalys`

# Now add the costs and disutility of x-ray
total_costs <- total_costs + (prop_nofrac_referred + prop_frac_diagnosed) * cost_xray
total_qalys <- total_qalys + (prop_nofrac_referred + prop_frac_diagnosed) * disutility_xray


# Calculate total and incremental net benefits
net_benefit <- total_qalys * 20000 - total_costs
inb <- net_benefit - net_benefit[, "SoC"]

# CEAC at £20,000
prob_ce <- rep(NA, 2)
names(prob_ce) <- treatment_names
prob_ce["SoC"] <- sum(net_benefit[, "SoC"] >= net_benefit[, "Vfrac"]) / n_samples
prob_ce["Vfrac"] <- sum(net_benefit[, "Vfrac"] >= net_benefit[, "SoC"]) / n_samples

# Summarise results
format_results <- function(x, n_digits = 2) {
  paste0(format(mean(x), nsmall = n_digits, digits = n_digits), " (",
         format(quantile(x, prob = 0.025), nsmall = n_digits, digits = n_digits), " ,",
         format(quantile(x, prob = 0.975), nsmall = n_digits, digits = n_digits), ")")
}

results_matrix <- matrix(nrow = 9, ncol = 3)
rownames(results_matrix) <- c("OVF diagnosed", "OVF not diagnosed", 
                              "No OVF referred", "Total costs", 
                              "Total QALYs", "Net benefit", "Prob CE",
                              "EVPI", "Population EVPI")
colnames(results_matrix) <- c(treatment_names, "Vfrac - SoC")
for(treatment in treatment_names) {
  results_matrix[c(1:7), treatment] <- c(format_results(prop_frac_diagnosed[, treatment]),
                                   format_results(prop_frac_undiagnosed[, treatment]),
                                   format_results(prop_nofrac_referred[, treatment]),
                                   format_results(total_costs[, treatment]),
                                   format_results(total_qalys[, treatment]),
                                   format_results(net_benefit[, treatment]),
                                   prob_ce[treatment])
}
results_matrix[c(1:7), "Vfrac - SoC"] <- c(format_results(prop_frac_diagnosed[, "Vfrac"] - prop_frac_diagnosed[, "SoC"]),
                                     format_results(prop_frac_undiagnosed[, "Vfrac"] - prop_frac_undiagnosed[, "SoC"]),
                                     format_results(prop_nofrac_referred[, "Vfrac"] - prop_nofrac_referred[, "SoC"]),
                                 format_results(total_costs[, "Vfrac"] - total_costs[, "SoC"]),
                                 format_results(total_qalys[, "Vfrac"] - total_qalys[, "SoC"]),
                                 format_results(net_benefit[, "Vfrac"] - net_benefit[, "SoC"]),
                                 NA)



####### EVPI
# For numerical stability estimate expected NB based on perfect information first
enb_perfect_info <- rep(NA, n_samples)
for (i in 1:n_samples){
  enb_perfect_info[i] <- max(net_benefit[i, "SoC"],net_benefit[i, "Vfrac"])
}

# Take mean and subtract expected NB on current information to get EVPI
EVPI <- mean(enb_perfect_info) - max(colMeans(net_benefit))

# Population size (female age 65+)
base_population = 12390000 * 0.51 * fracture_prevalence

# Population for 10 years
discount = 0.035
population = sum(base_population * ((1 / (1 + discount)^(c(0:9)))))

# Total EVPI
total_EVPI = EVPI*population


results_matrix[c("EVPI", "Population EVPI"), "Vfrac - SoC"] <- 
  c(format(EVPI, digits = 2), total_EVPI)
write.csv(results_matrix, file = "results/simple_longterm_results.csv")


### draw picture


setwd('results')
# Plot cost-effectiveness plane using base graphics
jpeg(file = paste0('results', "ce.plane.vfrac.vs standard of care.", n_samples,".jpg"))
vfrac_bcea = bcea(e = total_qalys, c = total_costs, ref = 2, interventions = c("standard of care", "vfrac"))
summary(vfrac_bcea, wtp = 20000)
ceplane.plot(vfrac_bcea, comparison = 1, wtp = 20000, graph = "base")
dev.off()
dev.new()

# Cost-effectiveness acceptability curve
vfrac_multi_ce = multi.ce(vfrac_bcea)
jpeg(file = paste0('results', "CEAC", n_samples,".jpg"))
mce.plot(vfrac_multi_ce,pos = c(1, 0.5), graph = c("base","ggplot2")) 
dev.off()
dev.new()



