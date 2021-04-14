# Vfrac health economics
# Analysis of long-term costs and QALYs only
# Howard Thom 14April2021

# All proprotions derived from Vfrac dataaset follow beta-distributions

library(readxl)

set.seed(154523450)

treatment_names <- c("SoC", "Vfrac")
n_samples <- 1000

# Need to add correct values for these (e.g. £150 and -0.1)
cost_xray <- 0
disutility_xray <- 0

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
prop_with_fracture <- rbeta(n_samples, n_with_fracture, n_patients)



# Need proportions with fracture and diagnosed 
# (either by vfrac or GP referral for x-ray) for both treatment options
# Those without a fracture have no impact on their costs and QALYs
# Their only cost and utility decrement will be from unnecessary x-ray

prop_frac_diagnosed <- prop_frac_undiagnosed <- prop_nofrac_referred <- matrix(NA, nrow = n_samples, ncol = 2)
colnames(prop_frac_diagnosed) <- colnames(prop_frac_undiagnosed) <- 
  colnames(prop_nofrac_referred) <- treatment_names

n_patients <- dim(patient_infor)[1]

# For Vfrac (no uncertainty in proportion)
n_frac_diagnosed_temp <- sum(patient_infor$xvfracscreen =="1" & patient_infor$vfyncode == "Yes")
prop_frac_diagnosed[, "Vfrac"] <- rbeta(n_samples, n_frac_diagnosed_temp, n_patients - n_frac_diagnosed_temp)
n_frac_undiagnosed_temp <- sum(patient_infor$xvfracscreen =="0" & patient_infor$vfyncode == "Yes")
prop_frac_undiagnosed[, "Vfrac"] <- rbeta(n_samples, n_frac_undiagnosed_temp, n_patients - n_frac_undiagnosed_temp)
n_nofrac_referred_temp <- sum(patient_infor$xvfracscreen =="1" & patient_infor$vfyncode == "No")
prop_nofrac_referred[, "Vfrac"] <-  rbeta(n_samples, n_nofrac_referred_temp, n_patients)

# For SoC (GP consultation plus referral for x-ray only way to be diagnosed)
n_frac_gpconsultation <- sum(patient_infor$mrbackpainbas =="1" & patient_infor$vfyncode =="Yes", na.rm = TRUE)
prop_frac_diagnosed[, "SoC"] <- rbeta(n_samples, n_frac_gpconsultation, n_patients) * prop_referred
n_frac_nogp <- sum(patient_infor$mrbackpainbas == "0" & patient_infor$vfyncode =="Yes", na.rm = TRUE)
prop_frac_undiagnosed[, "SoC"] <- rbeta(n_samples, n_frac_nogp, n_patients) + 
  rbeta(n_samples, n_frac_gpconsultation, n_patients) * (1 - prop_referred)
n_nofrac_referred_temp <- sum(patient_infor$mrbackpainbas =="1" & patient_infor$vfyncode =="No", na.rm = TRUE)
prop_nofrac_referred[, "SoC"] <- prop_referred * rbeta(n_samples, n_nofrac_referred_temp, n_patients)

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


# Scale all results to be per fracture patient
total_costs <- total_costs / prop_with_fracture
total_qalys <- total_qalys / prop_with_fracture


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

results_matrix <- matrix(nrow = 5, ncol = 2)
rownames(results_matrix) <- c("Total costs", "Total QALYs", "Net benefit", "INB", "Prob CE")
colnames(results_matrix) <- treatment_names
for(treatment in treatment_names) {
  results_matrix[, treatment] <- c(format_results(total_costs[, treatment]),
                                   format_results(total_qalys[, treatment]),
                                   format_results(net_benefit[, treatment]),
                                   format_results(inb[, treatment]),
                                   prob_ce[treatment])
}

write.csv(results_matrix, file = "results/simple_longterm_results.csv")


####### EVPI
# For numerical stability estimate expected NB based on perfect information first
enb_perfect_info <- rep(NA, n_samples)
for (i in 1:n_samples){
  enb_perfect_info[i] <- max(net_benefit[i, "SoC"],net_benefit[i, "Vfrac"])
}

# Take mean and subtract expected NB on current information to get EVPI
EVPI <- mean(enb_perfect_info) - max(colMeans(net_benefit))

# Population size (female age 65+)
base_population = 12390000*0.51

# Population for 10 years
discount = 0.035
population = sum(base_population * ((1 / (1 + discount)^(c(0:9)))))

# Total EVPI
total_EVPI = EVPI*population



