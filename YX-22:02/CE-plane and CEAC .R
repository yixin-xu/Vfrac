library(readxl)
library(dplyr)
library(BCEA)

n_samples = 1000
qalys_standard_raw = read.csv('results/longterm_standard_qalys.csv')
qalys_standard = qalys_standard_raw[2:1001,]
costs_standard_raw = read.csv('results/longterm_standard_costs.csv')
costs_standard = costs_standard_raw[2:1001,]
qalys_vfrac = read.csv('results/longterm_vfrac_qalys.csv')
costs_vfrac = read.csv('results/longterm_vfrac_costs.csv')

inc_costs = costs_vfrac[,2] - costs_standard[,2]
inc_qalys = qalys_vfrac[,2] - qalys_standard[,2]
costs = cbind(costs_standard$x, costs_vfrac$colMeans.random_costs..na.rm...TRUE.)
effects = cbind(qalys_standard$x, qalys_vfrac$colMeans.random_QALYs..na.rm...TRUE.)

setwd('results')
# Plot cost-effectiveness plane using base graphics
jpeg(file = paste0('results', "ce.plane.vfrac.vs standard of care.", n_samples,".jpg"))
vfrac_bcea = bcea(e = effects, c = costs, ref = 2, interventions = c("standard of care", "vfrac"))
ceplane.plot(vfrac_bcea, comparison = 1, wtp = 20000, graph = "base")
dev.off()
dev.new()

# Cost-effectiveness acceptability curve
vfrac_multi_ce = multi.ce(vfrac_bcea)
jpeg(file = paste0('results', "CEAC", n_samples,".jpg"))
mce.plot(vfrac_multi_ce,pos = c(1, 0.5), graph = c("base","ggplot2")) 
dev.off()
dev.new()



          