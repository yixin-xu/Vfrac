# costs and qalys of bootstrap
## Long term standard of care
# Gp+ not diagnosed
bcosts_gp_not = data.frame(matrix(NA, nrow = length(gp_notdiagnosed$xvfracid), ncol = n_samples)) 
fcosts_gp_not = data.frame(matrix(NA, nrow = length(gp_notdiagnosed$xvfracid), ncol = n_samples)) 
bqalys_gp_not = data.frame(matrix(NA, nrow = length(gp_notdiagnosed$xvfracid), ncol = n_samples)) 
fqalys_gp_not = data.frame(matrix(NA, nrow = length(gp_notdiagnosed$xvfracid), ncol = n_samples)) 
rownames(bcosts_gp_not) = rownames(fcosts_gp_not)=rownames(bqalys_gp_not)=rownames(fqalys_gp_not)= gp_notdiagnosed$xvfracid


bcosts_gp_not[unlist(lapply(gp_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$bcosts[bootstrap_samples_gp_notdiagnosed[unlist(lapply(gp_notdiagnosed$xvfracid, toString)),]]
fcosts_gp_not[unlist(lapply(gp_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$fcosts[bootstrap_samples_gp_notdiagnosed[unlist(lapply(gp_notdiagnosed$xvfracid, toString)),]]
bqalys_gp_not[unlist(lapply(gp_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$beq5d_score[bootstrap_samples_gp_notdiagnosed[unlist(lapply(gp_notdiagnosed$xvfracid, toString)),]]*0.25
fqalys_gp_not[unlist(lapply(gp_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$feq5d_score[bootstrap_samples_gp_notdiagnosed[unlist(lapply(gp_notdiagnosed$xvfracid, toString)),]]*0.25

bcosts_gp_not_mean = colMeans(bcosts_gp_not,na.rm = TRUE)
mean(bcosts_gp_not_mean)
quantile(bcosts_gp_not_mean,.025)
quantile(bcosts_gp_not_mean,.975)

fcosts_gp_not_mean = colMeans(fcosts_gp_not,na.rm = TRUE)
mean(fcosts_gp_not_mean)
quantile(fcosts_gp_not_mean,.025)
quantile(fcosts_gp_not_mean,.975)

bqalys_gp_not_mean = colMeans(bqalys_gp_not,na.rm = TRUE)
mean(bqalys_gp_not_mean)
quantile(bqalys_gp_not_mean,.025)
quantile(bqalys_gp_not_mean,.975)

fqalys_gp_not_mean = colMeans(fqalys_gp_not,na.rm = TRUE)
mean(fqalys_gp_not_mean)
quantile(fqalys_gp_not_mean,.025)
quantile(fqalys_gp_not_mean,.975)

##GP+diagnosed
bcosts_gp_dia = data.frame(matrix(NA, nrow = length(gp_diagnosed$xvfracid), ncol = n_samples)) 
fcosts_gp_dia = data.frame(matrix(NA, nrow = length(gp_diagnosed$xvfracid), ncol = n_samples)) 
bqalys_gp_dia = data.frame(matrix(NA, nrow = length(gp_diagnosed$xvfracid), ncol = n_samples)) 
fqalys_gp_dia = data.frame(matrix(NA, nrow = length(gp_diagnosed$xvfracid), ncol = n_samples)) 
rownames(bcosts_gp_dia) = rownames(fcosts_gp_dia)=rownames(bqalys_gp_dia)=rownames(fqalys_gp_dia)= gp_diagnosed$xvfracid


bcosts_gp_dia[unlist(lapply(gp_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$bcosts[bootstrap_samples_gp_diagnosed[unlist(lapply(gp_diagnosed$xvfracid, toString)),]]
fcosts_gp_dia[unlist(lapply(gp_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$fcosts[bootstrap_samples_gp_diagnosed[unlist(lapply(gp_diagnosed$xvfracid, toString)),]]
bqalys_gp_dia[unlist(lapply(gp_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$beq5d_score[bootstrap_samples_gp_diagnosed[unlist(lapply(gp_diagnosed$xvfracid, toString)),]]*0.25
fqalys_gp_dia[unlist(lapply(gp_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$feq5d_score[bootstrap_samples_gp_diagnosed[unlist(lapply(gp_diagnosed$xvfracid, toString)),]]*0.25

bcosts_gp_dia_mean = colMeans(bcosts_gp_dia,na.rm = TRUE)
mean(bcosts_gp_dia_mean)
quantile(bcosts_gp_dia_mean,.025)
quantile(bcosts_gp_dia_mean,.975)

fcosts_gp_dia_mean = colMeans(fcosts_gp_dia,na.rm = TRUE)
mean(fcosts_gp_dia_mean)
quantile(fcosts_gp_dia_mean,.025)
quantile(fcosts_gp_dia_mean,.975)

bqalys_gp_dia_mean = colMeans(bqalys_gp_dia,na.rm = TRUE)
mean(bqalys_gp_dia_mean)
quantile(bqalys_gp_dia_mean,.025)
quantile(bqalys_gp_dia_mean,.975)

fqalys_gp_dia_mean = colMeans(fqalys_gp_not,na.rm = TRUE)
mean(fqalys_gp_dia_mean)
quantile(fqalys_gp_dia_mean,.025)
quantile(fqalys_gp_dia_mean,.975)

## not GP+ not diagnosed
bcosts_notgp_not = data.frame(matrix(NA, nrow = length(notgp_notdiagnosed$xvfracid), ncol = n_samples)) 
fcosts_notgp_not = data.frame(matrix(NA, nrow = length(notgp_notdiagnosed$xvfracid), ncol = n_samples)) 
bqalys_notgp_not = data.frame(matrix(NA, nrow = length(notgp_notdiagnosed$xvfracid), ncol = n_samples)) 
fqalys_notgp_not = data.frame(matrix(NA, nrow = length(notgp_notdiagnosed$xvfracid), ncol = n_samples)) 
rownames(bcosts_notgp_not) = rownames(fcosts_notgp_not)=rownames(bqalys_notgp_not)=rownames(fqalys_notgp_not)= notgp_notdiagnosed$xvfracid


bcosts_notgp_not[unlist(lapply(notgp_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$bcosts[bootstrap_samples_notgp_notdiagnosed[unlist(lapply(notgp_notdiagnosed$xvfracid, toString)),]]
fcosts_notgp_not[unlist(lapply(notgp_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$fcosts[bootstrap_samples_notgp_notdiagnosed[unlist(lapply(notgp_notdiagnosed$xvfracid, toString)),]]
bqalys_notgp_not[unlist(lapply(notgp_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$beq5d_score[bootstrap_samples_notgp_notdiagnosed[unlist(lapply(notgp_notdiagnosed$xvfracid, toString)),]]*0.25
fqalys_notgp_not[unlist(lapply(notgp_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$feq5d_score[bootstrap_samples_notgp_notdiagnosed[unlist(lapply(notgp_notdiagnosed$xvfracid, toString)),]]*0.25

bcosts_notgp_not_mean = colMeans(bcosts_notgp_not,na.rm = TRUE)
mean(bcosts_notgp_not_mean)
quantile(bcosts_notgp_not_mean,.025)
quantile(bcosts_notgp_not_mean,.975)

fcosts_notgp_not_mean = colMeans(fcosts_notgp_not,na.rm = TRUE)
mean(fcosts_notgp_not_mean)
quantile(fcosts_notgp_not_mean,.025)
quantile(fcosts_notgp_not_mean,.975)

bqalys_notgp_not_mean = colMeans(bqalys_notgp_not,na.rm = TRUE)
mean(bqalys_notgp_not_mean,na.rm = TRUE)
quantile(bqalys_notgp_not_mean,.025)
quantile(bqalys_notgp_not_mean,.975)

fqalys_notgp_not_mean = colMeans(fqalys_notgp_not,na.rm = TRUE)
mean(fqalys_notgp_not_mean,na.rm = TRUE)
quantile(fqalys_notgp_not_mean,.025)
quantile(fqalys_notgp_not_mean,.975)


## not GP+ diagnosed
bcosts_notgp_dia = data.frame(matrix(NA, nrow = length(notgp_diagnosed$xvfracid), ncol = n_samples)) 
fcosts_notgp_dia = data.frame(matrix(NA, nrow = length(notgp_diagnosed$xvfracid), ncol = n_samples)) 
bqalys_notgp_dia = data.frame(matrix(NA, nrow = length(notgp_diagnosed$xvfracid), ncol = n_samples)) 
fqalys_notgp_dia = data.frame(matrix(NA, nrow = length(notgp_diagnosed$xvfracid), ncol = n_samples)) 
rownames(bcosts_notgp_dia) = rownames(fcosts_notgp_dia)=rownames(bqalys_notgp_dia)=rownames(fqalys_notgp_dia)= notgp_diagnosed$xvfracid

bcosts_notgp_dia[unlist(lapply(notgp_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$bcosts[bootstrap_samples_notgp_diagnosed[unlist(lapply(notgp_diagnosed$xvfracid, toString)),]]
fcosts_notgp_dia[unlist(lapply(notgp_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$fcosts[bootstrap_samples_notgp_diagnosed[unlist(lapply(notgp_diagnosed$xvfracid, toString)),]]
bqalys_notgp_dia[unlist(lapply(notgp_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$beq5d_score[bootstrap_samples_notgp_diagnosed[unlist(lapply(notgp_diagnosed$xvfracid, toString)),]]*0.25
fqalys_notgp_dia[unlist(lapply(notgp_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$feq5d_score[bootstrap_samples_notgp_diagnosed[unlist(lapply(notgp_diagnosed$xvfracid, toString)),]]*0.25

bcosts_notgp_dia_mean = colMeans(bcosts_notgp_dia,na.rm = TRUE)
mean(bcosts_notgp_dia_mean)
quantile(bcosts_notgp_dia_mean,.025)
quantile(bcosts_notgp_dia_mean,.975)

fcosts_notgp_dia_mean = colMeans(fcosts_notgp_dia,na.rm = TRUE)
mean(fcosts_notgp_dia_mean)
quantile(fcosts_notgp_dia_mean,.025)
quantile(fcosts_notgp_dia_mean,.975)

bqalys_notgp_dia_mean = colMeans(bqalys_notgp_dia,na.rm = TRUE)
mean(bqalys_notgp_dia_mean)
quantile(bqalys_notgp_dia_mean,.025)
quantile(bqalys_notgp_dia_mean,.975)

fqalys_notgp_dia_mean = colMeans(fqalys_notgp_dia,na.rm = TRUE)
mean(fqalys_notgp_dia_mean)
quantile(fqalys_notgp_dia_mean,.025)
quantile(fqalys_notgp_dia_mean,.975)

