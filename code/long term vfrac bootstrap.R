# costs and qalys of bootstrap
## Long term vfrac
# vfrac+ not diagnosed
bcosts_re_not = data.frame(matrix(NA, nrow = length(recommended_notdiagnosed$xvfracid), ncol = n_samples)) 
fcosts_re_not = data.frame(matrix(NA, nrow = length(recommended_notdiagnosed$xvfracid), ncol = n_samples)) 
bqalys_re_not = data.frame(matrix(NA, nrow = length(recommended_notdiagnosed$xvfracid), ncol = n_samples)) 
fqalys_re_not = data.frame(matrix(NA, nrow = length(recommended_notdiagnosed$xvfracid), ncol = n_samples)) 
rownames(bcosts_re_not) = rownames(fcosts_re_not)=rownames(bqalys_re_not)=rownames(fqalys_re_not)= recommended_notdiagnosed$xvfracid


bcosts_re_not[unlist(lapply(recommended_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$bcosts[bootstrap_samples_recommended_notdiagnosed[unlist(lapply(recommended_notdiagnosed$xvfracid, toString)),]]
fcosts_re_not[unlist(lapply(recommended_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$fcosts[bootstrap_samples_recommended_notdiagnosed[unlist(lapply(recommended_notdiagnosed$xvfracid, toString)),]]
bqalys_re_not[unlist(lapply(recommended_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$beq5d_score[bootstrap_samples_recommended_notdiagnosed[unlist(lapply(recommended_notdiagnosed$xvfracid, toString)),]]*0.25
fqalys_re_not[unlist(lapply(recommended_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$feq5d_score[bootstrap_samples_recommended_notdiagnosed[unlist(lapply(recommended_notdiagnosed$xvfracid, toString)),]]*0.25

bcosts_re_not_mean = colMeans(bcosts_re_not,na.rm = TRUE)
mean(bcosts_re_not_mean)
quantile(bcosts_re_not_mean,.025)
quantile(bcosts_re_not_mean,.975)

fcosts_re_not_mean = colMeans(fcosts_re_not,na.rm = TRUE)
mean(fcosts_re_not_mean)
quantile(fcosts_re_not_mean,.025)
quantile(fcosts_re_not_mean,.975)

bqalys_re_not_mean = colMeans(bqalys_re_not,na.rm = TRUE)
mean(bqalys_re_not_mean)
quantile(bqalys_re_not_mean,.025)
quantile(bqalys_re_not_mean,.975)

fqalys_re_not_mean = colMeans(fqalys_re_not,na.rm = TRUE)
mean(fqalys_re_not_mean)
quantile(fqalys_re_not_mean,.025)
quantile(fqalys_re_not_mean,.975)

##vfrac+diagnosed
bcosts_re_dia = data.frame(matrix(NA, nrow = length(recommended_diagnosed$xvfracid), ncol = n_samples)) 
fcosts_re_dia = data.frame(matrix(NA, nrow = length(recommended_diagnosed$xvfracid), ncol = n_samples)) 
bqalys_re_dia = data.frame(matrix(NA, nrow = length(recommended_diagnosed$xvfracid), ncol = n_samples)) 
fqalys_re_dia = data.frame(matrix(NA, nrow = length(recommended_diagnosed$xvfracid), ncol = n_samples)) 
rownames(bcosts_re_dia) = rownames(fcosts_re_dia)=rownames(bqalys_re_dia)=rownames(fqalys_re_dia)= recommended_diagnosed$xvfracid


bcosts_re_dia[unlist(lapply(recommended_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$bcosts[bootstrap_samples_recommended_diagnosed[unlist(lapply(recommended_diagnosed$xvfracid, toString)),]]
fcosts_re_dia[unlist(lapply(recommended_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$fcosts[bootstrap_samples_recommended_diagnosed[unlist(lapply(recommended_diagnosed$xvfracid, toString)),]]
bqalys_re_dia[unlist(lapply(recommended_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$beq5d_score[bootstrap_samples_recommended_diagnosed[unlist(lapply(recommended_diagnosed$xvfracid, toString)),]]*0.25
fqalys_re_dia[unlist(lapply(recommended_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$feq5d_score[bootstrap_samples_recommended_diagnosed[unlist(lapply(recommended_diagnosed$xvfracid, toString)),]]*0.25

bcosts_re_dia_mean = colMeans(bcosts_re_dia,na.rm = TRUE)
mean(bcosts_re_dia_mean)
quantile(bcosts_re_dia_mean,.025)
quantile(bcosts_re_dia_mean,.975)

fcosts_re_dia_mean = colMeans(fcosts_re_dia,na.rm = TRUE)
mean(fcosts_re_dia_mean)
quantile(fcosts_re_dia_mean,.025)
quantile(fcosts_re_dia_mean,.975)

bqalys_re_dia_mean = colMeans(bqalys_re_dia,na.rm = TRUE)
mean(bqalys_re_dia_mean)
quantile(bqalys_re_dia_mean,.025)
quantile(bqalys_re_dia_mean,.975)

fqalys_re_dia_mean = colMeans(fqalys_re_not,na.rm = TRUE)
mean(fqalys_re_dia_mean)
quantile(fqalys_re_dia_mean,.025)
quantile(fqalys_re_dia_mean,.975)

## not vfrac+ not diagnosed
bcosts_notre_not = data.frame(matrix(NA, nrow = length(notrecommended_notdiagnosed$xvfracid), ncol = n_samples)) 
fcosts_notre_not = data.frame(matrix(NA, nrow = length(notrecommended_notdiagnosed$xvfracid), ncol = n_samples)) 
bqalys_notre_not = data.frame(matrix(NA, nrow = length(notrecommended_notdiagnosed$xvfracid), ncol = n_samples)) 
fqalys_notre_not = data.frame(matrix(NA, nrow = length(notrecommended_notdiagnosed$xvfracid), ncol = n_samples)) 
rownames(bcosts_notre_not) = rownames(fcosts_notre_not)=rownames(bqalys_notre_not)=rownames(fqalys_notre_not)= notrecommended_notdiagnosed$xvfracid


bcosts_notre_not[unlist(lapply(notrecommended_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$bcosts[bootstrap_samples_notrecommended_notdiagnosed[unlist(lapply(notrecommended_notdiagnosed$xvfracid, toString)),]]
fcosts_notre_not[unlist(lapply(notrecommended_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$fcosts[bootstrap_samples_notrecommended_notdiagnosed[unlist(lapply(notrecommended_notdiagnosed$xvfracid, toString)),]]
bqalys_notre_not[unlist(lapply(notrecommended_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$beq5d_score[bootstrap_samples_notrecommended_notdiagnosed[unlist(lapply(notrecommended_notdiagnosed$xvfracid, toString)),]]*0.25
fqalys_notre_not[unlist(lapply(notrecommended_notdiagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$feq5d_score[bootstrap_samples_notrecommended_notdiagnosed[unlist(lapply(notrecommended_notdiagnosed$xvfracid, toString)),]]*0.25

bcosts_notre_not_mean = colMeans(bcosts_notre_not,na.rm = TRUE)
mean(bcosts_notre_not_mean)
quantile(bcosts_notre_not_mean,.025)
quantile(bcosts_notre_not_mean,.975)

fcosts_notre_not_mean = colMeans(fcosts_notre_not,na.rm = TRUE)
mean(fcosts_notre_not_mean)
quantile(fcosts_notre_not_mean,.025)
quantile(fcosts_notre_not_mean,.975)

bqalys_notre_not_mean = colMeans(bqalys_notre_not,na.rm = TRUE)
mean(bqalys_notre_not_mean,na.rm = TRUE)
quantile(bqalys_notre_not_mean,.025)
quantile(bqalys_notre_not_mean,.975)

fqalys_notre_not_mean = colMeans(fqalys_notre_not,na.rm = TRUE)
mean(fqalys_notre_not_mean,na.rm = TRUE)
quantile(fqalys_notre_not_mean,.025)
quantile(fqalys_notre_not_mean,.975)

## not vfrac+ diagnosed
bcosts_notre_dia = data.frame(matrix(NA, nrow = length(notrecommended_diagnosed$xvfracid), ncol = n_samples)) 
fcosts_notre_dia = data.frame(matrix(NA, nrow = length(notrecommended_diagnosed$xvfracid), ncol = n_samples)) 
bqalys_notre_dia = data.frame(matrix(NA, nrow = length(notrecommended_diagnosed$xvfracid), ncol = n_samples)) 
fqalys_notre_dia = data.frame(matrix(NA, nrow = length(notrecommended_diagnosed$xvfracid), ncol = n_samples)) 
rownames(bcosts_notre_dia) = rownames(fcosts_notre_dia)=rownames(bqalys_notre_dia)=rownames(fqalys_notre_dia)= notrecommended_diagnosed$xvfracid

bcosts_notre_dia[unlist(lapply(notrecommended_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$bcosts[bootstrap_samples_notrecommended_diagnosed[unlist(lapply(notrecommended_diagnosed$xvfracid, toString)),]]
fcosts_notre_dia[unlist(lapply(notrecommended_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$fcosts[bootstrap_samples_notrecommended_diagnosed[unlist(lapply(notrecommended_diagnosed$xvfracid, toString)),]]
bqalys_notre_dia[unlist(lapply(notrecommended_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$beq5d_score[bootstrap_samples_notrecommended_diagnosed[unlist(lapply(notrecommended_diagnosed$xvfracid, toString)),]]*0.25
fqalys_notre_dia[unlist(lapply(notrecommended_diagnosed$xvfracid, toString)), 1:1000] = 
  patient_infor$feq5d_score[bootstrap_samples_notrecommended_diagnosed[unlist(lapply(notrecommended_diagnosed$xvfracid, toString)),]]*0.25

bcosts_notre_dia_mean = colMeans(bcosts_notre_dia,na.rm = TRUE)
mean(bcosts_notre_dia_mean)
quantile(bcosts_notre_dia_mean,.025)
quantile(bcosts_notre_dia_mean,.975)

fcosts_notre_dia_mean = colMeans(fcosts_notre_dia,na.rm = TRUE)
mean(fcosts_notre_dia_mean)
quantile(fcosts_notre_dia_mean,.025)
quantile(fcosts_notre_dia_mean,.975)

bqalys_notre_dia_mean = colMeans(bqalys_notre_dia,na.rm = TRUE)
mean(bqalys_notre_dia_mean)
quantile(bqalys_notre_dia_mean,.025)
quantile(bqalys_notre_dia_mean,.975)

fqalys_notre_dia_mean = colMeans(fqalys_notre_dia,na.rm = TRUE)
mean(fqalys_notre_dia_mean)
quantile(fqalys_notre_dia_mean,.025)
quantile(fqalys_notre_dia_mean,.975)

