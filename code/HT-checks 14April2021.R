mean(gp_notdiagnosed$feq5d_score, na.rm=TRUE)
#[1] 0.5466737
mean(gp_diagnosed$feq5d_score, na.rm=TRUE)
#[1] 0.4986
 mean(notgp_diagnosed$feq5d_score, na.rm=TRUE)
#[1] 0.5854783
 mean(notgp_notdiagnosed$feq5d_score, na.rm=TRUE)
#[1] 0.6500337


 mean(gp_notdiagnosed$beq5d_score, na.rm=TRUE)
# 0.5350104
 mean(gp_diagnosed$beq5d_score, na.rm=TRUE)
# 0.5399333
 mean(notgp_diagnosed$beq5d_score, na.rm=TRUE)
# 0.6250898
 mean(notgp_notdiagnosed$beq5d_score, na.rm=TRUE)
# 0.662194
 

 mean(patient_infor[is.element(patient_infor$xvfracid, randomsample_notreferred),"feq5d_score"])
#[1] 0.5389659

 mean(patient_infor[is.element(patient_infor$xvfracid, randomsample),"feq5d_score"], na.rm=TRUE)
#[1] 0.5447273


mean(patient_infor[is.element(patient_infor$xvfracid, notgp_xvfracid),"feq5d_score"], na.rm=TRUE)
gp_xvfracid 