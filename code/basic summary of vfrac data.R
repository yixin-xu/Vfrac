patient_infor = read.csv('results/qalys and costs.csv')

fracture = patient_infor[patient_infor$vfyncode =="Yes",]
mean(fracture$bcosts)
mean(fracture$beq5d_score)*0.25
mean(fracture$fcosts)
mean(fracture$feq5d_score,na.rm = TRUE)*0.25

fracture_gp = patient_infor[patient_infor$vfyncode =="Yes"&patient_infor$mrbackpainbas =="1",]
mean(fracture_gp$bcosts,na.rm = TRUE)
mean(fracture_gp$beq5d_score,na.rm = TRUE)*0.25
mean(fracture_gp$fcosts,na.rm = TRUE)
mean(fracture_gp$feq5d_score,na.rm = TRUE)*0.25

fracture_nogp = patient_infor[patient_infor$vfyncode =="Yes"&patient_infor$mrbackpainbas =="0",]
mean(fracture_nogp$bcosts,na.rm = TRUE)
mean(fracture_nogp$beq5d_score,na.rm = TRUE)*0.25
mean(fracture_nogp$fcosts,na.rm = TRUE)
mean(fracture_nogp$feq5d_score,na.rm = TRUE)*0.25

fracture_vfrac = patient_infor[patient_infor$vfyncode =="Yes"&patient_infor$xvfracscreen =="1",]
mean(fracture_vfrac$bcosts,na.rm = TRUE)
mean(fracture_vfrac$beq5d_score,na.rm = TRUE)*0.25
mean(fracture_vfrac$fcosts,na.rm = TRUE)
mean(fracture_vfrac$feq5d_score,na.rm = TRUE)*0.25

fracture_novfrac = patient_infor[patient_infor$vfyncode =="Yes"&patient_infor$xvfracscreen =="0",]
mean(fracture_novfrac$bcosts,na.rm = TRUE)
mean(fracture_novfrac$beq5d_score,na.rm = TRUE)*0.25
mean(fracture_novfrac$fcosts,na.rm = TRUE)
mean(fracture_novfrac$feq5d_score,na.rm = TRUE)*0.25
