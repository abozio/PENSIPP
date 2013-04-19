##########################  Analyses des masses des Avantages non contributifs ########
#MPENS[,120]
#[1] 90087093492 85766845923 81882572796 81842203794

# Années de référence: 2020
load("~/Desktop/PENSIPP 0.1/Simulations/CN/Dispositifs NC/ANC_CN.RData")
MPENS_CN  <- MPENS[2:7,]
PENREL_CN <- PENREL
load("~/Desktop/PENSIPP 0.1/Simulations/CN/Dispositifs NC/ANC.RData")
  
mtot   <-MPENS[1,120]-MPENS[7,120]    # masse totale des ANC
mtot_cn<-MPENS_CN[1,120]-MPENS_CN[6,120]   
c(mtot,mtot_cn,mtot-mtot_cn)

mtotDF   <-MPENS[1,120]-MPENS[4,120]    # masse des droits familiaux
mtotDF_cn<-MPENS_CN[1,120]-MPENS_CN[4,120]   
c(mtotDF,mtotDF_cn,mtotDF-mtotDF_cn)
mtotPA   <-MPENS[4,120]-MPENS[6,120]    # masse des PA
mtotPA_cn<-MPENS_CN[4,120]-MPENS_CN[5,120]  
c(mtotPA,mtotPA_cn,mtotPA-mtotPA_cn)
mtotMC   <-MPENS[6,120]-MPENS[7,120]    # masse des minima de pension
mtotMC_cn<-MPENS_CN[5,120]-MPENS_CN[6,120]  
c(mtotMC,mtotMC_cn,mtotMC-mtotMC_cn)

