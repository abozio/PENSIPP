##########################  Analyses des masses des Avantages non contributifs ########
# Années de référence: 2020
# CdC : DF: 6,4 Md€, 6 Md€ et 4,2 Md€338  OK dans ANCIEN REGIME


#[1] 90087093492 85766845923 81882572796 81842203794
# 3993

# Chargement des données
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load( (paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC_CN.RData")))
load( (paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC.RData")))
  
# Masse totale des ANC
mtot   <- (MPENS[1,120]-MPENS[7,120])/1e9    
mtot_cn<- (MPENS_CN[1,120]-MPENS_CN[6,120])/1e9
c(mtot,mtot_cn,mtot-mtot_cn)

# Masse des droits familiaux
mtotDF    <-(MPENS[1,120]-MPENS[4,120])/1e9   # Total DF 
mtotDF_cn <-(MPENS_CN[1,120]-MPENS_CN[4,120])/1e9  
c(mtotDF,mtotDF_cn,mtotDF-mtotDF_cn)
mtotMAJ   <-(MPENS[1,120]-MPENS[2,120])/1e9    # MAJ
mtotMAJ_cn<-(MPENS_CN[1,120]-MPENS_CN[2,120])/1e9   
c(mtotMAJ,mtotMAJ_cn,mtotMAJ-mtotMAJ_cn)       
mtotMDA   <-(MPENS[2,120]-MPENS[3,120])/1e9    #MDA
mtotMDA_cn<-(MPENS_CN[2,120]-MPENS_CN[3,120])/1e9   
c(mtotMDA,mtotMDA_cn,mtotMDA-mtotMDA_cn)
mtotAVPF   <-(MPENS[3,120]-MPENS[4,120])/1e9    #MDA
mtotAVPF_cn<-(MPENS_CN[3,120]-MPENS_CN[4,120])/1e9   
c(mtotAVPF,mtotAVPF_cn,mtotAVPF-mtotAVPF_cn)

# Masse des Périodes assimilées chomage (PA)
mtotPA   <-(MPENS[4,120]-MPENS[6,120])/1e9      
mtotPA_cn<-(MPENS_CN[4,120]-MPENS_CN[5,120])/1e9     
c(mtotPA,mtotPA_cn,mtotPA-mtotPA_cn)    

# Masse des minima de pension
mtotMC   <-(MPENS[6,120]-MPENS[7,120])/1e9   
mtotMC_cn<-(MPENS_CN[5,120]-MPENS_CN[6,120])/1e9    
c(mtotMC,mtotMC_cn,mtotMC-mtotMC_cn)

mtotDF+mtotPA+mtot

#Graphique

#ggplot(df, aes(experiment, value, fill=metric)) + geom_bar(position="dodge")

REF=c(mtotDF,mtotPA,mtotMC)
CN=c(mtotDF_cn,mtotPA_cn,mtotMC_cn)
barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"),legend = c("Droits familiaux","Périodes assimilées","Minima de pension"))
title("Masse des avantages contributif (2020, en Mds)")
