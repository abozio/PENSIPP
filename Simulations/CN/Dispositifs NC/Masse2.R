##########################  Analyses des masses des Avantages non contributifs ########
### 2e type d'ordre: minima de pensions neutralisés en premier


# Années de référence: 2020
# CdC : DF: 6,4 Md€, 6 Md€ et 4,2 Md€338  OK dans ANCIEN REGIME


#[1] 90087093492 85766845923 81882572796 81842203794
# 3993

# Chargement des données
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load( (paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC_CN.RData")))
load( (paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC_2.RData")))

graph_compar(PENREL      ,115,159,"Ratio pension/salaire")  
graph_compar(PENREL_CN      ,115,159,"Ratio pension/salaire")


# MASSE TOTALE DES PENSIONS


# Masse totale des ANC
mtot   <- (MPENS[1,120]-MPENS[7,120])/1e9    
mtot_cn<- (MPENS_CN[1,120]-MPENS_CN[6,120])/1e9
c(mtot,mtot_cn,mtot-mtot_cn)

# Masse des minima de pension
mtotMC   <-(MPENS[1,120]-MPENS[2,120])/1e9   
mtotMC_cn<-(MPENS_CN[5,120]-MPENS_CN[6,120])/1e9    
c(mtotMC,mtotMC_cn,mtotMC-mtotMC_cn)


# Masse des droits familiaux
mtotDF    <-(MPENS[2,120]-MPENS[5,120])/1e9   # Total DF 
mtotDF_cn <-(MPENS_CN[1,120]-MPENS_CN[4,120])/1e9  
c(mtotDF,mtotDF_cn,mtotDF-mtotDF_cn)
mtotMAJ   <-(MPENS[2,120]-MPENS[3,120])/1e9    # MAJ
mtotMAJ_cn<-(MPENS_CN[1,120]-MPENS_CN[2,120])/1e9   
c(mtotMAJ,mtotMAJ_cn,mtotMAJ-mtotMAJ_cn)       
mtotMDA   <-(MPENS[3,120]-MPENS[4,120])/1e9    #MDA
mtotMDA_cn<-(MPENS_CN[2,120]-MPENS_CN[3,120])/1e9   
c(mtotMDA,mtotMDA_cn,mtotMDA-mtotMDA_cn)
mtotAVPF   <-(MPENS[4,120]-MPENS[5,120])/1e9    #MDA
mtotAVPF_cn<-(MPENS_CN[3,120]-MPENS_CN[4,120])/1e9   
c(mtotAVPF,mtotAVPF_cn,mtotAVPF-mtotAVPF_cn)

# Masse des Périodes assimilées chomage (PA)
mtotPA   <-(MPENS[5,120]-MPENS[7,120])/1e9      
mtotPA_cn<-(MPENS_CN[4,120]-MPENS_CN[5,120])/1e9     
c(mtotPA,mtotPA_cn,mtotPA-mtotPA_cn)    


mtotDF+mtotPA+mtot


#Graphique

#ggplot(df, aes(experiment, value, fill=metric)) + geom_bar(position="dodge")

REF=c(mtotDF,mtotPA,mtotMC)
CN=c(mtotDF_cn,mtotPA_cn,mtotMC_cn)
barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"),legend = c("Droits familiaux","Périodes assimilées","Minima de pension"))
title("Masse des avantages contributif (2020, en Mds)")




# II.  PENSIONS A LIQUIDATION


# Masse totale des ANC
mtot   <- (MPENLIQ[1,120]-MPENLIQ[7,120])/1e9    
mtot_cn<- (MPENLIQ_CN[1,120]-MPENLIQ_CN[6,120])/1e9
c(mtot,mtot_cn,mtot-mtot_cn)

# Masse des minima de pension
mtotMC   <-(MPENLIQ[1,120]-MPENLIQ[2,120])/1e9   
mtotMC_cn<-(MPENLIQ_CN[5,120]-MPENLIQ_CN[6,120])/1e9    
c(mtotMC,mtotMC_cn,mtotMC-mtotMC_cn)


# Masse des droits familiaux
mtotDF    <-(MPENLIQ[2,120]-MPENLIQ[5,120])/1e9   # Total DF 
mtotDF_cn <-(MPENLIQ_CN[1,120]-MPENLIQ_CN[4,120])/1e9  
c(mtotDF,mtotDF_cn,mtotDF-mtotDF_cn)
mtotMAJ   <-(MPENLIQ[2,120]-MPENLIQ[3,120])/1e9    # MAJ
mtotMAJ_cn<-(MPENLIQ_CN[1,120]-MPENLIQ_CN[2,120])/1e9   
c(mtotMAJ,mtotMAJ_cn,mtotMAJ-mtotMAJ_cn)       
mtotMDA   <-(MPENLIQ[3,120]-MPENLIQ[4,120])/1e9    #MDA
mtotMDA_cn<-(MPENLIQ_CN[2,120]-MPENLIQ_CN[3,120])/1e9   
c(mtotMDA,mtotMDA_cn,mtotMDA-mtotMDA_cn)
mtotAVPF   <-(MPENLIQ[4,120]-MPENLIQ[5,120])/1e9    #MDA
mtotAVPF_cn<-(MPENLIQ_CN[3,120]-MPENLIQ_CN[4,120])/1e9   
c(mtotAVPF,mtotAVPF_cn,mtotAVPF-mtotAVPF_cn)

# Masse des Périodes assimilées chomage (PA)
mtotPA   <-(MPENLIQ[5,120]-MPENLIQ[7,120])/1e9      
mtotPA_cn<-(MPENLIQ_CN[4,120]-MPENLIQ_CN[5,120])/1e9     
c(mtotPA,mtotPA_cn,mtotPA-mtotPA_cn)    


mtotDF+mtotPA+mtot


#Graphique

#ggplot(df, aes(experiment, value, fill=metric)) + geom_bar(position="dodge")

REF=c(mtotDF,mtotPA,mtotMC)
CN=c(mtotDF_cn,mtotPA_cn,mtotMC_cn)
barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"),legend = c("Droits familiaux","Périodes assimilées","Minima de pension"))
title("Masse des avantages contributif (2020, en Mds)")

mtotDF+mtotPA+mtot

#Graphique

#ggplot(df, aes(experiment, value, fill=metric)) + geom_bar(position="dodge")

REF=c(mtotDF,mtotPA,mtotMC)
CN=c(mtotDF_cn,mtotPA_cn,mtotMC_cn)
barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"))
legend (1,0.5,c("Droits familiaux","Périodes assimilées","Minima de pension"))
title("Masse des avantages contributif (2020, en Mds)")

par("mar")
#par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mar=c(6.1, 4.1, 4.1, 2.1))
par(xpd=TRUE)
bp <- barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"))
legend.text <- c("Droits familiaux","Périodes assimilées","Minima de pension")
legend(mean(range(bp)), -0.5, legend.text, xjust = 0.5,fill=c("grey0","grey40","grey80"))
