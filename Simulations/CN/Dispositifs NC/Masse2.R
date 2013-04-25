##########################  Analyses des masses des Avantages non contributifs ########
### 2e type d'ordre: minima de pensions neutralisés en premier


# Années de référence: 2020
# CdC : DF: 6,4 Md€, 6 Md€ et 4,2 Md€338  OK dans ANCIEN REGIME


#[1] 90087093492 85766845923 81882572796 81842203794
# 3993

# Chargement des données
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load( (paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC_CN2b.RData")))
load( (paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC_2.RData")))

graph_compar(PENREL      ,115,159,"Ratio pension/salaire")  
graph_compar(PENREL_CN      ,115,159,"Ratio pension/salaire")


# MASSE TOTALE DES PENSIONS


# Masse totale des ANC
mtot   <- (MPENS[1,120]-MPENS[4,120])/1e9    
mtot_cn<- (MPENS_CN[1,120]-MPENS_CN[4,120])/1e9
c(mtot,mtot_cn,mtot-mtot_cn)

# Masse des minima de pension
mtotMC   <-(MPENS[1,120]-MPENS[2,120])/1e9   
mtotMC_cn<-(MPENS_CN[1,120]-MPENS_CN[2,120])/1e9    
c(mtotMC,mtotMC_cn,mtotMC-mtotMC_cn)


# Masse des droits familiaux
mtotDF    <-(MPENS[2,120]-MPENS[3,120])/1e9   # Total DF 
mtotDF_cn <-(MPENS_CN[2,120]-MPENS_CN[3,120])/1e9  
c(mtotDF,mtotDF_cn,mtotDF-mtotDF_cn)

# Masse des Périodes assimilées chomage (PA)
mtotPA   <-(MPENS[3,120]-MPENS[4,120])/1e9      
mtotPA_cn<-(MPENS_CN[3,120]-MPENS_CN[4,120])/1e9     
c(mtotPA,mtotPA_cn,mtotPA-mtotPA_cn)    


#Graphique

REF=c(mtotDF,mtotPA,mtotMC)
CN=c(mtotDF_cn,mtotPA_cn,mtotMC_cn)
barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"),legend = c("Droits familiaux","Périodes assimilées","Minima de pension"))
title("Graphe 7: \nMasse des avantages contributif (2020, en Mds)")




# II.  PENSIONS A LIQUIDATION
# Masse totale des ANC
mtotliq   <- (MPENLIQ[1,120]-MPENLIQ[4,120])/1e9    
mtotliq_cn<- (MPENLIQ_CN[1,120]-MPENLIQ_CN[4,120])/1e9
c(mtotliq,mtotliq_cn,mtotliq-mtotliq_cn)

# Masse des minima de pension
mtotliqMC   <-(MPENLIQ[1,120]-MPENLIQ[2,120])/1e9   
mtotliqMC_cn<-(MPENLIQ_CN[1,120]-MPENLIQ_CN[2,120])/1e9    
c(mtotliqMC,mtotliqMC_cn,mtotliqMC-mtotliqMC_cn)


# Masse des droits familiaux
mtotliqDF    <-(MPENLIQ[2,120]-MPENLIQ[3,120])/1e9   # Total DF 
mtotliqDF_cn <-(MPENLIQ_CN[2,120]-MPENLIQ_CN[3,120])/1e9  
c(mtotliqDF,mtotliqDF_cn,mtotliqDF-mtotliqDF_cn)

# Masse des Périodes assimilées chomage (PA)
mtotliqPA   <-(MPENLIQ[3,120]-MPENLIQ[4,120])/1e9      
mtotliqPA_cn<-(MPENLIQ_CN[3,120]-MPENLIQ_CN[4,120])/1e9     
c(mtotliqPA,mtotliqPA_cn,mtotliqPA-mtotliqPA_cn)    


#Graphique
#REF=c(mtotDF,mtotPA,mtotMC)
#CN=c(mtotDF_cn,mtotPA_cn,mtotMC_cn)
REF=c(mtotliqDF,mtotliqPA,mtotliqMC)*5
CN=c(mtotliqDF_cn,mtotliqPA_cn,mtotliqMC_cn)*5
par("mar")
#par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mar=c(5.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
bp <- barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"), ylim=c(0,3))
box()
title("Graphe 7 : Masse des avantages contributifs \n(Flux de liquidants 2020, en milliards)")
legend.text <- c("Droits familiaux","Périodes assimilées","Minima de pension")
legend(mean(range(bp)), -0.5, legend.text,cex=0.9, xjust = 0.5,yjust=1,fill=c("grey0","grey40","grey80"))
