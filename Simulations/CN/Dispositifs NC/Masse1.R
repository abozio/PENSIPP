##########################  Analyses des masses des Avantages non contributifs ########
### 2e type d'ordre: minima de pensions neutralisés en premier
# Chiffres : DF: 17 milliards, Chomage: 10 milliards, MC: 4,4. 

# Années de référence: 2020
# CdC : DF: 6,4 Md€, 6 Md€ et 4,2 Md€338  OK dans ANCIEN REGIME


rm(list = ls())
# Chargement des données
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load( paste0(cheminsource,"Simulations/CN/Dispositifs NC/MasseANC_CN.RData"))
load( paste0(cheminsource,"Simulations/CN/Dispositifs NC/MasseANC.RData"))

mtot        <- numeric(200)
mtot_cn<- numeric(200)
mtotMC<- numeric(200)
mtotMC_cn<- numeric(200)
mtotDF<- numeric(200)
mtotDF_cn<- numeric(200)
mtotMAJ<- numeric(200)
mtotMAJ_cn<- numeric(200)
mtotMDA<- numeric(200)
mtotMDA_cn<- numeric(200)
mtotAVPF<- numeric(200)
mtotAVPF_cn<- numeric(200)
mtotPA<- numeric(200)
mtotPA_cn<- numeric(200)

mtotliq        <- numeric(200)
mtotliq_cn<- numeric(200)
mtotliqMC<- numeric(200)
mtotliqMC_cn<- numeric(200)
mtotliqDF<- numeric(200)
mtotliqDF_cn<- numeric(200)
mtotliqMAJ<- numeric(200)
mtotliqMAJ_cn<- numeric(200)
mtotliqMDA<- numeric(200)
mtotliqMDA_cn<- numeric(200)
mtotliqAVPF<- numeric(200)
mtotliqAVPF_cn<- numeric(200)
mtotliqPA<- numeric(200)
mtotliqPA_cn<- numeric(200)


# MASSE TOTALE DES PENSIONS


# Masse totale des ANC
mtot[]   <- (MPENS[1,]-MPENS[7,])/1e9    
mtot_cn[]<- (MPENS_CN[1,]-MPENS_CN[6,])/1e9


# Masse des minima de pension
mtotMC[]   <-(MPENS[1,]-MPENS[2,])/1e9   
mtotMC_cn[]<-(MPENS_CN[1,]-MPENS_CN[2,])/1e9    

# Masse des droits familiaux
mtotDF[]    <-(MPENS[2,]-MPENS[5,])/1e9   # Total DF 
mtotDF_cn[] <-(MPENS_CN[2,]-MPENS_CN[5,])/1e9  

mtotMAJ[]    <-(MPENS[2,]-MPENS[3,])/1e9   # BONIF
mtotMAJ_cn[] <-(MPENS_CN[2,]-MPENS_CN[3,])/1e9  
mtotMDA[]    <-(MPENS[3,]-MPENS[4,])/1e9   # MDA
mtotMDA_cn[] <-(MPENS_CN[3,]-MPENS_CN[4,])/1e9 
mtotAVPF[]    <-(MPENS[4,]-MPENS[5,])/1e9   # AVPF
mtotMDA_cn[] <-(MPENS_CN[4,]-MPENS_CN[5,])/1e9 

#c(mtotDF,mtotDF_cn,mtotDF-mtotDF_cn)

# Masse des Périodes assimilées chomage (PA)
mtotPA[]   <-(MPENS[5,]-MPENS[7,])/1e9      
mtotPA_cn[]<-(MPENS_CN[5,]-MPENS_CN[6,])/1e9     


#Graphique
# Annee 2020
REF=c(mtotDF[120],mtotPA[120],mtotMC[120])
CN=c(mtotDF_cn[120],mtotPA_cn[120],mtotMC_cn[120])
par("mar")
#par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mar=c(5.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
bp <- barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"), ylim=c(0,35))
box()
title("Graphe 7a : Masse des avantages contributifs \n(Stock des retraité en 2020, en milliards)")
legend.text <- c("Droits familiaux","Périodes assimilées","Minima de pension")
legend(mean(range(bp)), -0.5, legend.text,cex=0.9, xjust = 0.5,yjust=1.5,fill=c("grey0","grey40","grey80"))

# Annee 2035
REF=c(mtotDF[135],mtotPA[135],mtotMC[135])
CN=c(mtotDF_cn[135],mtotPA_cn[135],mtotMC_cn[135])
par("mar")
#par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mar=c(5.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
bp <- barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"), ylim=c(0,60))
box()
title("Graphe 7a : Masse des avantages contributifs \n(Stock des retraité en 2020, en milliards)")
legend.text <- c("Droits familiaux","Périodes assimilées","Minima de pension")
legend(mean(range(bp)), -0.5, legend.text,cex=0.9, xjust = 0.5,yjust=1.5,fill=c("grey0","grey40","grey80"))


#Evolution
par(mar=c(6.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
plot   (seq(2000,2059,by=1),mtot[100:159],xlab="Annee", ylab="Masse stock ANC",ylim=c(0,100),col="grey0",lwd=4,type="l")
points (seq(2000,2059,by=1),mtot_cn[100:159],lwd=4,col="grey80",type="l")
title("Graphe : Evolution des dépenses ANC \n(stock, en milliards)", cex.main = 0.9)
legend.text<-c("Scenario de référence","Scenario CN")
legend("bottom",inset=c(-0.2,-0.55),cex=0.8,legend.text, fill=c("grey0","grey80"))



# II.  PENSIONS A LIQUIDATION
# Masse totale des ANC
mtotliq[]   <- (MPENLIQ[1,]-MPENLIQ[7,])/1e9    
mtotliq_cn[]<- (MPENLIQ_CN[1,]-MPENLIQ_CN[6,])/1e9

# Masse des minima de pension
mtotliqMC[]   <-(MPENLIQ[1,]-MPENLIQ[2,])/1e9   
mtotliqMC_cn[]<-(MPENLIQ_CN[1,]-MPENLIQ_CN[2,])/1e9    

# Masse des droits familiaux
mtotliqDF[]    <-(MPENLIQ[2,]-MPENLIQ[5,])/1e9   # Total DF 
mtotliqDF_cn[]<-(MPENLIQ_CN[2,]-MPENLIQ_CN[5,])/1e9  

mtotliqMAJ[]    <-(MPENLIQ[2,]-MPENLIQ[3,])/1e9   # BONIF
mtotliqMAJ_cn[] <-(MPENLIQ_CN[2,]-MPENLIQ_CN[3,])/1e9  
mtotliqMDA[]    <-(MPENLIQ[3,]-MPENLIQ[4,])/1e9   # MDA
mtotliqMDA_cn[] <-(MPENLIQ_CN[3,]-MPENLIQ_CN[4,])/1e9 
mtotliqAVPF[]    <-(MPENLIQ[4,]-MPENLIQ[5,])/1e9   # AVPF
mtotliqAVPF_cn[] <-(MPENLIQ_CN[4,]-MPENLIQ_CN[5,])/1e9 

# Masse des Périodes assimilées chomage (PA)
mtotliqPA[]  <-(MPENLIQ[5,]-MPENLIQ[7,])/1e9      
mtotliqPA_cn[]<-(MPENLIQ_CN[5,]-MPENLIQ_CN[6,])/1e9     



#Graphique

# Annee 2020
#REF=c(mtotDF,mtotPA,mtotMC)
#CN=c(mtotDF_cn,mtotPA_cn,mtotMC_cn)
REF=c(mtotliqDF[120],mtotliqPA[120],mtotliqMC[120])
CN=c(mtotliqDF_cn[120],mtotliqPA_cn[120],mtotliqMC_cn[120])
par("mar")
#par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mar=c(5.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
bp <- barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"), ylim=c(0,3))
box()
title("Graphique 4.2 : Masse des avantages contributifs \n(Flux de liquidants 2020, en milliards)",cex.main=0.9)
legend.text <- c("Droits familiaux","Périodes assimilées","Minima de pension")
legend(mean(range(bp)), -0.5, legend.text,cex=0.8, xjust = 0.5,yjust=1,fill=c("grey0","grey40","grey80"))


# Annee 2035
#REF=c(mtotDF,mtotPA,mtotMC)
#CN=c(mtotDF_cn,mtotPA_cn,mtotMC_cn)
REF=c(mtotliqDF[135],mtotliqPA[135],mtotliqMC[135])
CN=c(mtotliqDF_cn[135],mtotliqPA_cn[135],mtotliqMC_cn[135])
par("mar")
#par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mar=c(5.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
bp <- barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"), ylim=c(0,3))
box()
title("Graphe 7b : Masse des avantages contributifs \n(Flux de liquidants 2020, en milliards)")
legend.text <- c("Droits familiaux","Périodes assimilées","Minima de pension")
legend(mean(range(bp)), -0.5, legend.text,cex=0.8, xjust = 0.5,yjust=1,fill=c("grey0","grey40","grey80"))

# DF: 
REF=c(mtotliqMAJ[135],mtotliqMDA[135],mtotliqAVPF[120])
CN=c(mtotliqMAJ_cn[120],mtotliqMDA_cn[120],mtotliqAVPF_cn[120])
par("mar")
#par(mar=c(5.1, 4.1, 4.1, 2.1))
par(mar=c(5.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
bp <- barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"), ylim=c(0,3))
box()
title("Graphe 7b : Masse des avantages familiaux\n(Flux de liquidants 2020, en milliards)")
legend.text <- c("Bonfications","MDA","AVPF")
legend(mean(range(bp)), -0.5, legend.text,cex=0.8, xjust = 0.5,yjust=1,fill=c("grey0","grey40","grey80"))


# Evolution
par(mar=c(6.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
plot   (seq(2000,2059,by=1),mtotliq[100:159],xlab="Annee", ylab="Masse stock ANC",ylim=c(0,4),col="grey0",lwd=4,type="l")
points (seq(2000,2059,by=1),mtotliq_cn[100:159],lwd=4,col="grey80",type="l")
title("Graphe : Evolution des dépenses ANC \n(flux de liquidants, en milliards)", cex.main = 0.9)
legend.text<-c("Scenario de référence","Scenario CN")
legend("bottom",inset=c(-0.2,-0.55),cex=0.8,legend.text, fill=c("grey0","grey80"))

par(mar=c(6.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
plot   (seq(2000,2059,by=1),mtotliqMDA[100:159],xlab="Annee", ylab="Masse stock ANC",ylim=c(0,4),col="grey0",lwd=4,type="l")
points (seq(2000,2059,by=1),mtotliqMDA_cn[100:159],lwd=4,col="grey80",type="l")
title("Graphe : Evolution des dépenses MDA\n(flux de liquidants, en milliards)", cex.main = 0.9)
legend.text<-c("Scenario de référence","Scenario CN")
legend("bottom",inset=c(-0.2,-0.55),cex=0.8,legend.text, fill=c("grey0","grey80"))

save.image( paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC.RData"))
mtotliqMDA[150:159]-mtotliqMDA_cn[150:159]
df[150:159]-dfCN[150:159]
