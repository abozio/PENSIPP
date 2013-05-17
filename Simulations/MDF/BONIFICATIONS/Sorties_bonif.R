###### Analyse des résultats: réforme des bonifications pour pension ######


###### # I. Analyse du dispositif existant:  #####

# Chargement des résultats: 
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/MDF/ANC.RData"))

# 1. Masse des pensions: 
mtotMAJ       <- numeric(200)
mtotliqMAJ    <- numeric(200)
mtotMAJ[]     <-(MPENS[1,]-MPENS[2,])/1e9       
mtotliqMAJ[]  <-(MPENLIQ[1,]-MPENLIQ[2,])/1e9   
# Valeurs en 2004 et 2006: 
mtotMAJ[104]
mtotMAJ[106]

# 2. Analyse des bénéficiaires du dispositif:  
gains     <- numeric(taille_max)  # gain individuel au dispositif (valeur)
gains_pct <- numeric(taille_max)  # gain individuel au dispositif (%)
gains     <- pliq_[,1]-pliq_[,2] 
gains_pct <- (pliq_[,1]-pliq_[,2])/pliq_[,2]

BENEF     <- numeric(200)   # % de bénéficiaires par génération
GAINS     <- numeric(200)   # Gains moyen des bénéficiaires pas génération (valeur)
GAINSH    <- numeric(200)
GAINSF    <- numeric(200)
GAINSPCT  <- numeric(200)   # Gains moyen des bénéficiaires pas génération (%)
GAINSHPCT  <- numeric(200)
GAINSFPCT <- numeric(200)

for (g in 30:90)
{
  # % Beneficiaires: 
  list <-which(pliq_[,1]>pliq_[,2] & t_liq[] <999 & t_naiss==g)
  list1<-which(t_liq[]<999 & t_naiss==g)
  BENEF[g]<-length(list)/length(list1)
  
  # Gains
  GAINS[g] <- mean(gains[which(pliq_[,1]>pliq_[,2] & t_liq[] <999 & t_naiss==g)])
  GAINSH[g]<- mean(gains[which(pliq_[,1]>pliq_[,2] & t_liq[] <999 & t_naiss==g & sexe==1)])
  GAINSF[g]<- mean(gains[which(pliq_[,1]>pliq_[,2] & t_liq[] <999 & t_naiss==g & sexe==2)])
  
  GAINSPCT[g] <- mean(gains_pct[which(pliq_[,1]>pliq_[,2] & pliq_[,2]>0 & t_liq[] <999 & t_naiss==g)])
  GAINSHPCT[g]<- mean(gains_pct[which(pliq_[,1]>pliq_[,2] & pliq_[,2]>0& t_liq[] <999 & t_naiss==g & sexe==1)])
  GAINSFPCT[g]<- mean(gains_pct[which(pliq_[,1]>pliq_[,2] & pliq_[,2]>0& t_liq[] <999 & t_naiss==g & sexe==2)])
}

# % de bénéficiaires
mean(BENEF[30:40])

# Bonifications moyennes pour les générations 1934 à 1938: 
mean(GAINS[34:38])
mean(GAINSH[34:38])
mean(GAINSF[34:38])
mean(GAINSHPCT[34:38])
mean(GAINSFPCT[34:38])


# 3. Rapport pension à liquidation moyenne Homme/femme par génération:

# Comparaison avec/sans dispositif:
RATIO_HF_1 <- numeric(200) # Scénario de référence
RATIO_HF_2 <- numeric(200) # No Bonif

for (g in 30:90)
{
  pensionH1 <- mean(pliq_[which(sexe==1 & t_naiss==g & t_liq<999),1])
  pensionF1 <- mean(pliq_[which(sexe==2 & t_naiss==g & t_liq<999),1])
  RATIO_HF_1[g]<- pensionF1/pensionH1
  
  pensionH2 <- mean(pliq_[which(sexe==1 & t_naiss==g & t_liq<999),2])
  pensionF2 <- mean(pliq_[which(sexe==2 & t_naiss==g & t_liq<999),2])
  RATIO_HF_2[g]<- pensionF2/pensionH2
}
# MOYENNE MOBILE 5G
MMRATIO_HF_1 <- numeric(200) # Scénario de référence
MMRATIO_HF_2 <- numeric(200) # NoBonif

for (g in 33:87)
{
  MMRATIO_HF_1[g]<-(RATIO_HF_1[g-2]+RATIO_HF_1[g-1]+RATIO_HF_1[g]+
                      RATIO_HF_1[g+1]+RATIO_HF_1[g+2])/5  
  MMRATIO_HF_2[g]<-(RATIO_HF_2[g-2]+RATIO_HF_2[g-1]+RATIO_HF_2[g]+
                      RATIO_HF_2[g+1]+RATIO_HF_2[g+2])/5  
}


# graphique
par(mar=c(5.1, 2.1, 2.1, 1.1))
plot   (seq(1933,1985,by=1),MMRATIO_HF_1[33:85],
        xlab="Année de naissance", ylab=NULL,ylim=c(0.5,1),
        col="grey80",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
points (seq(1933,1985,by=1),MMRATIO_HF_2[33:85],lwd=2,col="grey0",type="l",yaxs="i",xaxs="i")
legend.text<-c("Système actuel","No Bonif")
legend("topleft",legend.text, fill=c("grey80","grey0"), cex =0.6)




####### II. Analyse de la réforme proposée: ########

# Chargement des résultats: 
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/MDF/simul_v1.RData"))



# Controle: pas de modification pour ceux avec moins de 3 enfants
mean(pliq_[list2,1]-pliq_[list2,2])

# Rapport pension à liquidation moyenne Homme/femme par génération:
RATIO_HF_1 <- numeric(200) # Scénario de référence
RATIO_HF_2 <- numeric(200) # Majoration forfaitaire (index: pensions)
RATIO_HF_3 <- numeric(200) # Majoration forfaitaire  (index: inflation)

for (g in 35:90)
{
pensionH1 <- mean(pliq_[which(sexe==1 & t_naiss==g & t_liq<999),1])
pensionF1 <- mean(pliq_[which(sexe==2 & t_naiss==g & t_liq<999),1])
RATIO_HF_1[g]<- pensionF1/pensionH1

pensionH2 <- mean(pliq_[which(sexe==1 & t_naiss==g & t_liq<999),2])
pensionF2 <- mean(pliq_[which(sexe==2 & t_naiss==g & t_liq<999),2])
RATIO_HF_2[g]<- pensionF2/pensionH2

pensionH3 <- mean(pliq_[which(sexe==1 & t_naiss==g & t_liq<999),3])
pensionF3 <- mean(pliq_[which(sexe==2 & t_naiss==g & t_liq<999),3])
RATIO_HF_3[g]<- pensionF3/pensionH3
}
# MOYENNE MOBILE 5G
MMRATIO_HF_1 <- numeric(200) # Scénario de référence
MMRATIO_HF_2 <- numeric(200) # Majoration forfaitaire (index: pensions)
MMRATIO_HF_3 <- numeric(200) # Majoration forfaitaire  (index: inflation)
for (g in 40:85)
{
MMRATIO_HF_1[g]<-(RATIO_HF_1[g-2]+RATIO_HF_1[g-1]+RATIO_HF_1[g]+
                    RATIO_HF_1[g+1]+RATIO_HF_1[g+2])/5  
MMRATIO_HF_2[g]<-(RATIO_HF_2[g-2]+RATIO_HF_2[g-1]+RATIO_HF_2[g]+
                    RATIO_HF_2[g+1]+RATIO_HF_2[g+2])/5  
MMRATIO_HF_3[g]<-(RATIO_HF_3[g-2]+RATIO_HF_3[g-1]+RATIO_HF_3[g]+
                    RATIO_HF_3[g+1]+RATIO_HF_3[g+2])/5  
}


# graphique
par(mar=c(5.1, 2.1, 2.1, 1.1))
plot   (seq(1940,1985,by=1),MMRATIO_HF_1[40:85],
        xlab="Année de naissance", ylab=NULL,ylim=c(0.5,1),
        col="grey80",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
points (seq(1940,1985,by=1),MMRATIO_HF_2[40:85],lwd=2,col="grey40",type="l",yaxs="i",xaxs="i")
points (seq(1940,1985,by=1),MMRATIO_HF_3[40:85],lwd=2,col="grey0",type="l",yaxs="i",xaxs="i")
legend.text<-c("Système actuel","Réforme 1","Réforme 2")
legend("topleft",legend.text, fill=c("grey80","grey40","grey0"), cex =0.6)

# 2. Analyse gagnants/perdants

# Liste des bénéficiaires: 
list<-liquidants[which(n_enf[liquidants]>2)] 
listH<-liquidants[which(sexe[liquidants]==1 & n_enf[liquidants]>2)] 
listF<-liquidants[which(sexe[liquidants]==2 & n_enf[liquidants]>2)] 
# Remove bizarre: pliq_[,1]=0 
a <- list[pliq_[list,1]==0]
list <- setdiff(list,a)
listH <- setdiff(listH,a)
listF <- setdiff(listF,a)

# Gains moyens à la réforme: 
mean(pliq_[list,2]-pliq_[list,1])
mean(pliq_[listH,2]-pliq_[listH,1])
mean(pliq_[listF,2]-pliq_[listF,1])
mean(pliq_[list,3]-pliq_[list,1])
mean(pliq_[listH,3]-pliq_[listH,1])
mean(pliq_[listF,3]-pliq_[listF,1])

# En pourcentage: 
gain1<-(pliq_[,2]-pliq_[,1])/pliq_[,1]
gain2<-(pliq_[,3]-pliq_[,1])/pliq_[,1]

mean(gain1[list])
mean(gain1[listH])
mean(gain1[listF])
mean(gain2[list])


