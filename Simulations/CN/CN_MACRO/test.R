######### Analyse des résultats obtenus #######

rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/CN/CN_MICRO/micro2.RData"))


# EXPLIQUER LECART ENTRE LES DEUX POUR LE NIVEAU DE PENSION: 
# Ecart de pensions à liquidations => écarts de ratio 
# (écart de masse de pension finale effet cumulé des différences de pensions à liquidation)
# Ecart de pension à liquidation: 
diffpliq<-(PENLIQMOY[1,137:155]-PENLIQMOY[2,137:155])*FLUXLIQ[1,137:155]
sum(diffpliq)/1e9
(MPENS[1,155]-MPENS[2,155])/1e9


# Ageliq
par(oma=c(0,0,0,0))
par(mfrow=c(1,1))
par(mar=c(2.1, 2.1, 1.5, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),AGELIQ[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(min(AGELIQ[1,110:159]),max(AGELIQ[1,110:159])),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),AGELIQ[2,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(0.11,0.13),col="grey80",lwd=4,type="l")
legend.text <- c("Scénario de référence","CN")
legend("topleft",cex=0.8,legend.text, fill=c("grey0","grey80"))
abline (h=63)

plot   (seq(2009,2059,by=1),AGELIQ[1,109:159],xlab="Annee", ylab="age de départ en retraite",
        ylim=c(60,66),col="grey0",lwd=4,type="l")
abline(h=65)
abline(h=63)
abline(h=64)
abline(h=66)
abline(h=62)
abline(h=61)
abline(h=60)
title("PENSIPP: Ages de liquidation par année")

AGELIQGMOY<-numeric(200)
for (g in 20:90)
{
AGELIQGMOY[g]<-mean(ageliq[t_naiss==g & t_liq<999])  
} 
plot   (seq(1920,1990,by=1),AGELIQGMOY[20:90],xlab="Génération", ylab="age de départ en retraite",
        ylim=c(60,66),col="grey0",lwd=4,type="l")
abline(h=65)
abline(h=63)
abline(h=64)
abline(h=66)
abline(h=62)
abline(h=61)
abline(h=60)
title("PENSIPP: Ages de liquidation par génération")



# Mpension
plot   (seq(2010,2059,by=1),MPENS[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(min(MPENS[1,110:159]),max(MPENS[1,110:159])),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),MPENS[2,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(0.11,0.13),col="grey80",lwd=4,type="l")
legend.text <- c("Scénario de référence","CN")
legend("topleft",cex=0.8,legend.text, fill=c("grey0","grey80")

# Ratio
plot   (seq(2010,2059,by=1),RATIOPENS[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(min(RATIOPENS[1,110:159]),max(RATIOPENS[1,110:159])),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[2,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(0.11,0.13),col="grey80",lwd=4,type="l")
legend.text <- c("Scénario de référence","CN")
legend("topleft",cex=0.8,legend.text, fill=c("grey0","grey80"))

# Pension moyenne       
       plot   (seq(2010,2059,by=1),PENMOY[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",
               ylim=c(min(PENMOY[1,110:159]),max(PENMOY[1,110:159])),col="grey0",lwd=4,type="l")
       points (seq(2010,2059,by=1),PENMOY[2,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(0.11,0.13),col="grey80",lwd=4,type="l")
       legend.text <- c("Scénario de référence","CN")
       legend("topleft",cex=0.8,legend.text, fill=c("grey0","grey80"))
       

       
# Pension à liq
PENLIQMOY      <- matrix(nrow=4,ncol=200) 
for (t in 110:159)
{
PENLIQMOY[1,t]<- mean(pliq_[which(pliq_[,1]>0 & t_liq[]==t),1])
PENLIQMOY[2,t]<- mean(pliq_[which(pliq_[,2]>0 & t_liq[]==t),2])
}
plot   (seq(2010,2059,by=1),PENLIQMOY[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(min(PENLIQMOY[1,110:159]),max(PENLIQMOY[1,110:159])),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),PENLIQMOY[2,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(0.11,0.13),col="grey80",lwd=4,type="l")
legend.text <- c("Scénario de référence","CN")
legend("topleft",cex=0.8,legend.text, fill=c("grey0","grey80"))

# Nb de points à liquidation
POINTSCNMOY      <- matrix(nrow=4,ncol=200) 
for (t in 110:159)
{
  POINTSCNMOY[1,t]<- mean(points_cn[which(pliq_[,1]>0 & t_liq[]==t),1])
  POINTSCNMOY[2,t]<- mean(points_cn[which(pliq_[,2]>0 & t_liq[]==t),2])
}
plot   (seq(2015,2059,by=1),POINTSCNMOY[2,115:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(200000,800000),col="grey0",lwd=4,type="l")
# points (seq(2010,2059,by=1),CONV_MOY[2,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(0.11,0.13),col="grey80",lwd=4,type="l")
legend.text <- c("Points CN moyen des liquidants")
legend("top",cex=0.8,legend.text, fill=c("grey0"))

# coefficients de conversion
plot   (seq(2015,2059,by=1),CONV_MOY[2,115:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(0.04,0.06),col="grey0",lwd=4,type="l")
legend.text <- c("Coefficient de conversion moyen des liquidants")
legend("top",cex=0.8,legend.text, fill=c("grey0"))


plot   (seq(2015,2059,by=1),POINTSCNMOY[2,115:159]*CONV_MOY[2,115:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(20000,35000),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),PENLIQMOY[2,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(0.11,0.13),col="grey80",lwd=4,type="l")
legend.text <- c("Points CN moyen des liquidants")
legend("top",cex=0.8,legend.text, fill=c("grey0"))


       
# EXPLIQUER L'ECART ENTRE LES DEUX/ 

