###### Analyse des résultats: réforme des bonifications pour pension ######


###### # I. Analyse du dispositif existant:  #####

# Chargement des résultats: 
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/MDF/BONIFICATIONS/bonif3.RData"))
setwd ( (paste0(cheminsource,"Simulations/MDF/BONIFICATIONS")))   
# 1. Masse des pensions: 
mtotMAJ       <- numeric(200)
mtotliqMAJ    <- numeric(200)
mtotMAJ[]     <-(MPENS[1,]-MPENS[2,])/1e9       
mtotliqMAJ[]  <-(MPENLIQ[1,]-MPENLIQ[2,])/1e9   
# Valeurs en 2004 et 2006: 
mtotMAJ[104]
mtotMAJ[106]
# Evolution: 
# Evolution: 
par(mar=c(5.1, 4.1, 3.1, 1.1))
plot   (seq(2000,2050,by=1),mtotMAJ[100:150],
        xlab="Année", ylab="Masses bonifications",ylim=c(0,30),
        col="grey0",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
par(mar=c(5.1, 4.1, 3.1, 1.1))


x<-(seq(2000,2050,by=1))
par(mar=c(2.1, 4.1, 3.1, 4.1))
plot(x,mtotMAJ[100:150],type="l",col="grey0",xlab="",ylab="",
     ylim=c(0,30),yaxs="i",xaxs="i",lwd=4)
par(new=TRUE)
plot(x,(mtotMAJ[100:150]*1e9/MPENS[1,100:150]),type="l",col="grey80",xlab="",ylab="",yaxt="n",
     ylim=c(0,0.15),yaxs="i",xaxs="i",lwd=4)
title("Pensions versées au titre de majoration pour enfants par années \nEvolution par année", cex.main=0.8)
par(new=TRUE)
axis(4)
mtext(side = 2, text = "Masse des pensions bonification (en milliards)", line = 2.5, cex=0.8)
mtext(side = 4, text = "Ratio pensions bonification / pension totale", line = 2.5, cex=0.8)
legend.text<-c("masse MDA","ratio masse bonifications/masse totale des pensions")
legend("topleft",cex=0.8, legend.text, col=c("grey0", "grey80"),
       lty=c(1,1,2,2),bty="n")


plot   (seq(2000,2050,by=1),mtotliqMAJ[100:150],
        xlab="Année", ylab="Masses MAJ",ylim=c(0,2),
        col="grey0",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
title("Masse des pensions à liquidation versées au titre de la MDA \n(FLUX, en milliard)")


####### Pourcentage de bénéficiaire (au sens, % dont la pension est changé) #########
# (graphe 2)

length(which(t_liq<999 & sexe==2 & pliq_[,1]>pliq_[,2]))/length(which(t_liq<999 & sexe==2))
# Evolution:
benef<-numeric(200)
benefH<-numeric(200)
benefF<-numeric(200)
for (g in 30:90)
{
  benef[g]<-length(which(t_liq<999  & t_naiss==g & pliq_[,1]>pliq_[,2]))/
    length(which(t_liq<999 &t_naiss==g))
  benefF[g]<-length(which(t_liq<999 & sexe==2 &t_naiss==g & pliq_[,1]>pliq_[,2]))/
    length(which(t_liq<999 &t_naiss==g & sexe==2))
  benefH[g]<-length(which(t_liq<999 & sexe==1 &t_naiss==g & pliq_[,1]>pliq_[,2]))/
    length(which(t_liq<999 &t_naiss==g & sexe==1))
}  
par(mar=c(5.1, 4.1, 3.1, 1.1))
plot   (seq(1930,1990,by=1),benef[30:90],
        xlab="Génération", ylab="% des bénéficiaires MDA",ylim=c(0,0.5),
        col="grey0",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
#title("Pourcentage de femmes bénéficiaires de la MDA \n(par génération)")

#Lissage
gene<-seq(1930,1990,by=1)
y   <- lm (benef[30:90] ~ poly (gene, 4, raw=TRUE))
yH  <- lm (benefH[30:90] ~ poly (gene, 4, raw=TRUE))
yF  <- lm (benefF[30:90] ~ poly (gene, 4, raw=TRUE))

par(mar = c(4.1, 4.1, 2.1, 2.1))
plot(gene,fitted(yF), type="l",lwd=3,pch=16,
     ylim=c(0,0.5),col="rosybrown1",
     xlab="Année de naissance",ylab="Pension à liquidation")
lines(gene,fitted(yH), lwd=3, col="lightskyblue")
lines(gene,fitted(y), lwd=3, col="lightgreen")

#title(main= "Pourcentage de bénéficiaires des bonificiations\n Evolution par génération",cex.main=0.8)
legend("top",legend=c("HF","H","F"),
       lty=1,lwd=2,col=c("lightgreen","lightskyblue","rosybrown1"),
       ncol=3,bty="n",cex=0.8,,
)


# 2. Effets sur les pensions  

# Pension à liquidation avec et sans.
penliqmoy <-matrix(ncol=200,nrow=2)
penliqmoyH<-matrix(ncol=200,nrow=2)
penliqmoyF<-matrix(ncol=200,nrow=2)
for (g in 30:90)
{
  penliqmoy[1,g] <-mean(pliq_[which(t_liq<999 & t_naiss==g),1])
  penliqmoy[2,g] <-mean(pliq_[which(t_liq<999 & t_naiss==g),2])
  penliqmoyH[1,g]<-mean(pliq_[which(t_liq<999 & t_naiss==g & sexe==1),1])
  penliqmoyH[2,g]<-mean(pliq_[which(t_liq<999 & t_naiss==g & sexe==1),2])
  penliqmoyF[1,g]<-mean(pliq_[which(t_liq<999 & t_naiss==g & sexe==2),1])
  penliqmoyF[2,g]<-mean(pliq_[which(t_liq<999 & t_naiss==g & sexe==2),2])
}
par(mar=c(5.1, 2.1, 2.1, 1.1))
plot   (seq(1930,1990,by=1),penliqmoy[1,30:90],
        xlab="Année de naissance", ylab=NULL,ylim=c(8000,42000),
        col="green1",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
points (seq(1930,1990,by=1),penliqmoy[2,30:90],lwd=2,col="green4",type="l",yaxs="i",xaxs="i")
points (seq(1930,1990,by=1),penliqmoyH[1,30:90],lwd=2,col="lightskyblue",type="l",yaxs="i",xaxs="i")
points (seq(1930,1990,by=1),penliqmoyH[2,30:90],lwd=2,col="blue",type="l",yaxs="i",xaxs="i")
points (seq(1930,1990,by=1),penliqmoyF[1,30:90],lwd=2,col="rosybrown1",type="l",yaxs="i",xaxs="i")
points (seq(1930,1990,by=1),penliqmoyF[2,30:90],lwd=2,col="red1",type="l",yaxs="i",xaxs="i")
legend.text<-c("HF","HF no mda","H","H no mda","F","F no mda")
legend("topleft",legend.text, fill=c("green1","green4","lightskyblue","blue","rosybrown1","red1"), cex =0.6)

# Lissage
gene<-seq(1930,1990,by=1)
y1  <- lm (penliqmoy[1,30:90] ~ poly (gene, 4, raw=TRUE))
y2  <- lm (penliqmoy[2,30:90] ~ poly (gene, 4, raw=TRUE))
y1F <- lm (penliqmoyF[1,30:90]~ poly (gene, 4, raw=TRUE))
y2F <- lm (penliqmoyF[2,30:90] ~ poly (gene, 4, raw=TRUE))
y1H <- lm (penliqmoyH[1,30:90]~ poly (gene, 4, raw=TRUE))
y2H <- lm (penliqmoyH[2,30:90] ~ poly (gene, 4, raw=TRUE))

par(mar = c(4.1, 4.1, 3.1, 2.1))
plot(gene,fitted(y1F), type="l",lwd=3,pch=16,
     ylim=c(10000,50000),col="rosybrown1",
     xlab="Année de naissance",ylab="Pension à liquidation")
lines(gene,fitted(y2F), lwd=3, col="red1")
lines(gene,fitted(y1H), lwd=3, col="lightskyblue")
lines(gene,fitted(y2H), lwd=3, col="blue")
lines(gene,fitted(y1), lwd=3, col="lightgreen")
lines(gene,fitted(y2), lwd=3, col="green")

title(main= "Pension à liquidation: Evolution par génération \n (tous liquidants)",cex.main=0.8)
legend("top",legend=c("HF","HF no bonif","H","H no bonif","F","F no bonif"),
       lty=1,lwd=2,col=c("lightgreen","green","lightskyblue","blue", "rosybrown1","red1"),
       ncol=3,bty="n",cex=0.8,,
       )


# Pour les bénéficiaires:
penliqmoy2<-matrix(ncol=200,nrow=2)
penliqmoyF2<-matrix(ncol=200,nrow=2)
penliqmoyH2<-matrix(ncol=200,nrow=2)
for (g in 30:90)
{
  penliqmoy2[1,g]<-mean(pliq_[which(t_liq<999 & t_naiss==g & pliq_[,1]>pliq_[,2]),1])
  penliqmoy2[2,g]<-mean(pliq_[which(t_liq<999 & t_naiss==g & pliq_[,1]>pliq_[,2]),2])
  penliqmoyF2[1,g]<-mean(pliq_[which(t_liq<999 & t_naiss==g & pliq_[,1]>pliq_[,2] & sexe==2),1])
  penliqmoyF2[2,g]<-mean(pliq_[which(t_liq<999 & t_naiss==g & pliq_[,1]>pliq_[,2] & sexe==2),2])
  penliqmoyH2[1,g]<-mean(pliq_[which(t_liq<999 & t_naiss==g & pliq_[,1]>pliq_[,2] & sexe==1),1])
  penliqmoyH2[2,g]<-mean(pliq_[which(t_liq<999 & t_naiss==g & pliq_[,1]>pliq_[,2] & sexe==1),2])
}

# Lissage
gene<-seq(1930,1990,by=1)
y1 <- lm (penliqmoy2[1,30:90]~ poly (gene, 4, raw=TRUE))
y2 <- lm (penliqmoy2[2,30:90] ~ poly (gene, 4, raw=TRUE))
y1F <- lm (penliqmoyF2[1,30:90]~ poly (gene, 4, raw=TRUE))
y2F <- lm (penliqmoyF2[2,30:90] ~ poly (gene, 4, raw=TRUE))
y1H <- lm (penliqmoyH2[1,30:90]~ poly (gene, 4, raw=TRUE))
y2H <- lm (penliqmoyH2[2,30:90] ~ poly (gene, 4, raw=TRUE))


par(mar = c(4.1, 4.1, 3.1, 2.1))
plot(gene,fitted(y1F), type="l",lwd=3,pch=16,
     ylim=c(5000,45000),col="rosybrown1",
     xlab="Année de naissance",ylab="Pension à liquidation")
lines(gene,fitted(y2F), lwd=3, col="red1")
lines(gene,fitted(y1H), lwd=3, col="lightskyblue")
lines(gene,fitted(y2H), lwd=3, col="blue")
lines(gene,fitted(y1), lwd=3, col="lightgreen")
lines(gene,fitted(y2), lwd=3, col="green")
title(main= "Pension à liquidation: Evolution par génération \n (bénéficiaires)",cex.main=0.8)
legend("topleft",legend=c("HF","HF no bonif","H","H no bonif","F","F no bonif"),
       lty=1,lwd=2,col=c("lightgreen","green","lightskyblue","blue", "rosybrown1","red1"),
       ncol=3,bty="n",cex=0.8,,
)




# 3. Rapport pension à liquidation moyenne Homme/femme par génération:

# Comparaison avec/sans dispositif:
ratio<-matrix(ncol=200,nrow=2)
for (g in 30:90)
{
  ratio[1,g]<-penliqmoyF[1,g]/penliqmoyH[1,g]
  ratio[2,g]<-penliqmoyF[2,g]/penliqmoyH[2,g]
}

par(mar=c(5.1, 2.1, 2.1, 1.1))
plot   (seq(1930,1990,by=1),ratio[1,30:90],
        xlab="Année de naissance", ylab=NULL,ylim=c(0.5,1),
        col="grey0",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
points (seq(1930,1990,by=1),ratio[2,30:90],lwd=2,col="grey80",type="l",yaxs="i",xaxs="i")

gene<-seq(1930,1990,by=1)
y1 <- lm (ratio[1,30:90] ~ poly (gene, 4, raw=TRUE))
y2 <- lm (ratio[2,30:90] ~ poly (gene, 4, raw=TRUE))


par(mar = c(4.1, 4.1, 2.1, 2.1))
plot(gene,fitted(y1), type="l",lwd=3,pch=16,
     ylim=c(0.5,1),col="grey0",
     xlab="Année de naissance",ylab="Pension Femme/ Pension homme",cex=0.8)
lines(gene,fitted(y2), lwd=3, col="grey80")
#title(main= "Pension à liquidation: rapport Femme/Homme",cex.main=0.8)
legend("top",legend=c("Scénario de référence","No bonif"),
       lty=1,lwd=2,col=c("grey0","grey80"),
       ncol=1,bty="n",cex=0.8,,
)

#############################
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
ratio<-matrix(ncol=200,nrow=2)
for (g in 30:90)
{
  ratio[1,g]<-penliqmoyF[1,g]/penliqmoyH[1,g]
  ratio[2,g]<-penliqmoyF[2,g]/penliqmoyH[2,g]
}

par(mar=c(5.1, 2.1, 2.1, 1.1))
plot   (seq(1930,1990,by=1),ratio[1,30:90],
        xlab="Année de naissance", ylab=NULL,ylim=c(0.5,1),
        col="grey0",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
points (seq(1930,1990,by=1),ratio[2,30:90],lwd=2,col="grey80",type="l",yaxs="i",xaxs="i")

gene<-seq(1930,1990,by=1)
y1 <- lm (ratio[1,30:90] ~ poly (gene, 4, raw=TRUE))
y2 <- lm (ratio[2,30:90] ~ poly (gene, 4, raw=TRUE))


par(mar = c(4.1, 4.1, 4.1, 2.1))
plot(gene,fitted(y1), type="l",lwd=3,pch=16,
     ylim=c(0.5,1),col="grey0",
     xlab="Année de naissance",ylab="Pension Femme/ Pension homme")
lines(gene,fitted(y2), lwd=3, col="grey80")
title(main= "Pension à liquidation: rapport Femme/Homme",cex=0.8)
legend("top",legend=c("Scénario de référence","No mda"),
       lty=1,lwd=2,col=c("grey0","grey80"),
       ncol=1,bty="n",cex=0.8,,
)



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


