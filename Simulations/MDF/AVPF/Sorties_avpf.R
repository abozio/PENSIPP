###### Analyse des résultats: réforme AVPF ######


###### # I. Analyse du dispositif existant:  #####

# Chargement des résultats: 
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/MDF/AVPF/avpf.RData"))
setwd ( (paste0(cheminsource,"Simulations/MDF/AVPF/Graphes")))            
# 1. Masse des pensions: 
mtotAVPF      <- numeric(200)
mtotliqAVPF    <- numeric(200)
mtotAVPF[]     <-(MPENS[1,]-MPENS[2,])/1e9       
mtotliqAVPF[]  <-(MPENLIQ[1,]-MPENLIQ[2,])/1e9   
# Valeurs en 2004 et 2006: 
mtotAVPF[104]
mtotAVPF[106]
# Evolution: 
par(mar=c(5.1, 4.1, 3.1, 1.1))
plot   (seq(2000,2050,by=1),mtotAVPF[100:150],
        xlab="Année", ylab="Masses AVPF",ylim=c(0,30),
        col="grey0",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
title("Masse des pensions versée au titre de la AVPF \n(STOCK, en milliard)")
points(seq(2000,2050,by=1),(mtotAVPF[100:150]*100/MPENS[1,100:150]),col="grey80")
par(mar=c(5.1, 4.1, 3.1, 1.1))



x<-(seq(2000,2050,by=1))
par(mar=c(2.1, 4.1, 2.1, 4.1))
plot(x,mtotAVPF[100:150],type="l",col="grey0",xlab="",ylab="",
     ylim=c(0,30),yaxs="i",xaxs="i",lwd=4)
par(new=TRUE)
plot(x,(mtotAVPF[100:150]*1e9/MPENS[1,100:150]),type="l",col="grey80",xlab="",ylab="",yaxt="n",
     ylim=c(0,0.15),yaxs="i",xaxs="i",lwd=4)
#title("Pensions versées au titre des AVPF par années \nEvolution par année", cex.main=0.8)
par(new=TRUE)
axis(4)
mtext(side = 2, text = "Masse des pensions AVPF (en milliards)", line = 2.5, cex=0.8)
mtext(side = 4, text = "Ratio pensions AVPF/ pension totale", line = 2.5, cex=0.8)
legend.text<-c("masse AVPF","ratio masse AVPF/masse totale des pensions")
legend("topleft",cex=0.8, legend.text, col=c("grey0", "grey80"),
       lty=c(1,1,2,2),bty="n")



plot   (seq(2000,2050,by=1),mtotliqAVPF[100:150],
        xlab="Année", ylab="Masses AVPF",ylim=c(0,2),
        col="grey0",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
title("Masse des pensions versée au titre de la AVPF \n(FLUX, en milliard)")


# Pourcentage de bénéficiaire (au sens, % dont la pension est changé)
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

#Lissage
gene<-seq(1930,1990,by=1)
y   <- lm (benef[30:90] ~ poly (gene, 4, raw=TRUE))
yH  <- lm (benefH[30:90] ~ poly (gene, 4, raw=TRUE))
yF  <- lm (benefF[30:90] ~ poly (gene, 4, raw=TRUE))

par(mar = c(4.1, 4.1, 3.1, 2.1))
plot(gene,fitted(yF), type="l",lwd=3,pch=16,
     ylim=c(0,1),col="rosybrown1",
     xlab="Année de naissance",ylab="Pourcentages de bénéficiaires")
lines(gene,fitted(yH), lwd=3, col="lightskyblue")
lines(gene,fitted(y), lwd=3, col="lightgreen")

title(main= "Pourcentage de bénéficiaires de l'AVPF\n Evolution par génération",cex.main=0.8)
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
legend.text<-c("HF","HF no AVPF","H","H no AVPF","F","F no AVPF")
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
legend("top",legend=c("HF","HF no AVPF","H","H no AVPF","F","F no AVPF"),
       lty=1,lwd=2,col=c("lightgreen","green","lightskyblue","blue", "rosybrown1","red1"),
       ncol=3,bty="n",cex=0.8,,
)


# Pour les bénéficiaires:
penliqmoyF2<-matrix(ncol=200,nrow=2)
for (g in 30:90)
{
  penliqmoyF2[1,g]<-mean(pliq_[which(t_liq<999 & t_naiss==g & pliq_[,1]>pliq_[,2] & sexe==2),1])
  penliqmoyF2[2,g]<-mean(pliq_[which(t_liq<999 & t_naiss==g & pliq_[,1]>pliq_[,2] & sexe==2),2])
}

# Lissage
gene<-seq(1930,1990,by=1)
y1F <- lm (penliqmoyF2[1,30:90]~ poly (gene, 4, raw=TRUE))
y2F <- lm (penliqmoyF2[2,30:90] ~ poly (gene, 4, raw=TRUE))


par(mar = c(4.1, 4.1, 3.1, 2.1))
plot(gene,fitted(y1F), type="l",lwd=3,pch=16,
     ylim=c(0000,40000),col="rosybrown1",
     xlab="Année de naissance",ylab="Pension à liquidation")
lines(gene,fitted(y2F), lwd=3, col="red1")
title(main= "Pension à liquidation: Evolution par génération \n (bénéficiaires)",cex.main=0.8)
legend("topleft",legend=c("F","F no AVPF"),
       lty=1,lwd=2,col=c("rosybrown1","red1"),
       ncol=1,bty="n",cex=0.8,,
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
     xlab="Année de naissance",ylab="Pension Femme/ Pension homme")
lines(gene,fitted(y2), lwd=3, col="grey80")
#title(main= "Pension à liquidation: rapport Femme/Homme",cex.main=0.8)
legend("top",legend=c("Scénario de référence","No AVPF"),
       lty=1,lwd=2,col=c("grey0","grey80"),
       ncol=1,bty="n",cex=0.8,,
)

