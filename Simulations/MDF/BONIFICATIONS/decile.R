

# Chargement des données: 
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/MDF/ANC.RData"))
d<-pliq_
save(d,file=paste0(cheminsource,"Simulations/MDF/NB.RData"))
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/MDF/bonif.RData"))
load(paste0(cheminsource,"Simulations/MDF/NB.RData"))

load(paste0(cheminsource,"Simulations/MDF/bonif3.RData"))
pliq<- matrix(nrow=taille_max,ncol=4)
pliq<-pliq_[,1:4]
# pliq[,1]<-d[,1]       # Scénario de base
# pliq[,2]<-d[,2]       # No bonif
# pliq[,3]<-pliq_[,2]   # Scénario A
# pliq[,4]<-pliq_[,3]   # Scénario B

####### I. Divers #####
# Nombre de liquidants par génération
nbliq<-numeric(200)
for (g in 30:90)
{
  nbliq[g]<-(length(which(t_liq<999 & t_naiss==g)))
}

# Age de mort 
t_death<- numeric(taille_max)
a_death<- numeric(taille_max)
for (i in 1:55000)
{
t_death[i]<-which(statut[i,]==-3)[1]
}
a_death<-t_death-t_naiss

#Pourcentage de liquidants par génération: 
pliquid<-numeric(200)
pliquidH<-numeric(200)
pliquidF<-numeric(200)
for (g in 40:90)
{
  pliquid[g]<-(length(which(t_liq<999 & t_naiss==g & a_death>65)))/(length(which(t_naiss==g &a_death>65)))
  pliquidH[g]<-(length(which(t_liq<999 & t_naiss==g & sexe==1 &a_death>65)))/(length(which(t_naiss==g & sexe==1&a_death>65)))
  pliquidF[g]<-(length(which(t_liq<999 & t_naiss==g& sexe==2 &a_death>65)))/(length(which(t_naiss==g & sexe==2 &a_death>65)))
}
par(mar=c(5.1, 2.1, 2.1, 1.1))
plot   (seq(1940,1990,by=1),pliquid[40:80],
        xlab="Année de naissance", ylab=NULL,ylim=c(0.8,1),
        col="grey80",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
points (seq(1940,1980,by=1),pliquidH[40:80],lwd=2,col="grey40",type="l",yaxs="i",xaxs="i")
points (seq(1940,1980,by=1),pliquidF[40:80],lwd=2,col="grey0",type="l",yaxs="i",xaxs="i")
legend.text<-c("HF","H","F")
legend("topleft",legend.text, fill=c("grey80","grey40","grey0"), cex =0.6)

#Moyenne 5 générations: 
pliquidM<-numeric(10)
pliquidMF<-numeric(10)
pliquidMH<-numeric(10)
for (k in 1:10)
{
  pliquidM[k] <-mean(pliquid [(40+5*(k-1)):(40+4*(k))])  
  pliquidMH[k]<-mean(pliquidH[(40+5*(k-1)):(40+4*(k))])  
  pliquidMF[k]<-mean(pliquidF[(40+5*(k-1)):(40+4*(k))])  
}

plot(pliquidM[1:10],type="l",lwd=2,
     xaxt="n",ylim=c(0.8,1),col="grey0",
     xlab="Année de naissance",ylab="",
     main="Pourcentages de liquidants par génération")
points (pliquidM[1:10],col="grey0", pch=16)
lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10,labels=lab,cex.axis=0.7, srt=45)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45, pos = 1, xpd = TRUE)
lines(pliquidMH[1:10],col="grey40",type="l",lwd=2)
points (pliquidMH[1:10],col="grey40", pch=16)
lines(pliquidMF[1:10],col="grey80",type="l",lwd=2)
points (pliquidMF[1:10],col="grey80", pch=16)
legend("topright",legend=c("HF","H","F"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8,,
)

######## II. Quantile par generation et par sexe:   #########
Q1  <-matrix(0,ncol=200,nrow=3)
Q1H <-matrix(0,ncol=200,nrow=3)
Q1F <-matrix(0,ncol=200,nrow=3)
Q5  <-matrix(0,ncol=200,nrow=3)
Q5H <-matrix(0,ncol=200,nrow=3)
Q5F <-matrix(0,ncol=200,nrow=3)
Q9  <-matrix(0,ncol=200,nrow=3)
Q9H <-matrix(0,ncol=200,nrow=3)
Q9F <-matrix(0,ncol=200,nrow=3)


# Méthode 1: moyenne des déciles

# Quantiles par générations:
for (g in 30:90)
{
  #  Q1: 
  #  HF: 
  Q1[1,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g),1],probs = seq(0, 1, 0.10))[2]
  Q1[2,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g),2],probs = seq(0, 1, 0.10))[2]
  Q1[3,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g),3],probs = seq(0, 1, 0.10))[2]
  #  H: 
  Q1H[1,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==1),1],probs = seq(0, 1, 0.10))[2]
  Q1H[2,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==1),2],probs = seq(0, 1, 0.10))[2]
  Q1H[3,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==1),3],probs = seq(0, 1, 0.10))[2]
  #  F: 
  Q1F[1,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==2),1],probs = seq(0, 1, 0.10))[2]
  Q1F[2,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==2),2],probs = seq(0, 1, 0.10))[2]
  Q1F[3,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==2),3],probs = seq(0, 1, 0.10))[2]
  
  # Q5
  Q5[1,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g),1],probs = seq(0, 1, 0.10))[6]
  Q5[2,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g),2],probs = seq(0, 1, 0.10))[6]
  Q5[3,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g),3],probs = seq(0, 1, 0.10))[6]
  #  H: 
  Q5H[1,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==1),1],probs = seq(0, 1, 0.10))[6]
  Q5H[2,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==1),2],probs = seq(0, 1, 0.10))[6]
  Q5H[3,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==1),3],probs = seq(0, 1, 0.10))[6]
  #  F: 
  Q5F[1,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==2),1],probs = seq(0, 1, 0.10))[6]
  Q5F[2,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==2),2],probs = seq(0, 1, 0.10))[6]
  Q5F[3,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==2),3],probs = seq(0, 1, 0.10))[6]
  
  #Q9
  Q9[1,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g),1],probs = seq(0, 1, 0.10))[10]
  Q9[2,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g),2],probs = seq(0, 1, 0.10))[10]
  Q9[3,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g),3],probs = seq(0, 1, 0.10))[10]
  #  H: 
  Q9H[1,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==1),1],probs = seq(0, 1, 0.10))[10]
  Q9H[2,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==1),2],probs = seq(0, 1, 0.10))[10]
  Q9H[3,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==1),3],probs = seq(0, 1, 0.10))[10]
  #  F: 
  Q9F[1,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==2),1],probs = seq(0, 1, 0.10))[10]
  Q9F[2,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==2),2],probs = seq(0, 1, 0.10))[10]
  Q9F[3,g]<-quantile(pliq[which(t_liq<999  & t_naiss==g & sexe==2),3],probs = seq(0, 1, 0.10))[10]
}

save(Q1,Q1H,Q1F,Q5,Q5H,Q5F,Q9,Q9H,Q9F, file=paste0(cheminsource,"Simulations/MDF/quant1.RData"))


# Quantiles par générations:
for (g in 30:90)
{
  #  Q1: 
  #  HF: 
  Q1[1,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g),1],probs = seq(0, 1, 0.10))[2]
  Q1[2,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g),2],probs = seq(0, 1, 0.10))[2]
  Q1[3,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g),3],probs = seq(0, 1, 0.10))[2]
  #  H: 
  Q1H[1,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==1),1],probs = seq(0, 1, 0.10))[2]
  Q1H[2,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==1),2],probs = seq(0, 1, 0.10))[2]
  Q1H[3,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==1),3],probs = seq(0, 1, 0.10))[2]
  #  F: 
  Q1F[1,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==2),1],probs = seq(0, 1, 0.10))[2]
  Q1F[2,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==2),2],probs = seq(0, 1, 0.10))[2]
  Q1F[3,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==2),3],probs = seq(0, 1, 0.10))[2]
  
  # Q5
  Q5[1,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g),1],probs = seq(0, 1, 0.10))[6]
  Q5[2,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g),2],probs = seq(0, 1, 0.10))[6]
  Q5[3,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g),3],probs = seq(0, 1, 0.10))[6]
  #  H: 
  Q5H[1,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==1),1],probs = seq(0, 1, 0.10))[6]
  Q5H[2,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==1),2],probs = seq(0, 1, 0.10))[6]
  Q5H[3,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==1),3],probs = seq(0, 1, 0.10))[6]
  #  F: 
  Q5F[1,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==2),1],probs = seq(0, 1, 0.10))[6]
  Q5F[2,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==2),2],probs = seq(0, 1, 0.10))[6]
  Q5F[3,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==2),3],probs = seq(0, 1, 0.10))[6]
  
  #Q9
  Q9[1,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g),1],probs = seq(0, 1, 0.10))[10]
  Q9[2,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g),2],probs = seq(0, 1, 0.10))[10]
  Q9[3,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g),3],probs = seq(0, 1, 0.10))[10]
  #  H: 
  Q9H[1,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==1),1],probs = seq(0, 1, 0.10))[10]
  Q9H[2,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==1),2],probs = seq(0, 1, 0.10))[10]
  Q9H[3,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==1),3],probs = seq(0, 1, 0.10))[10]
  #  F: 
  Q9F[1,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==2),1],probs = seq(0, 1, 0.10))[10]
  Q9F[2,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==2),2],probs = seq(0, 1, 0.10))[10]
  Q9F[3,g]<-quantile(pliq[which(t_liq<999 & n_enf>2 & t_naiss==g & sexe==2),3],probs = seq(0, 1, 0.10))[10]
}

save(Q1,Q1H,Q1F,Q5,Q5H,Q5F,Q9,Q9H,Q9F, file=paste0(cheminsource,"Simulations/MDF/quant1enf.RData"))


###### Moyenne par génération:  ######

#Moyenne 5 générations: 
Q1M <-matrix(0,ncol=12,nrow=3)
Q1MH<-matrix(0,ncol=12,nrow=3)
Q1MF<-matrix(0,ncol=12,nrow=3)
Q5M <-matrix(0,ncol=12,nrow=3)
Q5MH<-matrix(0,ncol=12,nrow=3)
Q5MF<-matrix(0,ncol=12,nrow=3)
Q9M <-matrix(0,ncol=12,nrow=3)
Q9MH<-matrix(0,ncol=12,nrow=3)
Q9MF<-matrix(0,ncol=12,nrow=3)
for (k in 1:12)
{
  Q1M[1,k] <-mean(Q1[1,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q1M[2,k] <-mean(Q1[2,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q1M[3,k] <-mean(Q1[3,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q1MH[1,k] <-mean(Q1H[1,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q1MH[2,k] <-mean(Q1H[2,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q1MH[3,k] <-mean(Q1H[3,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q1MF[1,k] <-mean(Q1F[1,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q1MF[2,k] <-mean(Q1F[2,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q1MF[3,k] <-mean(Q1F[3,(30+5*(k-1)):(30+5*(k)-1)]) 
  
  Q5M[1,k] <-mean(Q5[1,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q5M[2,k] <-mean(Q5[2,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q5M[3,k] <-mean(Q5[3,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q5MH[1,k] <-mean(Q5H[1,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q5MH[2,k] <-mean(Q5H[2,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q5MH[3,k] <-mean(Q5H[3,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q5MF[1,k] <-mean(Q5F[1,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q5MF[2,k] <-mean(Q5F[2,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q5MF[3,k] <-mean(Q5F[3,(30+5*(k-1)):(30+5*(k)-1)]) 
  
  Q9M[1,k] <-mean(Q9[1,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q9M[2,k] <-mean(Q9[2,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q9M[3,k] <-mean(Q9[3,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q9MH[1,k] <-mean(Q9H[1,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q9MH[2,k] <-mean(Q9H[2,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q9MH[3,k] <-mean(Q9H[3,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q9MF[1,k] <-mean(Q9F[1,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q9MF[2,k] <-mean(Q9F[2,(30+5*(k-1)):(30+5*(k)-1)]) 
  Q9MF[3,k] <-mean(Q9F[3,(30+5*(k-1)):(30+5*(k)-1)]) 
  #print(c(k,(30+5*(k-1)):(30+5*(k)-1)))
}


####### Graphiques #####

# Femmes: 

plot(Q1MF[3,1:12],type="l",lwd=2,
     ylim=c(0,10000),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q1MF[3,3:12],col="grey0", pch=16)
lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10, label=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
lines(Q1MF[2,3:12],col="grey40",type="l",lwd=2)
points (Q1MF[2,3:12],col="grey40", pch=16)
lines(Q1MF[1,3:12],col="grey80",type="l",lwd=2)
points (Q1MF[1,3:12],col="grey80", pch=16)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
       )

plot(Q5MF[3,3:12],type="l",lwd=2,
     ylim=c(10000,30000),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q5MF[3,3:12],col="grey0", pch=16)
lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10, label=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
lines(Q5MF[2,3:12],col="grey40",type="l",lwd=2)
points (Q5MF[2,3:12],col="grey40", pch=16)
lines(Q5MF[1,3:12],col="grey80",type="l",lwd=2)
points (Q5MF[1,3:12],col="grey80", pch=16)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
)

plot(Q9MF[3,3:12],type="l",lwd=2,
     ylim=c(12000,52000),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q9MF[3,3:12],col="grey0", pch=16)
lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10, label=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
lines(Q9MF[2,3:12],col="grey40",type="l",lwd=2)
points (Q9MF[2,3:12],col="grey40", pch=16)
lines(Q9MF[1,3:12],col="grey80",type="l",lwd=2)
points (Q9MF[1,3:12],col="grey80", pch=16)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
)

plot(Q1MF[3,3:12],type="l",lwd=2,
     ylim=c(0,50000),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q1MF[3,3:12],col="grey0", pch=16)
lines(Q1MF[2,3:12],col="grey40",type="l",lwd=2)
points (Q1MF[2,3:12],col="grey40", pch=16)
lines(Q1MF[1,3:12],col="grey80",type="l",lwd=2)

lines(Q5MF[3,3:12],col="grey0",type="l",lwd=2)
points (Q5MF[3,3:12],col="grey0", pch=16)
lines(Q5MF[2,3:12],col="grey40",type="l",lwd=2)
points (Q5MF[2,3:12],col="grey40", pch=16)
lines(Q5MF[1,3:12],col="grey80",type="l",lwd=2)
points (Q5MF[1,3:12],col="grey80", pch=16)

lines(Q9MF[3,3:12],col="grey0",type="l",lwd=2)
points (Q9MF[3,3:12],col="grey0", pch=16)
lines(Q9MF[2,3:12],col="grey40",type="l",lwd=2)
points (Q9MF[2,3:12],col="grey40", pch=16)
lines(Q9MF[1,3:12],col="grey80",type="l",lwd=2)
points (Q9MF[1,3:12],col="grey80", pch=16)

lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10, label=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
        )



# Hommmes

plot(Q1MH[3,3:12],type="l",lwd=2,
     ylim=c(2000,65000),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q9MH[3,3:12],col="grey0", pch=16)
lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10, label=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
lines(Q1MH[2,3:12],col="grey40",type="l",lwd=2)
points (Q1MH[2,3:12],col="grey40", pch=16)
lines(Q9MH[1,3:12],col="grey80",type="l",lwd=2)
points (Q9MH[1,3:12],col="grey80", pch=16)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
)

plot(Q9MH[3,3:12],type="l",lwd=2,
     ylim=c(2000,65000),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q9MH[3,3:12],col="grey0", pch=16)
lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10, label=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
lines(Q9MH[2,3:12],col="grey40",type="l",lwd=2)
points (Q9MH[2,3:12],col="grey40", pch=16)
lines(Q9MH[1,3:12],col="grey80",type="l",lwd=2)
points (Q9MH[1,3:12],col="grey80", pch=16)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
)



# Q1: 
plot(Q1MF[3,1:8],type="l",lwd=2,
     ylim=c(2000,14000),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q1MF[3,1:8],col="grey0", pch=16)
lines(Q1MF[2,1:8],col="grey40",type="l",lwd=2)
points (Q1MF[2,1:8],col="grey40", pch=16)
lines(Q1MF[1,1:8],col="grey80",type="l",lwd=2)
points (Q1MF[1,1:8],col="grey80", pch=16)

lines(Q1MH[3,1:8],col="grey0",type="l",lwd=2)
points (Q1MH[3,1:8],col="grey0", pch=17)
lines(Q1MH[2,1:8],col="grey40",type="l",lwd=2)
points (Q1MH[2,1:8],col="grey40", pch=17)
lines(Q1MH[1,1:8],col="grey80",type="l",lwd=2)
points (Q1MH[1,1:8],col="grey80", pch=17)


lab<-c("1930-1934","1935-1939","1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969")
axis(1,at=1:8, label=FALSE)
text(seq(1, 8, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
       )


# Q5: 
plot(Q5MF[3,3:12],type="l",lwd=2,
     ylim=c(12000,36000),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q5MF[3,3:12],col="grey0", pch=16)
lines(Q5MF[2,3:12],col="grey40",type="l",lwd=2)
points (Q5MF[2,3:12],col="grey40", pch=16)
lines(Q5MF[1,3:12],col="grey80",type="l",lwd=2)
points (Q5MF[1,3:12],col="grey80", pch=16)

lines(Q5MH[3,3:12],col="grey0",type="l",lwd=2)
points (Q5MH[3,3:12],col="grey0", pch=17)
lines(Q5MH[2,3:12],col="grey40",type="l",lwd=2)
points (Q5MH[2,3:12],col="grey40", pch=17)
lines(Q5MH[1,3:12],col="grey80",type="l",lwd=2)
points (Q5MH[1,3:12],col="grey80", pch=17)


lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10, label=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
)




# Q9: 
plot(Q9MF[3,3:12],type="l",lwd=2,
     ylim=c(20000,80000),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q9MF[3,3:12],col="grey0", pch=16)
lines(Q9MF[2,3:12],col="grey40",type="l",lwd=2)
points (Q9MF[2,3:12],col="grey40", pch=16)
lines(Q9MF[1,3:12],col="grey80",type="l",lwd=2)
points (Q9MF[1,3:12],col="grey80", pch=16)

lines(Q9MH[3,3:12],col="grey0",type="l",lwd=2)
points (Q9MH[3,3:12],col="grey0", pch=17)
lines(Q9MH[2,3:12],col="grey40",type="l",lwd=2)
points (Q9MH[2,3:12],col="grey40", pch=17)
lines(Q9MH[1,3:12],col="grey80",type="l",lwd=2)
points (Q9MH[1,3:12],col="grey80", pch=17)


lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10, label=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
)


#Q9/Q1
plot(Q9MF[3,3:12]/Q1MF[3,3:12],type="l",lwd=2,
     ylim=c(0,10),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q9MF[3,3:12]/Q1MF[3,3:12],col="grey0", pch=16)
lines(Q9MF[2,3:12]/Q1MF[2,3:12],col="grey40",type="l",lwd=2)
points (Q9MF[2,3:12]/Q1MF[2,3:12],col="grey40", pch=16)
lines(Q9MF[1,3:12]/Q1MF[1,3:12],col="grey80",type="l",lwd=2)
points (Q9MF[1,3:12]/Q1MF[1,3:12],col="grey80", pch=16)

lines(Q9MH[3,3:12]/Q1MH[3,3:12],col="grey0",type="l",lwd=2)
points (Q9MH[3,3:12]/Q1MH[3,3:12],col="grey0", pch=17)
lines(Q9MH[2,3:12]/Q1MH[2,3:12],col="grey40",type="l",lwd=2)
points (Q9MH[2,3:12]/Q1MH[2,3:12],col="grey40", pch=17)
lines(Q9MH[1,3:12]/Q1MH[1,3:12],col="grey80",type="l",lwd=2)
points (Q9MH[1,3:12]/Q1MH[1,3:12],col="grey80", pch=17)


lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10, label=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
)


# Méthode 2: quantiles calculés sur toutes les générations regroupées

Q1M <-matrix(0,ncol=12,nrow=3)
Q1MH<-matrix(0,ncol=12,nrow=3)
Q1MF<-matrix(0,ncol=12,nrow=3)
Q5M <-matrix(0,ncol=12,nrow=3)
Q5MH<-matrix(0,ncol=12,nrow=3)
Q5MF<-matrix(0,ncol=12,nrow=3)
Q9M <-matrix(0,ncol=12,nrow=3)
Q9MH<-matrix(0,ncol=12,nrow=3)
Q9MF<-matrix(0,ncol=12,nrow=3)
for (k in 1:12)
{ 
  Q1M[1,k]<-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)),1],probs = seq(0, 1, 0.10))[2]
  Q1M[2,k]<-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)),2],probs = seq(0, 1, 0.10))[2]
  Q1M[3,k]<-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)),3],probs = seq(0, 1, 0.10))[2]
  Q1MH[1,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1) & sexe==1),1],probs = seq(0, 1, 0.10))[2]
  Q1MH[2,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1) & sexe==1),2],probs = seq(0, 1, 0.10))[2]
  Q1MH[3,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1) & sexe==1),3],probs = seq(0, 1, 0.10))[2]
  Q1MF[1,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1) & sexe==2),1],probs = seq(0, 1, 0.10))[2]
  Q1MF[2,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1) & sexe==2),2],probs = seq(0, 1, 0.10))[2]
  Q1MF[3,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1) & sexe==2),3],probs = seq(0, 1, 0.10))[2]
  
  Q5M[1,k]<-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)),1],probs = seq(0, 1, 0.10))[6]
  Q5M[2,k]<-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)),2],probs = seq(0, 1, 0.10))[6]
  Q5M[3,k]<-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)),3],probs = seq(0, 1, 0.10))[6]
  Q5MH[1,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)& sexe==1),1],probs = seq(0, 1, 0.10))[6]
  Q5MH[2,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)& sexe==1),2],probs = seq(0, 1, 0.10))[6]
  Q5MH[3,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)& sexe==1),3],probs = seq(0, 1, 0.10))[6]
  Q5MF[1,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)& sexe==2),1],probs = seq(0, 1, 0.10))[6]
  Q5MF[2,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)& sexe==2),2],probs = seq(0, 1, 0.10))[6]
  Q5MF[3,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)& sexe==2),3],probs = seq(0, 1, 0.10))[6]
  
  Q9M[1,k]<-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)),1],probs = seq(0, 1, 0.10))[10]
  Q9M[2,k]<-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)),2],probs = seq(0, 1, 0.10))[10]
  Q9M[3,k]<-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)),3],probs = seq(0, 1, 0.10))[10]
  Q9MH[1,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)& sexe==1),1],probs = seq(0, 1, 0.10))[10]
  Q9MH[2,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)& sexe==1),2],probs = seq(0, 1, 0.10))[10]
  Q9MH[3,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)& sexe==1),3],probs = seq(0, 1, 0.10))[10]
  Q9MF[1,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)& sexe==2),1],probs = seq(0, 1, 0.10))[10]
  Q9MF[2,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)& sexe==2),2],probs = seq(0, 1, 0.10))[10]
  Q9MF[3,k] <-quantile(pliq[which(t_liq<999 & t_naiss>=(30+5*(k-1)) & t_naiss<=(30+5*(k)-1)& sexe==2),3],probs = seq(0, 1, 0.10))[10]
  
}

#### Graphiques #####


# Q1: 
plot(Q1MF[3,3:12],type="l",lwd=2,
     ylim=c(0,12000),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q1MF[3,3:12],col="grey0", pch=16)
lines(Q1MF[2,3:12],col="grey40",type="l",lwd=2)
points (Q1MF[2,3:12],col="grey40", pch=16)
lines(Q1MF[1,3:12],col="grey80",type="l",lwd=2)
points (Q1MF[1,3:12],col="grey80", pch=16)

lines(Q1MH[3,3:12],col="grey0",type="l",lwd=2)
points (Q1MH[3,3:12],col="grey0", pch=17)
lines(Q1MH[2,3:12],col="grey40",type="l",lwd=2)
points (Q1MH[2,3:12],col="grey40", pch=17)
lines(Q1MH[1,3:12],col="grey80",type="l",lwd=2)
points (Q1MH[1,3:12],col="grey80", pch=17)


lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10, label=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
)


# Q5: 
plot(Q5MF[3,3:12],type="l",lwd=2,
     ylim=c(12000,36000),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q5MF[3,3:12],col="grey0", pch=16)
lines(Q5MF[2,3:12],col="grey40",type="l",lwd=2)
points (Q5MF[2,3:12],col="grey40", pch=16)
lines(Q5MF[1,3:12],col="grey80",type="l",lwd=2)

lines(Q5MH[3,3:12],col="grey0",type="l",lwd=2)
points (Q5MH[3,3:12],col="grey0", pch=17)
lines(Q5MH[2,3:12],col="grey40",type="l",lwd=2)
points (Q5MH[2,3:12],col="grey40", pch=17)
lines(Q5MH[1,3:12],col="grey80",type="l",lwd=2)
points (Q5MH[1,3:12],col="grey80", pch=17)


lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10, label=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
)




# Q9: 
plot(Q9MF[3,3:12],type="l",lwd=2,
     ylim=c(20000,80000),col="grey0",xaxt="n",
     xlab="Année de naissance",ylab="")
points (Q9MF[3,3:12],col="grey0", pch=16)
lines(Q9MF[2,3:12],col="grey40",type="l",lwd=2)
points (Q9MF[2,3:12],col="grey40", pch=16)
lines(Q9MF[1,3:12],col="grey80",type="l",lwd=2)
points (Q9MF[1,3:12],col="grey80", pch=16)

lines(Q9MH[3,3:12],col="grey0",type="l",lwd=2)
points (Q9MH[3,3:12],col="grey0", pch=17)
lines(Q9MH[2,3:12],col="grey40",type="l",lwd=2)
points (Q9MH[2,3:12],col="grey40", pch=17)
lines(Q9MH[1,3:12],col="grey80",type="l",lwd=2)
points (Q9MH[1,3:12],col="grey80", pch=17)


lab<-c("1940-1944","1945-1949","1950-1954","1955-1959","1960-1964","1965-1969","1970-1974","1975-1979","1980-1984","1985-1989")
axis(1,at=1:10, label=FALSE)
text(seq(1, 10, by=1), par("usr")[3] - 0.2, labels = lab, srt = 45,offset=1.3, pos = 1, xpd = TRUE,cex=0.8)
legend("topright",legend=c("Réforme","No bonif","Législation actuelle"),
       lty=1,lwd=2,pch=21,col=c("grey0","grey40","grey80"),
       ncol=3,bty="n",cex=0.8
)

