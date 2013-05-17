
  
t0  <- Sys.time()
  
#### Chargement des programmes source ####
  
# Déclaration du chemin pour les fichiers sources
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R"      )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R"          )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsRetr.R"         )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsCN.R"           )) )  
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
setwd ( (paste0(cheminsource,"Simulations/MDF/Carrieres"                                    )) )


########## I. Statisitiques Descriptives  ##########
dureemaj60 <- numeric(taille_max)    # durée tot maj validée à 60 ans
duree60    <- numeric(taille_max)    # durée tot validée à 60 ans
duremp60   <- numeric(taille_max)    # durée emploi à 60 ans
durcho60   <- numeric(taille_max)    # durée chom à 60 ans
duravpf60  <- numeric(taille_max)    # durée chom à 60 ans
salc       <- numeric(taille_max)    # somme des salaires
salm       <- numeric(taille_max)    # salaire moyen


for (i in 1:55000)
{
if (t_naiss[i]<101)
{
DurBase(i,t_naiss[i]+60)  
DurMajo(i,t_naiss[i]+60)  
#print(c(i,duree_tot_maj))
dureemaj60[i]<-duree_tot_maj
duree60[i]   <-duree_tot
duremp60[i]  <-duree_emp
durcho60[i]  <-duree_avpf
duravpf60[i] <-duree_cho
salc[i]      <-sum(salaire[i,1:160])
salm[i]      <- mean(salaire[i,which(salaire[i,]>0)])
}
}
# Par génération: salaire moyen et durée validée à 60 ans. 
salcum   <- numeric(200)   # salaire cumulé 
salcumF  <- numeric(200)   
salcumH  <- numeric(200)
salmoy   <- numeric(200)   # salaire moyen 
salmoyF  <- numeric(200)   
salmoyH  <- numeric(200)
durval   <- numeric(200)   # durée validée à 60 ans
durvalH  <- numeric(200)
durvalF  <- numeric(200)
duremp   <- numeric(200)   # durée en emploi à 60 ans
durempH  <- numeric(200) 
durempF  <- numeric(200)
duravpf  <- numeric(200)   # durée en avpf 
duravpfH <- numeric(200) 
duravpfF <- numeric(200)
n_enf    <- numeric(200)   # nombre d'enfants moyen


for (g in 30:90)
{
salcum [g]<-mean(salc[which(t_naiss==g)])
salcumH[g]<-mean(salc[which(t_naiss==g & sexe==1)])
salcumF[g]<-mean(salc[which(t_naiss==g & sexe==2)])
salmoy [g]<-mean(salm[which(t_naiss==g)],na.rm=TRUE)
salmoyH[g]<-mean(salm[which(t_naiss==g & sexe==1)],na.rm=TRUE)
salmoyF[g]<-mean(salm[which(t_naiss==g & sexe==2)],na.rm=TRUE)
durval [g]<-mean(dureemaj60[which(t_naiss==g)])
durvalH[g]<-mean(dureemaj60[which(t_naiss==g & sexe==1)])
durvalF[g]<-mean(dureemaj60[which(t_naiss==g & sexe==2)])
duremp [g]<-mean(duremp60[which(t_naiss==g)])
durempH[g]<-mean(duremp60[which(t_naiss==g & sexe==1)])
durempF[g]<-mean(duremp60[which(t_naiss==g & sexe==2)])
duravpf [g]<-mean(duravpf60[which(t_naiss==g)])
duravpfH[g]<-mean(duravpf60[which(t_naiss==g & sexe==1)])
duravpfF[g]<-mean(duravpf60[which(t_naiss==g & sexe==2)])
}  

### Graphes: 
# Salaire moyen
plot   (seq(1930,1990,by=1),salmoy[30:90],
        xlab="Année de naissance", ylab=NULL,ylim=c(0,56000),
        col="grey80",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
points (seq(1930,1990,by=1),salmoyH[30:90],lwd=2,col="grey40",type="l",yaxs="i",xaxs="i")
points (seq(1930,1990,by=1),salmoyF[30:90],lwd=2,col="grey0",type="l",yaxs="i",xaxs="i")
legend.text<-c("HF","H","F")
title("salaire moyen par génération")
legend("topleft",legend.text, fill=c("grey80","grey40","grey0"), cex =0.6)

# Durée validée
plot   (seq(1930,1990,by=1),durval[30:90],
        xlab="Année de naissance", ylab=NULL,ylim=c(30,42),
        col="grey80",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
points (seq(1930,1990,by=1),durvalH[30:90],lwd=2,col="grey40",type="l",yaxs="i",xaxs="i")
points (seq(1930,1990,by=1),durvalF[30:90],lwd=2,col="grey0",type="l",yaxs="i",xaxs="i")
legend.text<-c("HF","H","F")
title("durée validée à 60 ans \npar génération")
legend("topright",legend.text, fill=c("grey80","grey40","grey0"), cex =0.6)

# Durée validée
plot   (seq(1930,1990,by=1),duremp[30:90],
        xlab="Année de naissance", ylab=NULL,ylim=c(20,42),
        col="grey80",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
points (seq(1930,1990,by=1),durempH[30:90],lwd=2,col="grey40",type="l",yaxs="i",xaxs="i")
points (seq(1930,1990,by=1),durempF[30:90],lwd=2,col="grey0",type="l",yaxs="i",xaxs="i")
legend.text<-c("HF","H","F")
title("durée cumulée en emploi à 60 ans \npar génération")
legend("topright",legend.text, fill=c("grey80","grey40","grey0"), cex =0.6)

# Durée avpf
plot   (seq(1930,1990,by=1),duravpf[30:90],
        xlab="Année de naissance", ylab=NULL,ylim=c(0,5),
        col="grey80",lwd=2,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
points (seq(1930,1990,by=1),duravpfH[30:90],lwd=2,col="grey40",type="l",yaxs="i",xaxs="i")
points (seq(1930,1990,by=1),duravpfF[30:90],lwd=2,col="grey0",type="l",yaxs="i",xaxs="i")
legend.text<-c("HF","H","F")
title("durée cumulée en avpf à 60 ans \npar génération")
legend("topright",legend.text, fill=c("grey80","grey40","grey0"), cex =0.6)










########## II. Programme Didier  ##########

# HOMMES ET FEMMES
salmoy_g <- matrix(nrow=110,ncol=60)    # Salaire moyen par âge et génération
salmoy_t <- matrix(nrow=160,ncol=60)    # Salaire moyen par âge et date 
emploi_g <- matrix(nrow=110,ncol=60)    # Taux d'emploi par âge et génération
emploi_t <- matrix(nrow=160,ncol=60)    # Taux d'emploi par âge et date
duree_g  <- matrix(0,nrow=110,ncol=60)  # Duree en emploi par âge et G
# Hommes
salmoy_gH <- matrix(nrow=110,ncol=60)
salmoy_tH <- matrix(nrow=160,ncol=60)
emploi_gH <- matrix(nrow=110,ncol=60)
emploi_tH <- matrix(nrow=160,ncol=60)
duree_gH  <- matrix(0,nrow=110,ncol=60)
# Femmes
salmoy_gF <- matrix(nrow=110,ncol=60)
salmoy_tF <- matrix(nrow=160,ncol=60)
emploi_gF <- matrix(nrow=110,ncol=60)
emploi_tF <- matrix(nrow=160,ncol=60)
duree_gF  <- matrix(0,nrow=110,ncol=60)


for (g in 20:100)
{
  for (a in 15:60)
  {
    t <- g+a
    salmoy_g[g,a] <- mean(salaire[t_naiss==g & salaire[,t]>0,t])/Prix[t]
    emploi_g[g,a] <- sum (statut[t_naiss==g & is.element(statut[,t],codes_act),t])/
                     sum (statut[t_naiss==g & statut[,t]>0                    ,t])
    duree_g[g,a]  <- duree_g[g,a-1]+emploi_g[g,a]
    
    salmoy_gH[g,a] <- mean(salaire[t_naiss==g & sexe==1 & salaire[,t]>0,t])/Prix[t]
    emploi_gH[g,a] <- sum (statut[t_naiss==g & sexe==1 & is.element(statut[,t],codes_act),t])/
      sum (statut[t_naiss==g & sexe==1 & statut[,t]>0                    ,t])
    duree_gH[g,a]  <- duree_gH[g,a-1]+emploi_gH[g,a]
    
    salmoy_gF[g,a] <- mean(salaire[t_naiss==g & sexe==2 &  salaire[,t]>0,t])/Prix[t]
    emploi_gF[g,a] <- sum (statut[t_naiss==g & sexe==2 & is.element(statut[,t],codes_act),t])/
      sum (statut[t_naiss==g & sexe==2 & statut[,t]>0                    ,t])
    duree_gF[g,a]  <- duree_gF[g,a-1]+emploi_gF[g,a]
  }
}

for (t in 90:160)
{
  for (a in 20:60)
  {
    g <- t-a
    salmoy_t[t,a] <- mean(salaire[t_naiss==g & salaire[,t]>0,t])/Prix[t]
    emploi_t[t,a] <- sum (statut[t_naiss==g & is.element(statut[,t],codes_act),t])/
                     sum (statut[t_naiss==g & statut[,t]>0                    ,t])

    salmoy_tH[t,a] <- mean(salaire[t_naiss==g & sexe==1 & salaire[,t]>0,t])/Prix[t]
    emploi_tH[t,a] <- sum (statut[t_naiss==g & sexe==1 & is.element(statut[,t],codes_act),t])/
      sum (statut[t_naiss==g & statut[,t]>0                    ,t])
    
    salmoy_tF[t,a] <- mean(salaire[t_naiss==g & sexe==2 & salaire[,t]>0,t])/Prix[t]
    emploi_tF[t,a] <- sum (statut[t_naiss==g & sexe==2 & is.element(statut[,t],codes_act),t])/
      sum (statut[t_naiss==g & statut[,t]>0                    ,t])
  }
}

# Graphes 3D 
pas <- 2
persp(seq(1940,2000,pas),seq(20,60,pas),salmoy_g[seq(40,100,pas),seq(20,60,pas)],
      ticktype="detailed",theta=30 ,phi=40,r=3,d=3,shade=.1,
      main="Salaire moyen par age et génération",xlab="Generation",ylab="Age",zlab="")
persp(seq(1990,2060,pas),seq(20,60,pas),salmoy_t[seq(90,160,pas),seq(20,60,pas)],
      ticktype="detailed",theta=-40 ,phi=20,r=3,d=3,shade=.1,
      main="Salaire moyen par age et période",xlab="Periode"   ,ylab="Age",zlab="")
persp(seq(1940,2000,pas),seq(20,60,pas),emploi_g[seq(40,100,pas),seq(20,60,pas)],
      ticktype="detailed",theta=150,phi=40,r=3,d=3,shade=.1,
      main="Taux d'emploi par age et génération, vue 1",xlab="Generation",ylab="Age",zlab="")
persp(seq(1940,2000,pas),seq(20,60,pas),emploi_g[seq(40,100,pas),seq(20,60,pas)],
      ticktype="detailed",theta=30 ,phi=40,r=3,d=3,shade=.1,
      main="Taux d'emploi par age et génération, vue 2",xlab="Generation",ylab="Age",zlab="")
persp(seq(1990,2060,pas),seq(20,60,pas),emploi_t[seq(90,160,pas),seq(20,60,pas)],
      ticktype="detailed",theta=150 ,phi=40,r=3,d=3,shade=.1,
      main="Taux d'emploi par age et période, vue 1",xlab="Periode"   ,ylab="Age",zlab="")
persp(seq(1990,2060,pas),seq(20,60,pas),emploi_t[seq(90,160,pas),seq(20,60,pas)],
      ticktype="detailed",theta=30 ,phi=40,r=3,d=3,shade=.1,
      main="Taux d'emploi par age et période, vue 2",xlab="Periode"   ,ylab="Age",zlab="")
persp(seq(1940,2000,pas),seq(20,60,pas),duree_g[seq(40,100,pas) ,seq(20,60,pas)],
      ticktype="detailed",theta=30 ,phi=40,r=3,d=3,shade=.1,
      main="Taux d'emploi cumulé (durée) par age et génération",xlab="Generation",ylab="Age",zlab="")

