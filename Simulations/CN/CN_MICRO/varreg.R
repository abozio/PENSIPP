# Variable expliquée: gain (en pourcentage)
# Variables explicatives: 
# - Bénéficiaires des dispositifs ANC
# - Durée en emploi à 60 ans
# - Sexe
# - Age de fin d'étude
# - t_liq < 135 
# - Cadre
# - décile 



# I. Data:


# Bénéficiaires de dispositifs: 
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC_CN2.RData"))
listPA<-which(pliq_CN[,5]>pliq_CN[,6])
listDF<-which(pliq_CN[,2]>pliq_CN[,5])
listMAJ<-which(pliq_CN[,2]>pliq_CN[,3])
listMDA<-which(pliq_CN[,3]>pliq_CN[,4])
listAVPF<-which(pliq_CN[,4]>pliq_CN[,5])
listMC<-which(pliq_CN[,1]>pliq_CN[,2])
save (listPA,listDF,listMC,listMAJ,listMDA,listAVPF,file=(paste0(cheminsource,"Simulations/CN/CN_MICRO/benefANC.RData")))

# Duree validée à 50 ans et cadre
load(paste0(cheminsource,"Simulations/CN/scenario de reference/VariantesMacro.RData"))
duree<-numeric(taille_max)
dureemaj<-numeric(taille_max)
pccadre<-numeric(taille_max)
part<-numeric(taille_max)
nbSP<-numeric(taille_max)
o<-0
c<-0
for (i in 1:55000)
{
# 
# DurBase(i,t_naiss[i]+50)  
# DurMajo(i,t_naiss[i]+50)
# 
# duree[i]   <-duree_tot
# dureemaj[i]<-duree_tot_maj
# o <- Duree(i,1,160,codes_occ)
# c <- Duree(i,1,160,cadre)
# if (o>0) {pccadre[i]<-c/o}
# Part du salaire au-dessus du plafond
partSP<-numeric(200)
partSP[salaire[i,]>0]<- pmax(0,salaire[i,salaire[i,]>0]-PlafondSS[salaire[i,]>0])
nSP<-length(which( salaire[i,salaire[i,]>0]>PlafondSS[salaire[i,]>0]))
if (length(which( salaire[i,]>0))>0) {nbSP[i]<-nSP/length(which( salaire[i,]>0))}
if (length(which( salaire[i,]>0))>0) {part[i]<-sum(partSP[salaire[i,]>0])/sum(salaire[salaire[i,]>0])}
}  



save (duree, dureemaj,pccadre,nbSP,part,file=(paste0(cheminsource,"Simulations/CN/CN_MICRO/cad.RData")))


# Chargement des données
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/CN/CN_MICRO/micro2.RData"))
# Points bizarres: liquidants_rg alors que carrière fp: enlever
bizar <- (which(dureefp>2 & pliq_fp[,1]==0 & t_liq>115))
liquidants_rg <- setdiff(liquidants_rg,bizar)

# Y
id<-liquidants_rg
reg<-as.data.frame(id)

gains <-numeric(taille_max)
gains <-(pliq_[,2]-pliq_[,1])/pliq_[,1]
reg$gains<-gains[liquidants_rg]

# Xs

# Sexe:
sexe2<-sexe
sexe2[sexe==2]<-0
reg$male<-sexe2[liquidants_rg]


# t_liq + dummy 
reg$t_liq<-t_liq[liquidants_rg]
liq2035<-numeric(taille_max)
liq2035[t_liq>134]<-1
reg$liq2035<-liq2035[liquidants_rg]

# Findet
reg$findet<-findet[liquidants_rg]

# ANC
load(paste0(cheminsource,"Simulations/CN/CN_MICRO/benefANC.RData"))
PA <- numeric(taille_max)
DF <- numeric(taille_max)
MDA <- numeric(taille_max)
AVPF<- numeric(taille_max)
MAJ <- numeric(taille_max)
MC <- numeric(taille_max)
ANC<-  numeric(taille_max)
listANC<-union(listPA,listDF)
listANC<-union(listANC,listMC)
PA[listPA]<-1
DF[listDF]<-1
MAJ[listMAJ]<-1
MDA[listMDA]<-1
AVPF[listAVPF]<-1
MC[listMC]<-1
ANC[listANC]<-1
reg$PA<-PA[liquidants_rg]
reg$DF<-DF[liquidants_rg]
reg$MAJ<-MAJ[liquidants_rg]
reg$MDA<-MDA[liquidants_rg]
reg$AVPF<-AVPF[liquidants_rg]
reg$MC<-MC[liquidants_rg]
reg$ANC<-ANC[liquidants_rg]

z <- union(listMDA,listMAJ
           )

# Déciles (LOOP!)
q1 <- quantile(pliq_[liquidants_rg,1],probs = seq(0, 1, 0.10))
dec <- numeric(taille_max)
for (k in 2:11)
{
  dec[ liquidants_rg[which( pliq_[liquidants_rg,1]>q1[k-1] & pliq_[liquidants_rg,1]<q1[k])]]<-(k-1)
}
reg$dec<-dec[liquidants_rg]
d1<-numeric(length(liquidants_rg))
d1[dec[liquidants_rg]==1]<-1
reg$d1<-d1
d2<-numeric(length(liquidants_rg))
d2[dec[liquidants_rg]==2]<-1
reg$d2<-d2
d3<-numeric(length(liquidants_rg))
d3[dec[liquidants_rg]==3]<-1
reg$d3<-d3
d4<-numeric(length(liquidants_rg))
d4[dec[liquidants_rg]==4]<-1
reg$d4<-d4
d5<-numeric(length(liquidants_rg))
d5[dec[liquidants_rg]==5]<-1
reg$d5<-d5
d6<-numeric(length(liquidants_rg))
d6[dec[liquidants_rg]==6]<-1
reg$d6<-d6
d7<-numeric(length(liquidants_rg))
d7[dec[liquidants_rg]==7]<-1
reg$d7<-d7
d8<-numeric(length(liquidants_rg))
d8[dec[liquidants_rg]==8]<-1
reg$d8<-d8
d9<-numeric(length(liquidants_rg))
d9[dec[liquidants_rg]==9]<-1
reg$d9<-d9
d10<-numeric(length(liquidants_rg))
d10[dec[liquidants_rg]==10]<-1
reg$d10<-d10

# Duree
load(paste0(cheminsource,"Simulations/CN/CN_MICRO/dur50.RData"))
reg$dur<-duree[liquidants_rg]
reg$cad<-pccadre[liquidants_rg]
reg$cad2<-0
reg$cad2[pccadre[liquidants_rg]>0.75]<-1
reg$partSP<-part[liquidants_rg]
# cadre
load(paste0(cheminsource,"Simulations/CN/CN_MICRO/cad.RData"))
reg$partsp<-part[liquidants_rg]
reg$nbsp<-nbSP[liquidants_rg]

save(reg,file=paste0(cheminsource,"Simulations/CN/CN_MICRO/varreg3.RData"))

