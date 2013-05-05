############  GRAPHIQUES ET TABLEAUX DE LA PARTIE 6   ##########

# Importation des données
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/CN/CN_MACRO/CN1.RData"))
b <- pliq_[,2]
load(paste0(cheminsource,"Simulations/CN/CN_MICRO/micro2.RData"))
pliq_[,3]<-b

# Points bizarres: liquidants_rg alors que carrière fp: enlever
bizar <- (which(dureefp>2 & pliq_fp[,1]==0 & t_liq>115))
liquidants_rg <- setdiff(liquidants_rg,bizar)
length(liquidants_rg)

####### Graphique 6.1 : Comparaison pensions avant/après ######
#par(mar=c(6.1, 3.1, 4.1, 2.1))
par(mar = c(4.1, 4.1, 4.1, 2.1))
plot  (pliq_[liquidants_rg ,1]/1e3,pliq_[liquidants_rg ,2]/1e3,yaxs="i",xaxs="i",
       xlim=c(0,110),ylim=c(0,110),pch="°",
       xlab="",ylab="")
abline(0,1,col="red")
mtext(side = 1, text = "Pension avant réforme", line = 2.5)
mtext(side = 2, text = "Pension après réforme", line = 2.5)
title("Graphe 5.1 : \nPensions avant et après réforme \n(pension annuelle, en milliers d'euros)",cex.main=0.9)

# version article
par(mar = c(2.1, 2.1, 1.5, 1.1))
plot  (pliq_[liquidants_rg ,1]/1e3,pliq_[liquidants_rg ,2]/1e3,yaxs="i",xaxs="i",
       xlim=c(0,110),ylim=c(0,110),pch="°",
       xlab="",ylab="")
abline(0,1,col="red")
mtext(side = 1, text = "Pension avant réforme", line = 2.5)
mtext(side = 2, text = "Pension après réforme", line = 2.5)


######## Tableau 5.1 : Gains moyens par décile de pension dans le système initial ######
gains <-numeric(taille_max)
gains2<-numeric(taille_max)
#gains3<-numeric(taille_max)

gains [liquidants_rg]<-(pliq_[liquidants_rg,2]-pliq_[liquidants_rg,1])/pliq_[liquidants_rg,1]
gains2[liquidants_rg]<-(pliq_[liquidants_rg,2]-pliq_[liquidants_rg,1])
q1 <- quantile(pliq_[liquidants_rg,1],probs = seq(0, 1, 0.10))
dec <- numeric(length(liquidants_rg))
Mdec <- numeric(10)
Mdec2<- numeric(10)
for (k in 2:11)
{
dec[ liquidants_rg[which( pliq_[liquidants_rg,1]>q1[k-1] & pliq_[liquidants_rg,1]<q1[k])]]<-(k-1)
}
for (k in 1:10)
{
Mdec[k]<-mean(gains[liquidants_rg[which(dec[liquidants_rg]==k)]])
Mdec2[k]<-mean(gains2[liquidants_rg[which(dec[liquidants_rg]==k)]])
}
x <- c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")
par(mar = c(4.1, 2.1, 4.1, 2.1))
plot   (Mdec,xlab="", ylab="", xaxt="n",cex.axis=0.8,
        ylim=c(-1,1),lwd=4,col="grey0",type="l")
points (Mdec2,xlab="", ylab="", xaxt="n",cex.axis=0.8,
                    ylim=c(-1,1),lwd=4,col="grey40",type="l")
axis(1, at=1:10, labels=x,cex.axis=0.8)
abline(h=0,lty=2,col="grey80")
title("Graphe 5.2 : \nGains à la réforme par décile \n(pension ancien système - pension nouveau système)", cex.main=0.9)


####### Régressions linéaires  #########
# voir varreg.R pour l'organisation des variables
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/CN/CN_MICRO/varreg2.RData"))
reg$sexe<-reg$male
reg$cadre<-reg$cad

library(texreg)

# Déterminants de 
attach(reg)
m1 <- lm(reg$gains ~ 
                      d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 +
                     dur + cad  
                    # liq2035  +
                    # ANC + 
                    # PA + MC + DF 
        )
summary(m1)["coefficients"]

m2 <- lm(reg$gains ~ 
                d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 +
                partsp +
                cad +
                male 
             # liq2035  +
             # ANC + 
         #     PA + dur
       #  + MC + DF 
)
summary(m2)["coefficients"]

m3 <- lm(reg$gains ~ 
           #d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 +
           cad +
           nbsp +
           t_liq + 
           dur +
         
          ANC 
         # PA + MC + DF 
)
summary(m3)
summary(m3)["coefficients"]


texreg(list(m1,m2,m3), booktabs = TRUE, dcolumn = TRUE)


# Effet des dispositifs non contributifs

m1 <- lm(reg$gains ~ ANC 
             #  d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 
             # dur + cad + 
             # liq2035  +
             # ANC + 
             # PA + MC + DF 
        )
summary(m1)["coefficients"]


m2 <- lm(reg$gains ~ MC + PA + DF
              #  d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 
              # dur + cad + 
              # liq2035  +
              # ANC + 
              # PA + MC + DF 
)
summary(m2)["coefficients"]


m3 <- lm(reg$gains ~      d1 + d10 +
                          MC + MC*d1 + MC*d10 +
                          PA + PA*d1 + PA*d10 +
                          DF + DF*d1 + DF*d10
             #  d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 
             # dur + cad + 
             # liq2035  +
             # ANC + 
             # PA + MC + DF 
)
summary(m3)["coefficients"]

m3a <- lm(reg$gains ~ MC + d1 + d10 + MC*d1 + MC*d10
              #  d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 
              # dur + cad + 
              # liq2035  +
              # ANC + 
              # PA + MC + DF 
)
summary(m3a)["coefficients"]

m3b <- lm(reg$gains ~ PA + d1 + d10 + PA*d1 + PA*d10
              #  d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 
              # dur + cad + 
              # liq2035  +
              # ANC + 
              # PA + MC + DF 
)
summary(m3b)["coefficients"]

m3c <- lm(reg$gains ~ DF + d1 + d10 + DF*d1 + DF*d10
              #  d2 + d3 + d4 + d5 + d6 + d7 + d8 + d9 + d10 
              # dur + cad + 
              # liq2035  +
              # ANC + 
              # PA + MC + DF 
)
summary(m3c)["coefficients"]



########### Graphique 6.3: Ratios interdéciles dans les différents scénarios ######
# Initialisation
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/CN/CN_MACRO/CN1.RData"))
b <- pliq_[,2]
load(paste0(cheminsource,"Simulations/CN/CN_MICRO/micro2.RData"))
pliq_[,3]<-b
# Points bizarres: liquidants_rg alors que carrière fp: enlever
bizar <- (which(dureefp>2 & pliq_fp[,1]==0 & t_liq>115))
liquidants_rg <- setdiff(liquidants_rg,bizar)

q1 <- quantile(pliq_[liquidants_rg,1],probs = seq(0, 1, 0.10))
q2 <- quantile(pliq_[liquidants_rg,2],probs = seq(0, 1, 0.10))
q3 <- quantile(pliq_[liquidants_rg,3],probs = seq(0, 1, 0.10))
y1 <- c(q1[2]/q1[2],q1[3]/q1[2],q1[4]/q1[2],q1[5]/q1[2],q1[6]/q1[2],q1[7]/q1[2],q1[8]/q1[2],q1[9]/q1[2],q1[10]/q1[2])
y2 <- c(q2[2]/q2[2],q2[3]/q2[2],q2[4]/q2[2],q2[5]/q2[2],q2[6]/q2[2],q2[7]/q2[2],q2[8]/q2[2],q2[9]/q2[2],q2[10]/q2[2])
y3 <- c(q3[2]/q3[2],q3[3]/q3[2],q3[4]/q3[2],q3[5]/q3[2],q3[6]/q3[2],q3[7]/q3[2],q3[8]/q3[2],q3[9]/q3[2],q3[10]/q3[2])
x <- c("D1","D2/D1","D3/D1","D4/D1","D5/D1","D6/D1","D7/D1","D8/D1","D9/D1")
par(mar = c(4.1, 2.1, 4.1, 1.1))
plot   (y1,xlab="", ylab="", xaxt="n",
        ylim=c(min( y1,y2,y3,y3,na.rm=TRUE),max( y1,y2,y3,na.rm=TRUE)),lwd=4,col="grey0",type="l")
points (y2,xlab="", ylab="", xaxt="n",
        ylim=c(min( y1,y2,y3,na.rm=TRUE),max( y1,y2,y3,na.rm=TRUE)),lwd=4,col="grey80",type="l")
points (y3,xlab="", ylab="", xaxt="n",
        ylim=c(min( y1,y2,y3,na.rm=TRUE),max( y1,y2,y3,na.rm=TRUE)),lwd=4,col="grey40",type="l")
axis(1, at=1:9, labels=x,cex.axis=0.7)
title("Graphe 5.2 : \nRapports interdéciles (pensions à liquidation) \ndans les différents systèmes", cex.main=0.9)
legend("toplef",,cex=0.8,
       c("Système actuel","Comptes notionnels 23% ANC", "Comptes notionnels 25.8% sans ANC"), 
       fill=c("grey0","grey80","grey40"))


# Version article
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load(paste0(cheminsource,"Simulations/CN/CN_MACRO/CN1.RData"))
b <- pliq_[,2]
load(paste0(cheminsource,"Simulations/CN/CN_MICRO/micro2.RData"))
pliq_[,3]<-b
# Points bizarres: liquidants_rg alors que carrière fp: enlever
bizar <- (which(dureefp>2 & pliq_fp[,1]==0 & t_liq>115))
liquidants_rg <- setdiff(liquidants_rg,bizar)

q1 <- quantile(pliq_[liquidants_rg,1],probs = seq(0, 1, 0.10))
q2 <- quantile(pliq_[liquidants_rg,2],probs = seq(0, 1, 0.10))
q3 <- quantile(pliq_[liquidants_rg,3],probs = seq(0, 1, 0.10))
y1 <- c(q1[2]/q1[2],q1[3]/q1[2],q1[4]/q1[2],q1[5]/q1[2],q1[6]/q1[2],q1[7]/q1[2],q1[8]/q1[2],q1[9]/q1[2],q1[10]/q1[2])
y2 <- c(q2[2]/q2[2],q2[3]/q2[2],q2[4]/q2[2],q2[5]/q2[2],q2[6]/q2[2],q2[7]/q2[2],q2[8]/q2[2],q2[9]/q2[2],q2[10]/q2[2])
y3 <- c(q3[2]/q3[2],q3[3]/q3[2],q3[4]/q3[2],q3[5]/q3[2],q3[6]/q3[2],q3[7]/q3[2],q3[8]/q3[2],q3[9]/q3[2],q3[10]/q3[2])
x <- c("D1","D2/D1","D3/D1","D4/D1","D5/D1","D6/D1","D7/D1","D8/D1","D9/D1")
par(mar = c(2.1, 2.1, 1.5, 1.1))
plot   (y1,xlab="", ylab="", xaxt="n",
        ylim=c(min( y1,y2,y3,y3,na.rm=TRUE),max( y1,y2,y3,na.rm=TRUE)),lwd=4,col="grey0",type="l")
points (y2,xlab="", ylab="", xaxt="n",
        ylim=c(min( y1,y2,y3,na.rm=TRUE),max( y1,y2,y3,na.rm=TRUE)),lwd=4,col="grey80",type="l")
points (y3,xlab="", ylab="", xaxt="n",
        ylim=c(min( y1,y2,y3,na.rm=TRUE),max( y1,y2,y3,na.rm=TRUE)),lwd=4,col="grey40",type="l")
axis(1, at=1:9, labels=x,cex.axis=0.7)
legend("topleft",,cex=0.8,
       c("Système actuel","Comptes notionnels 23% avec ANC", "Comptes notionnels 25.8% sans ANC"), 
       fill=c("grey0","grey80","grey40"))



####### Brouillons ######

# Courbes de Lorenz 

lorenz<-as.data.frame(pliq_[liquidants_rg,])
lorenz$V4 <- NULL 
lorenz$id<-sequence(length(liquidants_rg))
library(ineq)
par(mar = c(6.1, 4.1,2.1, 2.1))
plot(Lc(lorenz$V1),lwd=2,col="grey0",main="")
par(new=TRUE)
plot(Lc(lorenz$V2),lwd=2,col="grey80", main="")
#par(new=TRUE)
#plot(Lc(lorenz$V3),lwd=4,col="grey40")
legend("topleft",cex=0.8,
       c("Système actuel","Comptes notionnels"), 
       fill=c("grey0","grey80"))
title("Graphique 5.2: Courbes de Lorenz",cex.main=0.9)


# Graphe 6-2: Impact des ANC

load(paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC2.RData"))
# 2: ts ANC  3: no MC  6: No DF : 7: No ANC
q1 <- quantile(pliq_[liquidants_rg,1],probs = seq(0, 1, 0.10))
#q2 <- quantile(pliq_[liquidants_rg,2],probs = seq(0, 1, 0.10))
#q3 <- quantile(pliq_[liquidants_rg,5],probs = seq(0, 1, 0.10))
#q4 <- quantile(pliq_[liquidants_rg,7],probs = seq(0, 1, 0.10))
gains<-numeric(taille_max)
gains1<-numeric(taille_max)
gains2<-numeric(taille_max)
gains3<-numeric(taille_max)
gains4<-numeric(taille_max)
gains[liquidants_rg]<-(pliq_[liquidants_rg,1]-pliq_[liquidants_rg,7])/pliq_[liquidants_rg,1]
gains2[liquidants_rg]<-(pliq_[liquidants_rg,1]-pliq_[liquidants_rg,2])/pliq_[liquidants_rg,1]
gains3[liquidants_rg]<-(pliq_[liquidants_rg,3]-pliq_[liquidants_rg,5])/pliq_[liquidants_rg,3]
gains4[liquidants_rg]<-(pliq_[liquidants_rg,5]-pliq_[liquidants_rg,7])/pliq_[liquidants_rg,5]
dec <- numeric(length(liquidants_rg))
Mdec <- numeric(10)
Mdec2<- numeric(10)
Mdec3<- numeric(10)
Mdec4<- numeric(10)
for (k in 2:11)
{
  dec[ liquidants_rg[which( pliq_[liquidants_rg,1]>q1[k-1] & pliq_[liquidants_rg,1]<q1[k])]]<-(k-1)
}
for (k in 1:10)
{
  Mdec[k]<-mean(gains[liquidants_rg[which(dec[liquidants_rg]==k)]])*100 
  Mdec2[k]<-mean(gains2[liquidants_rg[which(dec[liquidants_rg]==k)]])*100
  Mdec3[k]<-mean(gains3[liquidants_rg[which(dec[liquidants_rg]==k)]])*100
  Mdec4[k]<-mean(gains4[liquidants_rg[which(dec[liquidants_rg]==k)]],na.rm=TRUE)*100
}




load(paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC_CN2.RData"))
# 2: ts ANC  3: no MC  6: No DF : 7: No ANC
q1 <- quantile(pliq_[liquidants_rg,2],probs = seq(0, 1, 0.10))
#q2 <- quantile(pliq_[liquidants_rg,2],probs = seq(0, 1, 0.10))
#q3 <- quantile(pliq_[liquidants_rg,5],probs = seq(0, 1, 0.10))
#q4 <- quantile(pliq_[liquidants_rg,7],probs = seq(0, 1, 0.10))
gains<-numeric(taille_max)
gains1<-numeric(taille_max)
gains2<-numeric(taille_max)
gains3<-numeric(taille_max)
gains4<-numeric(taille_max)
gains[liquidants_rg]<-(pliq_[liquidants_rg,2]-pliq_[liquidants_rg,7])/pliq_[liquidants_rg,2]
gains2[liquidants_rg]<-(pliq_[liquidants_rg,2]-pliq_[liquidants_rg,3])/pliq_[liquidants_rg,2]
gains3[liquidants_rg]<-(pliq_[liquidants_rg,3]-pliq_[liquidants_rg,6])/pliq_[liquidants_rg,3]
gains4[liquidants_rg]<-(pliq_[liquidants_rg,6]-pliq_[liquidants_rg,7])/pliq_[liquidants_rg,6]
dec <- numeric(length(liquidants_rg))
MdecCN <- numeric(10)
MdecCN2<- numeric(10)
MdecCN3<- numeric(10)
MdecCN4<- numeric(10)
for (k in 2:11)
{
  dec[ liquidants_rg[which( pliq_[liquidants_rg,1]>q1[k-1] & pliq_[liquidants_rg,1]<q1[k])]]<-(k-1)
}
for (k in 1:10)
{
  MdecCN[k]<-mean(gains[liquidants_rg[which(dec[liquidants_rg]==k)]],na.rm=TRUE)*100
  MdecCN2[k]<-mean(gains2[liquidants_rg[which(dec[liquidants_rg]==k)]],na.rm=TRUE)*100
  MdecCN3[k]<-mean(gains3[liquidants_rg[which(dec[liquidants_rg]==k)]],na.rm=TRUE)*100
  MdecCN4[k]<-mean(gains4[liquidants_rg[which(dec[liquidants_rg]==k)]],na.rm=TRUE)*100
}


x <- c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")
par(mar = c(2.1, 2.1, 1.5, 1.1))
y1 <- Mdec
y2 <- MdecCN
plot   (y1,xlab="", ylab="", xaxt="n",
        ylim=c(min( y1,y2,na.rm=TRUE),max( y1,y2,na.rm=TRUE)),lwd=4,col="grey0",type="l")
points (y2,xlab="", ylab="", xaxt="n",
        ylim=c(min( y1,y2,na.rm=TRUE),max( y1,y2,na.rm=TRUE)),lwd=4,col="grey80",type="l")
legend("topleft",,cex=0.8,
       c("Système actuel","Comptes notionnels"), 
       fill=c("grey0","grey80"))

x <- c("D1","D2","D3","D4","D5","D6","D7","D8","D9","D10")
barplot(matrix(c(Mdec[1],MdecCN[1],Mdec[2],MdecCN[2],Mdec[3],MdecCN[3],
                 Mdec[4],MdecCN[4],Mdec[5],MdecCN[5],Mdec[6],MdecCN[6],
                 Mdec[7],MdecCN[7],Mdec[8],MdecCN[8],Mdec[9],MdecCN[9],
                 Mdec[10],MdecCN[10]),nr=2), beside=T, 
        col=c("grey80","grey0"), ylim=c(0,70),
        names.arg=x)
box()
legend("top", c("Scénario de référence","Comptes notionnels"), pch=15, 
       col=c("grey80","grey0"), 
       bty=)