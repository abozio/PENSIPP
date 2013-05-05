############## Analyse micro ##################


######## Mico  ###########
# Graphique 4.1
x <- c(0,10000,20000,30000)
y1<-c(0,15000,20000,30000)
y2 <- c(0.20,0.20,0,0)
y3 <- c(0,10000,20000,30000)
plot(x,y1,type="l",col="grey0",xaxt="n",yaxt="n",ylab="",xlab="",xlim=c(0,max(x)),ylim=c(0,30000),yaxs="i",xaxs="i",lwd=4)
par(new=TRUE)
plot(x,y2,type="l",col="grey40", xaxt="n",yaxt="n",ylab="",xlab="",ylim=c(0,1),xlim=c(0,max(x)),yaxs="i",xaxs="i",lwd=4)
par(new=TRUE)
plot(x,y3,type="l",col="grey40", xaxt="n",yaxt="n",ylab="",xlab="",ylim=c(0,max(y3)),xlim=c(0,max(x)),yaxs="i",xaxs="i",lty=2)
abline(v=10000,lty=2,col="grey0") 
text(11000,1000, "seuil", col = "grey0",cex = 0.8)
abline(v=20000,lty=2,col="grey0") 
text(22000,1000, "plafond", col = "grey0",cex = 0.8)
mtext(side = 1, text = "Cotisation avant majoration", line = 1)
mtext(side = 2, text = "Cotisation avec majoration", line = 1)
title("Graphe 4.1 : \nLe minimum contributif dans le nouveau système",cex.main=0.9)
legend("topleft",cex=0.7,c("cotisations versées","taux de majoration"), fill=c("grey0","grey40"))

# Graphique 4.1 version article
par(mar=c(2.1, 2.1, 2.1, 2.1))
x <- c(0,10000,20000,30000)
y1<-c(0,15000,20000,30000)
y2 <- c(0.20,0.20,0,0)
y3 <- c(0,10000,20000,30000)
plot(x,y1,type="l",col="grey0",xaxt="n",yaxt="n",ylab="",xlab="",xlim=c(0,max(x)),ylim=c(0,30000),yaxs="i",xaxs="i",lwd=4)
par(new=TRUE)
plot(x,y2,type="l",col="grey40", xaxt="n",yaxt="n",ylab="",xlab="",ylim=c(0,1),xlim=c(0,max(x)),yaxs="i",xaxs="i",lwd=4)
par(new=TRUE)
plot(x,y3,type="l",col="grey40", xaxt="n",yaxt="n",ylab="",xlab="",ylim=c(0,max(y3)),xlim=c(0,max(x)),yaxs="i",xaxs="i",lty=2)
abline(v=10000,lty=2,col="grey0") 
text(11000,1000, "seuil", col = "grey0",cex = 0.9)
abline(v=20000,lty=2,col="grey0") 
text(22000,1000, "plafond", col = "grey0",cex = 0.9)
mtext(side = 1, text = "Cotisations avant majoration", line = 1)
mtext(side = 2, text = "Cotisations avec majoration", line = 1)
legend("topleft",cex=0.9,c("cotisations versées","taux de majoration"), fill=c("grey0","grey40"))



############ Masses ANC   ########
rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
load( paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC.RData"))

# Graphique 4.2
REF=c(mtotliqDF[125],mtotliqPA[125],mtotliqMC[125])
CN=c(mtotliqDF_cn[125],mtotliqPA_cn[125],mtotliqMC_cn[125])
par("mar")
par(mar=c(5.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
bp <- barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"), ylim=c(0,3),xlab="",xaxs="i")
box()
title("Graphe 4.2 : Masse des avantages non contributifs \n(Flux de liquidants 2025, en milliards)",cex.main=0.9,)
legend.text <- c("Droits familiaux","Périodes assimilées","Minima de pension")
legend(mean(range(bp)), -0.7, legend.text,cex=0.8, xjust = 0.5,yjust=1,fill=c("grey0","grey40","grey80"))

# Graphique 4.2 version article
par("mar")
par(mar=c(5.1, 3.1, 3.1, 2.1))
par(xpd=TRUE)
bp <- barplot(cbind(REF,CN),col=c("grey0","grey40","grey80"), ylim=c(0,3),xlab="",xaxs="i")
box()
legend.text <- c("Droits familiaux","Périodes assimilées","Minima de pension")
legend(mean(range(bp))-0.05, -0.7, legend.text,cex=0.8, xjust = 0.5,yjust=1,fill=c("grey0","grey40","grey80"), horiz=TRUE)


# Graphe 4.3: Composition des ANC au cours du temps
par(mar=c(5.1, 3.1, 3.5, 2.1))
par(xpd=TRUE)
plot   (seq(2000,2059,by=1),mtotliq[100:159],xlab="Annee", ylab="Masse ANC",ylim=c(0,5),col="grey0",lwd=4,type="l",yaxs="i")
points (seq(2000,2059,by=1),mtotliq_cn[100:159],lwd=4,col="grey80",type="l",yaxs="i")
title("Graphe 4.3.a : Evolution des masses ANC  \nFLUX (en milliards)", cex.main = 0.9)
legend.text<-c("Scenario de référence","Scenario CN")
legend("topleft",cex=0.8,legend.text, fill=c("grey0","grey80"))

# Graphe 4.3a: Composition des ANC au cours du temps
par(mar=c(5.1, 2.1, 2.1, 1.1))
mc <-  numeric(200)
df <-  numeric(200)
pa <-  numeric(200)
mc[]<-mtotliqMC[]+mtotliqPA[]+mtotliqDF[]
mc[81:2059]<-filter(mc[81:2059], c(1/3, 1/3, 1/3))  # moyenne mobile 3 ans
pa[]<-mtotliqPA[]+mtotliqDF[]
pa[81:2059]<-filter(pa[81:2059], c(1/3, 1/3, 1/3))
df[]<-mtotliqDF[]
df[81:2059]<-filter(df[81:2059], c(1/3, 1/3, 1/3))
mcCN <-  numeric(200)
dfCN <-  numeric(200)
paCN <-  numeric(200)
mcCN[]<-mtotliqMC_cn[]+mtotliqPA_cn[]+mtotliqDF_cn[]
paCN[]<-mtotliqPA_cn[]+mtotliqDF_cn[]
dfCN[]<-mtotliqDF_cn[]
mcCN[81:2059]<-filter(mcCN[81:2059], c(1/3, 1/3, 1/3)) 
dfCN[81:2059]<-filter(dfCN[81:2059], c(1/3, 1/3, 1/3)) 
paCN[81:2059]<-filter(paCN[81:2059], c(1/3, 1/3, 1/3)) 
par(oma=c(0,0,2,0))
par(mfrow=c(1,2))
plot   (seq(2000,2059,by=1),mc[100:159],xlab=NULL, ylab=NULL,ylim=c(0,5),col="grey80",lwd=4,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
par(new=TRUE)
points (seq(2000,2059,by=1),pa[100:159],lwd=4,col="grey40",type="l",yaxs="i",xaxs="i")
par(new=TRUE)
points (seq(2000,2059,by=1),df[100:159],lwd=4,col="grey0",type="l",yaxs="i",xaxs="i")
polygon (c(seq(2000,2059,by=1),2059,2000),c(mc[100:159],0,0),col="grey80")
polygon (c(seq(2000,2059,by=1),2059,2000),c(pa[100:159],0,0),col="grey40")
polygon (c(seq(2000,2059,by=1),2059,2000),c(df[100:159],0,0),col="grey0")
mtext(side = 3, text = "Scénario de référence", line = 0.6,cex = 0.8)
#title("Scénario de référence", cex.main = 0.7)
legend.text<-c("MC","PA","DF")
legend("topleft",legend.text, fill=c("grey80","grey40","grey0"), cex =0.6)


plot   (seq(2000,2059,by=1),mcCN[100:159],xlab=NULL, ylab=NULL,ylim=c(0,5),col="grey80",lwd=4,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
par(new=TRUE)
points (seq(2000,2059,by=1),paCN[100:159],lwd=4,col="grey40",type="l",yaxs="i",xaxs="i")
par(new=TRUE)
points (seq(2000,2059,by=1),dfCN[100:159],lwd=4,col="grey0",type="l",yaxs="i",xaxs="i")
mtext(side = 3, text = "Comptes notionnels", line = 0.6,cex = 0.8)
#title("Comptes notionnels", cex.main = 0.7)
polygon (c(seq(2000,2059,by=1),2059,2000),c(mcCN[100:159],0,0),col="grey80")
polygon (c(seq(2000,2059,by=1),2059,2000),c(paCN[100:159],0,0),col="grey40")
polygon (c(seq(2000,2059,by=1),2059,2000),c(dfCN[100:159],0,0),col="grey0")
legend.text<-c("MC","PA","DF")
legend("topleft",legend.text, fill=c("grey80","grey40","grey0"), cex =0.6)

mtext(side = 3, text = "Ratio pensions ANC / pensions totales", outer=TRUE,line = 0)
par(mfrow=c(1,1))

# Graphe 4.3a version article
par(mar=c(2.1, 2.1, 2.1, 1.1))
mc <-  numeric(200)
df <-  numeric(200)
pa <-  numeric(200)
mc[]<-mtotliqMC[]+mtotliqPA[]+mtotliqDF[]
mc[81:2059]<-filter(mc[81:2059], c(1/3, 1/3, 1/3))  # moyenne mobile 3 ans
pa[]<-mtotliqPA[]+mtotliqDF[]
pa[81:2059]<-filter(pa[81:2059], c(1/3, 1/3, 1/3))
df[]<-mtotliqDF[]
df[81:2059]<-filter(df[81:2059], c(1/3, 1/3, 1/3))
mcCN <-  numeric(200)
dfCN <-  numeric(200)
paCN <-  numeric(200)
mcCN[]<-mtotliqMC_cn[]+mtotliqPA_cn[]+mtotliqDF_cn[]
paCN[]<-mtotliqPA_cn[]+mtotliqDF_cn[]
dfCN[]<-mtotliqDF_cn[]
mcCN[81:2059]<-filter(mcCN[81:2059], c(1/3, 1/3, 1/3)) 
dfCN[81:2059]<-filter(dfCN[81:2059], c(1/3, 1/3, 1/3)) 
paCN[81:2059]<-filter(paCN[81:2059], c(1/3, 1/3, 1/3)) 
par(oma=c(0,0,0,0))
par(mfrow=c(1,2))
plot   (seq(2000,2059,by=1),mc[100:159],xlab=NULL, ylab=NULL,ylim=c(0,5),col="grey80",lwd=4,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
par(new=TRUE)
points (seq(2000,2059,by=1),pa[100:159],lwd=4,col="grey40",type="l",yaxs="i",xaxs="i")
par(new=TRUE)
points (seq(2000,2059,by=1),df[100:159],lwd=4,col="grey0",type="l",yaxs="i",xaxs="i")
polygon (c(seq(2000,2059,by=1),2059,2000),c(mc[100:159],0,0),col="grey80")
polygon (c(seq(2000,2059,by=1),2059,2000),c(pa[100:159],0,0),col="grey40")
polygon (c(seq(2000,2059,by=1),2059,2000),c(df[100:159],0,0),col="grey0")
mtext(side = 3, text = "Scénario de référence", line = 0.6,cex = 0.8)
#title("Scénario de référence", cex.main = 0.7)
legend.text<-c("MC","PA","DF")
legend("topleft",legend.text, fill=c("grey80","grey40","grey0"), cex =0.6)


plot   (seq(2000,2059,by=1),mcCN[100:159],xlab=NULL, ylab=NULL,ylim=c(0,5),col="grey80",lwd=4,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
par(new=TRUE)
points (seq(2000,2059,by=1),paCN[100:159],lwd=4,col="grey40",type="l",yaxs="i",xaxs="i")
par(new=TRUE)
points (seq(2000,2059,by=1),dfCN[100:159],lwd=4,col="grey0",type="l",yaxs="i",xaxs="i")
mtext(side = 3, text = "Comptes notionnels", line = 0.6,cex = 0.8)
#title("Comptes notionnels", cex.main = 0.7)
polygon (c(seq(2000,2059,by=1),2059,2000),c(mcCN[100:159],0,0),col="grey80")
polygon (c(seq(2000,2059,by=1),2059,2000),c(paCN[100:159],0,0),col="grey40")
polygon (c(seq(2000,2059,by=1),2059,2000),c(dfCN[100:159],0,0),col="grey0")
legend.text<-c("MC","PA","DF")
legend("topleft",legend.text, fill=c("grey80","grey40","grey0"), cex =0.6)


# Graphe 4.4 
partANC<-numeric(200)
partANC_CN<-numeric(200)
partANC<-mtot[]/MPENS[1,]*1e9
partANC_CN<-mtot_cn[]/MPENS_CN[1,]*1e9

x<-(seq(2010,2059,by=1))
par(mar=c(2.1, 4.1, 1.5, 4.1))
plot(x,mtot[110:159],type="l",col="grey0",xlab="",ylab="",
     ylim=c(0,100),yaxs="i",xaxs="i",lwd=4)
points(x,mtot_cn[110:159],lwd=4,col="grey80",type="l")
par(new=TRUE)
plot(x,partANC[110:159],type="l",col="grey0",xlab="",ylab="",yaxt="n",
     ylim=c(0,1),yaxs="i",xaxs="i",lwd=4,lty=2)
points(x,partANC_CN[110:159],lwd=4,col="grey80",type="l",yaxt="n",lty=2)
par(new=TRUE)
axis(4)
mtext(side = 2, text = "Masse des pensions ANC (en milliards)", line = 2.5)
mtext(side = 4, text = "Ratio pensions ANC / pension totale", line = 2.5)
legend.text<-c("masse ANC scénario de référence","masse ANC scénario CN",
               "masse ANC/masse totale scénario de référence","masse ANC/masse totale scénario CN")
legend("topleft",cex=0.8, legend.text, col=c("grey0", "grey80", "grey0", "grey80"),
       lty=c(1,1,2,2),bty="n")


######## Scénario 23 ANC  #########
load(paste0(cheminsource,"Simulations/CN/CN_MACRO/CN2.RData"))

# Graphe 4.5: MPENS
par(mar=c(4.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),MPENS[1,110:159]/1e9,xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(200,600),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),MPENS[2,110:159]/1e9,lwd=4,col="grey80",type="l")
title("Graphe 4.5 :\nMasse des pensions (en milliards d'euros)" , cex.main = 0.9)
legend.text<-c("Scénario de référence","Scénario CN")
#legend("bottom",inset=c(-0.2,-0.65),cex=0.8,legend.text, fill=c("grey80","grey0","grey40"))
legend("topleft",cex=0.75,legend.text, fill=c("grey0","grey80"))


# Graphe 4.6: Scénarios macro
par(mar=c(4.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),RATIOPENS[2,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(0.11,0.15),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[3,110:159],lwd=4,col="grey40",type="l")
points (seq(2010,2059,by=1),RATIOPENS[4,110:159],lwd=4,col="grey80",type="l")
title("Graphe 4.6 :\nRégime CN : Evolution du ratio retraite/PIB \nVariantes de scénario de croissance", cex.main = 0.9)
legend.text<-c("Scenario A (g=2%)","Scenario B (g=1.5%)","Scenario C (g=1%)")
#legend("bottom",inset=c(-0.2,-0.65),cex=0.8,legend.text, fill=c("grey80","grey0","grey40"))
legend("topleft",cex=0.75,legend.text, fill=c("grey80","grey0","grey40"))

