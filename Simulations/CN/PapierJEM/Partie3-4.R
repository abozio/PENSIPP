######### Graphes partie 3 #########


#Graphe 3.1
load(paste0(cheminsource,"Simulations/CN/Scenario de reference/EffetsReformes.RData"))
par(mar=c(2.1, 2.1, 1.5, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),RATIOPENS[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(min(RATIOPENS[,110:159],na.rm=TRUE),max(RATIOPENS[,110:159],na.rm=TRUE)),col="grey90",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[2,110:159],lwd=4,col="grey80",type="l")
points (seq(2010,2059,by=1),RATIOPENS[3,110:159],lwd=4,col="grey40",type="l")
points (seq(2010,2059,by=1),RATIOPENS[4,110:159],lwd=4,col="grey0",type="l")
legend.text <- c("Legislation 2012","Legislation 2003","Legislation 1993","Legislation 1992")
legend("topleft",cex=0.8,legend.text, fill=c("grey0","grey40","grey80","grey90"))


#Graphe 3.2
load(paste0(cheminsource,"Simulations/CN/Scenario de reference/VariantesMacro.RData"))
par(mar=c(2.1, 2.1, 1.5, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),RATIOPENS[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(0.11,0.16),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[2,110:159],lwd=4,col="grey40",type="l")
points (seq(2010,2059,by=1),RATIOPENS[3,110:159],lwd=4,col="grey80",type="l")
legend.text<-c("Scenario A (g=2%)","Scenario B (g=1.5%)","Scenario C (g=1%)")
legend("topleft",cex=0.8,legend.text, fill=c("grey80","grey0","grey40"))

RATIOPENSEF<-RATIOPENS
 

######### Graphes partie 4

##### Scénario 25.8 non ANC
load(paste0(cheminsource,"Simulations/CN/CN_MACRO/CN1.RData"))
# Graphe 4.1 : réforme
par(mar=c(2.1, 2.1, 1.5, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),RATIOPENS[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(0.11,0.14),col="grey80",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[2,110:159],lwd=4,col="grey0",type="l")
legend.text<-c("Scenario de référence","Comptes notionnels")
legend("topleft",cex=0.8,legend.text, fill=c("grey80","grey0"))


# Graphe 4.2: effets macro
par(oma=c(0,0,0,0))
par(mfrow=c(1,2))
par(mar=c(2.1, 2.1, 1.5, 1.1))
plot   (seq(2010,2059,by=1),RATIOPENS[2,110:159],
        xlab="", ylab=NULL,ylim=c(0.11,0.16),col="grey80",
        lwd=4,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
points (seq(2010,2059,by=1),RATIOPENS[3,110:159],lwd=4,col="grey40",type="l",yaxs="i",xaxs="i")
points (seq(2010,2059,by=1),RATIOPENS[4,110:159],lwd=4,col="grey0",type="l",yaxs="i",xaxs="i")
mtext(side = 3, text = "Comptes notionnels", line = 0.3,cex = 0.9)
#title("Scénario de référence", cex.main = 0.7)
legend.text<-c("g = 1.5%","g=1%","g=2%")
legend("topleft",legend.text, fill=c("grey80","grey40","grey0"), cex =0.8)


plot   (seq(2010,2059,by=1),RATIOPENSEF[1,110:159],
        xlab="", ylab=NULL,ylim=c(0.11,0.16),col="grey80",
                lwd=4,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
par(new=TRUE)
points (seq(2010,2059,by=1),RATIOPENSEF[2,110:159],lwd=4,col="grey40",type="l",yaxs="i",xaxs="i")
par(new=TRUE)
points (seq(2010,2059,by=1),RATIOPENSEF[3,110:159],lwd=4,col="grey0",type="l",yaxs="i",xaxs="i")
mtext(side = 3, text = "Scénario de référence", line = 0.3,cex = 0.9)
                #title("Scénario de référence", cex.main = 0.7)
legend.text<-c("g = 1.5%","g=1%","g=2%")
legend("topleft",legend.text, fill=c("grey80","grey40","grey0"), cex =0.8)

par(mfrow=c(1,1))

# Graphe 4.3 : gestion de la bosse (discount)

load("~/Desktop/PENSIPP 0.1/Simulations/CN/CN_MACRO/CNeq(90).RData")
par(oma=c(0,0,0,0))
par(mfrow=c(1,1))
par(mar=c(2.1, 2.1, 1.5, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),RATIOPENS[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(0.11,0.14),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[2,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(0.11,0.13),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[3,110:159],lwd=4,col="grey80",type="l")
legend.text <- c("Scénario de référence", "CN","CN réduction droits acquis (5%)")
legend("topleft",cex=0.8,legend.text, fill=c("grey0","grey80","grey40"))

load("~/Desktop/PENSIPP 0.1/Simulations/CN/CN_MACRO/CNeq(95).RData")
par(oma=c(0,0,0,0))
par(mfrow=c(1,1))
par(mar=c(2.1, 2.1, 1.5, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),RATIOPENS[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(0.11,0.14),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[2,110:159],lwd=4,col="grey40",type="l")
points (seq(2010,2059,by=1),RATIOPENS[3,110:159],lwd=4,col="grey80",type="l")
legend.text <- c("Scénario de référence","CN","CN réduction droits acquis (5%)")
legend("topleft",cex=0.8,legend.text, fill=c("grey0","grey80","grey40"))


##### Scénario 23% avec ANC
load(paste0(cheminsource,"Simulations/CN/CN_MACRO/CN2.RData"))
# Graphe 4.4 : réforme
par(mar=c(2.1, 2.1, 1.5, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),RATIOPENS[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(0.11,0.14),col="grey80",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[2,110:159],lwd=4,col="grey0",type="l")
legend.text<-c("Scenario de référence","Comptes notionnels")
legend("topleft",cex=0.8,legend.text, fill=c("grey80","grey0"))


# Graphe 4.5:  effets macro
par(oma=c(0,0,0,0))
par(mfrow=c(1,2))
par(mar=c(2.1, 2.1, 1.5, 1.1))
plot   (seq(2010,2059,by=1),RATIOPENS[2,110:159],
        xlab="", ylab=NULL,ylim=c(0.11,0.16),col="grey80",
        lwd=4,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
points (seq(2010,2059,by=1),RATIOPENS[3,110:159],lwd=4,col="grey40",type="l",yaxs="i",xaxs="i")
points (seq(2010,2059,by=1),RATIOPENS[4,110:159],lwd=4,col="grey0",type="l",yaxs="i",xaxs="i")
mtext(side = 3, text = "Comptes notionnels", line = 0.3,cex = 0.9)
#title("Scénario de référence", cex.main = 0.7)
legend.text<-c("g = 1.5%","g=1%","g=2%")
legend("topleft",legend.text, fill=c("grey80","grey40","grey0"), cex =0.8)


plot   (seq(2010,2059,by=1),RATIOPENSEF[1,110:159],
        xlab="", ylab=NULL,ylim=c(0.11,0.16),col="grey80",
        lwd=4,type="l",yaxs="i",xaxs="i",cex.axis=0.8)
par(new=TRUE)
points (seq(2010,2059,by=1),RATIOPENSEF[2,110:159],lwd=4,col="grey40",type="l",yaxs="i",xaxs="i")
par(new=TRUE)
points (seq(2010,2059,by=1),RATIOPENSEF[3,110:159],lwd=4,col="grey0",type="l",yaxs="i",xaxs="i")
mtext(side = 3, text = "Scénario de référence", line = 0.3,cex = 0.9)
#title("Scénario de référence", cex.main = 0.7)
legend.text<-c("g = 1.5%","g=1%","g=2%")
legend("topleft",legend.text, fill=c("grey80","grey40","grey0"), cex =0.8)

par(mfrow=c(1,1))

# Graphe 4.6 : gestion de la bosse (discount)

load("~/Desktop/PENSIPP 0.1/Simulations/CN/CN_MACRO/CNeq2(90).RData")
par(oma=c(0,0,0,0))
par(mfrow=c(1,1))
par(mar=c(2.1, 2.1, 1.5, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),RATIOPENS[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(0.11,0.14),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[2,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(0.11,0.13),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[3,110:159],lwd=4,col="grey80",type="l")
legend.text <- c("Scénario de référence", "CN (23% + ANC)","CN réduction droits acquis (10%)")
legend("topleft",cex=0.8,legend.text, fill=c("grey0","grey80","grey40"))

load("~/Desktop/PENSIPP 0.1/Simulations/CN/CN_MACRO/CNeq2(95).RData")
par(oma=c(0,0,0,0))
par(mfrow=c(1,1))
par(mar=c(2.1, 2.1, 1.5, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),RATIOPENS[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",
        ylim=c(0.11,0.14),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[2,110:159],lwd=4,col="grey40",type="l")
points (seq(2010,2059,by=1),RATIOPENS[3,110:159],lwd=4,col="grey80",type="l")
legend.text <- c("Scénario de référence","CN (23% + ANC)","CN réduction droits acquis (5%)")
legend("topleft",cex=0.8,legend.text, fill=c("grey0","grey80","grey40"))

# Graphe 4.7: evolution du coefficient de conversion. 
load(paste0(cheminsource,"Simulations/CN/CN_MICRO/micro2.RData"))
x<-(seq(2015,2059,by=1))
y1 <- CONV_MOY[2,115:159]
y2 <- AGELIQ[1,115:159]

par(mar=c(2.1, 4.1, 1.5, 4.1))
plot(x,y1,type="l",col="grey0",xlab="",ylab="",
     ylim=c(0.04,0.06),yaxs="i",xaxs="i",lwd=4)
par(new=TRUE)
plot(x,y2,type="l",col="grey80", xlab="",ylab="", yaxt="n",
     ylim=c(60,66),yaxs="i",xaxs="i",lwd=4)
par(new=TRUE)
axis(4)
mtext(side = 2, text = "Coefficient de conversion moyen", line = 2.5)
mtext(side = 4, text = "Age moyen de liquidation", line = 2.5)
legend("bottom",cex=0.9,c("Coefficient de conversion","Age de liquidation"), fill=c("grey0","grey80"))
