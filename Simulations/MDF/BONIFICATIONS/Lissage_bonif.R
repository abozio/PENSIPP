########## Lissage des déciles #######

rm(list = ls())
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"

#1. Tous liquidants

# Chargement des données: 
load(paste0(cheminsource,"Simulations/MDF/BONIFICATIONS/quant1.RData"))

# Création du data.frame: 
gene<-(seq(1930,1970,by=1))
data<-as.data.frame(gene)
data$S1_D1  <-Q1 [1,30:70]    # D1 scénario de référence
data$S1_D1_H<-Q1H[1,30:70]
data$S1_D1_F<-Q1F[1,30:70]
data$S2_D1  <-Q1 [2,30:70]     # D1 no bonif
data$S2_D1_H<-Q1H[2,30:70]
data$S2_D1_F<-Q1F[2,30:70]
data$S3_D1  <-Q1 [3,30:70]     # D1 réforme
data$S3_D1_H<-Q1H[3,30:70]
data$S3_D1_F<-Q1F[3,30:70]

data$S1_D9  <-Q9 [1,30:70]    # D9 scénario de référence
data$S1_D9_H<-Q9H[1,30:70]
data$S1_D9_F<-Q9F[1,30:70]
data$S2_D9  <-Q9 [2,30:70]     # D9 no bonif
data$S2_D9_H<-Q9H[2,30:70]
data$S2_D9_F<-Q9F[2,30:70]
data$S3_D9  <-Q9 [3,30:70]     # D9 réforme
data$S3_D9_H<-Q9H[3,30:70]
data$S3_D9_F<-Q9F[3,30:70]

data$S1_D5  <-Q5 [1,30:70]    # D9 scénario de référence
data$S1_D5_H<-Q5H[1,30:70]
data$S1_D5_F<-Q5F[1,30:70]
data$S2_D5  <-Q5 [2,30:70]     # D9 no bonif
data$S2_D5_H<-Q5H[2,30:70]
data$S2_D5_F<-Q5F[2,30:70]
data$S3_D5  <-Q5 [3,30:70]     # D9 réforme
data$S3_D5_H<-Q5H[3,30:70]
data$S3_D5_F<-Q5F[3,30:70]

#D1
attach(data)
y1F <- lm (S1_D1_F ~ poly (gene, 4, raw=TRUE))
y2F <- lm (S2_D1_F ~ poly (gene, 4, raw=TRUE))
y3F <- lm (S3_D1_F ~ poly (gene, 4, raw=TRUE))

y1H <- lm (S1_D1_H ~ poly (gene, 4, raw=TRUE))
y2H <- lm (S2_D1_H ~ poly (gene, 4, raw=TRUE))
y3H <- lm (S3_D1_H ~ poly (gene, 4, raw=TRUE))

par(mar = c(4.1, 4.1, 4.1, 2.1))
plot(gene,fitted(y1F), type="l",lwd=3,pch=16,
     ylim=c(2000,13000),col="rosybrown1",
     xlab="Année de naissance",ylab="Premier décile de pension")
lines(gene,fitted(y2F), lwd=3, col="red1")
lines(gene,fitted(y3F), lwd=3, col="red4")
lines(gene,fitted(y1H), lwd=3, col="lightskyblue")
lines(gene,fitted(y2H), lwd=3, col="blue")
lines(gene,fitted(y3H), lwd=3, col="darkblue")

abline(v=1948)
title(main= "D1: Evolution par génération \n(tous liquidants)",cex=0.8)
legend("top",legend=c("Législation actuelle H","No bonif H","Réforme H",
                      "Législation actuelle F","No bonif F","Réforme F"),
       lty=1,lwd=2,col=c("lightskyblue","blue","darkblue", "rosybrown1","red1","red4"),
       ncol=2,bty="n",cex=0.8,,
       )



# D5
y1F <- lm (S1_D5_F ~ poly (gene, 4, raw=TRUE))
y2F <- lm (S2_D5_F ~ poly (gene, 4, raw=TRUE))
y3F <- lm (S3_D5_F ~ poly (gene, 4, raw=TRUE))

y1H <- lm (S1_D5_H ~ poly (gene, 4, raw=TRUE))
y2H <- lm (S2_D5_H ~ poly (gene, 4, raw=TRUE))
y3H <- lm (S3_D5_H ~ poly (gene, 4, raw=TRUE))

par(mar = c(4.1, 4.1, 4.1, 2.1))
plot(gene,fitted(y1F), type="l",lwd=3,pch=16,
     ylim=c(10000,26000),col="rosybrown1",
     xlab="Année de naissance",ylab="Cinquième décile de pension")
lines(gene,fitted(y2F), lwd=3, col="red1")
lines(gene,fitted(y3F), lwd=3, col="red4")
lines(gene,fitted(y1H), lwd=3, col="lightskyblue")
lines(gene,fitted(y2H), lwd=3, col="blue")
lines(gene,fitted(y3H), lwd=3, col="darkblue")

abline(v=1948)
title(main= "D5: Evolution par génération \n (tous liquidants)",cex=0.8)
legend("top",legend=c("Législation actuelle H","No bonif H","Réforme H",
                      "Législation actuelle F","No bonif F","Réforme F"),
       lty=1,lwd=2,col=c("lightskyblue","blue","darkblue", "rosybrown1","red1","red4"),
       ncol=2,bty="n",cex=0.8,,
)



# D9
y1F <- lm (S1_D9_F ~ poly (gene, 4, raw=TRUE))
y2F <- lm (S2_D9_F ~ poly (gene, 4, raw=TRUE))
y3F <- lm (S3_D9_F ~ poly (gene, 4, raw=TRUE))

y1H <- lm (S1_D9_H ~ poly (gene, 4, raw=TRUE))
y2H <- lm (S2_D9_H ~ poly (gene, 4, raw=TRUE))
y3H <- lm (S3_D9_H ~ poly (gene, 4, raw=TRUE))

par(mar = c(4.1, 4.1, 4.1, 2.1))
plot(gene,fitted(y1F), type="l",lwd=3,pch=16,
     ylim=c(10000,55000),col="rosybrown1",
     xlab="Année de naissance",ylab="Neuvième décile de pension")
lines(gene,fitted(y2F), lwd=3, col="red1")
lines(gene,fitted(y3F), lwd=3, col="red4")
lines(gene,fitted(y1H), lwd=3, col="lightskyblue")
lines(gene,fitted(y2H), lwd=3, col="blue")
lines(gene,fitted(y3H), lwd=3, col="darkblue")

abline(v=1948)
title(main= "D9: Evolution par génération \n (tous liquidants)",cex=0.8)
legend("top",legend=c("Législation actuelle H","No bonif H","Réforme H",
                      "Législation actuelle F","No bonif F","Réforme F"),
       lty=1,lwd=2,col=c("lightskyblue","blue","darkblue", "rosybrown1","red1","red4"),
       ncol=2,bty="n",cex=0.8,,
)


# 2. Bénéficiaires

# Chargement des données: 
load(paste0(cheminsource,"Simulations/MDF/quant1enf.RData"))

# Création du data.frame: 
gene<-(seq(1930,1970,by=1))
data<-as.data.frame(gene)
data$S1_D1  <-Q1 [1,30:70]    # D1 scénario de référence
data$S1_D1_H<-Q1H[1,30:70]
data$S1_D1_F<-Q1F[1,30:70]
data$S2_D1  <-Q1 [2,30:70]     # D1 no bonif
data$S2_D1_H<-Q1H[2,30:70]
data$S2_D1_F<-Q1F[2,30:70]
data$S3_D1  <-Q1 [3,30:70]     # D1 réforme
data$S3_D1_H<-Q1H[3,30:70]
data$S3_D1_F<-Q1F[3,30:70]

data$S1_D9  <-Q9 [1,30:70]    # D9 scénario de référence
data$S1_D9_H<-Q9H[1,30:70]
data$S1_D9_F<-Q9F[1,30:70]
data$S2_D9  <-Q9 [2,30:70]     # D9 no bonif
data$S2_D9_H<-Q9H[2,30:70]
data$S2_D9_F<-Q9F[2,30:70]
data$S3_D9  <-Q9 [3,30:70]     # D9 réforme
data$S3_D9_H<-Q9H[3,30:70]
data$S3_D9_F<-Q9F[3,30:70]

data$S1_D5  <-Q5 [1,30:70]    # D9 scénario de référence
data$S1_D5_H<-Q5H[1,30:70]
data$S1_D5_F<-Q5F[1,30:70]
data$S2_D5  <-Q5 [2,30:70]     # D9 no bonif
data$S2_D5_H<-Q5H[2,30:70]
data$S2_D5_F<-Q5F[2,30:70]
data$S3_D5  <-Q5 [3,30:70]     # D9 réforme
data$S3_D5_H<-Q5H[3,30:70]
data$S3_D5_F<-Q5F[3,30:70]

#D1
attach(data)
y1F <- lm (S1_D1_F ~ poly (gene, 4, raw=TRUE))
y2F <- lm (S2_D1_F ~ poly (gene, 4, raw=TRUE))
y3F <- lm (S3_D1_F ~ poly (gene, 4, raw=TRUE))

y1H <- lm (S1_D1_H ~ poly (gene, 4, raw=TRUE))
y2H <- lm (S2_D1_H ~ poly (gene, 4, raw=TRUE))
y3H <- lm (S3_D1_H ~ poly (gene, 4, raw=TRUE))

par(mar = c(4.1, 4.1, 4.1, 2.1))
plot(gene,fitted(y1F), type="l",lwd=3,pch=16,
     ylim=c(2000,15000),col="rosybrown1",
     xlab="Année de naissance",ylab="Premier décile de pension")
lines(gene,fitted(y2F), lwd=3, col="red1")
lines(gene,fitted(y3F), lwd=3, col="red4")
lines(gene,fitted(y1H), lwd=3, col="lightskyblue")
lines(gene,fitted(y2H), lwd=3, col="blue")
lines(gene,fitted(y3H), lwd=3, col="darkblue")

abline(v=1948)
title(main= "D1: Evolution par génération \n(bénéficiaires des bonifications)",cex=0.8)
legend("topleft",legend=c("Législation actuelle H","No bonif H","Réforme H",
                      "Législation actuelle F","No bonif F","Réforme F"),
       lty=1,lwd=2,col=c("lightskyblue","blue","darkblue", "rosybrown1","red1","red4"),
       ncol=2,bty="n",cex=0.8,,
)



# D5
y1F <- lm (S1_D5_F ~ poly (gene, 4, raw=TRUE))
y2F <- lm (S2_D5_F ~ poly (gene, 4, raw=TRUE))
y3F <- lm (S3_D5_F ~ poly (gene, 4, raw=TRUE))

y1H <- lm (S1_D5_H ~ poly (gene, 4, raw=TRUE))
y2H <- lm (S2_D5_H ~ poly (gene, 4, raw=TRUE))
y3H <- lm (S3_D5_H ~ poly (gene, 4, raw=TRUE))

par(mar = c(4.1, 4.1, 4.1, 2.1))
plot(gene,fitted(y1F), type="l",lwd=3,pch=16,
     ylim=c(8000,26000),col="rosybrown1",
     xlab="Année de naissance",ylab="Cinquième décile de pension")
lines(gene,fitted(y2F), lwd=3, col="red1")
lines(gene,fitted(y3F), lwd=3, col="red4")
lines(gene,fitted(y1H), lwd=3, col="lightskyblue")
lines(gene,fitted(y2H), lwd=3, col="blue")
lines(gene,fitted(y3H), lwd=3, col="darkblue")

abline(v=1948)
title(main= "D5: Evolution par génération \n (bénéficiaires des bonifications)",cex=0.8)
legend("topleft",legend=c("Législation actuelle H","No bonif H","Réforme H",
                      "Législation actuelle F","No bonif F","Réforme F"),
       lty=1,lwd=2,col=c("lightskyblue","blue","darkblue", "rosybrown1","red1","red4"),
       ncol=2,bty="n",cex=0.8,,
)



# D9
y1F <- lm (S1_D9_F ~ poly (gene, 4, raw=TRUE))
y2F <- lm (S2_D9_F ~ poly (gene, 4, raw=TRUE))
y3F <- lm (S3_D9_F ~ poly (gene, 4, raw=TRUE))

y1H <- lm (S1_D9_H ~ poly (gene, 4, raw=TRUE))
y2H <- lm (S2_D9_H ~ poly (gene, 4, raw=TRUE))
y3H <- lm (S3_D9_H ~ poly (gene, 4, raw=TRUE))

par(mar = c(4.1, 4.1, 4.1, 2.1))
plot(gene,fitted(y1F), type="l",lwd=3,pch=16,
     ylim=c(10000,58000),col="rosybrown1",
     xlab="Année de naissance",ylab="Neuvième décile de pension")
lines(gene,fitted(y2F), lwd=3, col="red1")
lines(gene,fitted(y3F), lwd=3, col="red4")
lines(gene,fitted(y1H), lwd=3, col="lightskyblue")
lines(gene,fitted(y2H), lwd=3, col="blue")
lines(gene,fitted(y3H), lwd=3, col="darkblue")

abline(v=1948)
title(main= "D9: Evolution par génération \n (bénéficiaires des bonifications)",cex=0.8)
legend("topleft",legend=c("Législation actuelle H","No bonif H","Réforme H",
                      "Législation actuelle F","No bonif F","Réforme F"),
       lty=1,lwd=2,col=c("lightskyblue","blue","darkblue", "rosybrown1","red1","red4"),
       ncol=2,bty="n",cex=0.8,,
)




