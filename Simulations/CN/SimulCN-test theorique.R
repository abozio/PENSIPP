# Ce programme simule les effets des comptes notionnels sur une population fictive 
# stationnaire de mortalité bloquée à son niveau de 2010 et en variant les hypothèses 
# de productivité. Celles-cisont controlées via l'hypothèse sur le plafond de la secu, 
# sur lequel les salaires s'alignent intégralement. L'âge de la retraite est fixe, égal 
# à 62 ans



cheminsource <- "/Users/didier/Desktop/PENSIPP 0.0/"

source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr.R"         )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R"      )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R"          )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsRetr.R"         )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsCN.R"           )) )

graph_compar <- function (serie,t1,t2,titre)
{
  plot   (seq(1900+t1,1900+t2,by=1),serie[2,t1:t2],xlab="Annee", ylab=titre,
          ylim=c(min(serie[,t1:t2],na.rm=TRUE),max(serie[,t1:t2],na.rm=TRUE)),lwd=1,type="l")
  points (seq(1900+t1,1900+t2,by=1),serie[3,t1:t2],lwd=2,type="l")
  points (seq(1900+t1,1900+t2,by=1),serie[4,t1:t2],lwd=3,type="l")
}

# Declaration des variable d'outputs
TRC         <- numeric(taille_max)        # Taux de remplacemnt cible des liquidants
pensionliq  <- numeric(taille_max)        # Pension à la liquidation
ageliq_     <- matrix(nrow=taille_max,ncol=4)
ageref      <- numeric(taille_max)
tdec        <- rep(201,taille_max)
actifs      <- numeric(taille_max)        # Filtre population active
retraites   <- numeric(taille_max)        # Filtre population retraitée
MSAL        <- matrix(nrow=4,ncol=200)    # Masse salariale par année
MPENS       <- matrix(nrow=4,ncol=200)    # Masse des pensions année
RATIOFIN    <- matrix(nrow=4,ncol=200)    # Ratio masse des pensions/masse des salaires par année
RATIODEM    <- matrix(nrow=4,ncol=200)    # Ratio +60ans/-60ans par année
SALMOY      <- matrix(nrow=4,ncol=200)    # Salaire moyen par année
DSALMOY     <- matrix(nrow=4,ncol=200)    # Croissance du salaire moyen
PENMOY      <- matrix(nrow=4,ncol=200)    # Pension moyenne par année
PENLIQMOY   <- matrix(nrow=4,ncol=200)    # Pension moyenne des liquidants par année
PENREL      <- matrix(nrow=4,ncol=200)    # Ratio pension/salaire
FLUXLIQ     <- matrix(nrow=4,ncol=200)    # Effectifs de liquidants
AGELIQ      <- matrix(nrow=4,ncol=160)    # Age de liquidation moyen par année
AGELIQH     <- matrix(nrow=4,ncol=160)    # Age de liquidation moyen homme par année
AGELIQF     <- matrix(nrow=4,ncol=160)    # Age de liquidation moyen femme par année
AGELIQgen   <- matrix(nrow=4,ncol=160)    # Age de liquidation moyen par génération
AGELIQgenH  <- matrix(nrow=4,ncol=160)    # Age de liquidation moyen homme par génération
AGELIQgenF  <- matrix(nrow=4,ncol=160)    # Age de liquidation moyen femme par génération
W           <- 2047.501
cibletaux<-numeric(taille_max)


# On bloque la survie a sa valeur de l'année 2000
for (t in 1:160)
{
  survie[1,,t] <- survie[1,,100]
  survie[2,,t] <- survie[2,,100]
}


#### Début de la simulation ####



for (sc in c(2,3,4))
{
  
  statut[1:taille_max,1:160]  <- -1
  salaire[1:taille_max,1:160] <- 0
  
  # Trajectoire exponentielle fictive pour le plafond, sur laquelle seront calés les salaires
  for (t in 1:160)
  {
    PlafondSS[t] <- PlafondSS[90]*exp(0.005*sc*(t-90))
    # Rupture de croissance en 1990 dans le scénario 4
    if (t>90 && sc==4) {PlafondSS[t] <- PlafondSS[90]*exp(0.01*(t-90))}
    Prix[t] <- 1
  }
  
  # Creation fichier de données individuelles bidon
  for (g in 1:160)
  {
    ideb  <- 10*(g-1)+1
    ifin  <- 10*g
    for (i in ideb:ifin)
    {
      ageref[i]        <- 62
      anaiss[i]        <- g
      findet[i]        <- 20
      sexe[i]          <- 1+i%%2
      pension_cn_pri[i] <- 0
      pension_cn_fp[i]  <- 0
      pension_cn_ind[i] <- 0
      pension[i]        <- 0
      ageliq[i]         <- 0
      liq[i]            <- 0
      for (t in g:160)
      {
        if (statut[i,t]>0 && 
            (sc==2 && (runif(1)<quotient[sexe[i],t-g+1,100] || (t-g)==100)) ||
            (sc>2  && (t==tdec[i])))
        {
          statut[i,t:160]  <- -3
          salaire[i,t:160] <-  0
#          print (c(sc,i,g,t-g))
          if (sc ==2) {tdec[i] <- t}
          break
        }
        else
        {
          if (t-g<20)
          {
            statut[i,t:160] <- 1
            salaire[i,t] <- 0
          }
          else
          {
            statut[i,t:160] <- non_cadreCN
            salaire[i,t]<- PlafondSS[t]*Prix[t]
          }
        }
      }
    }
  }
  

  AnneeDepartCN           <- 60  
  TauxCotCN[1:160]        <- 0.25
  RendementCN[1:160]      <- sc*0.005
  RendementCNPrev[1:160]  <- sc*0.005
  if (sc ==4) 
  {
    RendementCN[90:160]      <- 0.010
    RendementCNPrev[90:160]  <- 0.010     
  }
  RevaloCN[1:160]         <- 1

  Leg                     <- 110
  
  for (t in 60:160)   # Début boucle temporelle
  {
    print (c(sc,t))
    UseConv(60,65,t)
    for (i in 1:taille_max)       # Début boucle individuelle
    {
      
      # Liquidation

      if (statut[i,t]>0 && (t-anaiss[i]>=55) && (ageliq[i]==0))
      {
        UseLeg(110,anaiss[i])
        SimDir(i,t,"exo",ageref)
        if (liq[i]==t)
        {
          pensionliq[i] <- pension[i]
        }
      } 
      else if (ageliq[i]>0)
      { 
        Revalo(i,t,t+1)    
      }
      
    } # Fin de la boucle individuelle 
    
    ageliq_[,sc] <- liq-anaiss
    actifs       <- (salaire[,t]>0) & (statut[,t]>0)
    retraites    <- (pension>0) & (statut[,t]>0)
    if (sc >1)
    {
      SALMOY[sc,t]       <- mean (salaire[actifs,t]/Prix[t])
      MPENS[sc,t]        <- W*sum(pension[retraites])/Prix[t]     
      MSAL[sc,t]         <- W*sum(salaire[actifs,t])/Prix[t] 
      RATIOFIN[sc,t]     <- MPENS[sc,t]/MSAL[sc,t]
      RATIODEM[sc,t]     <- sum  ((t-anaiss>=60) & (statut[,t]>0))/sum((t-anaiss<60) &(statut[,t]>0))
      if (t>=61) {DSALMOY[sc,t]      <- SALMOY[sc,t]/SALMOY[sc,t-1]-1}
      PENMOY[sc,t]       <- mean (pension[retraites]/Prix[t])
      PENLIQMOY[sc,t]    <- mean (pension[which( (pension>0)&liq==t)])
      PENREL[sc,t]       <- PENMOY[sc,t]/SALMOY[sc,t]
      FLUXLIQ[sc,t]      <- W*sum(liq==t)
      AGELIQ[sc,t]       <- mean ( ageliq[which(liq==t)])
      AGELIQH[sc,t]      <- mean ( ageliq[which((liq==t)  & (sexe==1))] )
      AGELIQF[sc,t]      <- mean ( ageliq[which((liq==t) & (sexe==2))])
    }  
  } # Fin de de la boucle temporelle
  
  # Récapitulatifs par générations
  if (sc>0)
  {
    for (g in 20:80)
    {
      if (sc==1) {AGELIQgen[2,g] <- mean(ageref[which((anaiss==g))])}
      AGELIQgen[sc,g]      <- mean ( ageliq[which((anaiss==g) & (ageliq>0))])   
      AGELIQgenH[sc,g]     <- mean ( ageliq[which((anaiss==g) & (ageliq>0)& (sexe==1))])
      AGELIQgenF[sc,g]     <- mean ( ageliq[which((anaiss==g) & (ageliq>0)& (sexe==2))])
    }
  }
  
} # Fin boucle scenarios



#### Sorties ####
graph_compar(RATIOFIN    ,60,159,"Ratio Financier")
graph_compar(RATIODEM    ,60,159,"Ratio Démographique")
graph_compar(SALMOY      ,60,159,"Salaire moyen")
graph_compar(DSALMOY     ,60,159,"Croissance du salaire moyen")
graph_compar(PENMOY      ,60,159,"Pension moyenne")
graph_compar(PENLIQMOY   ,60,159,"Pension à liquidation moyenne")
graph_compar(PENREL      ,60,159,"Ratio pension/salaire")
graph_compar(FLUXLIQ     ,60,159,"Flux de liquidants");
graph_compar(AGELIQ      ,60,159,"Age moyen de liquidation")
graph_compar(AGELIQH     ,60,159,"Age moyen de liquidation - Hommes")
graph_compar(AGELIQF     ,60,159,"Age moyen de liquidation - Femmes")
graph_compar(AGELIQgen   , 20, 80,"Age moyen de liquidation par génération")
graph_compar(AGELIQgenH  , 20, 80,"Age moyen de liquidation par génération - Hommes")
graph_compar(AGELIQgenF  , 20, 80,"Age moyen de liquidation par génération - Femmes")

#save (ageref,file=(paste0(cheminsource,"Simulations/CN/ref.RData")))
