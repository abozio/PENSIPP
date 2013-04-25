############# Effet des réformes  #############

#Effet des réformes successives dans le régime actuel. 
#But: retrouver résultats du COR et de DB dans la note IPP. 




t0  <- Sys.time()

#### Chargement des programmes source ####
rm(list = ls())

# Déclaration du chemin pour les fichiers sources
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R"      )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R"          )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsRetr.R"         )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsCN.R"           )) )


# Declaration des variable d'outputs
TRC         <- numeric(taille_max)        # Taux de remplacemnt cible des liquidants
pensionliq  <- numeric(taille_max)        # Pension à la liquidation
actifs      <- numeric(taille_max)        # Filtre population active
retraites   <- numeric(taille_max)        # Filtre population retraitée
MSAL        <- matrix(nrow=4,ncol=200)    # Masse salariale par année
MPENS       <- matrix(nrow=4,ncol=200)    # Masse des pensions année
PIBREF      <- matrix(nrow=4,ncol=200)    # PIB annuel 
RATIOPENS   <- matrix(nrow=4,ncol=200)    # Ratio pension/PIB par année
RATIOFIN    <- matrix(nrow=4,ncol=200)    # Ratio masse des pensions/masse des salaires par année
RATIODEM    <- matrix(nrow=4,ncol=200)    # Ratio +60ans/-60ans par année
SALMOY      <- matrix(nrow=4,ncol=200)    # Salaire moyen par année
DSALMOY     <- matrix(nrow=4,ncol=200)    # Croissance du salaire moyen
PENMOY      <- matrix(nrow=4,ncol=200)    # Pension moyenne par année
PENLIQMOY   <- matrix(nrow=4,ncol=200)    # Pension moyenne des liquidants par année
PENREL      <- matrix(nrow=4,ncol=200)    # Ratio pension/salaire
TRCMOY      <- matrix(nrow=4,ncol=200)    # Taux de remplacement cible des liquidants par année
AGELIQ      <- matrix(nrow=4,ncol=160)    # Age de liquidation moyen par année
AGELIQH     <- matrix(nrow=4,ncol=160)    # Age de liquidation moyen homme par année
AGELIQF     <- matrix(nrow=4,ncol=160)    # Age de liquidation moyen femme par année
AGELIQgen   <- matrix(nrow=4,ncol=160)    # Age de liquidation moyen par génération
AGELIQgenH  <- matrix(nrow=4,ncol=160)    # Age de liquidation moyen homme par génération
AGELIQgenF  <- matrix(nrow=4,ncol=160)    # Age de liquidation moyen femme par génération
W           <- 2047.501
cibletaux<-numeric(taille_max)

#### Début de la simulation ####

#  Rprof(tmp<-tempfile())
for (sc in c(1,2,3,4))
{
  
  # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
  
  
  for (t in 60:160)   # Début de la boucle temporelle, commence en 60, 
    #individu le plus vieux de la base n? en 1909.
  {
    print (c(sc,t))
    if (sc==1)      {Leg <- min(92,t)}
    else if (sc==2) {Leg <- min(93,t)}
    else if (sc==3) {Leg <- min(103,t)}
    else            {Leg <- t}
    
    for (i in 1:55000)   # Début de la boucle individuelle
    {
      
      if ((t-t_naiss[i]>=55) && (ageliq[i]==0))
      {
        UseLeg(Leg,t_naiss[i])
        SimDir(i,t,"tp")
        
        if (t_liq[i]==t)
        {
          pensionliq[i] <- pension[i]
        }
        
      } 
      
      else if (ageliq[i]>0)
      { 
        Revalo(i,t,t+1)    
      }
      
    } # Fin de la boucle individuelle 
    
    actifs    <- (salaire[,t]>0) & (statut[,t]>0)
    retraites <- (pension>0) & (statut[,t]>0)
    SALMOY[sc,t]       <- mean (salaire[actifs,t]/Prix[t])
    MPENS[sc,t]        <- W*sum(pension[retraites])/Prix[t]     
    MSAL[sc,t]         <- W*sum(salaire[actifs,t])/Prix[t] 
    PIBREF[sc,t]       <- MSAL[sc,t]*(PIB[109]/Prix[109])/MSAL[sc,109]
    RATIOPENS[sc,t]    <- MPENS[sc,t]/PIBREF[sc,t]
    TRCMOY[sc,t]       <- mean (TRC[which(t_liq[]==t)])
    RATIOFIN[sc,t]     <- MPENS[sc,t]/MSAL[sc,t]
    RATIODEM[sc,t]     <- sum  ((t-t_naiss>=60) & (statut[,t]>0))/sum((t-t_naiss<60) &(statut[,t]>0))
    
    if (t>=110) 
    {
    DSALMOY[sc,t]      <- SALMOY[sc,t]/SALMOY[sc,t-1]-1}
    PENMOY[sc,t]       <- mean (pension[retraites]/Prix[t])
    PENLIQMOY[sc,t]    <- mean (pension[which( (pension>0)&t_liq==t)])
    PENREL[sc,t]       <- PENMOY[sc,t]/SALMOY[sc,t]
    AGELIQ[sc,t]       <- mean ( ageliq[which(t_liq==t)])
    AGELIQH[sc,t]      <- mean ( ageliq[which((t_liq==t)  & (sexe==1))] )
    AGELIQF[sc,t]      <- mean ( ageliq[which((t_liq==t) & (sexe==2))])
    
  } # Fin de de la boucle temporelle
  
  # Récapitulatifs par générations
  for (g in 20:80)
  {
    AGELIQgen[sc,g]      <- mean ( ageliq[which((t_naiss==g) & (ageliq>0))])   
    AGELIQgenH[sc,g]     <- mean ( ageliq[which((t_naiss==g) & (ageliq>0)& (sexe==1))])
    AGELIQgenF[sc,g]     <- mean ( ageliq[which((t_naiss==g) & (ageliq>0)& (sexe==2))])
  }
  
} # Fin boucle scenarios



#### Sorties ####

# Sauvegarde du workspace et du graphique
save.image(paste0(cheminsource,"Simulations/CN/Scenario de reference/EffetReformes.RData"))
par(mar=c(6.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),RATIOPENS[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(min(RATIOPENS[,110:159],na.rm=TRUE),max(RATIOPENS[,110:159],na.rm=TRUE)),col="grey90",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOPENS[2,110:159],lwd=4,col="grey80",type="l")
points (seq(2010,2059,by=1),RATIOPENS[3,110:159],lwd=4,col="grey40",type="l")
points (seq(2010,2059,by=1),RATIOPENS[4,110:159],lwd=4,col="grey0",type="l")
title("Graphe 1 : Evolution du ratio retraites/PIB \nEffet des réformes")
legend.text <- c("Legislation 2012","Legislation 2003","Legislation 1993","Legislation 1992")
legend("bottom",inset=c(-0.2,-0.6),cex=0.9,legend.text, fill=c("grey0","grey40","grey80","grey90"))