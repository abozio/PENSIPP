t0  <- Sys.time()
  
#### Chargement des programmes source ####
  
# D�claration du chemin pour les fichiers sources
cheminsource <- "/Users/didier/Desktop/PENSIPP 0.0/"
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R"      )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R"          )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsRetr.R"         )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsCN.R"           )) )
  
graph_compar <- function (serie,t1,t2,titre)
{
  
  plot   (seq(1900+t1,1900+t2,by=1),serie[1,t1:t2],xlab="Annee", ylab=titre,
         ylim=c(min(serie[,t1:t2],na.rm=TRUE),max(serie[,t1:t2],na.rm=TRUE)),lwd=1,type="l")
  points (seq(1900+t1,1900+t2,by=1),serie[2,t1:t2],lwd=1,type="l")
  points (seq(1900+t1,1900+t2,by=1),serie[3,t1:t2],lwd=2,type="l")
}

# Declaration des variable d'outputs
TRC         <- numeric(taille_max)        # Taux de remplacemnt cible des liquidants
pensionliq  <- numeric(taille_max)        # Pension à la liquidation
actifs      <- numeric(taille_max)        # Filtre population active
retraites   <- numeric(taille_max)        # Filtre population retraitée
MSAL        <- matrix(nrow=3,ncol=200)    # Masse salariale par année
MPENS       <- matrix(nrow=3,ncol=200)    # Masse des pensions année
PIBREF      <- matrix(nrow=3,ncol=200)    # PIB annuel 
RATIOPENS   <- matrix(nrow=3,ncol=200)    # Ratio pension/PIB par année
RATIOFIN    <- matrix(nrow=3,ncol=200)    # Ratio masse des pensions/masse des salaires par année
RATIODEM    <- matrix(nrow=3,ncol=200)    # Ratio +60ans/-60ans par année
SALMOY      <- matrix(nrow=3,ncol=200)    # Salaire moyen par année
DSALMOY     <- matrix(nrow=3,ncol=200)    # Croissance du salaire moyen
PENMOY      <- matrix(nrow=3,ncol=200)    # Pension moyenne par année
PENLIQMOY   <- matrix(nrow=3,ncol=200)    # Pension moyenne des liquidants par année
PENREL      <- matrix(nrow=3,ncol=200)    # Ratio pension/salaire
TRCMOY      <- matrix(nrow=3,ncol=200)    # Taux de remplacement cible des liquidants par année
AGELIQ      <- matrix(nrow=3,ncol=160)    # Age de liquidation moyen par année
AGELIQH     <- matrix(nrow=3,ncol=160)    # Age de liquidation moyen homme par année
AGELIQF     <- matrix(nrow=3,ncol=160)    # Age de liquidation moyen femme par année
AGELIQgen   <- matrix(nrow=3,ncol=160)    # Age de liquidation moyen par génération
AGELIQgenH  <- matrix(nrow=3,ncol=160)    # Age de liquidation moyen homme par génération
AGELIQgenF  <- matrix(nrow=3,ncol=160)    # Age de liquidation moyen femme par génération
W           <- 2047.501
cibletaux<-numeric(taille_max)
  
#### Début de la simulation ####
  
#  Rprof(tmp<-tempfile())
for (sc in c(1,2,3))
{

  # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
  setwd ( (paste0(cheminsource,"Simulations/CN"                                    )) )

  
  #Parametre de liquidation: TRC (cible et seuil)
  for (i in 1:taille_max)
  {
    if (sexe[i]==1){k[i]     <-LogLogist(0.01,1,0.5,1.2) } else {k[i]<-LogLogist(0.01,1,0.5,1.5)}
    delta_k[i] <- 0.01       # Augmentation de 1% par an après 60 ans (cf SimDir)
    beta[i]     <-200        # Seuil: élevé (TRC)  
  }
  cibletaux<-numeric(taille_max)
  
  
  for (t in 60:160)   # Début de la boucle temporelle, commence en 60, individu le plus vieux de la base n? en 1909.
  {
    print (c(sc,t))
    Leg <- t
    # UseOpt(c("nobonif","nomda","noassimil","nomc","nomg","noptsgratuits","noavpf"))
    
    for (i in 1:taille_max)   # Début de la boucle individuelle
    {
      
      if ((t-anaiss[i]>=55) && (ageliq[i]==0))
      {
        UseLeg(Leg,anaiss[i])
        #  SimDir(i,t,"sw",cibletaux[])  
        SimDir(i,t,"tp")
        
        if (liq[i]==t)
        {
          pensionliq[i] <- pension[i]
          #TRC[i]<-cibletaux[i]
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
    ### Correction des salaires s'il y a lieu
    ### NB : on redresse au fur et à mesure l'ensemble des salaires prospectifs, pour minimiser
    ### redressement à chaque date
    ### Correction des salaires s'il y a lieu
    ### NB : on redresse au fur et à mesure l'ensemble des salaires prospectifs, pour minimiser
    ### redressement à chaque date
    if (sc == 2 && t>=112) 
    {
      salaire[,t:160] <- salaire[,t:160]*SALMOY[sc,t-1]*1.01/SALMOY[sc,t]
      PlafondSS[t+1]  <- PlafondSS[t]*1.01
    }
    if (sc == 3 && t>=112) 
    {
      salaire[,t:160] <- salaire[,t:160]*SALMOY[sc,t-1]*1.02/SALMOY[sc,t]
      PlafondSS[t+1]  <- PlafondSS[t]*1.02
    }    
    SALMOY[sc,t]       <- mean (salaire[actifs,t]/Prix[t])    
    MPENS[sc,t]        <- W*sum(pension[retraites])/Prix[t]     
    MSAL[sc,t]         <- W*sum(salaire[actifs,t])/Prix[t] 
    PIBREF[sc,t]       <- MSAL[sc,t]*(PIB[109]/Prix[109])/MSAL[sc,109]
    RATIOPENS[sc,t]    <- MPENS[sc,t]/PIBREF[sc,t]
    TRCMOY[sc,t]       <- mean (TRC[which(liq[]==t)])
    RATIOFIN[sc,t]     <- MPENS[sc,t]/MSAL[sc,t]
    RATIODEM[sc,t]     <- sum  ((t-anaiss>=60) & (statut[,t]>0))/sum((t-anaiss<60) &(statut[,t]>0))

    if (t>=110) {DSALMOY[sc,t]      <- SALMOY[sc,t]/SALMOY[sc,t-1]-1}
    PENMOY[sc,t]       <- mean (pension[retraites]/Prix[t])
    PENLIQMOY[sc,t]    <- mean (pension[which( (pension>0)&liq==t)])
    PENREL[sc,t]       <- PENMOY[sc,t]/SALMOY[sc,t]
    AGELIQ[sc,t]       <- mean ( ageliq[which(liq==t)])
    AGELIQH[sc,t]      <- mean ( ageliq[which((liq==t)  & (sexe==1))] )
    AGELIQF[sc,t]      <- mean ( ageliq[which((liq==t) & (sexe==2))])
    
  } # Fin de de la boucle temporelle
  
  # Récapitulatifs par générations
  for (g in 20:80)
  {
    AGELIQgen[sc,g]      <- mean ( ageliq[which((anaiss==g) & (ageliq>0))])   
    AGELIQgenH[sc,g]     <- mean ( ageliq[which((anaiss==g) & (ageliq>0)& (sexe==1))])
    AGELIQgenF[sc,g]     <- mean ( ageliq[which((anaiss==g) & (ageliq>0)& (sexe==2))])
  }
  
} # Fin boucle scenarios

   
  
#### Sorties ####
graph_compar(RATIOPENS   ,110,159,"Ratio pension/PIB")
graph_compar(RATIOFIN    ,110,159,"Ratio Financier")
graph_compar(RATIODEM    ,110,159,"Ratio Démographique")
graph_compar(SALMOY      ,110,159,"Salaire moyen")
graph_compar(DSALMOY     ,110,159,"Croissance du salaire moyen")
graph_compar(PENMOY      ,110,159,"Pension moyenne")
graph_compar(PENLIQMOY   ,110,159,"Pension à liquidation moyenne")
graph_compar(PENREL      ,110,159,"Ratio pension/salaire")
graph_compar(AGELIQ      ,110,159,"Age moyen de liquidation")
graph_compar(AGELIQH     ,110,159,"Age moyen de liquidation - Hommes")
graph_compar(AGELIQF     ,110,159,"Age moyen de liquidation - Femmes")
graph_compar(AGELIQgen   , 20, 80,"Age moyen de liquidation par génération")
graph_compar(AGELIQgenH  , 20, 80,"Age moyen de liquidation par génération - Hommes")
graph_compar(AGELIQgenF  , 20, 80,"Age moyen de liquidation par génération - Femmes")
 

  