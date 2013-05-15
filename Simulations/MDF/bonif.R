############################### BONFIFICATION POUR ENFANTS ###################

## NOTE DU MDF: 
# Plusieurs possibilités sont habituellement envisagées, allant de la suppression 
# pure et simple à l’extension à enveloppe constante aux enfants de tous les rangs. 
# L’affectation d’un bonus dès le premier enfant changerait de nature le fonctionnement 
# de cette disposition, qui deviendrait redondante avec d’autres dispositions 
# destinées à compenser les charges de la famille (AF) ou les interruptions 
# d’activité (MDA, bonus cf. plus haut). 
# Ce dispositif, qui consiste à bonifier de 10% la pension des parents de 
# trois enfants et plus favorise plus les hommes, puisqu’ils ont des pensions 
# plus importantes. Cela reste difficile à justifier.
# Pour le rendre plus redistributif en fonction du revenu, mais aussi du genre, 
# il est proposé de le remplacer par un bonus forfaitaire. 
# Le montant forfaitaire peut être fixé de manière à ce que le changement 
# soit neutre en terme de finances publiques  (122€ par mois pour les retraités 
# partant à la retraite en 2008, d’après les simulations Destinie), ou à un niveau 
# moindre afin de financer d’autres aspects de la réforme.


## Scénario simulé: 
# A partir de 2008, date de la réforme, les bonifications pour enfants du système actuel 
# sont remplacées par une bonification forfaitaire pour les parents de trois enfants 
# ou plus. Le montant de la majoration est de 122E en 2008, puis est réevalué chaque 
# année sur la base du taux de croissance des pensions à liquidation (scénarion 2)
# ou de l'inflation (scénario 3).
# Les résultats sont comparés à une situation de statu quo (scénario 1)

rm(list = ls())

#### Chargement des programmes source ####

# Déclaration du chemin pour les fichiers sources
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R"      )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R"          )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsRetr.R"         )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsCN.R"           )) )



# Declaration des variable d'outputs

ageref      <- numeric(taille_max)
pliq_       <- matrix(nrow=taille_max,ncol=7)
pliq_rg       <- matrix(nrow=taille_max,ncol=7)
pliq_fp       <- matrix(nrow=taille_max,ncol=7)
gain        <- numeric(taille_max)
actifs      <- numeric(taille_max)        # Filtre population active
retraites   <- numeric(taille_max)        # Filtre population retraitée
actifsa      <- numeric(taille_max)        # Filtre population active
retraitesa   <- numeric(taille_max)  
liquidants  <- numeric(taille_max)
liquidants_fp <- numeric(taille_max)
liquidants_rg <- numeric(taille_max)
liquidants_in <- numeric(taille_max)
liquidants_po <- numeric(taille_max)

MSAL        <- matrix(nrow=3,ncol=200)    # Masse salariale par année
MPENS       <- matrix(nrow=3,ncol=200)    # Masse des pensions année
RATIOFIN    <- matrix(nrow=3,ncol=200)    # Ratio masse des pensions/masse des salaires par année
SALMOY      <- matrix(nrow=3,ncol=200)    # Salaire moyen par année
PENMOY      <- matrix(nrow=3,ncol=200)    # Pension moyenne par année
PENREL      <- matrix(nrow=3,ncol=200)    # Ratio pension/salaire
PENLIQMOY   <- matrix(nrow=3,ncol=200)    # Pension moyenne à liquidation
PENLIQMOY  <- matrix(nrow=3,ncol=200)    # Pension moyenne à liquidation
MPENLIQ     <- matrix(nrow=3,ncol=200)    # Masse des pension à liquidation
W           <- 2047.501

# MAJORATION FORFAITAIRE
flat_majo     <- matrix(nrow=3,ncol=200) 


#### Début de la simulation ####

#  Rprof(tmp<-tempfile())
for (sc in c(2,3))
  #  1: Normal Ref  
  #  2: Bonif forfaitaire indexé sur le taux de croissance des pensions
  #  3: Bonif forfaitaire indexé sur l'inflation
  
{
  # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
  setwd ( (paste0(cheminsource,"Simulations/MDF"                                    )) )



  for (t in 80:160)   # Début boucle temporelle
  {
    
    # Majoration: 
    flat_majo[2:3,108]<-122*12 
    if (sc==2 & t>108)
    {
    flat_majo[sc,t]<-flat_majo[sc,t-1]*((PENLIQMOY[sc,t-1]/PENLIQMOY[sc,t-2])) 
    }
    if (sc==3 & t>108)
    {
      flat_majo[sc,t]<-flat_majo[sc,t-1]*((Prix[t-1]/Prix[t-2])) 
    }
    
    
    print (c(sc,t,Options, flat_majo[sc,t]))
    
    
    # Liquidations  
    for (i in 1:55000)       # Début boucle individuelle
    {
      Leg <- t
      # Liquidation
      
      if ((t-t_naiss[i]>=55) && (ageliq[i]==0))
      {
        if (sc>1)
        {   
          # Neutralisation des bonifications pour pensions après 2008
          if (t==108){UseOpt("nobonif")}
          UseLeg(t,t_naiss[i])
          SimDir(i,t,"exo",ageref)
        }
        else          
        {
          UseLeg(t,t_naiss[i])
          SimDir(i,t,"TP")
        }
        
        # A la date de liquidation: 
        if (t_liq[i]==t)
        {
          # Enregistrement des âges de liquidation
          if (sc==1) 
          {
            ageref[i] <- t-t_naiss[i]
          }
          
          # Application de la majoration pour les parents de 3 enfants et plus. 
          if (sc>1) 
          {
            if (n_enf[i]>2 & t>=108)
            {
              pension[i]<-pension[i]+flat_majo[sc,t]
            }  
          }
          
          # Enregistrement des pensions à liquidation
          pliq_[i,sc]   <- pension[i]
          pliq_rg[i,sc] <- pension_rg[i]
          pliq_fp[i,sc] <- pension_fp[i]
          

        
        }
        
      } 
      
      else if (ageliq[i]>0)
      { 
        Revalo(i,t,t+1)    
      }
      
    } # Fin de la boucle individuelle 
    
    if (sc==1)
    {
      liquidants     <- which( t_liq < 999)
      liquidants_rg  <- which(t_liq < 999  & pension_rg>0 & pension_fp==0 & pension_in==0)
      liquidants_fp  <- which(t_liq < 999  & pension_fp>.75*pension)
      liquidants_in  <- which(t_liq < 999  & pension_in>.75*pension)
      liquidants_po  <- setdiff(liquidants,union(liquidants_rg,union(liquidants_fp,liquidants_in)))
    }
    
    
    actifs     <- (salaire[,t]>0) & (statut[,t]>0)
    retraites  <- (pension>0) & (statut[,t]>0)
    
    if (sc >0)
    {
      SALMOY[sc,t]       <- mean (salaire[actifs,t]/Prix[t])
      MPENS[sc,t]        <- W*sum(pension[retraites])/Prix[t] 
      MPENLIQ[sc,t]      <- W*sum(pension[which( (pension[]>0)&t_liq[]==t)])/Prix[t]  
      MSAL[sc,t]         <- W*sum(salaire[actifs,t])/Prix[t] 
      RATIOFIN[sc,t]     <- MPENS[sc,t]/MSAL[sc,t]
      PENMOY[sc,t]       <- mean (pension[retraites]/Prix[t])
      PENLIQMOY[sc,t]    <- mean (pension[which( (pension[]>0)&t_liq[]==t)])
      PENREL[sc,t]       <- PENMOY[sc,t]/SALMOY[sc,t]
    }  
  } # Fin de de la boucle temporelle
  
  
  
} # Fin boucle scenarios



#### Sorties ####

save.image(paste0(cheminsource,"Simulations/MDF/bonif.RData"))
