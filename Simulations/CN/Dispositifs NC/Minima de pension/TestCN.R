########## Test MICO  CN ############



t0  <- Sys.time()

#### Chargement des programmes source ####

# Déclaration du chemin pour les fichiers sources
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R"      )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R"          )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsRetr.R"         )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsCN.R"           )) )


# Declaration des variable d'outputs

ageref       <- numeric(taille_max)
pliq_         <- matrix(nrow=taille_max,ncol=3)
pliq_nomin    <- matrix(nrow=taille_max,ncol=3)
gain          <- numeric(taille_max)
actifs        <- numeric(taille_max)        # Filtre population active
retraites     <- numeric(taille_max)        # Filtre population retraitée
liquidants    <- numeric(taille_max)
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
MPENLIQ     <- matrix(nrow=3,ncol=200)    # Masse des pension à liquidation

pension_nomin<- numeric(taille_max)  # pension avant mv et mc (calculé dans liq)
points_cn_mc<- numeric(taille_max)       # pointscnmc
mcmg<-      numeric(taille_max)          # mc ou mg dans ancien régime

#### Début de la simulation ####

for (sc in 1:3)     #Debut boucle scénarios
{       
  
  # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
  #setwd ( (paste0(cheminsource,"Simulations/CN"                                    )) )
  
  # Options de simulation  
  # UseOpt(c("nobonif","nomda","noassimil","nomc","nomg","noptsgratuits","noavpf"))
  W           <- 2047.501    # Poids de l'échantillon
  # Options CN  
  
  
  TauxCotCN[]                 <- 0.23 
  plafond <- 8
  
  if (sc==2) {UseOpt(c("nomc","nomg"))}
  
  if (sc>2)        
  {
    AnneeDepartCN <- 115
    TauxCotCN[1:(AnneeDepartCN-1)]   <- 0  
    UseOptCN(c("valocot"))
  }
  
  for (t in 80:160)   # Début boucle temporelle
  {
    print (c(sc,t))
    
    if (sc>2)
    {
      if (t <110) {RendementCN[t]  <- PIB[t]/PIB[t-1]-1} 
      else        {RendementCN[t]  <- log((MSAL[sc,t-1]*Prix[t-1])/(MSAL[sc,t-6]*Prix[t-6]))/5}
      RendementCNPrev[t]           <- RendementCN[t]
      RevaloCN[t+1]                <- Prix[t]/Prix[t-1]
      UseConv(55,70,t)
      #  print (CoeffConv[60:80]) 
    }
    
    
    if (sc>2 && t==AnneeDepartCN)
    {
      for (i in 54)
      {
        if (ageliq[i]==0)
        {
          statut[i,statut[i,1:160]>1]<- statut[i,statut[i,1:160]>1]+100
          
        }
      }
    }
    
    
    # Liquidations  
    for (i in 1:10000)       # Début boucle individuelle
    {
      Leg <- t
      
      if ((t-t_naiss[i]>=55) && (ageliq[i]==0))
      {
        if (sc>1 && t>=AnneeDepartCN)
        {        
          UseLeg(t,t_naiss[i])
          SimDir(i,t,"exo",ageref)
        }
        else
          # Cas ou CN n'ont pas démarré, liquidation taux plein et conservation age
        {
          UseLeg(t,t_naiss[i])
          SimDir(i,t,"TP")
        }
        
        if (t_liq[i]==t)
        {
          # Enlever les pensions de l'ancien régime (à améliorer!)
          #  if (sc>1) { pension[i]<-pension_cn_pri[i]+pension_cn_fp[i]+pension_cn_ind[i]}
          points_cn_nc[i]<-points_cn_nc
          pliq_[i,sc] <- pension[i]
          pliq_nomin[i,sc] <- pension_nomin[i]
          if (sc==1) {ageref[i] <- t-t_naiss[i]}
          
        }
      } 
      
      else if (ageliq[i]>0)
      { 
        Revalo(i,t,t+1)    
      }
      
    } # Fin de la boucle individuelle 
    
    if (sc ==1)
    {
      liquidants     <- which(t_liq>=90 & t_liq <= 150)
      liquidants_rg  <- which(t_liq>=90 & t_liq <= 150 & pension_rg>0 & pension_fp==0 & pension_in==0)
      liquidants_fp  <- which(t_liq>=90 & t_liq <= 150 & pension_fp>.75*pension)
      liquidants_in  <- which(t_liq>=90 & t_liq <= 150 & pension_in>.75*pension)
      liquidants_po  <- setdiff(liquidants,union(liquidants_rg,union(liquidants_fp,liquidants_in)))
    }
    
    
    actifs     <- (salaire[,t]>0) & (statut[,t]>0)
    retraites  <- (pension>0) & (statut[,t]>0)
    if (sc >0)
    {
      SALMOY[sc,t]    <- mean (salaire[actifs,t]/Prix[t])
      MPENS[sc,t]        <- W*sum(pension[retraites])/Prix[t]     
      MSAL[sc,t]         <- W*sum(salaire[actifs,t])/Prix[t] 
      RATIOFIN[sc,t]     <- MPENS[sc,t]/MSAL[sc,t]
      PENMOY[sc,t]       <- mean (pension[retraites]/Prix[t])
      PENREL[sc,t]       <- PENMOY[sc,t]/SALMOY[sc,t]
    }  
  } # Fin de de la boucle temporelle
  
# Sauvegarde de mc et mc
if (sc==1){mcmg[] <- indic_mc[]+indic_mg[]}
  
} # Fin boucle scenarios



#### Sorties ####
# Avec/sans mico dans ancien régime
# Comparer au
list1<-  liquidants_rg[mcmg[]>0]
list1<- list1[!is.na(list1)] 
list2<-  liquidants[mcmg[]==0]
list2<- list2[!is.na(list2)] 
mean(pliq_[list1,1]-pliq_[list1,2])
mean(pliq_[list2,1]-pliq_[list2,2])
# Ecart mico pas mico avant/après
mean(pliq_[list2,1])-mean(pliq_[list1,1])
mean(pliq_[list2,2])-mean(pliq_[list1,2])

# Gagnant perdants chez les mico avant reforme:
mc<-mcmg[liquidants]
list1<-  liquidants[mcmg[]>0]
list1<- list1[!is.na(list1)] 
list2<-  liquidants[mcmg[]==0]
list2<- list2[!is.na(list2)] 
mean(pliq_[list1,1]-pliq_[list1,2])
mean(pliq_[list2,1]-pliq_[list2,2])
# Ecart mico pas mico avant/après
mean(pliq_[list2,1])-mean(pliq_[list1,1])
mean(pliq_[list2,2])-mean(pliq_[list1,2])

# Graphe avec: pension avec mico = f (pension sans MICO) et MinVieill
plot  (pliq_[list1,1],pliq_[list1,2],
       xlim=c(0,max(pliq_[liquidants,])),ylim=c(0,max(pliq_[liquidants,])),pch="°",
       xlab="pension ancien régime",ylab="pension CN")
title ("Effets micro sur bénéficiaires des minima dans l'ancien régime")
#points(pliq_[list2,1],pliq_[list2,2],col="blue",pch="°"   )
abline(h=mean(MinVieil1[115:160]),col="grey",lty=2)
abline(0,1,col="red")

# Graphe avec: pension avec mico = f (pension sans MICO) et MinVieill
plot  (pliq_[liquidants,2]-mccn[liquidants],pliq_[liquidants,2],
       xlim=c(0,max(pliq_[liquidants,])),ylim=c(0,max(pliq_[liquidants,])),pch="°",
       xlab="pension sans mico",ylab="pension avec mico")
title ("Minimum contributif CN")
abline(h=mean(MinVieil1[115:160]),col="grey",lty=2)
abline(0,1,col="red")

save.image(paste0(cheminsource,"Simulations/CN/Dispositifs NC/Testmico.RData"))