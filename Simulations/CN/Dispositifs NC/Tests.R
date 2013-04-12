############################### TEST NC #########
# Individus testes: 
# - Periode de chomage: 
# - AVPF
# - Enfants
load("~/Desktop/PENSIPP 0.1/Simulations/CN/CN1b/Resulats2.RData")
listENF  <-  which(n_enf>2&ageliq>0 & t_liq>115)[1:10]
listAVPF <- which((statut[,115]==avpf)&ageliq>0 & t_liq>115)[1:10]
listCHO  <- which((statut[,115]==chomeurCN) &ageliq>0 & t_liq>115)[1:10]
list  <- union(listENF,listAVPF)
list  <- union(list, listCHO)

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
pliq_       <- matrix(nrow=taille_max,ncol=3)
gain        <- numeric(taille_max)
actifs      <- numeric(taille_max)        # Filtre population active
retraites   <- numeric(taille_max)        # Filtre population retraitée
liquidants  <- numeric(taille_max)
liquidants_fp <- numeric(taille_max)
liquidants_rg <- numeric(taille_max)
liquidants_in <- numeric(taille_max)
liquidants_po <- numeric(taille_max)


#### Début de la simulation ####

for (sc in 1:3)     #Debut boucle scénarios
{       
  
  # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
  setwd ( (paste0(cheminsource,"Simulations/CN"                                    )) )
  
  # Options de simulation  
  W           <- 2047.501    # Poids de l'échantillon
  # Options CN  
  
  AnneeDepartCN <- 115
  TauxCotCN[]                 <- 0.27 
  plafond <- 8
  if (sc==2)        
  {
    TauxCotCN[1:AnneeDepartCN]   <- 0  
    UseOptCN(c("valocot","nonc"))
  }
  
  
  if (sc==3)        
  {
    TauxCotCN[1:(AnneeDepartCN-1)]   <- 0  
    UseOptCN(c("valocot"))
  }
  
  for (t in 80:160)   # Début boucle temporelle
  {
    print (c(sc,t))
    
    if (sc>1)
    {
      if (t <110) {RendementCN[t]  <- PIB[t]/PIB[t-1]-1} 
      else        {RendementCN[t]  <- log((MSAL[sc,t-1]*Prix[t-1])/(MSAL[sc,t-6]*Prix[t-6]))/5}
      RendementCNPrev[t]           <- RendementCN[t]
      RevaloCN[t+1]                <- Prix[t]/Prix[t-1]
      UseConv(55,70,t)
      #  print (CoeffConv[60:80]) 
    }
    
    
    if (sc>1 && t==AnneeDepartCN)
    {
      
      for (i in 31)
      {
        if (ageliq[i]==0)
        {
          #           for (u in 1:160)
          #           {
          #             if (is.element(statut[i,u],codes_act))
          #             {
          #               statut[i,u] <- statut[i,u]+100
          #             }
          #        }
          statut[i,statut[i,1:160]>1]<- statut[i,statut[i,1:160]>1]+100
          
        }
      }
    }
    
    
    # Liquidations  
    for (i in 31)       # Début boucle individuelle
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
          
          pliq_[i,sc] <- pension[i]
          if (sc==1) {ageref[i] <- t-t_naiss[i]}
        }
      } 
      
      else if (ageliq[i]>0)
      { 
        Revalo(i,t,t+1)    
      }
      
    } # Fin de la boucle individuelle 
    

  } # Fin de de la boucle temporelle
  
  
  
} # Fin boucle scenarios



#### Sorties ####

# RG seulement
plot  (pliq_[list,1],pliq_[list,2],
       xlim=c(0,max(pliq_[list,])),ylim=c(0,max(pliq_[list,])),pch="°",
       xlab="pension avant réforme",ylab="pension après réforme")
title ("CN1a: taux de cotisation fixe")
abline(0,1,col="red")

plot  (pliq_[list,1],pliq_[list,3],
       xlim=c(0,max(pliq_[list,])),ylim=c(0,max(pliq_[list,])),pch="°",
       xlab="pension avant réforme",ylab="pension après réforme")
title ("CN1b: valorisations des cotisations passees")
abline(0,1,col="red")


