############################### TRANSITIONS VERS UN SYSTEME DE COMPTES NOTIONNELS ###################
# CN1b : Valorisation des cotisations passees. 
# A partir de la date AnneeDebCN, tous les droits sont calcules dans le nouveau systeme. 
# Hypothese de depart en retraite: meme age que dans le scenario de reference
# On applique un taux de cotisation fixe en prospectif, par exemple celui qui equilibre le regime actuel a la date de debut de transition. 
# On reconstitue les cotisations versées pour chaque individus avant la transition. 

# Scénario 1 : Scénario de référence
# Scénario 2 : CN1a Valorisation des droits acquis avec un taux unique (le même qu'en prospectif)
# Scénario 3 : CN1b Valorisation des droits acquis avec les cotisations effectivement versées. 


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

MSAL        <- matrix(nrow=4,ncol=200)    # Masse salariale par année
MPENS       <- matrix(nrow=4,ncol=200)    # Masse des pensions année
RATIOFIN    <- matrix(nrow=4,ncol=200)    # Ratio masse des pensions/masse des salaires par année
SALMOY      <- matrix(nrow=4,ncol=200)    # Salaire moyen par année
PENMOY      <- matrix(nrow=4,ncol=200)    # Pension moyenne par année
PENREL      <- matrix(nrow=4,ncol=200)    # Ratio pension/salaire


#### Début de la simulation ####

for (sc in c(3))     #Debut boucle scénarios
{       
  
 # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
  setwd ( (paste0(cheminsource,"Simulations/CN"                                    )) )
  
# Options de simulation  
  # UseOpt(c("nobonif","nomda","noassimil","nomc","nomg","noptsgratuits","noavpf"))
  W           <- 2047.501    # Poids de l'échantillon
# Options CN  

  AnneeDepartCN <- 115
  TauxCotCN[]                 <- 0.27 
  plafond <- 8
  if (sc==3)        
  {
  TauxCotCN[1:AnneeDepartCN]   <- 0  
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
      for (i in 1:55000)
      {
        if (ageliq[i]==0)
        {
          for (u in 1:160)
          {
            if (is.element(statut[i,u],codes_act))
            {
              statut[i,u] <- statut[i,u]+100
            }
          }
        }
      }
    }
    
#     if (sc==3 && t==AnneeDepartCN)      # Changement des statuts à partir de AnneeDepartCN seulement
#     {
#       for (i in 1:55000)
#       {
#         if (ageliq[i]==0)
#         {
#           for (u in AnneeDepartCN:160)
#           {
#             if (is.element(statut[i,u],codes_act))
#             {
#               statut[i,u] <- statut[i,u]+100
#             }
#           }
#         }
#       }
#     }
#     
      
      
    # Liquidations  
    for (i in 1:55000)       # Début boucle individuelle
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

    if (sc ==1)
    {
      liquidants     <- which(t_liq>=115 & t_liq <= 150)
      liquidants_rg  <- which(t_liq>=115 & t_liq <= 150 & pension_rg>0 & pension_fp==0 & pension_in==0)
      liquidants_fp  <- which(t_liq>=115 & t_liq <= 150 & pension_fp>.75*pension)
      liquidants_in  <- which(t_liq>=115 & t_liq <= 150 & pension_in>.75*pension)
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
  


} # Fin boucle scenarios



#### Sorties ####
graph_compar(RATIOFIN        ,110,159,"Ratio Financier")
graph_compar(PENREL          ,110,159,"Ratio pension/salaire")

# RG seulement
plot  (pliq_[liquidants_rg ,1],pliq_[liquidants_rg ,2],
       xlim=c(0,max(pliq_[liquidants,])),ylim=c(0,max(pliq_[liquidants,])),pch="°",
       xlab="pension avant réforme",ylab="pension après réforme")
title ("CN1a: taux de cotisation fixe")
abline(0,1,col="red")

plot  (pliq_[liquidants_rg ,1],pliq_[liquidants_rg ,3],
       xlim=c(0,max(pliq_[liquidants,])),ylim=c(0,max(pliq_[liquidants,])),pch="°",
       xlab="pension avant réforme",ylab="pension après réforme")
title ("CN1b: valorisations des cotisations passees")
abline(0,1,col="red")



save.image("~/Desktop/PENSIPP 0.1/Simulations/CN/CN1b/Resulats2.RData")