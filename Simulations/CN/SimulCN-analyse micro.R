############################### TRANSITIONS VERS UN SYSTEME DE COMPTES NOTIONNELS ###################
# CN1a : Valorisation des cotisations passees. 
# A partir de la date AnneeDebCN, tous les droits sont calcules dans le nouveau systeme. 
# Hypoth?se de d?part en retraite: m?me ?ge que dans le sc?nario de r?f?rence
# On applique un taux de cotisation fixe, par exemple celui qui equilibre le regime actuel a la date de debut de transition. 

t0  <- Sys.time()

#### Chargement des programmes source ####

# Déclaration du chemin pour les fichiers sources
cheminsource <- "/Users/didier/Desktop/PENSIPP 0.0/"
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R"      )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R"          )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsRetr.R"         )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsCN.R"           )) )

graph_compar <- function (serie,t1,t2,titre)
{
  plot   (seq(1900+t1,1900+t2,by=1),serie[1,t1:t2],xlab="Annee", ylab=titre,
          ylim=c(min(serie[,t1:t2],na.rm=TRUE),max(serie[,t1:t2],na.rm=TRUE)),lwd=2,col="orange",type="l")
  points (seq(1900+t1,1900+t2,by=1),serie[2,t1:t2],lwd=3,type="l")
  points (seq(1900+t1,1900+t2,by=1),serie[3,t1:t2],lwd=1,type="l")
  points (seq(1900+t1,1900+t2,by=1),serie[4,t1:t2],lwd=2,type="l")
}

# Declaration des variable d'outputs

ageref      <- numeric(taille_max)
pliq_       <- matrix(nrow=taille_max,ncol=2)
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



W           <- 2047.501
cibletaux<-numeric(taille_max)




#### Début de la simulation ####

load(file=(paste0(cheminsource,"Simulations/CN/ref.RData")))
#  Rprof(tmp<-tempfile())
for (sc in c(1,2))

{
  
  # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
  setwd ( (paste0(cheminsource,"Simulations/CN"                                    )) )
  
  

  
  for (t in 60:120)   # Début boucle temporelle
  {
    print (c(sc,t))
    if (sc>1)
    {
      AnneeDepartCN <- 115
      TauxCotCN[t]                 <- 0.27 #MPENS[1,115]/MSAL[1,115]
      if (t <110) {RendementCN[t]  <- PIB[t]/PIB[t-1]-1} 
      else        {RendementCN[t]  <- log((MSAL[sc,t-1]*Prix[t-1])/(MSAL[sc,t-6]*Prix[t-6]))/5}
      RendementCNPrev[t]           <- RendementCN[t]
      RevaloCN[t+1]                <- Prix[t]/Prix[t-1]
      UseConv(55,70,t)
      #  print (CoeffConv[60:80]) 
    }
    
    if (sc>1 && t==AnneeDepartCN)
    {
      for (i in 1:taille_max)
      {
        if (ageliq[i]==0)
        {
          for (u in 1:160)   # Variante 1:160
          {
            if (is.element(statut[i,u],codes_act))
            {
              statut[i,u] <- statut[i,u]+100
            }
          }
        }
      }
    }
      
      
    # Liquidations  
    for (i in 1:taille_max)       # Début boucle individuelle
    {
      
      # Liquidation

      if ((t-anaiss[i]>=55) && (ageliq[i]==0))
      {
        if (sc>1 && t>=AnneeDepartCN)
        {
          Leg <- t
          UseLeg(Leg,anaiss[i])
          SimDir(i,t,"exo",ageref)
#           if (t==115 && i==632) 
#           {
#             print (c(sc,t,i,anaiss[i],duree_rg,duree_tot,duree_rg_maj,duree_tot_maj,
#                      sam_rg,dar[i],pension_rg[i],statut[i,anaiss[i]:t]))
#           }
        }
        else
        # Cas ou CN n'ont pas démarré, liquidation taux plein et conservation age
        {
          Leg <- t
          UseLeg(Leg,anaiss[i])
          SimDir(i,t,"TP")
#           if (t==115 && i==632) 
#           {
#             print (c(sc,t,i,anaiss[i],duree_rg,duree_tot,duree_rg_maj,duree_tot_maj,
#                      sam_rg,dar[i],pension_rg[i],statut[i,anaiss[i]:t]))
#           }
        }
        
        if (liq[i]==t)
        {
          pliq_[i,sc] <- pension[i]
          if (sc==1) {ageref[i] <- t-anaiss[i]}
        }
      } 
      
      else if (ageliq[i]>0)
      { 
        Revalo(i,t,t+1)    
      }
      
    } # Fin de la boucle individuelle 

    if (sc ==1)
    {
      liquidants     <- which(liq>=115 & liq <= 120)
      liquidants_rg  <- which(liq>=115 & liq <= 120 & pension_rg>0 & pension_fp==0 & pension_in==0)
      liquidants_fp  <- which(liq>=115 & liq <= 120 & pension_fp>.75*pension)
      liquidants_in  <- which(liq>=115 & liq <= 120 & pension_in>.75*pension)
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



plot  (pliq_[liquidants_rg ,1],pliq_[liquidants_rg ,2],
       xlim=c(0,max(pliq_[liquidants,])),ylim=c(0,max(pliq_[liquidants,])),pch="°",
       xlab="pension avant réforme",ylab="pension après réforme")
points(pliq_[liquidants_fp ,1],pliq_[liquidants_fp ,2],col="blue",pch="+"   )
points(pliq_[liquidants_in,1],pliq_[liquidants_in,2],col="green",pch="x"   )
points(pliq_[liquidants_po,1],pliq_[liquidants_po,2],col="orange",pch="°")


#save (ageref,file=(paste0(cheminsource,"Simulations/CN/ref.RData")))
