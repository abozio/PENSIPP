# Comparaison 4 plafonds vs. 8 plafonds
# Effet du changement de plafond utilisé dans le calculs des pointCN. 


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

W           <- 2047.501

#### Début de la simulation ####

for (sc in c(1,2,3))
  
{ 
  # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
  setwd ( (paste0(cheminsource,"Simulations/CN/Comparaison plafonds"                )) )
  

# Options CN
  AnneeDepartCN <- 115
  TauxCotCN[]                 <- 0.27 #MPENS[1,115]/MSAL[1,115]
  if (sc==2) {plafond<-8}
  if (sc==3) {plafond<-4}

  for (t in 80:160)   # Début boucle temporelle
  {
    print (c(sc,t))
#    UseOpt(c("nobonif","nomda","noassimil","nomc","nomg","noptsgratuits","noavpf"))
    if (sc>1)
    {
      if (t <110) {RendementCN[t]  <- PIB[t]/PIB[t-1]-1} 
      else        {RendementCN[t]  <- log((MSAL[sc,t-1]*Prix[t-1])/(MSAL[sc,t-6]*Prix[t-6]))/5}
      RendementCNPrev[t]           <- RendementCN[t]
      RevaloCN[t+1]                <- Prix[t]/Prix[t-1]
      UseConv(55,70,t)
    }
    
    if (sc>1 && t==AnneeDepartCN)
    {
      for (i in (c(17798, 17877, 21816 ,22650)))
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
    for (i in (c(17798, 17877, 21816 ,22650)))       # Début boucle individuelle
    {
      Leg <- t
      # Liquidation
      if ((t>115) && (salaire[,t]>4*PlafondSS[t])){print(c("haut salraire", i))}
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
      liquidants     <- which(t_liq>=115 & t_liq <= 160)
      liquidants_rg  <- which(t_liq>=115 & t_liq <= 160 & pension_rg>0 & pension_fp==0 & pension_in==0)
      liquidants_fp  <- which(t_liq>=115 & t_liq <= 160 & pension_fp>.75*pension)
      liquidants_in  <- which(t_liq>=115 & t_liq <= 160 & pension_in>.75*pension)
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


# RG
plot  (pliq_[liquidants_rg ,1],pliq_[liquidants_rg ,2],
       xlim=c(0,max(pliq_[liquidants,])),ylim=c(0,max(pliq_[liquidants,])),pch="°",
       xlab="pension avant réforme",ylab="pension après réforme")
points(pliq_[liquidants_rg,1],pliq_[liquidants_rg,3],col="orange",pch="°")
abline(0,1,col="red")

# Indep
plot  (pliq_[liquidants_in ,1],pliq_[liquidants_in ,2],
       xlim=c(0,max(pliq_[liquidants,])),ylim=c(0,max(pliq_[liquidants,])),pch="°",
       xlab="pension avant réforme",ylab="pension après réforme")
points(pliq_[liquidants_in,1],pliq_[liquidants_in,3],col="orange",pch="°")
abline(0,1,col="red")

# FP
plot  (pliq_[liquidants_fp ,1],pliq_[liquidants_fp ,2],
       xlim=c(0,max(pliq_[liquidants,])),ylim=c(0,max(pliq_[liquidants,])),pch="°",
       xlab="pension avant réforme",ylab="pension après réforme")
points(pliq_[liquidants_fp,1],pliq_[liquidants_fp,3],col="orange",pch="°")
abline(0,1,col="red")

# Pas de difference quand on change le plafond!!
# Du à l'absence de hauts salaire dans l'échantillon après 2018: BIZARRE?
list
for (t in 115:160)
{
list<- which(salaire[t_naiss>40,t]>4*PlafondSS[t])  
print(c(t,list))
}

