############################### EFFETS DES DISPOSITIFS NON CONTRIBUTIFS CN ###################

# Masse des pensions pour chaque dispositif CN. 
# Neutralisation tour à tour de chaque dispositif. 

rm(list = ls())
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
pliq_       <- matrix(nrow=taille_max,ncol=7)
points_nc   <- matrix(nrow=taille_max,ncol=7)
points_pri   <- matrix(nrow=taille_max,ncol=7)
pension_nc_liq<- matrix(nrow=taille_max,ncol=7)

gain        <- numeric(taille_max)
actifs      <- numeric(taille_max)        # Filtre population active
retraites   <- numeric(taille_max)        # Filtre population retraitée
liquidants  <- numeric(taille_max)
liquidants_fp <- numeric(taille_max)
liquidants_rg <- numeric(taille_max)
liquidants_in <- numeric(taille_max)
liquidants_po <- numeric(taille_max)



MSAL        <- matrix(nrow=8,ncol=200)    # Masse salariale par année
MPENS       <- matrix(nrow=8,ncol=200)    # Masse des pensions année
RATIOFIN    <- matrix(nrow=8,ncol=200)    # Ratio masse des pensions/masse des salaires par année
SALMOY      <- matrix(nrow=8,ncol=200)    # Salaire moyen par année
PENMOY      <- matrix(nrow=8,ncol=200)    # Pension moyenne par année
PENREL      <- matrix(nrow=8,ncol=200)    # Ratio pension/salaire
PENLIQMOY   <- matrix(nrow=8,ncol=200)    # Pension moyenne à liquidation
MPENLIQ     <- matrix(nrow=8,ncol=200)    # Masse des pension à liquidation

W           <- 2047.501
cibletaux<-numeric(taille_max)
cot<-numeric(taille_max)



#### Début de la simulation ####

#  Rprof(tmp<-tempfile())
for (sc in c(1):7)
{
#  1: Normal Ref  
#  2: Normal CN
#  3: Neutralisation mc
#  4: Neutralisation Avantages familiaux (MDA, bonif, AVPF)  
#  5: Neutralisation Periodes assimilés   

  

  
  # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  



  if (sc==3) {UseOptCN(c("nomccn"))}
  if (sc==4) {UseOptCN(c("nomccn","nobonifcn"))}
  if (sc==5) {UseOptCN(c("nomccn","nobonifcn","nomdacn"))}
  if (sc==6) {UseOptCN(c("nomccn","nobonifcn","nomdacn","noavpfcn"))}
  if (sc==7) {UseOptCN(c("nomccn","nobonifcn","nomdacn","noavpfcn","noassimilcn"))}
  
  if (sc==3) {UseOpt(c("nomg","nomc"))}
  if (sc==4) {UseOpt(c("nomg","nomc","nobonif"))}
  if (sc==5) {UseOpt(c("nomg","nomc","nobonif","nomda"))}
  if (sc==6) {UseOpt(c("nomg","nomc","nobonif","nomda","noavpf"))}
  if (sc==7) {UseOpt(c("nomg","nomc","nobonif","nomda","noavpf","noassimil","noptsgratuits"))}
  
  

  
  TauxCotCN[]                 <- 0.23
  plafond <- 8
  AnneeDepartCN <- 115

  
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
    }
    
    
    if (sc>1 && t==AnneeDepartCN)
    {
      for (i in 1:55000)
      {
        if (ageliq[i]==0)
        {
          statut[i,statut[i,1:160]>1]<- statut[i,statut[i,1:160]>1]+100          
        }
      }
    }
    
    
    # Liquidations  
    for (i in 1:55000)       # Début boucle individuelle
    {
      Leg <- t
      
      if ((t-t_naiss[i]>=55) && (ageliq[i]==0))
      {
       # if (sc>1 && t>=AnneeDepartCN)
        if (sc>1)
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
          pliq_[i,sc]          <- pension[i]
          points_nc[i,sc]      <- points_cn_nc
          points_pri[i,sc]     <- points_cn_pri
          pension_nc_liq[i,sc] <- pension_cn_nc[i]
          if (sc==1) {ageref[i]<- t-t_naiss[i]}
        }
      } 
      
      else if (ageliq[i]>0)
      { 
        Revalo(i,t,t+1)    
      }
      
    } # Fin de la boucle individuelle 
    
    if (sc==1)
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

pliq_CN   <- pliq_[,2:7]
MPENS_CN  <- MPENS[2:7,]
PENREL_CN <- PENREL[2:7,]
MPENLIQ_CN <- MPENLIQ[2:7,]
RATIOFIN_CN<-RATIOFIN[2:7,]


#### Sorties ####
graph_compar(MPENS[,]     ,110,159,"Masse des pensions ")
graph_compar(PENREL[,]     ,115,159,"Ratio pension/salaire")
graph_compar(MPENLIQ[]      ,115,159,"")
graph_compar(PENMOY[]      ,110,159,"")
graph_compar(PENLIQMOY[]      ,110,159,"")

#save.image(paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC_CN2.RData"))
#save(MPENS_CN,MPENLIQ_CN,RATIOFIN_CN,file=paste0(cheminsource,"Simulations/CN/Dispositifs NC/MasseANC_CN.RData"))

 
# plot   (seq(1900+110,1900+159,by=1),MPENS[5,110:159],xlab="Annee", ylab="masse pension",
#         ylim=c(min(MPENS[5,110:159],na.rm=TRUE),max(MPENS[5,110:159],na.rm=TRUE)),lwd=2,col="orange",type="l")
# points (seq(1900+110,1900+159,by=1),MPENS[6,110:159],lwd=4,type="l")
# 
# plot   (seq(1900+110,1900+159,by=1),MPENS[5,110:159]-MPENS[6,110:159],xlab="Annee", ylab="masse pension",
#         ylim=c(min(MPENS[5,110:159]-MPENS[6,110:159],na.rm=TRUE),max(MPENS[5,110:159]-MPENS[6,110:159],na.rm=TRUE)),lwd=2,col="orange",type="l")