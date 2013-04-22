############################### EFFETS DES DISPOSITIFS NON CONTRIBUTIFS ###################

# Masse des pensions pour chaque dispositif. 
# Neutralisation tour à tour de chaque dispositif. 
# Questions: maintien d'un âge constant? 

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
gain        <- numeric(taille_max)
actifs      <- numeric(taille_max)        # Filtre population active
retraites   <- numeric(taille_max)        # Filtre population retraitée
liquidants  <- numeric(taille_max)
liquidants_fp <- numeric(taille_max)
liquidants_rg <- numeric(taille_max)
liquidants_in <- numeric(taille_max)
liquidants_po <- numeric(taille_max)



MSAL        <- matrix(nrow=7,ncol=200)    # Masse salariale par année
MPENS       <- matrix(nrow=7,ncol=200)    # Masse des pensions année
RATIOFIN    <- matrix(nrow=7,ncol=200)    # Ratio masse des pensions/masse des salaires par année
SALMOY      <- matrix(nrow=7,ncol=200)    # Salaire moyen par année
PENMOY      <- matrix(nrow=7,ncol=200)    # Pension moyenne par année
PENREL      <- matrix(nrow=7,ncol=200)    # Ratio pension/salaire
PENLIQMOY   <- matrix(nrow=7,ncol=200)    # Pension moyenne à liquidation
MPENLIQ     <- matrix(nrow=7,ncol=200)    # Masse des pension à liquidation

W           <- 2047.501
cibletaux<-numeric(taille_max)
cot<-numeric(taille_max)



#### Début de la simulation ####

#  Rprof(tmp<-tempfile())
for (sc in c(1,2,3,4,5,6,7))
  #  1: Normal Ref  
  #  2-4: Neutralisation Avantages familiaux (MDA, bonif, AVPF)  
  #  5-6: Neutralisation Periodes assimilés & Pts gratuits  
  #  7: Neutralisation Minima de pensions
  
{
  
  # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
  setwd ( (paste0(cheminsource,"Simulations/CN"                                    )) )
  

  if (sc==2) {UseOpt(c("nobonif"))}
  if (sc==3) {UseOpt(c("nobonif","nomda"))}
  if (sc==4) {UseOpt(c("nobonif","nomda","noavpf"))}
  if (sc==5) {UseOpt(c("nobonif","nomda","noavpf","noassimil"))}
  if (sc==6) {UseOpt(c("nobonif","nomda","noavpf","noassimil","noptsgratuits"))}
  if (sc==7) {UseOpt(c("nobonif","nomda","noavpf","noassimil","noptsgratuits","nomc","nomg"))}
  
  for (t in 80:160)   # Début boucle temporelle
  {
    print (c(sc,t))
    #    UseOpt(c("nobonif","nomda","noavpf","noassimil","noptsgratuits","nomc","nomg"))

  
    # Liquidations  
    for (i in 1:55000)       # Début boucle individuelle
    {
      Leg <- t
      # Liquidation
      
      if ((t-t_naiss[i]>=55) && (ageliq[i]==0))
      {
        if (sc>1)
        {        
          UseLeg(t,t_naiss[i])
          SimDir(i,t,"exo",ageref)
        }
        else
          
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



#### Sorties ####
graph_compar(MPENS       ,115,159,"Masse des pensions ")
graph_compar(PENREL      ,115,159,"Ratio pension/salaire")


save.image("~/Desktop/PENSIPP 0.1/Simulations/CN/Dispositifs NC/ANC.RData")