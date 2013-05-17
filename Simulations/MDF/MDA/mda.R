############################### MDA  ###################

# Masse des pensions pour chaque dispositif. 
# Neutralisation tour à tour de chaque dispositif. 

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
pens    <- matrix(nrow=taille_max,ncol=200)


MSAL        <- matrix(nrow=7,ncol=200)    # Masse salariale par année
MPENS       <- matrix(nrow=7,ncol=200)    # Masse des pensions année
MPENSa       <- matrix(nrow=7,ncol=200)    # Masse des pensions année
RATIOFIN    <- matrix(nrow=7,ncol=200)    # Ratio masse des pensions/masse des salaires par année
SALMOY      <- matrix(nrow=7,ncol=200)    # Salaire moyen par année
PENMOY      <- matrix(nrow=7,ncol=200)    # Pension moyenne par année
PENREL      <- matrix(nrow=7,ncol=200)    # Ratio pension/salaire
PENLIQMOY   <- matrix(nrow=7,ncol=200)    # Pension moyenne à liquidation
PENLIQMOY  <- matrix(nrow=7,ncol=200)    # Pension moyenne à liquidation
MPENLIQ     <- matrix(nrow=7,ncol=200)    # Masse des pension à liquidation
MPENLIQa     <- matrix(nrow=7,ncol=200)    # Masse des pension à liquidation
W           <- 2047.501
cibletaux<-numeric(taille_max)
cot<-numeric(taille_max)



#### Début de la simulation ####

#  Rprof(tmp<-tempfile())
for (sc in c(1,2))
  #  1: Normal Ref  
  #  2: No bonif
  #  3: No MDA
  #  4: No AVPF
  #  5: Neutralisation touts Avantages familiaux (MDA, bonif, AVPF)  

{
  # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
  setwd ( (paste0(cheminsource,"Simulations/MDF"                                    )) )
  
  if (sc==2) {UseOpt(c("nomda"  ))}

  
  for (t in 80:160)   # Début boucle temporelle
  {
    print (c(sc,t))
    
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
          pliq_[i,sc]   <- pension[i]
          pliq_rg[i,sc] <- pension_rg[i]
          pliq_fp[i,sc] <- pension_fp[i]
          
          if (sc==1) 
          {
            ageref[i] <- t-t_naiss[i]
          }
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
      PENLIQMOY[sc,t]    <- mean (pension[which( (pension[]>0)&t_liq[]==t)])
      PENREL[sc,t]       <- PENMOY[sc,t]/SALMOY[sc,t]
    }  
    
    pens[,t]<-pension[]
  } # Fin de de la boucle temporelle
  
  
  
} # Fin boucle scenarios



#### Sorties ####


save.image(paste0(cheminsource,"Simulations/MDF/MDA/mda.RData"))

