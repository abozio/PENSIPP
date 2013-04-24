########## Test MICO REGIME ACTUEL ############
### Ce programme vise à évaluer les effet du MICO dans PENSIPP, en comparant les 
# résulats obtenus (% de bénéficiaires, masse, aux chiffres de la CNAV)


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
pliq_         <- matrix(nrow=taille_max,ncol=2)
pliq_rg    <- matrix(nrow=taille_max,ncol=2)
pliq_fp    <- matrix(nrow=taille_max,ncol=2)
drg        <- matrix(nrow=taille_max,ncol=2)
dtot       <- matrix(nrow=taille_max,ncol=2)
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


    
mc          <-      numeric(taille_max) 
mincont     <-      numeric(taille_max) 
minga       <-      numeric(taille_max) 
mg          <-      numeric(taille_max) 

#### Début de la simulation ####

for (sc in 1:2)     #Debut boucle scénarios
{       
  
  # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )                                     
  
  # Options de simulation  
  # UseOpt(c("nobonif","nomda","noassimil","nomc","nomg","noptsgratuits","noavpf"))
  W           <- 2047.501    # Poids de l'échantillon
  if (sc==2) {UseOpt(c("nomc","nomg"))}

  for (t in 80:160)   # Début boucle temporelle
  {
    print (c(sc,t))

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
          pliq_[i,sc]   <- pension[i]
          pliq_rg[i,sc] <- pension_rg[i]
          pliq_fp[i,sc] <- pension_fp[i]
          if (sc==1) 
          {
          ageref[i] <- t-t_naiss[i]
          mc[i] <- indic_mc[i]
          mg[i] <- indic_mg[i]
          mincont[i]<-min_cont
          minga[i]  <-min_garanti
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
      SALMOY[sc,t]       <- mean (salaire[actifs,t]/Prix[t])
      MPENS[sc,t]        <- W*sum(pension[retraites])/Prix[t]     
      MSAL[sc,t]         <- W*sum(salaire[actifs,t])/Prix[t] 
      RATIOFIN[sc,t]     <- MPENS[sc,t]/MSAL[sc,t]
      PENMOY[sc,t]       <- mean (pension[retraites]/Prix[t])
      PENREL[sc,t]       <- PENMOY[sc,t]/SALMOY[sc,t]
    }  
      
  } # Fin de de la boucle temporelle
  


} # Fin boucle scenarios



#### Sorties ####
# Avec/sans mico dans ancien régime
# Comparaison avec chiffres CNAV 

#I. Stock: 42% des individus concernés en 2007

regimegen<-which(pliq_rg[,1]>0)
length(which(mc[regimegen]>0))/length(regimegen)   # % Individu au rg au mc
length(which(mc[regimegen]>0 & sexe[regimegen]==1))/length(which(sexe[regimegen]==1)) # % H au rg au mc
length(which(mc[regimegen]>0 & sexe[regimegen]==2))/length(which(sexe[regimegen]==2)) # % F au rg au mc

# II. Niveau de pension:  
# la pension moyenne hors minimum et avantages accessoires8 est de 2 700 € pour les femmes 
#et de 1 775 € pour les hommes. En intégrant le minimum contributif,
#les pensions annuelles versées par le régime général passent respectivttemen
# à 4 024 € et 2 482 €
beneficiairesH <- which(mc>0 & sexe==1)
beneficiairesF <- which(mc>0 & sexe==2)
mean(pliq_rg[beneficiairesF,1])
mean(pliq_rg[beneficiairesH,1])
mean(pliq_rg[beneficiairesF,2])
mean(pliq_rg[beneficiairesH,2])


save.image(paste0(cheminsource,"Simulations/CN/Dispositifs NC/Minima de pension/Testmico2.RData"))