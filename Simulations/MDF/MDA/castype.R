#### Cas type. 

# Chargement des données source
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R"      )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R"          )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsRetr.R"         )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsCN.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  


# Création des cas-types.
c<-1:5      
sexe[c]   <-2
t_naiss[c]<-70
n_enf[c]  <-3
for (i in 1:5) # Date de naissance des enfants
{
for (e in 1:n_enf[i])
{
  enf[i,e]     <- 10+e
  t_naiss[10+e] <- 90+2*e
}
}
# Etudes
statut[c,(t_naiss[1]:t_naiss[1]+20)]<-1
# Carrière
statut[c,(t_naiss[1]+20):(t_naiss[1]+70)]<-3  # Cadre
# Interruption: carrière type choisie: TP grâce à Majoration. 
statut[c,(t_naiss[1]+25):(t_naiss[1]+29)]<-inactif
salaire[c,(t_naiss[1]+25):(t_naiss[1]+29)]<-0
# Salaire: evolution du salaire tout au long de la carrière
for (t in (t_naiss[1]+29):(t_naiss[1]+70))
{
  salaire[1,t] <- PlafondSS[t]*affn(t,c(t_naiss[1]+60,t_naiss[1]+60),c(0.5,1))
  salaire[2,t] <- PlafondSS[t]*affn(t,c(t_naiss[1]+60,t_naiss[1]+60),c(0.75,1.25))
  salaire[3,t] <- PlafondSS[t]*affn(t,c(t_naiss[1]+60,t_naiss[1]+60),c(1,1.5))
  salaire[4,t] <- PlafondSS[t]*affn(t,c(t_naiss[1]+60,t_naiss[1]+60),c(1,2))
  salaire[5,t] <- PlafondSS[t]*affn(t,c(t_naiss[1]+60,t_naiss[1]+60),c(1.5,3))
}
save (t_naiss,sexe,enf,n_enf,statut,salaire,file=(paste0(cheminsource,"Modele/Outils/OutilsBio/castype.RData")))

# Simulation. 
pliq_       <- matrix(nrow=5,ncol=2)

for (sc in 1:2)
{
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/castype.RData"        )) )    
if (sc==2){UseOpt("nomda")}  

for (t in 120:150)
{
  Leg <- t
  
  # Liquidation
  for (i in 1:5)       # Début boucle individuelle
  {
    if ((t-t_naiss[i]>=55) && (ageliq[i]==0))
    {
    if (sc==2)
    {   
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
      pliq_[i,sc]   <- pension[i]
    }
    
  }
  
  else if (ageliq[i]>0)
  { 
    Revalo(i,t,t+1)    
  }  
  
} # Fin boucle i
} # Fin boucle t 
} # Fin boucle scénario

# Gains à la MDA: 
gains<-numeric(5)
gains<-pliq_[,1]-pliq_[,2]