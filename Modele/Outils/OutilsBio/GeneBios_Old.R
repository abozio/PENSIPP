
source( (paste0(cheminsource,"PENSIPP 0.0/Modele/Outils/OutilsRetraite/OutilsMS.R")) )
source( (paste0(cheminsource,"PENSIPP 0.0/Modele/Outils/OutilsRetraite/OutilsPensIPP.R")) )
source( (paste0(cheminsource,"PENSIPP 0.0/Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
 setwd((paste0(cheminsource,"PENSIPP 0.0/Modele/Param�tres/Demographie")

# Lecture données micro (NB : le tableau intermédiaire buf est lu sous forme de dataframe 
# et immédiatement converti en type matrix, ce qui est nécessaire pour l'extraction de la 
# sous-matrice des identifiants des enfants)
buf                          <- read.csv2("PopIni.csv",header=FALSE)
buf                          <- as.matrix(buf)
n                            <- nrow(buf)
statut[1:n,t_deb]            <- buf[,2]
anaiss[1:n]                  <- buf[,3]
sexe[1:n]                    <- buf[,4]
findet[1:n]                  <- buf[,5] 
pere[1:n]                    <- buf[,6]
mere[1:n]                    <- buf[,7]
conjoint[1:n,t_deb]          <- buf[,8]
n_enf[1:n]                   <- buf[,9]
enf[1:n,1:nb_enf_max]        <- buf[1:n,10:(9+nb_enf_max)]
age[which(statut[,t_deb]>0)] <- t_deb-anaiss[which(statut[,t_deb]>0)]+1
trim_naiss                   <- runif(1:taille_max)
trim_act                     <- runif(1:taille_max)-0.5


# variables micro annexes
liste        <- numeric(taille_max)
liste_h      <- numeric(taille_max)
liste_f      <- numeric(taille_max) 
proba        <- numeric(taille_max)
n_enf_c      <- numeric(taille_max)
aidants      <- numeric(taille_max)
parents      <- numeric(taille_max)

# Lecture des données de projection
pop          <- array(0,dim=c(2,age_max,t_fin))  
deces        <- array(0,dim=c(2,age_max,t_fin))
mig          <- array(0,dim=c(2,age_max,t_fin))
naissances   <- array(0,dim=c(  age_max,t_fin))
pop[1,,]     <- read_ap("PopHommes.csv"     ,age="colonne")
pop[2,,]     <- read_ap("PopFemmes.csv"     ,age="colonne")
deces[1,,]   <- read_ap("DecesHommes.csv"   ,age="colonne")
deces[2,,]   <- read_ap("DecesFemmes.csv"   ,age="colonne")
mig[1,,]     <- read_ap("SoldeMigHommes.csv",age="colonne")
mig[2,,]     <- read_ap("SoldeMigFemmes.csv",age="colonne")
naissances   <- read_ap("Naissances.csv"    ,age="colonne")

# Données intermédiaires et d'output
naiss_tot     <- array(dim=c(t_fin))
naiss_tot_sim <- array(dim=c(t_fin))
popini        <- array(dim=c(1,age_max))
popsim        <- array(dim=c(2,age_max,t_fin))
migsim        <- array(dim=c(2,age_max,t_fin))
en_couple     <- array(dim=c(2,age_max,t_fin))


# Calage sur les pyramides des ages initiales hommes et femmes
par(mfrow=c(1,2))
for (s in 1:2)
{
  # Les recalage étant fait séparément pour les hommes et les femmes, on s'appuie sur des taux 
  # de sondage spécifiques par sexe
  taux_sondage  <- length(which(statut[,t_deb]>0 & sexe==s))/sum(pop[s,1:age_max,t_deb])
  popini        <- pyram(statut[,t_deb]>0 & sexe==s)
  for (a in 1:age_max)
  {
    liste <- permut(which(statut[,t_deb]>0 & sexe==s & age==a)) 
    n     <- deflate(pop[s,a,t_deb])-length(liste)
    if ((n<0) & (a<age_max))
    {  
      liste         <- sel(liste,-n)
      age[liste]    <- a+1
      anaiss[liste] <- t_deb-a-1
    }
    else if (n>0)
    {
      liste         <- which(statut[,t_deb]>0 & sexe==s & age==a+1)
      liste         <- permut(liste)
      liste         <- sel(liste,n)
      age[liste]    <- a
      anaiss[liste] <- t_deb-a
    }
  }
  plot  (popini,type="p",col="red",ylab="effectifs",xlab="age",
         main=switch(s,"1"="Hommes","2"="Femmes"))
  points(pyram(statut[,107]>0 & sexe==s),type="l",col="black")
  points (pop[s,,107],type="l",col="green")
  plot  (popini-(pop[s,,107]),type="p",col="red",
         ylab="Ecarts à la pyramide réelle",xlab="age",
         main="")
  points(pyram(statut[,107]>0 & sexe==s)-pop[s,,107],type="l",col="black")
}

# Simulation
par(mfrow=c(1,1))
taux_sondage  <- length(which(statut[,t_deb]>0))/(sum(pop[1,1:age_max,t_deb])+sum(pop[2,1:age_max,t_deb]))
set.seed(123456789)

for (t in (t_deb+1):t_fin)
{
  
  cat (paste(substring(date(),12,19),": année",1900+t),"\n")
  
  # Calcul données année précédente
  for (s in 1:2)
  {
    popsim[s,,t-1]    <- pyram(statut[,t-1]>0 & sexe==s)
    en_couple[s,,t-1] <- pyram(statut[,t-1]>0 & conjoint[,t-1]>0 & sexe==s)
  }
  if (t %% 5 ==0) 
  {
    plot   (pop[1,,t-1]+pop[2,,t-1],    
            type="l",col="orangered",
            main=paste("Année",t+1900),
            xlab="age",ylab="effectifs",
            ylim=c(0,1000000))
    points (popsim[1,,t-1]+popsim[2,,t-1],
            type="l",col="black")
    points (en_couple[1,,t-1]+en_couple[2,,t-1],
            type="l",col="green")
    legend (x=85,y=975000,cex=0.7,bty="n",lwd=1,
            legend=c("Pop réelle","Pop simulée","En couple"),
            col=c("orangered","black","green"))
  }

  # Vieillissement et actualisation des données
  age[which(statut[,t-1]>0)] <- age[which(statut[,t-1]>0)]+1
  statut[,t]                 <- statut[,t-1]
  conjoint[,t]               <- conjoint[,t-1]
  
  # Simulation des séparations
  for (i in 1:taille_max)
  {
     proba[i] <- proba_separ(i,t)
  }
  liste <- tirage(proba,cible="sommeprob")
  for (i in liste)
  {
    conjoint[conjoint[i,t],t] <- -2
    conjoint[i,t]             <- -2
  }
  
  # Simulation des mises en couples
  for (i in 1:taille_max) 
  {
    proba[i] <- proba_union(i,t)
  }
  liste    <- tirage(proba,cible="sommeprob")
  liste_h  <- tri(sel(liste,keep=(sexe==1)),age)
  liste_f  <- tri(sel(liste,keep=(sexe==2)),age)
  while (length(liste_f)>0 && length(liste_h)>0)
  {
    f           <- shift(liste_f)
    h           <- shift(liste_h)
    liste_f     <- sel(liste_f,-1)
    liste_h     <- sel(liste_h,-1)
    conjoint[f,t] <- h
    conjoint[h,t] <- f
  }
    
  # Alimentation de la tranche 0 : simulation des naissances
  # On fait la liste des positions vacantes (liste) puis, age par age,
  # celle des meres potentielles (liste_f) et on en  
  # tire le nombre requis pour simuler exactement le nombre de naissances par age
  # de la mère de l'année écoulée, qu'on affecte aux positions données par liste

  for (i in 1:taille_max)
  {
    proba[i] <- proba_naissance(i,t)
  }
  for (a in 16:51)
  {
    liste_f <- tirage(proba*(age==a),cible=deflate(naissances[a,t-1]))  # Listes des meres
    liste   <- which(statut[,t]==0)                            # Liste des positions vacantes
    permut(liste_f)


    n <- arr_alea(0.488*length(liste_f)) # Nombre de naissances de garçons
#    print (c(t,"naissances",a,deflate(naissances[t-1,a]),length(liste_f),n))
    for (m in liste_f)
    {
      p                   <- conjoint[m,t]
      e                   <- shift(liste)
      liste               <- sel(liste,-1)
      sexe[e]             <- 1+(n>0)
      n                   <- n-1
      mere[e]             <- m
      pere[e]             <- p
      findet[e]           <- 0.5*(findet[p]+findet[m])
      enf[m,n_enf[m]+1]   <- e
      enf[p,n_enf[p]+1]   <- e
      n_enf[m]            <- n_enf[m]+1
      n_enf[p]            <- n_enf[p]+1
      anaiss[e]           <- t
      age[e]              <- 1
      statut[e,t]         <- 1
      conjoint[e,t]       <- celib
      enf[e,]             <- rep(0,nb_enf_max)
      n_enf[e]            <- 0
    }
  }
  
  # Simulation des migrations
  for (s in 1:2)
  {
    for (a in 1:age_max)
    {
         
      # Flux sortant : on tire les sortants parmi les présents d'age a
      if (mig[s,a,t-1]<0)
      {
        liste           <- which(statut[,t]>0 & sexe==s & age==a)
        liste           <- permut(liste)
        liste           <- sel(liste,deflate(-mig[s,a,t-1]))
        migsim[s,a,t]   <- length(liste)
        statut[liste,t] <- hors_terr
      }
      # Flux entrant : on tire dans la liste des positions vacantes 
      else if (mig[s,a,t-1]>0)
      {
        liste           <- which(statut[,t] == 0)
        liste           <- sel(liste,deflate(mig[s,a,t-1]))
        migsim[s,a,t]   <- length(liste)
        statut[liste,t] <- 1
        age[liste]      <- a
        sexe[liste]     <- s
        anaiss[liste]   <- t-a+1
      }
    }
  }
  
  # Simulation des décès
  # Version 1 : calage sur decès
  for (s in 1:2)
  {
    for (a in 1:age_max)
    {
      liste            <- which(statut[,t]>0 & sexe==s & age==a)
      liste            <- sel(liste,deflate(deces[s,a,t-1]))
      statut[liste,t]  <- decede
    }
  }  
  
  
#   # Version 2 : calage sur population 
#   for (s in 1:2)
#   {
#     for (a in 1:age_max)
#     {
#       liste              <- which(statut[,t]>0 & sexe==s & age==a)
#       deces              <- length(liste)-deflate(pop[s,a,t])
#       if (deces>0)
#       {
#         liste            <- permut(liste)
#         liste            <- sel(liste,deces)
#         statut[liste,t]  <- decede
#       }
#     }
#   }


#   # Calcul taille de l'environnement familial
#   if (t==t_deb | t==t_fin)
#   {
#     for (i in 1:taille_max)
#     {
#       aidants[i] <- nb_enf(i,option="tous")
#       if (matri[i]==2) {aidants[i] <- aidants[i]+1}
#       parents[i] <- 0
#       if (!is.na(pere[i])) {if (statut[pere[i]]>0) {parents[i] <- parents[i]+1}}
#       if (!is.na(mere[i])) {if (statut[mere[i]]>0) {parents[i] <- parents[i]+1}}
#     }
#     plot  (by_age(aidants,(sexe==1),pas=5),col="blue")
#     points(by_age(aidants,(sexe==2),pas=5),col="pink")
#     points(by_age(parents,pas=5),col="green")
#   }
  
#   # ecriture des résultats
#   save (anaiss,sexe,findet,pere,mere,conjoint,
#          enf,statut,
#          file="essai.RData")
  
}
     

# Module marché du travail : ajout de carrières totalement fictives, y compris valeurs rétrospectives
# pour la variable statut
# On tire un statut dominant sur le Mdt, et le statut effectif pourra s'en écarter aléatoirement, 
# en fonction d'épisodes de chomage, d'inactivité, ou de passages en non cadres du privé pour les
# fonctionnaires ou indépendants
# On tire par ailleurs un salaire relatif exprimé en % du plafond de la secu

for (i in 1:taille_max)
{
#  print (i)
  statut_ref <- rmult(1,codes_occ,c(.4,.2,.1,.1,.2)) 
  sal_rel    <- rlnorm(1,meanlog=0,sdlog=1)
  
  for (t in 1:t_fin)
  {
     salaire[i,t] <- 0 
     a <- t-anaiss[i]
     if (a<0)
     {
        statut[i,t] <- pas_ne
     }
     else if (statut[i,t] > -1)
     {
        if (a <= findet[i])
        {
           statut[i,t] <- inactif
        }
        else
        {
           statut[i,t]  <- rmult(1,c(statut_ref,non_cadre,chomeur,inactif),c(0.8,0.1,0.1,0.1))
           if (statut[i,t]>2)
           {
              salaire[i,t] <- sal_rel*PlafondSS[t]
           }
        }
     }
#     print (c(i,findet[i],anaiss[i],t,a,statut_ref,statut[i,t]))
  }
}

save("Bios.RData")
