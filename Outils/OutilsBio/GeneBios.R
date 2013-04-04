#####################################   GENEBIO : GENERATEUR DES BIOGRAPHIES INDIVIDUELLES   ############################



cheminsource <- "P:/Retraites/PENSIPP 0.0/"

source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R"      )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )

setwd ( (paste0(cheminsource,"Mod�le/Param�tres/Destinie/Importation des bios"   )) )


# Lecture données d'état initial (NB : le tableau intermédiaire buf est lu sous forme de dataframe 
# et immédiatement converti en type matrix, ce qui est nécessaire pour l'extraction de la 
# sous-matrice des identifiants des enfants)
buf                          <- read.csv2("popini.csv",header=FALSE)
buf                          <- as.matrix(buf)
n                            <- nrow(buf)
print (c(n," individus lus dans PopIni.csv"))
statut[1:n,t_deb]            <- buf[,2]
anaiss[1:n]                  <- buf[,3]
sexe[1:n]                    <- buf[,4]
findet[1:n]                  <- buf[,5] 
pere[1:n]                    <- buf[,6]
mere[1:n]                    <- buf[,7]
n_enf[1:n]                   <- buf[,8]
enf[1:n,1:nb_enf_max]        <- buf[1:n,9:(8+nb_enf_max)]
age[which(statut[,t_deb]>0)] <- t_deb-anaiss[which(statut[,t_deb]>0)]+1
trim_naiss                   <- runif(1:taille_max)
trim_act                     <- runif(1:taille_max)-0.5
moisnaiss                    <- round (trim_naiss*11)

# Lecture carrières et biographies matrimoniales
buf                          <- read.csv2("biosta1.csv",header=FALSE)
buf                          <- as.matrix(buf)
n                            <- nrow(buf)
print (c(n," individus lus dans BioSta4.csv"))
statut[1:n,1:t_fin]        <- buf[1:n,2:161]

buf                          <- read.csv2("biosal1.csv",header=FALSE)
buf                          <- as.matrix(buf)
n                            <- nrow(buf)
print (c(n," individus lus dans BioSal4.csv"))
salaire[1:n,1:t_fin]        <- buf[1:n,2:161]


buf                        <- read.csv2("biomat1.csv",header=FALSE)
buf                        <- as.matrix(buf)
n                          <- nrow(buf)
print (c(n," individus lus dans BioMat4.csv"))
conjoint[1:n,1:t_deb]      <- buf[1:n,2:110]


#Obtenir les missing: 
MV<-numeric(taille_max)
MV<- rowSums(statut)
MV[!is.na(MV)]<-0
MV[is.na(MV)]<-1
MV<-MV*seq_len(nrow(statut))
listMV<-MV[MV!=0]
print (listMV)
#Solution provisoire: inactif quand missing
statut[is.na(statut)]<- -3



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
  # Les recalages etant fait séparément pour les hommes et les femmes, on s'appuie sur des taux 
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
   # print (c("séparation",i,t,conjoint[i,t])) 
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
  #  print (c("naissance",i,t,conjoint[i,t]))
    proba[i] <- proba_naissance(i,t)
  }

  for (a in 16:51)
  {
    print(c("out",a, t))
    liste_f <- tirage(proba*(age==a),cible=deflate(naissances[a,t-1]))  # Listes des meres
    
    liste   <- which(statut[,t]==0)                            # Liste des positions vacantes
#    print (liste)
    permut(liste_f)


    n <- arr_alea(0.488*length(liste_f)) # Nombre de naissances de garçons
#    print (c(t,"naissances",a,deflate(naissances[t-1,a]),length(liste_f),n))
    for (m in liste_f)
    {
      print (m)
      p                   <- conjoint[m,t]
      e                   <- shift(liste)
      print(e)
     print (n_enf[m])
     print (enf[m,])
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

#   
#   Simulation des deces
#   Version 1 : calage sur deces
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

# for (i in 1:taille_max)
# {
#   #print (i)
#   statut_ref <- rmult(1,codes_occ,c(.4,.2,.1,.1,.2)) 
#   sal_rel    <- rlnorm(1,meanlog=0,sdlog=1)
#   
#   for (t in 108:t_fin)
#   {
#      salaire[i,t] <- 0 
#      a <- t-anaiss[i]
#      if (a<0)
#      {
#         statut[i,t] <- pas_ne
#      }
#      else if (statut[i,t] > -1)
#      {
#         if (a <= findet[i])
#         {
#            statut[i,t] <- inactif
#         }
#         else
#         {
#            statut[i,t]  <- rmult(1,c(statut_ref,non_cadre,chomeur,inactif),c(0.8,0.1,0.1,0.1))
#            if (statut[i,t]>2)
#            {
#               salaire[i,t] <- sal_rel*PlafondSS[t]
#            }
#         }
#      }
# # #     print (c(i,findet[i],anaiss[i],t,a,statut_ref,statut[i,t]))
#   }
# }

# # Periode de chomage indemnise
# salrefchom <- matrix (0     ,nrow=taille_max,ncol=t_fin     )
# chomind <- matrix (0     ,nrow=taille_max,ncol=t_fin     )
# 
# 
# for (t in 1:160)
# {  
#   for (i in 1:2)
#   {  
#     if ((statut[i,t]== chomeur) && ((statut[i,(t-1)]==cadre) ||((statut[i,(t-2)]==cadre)&&(chomind[i,(t-1)]==1 ) ) )          
#     { 
#       chomind[i,t] <<- 1
#       #  #On prend comme assiete le dernier salaire percu avant chomage
#       liste <- which(statut[i,1:(t-1)]==cadre)
#       salrefchom[i,t]  <- salaire[i,liste[length(liste)]]
#     }
#     
#     if ((statut[i,t]== chomeur) && ((statut[i,(t-1)]==non_cadre) ||((statut[i,(t-2)]==non_cadre)&&(chomind[i,(t-1)]==1) ) ) )          
#     { 
#       chomind[i,t] <<- 1
#       #  #On prend comme assiete le dernier salaire percu avant chomage
#       liste <- which(statut[i,1:(t-1)]==cadre)
#       salrefchom[i,t]  <- salaire[i,liste[length(liste)]]
#     }
#   }  
# }


#save (anaiss,sexe,findet,pere,mere,conjoint,enf,n_enf,statut,salaire,file="Bios.RData")
#save.image(file="Bios.RData")
# 
# 
# 
# # Affichages subsidiaires
# 
# # A) Flux migratoires simulés par age en 2030
# # par(mfrow=c(1,2))
# # plot  ((migsim[1,,130]+migsim[2,,130])/taux_sondage,type="p",col="red",
# #        ylab="Flux",xlab="Age",ylim=c(-20000,+30000))
# # points((mig[1,,130]+mig[2,,130]),type="l",col="black")  
# 
# 
# # B) distribution des nombres d'enfants par génération
# par(mfrow=c(1,2))
# barplot(table(n_enf[is.element(anaiss, 50:59 )]),ylim=c(1,500),xlab="Générations 1950-1959")
# barplot(table(n_enf[is.element(anaiss,100:109)]),ylim=c(1,500),xlab="Générations 2000-2009")