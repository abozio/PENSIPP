###############  BIOS DESTINIE  ******************

# Extraction des biographie individuelle (liens familiaux, statuts, salaire) de la base de Destinie 2009. 
# Programmes conjoint et statut dans P:\Retraites\Destinie\VersR


cheminsource <- "P:/Retraites/PENSIPP 0.0/"



source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R"      )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )

setwd (  (paste0(cheminsource,"Modele/Parametres/Destinie/Importation des bios"  )) )

# Lecture données d'état initial (NB : le tableau intermédiaire buf est lu sous forme de dataframe 
# et immédiatement converti en type matrix, ce qui est nécessaire pour l'extraction de la 
# sous-matrice des identifiants des enfants)
buf                          <- read.csv2("pop.csv",header=FALSE)
buf                          <- as.matrix(buf)
n                            <- nrow(buf)
print (c(n," individus lus dans Pop.csv"))
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
buf                          <- read.csv2("biosta.csv",header=FALSE)
buf                          <- as.matrix(buf)
n                            <- nrow(buf)
print (c(n," individus lus dans BioSta.csv"))
statut[1:n,1:160]        <- buf[1:n,2:161]
# Decede quands plus de statut
statut[is.na(statut)]<- -3

buf                          <- read.csv2("biosal.csv",header=FALSE)
buf                          <- as.matrix(buf)
n                            <- nrow(buf)
print (c(n," individus lus dans BioSal.csv"))
salaire[1:n,1:160]        <- buf[1:n,2:161]
# =0 quands plus de statut
salaire[is.na(salaire)]<- 0


buf                        <- read.csv2("biomat.csv",header=FALSE)
buf                        <- as.matrix(buf)
n                          <- nrow(buf)
print (c(n," individus lus dans BioMat.csv"))
conjoint[1:n,1:160]      <- buf[1:n,2:161]



### Periodes de chomage indemnise
salrefchom <- matrix (0     ,nrow=taille_max,ncol=t_fin     )
chomind    <- matrix (0     ,nrow=taille_max,ncol=t_fin     )


for (t in 1:160)
{  
  for (i in 1:taille_max)
  {  
    if ( (statut[i,t]== chomeur) && ((statut[i,(t-1)]==cadre) ||((statut[i,(t-2)]==cadre)&&(chomind[i,(t-1)]==1) ) ) )          
    { 
      chomind[i,t] <- 1
      #  #On prend comme assiete le dernier salaire percu avant chomage
      liste <- which(statut[i,1:(t-1)]==cadre)
      salrefchom[i,t]  <- salaire[i,liste[length(liste)]]
    }
        
        if ((statut[i,t]== chomeur) && ((statut[i,(t-1)]==non_cadre) ||((statut[i,(t-2)]==non_cadre) && (chomind[i,(t-1)]==1) ) ) )          
        { 
          chomind[i,t] <- 1
          #  #On prend comme assiete le dernier salaire percu avant chomage
          liste <- which(statut[i,1:(t-1)]==cadre)
          salrefchom[i,t]  <- max(0,salaire[i,liste[length(liste)]])
        }
  }  
}

# Variables sauvegard�es

save (taille_max,anaiss,sexe,findet,pere,mere,conjoint,enf,n_enf,statut,salaire,age,trim_naiss,trim_act,k,chomind,salrefchom,file=(paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie.RData")))
