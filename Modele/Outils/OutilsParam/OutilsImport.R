############################################ OUTILS POUR L'IMPORTATION DES PARAMETRES LEGISLATIFS ################################
# 
# 
# Cette biblioth??que rassemble les outils permettant d'importer les param??tres des fichiers Bar??mes IPP dont on se sert pour 
# la microsimulation: 
# - save_param : importe les donn??es des fichiers excel, sous la forme d'un objet "zoo", s??rie temporelle au pas mensuel
# - import_param_mois/trim/annee : ?? partir de la s??rie temporelle obtenue (augment?? de la projection jusqu'en 2100 dans ProjParam), 
# importe un vecteur de valeurs pour le param??tre, au format d??sir??. 


# NB: PROBLEME D'HARMONISATION DE LA PRESENTATION DANS LEGISLATION (lignes ?? supprimer, ...)
# Pour l'instant save_param ok si premi??re ligne = nom de la variable et deuxi??me ligne = ?? supprimer, 3e ligne = premi??re observation.


#Packages utilis??s
library (gdata)
library (zoo)



# Save Param : sauve un "zoo, s??rie temporelle du param??tre d'int??r??t, avec date au pas mensuel associ??. 
# Pour utilisation g??n??rale : changer chemin_param (chemin pour le fichier l??gislatif dont on extrait le param??tre)
# et chemin_sauv (dossier d'enregistrement du fichier)
# Exemple d'appel : RevaloRG <- save_param("RevaloRG","Coefficient","janv. 2015"), RevaloRG ??tant la feuille correspondante dans L??gislation Retraite 
# et Coefficient le nom de la varibale dans le fichier. A partir de janvier 2015, NAs.


param.save<-function(feuille,var,debproj="NULL") 
{
  chemin_param <- "P:/Retraites/Legislation/Barèmes IPP - Législation retraite.xlsx"
  chemin_sauv  <- "P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/"
  
  #Date de début de projection du barème, par défaut janvier 2013 
  
  # Importation de la feuille
  buf             <- read.xls(chemin_param,sheet=feuille,perl="P:/Retraites/Destinie/Perl/bin/perl.exe",na.strings = c("NA",""),method="tab")
  # Supression de la premi??re ligne
  buf             <- buf[-1,]
  # Supression des observation sans date
  buf             <- buf[!is.na(buf$Date),]
  # On garde uniquement les variables date et var
  buf             <- subset(buf,select=c("Date",var))
  # On convertit la date au format mois/an 
  buf$Date        <- as.yearmon(buf$Date)
  # Probl??me : read.xls importe les pourcentages et les valeurs en euros au format string (36,372 ???,  ). Solution provisoire(?)

  buf[,var]       <-sub("??,¬","", buf[,var])
  buf[,var]       <-sub("%","", buf[,var])
  buf[,var]       <-sub(",","", buf[,var])
  buf[,var]       <-as.numeric(buf[,var])
  
  # Dataframe avec toutes les dates de janvier 1901 ?? d??cembre 2100
  a          <- data.frame((seq(as.yearmon("1901-01"), as.yearmon("2100-12"), by=1/12)))
  colnames(a)<- "a"
  
  # Merge 
  buf <- merge(buf,a, by.x="Date",by.y="a",all=TRUE)
  
  # On extrait la s??rie temporelle du param??tre (format zoo)
  z <- zoo(buf[,var],buf$Date)
  # On compl??te var pour les dates non renseign??es (// carryforward sous stata)
  z <- na.locf(z, na.rm=FALSE)
  # On remplace par NA pour les dates au delà de la date de projection
  if (debproj=="NULL"){debproj <-"janv. 2013" }
  z[index(z)>=debproj]<-NA
  
  save (z,file=(paste0(chemin_sauv,var,".RData")))
}


# Series de fonctions qui cr??ent ?? partir de la s??rie temporelle un vecteur, dont la longueur d??pend du pas consid??r??
# NB: A faire apr??s la projection. 
# A utiliser lorsque l'on appelle les variables dans les programmes (DefVarRetr)

param.import.mois<-function(var)
{  
# Chargement de la s??rie temporelle  
chemin_sauv  <- "P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/"
load  (paste0(chemin_sauv,var,".RData"))

# Extraction du vecteur param??tre mensuel
param_mois       <- numeric (2400)
param_mois       <- coredata(z)

return (param_mois)
}


param.import.trim<-function(var)
{  
  chemin_sauv  <- "P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/"
  load  (paste0(chemin_sauv,var,".RData"))
  # Cr??ation de la s??rie temporelle trimestrielle
  z.qtr <- aggregate(z, as.yearqtr, mean)
  
  param_trim        <- numeric (800)
  param_trim        <- coredata(z.qtr)
  
  return (param_trim)
}


param.import.annee<-function(var)
{  
  chemin_sauv  <- "P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/"
  load  (paste0(chemin_sauv,var,".RData"))
  z.yr <- aggregate(z, floor, mean) 
  
  param_annee        <- numeric (200)
  param_annee        <- coredata(z.yr)
  
  return (param_annee)
}


# Outils de projection : 

# param.prolong projete la derni??re valeur jusqu'?? la fin de projection
param.prolong <-function(var)
{
chemin_sauv  <- "P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/"  

load  (paste0(chemin_sauv,var,".RData"))
z <- na.locf(z, na.rm=FALSE)
save (z,file=(paste0(chemin_sauv,var,".RData")))
}

# param.index projete la variable en indexant sur l'??volution annuelle d'une autre
    #ex: indexation de RevaloRG sur les prix ?? partir de 2008:  param.index("RevaloRG", "PRIX", 2010)
param.index <- function (var,index,date)
{
  #On charge les deux variables
  load  (paste0("P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/",index,".RData"))
  ind <- z
  load  (paste0("P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/",var,".RData"))
  # On remet ?? NA les valeur au-del?? de la date d'indexation
  z[index(z)>date] <- NA
  # On multiplie par la valeur du param??tre par le taux de variation de la variable index
  for (k in date:2100){coredata(z[index(z)==(k+1)])<-coredata(z[index(z)==k])*coredata(ind[index(ind)==k+1])/coredata(ind[index(ind)==k])}
  # On compl??te pour les valeurs manquante
  z[index(z)>=date] <- na.locf(z[index(z)>=date], na.rm=FALSE)
  
  save (z,  file= (paste0("P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/",var,".RData")))
# RETURN z? 
}


  