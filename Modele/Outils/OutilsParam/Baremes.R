#####################################   BAREMES : IMPORTATION ET PROJECTION DES BAREMES   ############################

# PROVISOIRE: a unifier avec la legislation fiscale. 
# PROVISOIRE: classement (selon hypothese de projection par exemple). 
# ATTENTION: Pour le moment, date de fin de l??gislation et de d??but de projection est janvier 2013: ?? adapter (dans OutilsImport et Baremes chaque ann??e)

# ATTENTION : uniformisation des fichiers l??gislatifs: 
# - Premi??re  ligne avec nom de variable telle que appelle (deuxi??me arguement de la fonction), deuxi??me ligne supprim??e
# - Unit?? mon??taire : euro simple (pas euro (??? 123 par ex))
# - Rien d'autre que des dates dans la colonne date


library (gdata)
library (zoo)

# I. Importation des Param??tres généraux 
# Pour l'instant: Importation des s??ries Destinie: à am??liorer. 

buf     <- read.xls("P:/Retraites/Legislation/ParamEco.xlsx",sheet="1",perl="P:/Retraites/Destinie/Perl/bin/perl.exe",na.strings = c("NA",""),method="tab")
z <- zoo(buf[,"Indice.Prix"],buf$Date, frequency=1)
save (z,file="P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/PRIX.RData")
z <- zoo(buf[,"SMIC"],buf$Date, frequency=1)
save (z,file="P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/SMIC.RData")
z <- zoo(buf[,"SMPT"],buf$Date, frequency=1)
save (z,file="P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/SMPT.RData")
z <- zoo(buf[,"PIB.nominal"],buf$Date, frequency=1)
save (z,file="P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/PIB.RData")


#Salaire Moyen Par T??te (SMPT)

# II. Importation et projection des bar??mes des retraite (Bar??mes IPP - L??gislation retraite.xls)

source("P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/OutilsImport.R")
chemin_sauv  <- "P:/Retraites/PENSIPP 0.0/Outils/OutilsParam/"
 
# Parametres retraite de base et pensions minimales
# 
# RevaloSPC       : Coeff de revalorisation des salaires portes au compte (base 1 en 2003)
# Hypothese de projection/retrospectif: egal a 1 apres 2012 et avant premiere date (BIZARRE?)
# RevaloRG        : Coeff revalo nominale des pensions RG en cours de service
# Hypothese de projection/retrospectif: egal a 1 apres 2012 et avant premiere date (BIZARRE?)
# RevaloFP        : Coeff revalo nominale des pensions FP en cours de service
# Hypothese de projection/retrospectif: egal a 1 apres 2012 et avant premiere date (BIZARRE?)

# TauxSalRGSP     : Taux salarie regime general sous plafond
# TauxEmpRGSP     : Taux employeur regime general sous plafond
# TauxSalRGSalTot : Taux RG sur l'ensemble du salaire, partie salariee (rq : cette cotisation est affectee a l'assurance veuvage jusqu'en 2004, a l'assurance vieillesse ensuite)
# TauxEmpRGSalTot : Taux RG sur l'ensemble du salaire, partie patronale
  # Augmentation programmée jusqu'à 2016, constant ensuite
# TauxCotFP       : Taux de cotisation salarie du secteur public
  # Hypothese de projection: augmentation programm??e jusqu'?? 2020 par loi de 2010, constant ensuite
# MinVieil1       : minimum vieillesse personne seule
# MinVieil2       : minimum vieillesse pour un couple
# Hypothese de projection: 2% de croissance annuelle (hyp Destinie). NB: conversion en euros (F et AF)
# MinCont1        : minimum contributif
# MinCont2        : majoration du minimum contributif
# Hypothese de projection/retrospectif: Egal à 0 avant premi??re date et constant après 
# MinPR           : minimum de pension du regime general
# CoefMdaRG       : Coefficient MDA du RG
# CoefMdaFP       : Coefficient MDA de la FP

#PlafondSS
param.save("PSS","pss")
load  (paste0(chemin_sauv,"pss",".RData"))
z[index(z)<2002]<-z[index(z)<2002]/6.55957
z[index(z)<1960]<-z[index(z)<1960]/100
save (z,file=(paste0(chemin_sauv,"pss",".RData")))
param.index("pss","SMPT", 2012)

#RevaloSPC    
param.save("RevaloRG","RevaloRG")
load  (paste0(chemin_sauv,"PRIX",".RData"))
p <- z
load  (paste0(chemin_sauv,"RevaloRG",".RData"))
for (k in 2012:2100){coredata(z[index(z)==(k+1)])<-coredata(ind[index(ind)==k+1])/coredata(ind[index(ind)==k])}
z<- na.locf(z, na.rm=FALSE)
save (z,file=(paste0(chemin_sauv,"RevaloRG",".RData")))


#RevaloFP      

# TauxSalRGSP
param.save("Cotisations_RG","TauxSalRGSP","janv. 2017")
load  (paste0(chemin_sauv,"TauxSalRGSP",".RData"))
z <- na.locf(z, na.rm=FALSE)
z[is.na(coredata(z))] <- 0
z<-z/100
save (z,file=(paste0(chemin_sauv,"TauxSalRGSP",".RData")))

#TauxEmpRGSP     
param.save("Cotisations_RG","TauxEmpRGSP","janv. 2017")
load  (paste0(chemin_sauv,"TauxEmpRGSP",".RData"))
z <- na.locf(z, na.rm=FALSE)
z[is.na(coredata(z))] <- 0
z<-z/100
save (z,file=(paste0(chemin_sauv,"TauxEmpRGSP",".RData")))


TauxTotRGSalTot    <- param.save()

#TauxFP             
param.save("Retenue_pension","ret_pens")
load  (paste0(chemin_sauv,"ret_pens",".RData"))
z[index(z)>=2013]<-8.66
z[index(z)>=2014]<-8.93
z[index(z)>=2015]<-9.20
z[index(z)>=2016]<-9.47
z[index(z)>=2017]<-9.74
z[index(z)>=2018]<-10.01
z[index(z)>=2019]<-10.28
z[index(z)>=2020]<-10.55
z<- z/100
save (z,file=(paste0(chemin_sauv,"ret_pens",".RData")))

# MinVieil1       
param.save("MV_merge","mv_seul_ann")
load  (paste0(chemin_sauv,"mv_seul_ann",".RData"))
z[index(z)<2002]<-z[index(z)<2002]/6.55957
z[index(z)<1960]<-z[index(z)<1960]/100
for (k in 2013:2100){z[index(z)==k]<-z[index(z)==k-1]*1.02}
z[index(z)>2012]<- na.locf(z[index(z)>2012])
save (z,file=(paste0(chemin_sauv,"mv_seul_ann",".RData")))
# MinVieil2       
param.save("MV_merge","mv_couple_ann")
load  (paste0(chemin_sauv,"mv_couple_ann",".RData"))
z[index(z)<2002]<-z[index(z)<2002]/6.55957
z[index(z)<1960]<-z[index(z)<1960]/100
for (k in 2013:2100){z[index(z)==k]<-z[index(z)==k-1]*1.02}
z[index(z)>2012]<- na.locf(z[index(z)>2012])
save (z,file=(paste0(chemin_sauv,"mv_couple_ann",".RData")))


# Mincont1      
param.save("Min_contributif","min_cont")
load  (paste0(chemin_sauv,"min_cont",".RData"))
z[index(z)<2002]<-z[index(z)<2002]/6.55957
z <- na.locf(z, na.rm=FALSE)
z[is.na(coredata(z))] <- 0
save (z,file=(paste0(chemin_sauv,"min_cont",".RData")))
# Mincont2        
param.save("Min_contributif","min_cont_maj")
load  (paste0(chemin_sauv,"min_cont_maj",".RData"))
z[index(z)<2002]<-z[index(z)<2002]/6.55957
z <- na.locf(z, na.rm=FALSE)
z[is.na(coredata(z))] <- 0
save (z,file=(paste0(chemin_sauv,"min_cont_maj",".RData")))

MinPR           <- param.save()
CoefMdaRG       <- param.save()
CoefMdaFP       <- param.save()


# Parametres des regimes complementaires
# Hypotheses de projection/retrospectif: constant apres 2012 et =0 avant premiere date                            
# ###############################################################################
# NB : ACTUELLEMENT, PARTAGE SALARIE/EMPLOYEUR SUPPOSE FIXE a 40/60. Verifier
# ###############################################################################
#                                                                        
# TauxARRCO_1   : Taux contractuel global ARRCO tranche 1
# TauxARRCO_2   : Taux contractuel global ARRCO tranche 2
# TauxARRCO_S1  : Taux contractuel salarie ARRCO tranche 1
# TauxARRCO_S2  : Taux contractuel salarie ARRCO tranche 2 
# TauxAppARRCO  : Taux appel ARRCO
# SalRefARRCO   : Salaire de reference ARRCO (nominal)
# ValPtARRCO    : Valeur du point ARRCO (nominal)
# TauxAGIRC_B   : Taux contractuel global AGIRC tranche B
# TauxAGIRC_C   : Taux contractuel global AGIRC tranche C
# TauxAGIRC_SB  : Taux contractuel salarie AGIRC tranche B
# TauxAGIRC_SC  : Taux contractuel salarie AGIRC tranche C
# TauxAppAGIRC  : Taux Appel AGIRC
# SalRefAGIRC   : Salaire de reference AGIRC (nominal)
# ValPtAGIRC    : Valeur du point AGIRC (nominal)

# GMP           : garantie minimale de point a l'AGIRC

TauxARRCO_1     <- numeric(200)
TauxARRCO_2     <- numeric(200)
TauxARRCO_S1    <- numeric(200)
TauxARRCO_S2    <- numeric(200)
#TauxAppARRCO
param.save("Cotisations_arrco","taux_appel_arrco")
load  (paste0(chemin_sauv,"taux_appel_arrco",".RData"))
z <- na.locf(z, na.rm=FALSE)
z[is.na(coredata(z))] <- 0
z<-z/100
save (z,file=(paste0(chemin_sauv,"taux_appel_arrco",".RData")))

# SalRefARRCO    
param.save("Salaire_ref_arrco","sal_ref_arrco_euros")
load  (paste0(chemin_sauv,"sal_ref_arrco_euros",".RData"))
z <- na.locf(z, na.rm=FALSE)
z[is.na(coredata(z))] <- 0
save (z,file=(paste0(chemin_sauv,"sal_ref_arrco_euros",".RData")))

# ValPtARRCO     
param.save("Val_point_arrco","val_point_arrco")
load  (paste0(chemin_sauv,"val_point_arrco",".RData"))
z <- na.locf(z, na.rm=FALSE)
z[is.na(coredata(z))] <- 0
save (z,file=(paste0(chemin_sauv,"val_point_arrco",".RData")))

TauxAGIRC_B     <- numeric(200)
TauxAGIRC_C     <- numeric(200)
TauxAGIRC_SB    <- numeric(200)
TauxAGIRC_SC    <- numeric(200)
TauxAppAGIRC    <- numeric(200)

# SalRefAGIRC
param.save("Salaire_ref_agirc","sal_ref_agirc_euros")
load  (paste0(chemin_sauv,"sal_ref_agirc_euros",".RData"))
z <- na.locf(z, na.rm=FALSE)
z[is.na(coredata(z))] <- 0
save (z,file=(paste0(chemin_sauv,"sal_ref_agirc_euros",".RData")))

# ValPtAGIRC      
param.save("Val_point_agirc","val_point_agirc")
load  (paste0(chemin_sauv,"val_point_agirc",".RData"))
z <- na.locf(z, na.rm=FALSE)
z[is.na(coredata(z))] <- 0
save (z,file=(paste0(chemin_sauv,"val_point_agirc",".RData")))


GMP             <- numeric(200)

# Parametres de calcul des reversions
#                                                                        
# TauxRevRG     : Taux de reversion du RG
# MinRevRG      : Reversion minimale du RG
# MaxRevRG      : Reversion maximale du RG
# PlafRevRG     : Plafond de ressources reversions RG et In
# TauxRevARRCO  : Taux de reversion ARRCO
# TauxRevAGIRC  : Taux de reversion AGIRC
# TauxRevFP     : Taux de reversion de la FP
# TauxRevInd    : Taux de reversion des regimes d'independants

TauxRevRG       <- numeric(200)
MinRevRG        <- numeric(200)
MaxRevRG        <- numeric(200)
PlafRevRG       <- numeric(200)
TauxRevARRCO    <- numeric(200)
TauxRevAGIRC    <- numeric(200)
TauxRevFP       <- numeric(200)
TauxRevInd      <- numeric(200)


# Parametres de calcul des preretraites
#                                                                        
# TauxPR1       : Taux preretraite tranche 1
# TauxPR2       : Taux preretraite tranche 2
# TauxPR3       : Taux preretraite tranche 3
# AgeEligPR     : Age d'acces a la preretraite

TauxPR1         <- numeric(200)
TauxPR2         <- numeric(200)
TauxPR3         <- numeric(200)
AgeEligPR       <- numeric(200)


# Parametres des prestations familiales
#                                                                        
# BMAF          : Base mensuelle des allocations familiales
# PlafCF1       : Plafond ressources du CF et de l'APJE (1er enfant)
# PlafCF2       : Plafond ressources du CF et de l'APJE (2eme enfant)
# PlafCF3       : Plafond ressources du CF et de l'APJE (3eme enfant)
# PlafCF4       : Plafond ressources du CF et de l'APJE (4eme enfant)
# PlafCF5       : Plafond ressources du CF et de l'APJE (enfant suppl.)
# MajoPlafCF    : Majoration du plafond ressources du CF et de l'APJE
# PlafARS1      : Plafond de ressource de l'ARS (1er enfant)
# PlafARS2      : Plafond de ressource de l'ARS (2eme enfant)
# PlafARS3      : Plafond de ressource de l'ARS (3eme enfant)
# PlafARS4      : Plafond de ressource de l'ARS (4eme enfant)
# PlafARS5      : Plafond de ressource de l'ARS (enfant suppl.)

BMAF            <- numeric(200)
PlafCF1         <- numeric(200)
PlafCF2         <- numeric(200)
PlafCF3         <- numeric(200)
PlafCF4         <- numeric(200)
PlafCF5         <- numeric(200)
MajoPlafCF      <- numeric(200)
PlafARS1        <- numeric(200)
PlafARS2        <- numeric(200)
PlafARS3        <- numeric(200)
PlafARS4        <- numeric(200)
PlafARS5        <- numeric(200)



# Taux de cotisation divers
# Les valeurs lues dans ce fichier doivent-etre vues comme des valeurs de
# reference qui peuvent faire l'objet de modifications selon les besoins des
# scenarios.                              
#                                                                        
# ################################################################################
# NB : SEUIL d'EXONERATION CSG ET CRDS TRES APPROXIMATIFS ET FORMULE A PRECISER
#  (DANS OUTILS DROITS).
# ################################################################################
# 
# TauxMalSP     : Taux de cotisation maladie sous plafond (jusqu'en 1979)
# TauxMalTot    : Taux de cotisation maladie deplafonnee (depuis 1967)
# TauxAGFF      : Taux AGFF (depuis 2001)
# TauxASSEDIC   : Taux assurance chomage
# TauxCSGSal    : Taux CSG sur salaires
# TauxMalRetFra : Taux de cotisation maladie sur les retraites (personnes domicilees fiscalement en France)
# TauxMalRetEtr : Taux de cotisation maladie sur les retraites (personnes domicilees fiscalement hors de France)
# TauxCSGRet    : Taux CSG +CRDS sur retraites
# SeuilExoCSG   : Seuil d'exoneration CSG sur retraites

TauxMalSP       <- numeric(200)
TauxMalTot      <- numeric(200)
TauxAGFF_1      <- numeric(200)
TauxAGFF_2      <- numeric(200)
TauxAssedic     <- numeric(200)
TauxCSGSal      <- numeric(200)
TauxCSGRet      <- numeric(200)
SeuilExoCSG     <- numeric(200)

  


II. Projection des baremes 
