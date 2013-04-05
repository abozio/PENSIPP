


#####################################   DefVarRetr : INITIALISATION DES VARIABLES DE SIMULATION DE RETRAITE   ############################
# 
# NB : ParamMortalite (fonction survie)        
# 6.3  Autres parametres
#  CN ! 

# PLAN: 
#     VUE D'ENSEMBLE
# I.  VARIABLES INDIVIDUELLES D'INPUTS
# II. VARIABLES INDIVIDUELLES D'OUTPUTS
# III.  VARIABLES INDIVIDUELLES DE TRAVAIL 
# IV.   PARAMETRES LUS DANS LE FICHIER LEGISLATION
# V.  PARAMETRES GENERES LA FONCTION USELEG
# VI. DIVERS
#      VI.1. Codes
#      VI.2. Procedures Clone et Delete
#      VI.3. Parametres Comptes Notionnels 

####### VUE D'ENSEMBLE
# 
# Cette bibliotheque assure l'initialisation des variables indispensables a une
# simulation des retraites par PENSIPP. Le jeu defini par DefVarRetr est
# un jeu de base que l'utilisateur peut etoffer dans le programme appelant en
# fonction de ses besoins. En particulier, cette bibliotheque ne predefinit
# aucune variable de comptage macro : c'est a l'utilisateur qu'il revient de
# creer les variables dans lesquelles il stockera les resultats des comptages
# effectues au cours de la simulation.
# 
# Les variables que definit DefVarRetr sont essentiellement des variables
# individuelles et des parametres de calcul des droits a retraite. Plus
# precisement, on distinguera cinq rubriques :
# 
# 
# - VARIABLES INDIVIDUELS D'INPUTS : Les variables individuelles qui vont servir d'input aux simulations de
# retraite. Il va s'agir pour l'essentiel de donnees recuperees du generateur
# de biographies, et aussi de quelques parametres de comportement auxquels ce
# module attribue des valeurs par defaut, modifiables au besoin. 
# 
# - VARIABLES INDIVIDUELS D'OUTPUTS : Des variables individuelles d'output destinees 
# a accueillir les resultats de la simulation des droits a retraite. Le calcul de ces 
# droits constitue l'objet principal de la simulation. Il est realise a l'aide des 
# autres fonctions de la bibliotheque OutilsRetr. La encore, DefVarRetr se contente 
# de definir ces variables sans leur donner de valeurs par defaut.
# 
# - VARIABLES INDIVIDUELS DE TRAVAIL : Ces variables sont des
# scalaires utilises temporairement pour stocker des intermediaires de calcul.
# Leurs valeurs sont perdues lorsque le programme passe a la simulation de
# l'individu suivant. ce systeme economise la place memoire. Mais, en fonction
# des besoins, on peut creer des variables permanentes dans le programme
# appelant pour en stocker les resultats.
# 
# - PARAMETRES LUS DANS LE FICHIER LEGISLATION : Series temporelles lues dans 
# le fichier bareme retraite IPP. Les valeurs par defaut de ces parametres sont lues par DefVarRetr, 
# et peuvent etre modifiees ensuite au gre des scenarios.
#                                     
# - PARAMETRES GENERES LA FONCTION USELEG : Des parametres de calcul des droits a retraite plus complexes, traites comme
# scalaires. Leurs valeurs sont fixes par appel du programme UseLeg.
#                                     
# - DIVERS : 
# Une derniere categorie hybride comprenant une variable Options definisant
# les options de simulation et mise a jour par la procedure UseOpt de OutilsRetr
# et un ensemble de codes pour quelques variables individuelles, facilitant les
# operations de tabulation sur ces variables.Outre ces definition de variables, cette bibliotheque definit quelques procedures
# de tabulation complementaires aux procedures standard de OutilsMS, telles
# que des moyennes par age et par generation. Ces procedures sont definies dans cette
# bibliotheque plutot que dans OutilsBase  car elles supposent la predeclaration
# des variables age et anaiss qui sont inconnues de OutilsBase.


##### I. VARIABLES INDIVIDUELLES D'INPUT  #####

##Liste des variables

# - Variables lues directement dans les fichiers issus du generateur de
# biographies
#                                          
# anaiss[i]         : annee de naissance
# trim_naiss[i]     : variable [0,1] pour affiner la date de naissance
# moisnaiss[i]      : mois de naissance (0=janvier, 11=décembre)
# trim_act          : variable [-0,5,0,5] pour affiner la date de début d'activité
# sexe[i]           : sexe
# findet[i]         : age de fin d'etudes
# pere[i]           : identifiant du pere
# mere[i]           : identifiant
# matri[i]          : statut matrimonial
# conjoint[i]       : identifiant du conjoint
# enf[i][e]         : identifiant du e-ieme enfant
# statut[i,a]      : statut par id et age
# salaire[i,a]     : salaire par id et age                      
# deces[i]          : indique si l'individu est decede l'annee en cours
# 
# - Variables de preferences 
# 
# taux[i]           : preference pour le present
# k[i]              : preference pour le loisir
# gamma[i]          : aversion pour le risque
# seuil[i]          : seuil de declenchement du report du depart en retraite
# Les variables de preference taux, k, gamma et seuil et ageexo,
# elles sont initialisees a, respectivement, 0.03, 4, 2, 0 et 65. Elles sont
# modifiables par le programme appelant si ce parametrage ne convient pas ou si
# on veut introduire une heterogeneite individuelle de ces parametres.


##Initialisation ## 

anaiss     <- numeric(taille_max)
adeces     <- numeric(taille_max)
age        <- numeric(taille_max)
sexe       <- numeric(taille_max)
findet     <- numeric(taille_max)
pere       <- numeric(taille_max)
mere       <- numeric(taille_max)
n_enf      <- numeric(taille_max)
trim_naiss <- numeric(taille_max)
trim_act   <- numeric(taille_max)
enf        <- matrix (0     ,nrow=taille_max,ncol=nb_enf_max)
conjoint   <- matrix (0     ,nrow=taille_max,ncol=t_fin     )
statut     <- matrix (0     ,nrow=taille_max,ncol=t_fin     )    
salaire    <- matrix (0     ,nrow=taille_max,ncol=t_fin     )
anaiss     <- numeric(taille_max)
adeces     <- numeric(taille_max)
age        <- numeric(taille_max)
sexe       <- numeric(taille_max)

taux       <- numeric(taille_max)
k          <- numeric(taille_max)
gamme      <- numeric(taille_max)
seuil      <- numeric(taille_max)



# ##### II. VARIABLES INDIVIDUELLES D'OUTPUT #####
# 
# ##Liste des variables                                    
# 
# Il s'agit dans tous les cas de variables simples a
# un seul indice stockant des caracteristiques individuelles instantanees.
# DefVarRetr se contente de predefinir celles de ces variables d'output qui sont
# indispensables qu fonctionnement du programme, i.e. celles que vont renseigner
# les procedures de OutilsRetr.
# 
# pension_rg[i]     : pension regime general
# pension_ar[i]     : pension arrco
# pension_ag[i]     : pension agirc
# pension_fp[i]     : pension fonction publique
# pension_in[i]     : pension d'independant
# pension_cn_pri[i] : pension comptes notionnels secteur prive
# pension_cn_fp[i]  : pension comptes notionnels secteur public
# pension[i]        : pension directe totale (somme des precedentes)
# VFU_rg[i]         : montant du versement forfaitaire unique au RG
# VFU_ar[i]         : montant du versement forfaitaire unique a l'Arrco
# VFU_ag[i]         : montant du versement forfaitaire unique a l'Agirc
# rev_rg[i]         : reversion regime general
# rev_ar[i]         : reversion arrco
# rev_ag[i]         : reversion agirc
# rev_fp[i]         : reversion fonction publique
# rev_in[i]         : reversion d'independant
# rev_cn_pri[i]     : reversions comptes notionels secteur prive
# rev_cn_fp[i]      : reversions comptes notionels secteur public
# rev[i]            : reversion totale (somme des cinq precedentes)
# majo_min_rg[i]    : montant de la majoration de pension au RG liee a l'application du minimum contributif (montant differentiel inclus dans pension_rg)
# majo_min_in[i]    : montant de la majoration de pension au regime d'independant liee a l'application du minimum contributif (montant differentiel inclus dans pension_rg)
# majo_min_fp[i]    : montant de la majoration de pension a la FP liee a l'application du minimum garanti (montant differentiel inclus dans pension_rg)
# majo_3enf_rg[i]   : montant de la majoration de pension totale (droits directs + reversion) pour 3 enfants au RG
# majo_3enf_ar[i]   : montant de la majoration de pension totale (droits directs + reversion) pour 3 enfants a l'ARRCO
# majo_3enf_ag[i]    : montant de la majoration de pension totale (droits directs + reversion) pour 3 enfants a l'AGIRCmajo_3enf_in[i]   : montant de la majoration de pension totale (droits directs + reversion) pour 3 enfants au regime d'independant
# majo_3enf_fp[i]   : montant de la majoration de pension totale (droits directs + reversion) pour 3 enfants a la FP
# ageliq[i]         : age a la liquidation de la totalite des droits (age entier au 31/12 de l'annee)
# agefin_primoliq[i]: age fin (au mois pres) a la premiere liquidation d'un droit direct
# agefin_totliq[i]  : age fin (au mois pres) a la liquidation de la totalite des droits directs (superieur ou egal a agefin_primoliq[i])
# pliq[i]           : pension a la liquidation
# ageliqrev[i]      : age (entier en difference de millesime) a la liquidation des pensions de reversion
# dar[i]            : indicatrice qui precise si l'individu part en anticipe pour carrieres longues      #ajout oct 2010
# liq[i]            : indicatrice qui precise que tous les droits directs ont ete liquides
# primoliq[i]       : indicatrice qui precise que les premiers droits directs (a la FP) ont ete liquides, mais pas forcement ceux au RG
# durdecote_rg [i]  : duree (en annee) determinant la decote au rg et pour les independants (duree = 0 pour un depart sans decote)
# dursurcote_rg[i]  : duree (en annee) determinant la surcote au rg et pour les independants (duree = 0 pour un depart sans surcote)
# durdecote_fp[i]   : duree (en annee) determinant la decote a la fp (duree = 0 pour un depart sans decote)
# dursurcote_fp[i]  : duree (en annee) determinant la surcote a la fp (duree = 0 pour un depart sans surcote)
# tauxliq_rg[i]     : taux de liquidation au RG (=1 en cas de depart sans decote ni surcote) et au regime des independants
# tauxliq_fp[i]     : taux de liquidation a la FP (=1 en cas de depart sans decote ni surcote)
# tauxliq_ar[i]     : taux de liquidation a l'ARRCO eta l'AGIRC (=1 en cas de depart sans decote ni surcote)
# tp[i]             : indicatrice de depart avec le taux plein (definie differemment selon les regimes d'appartenance)

pension_rg      <- numeric(taille_max)
pension_ar      <- numeric(taille_max)
pension_ag      <- numeric(taille_max)
pension_fp      <- numeric(taille_max)
pension_in      <- numeric(taille_max)
pension_cn_pri  <- numeric(taille_max)
pension_cn_fp   <- numeric(taille_max)
pension         <- numeric(taille_max)
rev_rg          <- numeric(taille_max)
rev_ar          <- numeric(taille_max)
rev_ag          <- numeric(taille_max)
rev_fp          <- numeric(taille_max)
rev_in          <- numeric(taille_max)
rev_cn_pri      <- numeric(taille_max)
rev_cn_fp       <- numeric(taille_max)
rev             <- numeric(taille_max)
ageliq          <- numeric(taille_max)
pliq            <- numeric(taille_max)
liq             <- numeric(taille_max)
min_vieil       <- numeric(taille_max)
indic_mc        <- numeric(taille_max)
indic_mg        <- numeric(taille_max)
dar             <- numeric(taille_max)


# ##### VARIABLES INDIVIDUELLES DE TRAVAIL ######
# 
# ##Liste des variables
# 
# duree_rg      : duree cotisee au regime general
# duree_fp      : duree cotisee dans les regimes du secteur public
# duree_fpa     : idem, en service actif
# duree_fps     : idem, en service sedentaire
# duree_in      : duree cotisee en regime d'independant
# duree_tot     : duree cotisee totale
# duree_avpf    : duree passee en AVPF
# duree_rg_maj  : duree RG incluant AVPF et MDA (selon options)
# duree_fp_maj  : duree FP incluant MDA eventuelle (selon options)
# duree_in_maj  : duree independant incluant MDA eventuelle(selon options)
# duree_tot_maj : duree totale incluant AVPF et MDA (selon options)
# dureecotdra_tot:duree totale cotisee, servant pour le calcul de l'eligibilite a la retraite anticipee pour carriere longue
# dureecotmin_tot:duree totale cotisee, servant pour le calcul des minimum contributif et garanti
# sam_rg        : SAM servant au calcul de la pension RG
# sam_in        : SAM pour le calcul de la pension d'independant
# sr_fp         : salaire de reference pour le calcul de la pension FP
# samuni_rgin   : SAM calcule sur l'ensemble des revenus d'activite annuels au RG et chez les independants
# samuni        : SAM calcule sur l'ensemble des revenus d'activite (tous regimes), selon les regles du RG
# points_arrco  : cumul points ARRCO
# points_agirc  : cumul points AGIRC
# points_FP     : cumul points FP "fictif" (calcules sur les salaires FP, comme s'il s'agissait de salaires sur l'assiette ARRCO)
# points_cn_pri : cumul points compte notionnel du secteur prive
# points_cn_fp  : cumul points compte notionnel du secteur public
# min_cont      : minimum contributif au RG
# min_cont_in   : minimum contributif au RSI (regime d'independant)
# min_garanti   : minimum garanti
# id_clone      : id de l'individu de depart lorsqu'on travaille sur un clone
# liqpublic     : indicatrice de liquidation de la pension a la FP
# liqprive      : indicatrice de liquidation des pensions dans les regimes du prive (RG et independants)
# liqtot        : indicatrice de liquidation de la totalite des pension


duree_rg        <- 0
duree_fp        <- 0
duree_fpa       <- 0
duree_fps       <- 0
duree_in        <- 0
duree_tot       <- 0
duree_avpf      <- 0
duree_cho       <- 0
duree_PR        <- 0
duree_emp       <- 0
duree_rg_maj    <- 0
duree_in_maj    <- 0
duree_tot_maj   <- 0
sam_rg          <- 0
sam_in          <- 0
sr_fp           <- 0
taux_prime      <- 0
points_arrco    <- 0
points_agirc    <- 0
points_cn_pri   <- 0
points_cn_fp    <- 0
min_cont        <- 0
min_garanti     <- 0


##### IV. PARAMETRES LUS DANS LE FICHIER BAREME RETRAITE #####

# Pour calculer les droits a retraite et proceder aux simulations de comportement
# qui vont alimenter ces variables d'output, la bibliotheque OutilsRetr et les
# bibliotheques qu'elle appelle vont avoir besoin de nombreux parametres. Une
# partie de ces parametres sont stockes dans les fichier legislation 
# 
# - Le fichier Baremes IPP - Legislation retraite.xls contient des elements de baremes qui ont la
# forme de series temporelles simples. Pour ces series temporelles, l'indice 0 correspond a l'annee 1900.
# Par exemple PlafondSS[98] est le plafond de la securite sociale de l'annee
# 1998, PlafondSS[102] est celui de l'annee 2002.
# 
# On precise que les donnees monetaires de ce fichier sont des donnees nominales,
# a la conversion pres en euros des anciens francs et des francs en Euros.
#                                                                                                                                          
# 
# Parametres generaux
# Prix          : Indice de prix (2005=1)
# PlafondSS     : Plafond de la securite sociale (BRUT ANNUEL, nominal)
# SMIC          : le SMIC
# PointFP       : la valeur du point fonction publique

Prix           <- numeric(180)
PlafondSS      <- numeric(180)
SMIC           <- numeric(180)
PointFP        <- numeric(180)
SMPT           <- numeric(180)
PIB            <- numeric(180)

# Parametres retraite de base et pensions minimales
# 
# RevaloSPC       : Coeff de revalorisation des salaires portes au compte (base 1 en 2003)
# RevaloRG        : Coeff revalo nominale des pensions RG en cours de service
# RevaloFP        : Coeff revalo nominale des pensions FP en cours de service
# TauxSalRGSP     : Taux salarie regime general sous plafond
# TauxEmpRGSP     : Taux employeur regime general sous plafond
# TauxTotRGSP     : Taux global regime general sous plafond
# TauxSalRGSalTot : Taux RG sur l'ensemble du salaire, partie salariee (rq : cette cotisation est affectee a l'assurance veuvage jusqu'en 2004, a l'assurance vieillesse ensuite)
# TauxEmpRGSalTot : Taux RG sur l'ensemble du salaire, partie patronale
# TauxTotRGSalTot : Taux RG sur l'ensemble du salaire, partie patronale
# TauxCotFP       : Taux de cotisation salarie du secteur public
# MinVieil1       : minimum vieillesse personne seule
# MinVieil2       : minimum vieillesse pour un couple
# MinCont1        : minimum contributif
# MinCont2        : majoration du minimum contributif
# MinPR           : minimum de pension du regime general
# CoefMdaRG       : Coefficient MDA du RG
# CoefMdaFP       : Coefficient MDA de la FP

RevaloSPC       <- param.import.annee
RevaloRG        <- param.import.annee("RevaloRG")
RevaloFP        <- param.import.annee
TauxSalRGSP1    <- param.import.annee("TauxSalRGSP")
TauxEmpRGSP     <- param.import.annee)
TauxTotRGSP     <- TauxSalRGSP + TauxEmpRGSP
TauxSalRGSalTot <- param.import.annee)
TauxEmpRGSalTot <- param.import.annee)
TauxTotRGSalTot <- TauxRGSalTot + TauxRGSalTot
TauxFP          <- param.import.annee)
MinVieil1       <- param.import.annee)
MinVieil2       <- param.import.annee)
Mincont1        <- param.import.annee)
Mincont2        <- param.import.annee)
MinPR           <- param.import.annee)
CoefMdaRG       <- param.import.annee)
CoefMdaFP       <- param.import.annee)

# Parametres des regimes complementaires
#                                                                        
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

TauxARRCO_1     <- param.import.annee
TauxARRCO_2     <- param.import.annee
TauxARRCO_S1    <- param.import.annee
TauxARRCO_S2    <- param.import.annee
TauxAppARRCO    <- param.import.annee
SalRefARRCO     <- param.import.annee
ValPtARRCO      <- param.import.annee
TauxAGIRC_B     <- param.import.annee
TauxAGIRC_C     <- param.import.annee
TauxAGIRC_SB    <- param.import.annee
TauxAGIRC_SC    <- param.import.annee
TauxAppAGIRC    <- param.import.annee
SalRefAGIRC     <- param.import.annee
ValPtAGIRC      <- param.import.annee
GMP             <- param.import.annee

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

TauxRevRG       <- param.import.annee
MinRevRG        <- param.import.annee
MaxRevRG        <- param.import.annee
PlafRevRG       <- param.import.annee
TauxRevARRCO    <- param.import.annee
TauxRevAGIRC    <- param.import.annee
TauxRevFP       <- param.import.annee
TauxRevInd      <- param.import.annee


# Parametres de calcul des preretraites
#                                                                        
# TauxPR1       : Taux preretraite tranche 1
# TauxPR2       : Taux preretraite tranche 2
# TauxPR3       : Taux preretraite tranche 3
# AgeEligPR     : Age d'acces a la preretraite

TauxPR1         <- param.import.annee
TauxPR2         <- param.import.annee
TauxPR3         <- param.import.annee
AgeEligPR       <- param.import.annee


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

BMAF            <- param.import.annee
PlafCF1         <- param.import.annee
PlafCF2         <- param.import.annee
PlafCF3         <- param.import.annee
PlafCF4         <- param.import.annee
PlafCF5         <- param.import.annee
MajoPlafCF      <- param.import.annee
PlafARS1        <- param.import.annee
PlafARS2        <- param.import.annee
PlafARS3        <- param.import.annee
PlafARS4        <- param.import.annee
PlafARS5        <- param.import.annee


 
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

TauxMalSP       <- param.import.annee
TauxMalTot      <- param.import.annee
TauxAGFF_1      <- param.import.annee
TauxAGFF_2      <- param.import.annee
TauxAssedic     <- param.import.annee
TauxCSGSal      <- param.import.annee
TauxCSGRet      <- param.import.annee
SeuilExoCSG     <- param.import.annee




# - Parametres de l'eventuel regime en comptes notionels VI
# 
# RendementCN   : Rendement capital durant la vie active
# RendementCNPrev : Le meme en previsionnel (pour calcul coeffs de conversion)
# TauxCotCN     : Taux de cotisation
# RevaloCN      : Revalorisation apres liquidation


##### V. Parametres generes par la fonction Useleg #####

AgeMinRG        <- 0
AgeMinFP        <- 0
AgeMinFPS       <- 0
AgeMinFPA       <- 0
AgeMaxRG        <- 0
AgeMaxFP        <- 0
AgeAnnDecRG     <- 0
AgeAnnDecFP     <- 0
AgeMinMG        <- 0
ageouvdroitfp   <- 0
DureeCibRG      <- 0
DureeProratRG   <- 0
DureeCalcSAM    <- 0
TauxPleinRG     <- 0
DecoteRG        <- 0
DureeCibFP      <- 0
DecoteFP        <- 0
SurcoteFP       <- 0
SurcoteRG1      <- 0
SurcoteRG2      <- 0
SurcoteRG3      <- 0
DureeMinFP      <- 0
AgeAnnDecFP     <- 0
AnOuvDroitFP    <- NA
MajTauxRGMax    <- 0
TauxRGMax       <- 0
DureeValCibDRA  <- numeric(4)
DureeCotCibDRA  <- numeric(4)
DebActCibDRA    <- numeric(4)
AgeDRA          <- numeric(4)