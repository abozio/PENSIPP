

#####################################   DefVarRetr : INITIALISATION DES VARIABLES DE SIMULATION DE RETRAITE   ############################
# 

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
# des variables age et t_naiss qui sont inconnues de OutilsBase.




# Parametres generaux de la microsimulation
taille_max   <- 110000
nb_enf_max   <- 6 
age_max      <- 108
#age_max      <- 121
t_deb        <- 109
t_fin        <- 160

# Codes conjoint
celib     <- -1
separ     <- -2
veuf      <- -3
#Codes statut
pas_ne    <- -1
decede    <- -3
inactif   <-  1
chomeur   <-  2
non_cadre <-  3
cadre     <-  4
fonct_a   <-  5
fonct_s   <-  6
indep     <-  7
avpf      <-  8
preret    <-  9
codes_act <- c(2:7)
codes_occ <- c(3:7)

inactifCN   <-  101
chomeurCN   <-  102
non_cadreCN <-  103
cadreCN     <-  104
fonct_aCN   <-  105
fonct_sCN   <-  106
indepCN     <-  107
avpfCN      <-  108
avpf      <-  8
preret    <-  9
codes_actCN <- c(102:107)
codes_occCN <- c(103:107)



# Declaration variables micro
t_naiss    <- numeric(taille_max)
t_naiss[]  <- 999
t_deces    <- numeric(taille_max)
t_deces[]  <- 999
age        <- numeric(taille_max)
sexe       <- numeric(taille_max)
findet     <- numeric(taille_max)
tauxprime  <- numeric(taille_max)
pere       <- numeric(taille_max)
mere       <- numeric(taille_max)
n_enf      <- numeric(taille_max)
trim_naiss <- numeric(taille_max)
trim_act   <- numeric(taille_max)
moisnaiss  <- numeric(taille_max)
enf        <- matrix (0     ,nrow=taille_max,ncol=nb_enf_max)
conjoint   <- matrix (0     ,nrow=taille_max,ncol=t_fin     )
statut     <- matrix (0     ,nrow=taille_max,ncol=t_fin     )    
salaire    <- matrix (0     ,nrow=taille_max,ncol=t_fin     )
k          <- numeric(taille_max)
delta_k    <- numeric(taille_max)
cibletaux  <- numeric(taille_max)
beta       <- numeric(taille_max)


Prix           <- numeric(180)
PlafondSS      <- numeric(180)
SMIC           <- numeric(180)
PointFP        <- numeric(180)
SMPT           <- numeric(180)
PIB            <- numeric(180)

# Chomage indemnis?:
salrefchom <- matrix (0     ,nrow=taille_max,ncol=t_fin     )
chomind    <- matrix (0     ,nrow=taille_max,ncol=t_fin     )



# Variables retraite individuelles principales
pension_rg      <- numeric(taille_max)
pension_ar      <- numeric(taille_max)
pension_ag      <- numeric(taille_max)
pension_fp      <- numeric(taille_max)
pension_in      <- numeric(taille_max)
pension_cn_pri  <- numeric(taille_max)
pension_cn_fp   <- numeric(taille_max)
pension_cn_ind  <- numeric(taille_max)
pension_cn_nc   <- numeric(taille_max)
pension_nomin   <- numeric(taille_max)
pension         <- numeric(taille_max)
mccn            <- numeric(taille_max)
cotret          <- numeric(taille_max)
cotrettot       <- numeric(taille_max)
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
t_liq           <- numeric(taille_max)
t_liq[]         <- 999
min_vieil       <- numeric(taille_max)
indic_mc        <- numeric(taille_max)
indic_mg        <- numeric(taille_max)
dar             <- numeric(taille_max)

# Variables retraite intermediaires (ajouts pour CN à améliorer)
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
duree_empCN     <- 0
duree_rg_maj    <- 0
duree_in_maj    <- 0
duree_tot_maj   <- 0
dureecotdra_tot <- 0
sam_rg          <- 0
sam_in          <- 0
sr_fp           <- 0
taux_prime      <- 0
points_arrco    <- 0
points_agirc    <- 0
points_cn_pri   <- 0
points_cn_fp    <- 0
points_cn_ind   <- 0
points_cn_nc    <- 0
points_mccn     <- 0
txmc            <- 0
plafond         <- 0
min_cont        <- 0
min_garanti     <- 0


# Param??tres de type serie temporelle
Prix            <- numeric(200)
PlafondSS       <- numeric(200)
SMIC            <- numeric(200)
PointFP         <- numeric(200)
SMPT            <- numeric(200)
PIB             <- numeric(200)
RevaloSPC       <- numeric(200)
RevaloRG        <- numeric(200)
RevaloFP        <- numeric(200)
TauxSalRGSP     <- numeric(200)
TauxEmpRGSP     <- numeric(200)
TauxTotRGSP     <- numeric(200)
TauxSalRGSalTot <- numeric(200)
TauxEmpRGSalTot <- numeric(200)
TauxTotRGSalTot <- numeric(200)
TauxFP          <- numeric(200)
MinVieil1       <- numeric(200)
MinVieil2       <- numeric(200)
Mincont1        <- numeric(200)
Mincont2        <- numeric(200)
MinPR           <- numeric(200)
CoefMdaRG       <- numeric(200)
CoefMdaFP       <- numeric(200)
TauxARRCO_1     <- numeric(200)
TauxARRCO_2     <- numeric(200)
TauxARRCO_S1    <- numeric(200)
TauxARRCO_S2    <- numeric(200)
TauxAppARRCO    <- numeric(200)
SalRefARRCO     <- numeric(200)
ValPtARRCO      <- numeric(200)
TauxAGIRC_B     <- numeric(200)
TauxAGIRC_C     <- numeric(200)
TauxAGIRC_SB    <- numeric(200)
TauxAGIRC_SC    <- numeric(200)
TauxAppAGIRC    <- numeric(200)
SalRefAGIRC     <- numeric(200)
ValPtAGIRC      <- numeric(200)
GMP             <- numeric(200)
TauxRevRG       <- numeric(200)
MinRevRG        <- numeric(200)
MaxRevRG        <- numeric(200)
PlafRevRG       <- numeric(200)
TauxRevARRCO    <- numeric(200)
TauxRevAGIRC    <- numeric(200)
TauxRevFP       <- numeric(200)
TauxRevInd      <- numeric(200)
TauxPR1         <- numeric(200)
TauxPR2         <- numeric(200)
TauxPR3         <- numeric(200)
AgeEligPR       <- numeric(200)
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
TauxMalSP       <- numeric(200)
TauxMalTot      <- numeric(200)
TauxAGFF_1      <- numeric(200)
TauxAGFF_2      <- numeric(200)
TauxAssedic     <- numeric(200)
TauxCSGSal      <- numeric(200)
TauxCSGRet      <- numeric(200)
SeuilExoCSG     <- numeric(200)
RendementCN     <- numeric(200)
RendementCNPrev <- numeric(200)
TauxCotCN       <- numeric(200)
RevaloCN        <- numeric(200)



# Lecture des parametres simples
setwd((paste0(cheminsource,"Modele/Parametres/Destinie/Parametres Sociaux")))
buf <- read.csv2("ParamEco.csv",dec=".",sep=";",header=TRUE)
t_min <- buf[1,1]%%1900
t_max <- buf[nrow(buf),1]%%1900
Prix[t_min:t_max]            <- buf[1:nrow(buf),2]
PlafondSS[t_min:t_max]       <- buf[1:nrow(buf),3]
SMIC[t_min:t_max]            <- buf[1:nrow(buf),4]
PointFP[t_min:t_max]         <- buf[1:nrow(buf),5]
SMPT[t_min:t_max]            <- buf[1:nrow(buf),6]
PIB[t_min:t_max]             <- buf[1:nrow(buf),7]
PIB[t_min:t_max]            <- PIB[t_min:t_max]*1000000000
buf <- read.csv2("ParamRetBase.csv",dec=".",sep=";",header=TRUE)
t_min <- buf[1,1]%%1900
t_max <- buf[nrow(buf),1]%%1900
RevaloSPC[t_min:t_max]       <- buf[1:nrow(buf),2]
RevaloRG[t_min:t_max]        <- buf[1:nrow(buf),3]
RevaloFP[t_min:t_max]        <- buf[1:nrow(buf),4]
TauxSalRGSP[t_min:t_max]     <- buf[1:nrow(buf),5]
TauxEmpRGSP[t_min:t_max]     <- buf[1:nrow(buf),6]
TauxTotRGSP[t_min:t_max]     <- TauxSalRGSP[t_min:t_max] + TauxEmpRGSP[t_min:t_max]
TauxTotRGSalTot[t_min:t_max] <- buf[1:nrow(buf),7]
TauxFP[t_min:t_max]          <- buf[1:nrow(buf),8]
MinVieil1[t_min:t_max]       <- buf[1:nrow(buf),9]
MinVieil2[t_min:t_max]       <- buf[1:nrow(buf),10]
Mincont1[t_min:t_max]        <- buf[1:nrow(buf),11]
Mincont2[t_min:t_max]        <- buf[1:nrow(buf),12]
MinPR[t_min:t_max]           <- buf[1:nrow(buf),13]
CoefMdaRG[t_min:t_max]       <- buf[1:nrow(buf),14]
CoefMdaFP[t_min:t_max]       <- buf[1:nrow(buf),15]
buf <- read.csv2("ParamRetComp.csv",dec=".",sep=";",header=TRUE)
t_min <- buf[1,1]%%1900
t_max <- buf[nrow(buf),1]%%1900
TauxARRCO_1[t_min:t_max]     <- buf[1:nrow(buf),2]
TauxARRCO_2[t_min:t_max]     <- buf[1:nrow(buf),3]
TauxARRCO_S1[t_min:t_max]    <- buf[1:nrow(buf),4]
TauxARRCO_S2[t_min:t_max]    <- buf[1:nrow(buf),5]
TauxAppARRCO[t_min:t_max]    <- buf[1:nrow(buf),6]
SalRefARRCO[t_min:t_max]     <- buf[1:nrow(buf),7]
ValPtARRCO[t_min:t_max]      <- buf[1:nrow(buf),8]
TauxAGIRC_B[t_min:t_max]     <- buf[1:nrow(buf),9]
TauxAGIRC_C[t_min:t_max]     <- buf[1:nrow(buf),10]
TauxAGIRC_SB[t_min:t_max]    <- buf[1:nrow(buf),11]
TauxAGIRC_SC[t_min:t_max]    <- buf[1:nrow(buf),12]
TauxAppAGIRC[t_min:t_max]    <- buf[1:nrow(buf),13]
SalRefAGIRC[t_min:t_max]     <- buf[1:nrow(buf),14]
ValPtAGIRC[t_min:t_max]      <- buf[1:nrow(buf),15]
GMP[t_min:t_max]             <- buf[1:nrow(buf),16]
buf <- read.csv2("ParamRev.csv",dec=".",sep=";",header=TRUE)
t_min <- buf[1,1]%%1900
t_max <- buf[nrow(buf),1]%%1900
TauxRevRG[t_min:t_max]       <- buf[1:nrow(buf),2]
MinRevRG[t_min:t_max]        <- buf[1:nrow(buf),3]
MaxRevRG[t_min:t_max]        <- buf[1:nrow(buf),4]
PlafRevRG[t_min:t_max]       <- buf[1:nrow(buf),5]
TauxRevARRCO[t_min:t_max]    <- buf[1:nrow(buf),6]
TauxRevAGIRC[t_min:t_max]    <- buf[1:nrow(buf),7]
TauxRevFP[t_min:t_max]       <- buf[1:nrow(buf),8]
TauxRevInd[t_min:t_max]      <- buf[1:nrow(buf),9]
buf <- read.csv2("ParamPreret.csv",dec=".",sep=";",header=TRUE)
t_min <- buf[1,1]%%1900
t_max <- buf[nrow(buf),1]%%1900
TauxPR1[t_min:t_max]         <- buf[1:nrow(buf),2]
TauxPR2[t_min:t_max]         <- buf[1:nrow(buf),3]
TauxPR3[t_min:t_max]         <- buf[1:nrow(buf),4]
AgeEligPR[t_min:t_max]       <- buf[1:nrow(buf),5]
buf <- read.csv2("ParamFam.csv",dec=".",sep=";",header=TRUE)
t_min <- buf[1,1]%%1900
t_max <- buf[nrow(buf),1]%%1900
BMAF[t_min:t_max]            <- buf[1:nrow(buf),2]
PlafCF1[t_min:t_max]         <- buf[1:nrow(buf),3]
PlafCF2[t_min:t_max]         <- buf[1:nrow(buf),4]
PlafCF3[t_min:t_max]         <- buf[1:nrow(buf),5]
PlafCF4[t_min:t_max]         <- buf[1:nrow(buf),6]
PlafCF5[t_min:t_max]         <- buf[1:nrow(buf),7]
MajoPlafCF[t_min:t_max]      <- buf[1:nrow(buf),8]
PlafARS1[t_min:t_max]        <- buf[1:nrow(buf),9]
PlafARS2[t_min:t_max]        <- buf[1:nrow(buf),10]
PlafARS3[t_min:t_max]        <- buf[1:nrow(buf),11]
PlafARS4[t_min:t_max]        <- buf[1:nrow(buf),12]
PlafARS5[t_min:t_max]        <- buf[1:nrow(buf),13]
buf <- read.csv2("ParamAutres.csv",dec=".",sep=";",header=TRUE)
t_min <- buf[1,1]%%1900
t_max <- buf[nrow(buf),1]%%1900
TauxMalSP[t_min:t_max]       <- buf[1:nrow(buf),2]
TauxMalTot[t_min:t_max]      <- buf[1:nrow(buf),3]
TauxAGFF_1[t_min:t_max]      <- buf[1:nrow(buf),4]
TauxAGFF_2[t_min:t_max]      <- buf[1:nrow(buf),5]
TauxAssedic[t_min:t_max]     <- buf[1:nrow(buf),6]
TauxCSGSal[t_min:t_max]      <- buf[1:nrow(buf),7]
TauxCSGRet[t_min:t_max]      <- buf[1:nrow(buf),8]
SeuilExoCSG[t_min:t_max]     <- buf[1:nrow(buf),9]

# Parametres complexes geres par useleg
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
LegDRA          <- 0
DureeValCibDRA  <- numeric(5)
DureeCotCibDRA  <- numeric(5)
DebActCibDRA    <- numeric(5)
AgeDRA          <- numeric(5)

# Autres parametres
CoeffConv       <- numeric(120)
Options         <- 0
OptionsCN       <- 0
AnneeDepartCN   <- 999
CibleLiq        <- 0




Clone <- function(i,j)
{
  t_naiss[j]         <<- t_naiss[i]
  t_deces[j]         <<- t_deces[i]
  age[j]             <<- age[i]
  sexe[j]            <<- sexe[i]
  findet[j]          <<- findet[i]
  pere[j]            <<- pere[i]
  mere[j]            <<- mere[i]
  n_enf[j]           <<- n_enf[i]
  enf[j,]            <<- enf[i,]
  conjoint[j]        <<- conjoint[i]
  trim_naiss[j]      <<- trim_naiss[i]
  trim_act[j]        <<- trim_act[i]
  statut[j,1:t_fin]  <<- statut[i,1:t_fin]     
  salaire[j,1:t_fin] <<- salaire[i,1:t_fin]
}

Delete <- function(i)
{
  t_naiss[i]          <<- 0
  t_deces[i]          <<- 0
  age[i]             <<- 0
  sexe[i]            <<- 0
  findet[i]          <<- 0
  pere[i]            <<- 0
  mere[i]            <<- 0
  n_enf[i]           <<- 0
  enf[i,]            <<- rep(0,nb_enf_max)
  conjoint[i]        <<- 0
  trim_naiss[i]      <<- 0
  trim_act[i]        <<- 0
  statut[i,1:t_fin]  <<- rep(0,t_fin)    
  salaire[i,1:t_fin] <<- rep(0,t_fin)
}