# 
#                                   OutilsRetr : PROGRAMMES DE SIMULATION DES RETRAITES
# 

# 
# I. Programmes precisant les hypotheses de simulation des retraites
#     UseOpt     : choix des options generales de simulation

# 
# II. Fonctions servant a calculer les variables intermediaires 
#
#    DurBase     : calcul des durees de base (durees cotisations et AVPF)
#    DurMajo     : calcul des durees majorees
#    SalBase     : calcul des SAM et du salaire de reference FP
#    Points      : calcul des decomptes de points ARRCO, AGIRC et comptes
#                  notionnels
#    MinCont     : calcul du minimum contributif
#    MinGaranti  : calcul du minimum garanti
# 
# III. Fonctions testant les conditions d'acces a la retraite :
# 
#    AgeTrim     : retourne un age trimestrialise
#    AgeMin      : indique si l'individu a atteint l'age minimum d'ouverture des droits
#    AgeMax      : indique si l'individu a depasse l'age de mise ?? la retraite d'office
#    TauxPlein   : indique si l'individu a atteint les conditions du taux plein
# 
# IV. Des programmes d'initialisation ou de projection des pensions.
# 
#    Liq         : calcul droits complets ?? la liquidation
#    Revalo      : revalorisation de l'ensemble des droits (y.c. reversion)
#    SimDir      : initialisation ou projection des droits directs
#
# V. Calcul d'indicateurs derives
#
#    CotRet      : calcule les cotisations retraite sur salaires





source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R")) )




############# Fonction UseOpt
UseOpt <- function(liste=c())
{
  Options <<- c()
  for (i in 1:length(liste))
  {
    Options[i] <<- tolower(liste[i])
    if (!(is.element(Options[i],c("nobonif","nomda","noassimil","nomc","nomg","noptsgratuits","noavpf")))) 
    {
      print (paste("Attention : option '",Options[i],"' inconnue"))
    }
  }
}



###### Duree
# -> Fonction duree : calcule le nombre d'annees passees par un individu donne, dans un ensemble 
# d'etats entre deux dates
# Exemple d'appel
#  annuites[i] <- duree(i,110,150,codes_occ)
Duree <- function(i,t1,t2,valeurs)
{
   return (sum(is.element(statut[i,t1:t2],valeurs)))
}

############ DurBase
DurBase <- function (i,t)
{

  # Reversement prealable des annees FP au RG si dureeFP < minimum
  if (Duree(i,1,t,c(fonct_a,fonct_s))<DureeMinFP)
  {
    annees_fp <- which(is.element(statut[i,1:t],c(fonct_a,fonct_s)))
    if (findet[i]>22) {statut[i,annees_fp] <<- cadre}
    else              {statut[i,annees_fp] <<- non_cadre}
  }
  

  
  # Durees par defaut, sans troncations
  duree_cho   <<- Duree(i,1,t,c(chomeur,chomeurCN))
  duree_PR    <<- Duree(i,1,t,preret)
  duree_emp   <<- Duree(i,1,t,codes_occ)
  duree_empCN <<- Duree(i,1,t,codes_occCN)  
  duree_fpa   <<- Duree(i,1,t,fonct_a)
  duree_fps   <<- Duree(i,1,t,fonct_s)
  duree_in    <<- Duree(i,1,t,indep)
  duree_avpf  <<- Duree(i,1,t,avpf)
  
  if (is.element("noassimil",Options))
  { duree_rg  <<- Duree(i,1,t,c(non_cadre,cadre))}
  else 
  {
  duree_rg  <<- Duree(i,1,t,c(non_cadre,cadre,chomeur,chomeurCN,preret))
  }
  
  if ((t>48) && (t<72)) {duree_rg <<- max(duree_rg,0.5*duree_rg+15)}
  
#   # Reversement  des annees FP au RG si dureeFP < minimum
#   if (Duree(i,1,t-1,c(fonct_a,fonct_s))<DureeMinFP)
#   {
#     annees_fp <- which(is.element(statut[i,1:t],c(fonct_a,fonct_s)))
#     if (findet[i]>22) {statut[i,annees_fp] <<- cadre}
#     else              {statut[i,annees_fp] <<- non_cadre}
#   }
#  
#   if ( ($duree_fp>0) &&  ($duree_fp<$DureeMinFP)  ) # Modif Marion 09/12/2011
#   {
#     print (" basculement ")    ;
#     # affectation de la dur?e ? la FP au RG
#     $duree_rg    += $duree_fp;
#     $duree_emprg += $duree_fp;
#     $duree_fp     = 0;
#     $duree_fpa    = 0;
#     $duree_fps    = 0;
#     # modification des statuts  (? corriger quand le Modele int?grera v?ritablement l'Ircantec)
#     #for (15..$age[$i]-1)
#     #{
#     # if ((In($statut_[$i][$_],@CodesFP)) && ($findet[$i]>21)) {$statut_[$i][$_]=$CodeCad};
#     # if ((In($statut_[$i][$_],@CodesFP)) && ($findet[$i]<22)) {$statut_[$i][$_]=$CodeNC };
#     #}
#     
#     # ATTENTION : modif Patrick 14/12/2011 : on ne modifie plus les statuts en cours de carri?re (sinon Durbase ne peut plus ?tre appel? qu'une fois par boucle ...)
#   }
#   
  
  
#  Pour finir, on ventile le surcroit de duree d'activite entre les quatres types de durees de cotisation, 
#  au prorata de leurs parts non redressees
  duree_tot <<- duree_rg+duree_in+duree_fpa+duree_fps
if (duree_tot>0)
{
  duree_rg  <<- duree_rg*(1+trim_act[i]/duree_tot)
  duree_in  <<- duree_in*(1+trim_act[i]/duree_tot)
  duree_fpa <<- duree_fpa*(1+trim_act[i]/duree_tot)
  duree_fps <<- duree_fps*(1+trim_act[i]/duree_tot)
}
  duree_fp  <<- duree_fpa+duree_fps
  duree_tot <<- duree_rg+duree_in+duree_fp
  if ((LegDRA<2012) || (t< 112+10/12)) {   dureecotdra_tot<<- duree_emp+duree_empCN}
  else                              {   dureecotdra_tot<<- duree_emp+duree_empCN+min(0.5,duree_cho)  }
  
  # AJOUT POUR INCLUSION ANNEES PASSEES SOUS REGIME DES CN
  duree_tot <<- duree_tot+duree_empCN
}

############# Calcul des durees majorees selon avantages familiaux
DurMajo <- function(i,t)
{
  duree_rg_maj  <<- duree_rg
  duree_fp_maj  <<- duree_fp
  duree_in_maj  <<- duree_in
  duree_tot_maj <<- duree_tot

  # Ajout de l'AVPF
  if (!(is.element("noavpf",Options))) {duree_rg_maj <<- duree_rg_maj+duree_avpf}
  
  # Ajout de la MDA
  nenfFP1<-numeric(6)
  nenfFP2<-numeric(6)
  if ((sexe[i]==2) && (!(is.element("nomda",Options))))

  {
    # Calcul du nombre d'enfants avec r??gles specifiques pour le public
#print (c(i,enf[i,]))

    for (e in 1:n_enf[i])
    {
       if (n_enf[i]>0)
       {
       if (is.element(statut[i,anaiss[enf[i,e]]],c(fonct_a,fonct_s)) )
        {
        if      (anaiss[enf[i,e]]<=104)  {nenfFP1 <- nenfFP1+1}
        else if (anaiss[enf[i,e]]> 104)  {nenfFP2 <- nenfFP2+1}
        }
       }
    }
    # Application de la MDA, en priorite au regime general
    if      (duree_rg>0) {duree_rg_maj <<- duree_rg_maj+CoefMdaRG[t]*n_enf[i]}
    else if (duree_in>0) {duree_in_maj <<- duree_in_maj+CoefMdaRG[t]*n_enf[i]}
    else if (duree_fp>0)
    {
      if  (t<104)  {duree_fp_maj <<- duree_fp_maj + CoefMdaFP[t]*nenfFP1}
      else         {duree_fp_maj <<- duree_fp_maj +
                                     CoefMdaFP[t]*nenfFP1+0.5*CoefMdaFP[t]*nenfFP2}
    }
  }
  duree_tot_maj <<- duree_rg_maj+duree_fp_maj+duree_in_maj
  
#  # AJOUT POUR INCLUSION ANNEES PASSEES SOUS REGIME DES CN : Meme correction que dans DurBase
  duree_tot_maj <<- duree_tot_maj+duree_empCN
}

#### Fonction SalBase
# Calcule le SAM et le SalRef Fonction Publique pour l'individu i s'il part ? la date t
# SR 12/12/12: ajout prise en compte AVPF

SalBase <- function(i,t)
{
  spc      <- numeric(t_fin)
  revalcum <- 1
  u        <- t
  while (u > anaiss[i]+15)
  {    
     spc[u]      <- revalcum*min(PlafondSS[u],salaire[i,u])
     if  ( (statut[i,u]==avpf) && (!(is.element("noavpf",Options))) )
     {
     spc[u] <- SMIC[u]*revalcum
     }
     revalcum    <- revalcum*RevaloSPC[u]
 #    print (c(revalcum,PlafondSS[u],salaire[i,u],salreval[u],revalcum*min(PlafondSS[u],salaire[i,u])))
     u           <- u-1
       
  }
  annees_rg <- which(is.element(statut[i,1:t],c(non_cadre,cadre,avpf)))
  annees_in <- which(is.element(statut[i,1:t],c(indep)))
  annees_fp <- which(is.element(statut[i,1:t],c(fonct_a,fonct_s)))
#   print (annees_in)
#   print (length(annees_in))
#   print (length(annees_rg))
#   print (sam_rg)
  if (length(annees_rg)>0) {sam_rg <<- mean(sort(spc[annees_rg],decreasing=TRUE)[1:min(DureeCalcSAM,length(annees_rg))])}
  if (length(annees_in)>0) {sam_in <<- mean(sort(spc[annees_in],decreasing=TRUE)[1:min(DureeCalcSAM,length(annees_in))])}
  if (length(annees_fp)>0) {sr_fp  <<- salaire[i,max(annees_fp)]*PointFP[t]/PointFP[max(annees_fp)]}
 
  #MODIFSR#
}

# -> Fonction Points
# Calcule les points ARRCO et AGIRC acquis par l'individu i ?? la date t. Les variables
# globales mises ?? jour sont points_arrco et points_agirc
Points <- function(i,t)
{
   
 sal_1        <- rep(0,t)
 sal_2        <- rep(0,t)
 sal_A        <- rep(0,t)
 sal_B        <- rep(0,t)
 sal_C        <- rep(0,t)
 sal_1        <- part(salaire[i,1:t],               0,  PlafondSS[1:t],which(statut[i,1:t]==non_cadre))
 sal_2        <- part(salaire[i,1:t],  PlafondSS[1:t],3*PlafondSS[1:t],which(statut[i,1:t]==non_cadre))
 sal_A        <- part(salaire[i,1:t],               0,  PlafondSS[1:t],which(statut[i,1:t]==cadre))
 sal_B        <- part(salaire[i,1:t],  PlafondSS[1:t],4*PlafondSS[1:t],which(statut[i,1:t]==cadre))
 sal_C        <- part(salaire[i,1:t],4*PlafondSS[1:t],8*PlafondSS[1:t],which(statut[i,1:t]==cadre))

 
 # Points gratuits pour p?riodes de ch?mage.
 # NB: implique que les statuts de chomage indemnis?s et salchomref soient d?finis (dans Genebio ou en d?but de programme?)
 if (!(is.element("noptsgratuits",Options)))
 {
 #  print("PtsGratuits")
   sal_1        <- sal_1 + part(salrefchom[i,1:t],               0,  PlafondSS[1:t],which(chomind[i,1:t]==1))
   sal_2        <- sal_2 + part(salrefchom[1,1:t],  PlafondSS[1:t],3*PlafondSS[1:t],which(chomind[i,1:t]==1))
   sal_A        <- sal_A + part(salrefchom[i,1:t],               0,  PlafondSS[1:t],which(chomind[i,1:t]==1))
   sal_B        <- sal_B + part(salrefchom[i,1:t],  PlafondSS[1:t],4*PlafondSS[1:t],which(chomind[i,1:t]==1))
 } 
  
  points_arrco <<- sum(sal_1[47: t]*TauxARRCO_1[47:t] /SalRefARRCO[47:t]) +
                   sum(sal_2[47: t]*TauxARRCO_2[47:t] /SalRefARRCO[47:t]) +
                   sum(sal_A[47: t]*TauxARRCO_1[47:t] /SalRefARRCO[47:t])

 
 
        
#Garantie minimale de point AGIRC (minimum 120 points)
 Pts_B_GMP<-numeric(200)
 if (!(is.element("noptsgratuits",Options)))
 {
 Pts_B_GMP<-pmax((GMP[1:160]*(statut[i,1:160]==cadre)),(sal_B[1:160]*TauxAGIRC_B[1:160] /SalRefAGIRC[1:160]))
 points_agirc <<-  sum(Pts_B_GMP[47: t]) +
   sum(sal_C[47: t]*TauxAGIRC_C[47:t] /SalRefAGIRC[47:t])
 }
 else
{points_agirc <<- sum(sal_B[47: t]*TauxAGIRC_B[47:t] /SalRefAGIRC[47:t]) +sum(sal_C[47: t]*TauxAGIRC_C[47:t] /SalRefAGIRC[47:t])}
 

#Sans GMP : sum(sal_B[47: t]*TauxAGIRC_B[47:t] /SalRefAGIRC[47:t]) +sum(sal_C[47: t]*TauxAGIRC_C[47:t] /SalRefAGIRC[47:t])
  
#print (c(points_agirc, points_arrco))
 
}
 

############# Calcul du minimum contributif
MinCont <- function(i,t)
{
  majo      <-  Mincont2[t]-Mincont1[t]
  min_cont  <<- 0;
  
  if (TauxPlein(i,t))
  {
    if ((t>83) && (t<104))
    {
      min_cont <<- Mincont1[t]*min(1,(duree_rg_maj/DureeProratRG))
    }
    else if (t>=104)
    {
      # Cas d'un monopensionne RG ou d'un poly ??? carri??re incompl??te : r??gle simple
      if ((duree_rg_maj==duree_tot_maj) || (duree_tot_maj<DureeCibRG))
      {
        min_cont <<- Mincont1[t]*min(1,(duree_rg_maj/DureeProratRG))+
                    majo*min(1,(duree_rg/DureeProratRG))
      }
      else
      {
        # Cas d'un polypensionne ayant cotise moins que la duree requise
        if (duree_tot<DureeProratRG)
        {
          min_cont <<- Mincont1[t]*min(1,(duree_rg_maj/duree_tot_maj))+
                      majo*(duree_rg_maj/duree_tot_maj)*min(1,duree_tot/DureeProratRG)
        }
        else if (duree_tot>=DureeProratRG)
        {
          min_cont <<- Mincont1[t]*duree_rg_maj/duree_tot_maj+
                      majo*duree_rg_maj/duree_tot_maj
        }
      }
    }
  }
}

############ MinGaranti
MinGaranti <- function(i,t)
{
  d           <- duree_fp
  min_garanti <- 0
  if (d< DureeMinFP) {coef <- 0} #modif Marion 28/07/2011
  
  else if (t<111)
  {
    if (t<=103)      {coef <- 216*(60.0+4.00*part(d,15,25)                       )}
    else if (t==104) {coef <- 217*(59.7+3.80*part(d,15,25.5)+0.04*part(d,25.5,40))}
    else if (t==105) {coef <- 218*(59.4+3.60*part(d,15,26.0)+0.08*part(d,26.0,40))}
    else if (t==106) {coef <- 219*(59.1+3.40*part(d,15,26.5)+0.13*part(d,26.5,40))}
    else if (t==107) {coef <- 220*(58.8+3.20*part(d,15,27.0)+0.21*part(d,27.0,40))}
    else if (t==108) {coef <- 221*(58.5+3.10*part(d,15,27.5)+0.22*part(d,27.5,40))}
    else if (t==109) {coef <- 222*(58.2+3.00*part(d,15,28.0)+0.23*part(d,28.0,40))}
    else if (t==110) {coef <- 223*(57.9+2.85*part(d,15,28.5)+0.31*part(d,28.5,40))}
    else if (t==111) {coef <- 224*(57.6+2.75*part(d,15,29.0)+0.35*part(d,29.0,40))}
    else if (t==112) {coef <- 225*(57.5+2.65*part(d,15,29.5)+0.38*part(d,29.5,40))}
    else if (t>=113) {coef <- 227*(57.5+2.50*part(d,15,30.0)+0.50*part(d,30.0,40))};
  }
  
  #Reforme 2010 Modif 27/10/2010 Marion
  else if (t>=111)
  {
  
    if    ((age[i]<AgeMinMG) || (d<DureeCibFP))  {coef <- 0}
    else if (t==111) {coef <- 224*(57.6+2.75*part(d,15,29.0)+0.35*part(d,29.0,40))}
    else if (t==112) {coef <- 225*(57.5+2.65*part(d,15,29.5)+0.38*part(d,29.5,40))}
    else if (t>=113) {coef <- 227*(57.5+2.50*part(d,15,30.0)+0.50*part(d,30.0,40))};
  }
  
  min_garanti <<- coef*PointFP[t]/100;
}

############# AGETRIM
AgeTrim <- function(i,t)
{
  return (t-anaiss[i]+trim_naiss[i])
}

# ############ AGEMIN VERSION SANS DRA, A REPRENDRE
# AgeMin <- function(i,t)
# {  
#   if      ((Duree(i,1,t-1,fonct_a)>14) && ((t-anaiss[i])>=AgeMinFPA))   {return (TRUE)}
#   else if ((Duree(i,1,t-1,fonct_s)> 0) && ((t-anaiss[i])>=AgeMinFPS))   {return (TRUE)}
#   else if (                   ((t-anaiss[i])>=AgeMinRG )           )    {return (TRUE)}
#   else                                                                  {return (FALSE)}
# 
# #CN
#   if (t>AnneeDepartCN) {if (t-anaiss[i]>=60) {return (TRUE)} else {return (FALSE)}}  
# }

# ########### AGEMIN VERSION AVEC DRA

AgeMin <- function(i,t)  
{
#print(c("agemin", t))
agetest<-(t-anaiss[i])
# Modification des AgeMinFP et AgeMinRG si conditions DRA remplies
  
dar[i]<<-0  #par defaut
agedeb <- min(30, which(statut[i,] %in% c(codes_occ,codes_occCN))[1]-anaiss[i])

# calcul des durees necessaires aux conditions
DurBase(i,t)
DurMajo(i,t)
  
if ((LegDRA>=2003) && (t>=104)  && (agetest<AgeSurcote))  #SR suppression && (agetest<AgeSurcote) PBL?
{
for (j in 1:5)
{  

if (          (agedeb<=DebActCibDRA[j])
               && (dureecotdra_tot>=DureeCotCibDRA[j])
               && (duree_tot_maj>=DureeValCibDRA[j])
               && (AgeDRA[j]<=agetest)
               && ( agetest < max(AgeMinRG,AgeMinFP))
   )
             #  && (Arr(12*agetest) < Arr(12*Max(AgeMinRG,AgeMinFP))))
      {
    #    print (j)
        dar[i]   <<- 1
        AgeMinRG <<- min(AgeMinRG,agetest)
        AgeMinFP <<- min(AgeMinFP,agetest)
      }

    }  # fin de la boucle sur j ("scenario" de depart anticipe)
  }  # fin de la boucle sur Leg
  
#Pour les individus qui partent en DRA, on change les conditions de taux plein. 
if (dar[i]==1) 
{
AgeAnnDecFP<<-AgeMinFP
AgeAnnDecRG<<-AgeMinRG
}
  
  if      (agetest>= min(AgeMinRG,AgeMinFP))                            {return (TRUE)}
  else                                                                  {return (FALSE)}

}



############ AGEMAX version booleen (retourne TRUE quand l'age Max est atteint), adaptation de PERL
# Version sans difference polypensionne. Voir version D2. 
AgeMax <- function(i,t)
{  
  if  ((statut[i,t]%in% fonct_a) && (duree_fpa>=15) && (t-anaiss[i]>=AgeMaxFP))  { return (TRUE)}     #A REPRENDRE 15 : duree min FPA
  else if (  (statut[i,t]%in% fonct_a)              && (t-anaiss[i]>=AgeMaxFP)    )       { return (TRUE)}
  else if (  (statut[i,t]%in% c(fonct_a,fonct_s))   && (t-anaiss[i]>=AgeMaxFP)    )       { return (TRUE)}
  else if (  (statut[i,t]%in% c(cadre,non_cadre))   && (t-anaiss[i]>=AgeMaxRG)    )       { return (TRUE)}
  else if (  (t-anaiss[i]>=70)    )                                                       { return (TRUE)}
  else                                                                                    { return (FALSE)}
  
  #CN
  if (t>AnneeDepartCN) {if (t-anaiss[i]>=70) {return (TRUE)} else {return (FALSE)}}
}


############ TAUX PLEIN
TauxPlein <- function (i,t)
{
  a <- AgeTrim(i,t)
  
  # (Re-)calcul de l'ensemble des durees
#  DurBase(i,t)
#  DurMajo(i,t)
  
  # Test de la condition du taux plein
  # NB : Pour simplifier, les individus de la fonction publique active n'ont pas
  # le droit de liquider avant 60 ans dans le cas o??? ils auraient aussi valide?
  # au Rg ou comme inde?pendant. Si on ne met pas cette condition leur pension
  # au Rg est nulle. Ils sont peu nombreux donc c'est plus simple comme ???a.

  if  (
       ((a>=AgeAnnDecRG) && (duree_rg_maj  >0))           ||
       ((a>=AgeAnnDecFP) && (duree_fp_maj  >0))           || 
       ((a>=AgeMinRG   ) && (duree_tot_maj >=DureeCibRG)) ||
       ((a>=AgeMinFPS  ) && (duree_fp_maj  >=DureeCibFP)) ||
       ((a>=AgeMinFPA  ) && (duree_fp_maj  >=DureeCibFP)  &&
        (duree_fpa>DureeMinFP-1)                          &&
        (duree_rg==0)                                     &&
        (duree_in==0))
    )
  {
    return (TRUE)
  }
  else 
  {
    return (FALSE)
  }
}



############ Fonction Liq
Liq <- function(i,t)
{
#print ("liq")
  a <- AgeTrim(i,t)
  DurBase(i,t)   
  DurMajo(i,t)
  SalBase(i,t)
  Points(i,t)  
  PointsCN(i,t)
  MinCont(i,t)  
  MinGaranti(i,t)

  # Revision : on ne compte une distance au taux plein s>0 que pour des
  # personnes encore en emploi au moment de liquider (a ameliorer eventuellement
  # pour le cas des individus qui auraient une transition d'emploi entre
  # le taux plein et l'??'ge courant, a priori peu probable car, dans ce cas, doivent
  # partir d??s la perte d'emploi)
  distance  <- 0  # RAJOUTE POUR INITIALISER MAIS LES EFFETS SONT A CONTROLER
  
  if (t<83) {distance  <- a-AgeAnnDecRG}
  else
  {
    if ((a<AgeAnnDecRG) && (duree_tot_maj<DureeCibRG))
    {
      distance <- max(a-AgeAnnDecRG,duree_tot_maj-DureeCibRG)
    }
    else if (is.element(statut[i,t],codes_occ))
    {
      distance <- max(0,min(a-AgeMinRG   ,duree_tot_maj-DureeCibRG))
    }
  }


  
  if ((t>103)&&(distance>0)) {distance <- min(t-104,distance)}
  ######### Sous module regime general ########################################
  # Calculs du coeff de proratisation et du taux
  prorat <- min(1,duree_rg_maj/DureeProratRG)
#   print (c("prorat",prorat))
#   print(duree_rg_maj)
#   print(DureeProratRG)

  if (a<AgeMinRG) {taux <- 0}
  else
  {
    taux <- TauxPleinRG + 
      DecoteRG*min(0,distance) + 
      SurcoteRG1*(max(distance,0)) + 
      (SurcoteRG2-SurcoteRG1)*max(distance-1,0) + 
      (SurcoteRG3-SurcoteRG2)*max(distance-2,0) 
  }
  # Calcul de la pension avec condition de plafonnement
  pension_rg[i] <<- min(
    (TauxRGMax+MajTauxRGMax*(max(0,distance)))*PlafondSS[t],
    taux*prorat*sam_rg
    )
  

# print ("tauxdeliq")
# print (distance)
# print (DecoteRG)
# print (taux)
# print (sam_rg)
# print (prorat)

  # Prise en compte optionnelle du minimum contributif
  if (!(is.element("NoMC",Options)) && (distance>=0) && (pension_rg[i]>0))
  {
    if (TauxPlein(i,t))
    {
      pension_rg[i] <<-  max(pension_rg[i],min_cont)
      if (pension_rg[i]==min_cont)
      {
        indic_mc[i] <<- 1
      }
    }
  }  
  # Prise en compte optionnelle de la Bonif pour 3 enfants et plus (10% )
  if (!(is.element("NoBonif",Options)) && (n_enf[i]>=3)) {pension_rg[i] <<- pension_rg[i]*1.1}
  
  
  ######### Sous module ARRCO/AGIRC ###########################################
  # Calcul du coefficient d'abattement  #MODIF SR# affn
  if      (t<55) {taux <- affn(age[i],c(59,60,65,70),c(0,0.75,1,1.25)   )}
  else if (t<64) {taux <- affn(age[i],c(59,60,62,65),c(0,0.78,0.88,1.0) )}
  else if (t<83) {taux <- affn(age[i],c(49,60,62,65),c(0,0.78,0.88,1.0) )}
  else
  {
    if      (a<55)       {taux <- 0.0}
    else if (a<AgeMinRG) {taux <- 0.78+0.07*(a-AgeMinRG)}
    else if (a<AgeMaxRG) {taux <- 1.0 +0.04*min(distance,0)+0.01*min(distance+3,0)}
    else                 {taux <- 1.00}
  }
  
  # Calcul des deux pensions
  pension_ar[i] <<- taux*points_arrco*ValPtARRCO[t]
  pension_ag[i] <<- taux*points_agirc*ValPtAGIRC[t]
  
  # Application optionnelle de la Bonif pour 3 enfants et plus
  # Avant 2012: pension ARRCO , 5% ; pension AGIRC , 8% + 4% par enfant de rang sup ? 4
  # Apr?s 2012: pension ARRCO , 10% ; pension AGIRC , 10% (alignement RG)


  if (!(is.element("NoBonif",Options)) &&  (n_enf[i]>=3))
  {
     if (Leg <= 2011)
     {  

    pension_ar[i] <<- pension_ar[i]*1.05
    pension_ag[i] <<- pension_ag[i]*1.08+0.04*(n_enf[i]-3)
      }
     else 
     {  

       pension_ar[i] <<- pension_ar[i]*1.10
       pension_ag[i] <<- pension_ag[i]*1.10
     }    
  }


  ######## Sous module pension civile #########################################
  # Calcul du coefficient de proratisation
  prorat <- min(1,duree_fp_maj/DureeCibFP)
  
  # Calcul du taux
  taux <- 0
  if ((a >= AgeMinFPS) ||
     ((a >= AgeMinFPA) && (duree_fpa>0) && (duree_rg==0) && (duree_in==0)))
    
    # Pour simplifier, les individus de la fonction publiqua active n'ont pas
    # le droit de liquider avant 60 ans dans le cas o???? ils auraient aussi valide
    # au Rg ou comme independant.
    # Si on ne met pas cette condition leur pension au Rg est nulle
    # Ils sont peu nombreux donc c'est plus simple comme ???a.
    
  {
    taux <-  0.75*
      (1.0-DecoteFP *max(0,min(AgeAnnDecFP-(t-anaiss[i]),DureeCibFP-duree_tot_maj))+
       SurcoteFP*max(0,min(a-AgeMinFPS,duree_tot_maj-DureeCibFP)))
  }
  
  # Calcul de la pension 
  # Modif SR: pas de limitation ? 0.8
  pension_fp[i] <<- taux*prorat*sr_fp
#   print ("pensionfp")
#   print (  pension_fp[i])
#   print (taux)
#   print (prorat)
#   print(sr_fp)
#   print(DureeCibFP)
#   
  # Application du minimum garanti
  # pour l'instant la valeur du point d'indice est bidon
  # le resultat est donc faux, mais le calcul fonctionne
  if (!(is.element("NoMG",Options)) && (pension_fp[i]==0))
  {
    pension_fp[i]    <<- max(pension_fp[i],min_garanti)
    if (pension_fp[i] <= min_garanti) {indic_mg[i] <<- 1}
  }
  
  # Application optionnelle de la Bonif pour 3 enfants et plus
  # 10%  + 5% par enfant de rang sup ??? 4 (
  # Limitation de la pension si la retraite apr??s bonif depasse le
  # salaire de reference
  if (!(is.element("NoBonif",Options)) && (n_enf[i]>=3))
  {
    pension_fp[i]  <<-  min(pension_fp[i]*(1.1+0.05*(n_enf[i]-3)),sr_fp)
  }
  
  ########### Sous module pension d'independant ###############################
  # Calcul reproduisant les r??gles regime general,??? valider
  prorat <- min(1,duree_in_maj/DureeProratRG)
  if (a<AgeMinRG) {taux <- 0}
  else
  {
    taux <- TauxPleinRG+DecoteRG*min(0,distance) +
     SurcoteRG1            *max(distance,0)     +
    (SurcoteRG2-SurcoteRG1)*max(distance-1,0)   +
    (SurcoteRG3-SurcoteRG2)*max(distance-2,0)   
  }
  
  # Calcul de la pension avec condition de plafonnement
  pension_in[i] <<- min(
    (TauxRGMax+MajTauxRGMax*(max(0,distance)))*PlafondSS[t],
    taux*prorat*sam_in)
  
  # Prise en compte optionnelle de la Bonif pour 3 enfants et plus  (10% )
  if (!(is.element("NoBonif",Options)) && (n_enf[i]>=3)) {pension_in[i] <<- pension_in[i]*1.1}
  
  ########### Sous module compte notionel ###############################
   if (t>=AnneeDepartCN)
   {  
  
  pension_cn_pri[i] <<-CoeffConv[t-anaiss[i]]*points_cn_pri
#  print (c(pension_cn_pri[i],CoeffConv[t-anaiss[i]],points_cn_pri))
  pension_cn_fp[i]  <<-CoeffConv[t-anaiss[i]]*points_cn_fp
  pension_cn_ind[i] <<-CoeffConv[t-anaiss[i]]*points_cn_ind
   }
  
  ######### Mises a jour finales ##############################################

  pension[i]             <<- pension_rg[i]+pension_ar[i]+pension_ag[i]+
                             pension_fp[i]+pension_in[i]+
                             pension_cn_pri[i]+pension_cn_fp[i]+pension_cn_ind[i]
  pliq[i]                <<- pension[i]
  ageliq[i]              <<- t-anaiss[i]
  liq[i]                 <<- t
  
  
  
  for (u in (t:t_fin))
  {
    if (statut[i,u]>0) {statut[i,u]   <<-  inactif}
    salaire[i,u] <<- 0
  }
}


##### Fonction Revalo
# Revalorise les composantes de la pension de l'individu i entre les deux dates indiquees. 
Revalo <- function(i,t1,t2)
{

  # Revalos regimes de base 
  for (u in (t1+1):t2)     
  {
    pension_rg[i] <<- pension_rg[i]*RevaloRG[u]
    pension_fp[i] <<- pension_fp[i]*RevaloFP[u]
    pension_in[i] <<- pension_in[i]*RevaloRG[u]
    rev_rg[i]     <<- rev_rg[i]*RevaloRG[u]
    rev_fp[i]     <<- rev_fp[i]*RevaloFP[u]
    rev_in[i]     <<- rev_in[i]*RevaloRG[u]

   # Revalos Comptes Notionels
    pension_cn_pri[i] <<- pension_cn_pri[i]*RevaloCN[u]
    pension_cn_fp[i]  <<- pension_cn_fp[i]*RevaloCN[u]
    pension_cn_ind[i] <<- pension_cn_ind[i]*RevaloCN[u]
  }
  
  # Revalos des regimes complementaires
  pension_ar[i] <<- pension_ar[i]*ValPtARRCO[t2]/ValPtARRCO[t1]
  rev_ar[i]     <<- rev_ar[i]    *ValPtARRCO[t2]/ValPtARRCO[t1]
  pension_ag[i] <<- pension_ag[i]*ValPtAGIRC[t2]/ValPtAGIRC[t1]
  rev_ag[i]     <<- rev_ag[i]    *ValPtAGIRC[t2]/ValPtAGIRC[t1]


  
  # Mises a jour globales
  pension[i]  <<- pension_rg[i]+pension_ar[i]+pension_ag[i]+
                  pension_fp[i]+pension_in[i]+
                  pension_cn_pri[i]+pension_cn_fp[i]+  pension_cn_ind[i]
  rev[i]      <<- rev_rg[i]+rev_ar[i]+rev_ag[i]+
                  rev_fp[i]+rev_in[i]
}




# Fonction SimDir : 
# Initialise ou actualise les droits directs. Ce programme est appele a
# l'interieur d'une boucle sur donnees individuelles. 

SimDir <- function(i,t,comportement="TP",cible=c())
{

#  liq[i] <<- 0   #Remise ? 0 de l'indicatrice de liquidation (generee par liq)
# DIVERSES CORRECTIONS Didier 20/3/2013 :
# - AJOUT APPEL SUPPLEMENTAIRE DE DURBASE POUR QUE duree_tot SOIT BIEN RENSEIGNEE AVANT TEST (VOIR
#   S'IL FAUDRA VRAIMENT GARDER CETTE CONDITION)
# - Ajoute d'une CONDITION PAR UNE CONDITION SUR L'AGE SIMPLE SI ON EST EN COMPTES NOTIONNELS :mise 
#   à 55 ans pour ratisser large
  
  DurBase(i,t)
  if (tolower(comportement) == "exo")   # AJOUT CONTROLES SI CIBLE MAL DEFINIE
  {
    if (!(is.na(cible[i])) && cible[i]>0 && (t-anaiss[i])==cible[i])
    {
      AgeMin(i,t)    # On appelle quand même AgeMin pour activer modifs paramètres si DRA
      Liq(i,t)
    }
  }
  else if (AgeMin(i,t) && (statut[i,t]>0))   
  {
    # Liquidation possible si l'individu n'a pas encore liquidé et a validé au moins une période.
    # RAJOUT D'UNE CONDITION DE DUREE CN>0 POUR EVITER LIQUIDATIONS BIDONS D'INDIVIDUS SANS AUCUN DROITS
    if (ageliq[i]==0 &&   (duree_tot>0 || (t>=AnneeDepartCN && duree_tot >0 && t-anaiss[i]>=55)))   ############   
    {  
    if (AgeMax(i,t))
    {
      Liq(i,t)
    }
    else if (tolower(comportement) == "tp")
    {
      if (TauxPlein(i,t))
      {
        Liq(i,t)
      }
    }
#     else if (tolower(comportement) == "exo")   # AJOUT CONTROLES SI CIBLE MAL DEFINIE
#     {
#       if (!(is.na(cible[i])) && cible[i]>0 && (t-anaiss[i])==cible[i])
#       {
#         Liq(i,t)
#       }
#     }
    else if (tolower(comportement) == "pmin")  # IDEM
    {
      Clone(i,taille_max)
      Liq(taille_max,t)
      if (!(is.na(cible[i])) && cible[i]>0 && pension[taille_max]>cible[i])
      {
        Liq(i,t)
        delete(taille_max)
      } 
    }
    else if (tolower(comportement)=="sw")
    {
      # Cette option fait partir en retraite dès que le niveau de vie en retraite est superieur
      # au niveau de vie sans liquidation (ex: chomeur fin de droit) ou d??s qu'un taux de remplacement
      # minimum cible est atteint, sauf si le report apporte un gain de prestation superieur??? une valeur seuil beta.
      # Prendre une valeur de beta tres elevee revient un modele a simple cible de taux de remplacement
#      print (c(i,cibletaux[i]))
      nvprev   <- salaire[i,t-1]*(1-0.2)    #Salaire "net", avec un taux de cotisation moyen ? 20%. 
      nvnondep <- salaire[i,t]
      #$kinst = Max(Min(1,$k[0]),($k[0]*((1+$delta_k[0])**(Max(0,$agetest-60)))));
      kinst <- k[i]*((1+delta_k[i])^(max(0,(t-anaiss[i])-60)))
      cibletaux[i]<<-1/kinst 
#      print (c(i,cibletaux[i]))
      Clone(i,taille_max)
      Liq(taille_max,t)
      nvdep    <- pension[taille_max]
      Clone(i,taille_max)
      Liq(taille_max,t+1)
      nvdep1   <- pension[taille_max]
#      print(c("sw",t,nvdep,nvprev,cibletaux[i]))
      if ((nvdep>nvnondep) || ((nvdep>cibletaux[i]*nvprev) && (nvdep1<(1+beta[i])*nvdep)))
      {
        Liq(i,t)
      #  print (c(i, beta[i],cibletaux[i]))
      }
      Delete(taille_max)
    }
    }
#     else
#     { 
#        #print ("revalo")
#        Revalo(i,t,t+1) 
#     }
  }
}

##################################### V. Calcul d'indicateurs derives

########Fonction CotRet
# Calcule la somme des cotisations retraite a une date donnee (il s'agit des cotisations salaries)
# Exemples d'appel :
# cotret[i,t]<- CotRet(i,t)

CotRet <- function(i,t)                                   
{
  
  sal_1        <- rep(0,t)
  sal_2        <- rep(0,t)
  sal_A        <- rep(0,t)
  sal_B        <- rep(0,t)
  sal_C        <- rep(0,t)
  sal_1        <- part(salaire[i,1:t],               0,  PlafondSS[1:t],which(statut[i,1:t]==non_cadre))
  sal_2        <- part(salaire[1,1:t],  PlafondSS[1:t],3*PlafondSS[1:t],which(statut[1,1:t]==non_cadre))
  sal_A        <- part(salaire[i,1:t],               0,  PlafondSS[1:t],which(statut[i,1:t]==cadre))
  sal_B        <- part(salaire[i,1:t],  PlafondSS[1:t],4*PlafondSS[1:t],which(statut[i,1:t]==cadre))
  sal_C        <- part(salaire[i,1:t],4*PlafondSS[1:t],8*PlafondSS[1:t],which(statut[i,1:t]==cadre))
  
  

cot_non_cadre <-   sum(TauxSalRGSalTot[1:t]*salaire[i,statut[i,t]%in% non_cadre])      +
                   sum(TauxSalRGSP[1:t]*sal_1[1:t])                                    +
                   sum(TauxARRCO_S1[1:t]*TauxAppARRCO[1:t]*sal_1[1:t])                 +
                   sum(TauxARRCO_S2[1:t]*TauxAppARRCO[1:t]*sal_2[1:t]) 

cot_cadre     <-   sum(TauxSalRGSalTot[1:t]*salaire[i,statut[i,t]%in% cadre])         +
                   sum(TauxSalRGSP[1:t]*sal_A[1:t])                                   +
                   sum(TauxARRCO_S1[1:t]*TauxAppARRCO[1:t]*sal_B[1:t])                +
                   sum(TauxARRCO_S2[1:t]*TauxAppARRCO[1:t]*sal_C[1:t]) 



cot_fp <- sum(TauxFP[1:t]*salaire[i,statut[i,t]%in% c(fonct_a,fonct_s)]/(1+taux_prim[i]))

cot_ind <- sum(TauxSalRGSalTot[1:t]*salaire[i,statut[i,t]%in% indep])  +
           sum(TauxSalRGSP[t]*Part(salaire[i,t],0,PlafondSS[t])) 
  
cot <- cot_fp + cot_ind + cot_cadre + cot_non_cadre
               
               
            
 
return (cot)
              
}



# ### Partie test sur cas types
# Test <- function(tliq,generation,debut_carr,saldeb,salfin,secteur) 
# {
#   anaiss[1] <<- generation
#   sexe[1]   <<- 1
#   n_enf[1]  <<- 2
#   for (e in 1:n_enf[1])
#   {
#     enf[1,e]     <<- 10+e
#     anaiss[10+e] <<- 70+2*e
#   }
#   findet[1] <<- debut_carr-anaiss[1]-1
#   statut[1,1:anaiss[1]-1]              <<- pas_ne
#   statut[1,anaiss[1]:(debut_carr-1)]   <<- inactif
#   statut[1,debut_carr:(anaiss[1]+80)]  <<- secteur
#   for (t in debut_carr:(anaiss[1]+80))
#   {
#     salaire[1,t] <<- PlafondSS[t]*affn(t,c(debut_carr,generation+60),c(saldeb,salfin))
#   }
#   print (statut[1,])
#   print (salaire[1,])
#   UseLeg(tliq,anaiss[1])
#   Liq(1,tliq)
#   print (c(tliq-anaiss[1],duree_tot,duree_tot_maj,sam_rg,sam_in,sr_fp,points_arrco,points_agirc))
#   print (c(pension[1],pension_rg[1],pension_ar[1],pension_ag[1],pension_fp[1],pension_in[1]))
# }
# 
# Test(110,50,70,0.75,1.5,cadre)
# 
