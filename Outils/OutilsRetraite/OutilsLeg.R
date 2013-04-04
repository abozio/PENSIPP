#####################################   OutilsLeg : Les paramètres du système de retraite (fonction UseLeg)  ############################


# La fonction UseLeg initialise les paramètres du système de retraite qui varient selon 
#  (i) la législation 
#  (ii) l'individu (par exemple son année de naissance) 
# Il s'agit des paramètres que DefVarRetr ne lit pas directement dans les paramètres retraite (qui sont généraux).
# Comme les paramètres de calcul de la retraite dépendront en général de l'individu, à année donnée, elle doit
# être appelée à l'intérieur de la boucle individuelle de calcul des pensions.
# 
# On rappelle que ces paramètres sont :
#   
# DureeCibRG      : Durée cible régime général
# DureeProratRG   : Durée de proratisation du régime général
# DureeCalcSAM    : Nombre d'années pour calcul du SAM dans le RG
# TauxPleinRG     : Taux plein du régime général
# DecoteRG        : Coefficient de décote du régime général (par année d'écart au taux plein)
# SurcoteRG1      : Coefficient de surcote du régime général, 1ere année
# SurcoteRG2      : Coefficient de surcote du régime général, 2eme année
# SurcoteRG3      : Coefficient de surcote du régime général, 3eme année
# TauxRGMax       : Taux maximal de la pension RG
# DureeCibFP      : Duree cible de la fonction publique
# DecoteFP        : Decote fonction publique
# SurcoteFP       : Surcote fonction publique
# AgeAnnDecFP     : Age d'annulation de la décote fonction publique.
# 
# La fonction UseLeg fixe les valeurs de ces différents paramètres en fonction de l'année et
# de la génération. Par exemple :
#   
#   UseLeg(t,anaiss[i]);
# 
# signifie que, jusqu'au prochain appel de UseLeg, on va utiliser les paramètres
# que prévoyait la législation de l'année t pour les individus nés en anaiss[i].
# 
# Cette fonction permet de simuler d'autres règles de calcul des droits que celles
# s'appliquant ou devant s'appliquer à l'individu i.
# 
# UseLeg(1992,anaiss[i]);
# UseLeg(1992,1942);
# 
# mettent en oeuvre le barème que prévoyait la législation de 1992 pour les
# individus nés en anaiss[i] ou pour la génération 1942. Si ceci ne suffit pas
# à décrire le scénario souhaité, il est toujours possible de modifier à la main
# un ou plusieurs paramètres après ces appels de UseLeg, comme on a vu pouvoir
# le faire pour les paramètres lus dans ParamSociaux.xls.
# 
# On notera que, dans ces appels, on utilise le millesime complet. En fait, pour
# pouvoir utiliser les mêmes compteurs de date que ceux qui indicent les séries
# temporelles le programme accepte aussi les dates ramenées à l'origine 1900, i.e.
# on aurait pu écrire :
# 
# UseLeg(103,anaiss[i]);
# 
# C'est ce qui était implicitement fait dans le premier exemple d'appel si t
# était une variable de boucle comprise entre 103 et 150.
# 
# 



source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )


############# Fonction UseLeg  ##########
UseLeg <- function(Leg,g)
{
  Leg <- 1900+(Leg %% 1900)
  g   <- 1900+(g   %% 1900)
  
  # INITIALISATION 
  
  # Parametres fixes dans les hypotheses de reference
  
  AgeMinRG    <<- 60
  AgeMinFPS   <<- 60
  AgeMinFPA   <<- 55
  AgeMaxRG    <<- 65
  AgeMaxFP    <<- 65
  AgeAnnDecRG <<- 65
  AgeMinMG    <<- NA
  AgeMinMG    <<- 60
  DureeMinFP  <<- 15
  DureeMinFPA <<- 15       
  AnOuvDroitFP<<- NA
  AgeSurcote  <<- 60
  DureeCalcSAM<<- 10
  LegDRA      <<- Leg 
  
  #On prevoit 4 "jeux" de param?tres d'acc?s ? la DRA + par defaut, 
  # la duree requise pour la retraite anticipee est fixee ? une valeur 
  # abusivement elevee
  DureeValCibDRA <<- c(99,99,99,99,99)        
  DureeCotCibDRA <<- c(99,99,99,99,99)      
  DebActCibDRA   <<- c(0,0,0,0,0)         
  AgeDRA         <<- c(99,99,99,99,99)      
  
  AgeMaxFPA   <<- 60           
  AgeMaxFPS   <<- 65           
  
  # Determination du statut et de l'annee d'ouverture des droits pour les fonctionnaires
  
  if (Leg < 2010)      {   AgeMinFP <<- 60 }
  else if (Leg == 2010)
  {
    AgeMinFP  <<- affn(g,c(1950,1956),c(60,62))
    if ((g==1951) && (trim_naiss[i]<=0.5))       {   AgeMinFP    <<- 60  }
  }
  else if (Leg >= 2011)
  {
    
    AgeMinFP  <<- affn(g,c(1950,1956),c(60,62))
    if ((g==1951) && (trim_naiss[i]<=0.5))      {   AgeMinFP    <<- 60  }
    AgeMinFP  <<- AgeMinFP + affn(g,c(1951,1955,1956),c(0,4/12,0))
  }
  
  #FP active
  if ((Leg<2010) || (t<111))
  {
    DureeMinFPA <<- 15
    DureeMinFP  <<- 15
  }
  else
  {
    DureeMinFPA <<- affn(Leg,c(2011,2012,2016),c(15,(15+8/12),17))       #RQ : on ignore la condition de 15+4/12 pour les nes au second semestre 2011
    DureeMinFP  <<- 2
  }
  
  durfpa <<-0
  durfp  <<-0
  nenf   <<-0
  a      <<-15
  
  while ( (a<=min((t-anaiss[i]-1),AgeMinFP)) && is.na(AnOuvDroitFP) )  #  while ((a<=Min((t-anaiss[i]-1),(int(Arr(1+moisnaiss[i]+12*AgeMinFP)/12)-1))) && (AnOuvDroitFP==NA))) )
  {
    
    if  (statut[i,(anaiss[i]+a)]%in% fonct_a)                             {durfpa  <<- durfpa +1}
    if  (statut[i,a] %in% c(fonct_a,fonct_s))                             {durfp   <<- durfp +1}
    
    #  print (c("durfpa", durfpa))
    if ((durfpa>=DureeMinFPA) && (a>=54) )                              {AnOuvDroitFP <<- anaiss[i]+a+1 }
    if (Leg<2010)
    {
      if ((enf[i,3]>0) && (anaiss[enf[i,3]]<=anaiss[i]+a) &&                     #MODIF SR : (enf[i][3]>0) par reconnu, changment avec NA).        
        (durfp>=DureeMinFP) && (sexe[i] == 2))                {AnOuvDroitFP <<- anaiss[i]+a+1 }
    }
    else
    {
      if ( (enf[i,3]>0) && (anaiss[enf[i,3]]<=anaiss[i]+a) &&
        (durfp>=15) && (sexe[i] == 2) && (anaiss[i]+a<2012))  {AnOuvDroitFP <<- anaiss[i]+a+1 }
    }
    a <- a+1
  }
  if ( !is.na(AnOuvDroitFP)  && (AnOuvDroitFP-anaiss[i]<AgeMinFP))       { AgeMinFP     <<- AnOuvDroitFP-anaiss[i]}
  else                                                                   { AnOuvDroitFP <<- AgeMinFP+anaiss[i]    }
  
  
  # Param??tres dependant de l'age et/ou de la generation
  
  # a - Secteur prive
  
  if (Leg<1946)
  {
    DureeCibRG     <<-99
    DureeProratRG  <<-1
    DureeCalcSAM   <<-0
    TauxPleinRG    <<-0
    DecoteRG       <<-0
    SurcoteRG1     <<-0.0
    SurcoteRG2     <<-0.0
    SurcoteRG3     <<-0.0
    TauxRGMax      <<-0.0
    MajTauxRGMax   <<-0.0
  }
  else if (Leg<1972)
  {
    DureeCibRG     <<- 99
    DureeProratRG  <<- 30
    DureeCalcSAM   <<- 10
    TauxPleinRG    <<- 0.4
    DecoteRG       <<- 0.040
    SurcoteRG1     <<- 0.040
    SurcoteRG2     <<- 0.040
    SurcoteRG3     <<- 0.040
    TauxRGMax      <<- 0.40
    MajTauxRGMax   <<- 0.04
  }
  else if (Leg<1983)
  {
    DureeCibRG     <<- 99
    DureeProratRG  <<- 37.5
    DureeCalcSAM   <<- 10
    TauxPleinRG    <<- 0.5
    DecoteRG       <<- 0.05
    SurcoteRG1     <<- 0.05
    SurcoteRG2     <<- 0.05
    SurcoteRG3     <<- 0.05
    TauxRGMax      <<- affn(Leg,c(1972,1975),c(0.40,0.50)) 
    MajTauxRGMax   <<- affn(Leg,c(1972,1975),c(0.04,0.05))
  }
  else if (Leg<1993)
  {
    DureeCibRG     <<- 37.5
    DureeProratRG  <<- 37.5
    DureeCalcSAM   <<- 10
    TauxPleinRG    <<- 0.5
    DecoteRG       <<- 0.05
    SurcoteRG1     <<- 0.0
    SurcoteRG2     <<- 0.0
    SurcoteRG3     <<- 0.0
    TauxRGMax      <<- 0.5
    MajTauxRGMax   <<- 0.0
  }
  else if (Leg<2003)
  {
    DureeCibRG     <<- affn(g,c(1933,1943),c(37.5,40))
    DureeProratRG  <<- 37.5
    DureeCalcSAM   <<- affn(g,c(1933,1948),c(10,25))
    TauxPleinRG    <<- 0.5
    DecoteRG       <<- 0.05
    SurcoteRG1     <<- 0.0
    SurcoteRG2     <<- 0.0
    SurcoteRG3     <<- 0.0
    TauxRGMax      <<- 0.5
    MajTauxRGMax   <<- 0.0
  }
  else if (Leg<2007)
  {
    DureeCibRG     <<- affn(g,c(1933,1943,1948,1949,1950,1951,1952,1953 ,1954 ,1955,1957,1958 ),
                            c(37.5,  40,  40,40.25,40.5,40.75,  41,41.25,41.25,41.5,41.5,41.75))
    DureeProratRG  <<- affn(g,c(1943,1948,1949,1950,1951,1952,1953 ,1954 ,1955,1957,1958 ),
                            c(37.5,  40,40.25,40.5,40.75,  41,41.25,41.25,41.5,41.5,41.75))
    DureeCalcSAM   <<- affn(g,c(1933,1948),c(10,25))
    TauxPleinRG    <<- 0.5
    DecoteRG       <<- affn(Leg,c(2004,2013),c( 0.05,0.0250))
    SurcoteRG1     <<- affn(Leg,c(2003,2004),c(0,0.150))
    SurcoteRG2     <<- affn(Leg,c(2003,2004),c(0,0.0150))
    SurcoteRG3     <<- affn(Leg,c(2003,2004),c(0,0.0150))
    TauxRGMax      <<- 0.5
    MajTauxRGMax   <<- 0.0
  }
  else if (Leg<2009)  #OK voir cahier pour d???tails la surc???te majore la pension au RG et non le taux !
  {
    DureeCibRG     <<- affn(g,c(1933,1943,1948,1949 ,1950,1951 ,1952,1953 ,1954 ,1955,1957,1958 ),
                            c(37.5,  40,  40,40.25,40.5,40.75,  41,41.25,41.25,41.5,41.5,41.75))   #MODIF#
    DureeProratRG  <<- affn(g,c(1943,1948,1949 ,1950,1951 ,1952,1953 ,1954 ,1955,1957,1958 ),
                            c(37.5,  40,40.25,40.5,40.75,  41,41.25,41.25,41.5,41.5,41.75))
    DureeCalcSAM   <<- affn(g,c(1933,1948),c(10,25))
    TauxPleinRG    <<- 0.5
    DecoteRG       <<- affn(Leg,c(2004,2013),c(0.05,0.0250))
    SurcoteRG1     <<- 0.015
    SurcoteRG2     <<- 0.02
    SurcoteRG3     <<- 0.025
    TauxRGMax      <<- 0.5
    MajTauxRGMax   <<- 0.0
  }
  else if (Leg<2010)  #ajout qui permet de prendre en compte LFSS 70 ans et non plus 65 ans
    # modification de la surcote apr??s 2009 toutes les annees supplementaires ?? 5%
  {
    AgeMaxRG       <<- 70  
    AgeAnnDecRG    <<- 65  
    DureeCibRG     <<- affn(g,c(1933,1943,1948,1949 ,1950,1951 ,1952,1953 ,1954 ,1955,1957,1958 ),
                            c(37.5,  40,  40,40.25,40.5,40.75,41  ,41.25,41.25,41.5,41.5,41.75))
    DureeProratRG     <<- affn(g,c(1943,1948,1949 ,1950,1951 ,1952,1953 ,1954 ,1955,1957,1958 ),
                               c(37.5,  40,40.25,40.5,40.75,  41,41.25,41.25,41.5,41.5,41.75))
    DureeCalcSAM   <<- affn(g,c(1933,1948),c(10,25))
    TauxPleinRG    <<- 0.5
    DecoteRG       <<- affn(g,c(1943,1953),c(0.05,0.0250))  #MODIF# SR
    SurcoteRG1     <<- 0.025   # soit 1.25% * 4 * 50% <<- 2.5% pas de diff???renciation entre les 4 premiers et les suivants
    SurcoteRG2     <<- 0.025
    SurcoteRG3     <<- 0.025
    TauxRGMax      <<- 0.5
    MajTauxRGMax   <<- 0.0  
  }
  
  
  else if (Leg>=2010) #Re?forme 2010 modification AgeMinRG et AgeMaxRG
  {
    AgeMaxRG       <<- 70  #modifs magali 10-11-2011
    DureeCibRG     <<- affn(g,c(1933,1943,1948,1949,1950,1951,1952,1953 ,1954 ,1955,1957,1958 ),
                            c(37.5,  40,  40,40.25,40.5,40.75,  41,41.25,41.25,41.5,41.5,41.75)) 
    DureeProratRG  <<- affn(g,c(     1943,1948,1949,1950,1951,1952,1953 ,1954 ,1955,1957,1958 ),
                            c(       37.5,  40,40.25,40.5,40.75,  41,41.25,41.25,41.5,41.5,41.75))
    DureeCalcSAM   <<- affn(g,c(1933,1948),c(10,25))
    TauxPleinRG    <<- 0.5
    DecoteRG       <<- affn(g,c(1943,1953),c(0.05,0.0250))  
    SurcoteRG1     <<- 0.025   # soit 1.25% * 4 * 50% <<- 2.5% pas de diff???renciation entre les 4 premiers et les suivants
    SurcoteRG2     <<- 0.025
    SurcoteRG3     <<- 0.025
    TauxRGMax      <<- 0.5
    MajTauxRGMax   <<- 0.0
    
    AgeMinRG      <<- affn(g,c(1950,1956),c(60,62))
    AgeAnnDecRG   <<- affn(g,c(1950,1956),c(65,67))
    
    if ((g==1951) && (trim_naiss[i]<=0.5))
    {
      AgeMinRG    <<- 60
      AgeAnnDecRG <<- 65
    }
    if (Leg>=2011)            #acceleration de la reforme, cf. PLFSS 2012 rectifie
    {
      AgeMinRG       <<- AgeMinRG    + affn(g,c(1951,1955,1956),c(0,4/12,0))
      AgeAnnDecRG    <<- AgeAnnDecRG + affn(g,c(1951,1955,1956),c(0,4/12,0))
    }
    AgeSurcote     <<- AgeMinRG
    
  }
  
  # b- Secteur public
  if  (Leg<2003)
  {
    DureeCibFP   <<- 37.5
    DecoteFP     <<- 0
    SurcoteFP    <<- 0
    AgeAnnDecFP  <<- AgeMinFP  
  }
  else if ((Leg<2010) || (t<111)) #modif Patrick 09/12/2011 : pour prendre en compte l'annee d'ouverture des droits, et non la generation
  {
    DureeCibFP  <<- affn(AnOuvDroitFP,c(103,108,112,113,114,115,117,118),c(37.5,40,41,41.25,41.25,41.5,41.5,41.75))  
    SurcoteFP   <<- affn(Leg,c(2003,2004,2008,2009),c(0,0.03,0.03,0.05))
    AgeAnnDecFP <<- AgeMinFP + affn(AnOuvDroitFP,c(105,106,108,120),c(0,1,2,5))
    if (AgeMinFP<55)   { AgeAnnDecFP <<- AgeMinFP}   #jamais de decote pour les m??res de trois enfants
    AgeMinMG    <<- AgeMinFP  
    if                  (statut[i,t]%in% fonct_a)       {      AgeMaxFP    <<- 60  }    # ((age[i]>0) && (statut[i,t]%in% fonct_a)) pourquoi age?
    else                                                {      AgeMaxFP    <<- 65  }
  }
  else   # Reforme de 2010
  {
    if ( ((AgeMinFP<55) && (g+60-5<2011))             # Les parents de 3 enfants ?? moins de 5 ans de la retraite gardent l'ancien mode de calcul
         || ((durfpa>=DureeMinFPA) && (g+55<2011))    # Les generations ayant atteint l'age d'ouverture des droits en 2010 gardent l'ancien mode de calcul
         || (g+60<2011) )
    {
      DureeCibFP  <<- affn(AnOuvDroitFP,c(2003,2008,2012,2013,2014,2015,2017,2018),c(37.5,40,41,41.25,41.25,41.5,41.5,41.75)) 
      DecoteFP    <<- affn(AnOuvDroitFP,c(105,115),c(0,0.05))
      SurcoteFP   <<- affn(Leg,c(2003,2004,2008,2009),c(0,0.03,0.03,0.05))
      AgeAnnDecFP <<- AgeMinFP + affn(AnOuvDroitFP,c(105,106,108,120),c(0,1,2,5))
      if (AgeMinFP<55)   { AgeAnnDecFP = AgeMinFP}   #jamais de decote pour les m??res de trois enfants
      AgeMinMG    <<- AgeMinFP    
      if                         (statut[i,t]%in% fonct_a) {      AgeMaxFP    <<- 60  } # ((age[i]>0) && (statut[i,t]%in% fonct_a)) pourquoi age?
      else                                                 {      AgeMaxFP    <<- 65  }
    }
    
    
    else if (durfpa>=DureeMinFPA) 
    {
      
      AgeMinFP  <<- affn(g,c(1955,1961),c(55,57))
      if ((g==1951) && (trim_naiss[i]<=0.5))       {   AgeMinFP    <<- 55  }
      if (Leg>=2011)          { AgeMinFP <<- AgeMinFP + affn(g,c(1956,1960,1961),c(0,4/12,0))  }  #acceleration de la reforme, cf. PLFSS 2012 rectifie
      AnOuvDroitFP <<-anaiss[i]+AgeMinFP                                                    #AnOuvDroitFP = int(anaiss[i]+moisnaiss[i]/12+ageouvdroitfp)
      AgeAnnDecFP <<- AgeMinFP + affn(AnOuvDroitFP,c(105,106,108,120),c(0,1,2,5))
      DecoteFP    <<- affn(AnOuvDroitFP,c(105,115),c(0,0.05))
      DureeCibFP     <<- affn(g,c(1933,1943,1948,1949,1950,1951,1952,1953 ,1954 ,1955,1957,1958 ),
                              c(37.5,  40,  40,40.25,40.5,40.75,  41,41.25,41.25,41.5,41.5,41.75))
      #DureeCibFP  <<- Affn(g,1938,37.5,1948,40,1953,40,1957,41,1958,41.25,1959,41.25,1960,41.5,1962,41.5,1963,41.75) #modifs marion 31-01-2011  Rythme different de FPS, normal? 
      AgeMinMG   <<- AgeMinFP + affn(AnOuvDroitFP,c(2010,2011,2015,2016,2020),c(0,0.5,3.5,4,5))
      if        (statut[i,t]%in% fonct_a)                  {     AgeMaxFP  <<-AgeMinFP+5   }  # ((age[i]>0) && (statut[i,t]%in% fonct_a)) pourquoi age?
      else                                                 {     AgeMaxFP  <<-AgeMinFP+10  }
    }
    
    else if (AgeMinFP<60)         #m??res de 3 enfants ayant toutes les conditions avant 2011, mais moins de 55 ans : conservent le droit de depart, mais avec les param??tres de leur generation
    {
      ageouvdroitfp  <<- affn(g,c(1950,1956),c(60,62))
      if ((g==1951) && (trim_naiss[i]<=0.5))        {   ageouvdroitfp    = 60  }
      if (Leg>=2011)                                { ageouvdroitfp <<- ageouvdroitfp + affn(g,c(1951,1955,1956,),c(0,4/12,0))  }  #acceleration de la reforme, cf. PLFSS 2012 rectifie
      AnOuvDroitFP   <<- anaiss[i]+AgeMinFP                                                    #AnOuvDroitFP = int(anaiss[i]+moisnaiss[i]/12+ageouvdroitfp)
      AgeAnnDecFP    <<- 60 + affn(AnOuvDroitFP,c(105,106,108,120),c(0,1,2,5))
      DecoteFP       <<- affn(AnOuvDroitFP,c(105,115),c(0,0.05))
      DureeCibFP     <<- affn(g,c(1933,1943,1948,1949,1950,1951,1952,1953 ,1954 ,1955,1957,1958 ),
                              c(37.5,  40,  40,40.25,40.5,40.75,  41,41.25,41.25,41.5,41.5,41.75))
      AgeMinMG     <<- 60 + affn(AnOuvDroitFP,c(2010,2011,2015,2016,2020),c(0,0.5,3.5,4,5))
      AgeMaxFP     <<- ageouvdroitfp+5
    }
    
    else                       # Cas general
    {
      AgeMinFP  <<- affn(g,c(1950,1956),c(60,62))
      if ((g==1951) && (trim_naiss<=0.5))      {   AgeMinFP    = 60  }
      if (Leg>=2011)          { AgeMinFP <<- AgeMinFP + affn(g,c(1951,1955,1956),c(0,4/12,0))  }  #acceleration de la reforme, cf. PLFSS 2012 rectifie
      AnOuvDroitFP   <<- anaiss[i]+AgeMinFP                                                  #AnOuvDroitFP = int(anaiss[i]+moisnaiss[i]/12+AgeMinFP)
      AgeAnnDecFP    <<-AgeMinFP + affn(AnOuvDroitFP,c(105,106,108,120),c(0,1,2,5))
      DecoteFP       <<- affn(AnOuvDroitFP,c(105,115),c(0,0.05))
      DureeCibFP     <<- affn(g,c(1933,1943,1948,1949,1950,1951,1952,1953 ,1954 ,1955,1957,1958 ),
                              c(37.5,  40,  40,40.25,40.5,40.75,  41,41.25,41.25,41.5,41.5,41.75))
      AgeMinMG     <<- AgeMinFP + affn(AnOuvDroitFP,c(2010,2011,2015,2016,2020),c(0,0.5,3.5,4,5))
      AgeMaxFP     <<- AgeMinFP+5
    }
    SurcoteFP   <<- 0.05
  }
  
  # 
  # # c- r?gles pour la retraite anticipee pour carri?re longue
  
  moisnaiss[i] <- 6    #### JE METS TEMPORAIREMENT CETTE VALEUR BIDON
  if ((Leg<2003) || (t<104))
  {
    
  }
  else if ((Leg<2009) || (t<109))
  {
    DureeValCibDRA <<- c(42,42,42,99,99)   
    DureeCotCibDRA <<- c(42,41,40,99,99)
    DebActCibDRA   <<- c(16,16,17,0,0)
    AgeDRA         <<- c(56,58,59,99,99)
    
  }
  else if ((Leg<2010) || (t<111))
  {
    DureeValCibDRA <<- c(42,42,42,99,99)   
    DureeCotCibDRA <<- c(DureeCibRG+2,DureeCibRG+1,DureeCibRG,99,99)
    DebActCibDRA   <<- c(16,16,17,0,0)
    AgeDRA         <<- c(56,58,59,99,99)
    
  }
  else
  {
    DureeValCibDRA <<- c(DureeCibRG+2,DureeCibRG+2,DureeCibRG+2,DureeCibRG+2,99)   
    DureeCotCibDRA <<- c(DureeCibRG+2,DureeCibRG+1,DureeCibRG,DureeCibRG,99)
    DebActCibDRA   <<- c(16,16,17,18,0)
    AgeDRA         <<- c(affn((g+56),c(2010,2016),c(56,58)),affn((g+58),c(2010,2016),c(58,60)),affn((g+59),c(2010,2016),c(59,61)),60,99)
    if (g+moisnaiss[i]/12+56<2011+7/12)        {   AgeDRA<<-c(56,58,59,60,99)  }
    
    
    if (Leg>2011)  #acceleration de la reforme
    {
      AgeDRA   <<-   AgeDRA  + c(affn((g+56),c(2011,2015,2016),c(0,(4/12),0)),affn((g+59),c(2011,2015,2016),c(0,(4/12),0)),affn((g+59),c(2011,2015,2016),c(0,(4/12),0)),0,0)
    }
    
    
    if ((Leg>=2012) && (t>=112)) #nouvelles modalites de depart au titre des carri??res longues
    {
      DureeValCibDRA  <<- c(DureeCibRG+2,DureeCibRG+2,DureeCibRG+2,DureeCibRG+2, DureeCibRG)
      DureeCotCibDRA  <<- c(DureeCibRG+2,DureeCibRG+1,DureeCibRG,DureeCibRG,DureeCibRG)
      DebActCibDRA    <<- c(16,16,17,18,20)
      if (112+10/12>anaiss[i]+moisnaiss[i]/12+60)  { AgeDRA <<- AgeDRA + c(0,0,0,0,(112+10/12-(anaiss[i]+moisnaiss[i]/12+60))-99) } 
      else                                         { AgeDRA <<- AgeDRA + c(0,0,0,0,(112+10/12-(anaiss[i]+moisnaiss[i]/12+60))-99)}     
    }
    
  }
  
  
}