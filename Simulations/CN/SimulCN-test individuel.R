cheminsource <- "/Users/didier/Desktop/PENSIPP 0.0/"
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R"      )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R"          )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsRetr.R"         )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsCN.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
setwd ( (paste0(cheminsource,"Simulations/CN"                                    )) )
cible <- numeric(taille_max)

i             <- 17
t             <- 115
ageliq[i]     <- 0
ageref[i]     <- t-anaiss[i]
AnneeDepartCN <- t
Leg           <- t
UseLeg(t,anaiss[i])

anaiss[i]
SimDir(i,t,"exo",ageref)
if (i==17 && t==115) {print (c("main 1",liq[i],pension_cn_pri[i]))}
if (t==115 && ((t-anaiss[i])==ageref[i] || liq[i]==t))
{
  print ("main 2",c(i,anaiss[i],liq[i],ageref[i],AgeMin(i,t),points_cn_pri,points_cn_fp,pension_cn_pri[i],pension_cn_fp[i]))
}