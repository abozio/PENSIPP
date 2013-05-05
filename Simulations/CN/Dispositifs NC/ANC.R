############################### EFFETS DES DISPOSITIFS NON CONTRIBUTIFS ###################

# Masse des pensions pour chaque dispositif. 
# Neutralisation tour à tour de chaque dispositif. 
# Questions: maintien d'un âge constant? 

t0  <- Sys.time()

#### Chargement des programmes source ####

# Déclaration du chemin pour les fichiers sources
cheminsource <- "/Users/simonrabate/Desktop/PENSIPP 0.1/"
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R"           )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R"      )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R"          )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsRetr.R"         )) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsCN.R"           )) )



# Declaration des variable d'outputs

ageref      <- numeric(taille_max)
pliq_       <- matrix(nrow=taille_max,ncol=7)
pliq_rg       <- matrix(nrow=taille_max,ncol=7)
pliq_fp       <- matrix(nrow=taille_max,ncol=7)
gain        <- numeric(taille_max)
actifs      <- numeric(taille_max)        # Filtre population active
retraites   <- numeric(taille_max)        # Filtre population retraitée
actifsa      <- numeric(taille_max)        # Filtre population active
retraitesa   <- numeric(taille_max)  
liquidants  <- numeric(taille_max)
liquidants_fp <- numeric(taille_max)
liquidants_rg <- numeric(taille_max)
liquidants_in <- numeric(taille_max)
liquidants_po <- numeric(taille_max)
mc <-      numeric(taille_max) 
mincont <-      numeric(taille_max) 
minga <-      numeric(taille_max) 
mg <-      numeric(taille_max) 


MSAL        <- matrix(nrow=7,ncol=200)    # Masse salariale par année
MPENS       <- matrix(nrow=7,ncol=200)    # Masse des pensions année
MPENSa       <- matrix(nrow=7,ncol=200)    # Masse des pensions année
RATIOFIN    <- matrix(nrow=7,ncol=200)    # Ratio masse des pensions/masse des salaires par année
SALMOY      <- matrix(nrow=7,ncol=200)    # Salaire moyen par année
PENMOY      <- matrix(nrow=7,ncol=200)    # Pension moyenne par année
PENREL      <- matrix(nrow=7,ncol=200)    # Ratio pension/salaire
PENLIQMOY   <- matrix(nrow=7,ncol=200)    # Pension moyenne à liquidation
PENLIQMOY  <- matrix(nrow=7,ncol=200)    # Pension moyenne à liquidation
MPENLIQ     <- matrix(nrow=7,ncol=200)    # Masse des pension à liquidation
MPENLIQa     <- matrix(nrow=7,ncol=200)    # Masse des pension à liquidation
W           <- 2047.501
cibletaux<-numeric(taille_max)
cot<-numeric(taille_max)

# Modification valeur mico. 


#### Début de la simulation ####

#  Rprof(tmp<-tempfile())
for (sc in c(1,2,3,4,5,6,7))
  #  1: Normal Ref  
  #  2-4: Neutralisation Avantages familiaux (MDA, bonif, AVPF)  
  #  5-6: Neutralisation Periodes assimilés & Pts gratuits  
  #  7: Neutralisation Minima de pensions
{
  # Reinitialisation variables
  source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
  load  ( (paste0(cheminsource,"Modele/Outils/OutilsBio/BiosDestinie2.RData"        )) )  
#  setwd ( (paste0(cheminsource,"Simulations/CN"                                    )) )
  

# if (sc==2) {UseOpt(c("nobonif"))}
# if (sc==3) {UseOpt(c("nobonif","nomda"))}
# if (sc==4) {UseOpt(c("nobonif","nomda","noavpf"))}
# if (sc==5) {UseOpt(c("nobonif","nomda","noavpf","noassimil"))}
# if (sc==6) {UseOpt(c("nobonif","nomda","noavpf","noassimil","noptsgratuits"))}
# if (sc==7) {UseOpt(c("nobonif","nomda","noavpf","noassimil","noptsgratuits","nomc","nomg"))}

  if (sc==2) {UseOpt(c("nomc","nomg"))}
  if (sc==3) {UseOpt(c("nomc","nomg","nobonif"))}
  if (sc==4) {UseOpt(c("nomc","nomg","nobonif","nomda"))}
  if (sc==5) {UseOpt(c("nomc","nomg","nobonif","nomda","noavpf"))}
  if (sc==6) {UseOpt(c("nomc","nomg","nobonif","nomda","noavpf","noassimil"))}
  if (sc==7) {UseOpt(c("nomc","nomg","nobonif","nomda","noavpf","noassimil","noptsgratuits"))}
  
  
  for (t in 80:160)   # Début boucle temporelle
  {
    print (c(sc,t))
    #    UseOpt(c("nobonif","nomda","noavpf","noassimil","noptsgratuits","nomc","nomg"))

  
    # Liquidations  
    for (i in 1:55000)       # Début boucle individuelle
    {
      Leg <- t
      # Liquidation
      
      if ((t-t_naiss[i]>=55) && (ageliq[i]==0))
      {
        if (sc>1)
        {        
          UseLeg(t,t_naiss[i])
          SimDir(i,t,"exo",ageref)
        }
        else          
        {
          UseLeg(t,t_naiss[i])
          SimDir(i,t,"TP")
        }
        
        if (t_liq[i]==t)
        {
          pliq_[i,sc] <- pension[i]
          pliq_rg[i,sc] <- pension_rg[i]
          pliq_fp[i,sc] <- pension_fp[i]
          
          if (sc==1) 
          {
            ageref[i] <- t-t_naiss[i]
            mc[i] <- indic_mc[i]
            mg[i] <- indic_mg[i]
            mincont[i]<-min_cont
            minga[i]<-min_garanti
          }
        }
      } 
      
      else if (ageliq[i]>0)
      { 
        Revalo(i,t,t+1)    
      }
      
    } # Fin de la boucle individuelle 
    
    if (sc==1)
    {
      liquidants     <- which(t_liq>=105 & t_liq <= 150)
      liquidants_rg  <- which(t_liq>=105 & t_liq <= 150 & pension_rg>0 & pension_fp==0 & pension_in==0)
      liquidants_fp  <- which(t_liq>=105 & t_liq <= 150 & pension_fp>.75*pension)
      liquidants_in  <- which(t_liq>=105 & t_liq <= 150 & pension_in>.75*pension)
      liquidants_po  <- setdiff(liquidants,union(liquidants_rg,union(liquidants_fp,liquidants_in)))
    }
    
    
    actifs     <- (salaire[,t]>0) & (statut[,t]>0)
    retraites  <- (pension>0) & (statut[,t]>0)
    actifsa     <- (salaire[1:10000,t]>0) & (statut[1:10000,t]>0)
    retraitesa  <- (pension[1:10000]>0) & (statut[1:10000,t]>0)
    if (sc >0)
    {
      SALMOY[sc,t]       <- mean (salaire[actifs,t]/Prix[t])
      MPENS[sc,t]        <- W*sum(pension[retraites])/Prix[t] 
      MPENSa[sc,t]        <- W*sum(pension[retraitesa])/Prix[t]
      MPENLIQ[sc,t]      <- W*sum(pension[which( (pension[]>0)&t_liq[]==t)])/Prix[t]  
      MPENLIQa[sc,t]      <- W*sum(pension[which( (pension[1:10000]>0)&t_liq[1:10000]==t)])/Prix[t]
      MSAL[sc,t]         <- W*sum(salaire[actifs,t])/Prix[t] 
      RATIOFIN[sc,t]     <- MPENS[sc,t]/MSAL[sc,t]
      PENMOY[sc,t]       <- mean (pension[retraites]/Prix[t])
      PENLIQMOY[sc,t]    <- mean (pension[which( (pension[]>0)&t_liq[]==t)])
      PENLIQMOY[sc,t]    <- mean (pension[which( (pension[]>0)&t_liq[]==t)])
      PENREL[sc,t]       <- PENMOY[sc,t]/SALMOY[sc,t]
    }  
  } # Fin de de la boucle temporelle
  
  
  
} # Fin boucle scenarios



#### Sorties ####
#graph_compar(MPENS       ,115,159,"Masse des pensions ")
#graph_compar(PENREL      ,115,159,"Ratio pension/salaire")
#graph_compar(MPENLIQ      ,115,159,"Ratio pension/salaire")
par(mar=c(6.1, 3.1, 4.1, 2.1))
par(xpd=TRUE)
plot   (seq(2010,2059,by=1),RATIO[1,110:159],xlab="Annee", ylab="ratio retraite/PIB",ylim=c(0.20,0.30),col="grey0",lwd=4,type="l")
points (seq(2010,2059,by=1),RATIOFIN[7,110:159],lwd=4,col="grey80",type="l")
title("Graphe : Evolution du ratio retraites/PIB \n Comparaison Système actuel avec ou sans ANC", cex.main = 0.9)
legend.text <- c("ANC","NO ANC")
legend("bottom",inset=c(-0.2,-0.55),cex=0.8,legend.text, fill=c("grey0","grey80"))



save.image(paste0(cheminsource,"Simulations/CN/Dispositifs NC/ANC2.RData"))
#save(MPENS,MPENLIQ,RATIOFIN,file=paste0(cheminsource,"Simulations/CN/Dispositifs NC/MasseANC.RData"))

