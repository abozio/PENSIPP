
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsRetr.R")) )

###### Fonction UseOptCN
# Spécifie les options supplémentaires si option "CN" a été demandées avec UseOpt.
# Il y a deux arguments. Le premier est le millésime de démarrage du basculement.
# Le second est une chaine de caract?re, avec les mêmes principes que dans UseOpt,
# pouvant contenir les éléments suivants :

# Exemples:
#UseOptCN(2010,c("immediat", "rg"))



UseOptCN <- function(liste=c())
{
  OptionsCN <<- c()
  for (i in 1:length(liste))
  {
    OptionsCN[i] <<- tolower(liste[i])
    if (!(is.element(OptionsCN[i],c("valocot","noassimilcn","noavpfcn","nomdacn","nobonifcn","nomccn")))) 
    {
      print (paste("Attention : option '",OptionsCN[i],"' inconnue"))
    }
  }
}


#####Fonction UseConv
# Met a jour les coefficients de conversion si r?gime en comptes notionnels.
# Il y a deux modes d'appel. Le premier mode doit-?tre utilis? lorsqu'on veut des
# coefficients de conversion bas?s sur l'esp?rance de vie courante. 
# Calcule les coefficients de conversion entre $agemin et $agemax ?gaux ? 60 et 70
# ans sur la base de la mortalit?  de la date $t.
# Exemple : UseConv(60,70,$t);
setwd((paste0(cheminsource,"Modele/Parametres/Demographie")))
       
age_max      <- 121
quotient           <- array(0,dim=c(2,121,200))
survie             <- array(0,dim=c(2,121,200))
survie[1,,1:160]   <- read_ap("SHnew.csv")
survie[2,,1:160]   <- read_ap("SFnew.csv")
quotient[1,,1:160] <- read_ap("QHnew.csv")
quotient[2,,1:160] <- read_ap("QFnew.csv")


UseConv <- function(agemin,agemax,t)
{
#  if (t>=AnneeDepartCN)   # Supprime : ne m'a pas semblé utile
  {
    # Coeffs définis par la mortalité (suppose que les coefficients de revalo
    # prospectifs sont au moins définis jusqu'en 2050)
    
    taux_croi <- RendementCNPrev[t]-(RevaloCN[t]-1)      #NB : Revalo et rendement pas dans la m???me unit??? !)
    for (a in (agemin:agemax))
    {
      CoeffConv[a] <<-0
      
      for (u in (a:120))
      {
        CoeffConv[a] <<- CoeffConv[a] + ((1+taux_croi)**(-u+a))*(survie[1,u,t]+survie[2,u,t])        
      }

      
      CoeffConv[a] <<- (survie[1,40,t]+survie[2,40,t])/CoeffConv[a]
      #print (c(a,CoeffConv[a]))
     
    }
    # Coeffs hors des bornes
    CoeffConv[0:(agemin-1)] <<- 0
    CoeffConv[(agemax+1):120] <<- CoeffConv[agemax]
  }
}




##### Fonction PointsCN
# Calcule les points portés au compte individuel acquis par l'individu i à la date t.
PointsCN <- function(i,t,plafond)
{

  points_cn_pri  <<- 0
  points_cn_fp   <<- 0
  points_cn_ind  <<- 0
  points_cn_nc   <<- 0
  points_mccn    <<- 0
  # Calcul des cumuls de points CN, selon étendue du nouveau régime
  
  if (t>=AnneeDepartCN)
  {
    for (a in (30:t))   
    {
 #     print(c("a",a,points_cn_pri ))
      # Application du rendement aux cotisations portees au compte : A DISCUTER & 
      points_cn_pri <<- points_cn_pri*(1+RendementCN[a-1])
      points_cn_ind <<- points_cn_ind*(1+RendementCN[a-1])
      points_cn_fp  <<- points_cn_fp* (1+RendementCN[a-1])
      points_cn_nc  <<- points_cn_nc* (1+RendementCN[a-1])
      
  if (is.element("valocot",OptionsCN))
  {
  if (a < AnneeDepartCN) {points_cn_pri <<- points_cn_pri + CotRetTotAnn(i,a)}
  # ajouté au point privé par défault (décomposition par régime nécessaire?)
  }


# PointsCN pour les periodes travaillees.  

 if (statut[i,a]%in% c(cadreCN,non_cadreCN))
 {
   points_cn_pri <<- points_cn_pri + TauxCotCN[a]*min(salaire[i,a],plafond*PlafondSS[a])
 # print(c(plafond,min(salaire[i,a],plafond*PlafondSS[a])))
 }
      
 else if (statut[i,a]%in% indepCN)
 {
   points_cn_ind<<-points_cn_ind + TauxCotCN[a]*min(salaire[i,a],plafond*PlafondSS[a])
 }
      
 else if (statut[i,a]%in% c(fonct_aCN,fonct_sCN))
 {
   points_cn_fp  <<- points_cn_fp+ TauxCotCN[a]*min(salaire[i,a],plafond*PlafondSS[a])   
 }

# Avantages Non contributifs CN

if ((statut[i,a]==chomeurCN) & (!(is.element("noassimilcn",OptionsCN))))
{
#print(c("chom",i,a,points_cn_nc  ))
# Assiette de cotisation: dernier salaire ou SMIC, plafonné à 4 SMIC.   
liste  <- which(salaire[i,1:(a-1)]>0)
salref <- salaire[i,liste[length(liste)]]  
salref <- min(max(salref,SMIC[a]),4*PlafondSS[a])
#print(c("chom1",a, points_cn_nc  ))
points_cn_nc  <<- points_cn_nc+ TauxCotCN[115]*salref
#print(c("chom2",i,a, points_cn_nc  ))
}  

# AVPF
if ((statut[i,a]%in% avpfCN) & (!(is.element("noavpfcn",OptionsCN))))
{
 #print(c("avpf",a))
  # Assiette de cotisation: SMIC de l'annee en cours.   
  points_cn_nc  <<- points_cn_nc+ TauxCotCN[115]*SMIC[a]
}  

# MDA (lors d'une naissance a l'annee a)
if ((sexe[i]==2) &(is.element(a,t_naiss[enf[i,]])) & (!(is.element("nomdacn",OptionsCN))))
{
#print(c("mda",a))
  # Assiette de cotisation: Moyenne des salaire des années précédentes ou SMIC, plafonne à 4 SMIC., plafonné à 4 SMIC.  
  #liste <- which(is.element(statut[i,1:a],codes_occCN))
 # salref<-min(max(mean(salaire[i,liste]),SMIC[a]),4*SMIC[a])
  salref <- min(max(salaire[i,(a-1)],SMIC[a]),2*PlafondSS[a])
  points_cn_nc  <<- points_cn_nc + TauxCotCN[115]*salref
}  

#print(c(points_cn_nc,a))

  }  # Fin boucle sur a
      
#Bonfication pour pension: 
if (!(is.element("nobonifcn",OptionsCN)))
{
if (n_enf[i]>2)
{ 
  points_cn_pri<<- 1.10*points_cn_pri 
  points_cn_fp <<- 1.15*points_cn_fp 
  points_cn_ind<<- 1.10*points_cn_ind
  points_cn_nc <<- 1.10*points_cn_nc
}
}

# Calcul du MICO: 
points_cn_cont <- points_cn_pri + points_cn_fp + points_cn_ind 
seuil<-MinVieil1[t]/CoeffConv[60] # Seuil: nb de point pour avoir le minimum vieillesse 
pfd <- SMIC[t]/CoeffConv[60]  # Pfd: plafond, nb de points pour lequel le mico est =0.
txmc <- 0
txmc<-affn(points_cn_cont,c(0,seuil,pfd),c(0.1,0.1,0))   
points_mccn<<-txmc*points_cn_cont
    
    
  }
}