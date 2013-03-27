
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsMS.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsPensIPP.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/DefVarRetr_Destinie.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsLeg.R")) )
source( (paste0(cheminsource,"Modele/Outils/OutilsRetraite/OutilsRetr.R")) )

###### Fonction UseOptCN
# Spécifie les options supplémentaires si option "CN" a été demandées avec UseOpt.
# Il y a deux arguments. Le premier est le millésime de démarrage du basculement.
# Le second est une chaine de caractère, avec les mêmes principes que dans UseOpt,
# pouvant contenir les éléments suivants :

# Exemples:
#UseOptCN(2010,c("immediat", "rg"))



UseOptCN <- function(aCN,liste=c())
{    
  AnneeDepartCN <<-aCN 
  OptionsCN     <<- c()
  for (a in 1:length(liste))
  {
    OptionsCN[a] <<- tolower(liste[a])
  }
  
  # Calcul de l'âge de démarrage du décompte des points
  if (is.element("immediat",liste))    
  {
    t_debCN <<-1
  }
  else if (is.element("progressif",liste))  
  {
    t_debCN<<-(AnneeDepartCN)
  }

  
}


#####Fonction UseConv
# Met a jour les coefficients de conversion si régime en comptes notionnels.
# Il y a deux modes d'appel. Le premier mode doit-être utilisé lorsqu'on veut des
# coefficients de conversion basés sur l'espérance de vie courante. 
# Calcule les coefficients de conversion entre $agemin et $agemax égaux à 60 et 70
# ans sur la base de la mortalité  de la date $t.
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
  if (t>=AnneeDepartCN)
  {
    # Coeffs d???finis par la mortalit??? (suppose que les coefficients de revalo
    # prospectifs sont au moins d???finis jusqu'en 2050)
    
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
PointsCN <- function(i,t)
{
  points_cn_pri  <<- 0
  points_cn_fp   <<- 0
  points_cn_ind  <<- 0
  
  # Calcul des cumuls de points CN, selon étendue du nouveau régime
  if (t>=AnneeDepartCN)
  {
    for (a in (30:t))   
    {

      # Application du rendement aux cotisations portées au compte : A DISCUTER & 
      points_cn_pri<<- points_cn_pri*(1+RendementCN[a-1])
      points_cn_ind<<- points_cn_ind*(1+RendementCN[a-1])
      points_cn_fp <<- points_cn_fp*(1+RendementCN[a-1])
      

 if (statut[i,a]%in% c(cadreCN,non_cadreCN))
 {
 points_cn_pri <<-  points_cn_pri + TauxCotCN[a]*min(salaire[i,a],8*PlafondSS[a])
 }
 else if (statut[i,a]%in% indepCN)
 {
 points_cn_ind <<-  points_cn_ind + TauxCotCN[a]*min(salaire[i,a],8*PlafondSS[a])
 }
 else if (statut[i,a]%in% c(fonct_aCN,fonct_sCN))
 {
 points_cn_fp  <<- points_cn_fp +TauxCotCN[a]*min(salaire[i,a],8*PlafondSS[a])
 }
      
  
    }  # Fin boucle sur a
 #   print(c(points_cn_pri,points_cn_ind,points_cn_fp))  
  }
}