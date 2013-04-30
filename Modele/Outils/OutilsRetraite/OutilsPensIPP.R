######################################################################################################
#                         Bibliotheque d'utilitaires generaux PENSIPP                                #
#                                                                                                    #
#                                                                                                    #
# Cette bibliotheque contient                                                                        #
# B) des fonctions calculant diverses probabilites d'evenements demographiques                       #
# C) des procedures de gestion des liens familiaux                                                   #
# D) des procedures de tabulation                                                                    #
# E) des fonctions d'entree-sortie'                                                                  #
#                                                                                                    #
# Quelques conventions :                                                                             #
# - R demarrant l'indicage des tableaux par 1, l'age est egalement compte a partir de 1. Autrement   #
#   dit, si on se situe au 1/1 d'une annee donnee, il s'agira de l'age atteint dans l'annee.         #
# - Toutes les dates sont comptees par rapport a l'annee 1900, par exemple t_naiss[i]=90 pour un     #
#   individu ne en 1990, statut[i,110] pour son statut en 2010. De meme, pour les series temporelles,#
#   x[107] correspond a une valeur de l'annee 2007 et la premiere position utilisable est donc       #
#   l'annee 1901.                                                                                    #
# - Lorsqu'un apparente n'est pas defini, son ID est a zero                                          #
# - Codes particuliers                                                                               #
#   conjoint : -3=veuf, -2=separe, -1=celibataire, >0=id du conjoint                                 #
#   statut   : -2=non vivant, -1=vivant hors territoire, >0=statuts d'activite des presents          #
#                                                                                                    #
######################################################################################################



#A ) PROBA D'EVENEMENTS DEMOGRAPHIQUES--------

proba_union <- function(i,t)
# Coefficients d'apres Duee (2006)
{
  # On calcule l'age courant et la duree depuis la separation (s'il y a lieu)
  a     <- age[i]
#  d     <- t-max(which(conjoint[i,]>0))
  d <- 0
  p <- 0
  if (statut[i,t]>0)
  {
    if (conjoint[i,t] == celib) 
    {
      if (sexe[i] ==1) 
      {
        p <- exp(-73.10+8.96*a-4.08*(a**2)/10+7.96*(a**3)/1000-5.72*(a**4)/100000)
      }
      else
      {
        p <- exp(-68.49+9.06*a-4.41*(a**2)/10+9.14*(a**3)/1000-6.91*(a**4)/100000)
      }
    }
    else if (conjoint[i,t] == separ || conjoint[i,t] == veuf)
    {
      if (sexe[i]==1)
      {
        p <- exp( -0.25-0.09*d+0.19*(d==0)-0.05*(a-d)+0.41*(n_enf[i]==0))
      }
      else
      {
        p <- exp( 0.33-0.10*d+0.14*(d==0)-0.08*(a-d)+0.41*(n_enf[i]==0)
                                                    -0.57*(conjoint[i,t]==veuf))
      }
    }
  }
  return (p)
}

proba_separ <- function(i,t)
# Les probas sont appliquees a la femme
{
  if (statut[i,t]>0 && sexe[i]==2 && conjoint[i,t]>0)
  {
    # age a l'union, duree ecoulee et nombre d'enfants de l'union en cours
    a <- min(which(conjoint[i,]==conjoint[i,t]))-anaiss[i]
    d <- age[i]-a
    n_union <- length(intersect(liste_enf(i,t),liste_enf(conjoint[i,t],t)))
    n_autre <- n_enf[i]-n_union
#    print (c(a,d,n_union,n_autre))
    return (exp(-2.02-0.06*d-0.04*a+0.58*(n_union==0)
                                   +0.21*(n_union==1)
                                   +0.13*(n_union>3)
                                   +0.41*(n_autre>0)))
  }
  else
  {
    return(0)
  }
}

proba_naissance <- function(i,t)
  # A ce stade, fonction se contentant de controler le fait d'etre une femme en couple et 
  # le fait d'avoir un nombre d'enfants inferieur au nombre maximum gere par le programme. 
{
  p <- 0
  if (statut[i,t]>0)
  {
    if (conjoint[i,t]>0)
    {
      if ((age[i] > 15) && (age[i]<52) && (sexe[i]==2) 
          && (n_enf[i]<nb_enf_max) && (n_enf[conjoint[i,t]]<nb_enf_max))
      {
        p <- 0.5
      }
    }
  }
  return (p)
}

# B) FONCTIONS DE GESTION DES LIENS FAMILIAUX----------

# -> liste_enf : retourne la liste des id des enfants de l'individu i : par defaut l'ensemble
#                des ces enfants, ou seulement les enfants encore en vie (option="vivants")
liste_enf  <- function(i,t,option="tous")
{
  liste  <- c() 
  nb_enf <- 0
  for (e in 1:nb_enf_max)
  {
    if (enf[i,e]>0)
    {
      if ((option == "tous") | (option == "vivants" & statut[enf[i,e],t]>0))
      {
        nb_enf <- nb_enf+1
        liste[nb_enf] <- enf[i,e]
      }
    }
  } 
  return (liste)
}

# -> nb_enf : retourne le nombre d'enfants de l'individu i : par défaut l'ensemble
#             des ces enfants, ou seulement les enfants encore en vie (option="vivants")
nb_enf <- function(i,t,option="tous")
{
  nb_enf <- 0
  if (statut[i,t]>0)
  {
    for (e in 1:nb_enf_max)
    {
      if (enf[i,e]>0)  
      {
        if ((option == "tous") | (option == "vivants" & statut[enf[i,e],t]>0))
        {
          nb_enf <- nb_enf+1
        }
      }
    }
  } 
  return (nb_enf)
}

# C) FONCTION DE TABULATION SUR VARIABLES DESTINIE------------
#
# -> Fonction "pyram"
#
# Retourne des efffectifs par age, avec filtrage optionnel. Le second parametre, egalement optionnel, est le 
# pas de la pyramide, par exemple pas=5 pour un regroupement quinquennal. Par defaut, le pas est egal 
# est egal a 1
# 
# Exemple d'utilisations
#  pyramide[,t]    <- pyram(t)
#  pyrquinq[,t]    <- pyram(t,pas=5)
#  nb_maries[s,,t] <- pyram(t,sexe==s & matri==2,pas=5)
#  tx_maries[s,,t] <- pyram(t,sexe==s & matri==2)/pyram(sexe==s)

pyram <- function(filtre=rep(TRUE,taille_max),pas=1)
{
  pyramide <- numeric(floor((age_max-1)/pas)+1)
  pyramide <- ts(pyramide,start=1,frequency=1/pas)
  for (a in 1:length(pyramide))
  {
    pyramide[a] <- inflate(length(which(filtre & age>(a-1)*pas & age<=a*pas)))/pas
  }
  return (pyramide)
}

# -> function "by_age"
# Calcul de differentes statistiques descriptives univariees pour une variable donnee, ventilees
# par age avec un pas ajustable (pas=1 par defaut) et filtrage eventuel. Par défaut, la statisti-
# que calculee est la moyenne (type="mean"). Les autres options possibles sont "min, "max", "median" 
# et "sd".
#
# Exemples d'appel
#  sal_moy[,t] <- by_age(salaire,t,(sexe==2),pas=5)
#  sd_sal[,t]  <- by_age(salaire,t,(sexe==2),pas=5,type="sd")

by_age <- function(var,t,filtre=rep(TRUE,taille_max),type="mean",pas=1)
{
  y <- numeric(floor((age_max-1)/pas)+1)
  y <- ts(y,start=0,frequency=1/pas)
  for (a in 1:length(y))
  {
    if (type == "mean")
    {
       y[a] <- mean(subset(var,filtre & age>(a-1)*pas & age<=a*pas))
    }
    if (type == "sum")
    {
       y[a] <- inflate(sum(subset(var,filtre & age>(a-1)*pas & age<=a*pas)))
    }
    else if (type == "max")
    {
       y[a] <- max(subset(var,filtre & age>(a-1)*pas & age<=a*pas))
    }
    else if (type == "min")
    {
       y[a] <- min(subset(var,filtre & age>(a-1)*pas & age<=a*pas))
    }
    else if (type == "median")
    {
       y[a] <- median(subset(var,filtre & age>(a-1)*pas & age<=a*pas))
    }
    else if (type == "sd")
    {
      y[a] <- sd(subset(var,filtre & age>(a-1)*pas & age<=a*pas))
    }
  }
  return (y)
}


# D) ENTREES-SORTIES------------------
  
# -> Fonction read_ap
#
# Lecture d'un tableau de donnees par age et periode dans le fichier dont le nom est passe en premier
# argument. Le resultat est sous forme de matrice dont le premier indice est la periode et le second
# est l'age, e.g. quotient_mortalite[t,a]. La meme fonction peut evidemment servir a lire un tableau 
# par generation et age, a charge pour l'utilisateur de l'utiliser ensuite de maniere adequate. Un 
# second parametre permet de specifier si l'age se lit en ligne ou en colonne dans le fichier d'origine

read_ap <- function (fichier="",age="ligne")
{  
  buf <- read.table(fichier,sep=";")
  if (age == "ligne")
  {
    a_min <- buf[1,2]
    a_max <- buf[1,ncol(buf)]
    t_min <- buf[2,1]%%1900
    t_max <- buf[nrow(buf),1]%%1900
    tab <- matrix(nrow=age_max,ncol=t_max)
    for (a in a_min:a_max)
    {
      for (t in t_min:t_max)
      {
        tab[a+1,t] <- buf[t-t_min+2,a-a_min+2]
      }
    }
  }
  else
  {
    t_min <- buf[1,2]%%1900
    t_max <- buf[1,ncol(buf)]%%1900
    a_min <- buf[2,1]
    a_max <- buf[nrow(buf),1]    
    tab <- matrix(nrow=age_max,ncol=t_max)
    for (a in a_min:a_max)
    {
      for (t in t_min:t_max)
      {
        tab[a+1,t] <- buf[a-a_min+2,t-t_min+2]
      }
    } 
  }
  return (tab)
}


# E) GRAPHIQUES ---------

# Graph_compar (DB): comparaison de series temporelles
graph_compar <- function (serie,t1,t2,titre)
{
  plot   (seq(1900+t1,1900+t2,by=1),serie[1,t1:t2],xlab="Annee", ylab=titre,
          ylim=c(min(serie[,t1:t2],na.rm=TRUE),max(serie[,t1:t2],na.rm=TRUE)),lwd=2,col="orange",type="l")
  points (seq(1900+t1,1900+t2,by=1),serie[2,t1:t2],lwd=4,type="l")
  points (seq(1900+t1,1900+t2,by=1),serie[3,t1:t2],lwd=3,type="l")
  points (seq(1900+t1,1900+t2,by=1),serie[4,t1:t2],lwd=2,type="l")

}

 