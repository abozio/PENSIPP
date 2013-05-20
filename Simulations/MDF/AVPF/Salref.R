
#### Fonction SalRefUniq
# Calcul un salaire de référence unique pour toutes les années travaillées. 

SalRefUniq <- function(i,t)
{

  spc      <- numeric(t_fin)
  revalcum <- 1
  u        <- t
  while (u > t_naiss[i]+15)
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

  annees_act<- which(is.element(statut[i,1:t],c(non_cadre,cadre,avpf,indep,fonct_a,fonct_s)))

  if (length(annees_act)>0) {sal_ref <<- mean(sort(spc[annees_act],decreasing=TRUE)[1:min(DureeCalcSAM,length(annees_act))])}
  else (sal_ref<<-0)
#print(c(i,sal_ref))
}
