pop          <- matrix(nrow=200,ncol=100)
surv         <- matrix(nrow=200,ncol=100)
wref         <- numeric(100)
naiss        <- numeric(200)
ev           <- numeric(200)
g            <- numeric(200)
rdt          <- numeric(200)
va           <- numeric(200)
vs           <- numeric(200)
taux         <- numeric(200)
total_points <- numeric(200)
total_cotis  <- numeric(200)
ratiodem     <- numeric(200)
ratiofin     <- numeric(200)
nvrel        <- numeric(200)
points       <- matrix(nrow=200,ncol=100)
pension      <- matrix(nrow=200,ncol=100)
pensionrel   <- matrix(nrow=200,ncol=100)
w            <- matrix(nrow=200,ncol=100)


ev          <- pmax(rep(80,200),pmin(rep(90,200),(200:400)/4))
ev          <- rep(80,200)
naiss       <- rep(1,200)
#naiss       <- pmax(rep(1,200),pmin(rep(1.5,200),(1:200)/50))
va          <- rep(1,200)
taux        <- rep(.25,200)
g           <- rep(.02,200)
g[120:200]  <- 0.00
rdt         <- g
wref[20:59] <- rep(1,40)



# Bloc démographique et carrière
for (t in 1:200)
{
  for (a in 1:100)
  {
    surv[t,a] <- min(1,(100-a)/(200-2*ev[t]))
  }
  if (t==1)
  {
    pop[1,] <- naiss[1]*surv[1,]
    w[1,20:59] <- wref[20:59]
  }
  else
  {
    pop[t,1] <- naiss[t]
    for (a in 2:100)
    {
      pop[t,a] <- pop[t-1,a-1]*(surv[t,a]/surv[t,a-1])
    }
    w[t,20:59] <- w[t-1,20:59]*(1+g[t])
  }
}

# Bolc retraites
for (t in 60:200)
{
  points[t,60] <- 0
  for (u in 20:59)
  {
    points[t,60] <- points[t,60]*(1+rdt[t-60+u])+taux[t-60+u]*w[t-60+u,u]/va[t-60+u]
  }
  for (a in 61:100)
  {
    points[t,a] <- points[t-1,a-1]*(1+rdt[t])
  }
  total_points[t] <- sum(points[t,60:100]*pop[t,60:100])
  total_cotis[t]  <- taux[t]*sum(w[t,20:59]*pop[t,20:59])
  vs[t]           <- total_cotis[t]/total_points[t]
  pension[t,60:100] <- points[t,60:100]*vs[t]
  pensionrel[t,60:100] <- pension[t,60:100]/(sum(w[t,20:59]*pop[t,20:59])/sum(pop[t,20:59]))
  ratiodem[t]       <- sum(pop[t,60:100])/sum(pop[t,20:59])
  print (c(t,ratiodem[t]))
  ratiofin[t]       <- sum(pension[t,60:100]*pop[t,60:100])/sum(w[t,20:59]*pop[t,20:59])
  nvrel[t]          <- ratiofin[t]/ratiodem[t]
}
  
persp((1:200),(1:100),pop[1:200,1:100],ticktype="detailed",theta=30 ,phi=40,r=3,d=3,shade=.1,
      main="Population",xlab="Date",ylab="Age",zlab="")
persp((1:200),(1:100),w[1:200,1:100],ticktype="detailed",theta=30 ,phi=40,r=3,d=3,shade=.1,
      main="Salaires",xlab="Date",ylab="Age",zlab="")
persp((100:200),(60:100),points[100:200,60:100],ticktype="detailed",theta=30 ,phi=40,r=3,d=3,shade=.1,
      main="Points",xlab="Date",ylab="Age",zlab="")
persp((100:200),(60:100),pension[100:200,60:100],ticktype="detailed",theta=120 ,phi=40,r=3,d=3,shade=.1,
      main="Pensions",xlab="Date",ylab="Age",zlab="",cex=.5,cex.axis=.5,cex.lab=.5)
persp((100:200),(60:100),pensionrel[100:200,60:100],ticktype="detailed",theta=120 ,phi=40,r=3,d=3,shade=.1,
      main="Pension/salaire moyen",xlab="Date",ylab="Age",zlab="",cex=.5,cex.axis=.5,cex.lab=.5)
plot(60:100,pension[100,60:100],ylim=c(0,max(pension[100:200,60:100])),type="l",col="red",xlab="age",ylab="pension")
for (t in 101:200)
{
  if (t%%10==0) {points(60:100,pension[t,60:100],type="l",col="red",lwd=2)} 
  else          {points(60:100,pension[t,60:100],type="l")}
}
plot(60:100,pensionrel[100,60:100],ylim=c(0.2,max(pensionrel[100:200,60:100])),type="l",col="red",lwd=2)
for (t in 101:200)
{
  if (t%%10==0) {points(60:100,pensionrel[t,60:100],type="l",col="red",lwd=2)} 
  else          {points(60:100,pensionrel[t,60:100],type="l")}
}
plot(100:200,ratiodem[100:200],ylim=c(0,1),type="l",col="blue",xlab="",ylab="",lwd=3)
#points(100:200,ratiofin[100:200],type="l",col="red")
points(100:200,nvrel[100:200],type="l",col="green",lwd=3)
points(100:200,vs[100:200]*10,type="l",col="cyan",lwd=3)
legend (x=100,y=0.22,cex=0.7,bty="n",lwd=3,
        legend=c("Ratio 60+/20-59","Ratio pension moyenne/salaire moyen","Valeur service du point (x10-"),
        col=c("blue","green","cyan"))


