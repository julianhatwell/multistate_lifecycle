library(Biograph)
library(survival)
library(eha)
library(mvna) 
library(etm)
library (msm)
library(Epi)
library(TraMineR)
library(lattice)
library(ggplot2)
data(GLHS)

Dmvna <- Biograph.mvna(GLHS)

na <- mvna(data=Dmvna$D
           , state.names=c("N","J")
           , tra=attr(Dmvna$D,"param")$trans_possible
           , cens.name=Dmvna$cens)

xyplot(na,tr.choice=c("N J","J N")
       , aspect=1
       , strip=strip.custom(bg="white"
                            , factor.levels=c("NoJob to Job"
                                              , "Job to NoJob")
                            , par.strip.text=list(cex=0.9))
       , scales=list(alternating=1)
       , xlab="Age in years"
       , xlim=c(10,60)
       , ylab="Nelson-Aalen esimates")

plot(na)

plot(c(0, na$'N J'$time)
     ,c(0,na$'N J'$na)
     , type="l"
     , xlab="Age (years)"
     , ylab="cumulative hazard"
     , xlim=c(10,50)
     , main="GLHS Labour market transitions"
     , col="red", axes=F, lwd=2)
lines(c(0, na$'N J'$time)
      , c(0,na$'N J'$na-sqrt(na$'N J'$var.aalen))
      , col="red",lty=2,lwd=1)
lines (c(0, na$'N J'$time)
       , c(0,na$'N J'$na+sqrt(na$'N J'$var.aalen))
       , col="red",lty=2,lwd=1)
axis(side=1, at=seq(10,50,by=5)
     , labels=seq(10,50,by=5)
     , cex.axis=0.8) 
axis(side=2, las=1
     , at=seq(0,max(na$'N J'$na), by=0.5)
     , labels=seq(0,max(na$'N J'$na), by=0.5)
     , cex.axis=0.8)
box()
abline(h=seq(0,max(na$'N J'$na), by=0.5)
       , lty=2,col="lightgrey")
abline(v=seq(10,50,by=5)
       , lty=2,col="lightgrey")
lines(c(0,na$'J N'$time)
      , c(0,na$'J N'$na)
      , col="darkgreen"
      , lty=1, lwd=2)
lines(c(0,na$'J N'$time)
      , c(0,na$'J N'$na-sqrt(na$'J N'$var.aalen))
      , col="darkgreen", lty=2)
lines(c(0,na$'J N'$time)
      , c(0,na$'J N'$na+sqrt(na$'J N'$var.aalen))
      , col="darkgreen", lty=2)
legend(10,4,c("NJ","JN")
       , col=c("red","darkgreen")
       , lty = 1, lwd = 1
       , cex=0.9, bg="white")

cumh.1 <- predict(na,times=seq(0,53,by=1))
