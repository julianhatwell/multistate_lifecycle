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

D <- Biograph.long(GLHS)
# only job episodes
DJ600 <- D$Depisode[D$Depisode$OR=="J",]
table(DJ600$sex)

DJ600$time <- DJ600$Tstop-DJ600$Tstart
data(rrdat) # the data are included in the Biograph package
rrdat <- data.frame(rrdat) 
DJ600$pres <- rrdat[,10] 
DJ600$NOJ <- rrdat[,2] 
DJ600$LFX <- DJ600$Tstart-DJ600$LMentry 
DJ600$PNOJ <- DJ600$NOJ-1 
surv  <- Surv(DJ600$Tstart,DJ600$Tstop,DJ600$status) 
surv2 <- with(DJ600, Surv(Tstart,Tstop,status))

# empirical survival function
KM <- survfit(Surv(time,status)~sex
              , data=DJ600)
plot(KM
     , conf.int=TRUE
     , main = "Empirical Survival Function"
     , xlab="Duration of job episode months)"
     , ylab="survival probability"
     , col=c("red","blue")
     , sub="Data from cohorts 1929-31, 1939-41 and 1949-51; 600 job episodes; Compare BR2002 p. 78"
     , cex.sub=0.7)
legend("topright"
       , c("Males","Females")
       , col=c("red","blue")
       , lty = 1
       , lwd = 1
       , cex=0.9, bg="white"
       )

z <- survreg(formula=Surv(time,status) ~ 1
             , data=DJ600
             , dist="exponential")
summary(z)
# exit rate
exp(-z$coefficients[1])
zp <- predict(z,se.fit=TRUE)

# using the covariates
zs <- survreg(Surv(time, status) ~ as.factor(sex)
              , data=DJ600
              , dist="exponential")
summary(zs)
# exit rate males
exp(-zs$coefficients[1])
# exit rate females
exp(-(zs$coefficients[1]+zs$coefficients[2]))
# female exit rate is 77% higher than male
100*(exp(-zs$coefficients[2])-1)

EM <- survreg(Surv(time, status) ~ as.factor(sex)+as.factor(cohort)
              , data=DJ600, dist="exponential")
# adjusting for representation in each cohort
# female exit rate is 66% higher than male
100*(exp(-EM$coefficients[2])-1)
coefficients(EM)

# Cox Model - Transition Rate
Cox_s <- coxph(Surv(time,status) ~ sex
               , data=DJ600
               , method="breslow")

summary(Cox_s)
# exp coef is the ratio between male and female
# female is 1.53 or 53% higher
summary(Cox_s)$coefficients[2]
# baseline hazard rates
basehaz(Cox_s, centered = FALSE)
basehaz(Cox_s) # synthetic average

Dm <- DJ600[DJ600$sex == "Male",]
Df <- DJ600[DJ600$sex == "Female",]

# separate strata for sex
Cox_s <- coxph(Surv(time,status) ~ + strata(sex)
               , data=DJ600
               , method="breslow")
sfits <- survfit(Cox_s)

plot(sfits[1]
     , conf.int=TRUE
     , lty=c(1,2,2)
     , xlab="Job duration (months)"
     , ylab="Survival probability"
     , col="red", mark.time=FALSE)
lines(sfits[2]$time
      , sfits[2]$surv
      , lty=1, col="blue")
lines(sfits[2]$time
      , sfits[2]$lower
      , lty=2, col="blue")
lines(sfits[2]$time
      , sfits[2]$upper
      , lty=2, col="blue")
legend("topright"
       , legend=c("Males","Females")
       , col=c("red","blue")
       , lty = 1
       , lwd = 1
       , cex=0.9,bg="white")

plot(sfits[2]
     , conf.int=T
     , lty=c(1,2,2)
     , fun="cumhaz"
     , xlab="Job duration (months)"
     , ylab="Cumulative hazard"
     , col="blue"
     , mark.time=FALSE)
lines(sfits[1]$time
      , -log(sfits[1]$surv)
      , lty=1, col="red")
lines(sfits[1]$time
      , -log(sfits[1]$lower)
      , lty=2, col="red")
lines(sfits[1]$time
      , -log(sfits[1]$upper)
      , lty=2, col="red")
legend("topright"
       , legend=c("Males","Females")
       , col=c("red","blue")
       , lty = 1
       , lwd = 1
       , cex=0.9,bg="white")

# a full model with all covariates
Cox_full <- coxph(Surv(Tstop-Tstart, status) ~
                    edu+as.factor(cohort)+LFX+PNOJ+pres
                  , data=DJ600
                  , method="breslow"
                  , na.action=na.exclude
                  , iter.max=100)

cfs <- survfit(Cox_full)
cbh <- basehaz(Cox_full)
ms <- coxph.detail(Cox_full)

# looking at whether hazard rate is static over time
Cox_full.zph <- cox.zph(Cox_full
                        , transform="identity"
                        , global=TRUE)
plot(Cox_full.zph[1])
plot(Cox_full.zph[6])

# predicting indivdual outcomes
# create a model with cohort and sex
Cox_sc <- coxph(Surv(Tstop-Tstart,status)~ 
                  sex+cohort
                , data=DJ600
                , method="breslow"
                , na.action=na.exclude
                , iter.max=100)
# estimate person years
y <- pyears(Cox_sc
            , DJ600
            , data.frame=TRUE
            , scale=1)

# create stats for hypothetical individual
ID.2.1 <- data.frame(edu=10
                     , cohort="1929-31"
                     , LFX=0
                     , PNOJ=0
                     , pres=22)

# generate the survival function for this indiv
sfit.2.1 <- survfit(Cox_full
                    , newdata=ID.2.1)

z <- cbind(sfit.2.1$time,sfit.2.1$surv)
# % chances
100 * z[which(z[,1]==120),2] # keep job at least 10 years
100 - (100 * z[which(z[,1]==120),2])  # exit before 10 years
100 * z[which(z[,1]==48),2] # keep job at least 4 years
100 - (100 * z[which(z[,1]==48),2])  # exit before 4 years

plot(sfit.2.1
     , las=1
     , xlab="Job duration (month)"
     , ylab="Survival probability"
     , mark.time=FALSE
     , cex.main=0.9
     , conf.int=T
     , col="black")
zv <- cbind(colnames(ID.2.1)
            , t(ID.2.1))
legend(220,1.0
       , legend=zv[,1]
       , box.lty=0, cex=0.9)
legend(280,1.0,legend=zv[,2]
       , box.lty=0, cex=0.9)

# comparing individuals
indiv <- data.frame(edu=c(9,9,19,19)
                    , cohort=c("1929-31","1939-41","1929-31","1939-41")
                    , LFX=0
                    , PNOJ=0
                    , pres=44)
sfit <- survfit(Cox_full,newdata=indiv)
colours <- c("black","red","blue","green")
plot(sfit
     , las=1
     , col=colours
     , xlab="Job duration (month)"
     , ylab="Survival probability"
     , main="Effects of covariates on job duration"
     , conf.int=F
     , mark.time=FALSE)
legend(150
       , 1.0
       , legend=c("edu=9, cohort=1929-31,LFX=0,PNOJ=0,pres=44", "edu=19,cohort=1939-41,LFX=0,PNOJ=0,pres=44", "edu=9, cohort=1929-31,LFX=0,PNOJ=0,pres=44", "edu=19,cohort=1939-41,LFX=0,PNOJ=0,pres=44")
       , cex=0.7
       , col=colours
       , fill=colours)

# ratio compared to the mean
z1 <- predict(Cox_full
              , newdata=indiv
              , type="risk"
              , se.fit=TRUE)

# hazard
# nelson aalen estimator
NeAa <- basehaz(coxph(Surv(time,status)~1,data=DJ600))
plot(NeAa[,2:1], type = "l")
# an alternative
fit <- survfit (Surv(time,status)~1
                , data=DJ600) 
cumhaz <- cumsum(fit$n.event/fit$n.risk)
plot(NeAa[,2], cumhaz, type = "l")

# extensions with the eha package
ev <- table.events(exit=DJ600$time,event=DJ600$status)

# base model without covariates
z <- aftreg(Surv(time,status) ~ 1
            , dist="weibull"
            , data=DJ600, shape=0)
summary(z)

# multiple covariates
z<- aftreg(Surv(time,status) ~ 
             edu+cohort+LFX+PNOJ+pres
           , dist="weibull"
           , data=DJ600)
summary(z)
# job exit rate is = r(t) = b * a^b * t^(b-1)
# a is scale, b is shape
# e.g. for an individual with 10 years edu and prestige score 22
shape <- exp(z$coefficients["log(shape)"])
scale <- exp(-z$coefficients["log(scale)"] + z$coefficients["edu"] * 10 + z$coefficients["pres"] * 22)
w <- hweibull(x=1:428
              , shape = shape
              , scale = scale)
plot(w, type = "l")
plot.aftreg(z, fn="haz"
            , ylim=c(0.01,0.018)
            , ylab="Job exit rate"
            , xlab="Job duration (months)")
plot.aftreg(z, fn="cum"
            , ylim=c(0.01,0.018)
            , ylab="Job exit rate"
            , xlab="Job duration (months)")