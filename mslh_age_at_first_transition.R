library(Biograph)
library(mvna) 
library(etm)
library (msm)
library(Epi)
library(TraMineR)
library(lattice)
library(ggplot2)
data(GLHS)

# the first NJ transition
# age at entry to jobs market
z <- TransitionAB(Bdata=GLHS,"NJ")
GLHS$age_at_LMentry <- z$age

colours=c("1929-31"="red"
          , "1939-41"="black"
          ,"1949-51"="purple")
p <- ggplot(GLHS
            , aes(y=age_at_LMentry
                  , colour=cohort
                  , shape=sex)) +
  theme_bw() +
  scale_colour_manual(values=colours)

# randomly assigned ID - not much cohort or gender effect visible
p + geom_point(aes(x = ID)) + facet_grid(sex~.)

# here you can see the cohorts. a bit artificial
# but isn't there a slight upward trend?
p + geom_point(aes(x = born)) + facet_grid(sex~.)

qplot(age_at_LMentry
      , data=GLHS
      , geom="histogram"
      , binwidth=1
      , fill=cohort)

qplot(age_at_LMentry
      , data=GLHS
      , geom="histogram"
      , binwidth=1
      , fill=cohort
      , facets=sex~.)

# creating a new factor 
# inferring edu level from duration of edu
GLHS.e <- GLHS 
GLHS.e$edu2<- factor(ifelse(GLHS$edu<=11,1,2)
                     , labels=c("-LowerSec","Middle+"))
p.e <- ggplot(data = GLHS.e
              , aes(x = ID
                    , y = age_at_LMentry
                    , colour = cohort
                    , shape = sex)) +
  theme_bw() +
  scale_colour_manual(values=colours)
p.e + geom_point() + facet_grid(edu2~.)
p.e + geom_point() + facet_wrap(~edu2,nrow=2)

tit <- "Age at labour market entry"
ages <- range(GLHS.e$age_at_LMentry)
bins <- floor(ages[1]):ceiling(ages[2])
histogram(~age_at_LMentry | cohort + edu2
          , data = GLHS.e
          , breaks = bins
          , main = tit
          , xlab="Age"
          , scale=list(x=list(alternating=FALSE)))

densityplot(~age_at_LMentry | sex
          , data = GLHS.e
          , groups = cohort
          , plot.points = "rug"
          , auto.key = TRUE
          , main = tit
          , xlab="Age"
          , scale=list(x=list(alternating=FALSE)))
