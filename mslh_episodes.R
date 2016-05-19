library(Biograph)
library(mvna) 
library(etm)
library (msm)
library(Epi)
library(TraMineR)
library(lattice)
library(ggplot2)
data(GLHS)

# to calculate the episode durations
D <- Biograph.long(GLHS)
DE <- D$Depisode
DE$id <- 1:nrow(DE)
DE$Duration <- DE$Tstop-DE$Tstart 
DE$StateOccupied <- DE$OR 
DE$Status <- factor(DE$status
                    , labels=c("Open","Closed"))

p.e <- ggplot(DE
      , aes(x=id
            , y=Duration
            , shape=Status
            , colour=StateOccupied))
p.e <- p.e + geom_point() + 
  scale_shape_manual(values=c(1,19)) + 
  scale_colour_manual(values=c("N"="red","J"="blue")) + 
  theme_bw() +
  labs(title="Durations of episodes in months")
p.e
  
p.e + theme(plot.title=element_text(colour="red"
                              , size="12"
                              , face="bold"
                              , hjust=0)
      , plot.background=element_rect(fill="lightskyblue1"
                                     , colour="black"
                                     , size=5)
)

# LEXIS
z <- Lexispoints(Bdata=GLHS
     , transition="NJ"
     , title="Calendar time and age\nat labour market entry"
     , cov="sex"
     , legend="topleft")
z <- Lexispoints(Bdata=GLHS
     , transition="JN"
     , title="Calendar time and age\nat labour market entry"
     , cov="sex"
     , legend="topleft")

# ggplot version
z <- Lexis.points(Bdata=GLHS
      , transition="NJ"
      , title="Labour market entry by sex and cohort"
      , cov="sex", group="cohort"
      , legend.pos=c(0.9,0.95), pdf=FALSE)
z <- Lexis.points(Bdata=GLHS
      , transition="JN"
      , title="Labour market entry by sex and cohort"
      , cov="sex", group="cohort"
      , legend.pos=c(0.9,0.95), pdf=FALSE)

# trying to do something about the embedded theme
z$theme$panel.background$fill <- "white"
z

subjectsID <- c(1,19,46,208) 
title1 <- "Lifelines for selection of respondents." 
z <- Lexislines.episodes(GLHS
                         , D$Depisode
                         , subjectsID
                         , title1)

# reformatting for year data
GLHS.yr <- date_b(Bdata=GLHS
                  , selectday=1
                  , format.out="year")
D <- Biograph.long(GLHS.yr) 
subjects <- c(1,78,120,208) 

z <- Biograph::Lexis.lines(Bdata=GLHS.yr
                 , Dlong=D$Depisode
                 , subjectsID = subjects
                 , title = " ")

w <- LexisOccExp(Bdata=GLHS,transition= "JN"
                 , nyear=5)
# set it back to stop asking
par(ask = FALSE)
levelplot(t(w$nevents)
          , xlab = "Calendar Time"
          , ylab = "Age")

z<- plot(x=occup$state_occup
         , namstates.desired=c("N","J","Censored")
         , colours=c("red","green","lightgrey")
         , title="States occupancies. GLHS"
         , area=TRUE, xmin=10, xmax=55)