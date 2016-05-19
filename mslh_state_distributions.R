library(Biograph)
library(mvna) 
library(etm)
library (msm)
library(Epi)
library(TraMineR)
library(lattice)
library(ggplot2)
data(GLHS)

# state occupancy distribution
occup <- Occup(GLHS)
z <- plot(x=occup$state_occup
          , namstates.desired=c("N","J","Censored")
          , colours=c("red","green","lightgrey")
          , title="States occupancies. GLHS"
          , area=TRUE, xmin=10, xmax=55)

# state distribution
DTraMineR <- seqconc(occup$st_age_1,sep="-") 
namst <- c(Parameters(GLHS)$namstates,"Censored") 
D.seq <- seqdef(DTraMineR)

seqplot(D.seq, type="d"
        , title="State distribution. GLHS"
        , ylab="Count"
        , xtlab=0:54
        , group=GLHS$sex)

# individual state sequences
subjects = c(1,20,208)
seqiplot(D.seq
         , tlim=GLHS$ID %in% subjects
         , ylab = "subjects"
         , yaxis = FALSE
         , withlegend = "right"
         , ltext = namst
         , legend.prop = 0.25
)
axis(side = 2
     , at = c(0.5, 1.75, 3.25)
     , labels = subjects
     , line = 13
     , lty = 0)

# state frequencies
n <- 10
seqfplot(D.seq
         , group=GLHS$sex
         , tlim=1:n
         , title="Sequence frequency plot"
         , xtlab=c(0:54)
         , ltext= rev(namst) #why???
         , las=1
         , ylab=paste(n
                      , " most frequent sequences (%)"
                      ,sep="") )