library(ggplot2)
library(here)

# QUESTION 1 ####

## a - Conclusions from Table 1 ####

## b - Relate AIC, dAIC, and Weights ####

## c - Make conclusions from figure, relate to table ####

## d - do the results indicate how occupancy changes w disturbance history? ####



# QUESTION 2 ####
sardine <- read.csv(here("Exam 2","Exam_2_SRdata.csv"))

## a - stock-recruitment plot with three models ####

#make density independent model
sr.ind <- nls(log(R)~log(a*S), data=sardine, start=c(a=.2), trace=T)
SEE.sr.ind <- summary(sr.ind)$sigma
pred.sr.ind <- exp(predict(sr.ind))*exp((SEE.sr.ind^2)/2)

#make B-H model
sr.bh <- nls(log(R)~log(a*S/(1+b*S)), data=sardine,start=c(a=1.0,b=5e-5), trace=T)
SEE.sr.bh <- summary(sr.bh)$sigma
pred.sr.bh <- exp(predict(sr.bh))*exp((SEE.sr.bh^2)/2)

#make Ricker model
# log(R)=log(a)+log(S)-b*S+e
sr.ric <- nls(log(R)~log(a)+log(S)-(b*S), start=c(a = 1.0, b=5e-5), data=sardine,trace=T)
SEE.sr.ric <- summary(sr.ric)$sigma
pred.sr.ric <- exp(predict(sr.ric))*exp((SEE.sr.ric^2)/2)

#plot
plot.sr <- ggplot(data=sardine, aes(x=S, y=R))+geom_point()+
            theme_bw()+labs(x="Spanwers", y="Recruits")+
            geom_line(aes(y=pred.sr.ind,color="Density Independent"))+
            geom_line(aes(y=pred.sr.bh,color="Beverton-Holt"))+
            geom_line(aes(y=pred.sr.ric,color="Ricker"))
plot.sr


## b - table of a and b parameters, AIC, dAIC ####

## c - diagnostic plots (normality, HOV) ####

## d - which model is best? ####



# QUESTION 3 ####

## a - YPR analysis to find Fmax and F0.1 ####

## b - SBPR analysis for F30 and F40 ####

## c - graph YPR vs F, including all four reference points ####

## d - graph % max SPR with all four reference points ####

## e - table to summarize results for all reference points ####

## f - tradeoffs, recommend a reference point


# QUESTION 4 ####

# QUESTION 5 ####
#1 hr so far
