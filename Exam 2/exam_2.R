library(ggplot2)
library(here)
library(fishmethods)

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
            theme_bw()+labs(x="Spawners", y="Recruits")+
            geom_line(aes(y=pred.sr.ind,color="Density Independent"))+
            geom_line(aes(y=pred.sr.bh,color="Beverton-Holt"))+
            geom_line(aes(y=pred.sr.ric,color="Ricker"))
plot.sr


## b - table of a and b parameters, AIC, dAIC ####

#assemble table
m <- c("Density Independent", "Beverton-Holt", "Ricker")
a <- c(0.3797, 0.9522, 0.7251)
b <- c("N/A", 0.0101, 0.0039)
AIC <- c(AIC(sr.ind),AIC(sr.bh),AIC(sr.ric))
dAIC <- AIC - min(AIC)
table.2b <- data.frame(Model=m, a=a, b=b, AIC=AIC, dAIC=dAIC)
table.2b

## c - diagnostic plots (normality, HOV) ####

#plot 3x2 grid of diagnosic plots
par(mfrow=c(3,2))
hist(resid(sr.ind), xlab="Residual", main = "Density Independent")
plot(resid(sr.ind)~fitted(sr.ind), main = "Density Independent")
abline(h=0, lty=2)
hist(resid(sr.bh), xlab="Residual", main = "Beverton-Holt")
plot(resid(sr.bh)~fitted(sr.bh), main = "Beverton-Holt")
abline(h=0, lty=2)
hist(resid(sr.ric), xlab="Residual", main = "Ricker")
plot(resid(sr.ric)~fitted(sr.ric), main = "Ricker")
abline(h=0, lty=2)

## d - which model is best? ####



# QUESTION 3 ####

trout <- read.csv(here("Exam 2", "Exam_2_trout data.csv"))
par(mfrow=c(1,1))

## a - YPR analysis to find Fmax and F0.1 ####

#YPR analysis
trout.ypr <- ypr(age=trout$age,wgt=trout$weight,partial=trout$selectivity, M=0.26,
              plus=FALSE,maxF=2,incrF=0.01, graph=TRUE)
trout.ypr$Reference_Points

## b - SBPR analysis for F30 and F40 ####

#SBPR analyses for each reference point
trout.sbpr.30 <- sbpr(age=trout$age, ssbwgt=trout$weight, partial=trout$selectivity,
                   pmat=trout$maturity, M=0.26, pF=0.5, pM=0.5, MSP=30,
                   plus=FALSE, maxF=2, incrF=0.01, graph=TRUE)
trout.sbpr.30$Reference_Point
trout.sbpr.40 <- sbpr(age=trout$age, ssbwgt=trout$weight, partial=trout$selectivity,
                      pmat=trout$maturity, M=0.26, pF=0.5, pM=0.5, MSP=40,
                      plus=FALSE, maxF=2, incrF=0.01, graph=TRUE)
trout.sbpr.40$Reference_Point

## c - graph YPR vs F, including all four reference points ####

plot.3.data<-data.frame(F=trout.ypr$F_vs_YPR$F, YPR=trout.ypr$F_vs_YPR$YPR,
                         SPR=trout.sbpr.30$F_vs_SPR$PSPR)
#basic graph
plot.3c <- ggplot(data=plot.3.data, aes(x=F, y=YPR))+geom_line()+
              theme_bw()+ labs(colour="Reference Point")+
              geom_vline(aes(xintercept=0.94, color="Fmax"), linetype="dashed")+
              geom_vline(aes(xintercept=0.5383887, color="F0.1"), linetype="dashed")+
              geom_vline(aes(xintercept=0.5808088, color="F_30%"), linetype="dashed")+
              geom_vline(aes(xintercept=0.4266088, color="F_40%"), linetype="dashed")
#add points
F <- c(0.94, 0.5383887, 0.5808088, 0.426088)
YPR <- c(0.1668414, 0.1564271, 0.159070255, 0.146347072) #kg
color <- c("Fmax", "F0.1","F_30%","F_40%")
points.df <- data.frame(F=F, YPR = YPR, color = color)
plot.3c <- plot.3c + geom_point(data=points.df, aes(color=color))
plot.3c

## d - graph % max SPR with all four reference points ####
plot.3d <- ggplot(data=plot.3.data, aes(x=F, y=SPR))+geom_line()+
  theme_bw()+ labs(y="% Max SPR", colour="Reference Point")+
  geom_vline(aes(xintercept=0.94, color="Fmax"), linetype="dashed")+
  geom_vline(aes(xintercept=0.5383887, color="F0.1"), linetype="dashed")+
  geom_vline(aes(xintercept=0.5808088, color="F_30%"), linetype="dashed")+
  geom_vline(aes(xintercept=0.4266088, color="F_40%"), linetype="dashed")
#add points
F <- c(0.94, 0.5383887, 0.5808088, 0.426088)
SPR <- c(16.772855, 32.299491, 30.043468, 39.737697) #%
color <- c("Fmax", "F0.1","F_30%","F_40%")
points.df <- data.frame(F=F.vector, YPR = YPR.vector, color = color.vector)
plot.3d <- plot.3d + geom_point(data=points.df, aes(color=color))
plot.3d

## e - table to summarize results for all reference points ####
ref <- c("Fmax", "F0.1", "F_30%", "F_40%")
u <- 1-exp(-F)
YPR <- c(0.1668414, 0.1564271, 0.159070255, 0.146347072) #kg
Percent_max_YPR <- (YPR/0.1668414)*100
SPR <- c(0.13506823,0.26010092,0.24193365,0.31999921) #kg
Percent_max_SPR <- c(16.772855, 32.299491, 30.043468, 39.737697)
table.3e <- data.frame(Reference_point=ref, F=F, u=u, YPR=YPR,
                       Percent_max_YPR=Percent_max_YPR, SPR=SPR,
                       Percent_max_SPR=Percent_max_SPR)
table.3e

## f - tradeoffs, recommend a reference point


# QUESTION 4 ####

# QUESTION 5 ####
#1 hr so far
#3 more hrs as of 9:30
#+2
