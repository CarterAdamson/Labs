library(ggplot2)
setwd("/Users/cpadamson/Dropbox/Grad/FISH 558/Lab 07 - Stock-recruitment")


# Question 1 ####
sr.data<-read.csv("SR_data.csv")

#a - fit three models, graph
mod.ind<-nls(log(Recruits)~log(a*Spawners),data=sr.data,start=c(a=2.0),trace=T)
summary(mod.ind)
see.ind<-summary(mod.ind)$sigma
pred.ind<-exp(predict(mod.ind))*exp((see.ind^2)/2)

mod.bh<-nls(log(Recruits)~log(a*Spawners/(1+b*Spawners)), data = sr.data, start=c(a=1.0,b=5e-5), trace=T)
summary(mod.bh)
see.bh<-summary(mod.bh)$sigma
pred.bh<-exp(predict(mod.bh))*exp((see.bh^2)/2)

mod.ric<-nls(log(Recruits)~log(a*Spawners*exp(-b*Spawners)), data=sr.data, start=c(a=1,b=0.00005), trace=T)
summary(mod.ric)
see.ric<-summary(mod.ric)$sigma
pred.ric<-exp(predict(mod.ric))*exp((see.ric^2)/2)

plot.1<-ggplot(data=sr.data, aes(x=Spawners, y=Recruits))+geom_point()+theme_bw()+
  geom_line(aes(y=pred.ind, color = "Density-Independent"))+
  geom_line(aes(y=pred.bh, color = "Beverton-Holt"))+
  geom_line(aes(y=pred.ric, color = "Ricker")) + ylim(0, 1250)
plot.1

#b - diagnostic plots for each
par(mfrow=c(2,2))
res<-resid(mod.ind)
fit<-fitted(mod.ind)

diagnostics = function(mod, xvar, xname){
  par(mfrow=c(2,2))
  r <- resid(mod)
  f <- fitted(mod)
  hist(r,xlab='Residual', prob=TRUE) 
  qqnorm(r)
  qqline(r)
  plot(r~xvar, ylab="Residuals", xlab=xname)
  abline(0,0)
  plot(r~f, ylab = "Residuals", xlab = "Fitted Values")
  abline(0,0)
  par(mfrow=c(1,1))
}

diagnostics(mod.ind, sr.data$Spawners, "Spawners")
diagnostics(mod.bh, sr.data$Spawners, "Spawners")
diagnostics(mod.ric, sr.data$Spawners, "Spawners")

#c - AIC table for each
AICs=c(AIC(mod.ind),AIC(mod.bh),AIC(mod.ric))
dAICs = AICs-min(AICs) 
AICs
dAICs


# Question 5 ####
#prep - create a simulation function for Ricker
RicSim=function(a=3, b=0.0015, e.sd=0.1){ 
  S.sim=seq(0,1500, length=45)
  R.sim = a*S.sim*exp(-b*S.sim)*exp(rnorm(n=length(S.sim),mean=0,sd=e.sd))
  R.sim.noe= a*S.sim*exp(-b*S.sim)
  plot(x=S.sim, y=R.sim, main=paste("a=", a,", b=",b,", e.sd=",e.sd, sep=""))
  lines(S.sim, R.sim.noe)
}

#a - Describe/graph effects of modifying parameters
par(mfrow=c(2,2))
RicSim(a=3, b=0.0015, e.sd=0.1)
RicSim(a=30, b=0.0015, e.sd=0.1)
RicSim(a=3, b=0.015, e.sd=0.1)
RicSim(a=3, b=0.0015, e.sd=1)



