library(ggplot2)
library(tidyverse)

#define parameters
K = 100000
r1 = 0.05
r2 = 0.2
t=c(0:140)
N0=10000

# question 1 ####
#1a: generate plot
N.cont.1 = K/(1+(((K-N0)/N0)*exp(-r1*t)))
N.cont.2 = K/(1+(((K-N0)/N0)*exp(-r2*t)))
df.cont = data.frame (t = t, N1 = N.cont.1, N2 = N.cont.2)
plot1 <- ggplot(data=df.cont, aes(x=t, y=N1, color = "r = 0.05"))+geom_line()
plot1 + geom_line(aes(x=t, y=N2, color = "r = 0.2")) + theme_bw() +ylab("N")

#1b: pop size after 10 years
N.1.10 <- N.cont.1[11]
N.2.10<-N.cont.2[11]

#1c: percent K
percent.1 <- (N.1.10/K) *100
percent.2 <- (N.2.10/K) *100

#1d: time to reach 75% of K
Ntarget = K * 0.75
#defining numerator and denominator separately to reduce parentheses confusion
numerator = (K/Ntarget)-1
denominator = (K-N0)/N0
t.target.1 = log(numerator/denominator)/-r1
t.target.2 = log(numerator/denominator)/-r2

#1e: growth rate (dN/dt) at K/2 (max)
N.max.growth = K*0.5
dN.dt.max.1 = r1*N.max.growth*(1-(N.max.growth)/K)
dN.dt.max.2 = r2*N.max.growth*(1-(N.max.growth)/K)

# question 2 ####

r3=3
r4=2
K=1000
N0=100
t=c(0:50)

#generate first data set
N.logist.discr.1 = rep(NA, length=length(t))
N.logist.discr.1[1] = N0

for(i in 1:(length(t)-1))  { 
  N.logist.discr.1[i+1] = N.logist.discr.1[i] + r3*N.logist.discr.1[i] *(1-N.logist.discr.1[i]/K)
}

#generate second data set
N.logist.discr.2 = rep(NA, length=length(t))
N.logist.discr.2[1] = N0

for(i in 1:(length(t)-1))  { 
  N.logist.discr.2[i+1] = N.logist.discr.2[i] + r4*N.logist.discr.2[i] *(1-N.logist.discr.2[i]/K)
}

#2a: plots
df.discr<- data.frame(t=t, N1 = N.logist.discr.1, N2=N.logist.discr.2)
plot2 <- ggplot(data=df.discr, aes(x=t, y=N1, color = "r = 3.0"))+geom_line()
plot2 + geom_line(aes(x=t, y=N2, color = "r = 2.0")) + theme_bw() +ylab("N")

# question 3 ####
N0=2
r=0.1
K=100
t=c(0:100)

#3a: plots
#plot N vs t
N.exp = N0*exp(r*t)
N.log = K/(1+(((K-N0)/N0)*exp(-r*t)))
df.3 = data.frame (t=t, N.exp=N.exp, N.log=N.log)
plot3 <- ggplot(data=df.3, aes(x=t, y=N.exp, color = "Exponential"))+geom_line()
plot3 + geom_line(aes(x=t, y=N.log, color = "Logistic")) + theme_bw() +ylab("N") +ylim(c(0, 500))

#plot dN vs N
dN.dt.log = r*N.log*(1-(N.log)/K)
dN.dt.exp = r*N.exp
df.4 <- cbind(df.3, dN.dt.exp, dN.dt.log)
plot4 <- ggplot(data=df.4, aes(x=N.exp, y=dN.dt.exp, color = "Exponential"))+geom_line()
plot4 + geom_line(aes(x=N.log, y=dN.dt.log, color = "Logistic")) + theme_bw() +
  ylab("dN.dt") +xlab("N")+ylim(c(0,5))+xlim(c(0,110))

#plot dN/N vs N
df.4<- mutate(df.4, dN.dt.exp/N.exp)
df.4<- mutate(df.4, dN.dt.log/N.log)
plot5<- ggplot(data=df.4, aes(x=N.exp, y=dN.dt.exp/N.exp, color="Exponential"))+
  geom_line()+geom_line(aes(x=N.log, y=dN.dt.log/N.log, color="Logistic"))+
  theme_bw()+ylab("dN.dt/N") +xlab("N")+xlim(c(0,110))

# question 5 ####

#5a/5b: plots
K = 1000
sigmar = 0.01
N0=20
t=c(0:120)
r.values= rnorm(t, mean=0.05, sd = sigmar)
N.cont.5 = K/(1+(((K-N0)/N0)*exp(-r.values*t)))
r.values.2= rnorm(t, mean=0.2, sd = sigmar)
N.cont.6 = K/(1+(((K-N0)/N0)*exp(-r.values.2*t)))
df.cont.5 = data.frame (t = t, N5 = N.cont.5, N6 = N.cont.6)
plot1 <- ggplot(data=df.cont.5, aes(x=t, y=N5, color = "rbar = 0.05"))+geom_line()
plot1 + geom_line(aes(x=t, y=N6, color = "rbar = 0.2")) + theme_bw() +ylab("N")

