library(here)
library(ggplot2)
library(tidyverse)
library(lattice)

# QUESTION 1

#a - graphical exploration
#set up data w cpue
croaker <- read.csv(here("Lab 12 - CPUE and Standardization","Croaker catch final.csv"))
croaker$CPUE=croaker$Count/((croaker$TowDist/1000)*(croaker$NetWidth/(0.3048*1000))) 
croaker$logCPUE1<-log(croaker$CPUE+1)

#explore month and region effects
histogram(~ Count,data=croaker)
histogram(~ Count,data=subset(croaker, CPUE>0))

histogram(~ logCPUE1|as.factor(Month),data=croaker)
histogram(~ logCPUE1|as.factor(Region),data=croaker)

plot(CPUE~Month, data=croaker)
plot(CPUE~Region, data=croaker)

max(croaker$Month) #11
min(croaker$Month) #3
histogram(~Month, data=croaker, breaks=8)
histogram(~Region, data=croaker)
histogram(~Month|Region, data=croaker) # none in month 8(7?) except for region 5
histogram(~Region|Month, data=croaker)
coplot(CPUE~Month|Region, croaker)

croaker %>% 
  ggplot(aes(x=Month, y=logCPUE1, group=Month)) + 
  geom_boxplot() +facet_wrap(~Region)
croaker %>% 
  ggplot(aes(x=Region, y=logCPUE1, group=Region)) + 
  geom_boxplot() +facet_wrap(~Month)

croaker %>% ggplot(aes(x=Month, y=CPUE))+geom_bar(stat="identity")+facet_wrap(~Region)
croaker %>% ggplot(aes(x=Region, y=CPUE))+geom_bar(stat="identity")+facet_wrap(~Month)

#proportions by month and region-----####
croakprop <- croaker %>%
  group_by(Month, Region) %>%
  summarise(ntot=n(), non_zero= sum(Count != 0)) %>% 
  mutate(catchprop=non_zero/ntot)

croakprop %>% ggplot(aes(x=Month, y=catchprop))+geom_bar(stat="identity")+facet_wrap(~Region)
croakprop %>% ggplot(aes(x=Region, y=catchprop))+geom_bar(stat="identity")+facet_wrap(~Month)

plot(catchprop~Month, data=croakprop)
plot(catchprop~Region, data=croakprop)

croakprop %>% 
  ggplot(aes(x=Month, y=catchprop, group=Month)) + 
  geom_boxplot() +facet_wrap(~Region)
croakprop %>% 
  ggplot(aes(x=Region, y=catchprop, group=Region)) + 
  geom_boxplot() +facet_wrap(~Month) #because there is only one data pt for each month/region, the boxplots are the same as bar graphs

#nominal andstrat mean from lab12-----####
croak.mean.nominal = croaker %>% group_by(Year) %>% 
  summarize(mean = mean(CPUE),
            var  = var(CPUE),
            sd   = sqrt(var),
            cv   = sqrt(var)/mean,
            n    = length(CPUE),
            se   = sd/sqrt(n))
croak.mean.nominal
croak.mean = croaker %>% group_by(Year, StratNum) %>% 
  summarize(strat.mean = mean(CPUE),
            strat.var  = var(CPUE),
            strat.n    = length(CPUE))
W=read.csv(here("Lab 12 - CPUE and Standardization", "CM weights_new.csv"))
croak.mean = left_join(croak.mean, W, by="StratNum")
croak.mean <- croak.mean %>% 
  mutate(mean.calc = strat.mean * Weight, 
         var.calc = Weight^2*(TotalN-strat.n)/(TotalN-1)*strat.var)
croak.index = croak.mean %>% group_by(Year) %>% 
  summarise(index = sum(mean.calc),
            var   = sum(var.calc),
            se    = sqrt(var),
            cv    = sqrt(var)/index,
            LCI   = index-1.96*se,
            UCI   = index+1.96*se)  
croak.index
par(mfrow=c(1,1))
# Plot mean and SE
plot(index~Year, data=croak.index, xlab="Year",ylab="Index",main="Croaker Indices (+/- SE)",
     ylim=c(min(index-se),max(index+se)),type="b")
lines(croak.mean.nominal$Year, croak.mean.nominal$mean, lty=2, col="red")
with(croak.index,  #the with(X, ...) allows you to refer to columns within the X dataframe, similar to when you "attach()" a dataframe
     segments(x0=Year,x1=Year, y0=index-se, y1=index+se) 
)
legend("topright",c("Stratified Mean","Arithmetic Mean"),col=c('black','red'),lty=c(1,2))

#strat mean from with fewer months -----####
#include months 5,6,7,9
croak.subset <- filter(croaker,Month>4)
croak.subset <- filter(croak.subset, Month<10)
croak.subset <- croak.subset <- filter(croak.subset, Month!=8)

croak.mean.strat <-  croak.subset %>% group_by(Year, StratNum) %>% 
  summarize(strat.mean = mean(CPUE),
            strat.var  = var(CPUE),
            strat.n    = length(CPUE),
            .groups="keep")
croak.mean.strat = left_join(croak.mean.strat, W, by="StratNum")
croak.mean.strat <- croak.mean.strat %>% 
  mutate(mean.calc = strat.mean * Weight, 
         var.calc = Weight^2*(TotalN-strat.n)/(TotalN-1)*strat.var)
croak.index.2 = croak.mean.strat %>% group_by(Year) %>% 
  summarise(index = sum(mean.calc),
            var   = sum(var.calc),
            se    = sqrt(var),
            cv    = sqrt(var)/index,
            LCI   = index-1.96*se,
            UCI   = index+1.96*se)  
croak.index.2

#make a ggplot with all three (croak.mean.nominal, croak.index, croak.index.2)
#dotted lines for the old two, solid line for the new one



#non ggplot version:
par(mfrow=c(1,1))
# Plot mean and SE
plot(index~Year, data=croak.index, xlab="Year",ylab="Index",main="Croaker Indices (+/- SE)",
     ylim=c(min(index-se),max(index+se)),type="b")
lines(croak.mean.nominal$Year, croak.mean.nominal$mean, lty=2, col="red")
with(croak.index,  #the with(X, ...) allows you to refer to columns within the X dataframe, similar to when you "attach()" a dataframe
     segments(x0=Year,x1=Year, y0=index-se, y1=index+se) 
)
legend("topright",c("Stratified Mean","Arithmetic Mean"),col=c('black','red'),lty=c(1,2))



