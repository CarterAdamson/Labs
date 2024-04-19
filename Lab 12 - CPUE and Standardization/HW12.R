library(here)
library(ggplot2)
library(tidyverse)
library(lattice)

# QUESTION 1 ####

##a - graphical exploration-----####
#set up data w cpue
croaker <- read.csv(here("Lab 12 - CPUE and Standardization","Croaker catch final.csv"))
croaker$CPUE=croaker$Count/((croaker$TowDist/1000)*(croaker$NetWidth/(0.3048*1000))) 
croaker$logCPUE1<-log(croaker$CPUE+1)

#explore month and region effects -- less useful plots commented out
histogram(~ Count,data=croaker, main="Histogram of Catch")
histogram(~ Count,data=subset(croaker, CPUE>0), main="Histogram of Non-Zero Catch")
#histogram(~ logCPUE1|as.factor(Month),data=croaker)
#histogram(~ logCPUE1|as.factor(Region),data=croaker)
#plot(CPUE~Month, data=croaker)
#plot(CPUE~Region, data=croaker)
#max(croaker$Month) #11
#min(croaker$Month) #3
#histogram(~Month, data=croaker, breaks=8)
#histogram(~Region, data=croaker)
#histogram(~Month|Region, data=croaker)
histogram(~Region|Month, data=croaker)
#coplot(CPUE~Month|Region, croaker)
croaker %>% 
  ggplot(aes(x=Month, y=logCPUE1, group=Month)) + 
  geom_boxplot() +facet_wrap(~Region)
croaker %>% 
  ggplot(aes(x=Region, y=logCPUE1, group=Region)) + 
  geom_boxplot() +facet_wrap(~Month)
#croaker %>% ggplot(aes(x=Month, y=CPUE))+geom_bar(stat="identity")+facet_wrap(~Region)
#croaker %>% ggplot(aes(x=Region, y=CPUE))+geom_bar(stat="identity")+facet_wrap(~Month)

##b - proportions by month and region-----####
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

##d - subset Month and plot new index-----####
##nominal andstrat mean from lab12-----
croak.mean.nominal = croaker %>% group_by(Year) %>% 
  summarize(mean = mean(CPUE),
            var  = var(CPUE),
            sd   = sqrt(var),
            cv   = sqrt(var)/mean,
            n    = length(CPUE),
            se   = sd/sqrt(n))
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

##strat mean with fewer months ----
#include months 5,6,7,9
croak.subset <- filter(croaker,Month>4)
croak.subset <- filter(croak.subset, Month<10)
croak.subset <- filter(croak.subset, Month!=8)
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
croak.indecies <- data.frame(croak.mean.nominal, croak.index, croak.index.2)
#make a ggplot with all three indecies
plot.mean <- ggplot(data=croak.indecies, aes(x=Year, y=mean, colour="Arithmetic Mean"))+
  geom_line(linetype="dashed")+ theme_bw()+labs(y="Croaker index")+
  geom_line(data=croak.indecies, aes(x=Year, y=index, colour = "Stratified Mean (all months)"), linetype="dashed")+
  geom_line(data=croak.indecies, aes(x=Year, y=index.1, colour="Stratified Mean (subset)"), linetype="solid") +
  geom_errorbar(aes(ymin=LCI.1, ymax=UCI.1, color="Stratified Mean (subset)"), width=0.2, linetype="dotted") #error bars if wanted
plot.mean
