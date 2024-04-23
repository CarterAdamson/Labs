library(here)
library(ggplot2)
library(gridExtra)
library(countreg)
library(topmodels)
library(tidyverse)


# 458 SECTION (extra credit) ####----
pinfish <- read.csv(here("Lab 13 - CPUE Standardization, model-based", "pinfish1992-2006.csv"))
pinfish$CPUE <- pinfish$number
pinfish$year <- as.factor(pinfish$year)
pinfish$veg <- as.factor(pinfish$veg)
pinfish$bot <- as.factor(pinfish$bot)
pinfish$logCPUE <- log(pinfish$CPUE+1)
pinfish$CPUE1 <- pinfish$CPUE+1
pinfish.pos <- subset(pinfish, CPUE>0)

## a - exploratory plots ####----
#CPUE histograms
par(mfrow=c(2,1))
hist(pinfish$CPUE, main="CPUE")
hist(pinfish.pos$CPUE, main ="CPUE>0")

#covariate effects on CPUE
p1 <- pinfish %>% ggplot(aes(x=year, y=logCPUE))+ggtitle("Year")+geom_boxplot()+labs(y="log(CPUE+1)")
p2 <- pinfish %>% ggplot(aes(x=veg, y=logCPUE))+ggtitle("Vegetation")+geom_boxplot()+labs(y="log(CPUE+1)")
p3 <- pinfish %>% ggplot(aes(x=bot, y=logCPUE))+ggtitle("Bottom Substrate")+geom_boxplot()+labs(y="log(CPUE+1)")
p4 <- pinfish %>% ggplot(aes(x=depth, y=logCPUE))+ggtitle("Depth (m)")+geom_point()+labs(y="log(CPUE+1)") #units not specified. Guessing m, since most frequen in 16 to 32 inch of water according to a source
p5 <- pinfish %>% ggplot(aes(x=sal, y=logCPUE))+ggtitle("Salinity (ppt)")+geom_point()+labs(y="log(CPUE+1)")
p6 <- pinfish %>% ggplot(aes(x=temp, y=logCPUE))+ggtitle("Temperature (C)")+geom_point()+labs(y="log(CPUE+1)")
grid.arrange(p1, p2, p3, p4, p5, p6) #combine all covariate effects onto one grid of plots


## b - mean CPUE & indices ####----
###0:nominal----
#summary stats by year
nominal = pinfish %>% group_by(year) %>% 
  summarize(mean = mean(CPUE),
            var  = var(CPUE),
            cv   = sqrt(var)/mean, #coefficient of variation
            n    = length(CPUE),
            se   = sqrt(var/n),
            LCI  = mean-1.96*se,
            UCI  = mean+1.96*se) 


###1: normal GLM----
mod.norm=glm(CPUE~year+veg+bot+depth+sal+temp, data=pinfish, family=gaussian(link='identity'))
#deviance explained
dev.norm = (mod.norm$null.deviance - mod.norm$deviance) / mod.norm$null.deviance
#diagnostic plots
par(mfrow=c(1,2))
plot(mod.norm, which=c(1:2))
#standardized predictions
pred.data<-data.frame(year=levels(pinfish$year),veg="Seagrass", bot="Sand",
                      depth=mean(pinfish$depth), sal=mean(pinfish$sal), temp=mean(pinfish$temp))
out.norm<-predict(mod.norm, newdata=pred.data, type="response",se.fit=T) #bc covariates are held constant, predictions are standardized yearly indices
#data frame with yearly predictions and summary stats
summ.norm<-as.data.frame(cbind(as.numeric(levels(pinfish$year)),out.norm$fit,out.norm$se.fit))
names(summ.norm)<-c("year","mean","SE")
summ.norm$CV<-summ.norm$SE/summ.norm$mean
summ.norm$LCI<-summ.norm[,2]-1.96*summ.norm[,3]  #Lower 95% Confidence Interval (LCI)
summ.norm$UCI<-summ.norm[,2]+1.96*summ.norm[,3]  #Upper 95% Confidence Interval (UCI)

###2: lognormal GLM----
mod.lognorm=glm(logCPUE~year+veg+bot+depth+sal+temp, data=pinfish,family=gaussian(link='identity'))
#deviance explained
dev.lognorm = (mod.lognorm$null.deviance - mod.lognorm$deviance) / mod.lognorm$null.deviance
#diagnostic plots
plot(mod.lognorm, which=c(1,2)) #the straight line here represents CPUE = 0 (log 0 is 1)
#import and run bias correction function, then add summary stats
lnorm.bias.cor=dget(here("Lab 13 - CPUE Standardization, model-based", "lnormBC.r"))  
summ.cor=lnorm.bias.cor(mod.lognorm)
summ.lognorm <- cbind(as.numeric(levels(pinfish$year)), summ.cor)
names(summ.lognorm) <- c("year", "mean", "SE")
summ.lognorm$CV<-summ.cor[,2]/summ.cor[,1] 
summ.lognorm$LCI<-summ.cor[,1]-1.96*summ.cor[,2] 
summ.lognorm$UCI<-summ.cor[,1]+1.96*summ.cor[,2] 

###3: gamma GLM----
mod.gamma=glm(CPUE1~year+veg+bot+depth+sal+temp, data=pinfish,family=Gamma(link='inverse'))
#deviance explained
dev.gamma = (mod.gamma$null.deviance - mod.gamma$deviance) / mod.gamma$null.deviance
#diagnostic plot (limited usefulness)
par(mfrow=c(1,1))
plot(mod.gamma, which=c(1))
#predictions
out.gamma<-predict(mod.gamma, newdata=pred.data, type="response",se.fit=T)
#summary data frame
summ.gamma<-as.data.frame(cbind(as.numeric(levels(pinfish$year)),out.gamma$fit,out.gamma$se.fit))
names(summ.gamma)<-c("year","mean","SE")
summ.gamma$CV<-summ.gamma$SE/summ.gamma$mean
summ.gamma$LCI<-summ.gamma[,2]-1.96*summ.gamma[,3]
summ.gamma$UCI<-summ.gamma[,2]+1.96*summ.gamma[,3]

###4: poisson GLM----
mod.poisson=glm(CPUE~year+veg+bot+depth+sal+temp, data=pinfish, family=poisson)
#deviance explained
dev.poisson = (mod.poisson$null.deviance - mod.poisson$deviance) / mod.poisson$null.deviance
#diagnostic: rootogram
breaks = seq(0, 300, by=5)
rootogram(mod.poisson, breaks=breaks)
#predictions
out.poisson<-predict(mod.poisson, newdata=pred.data, type="response", se.fit=T)
#summary data frame
summ.poisson <- as.data.frame(cbind(as.numeric(levels(pinfish$year)),out.poisson$fit, out.poisson$se.fit))
names(summ.poisson) <- c("year", "mean", "SE")
summ.poisson$CV<-summ.poisson$SE/summ.poisson$mean
summ.poisson$LCI<-summ.poisson[,2]-1.96*summ.poisson[,3]
summ.poisson$UCI<-summ.poisson[,2]+1.96*summ.poisson[,3]

###5: nb GLM----
mod.nb=glm.nb(CPUE~year+veg+bot+depth+sal+temp, data=pinfish, link="log")
#deviance explained
dev.nb = (mod.nb$null.deviance - mod.nb$deviance) / mod.nb$null.deviance
#diagnostic: rootogram
rootogram(mod.nb, breaks=breaks)
#predictions
out.nb<-predict(mod.nb, newdata=pred.data, type="response", se.fit=T)
#summary data frame
summ.nb <- as.data.frame(cbind(as.numeric(levels(pinfish$year)),out.nb$fit, out.nb$se.fit))
names(summ.nb) <- c("year", "mean", "SE")
summ.nb$CV<-summ.nb$SE/summ.nb$mean
summ.nb$LCI<-summ.nb[,2]-1.96*summ.nb[,3]
summ.nb$UCI<-summ.nb[,2]+1.96*summ.nb[,3]


##c - multipanel plot ####----
plot.nom <- ggplot(data=nominal, aes(x=year, y=mean, group=1))+geom_line()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI, width=0.1), linetype=2)+
  theme_bw()+ggtitle("Nominal")
plot.norm <- ggplot(data=summ.norm, aes(x=year, y=mean, group=1))+geom_line()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI, width=0.1), linetype=2)+
  theme_bw()+ggtitle("Normal GLM")
plot.lognorm <- ggplot(data=summ.lognorm, aes(x=year, y=mean, group=1))+geom_line()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI, width=0.1), linetype=2)+
  theme_bw()+ggtitle("Lognormal GLM")
plot.gamma <- ggplot(data=summ.gamma, aes(x=year, y=mean, group=1))+geom_line()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI, width=0.1), linetype=2)+
  theme_bw()+ggtitle("Gamma GLM")
plot.poisson <- ggplot(data=summ.poisson, aes(x=year, y=mean, group=1))+geom_line()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI, width=0.1), linetype=2)+
  theme_bw()+ggtitle("Poisson GLM")
plot.nb <- ggplot(data=summ.nb, aes(x=year, y=mean, group=1))+geom_line()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI, width=0.1), linetype=2)+
  theme_bw()+ggtitle("NB GLM")
grid.arrange(plot.nom, plot.norm, plot.lognorm, plot.gamma, plot.poisson, plot.nb)

##d - overlaid plot ####----
#assemble data for easier plotting
#standardizing by dividing each index by its mean
plot.d.data <- as.data.frame(cbind(as.numeric(levels(pinfish$year)), nominal$mean/mean(nominal$mean),
                                   summ.norm$mean/mean(summ.norm$mean),summ.lognorm$mean/mean(summ.lognorm$mean),
                                   summ.gamma$mean/mean(summ.gamma$mean),summ.poisson$mean/mean(summ.poisson$mean),
                                   summ.nb$mean/mean(summ.nb$mean)))
names(plot.d.data) <- c("year","nom", "norm", "lognorm","gamma","poisson", "nb")
palette <- c( "#FF0033", "#33CC33", "#FFCC00", "#66CCCC", "#FF99FF", "#9900FF")
plot.d.data %>% ggplot(aes(x=year, y=nom, color="Nominal Means"))+geom_line()+
  geom_line(aes(y=norm, color="Normal GLM"))+
  geom_line(aes(y=lognorm, color="Lognormal GLM"))+
  geom_line(aes(y=gamma, color="Gamma GLM"))+
  geom_line(aes(y=poisson, color="Poisson GLM"))+
  geom_line(aes(y=nb, color="Negative Binomial GLM"))+
  theme_bw()+labs(x="Year", y="Pinfish Index", color="Index Type")+
  scale_color_manual(values=palette)











