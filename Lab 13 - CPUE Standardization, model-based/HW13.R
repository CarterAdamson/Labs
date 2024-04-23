library(here)
library(ggplot2)
library(gridExtra)
library(tidyverse)


# 458 SECTION (extra credit) ####----
pinfish <- read.csv(here("Lab 13 - CPUE Standardization, model-based", "pinfish1992-2006.csv"))
pinfish$CPUE <- pinfish$number
pinfish$year <- as.factor(pinfish$year)
pinfish$veg <- as.factor(pinfish$veg)
pinfish$bot <- as.factor(pinfish$bot)
pinfish$logCPUE <- log(pinfish$CPUE+1)
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
#0:nominal
nominal = pinfish %>% group_by(year) %>% 
  summarize(mean = mean(CPUE),
            var  = var(CPUE),
            cv   = sqrt(var)/mean, #coefficient of variation
            n    = length(CPUE),
            se   = sqrt(var/n),
            LCI  = mean-1.96*se,
            UCI  = mean+1.96*se) 
plot.mean <- ggplot(data=nominal, aes(x=year, y=mean, group=1))+geom_line()+
  geom_errorbar(aes(ymin=LCI, ymax=UCI, width=0.2), linetype=3)+theme_bw()

#1: normal GLM
mod.norm=glm(CPUE~year+veg+bot+depth+sal+temp, data=pinfish, family=gaussian(link='identity'))
summary(mod1)




#diagnostics for normal glm
par(mfrow=c(2,2))
plot(mod.norm, which=c(1:2))
plot(mod.norm$fitted.values, mod.norm$residuals)
abline(h=0, col="red")
pred.norm<-data.frame(year=levels(pinfish$year),veg="Seagrass", bot="Sand",
                      depth=mean(pinfish$depth), sal=mean(pinfish$sal), temp=mean(pinfish$temp))
out.norm<-predict(mod.norm, newdata=pred.norm, type="response",se.fit=T) #bc covariates are held constant, predictions are standardized yearly indices



##you are here
## Summarize info and calculate statistics in yr.mean dataframe 
yr.mean<-as.data.frame(cbind(as.numeric(levels(AC1$year)),out$fit,out$se.fit))
names(yr.mean)<-c("year","mean","SE")
yr.mean$CV<-yr.mean$SE/yr.mean$mean
yr.mean$LCI<-yr.mean[,2]-1.96*yr.mean[,3]  #Lower 95% Confidence Interval (LCI)
yr.mean$UCI<-yr.mean[,2]+1.96*yr.mean[,3]  #Upper 95% Confidence Interval (UCI)

# Plot mean and 95% CIs
plot(yr.mean$mean~yr.mean$year,xlab="Year",ylab="Index (+/- 95%CI)",main="Year Means - Normal Dist",
     ylim=c(min(yr.mean$LCI),max(yr.mean$UCI)),type="b")
segments(x0=yr.mean[,1],x1=yr.mean[,1],y0=yr.mean[,5],y1=yr.mean[,6])
#this is a similar plot to the previous index vs year plot,
#but now the effects of all covariates except year are accounted for
























