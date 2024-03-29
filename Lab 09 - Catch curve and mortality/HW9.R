setwd("~/Dropbox/Grad/FISH 558/Labs/Lab 09 - Catch curve and mortality")
library(ggplot2)
library(FSAdata)
library(FSA)
library(tidyverse)

# Question 2 ####

#a - choose dataset
data("FHCatfishATL")
cat<-data.frame(age=seq(0, max(FHCatfishATL$age), by=1), number=rep(0, max(FHCatfishATL$age)+1))
#some rivers are data-poor. Decided to combine the three populations and analyze together
#combines numbers for each age across the three rivers
for(i in 1:length(cat$number)){
  for(j in 1:length(FHCatfishATL$age)){
  if(FHCatfishATL$age[j]==cat$age[i]){
    cat$number[i]<-cat$number[i]+FHCatfishATL$number[j]
  }
}
}
#zeroes ruin the catchCurve calculations
cat.short <- cat %>% slice(1:15)

#b - Z estimates using basic regression and Chap-Rob method
cc.basic <- catchCurve(number~age,data=cat.short,ages2use=4:14) #peak occurs at age 4, age 18 outlier must be excluded b/c 0s break it
summary(cc.basic)
confint(cc.basic)
cbind(Est=coef(cc.basic),confint(cc.basic))

cc.chaprob <- chapmanRobson(number~age,data=cat,ages2use=4:17)
summary(cc.chaprob)
cbind(Est=coef(cc.chaprob),confint(cc.chaprob))

plot(cc.basic, main="Basic Catch Curve")

# Question 3 ####

#b - M and v table
#data: threespine stickleback, Kiel Bay, Germany
Linf <- 6.9  #from von bertalanffy model
K <- 1.79     #k from von bertalanffy model
t0 <- -0.28    #t0 from the von bertalanffy model
tmax <- t0+3/K  # The maximum age for the population of fish
T <- 8.2
name <- c("HoenigNLS","PaulyLNoT", "PaulyL", "HoenigO","HoenigLM","HewittHoenig","tmax1",
          "K1", "JensenK1","AlversonCarney")
M <- c(metaM("HoenigNLS", tmax=tmax), metaM("PaulyLNoT", K=K, Linf=Linf), metaM("PaulyL", Linf=Linf, K=K, T=T),
       metaM("HoenigO", tmax=tmax), metaM("HoenigLM", tmax=tmax), metaM("HewittHoenig", tmax=tmax), 
      metaM("tmax1", tmax=tmax), metaM("K1", K=K), metaM("JensenK1", K=K),
      metaM("AlversonCarney", tmax=tmax, K=K))
v <- 1-exp(-M)
ybh<-data.frame(name, M, v)

#c - summary M and v
mean <- c(mean(M), mean(v))
SD <- c(sd(M), sd(v))
min <- c(min(M), min(v))
max<- c(max(M), max(v))
ybh.summary<-data.frame(mean, SD, min, max)
rownames(ybh.summary)<-c("M", "v")

#d - one method, different studies
#method: K1
#M and v table
Population <- c("Roscoff, France (M)", "Roscoff, France (F)", "Navarro River (inland), California",
                "Navarro River (upstream), California", "Navarro River (estuary), California", "Ooster Schelde, Netherlands",
                "Cheshire, UK", "Kiel Bay, Germany", "Kandalaksha Bay, Russia (M)", "Kandalaksha Bay, Russia (F)")
K <- c(4.2, 2.4, 2.09, 1.77, 1.78, 2.32, 0.64, 1.79, 0.57, 0.67)
M<- rep(0, 10)
for(i in 1:length(K)){
M[i]<-c(metaM("K1", K=K[i]))
} #warning for first iteration: "K value seems unreasonable", related to Males in Roscoff, France
v <-1-exp(-M)
sticky<-data.frame(Population, K, M, v)
#sumamry M and v
mean <- c(mean(M), mean(v))
SD <- c(sd(M), sd(v))
min <- c(min(M), min(v))
max<- c(max(M), max(v))
sticky.summary<-data.frame(mean, SD, min, max)
rownames(ybh.summary)<-c("M", "v")




