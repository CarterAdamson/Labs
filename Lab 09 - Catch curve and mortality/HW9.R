setwd("/Users/cpadamson/Dropbox/Grad/FISH 558/Lab 09 - Catch curve and mortality")
library(ggplot2)
library(FSAdata)
library(FSA)


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

#b - Z estimates using basic regression and Chap-Rob method
cc.basic <- catchCurve(number~age,data=cat,ages2use=4:14) #peak occurs at age 4, age 18 outlier must be excluded b/c 0s break it
summary(cc.basic)
cbind(Est=coef(cc.basic),confint(cc.basic))
plot(cc.basic, main="Basic Catch Curve")

cc1.chaprob <- chapmanRobson(number~age,data=CA,ages2use=4:10)
summary(cc1.chapman)
cbind(Est=coef(cc1.chapman),confint(cc1.chapman))
par(mar=c(5.1,4.1,4.1,2.1)) #reset figure margins, b/c otherwise can lead to errors with the plot function below...
plot(cc1.chapman, main="Chapman Robson method")