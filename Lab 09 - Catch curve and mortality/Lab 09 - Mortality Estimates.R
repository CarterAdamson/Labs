#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# FISH 458/558 - Fish Population Dynamics 
# Lab 9 - Catch curves and related methods
# A. Buchheister 
# March 2024
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


### Today
# - Stock Recruitment HW feedback
#   - Sort data by Spawners to avoid odd-looking plots
# - YPR/SPR Homework - questions?
#   - Annotate your Scripts
#   - Comment on the environment; don't save workspace image...
# - Student presentation 
# - Mortality Lab 
#   - Manual Catch Curve
#   - Figure out how to do catch curve with FSA package (work in small groups then discuss as class)
#   - Figure out how to get M estimates with FSA package (work in small groups then discuss as class)




### Examples of catch curve datasets (this will be needed for the homework):
library(FSA)
library(FSAdata)
help.search("Catch curve",package=c("FSAdata","FSA")) #Search for datasets and functions pertaining to catch curves the listed packages
  #How to import a dataset (after you find a dataset name):
  data(BrookTroutTH)
  ?BrookTroutTH
  str(BrookTroutTH)
  head(BrookTroutTH)
  
  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Conducting a manual catch curve -----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  
### Import data for a Catch curve example:

  #setwd("C:/Users/ab4577/HSU/Teaching/PopDy/2021_Spring/Labs/Lab 08 - Catch Curve, Mortality")
  CA=read.csv("Catch_at_age.csv")
  head(CA)
  
  CA$ln_num = log(CA$number)
  
  #Plots
  plot(number~age, data=CA, pch=16)
  plot(ln_num~age, data=CA, pch=16)

#Trim data to exclude ages that aren't fully selected (using all ages>peak)
CA.trim = subset(CA, age>3)

#Fit regression, look at diagnostics, and extract slope coefficient
mod1 = lm(ln_num~age, data=CA.trim)
summary(mod1)
plot(mod1, which=1) #one of the automatic residual vs. fitted plots
plot(mod1, which=2) #qq plot

Z = -mod1$coefficients[2]
Z

Z.se = summary(mod1)$coefficients[2,"Std. Error"]

#You could create a nice plot like those we've seen in class by modifying a plot like this:
plot(ln_num~age, data=CA, ylab = "log(catch)")
abline(mod1)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Estimates of Z using catchCurve() from the FSA package. ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#See "CatchCurve vignette.pdf"
library(FSA)
CA
?catchCurve

#Linear regression method
#Code copied and modified from FSA catchCurve() help page
  cc1 <- catchCurve(number~age,data=CA,ages2use=4:10)
  summary(cc1)
  cbind(Est=coef(cc1),confint(cc1))
  plot(cc1, main="Basic Catch Curve")

#Weighted Catch Curve method
  cc1.wtd <- catchCurve(number~age,data=CA,ages2use=4:10, weighted=T)
  summary(cc1.wtd)
  cbind(Est=coef(cc1.wtd),confint(cc1.wtd))
  plot(cc1.wtd, main="Weighted Catch Curve")

#Chapman Robson estimator
  cc1.chapman <- chapmanRobson(number~age,data=CA,ages2use=4:10)
  summary(cc1.chapman)
  cbind(Est=coef(cc1.chapman),confint(cc1.chapman))
  par(mar=c(5.1,4.1,4.1,2.1)) #reset figure margins, b/c otherwise can lead to errors with the plot function below...
  plot(cc1.chapman, main="Chapman Robson method")

#Note: if you get "Error in plot.new() : figure margins too large", 
#you should reset the margins to reasonable values, using: par(mar=c(5.1,4.1,4.1,2.1))

  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# EXTRA: Z Estimates using agesurv() from fishmethods package -----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#This section of code uses a different package 
#This is extra material.
  
library(fishmethods)

##Example: Run through canned example from the package
data(rockbass)
?rockbass
head(rockbass)
agesurv(age=rockbass$age,full=6)

## Use the function for our "CA" data: 
  #Reformat the catch at age data into the format needed by the agesurv() function
  CA.long = rep(CA$age, CA$number)
  
  #Get mortality estimates using multiple methods
  agesurv(age=CA.long, full=4, estimate="z")
  
  #Store results in dataframe
  CA.out = agesurv(age=CA.long, full=4, estimate="z")
  CA.results = CA.out$results 
  CA.results

#Note that the Catch curve estimates we calculated manually match the agesurv() function.
#But, now we can compare the estimates derived using different methods.

##NOTE:
# See also the catchCurve() function in the FSA package.



  
  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Estimate of Natural Mortality using Empirical Relationships ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#use the Mmethods() and metaM() functions in the FSA package.  read help menu.
library(FSA)
?Mmethods

## List names for available methods
Mmethods()
Mmethods("tmax") #gives all methods that use "tmax" (max age) data
Mmethods("K") #gives all methods that use "K" data

## Simple Examples
metaM("tmax",tmax=20)
metaM("tmax",tmax=20,justM=FALSE)
metaM("HoenigNLS",tmax=20)
metaM("HoenigNLS",tmax=20,justM=FALSE)
 
## Example with Patagonian Sprat ... from Table 2 in Cerna et al. (2014)
## http://www.scielo.cl/pdf/lajar/v42n3/art15.pdf
T <- 11   #temperature experienced by fish in Celsius
Linf <- 17.71  #from von bertalanffy model
K <- 0.78      #k from von bertalanffy model
t0 <- -0.46    #t0 from the von bertalanffy model
tmax <- t0+3/K  # The maximum age for the population of fish
t50 <- t0-(1/K)*log(1-13.5/Linf)  #The age (time) when half the fish in the population are mature.
metaM("RikhterEfanov1",t50=t50)
metaM("PaulyL",K=K,Linf=Linf,T=T)
metaM("PaulyL",K=K,Linf=Linf,T=T,justM=FALSE)
metaM("HoenigNLS",tmax=tmax)
metaM("HoenigO",tmax=tmax)
metaM("HewittHoenig",tmax=tmax)
metaM("AlversonCarney",K=K,tmax=tmax)

## Example of multiple calculations
metaM(c("RikhterEfanov1","PaulyL","HoenigO","HewittHoenig","AlversonCarney"),
     K=K,Linf=Linf,T=T,tmax=tmax,t50=t50)

## Example of multiple methods using Mmethods
# select some methods
metaM(Mmethods()[-c(15,20,22:24,26)],K=K,Linf=Linf,T=T,tmax=tmax,t50=t50)
# select just the Hoenig methods
metaM(Mmethods("Hoenig"),K=K,Linf=Linf,T=T,tmax=tmax,t50=t50)
 

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Obtaining life history data from fishbase for HW (for estimating M) -------
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# go to http://www.fishbase.org
# Search for your species of interest
# If you get multiple hits with the same common name and Species, click on one of the common names (which is a link)
# Click on the "Growth" link under the "More information" section
# (Note the other useful links that are available!)
# you can also click on the links for the graphs (e.g., M vs. K, M vs. Linf)









################## 
# EXTRA / JUNK CODE
##################

## Menhaden Catch at age & year
  library(FSAdata)
  data(Menhaden1)
  str(Menhaden1)
  head(Menhaden1)
  ages <- 0:6
  # Extract one year, delete year column (the -1), and transpose to be a vector
  ct <- t(Menhaden1[Menhaden1$year==1974,-1])
  plot(ct~ages,pch=16,type="b",xlab="Age",ylab="Est. Catch (Millions)",main="year==1974")

## Sunfish data 
  data(SunfishLP)
  str(SunfishLP)
  head(SunfishLP)
  plot(log(perc.freq)~age,data=SunfishLP)

## Sunfish data
  data("WhiteGrunt1")
  str(WhiteGrunt1)
  head(WhiteGrunt1)
  plot(log(catch)~age,data=WhiteGrunt1)
  cc1 <- catchCurve(catch~age,data=WhiteGrunt1,ages2use=7:22)
  summary(cc1)
  plot(cc1)

## Catch curves using BrookTrout data

  data(BrookTroutTH)
  plot(catch~age,data=BrookTroutTH,pch=19)
  
  ## demonstration of formula notation
  cc1 <- catchCurve(catch~age,data=BrookTroutTH,ages2use=2:6)
  summary(cc1)
  cbind(Est=coef(cc1),confint(cc1))
  plot(cc1)
  summary(cc1,parm="Z")
  cbind(Est=coef(cc1,parm="Z"),confint(cc1,parm="Z"))
  
  ## demonstration of excluding ages2use
  cc2 <- catchCurve(catch~age,data=BrookTroutTH,ages2use=-c(0,1))
  summary(cc2)
  plot(cc2)
  
  ## demonstration of using weights
  cc3 <- catchCurve(catch~age,data=BrookTroutTH,ages2use=2:6,weighted=TRUE)
  summary(cc3)
  plot(cc3)
  
  ## demonstration of returning the linear model results
  summary(cc3,parm="lm")
  cbind(Est=coef(cc3,parm="lm"),confint(cc3,parm="lm"))
  
  ## demonstration of ability to work with missing age classes
  df <- data.frame(age=c(  2, 3, 4, 5, 7, 9,12),
                   ct= c(100,92,83,71,56,35, 1))
  cc4 <- catchCurve(ct~age,data=df,ages2use=4:12)
  summary(cc4)
  plot(cc4)
  
  ## demonstration of ability to work with missing age classes
  ## evein if catches are recorded as NAs
  df <- data.frame(age=c(  2, 3, 4, 5, 6, 7, 8, 9,10,11,12),
                   ct= c(100,92,83,71,NA,56,NA,35,NA,NA, 1))
  cc5 <- catchCurve(ct~age,data=df,ages2use=4:12)
  summary(cc5)
  plot(cc5)


