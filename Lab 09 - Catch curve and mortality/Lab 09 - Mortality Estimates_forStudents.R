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


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Conducting a manual catch curve -----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#setwd("C:/Users/ab4577/HSU/Teaching/PopDy/2020_Spring/Labs/Lab 08 - Catch Curve, Mortality")
setwd("/Users/cpadamson/Dropbox/Grad/FISH 558/Lab 09 - Catch curve and mortality")
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
plot(mod1, which=1, add.smooth=F)
plot(mod1, which=2)

Z = -mod1$coefficients[2] #Note the negative; Z= - slope; Z is always positive
Z

Z.se = summary(mod1)$coefficients[2,"Std. Error"]
Z.se

#You could create a nice plot like those we've seen in class by modifying a plot like this:
plot(ln_num~age, data=CA, ylab = "log(catch)")
  abline(mod1)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Estimates of Z using the FSA package. ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#Look into the "FSA" package to find some 
#handy code to conduct a catch curve analysis.
#Suggestions: 
  # - Do a google search and see what functions you can come up with!
  # - install the package(s), and look at the functions in the help menu
  # - Read/skim the help menu (for the given function), and start with the examples at the bottom 
  # - Figure out how the data needs to be organized, and what options the function provides


## Code for installing packages and example code from the help menu:




## Now apply it to our "CA" data.  Note, make sure the data is organized correctly,
# and change the variable names as needed.



#How do the Catch curve estimates we calculated manually (above) compare to the function you used?






### Examples of catch curve datasets (this will be needed for the homework):
library(FSA)
library(FSAdata)
help.search("Catch curve",package=c("FSAdata","FSA"))
  #How to import a dataset (after you find a dataset name from above):
  data(BrookTroutTH) #This loads the dataset into your environment
  str(BrookTroutTH) #look at structure
  head(BrookTroutTH)

  


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Estimate of Natural Mortality using Empirical Relationships ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

library(FSA)

#use a process similar to what you did above and look into the metaM() function in the 
#FSA package.  Use it to figure out how to estimate M from life history parameters.

#Your code:
?metaM



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Obtaining life history data from fishbase for HW (for estimating M) -------
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# go to http://www.fishbase.org
# Search for your species of interest
# If you get multiple hits with the same common name and Species, click on one of the common names (which is a link)
# Click on the "Growth" link under the "More information" section
# (Note the other useful links that are available!)
# you can also click on the links for the graphs (e.g., M vs. K, M vs. Linf)



