#...................................................................................
#...................................................................................
# FISH 458/558 - Lab 11a: CPUE - stratified sampling 
# Altantic croaker
#
# Script modified from original version created by RJ Latour (September 2014)
#...................................................................................
#...................................................................................


#2024: 
# - Reminder: Takehome due tonight
# - Student Mini-Presentation
# - Go through lab script
# - Talk about HW
# - Review in-class exam



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# SETUP -----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Set your working directory to Source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in tow data for Atlantic Croaker (AC)
# These are data on the catches of Croaker in the Chesapeake Bay over multiple years
# from a longterm monitoring program that uses a bottom trawl.

AC=read.csv("Croaker catch final.csv")
head(AC)
summary(AC)

#TALK ABOUT TRAWL PROGRAM: see https://www.vims.edu/research/departments/fisheries/programs/mrg_oldwebsite/chesmmap/index.php 

#Data info:
# STATION - unique identifier for each station sampled (includes info on year, month, and station number)
# Date - date of sampling
# Year - year of sampling
# Month - Month of sampling
# Cruise - identifier for 1 of 5 cruises per year
# TowDist - towed distance in m; 
# Stratum - code comprised of single digit region code and 2-digit depth code
# Region - coded 1-5, with 1 being the northern most region of Chesapeake Bay
# Depth - coded 1-3, 1=shallow, 2=medium depth, 3=deep
# Count - Number of Atlantic Croaker caught at the station
# Netwidth - netwidth in ft (measured using acoustic sensors while the net was fishing)
# StratNum - code (1-14) for unique strata (region-depth combinations)

#define CPUE as count/area-swept - convert to count per km2
AC$CPUE=AC$Count/((AC$TowDist/1000)*(AC$NetWidth/(0.3048*1000))) 
head(AC)

# read in stratum weights (fraction of total survey area for each StratNum)
W=read.csv("CM weights_new.csv")
head(W)

#Data info:
# StratNum - code (1-14) for unique strata (region-depth combinations) 
# Weight - Proportion of total area represented by each stratum
# TotalN - all possible grid "cells" that could have been sampled within a stratum; 


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Subset dataset (for HW) ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@





#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Exploratory plots (some examples) ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#STUDENT: Work through the code in this section.  Discuss the plots with your
# peers, and make sure you have a basic idea for how the code works. 

library("lattice") 

## Histograms
  #histogram showing high occurrence of zeros in fish catches
  histogram(~ Count,data=AC,xlab="Catch", 
            main="Croaker Catch histogram", 
            breaks = 1000, xlim=c(0,200))
  #histogram with zeros excluded - Notice how we subset the AC data for CPUE>0
  histogram(~ Count,data=subset(AC, CPUE>0),xlab="Catch", 
            main="Croaker Catch histogram (Zeros excluded)", 
            breaks = 1000, xlim=c(0,200))

  #Conditional histogram of log(CPUE+1) (ie separate panels for some factor)
  histogram(~ log(CPUE+1)|as.factor(Depth),data=AC,xlab="log(CPUE+1)")
  #each panel represents a categorical depth value

## Sidebar... What proportion of tows were non-zero?   
AC$Pos = ifelse(AC$Count>0,1,0)
FracPos = sum(AC$Pos)/length(AC$Pos)
FracPos #Only 35% of tows were positive!


## Scatter plots
plot(CPUE~Year, data=AC)
plot(log(CPUE+1)~Year, data=AC)
  #Perhaps not super useful.  But what are the differences between the two plots?
#how many circles are stacked at 0? how can we know

### Boxplots
  #See a figure with a description of boxplots here: https://r-graph-gallery.com/boxplot.html.
  #If you need more explanation/detail, see here: https://towardsdatascience.com/understanding-boxplots-5e2df7bcbd51 
  #Make sure you know how to read a boxplot and what the parts mean.  
  #However, note that the whiskers don't really identify the minimum and maximum.
  #That is a poor description. 

  #Write notes here:

  #Boxplot using base package
  boxplot(CPUE~Year, data=AC, ylab="CPUE")
  boxplot(log(CPUE+1)~Year, data=AC, ylab="log(CPUE+1)") #Notice how log(CPUE+1) is a bit more informative

  #Boxplots using ggplot
  library(ggplot2)
  library(dplyr)
  AC %>% 
    ggplot(aes(x=Year, y=log(CPUE+1), group=Year)) + 
    geom_boxplot() 
  
  AC %>% 
    ggplot(aes(x=Year, y=log(CPUE+1), group=Year)) + 
    geom_boxplot() +
    facet_wrap(~Depth)  #the "facet_wrap" is how you create different panels

## DISCUSS WITH YOUR PEERS: ----
  # - What patterns and observations did you make about the dataset?
  # - What were the benefits/disadvantages of different plot types?
  # - When would you use CPUE vs. log(CPUE+1)?  Also, why do we add a 1 when logging?
  
#Typically, you would explore the data thoroughly before analyzing...
#You will need to do more of this for the HW.




#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Nominal Count-per-area-swept  ----
# (ie doesn't account for stratification or any factors)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


library(dplyr)
#helpful dplyr tutorial: https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html

#Calculate mean, var, and counts for each year 
AC.mean.nominal = AC %>% group_by(Year) %>% 
  summarize(mean = mean(CPUE),
            var  = var(CPUE),
            sd   = sqrt(var),
            cv   = sqrt(var)/mean,
            n    = length(CPUE),
            se   = sd/sqrt(n))
AC.mean.nominal


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Stratified Mean Calculation ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

library(dplyr)
#helpful dplyr tutorial: https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html

#Calculate mean, var, and counts for each year-StratNum combination by modifying the script above.
# You will need to change the group_by() statement. 
# Call the new variables: strat.mean, strat.var, strat.n, respectively.
# Save this as an object called: "AC.mean"

AC.mean = AC %>% group_by(Year, StratNum) %>% 
  summarize(strat.mean = mean(CPUE),
            strat.var  = var(CPUE),
            strat.n    = length(CPUE))
AC.mean

  #(Correct code available at bottom of script, but do it without looking!)


#Merge with the stratum-specific weights and the TotalN (based on area)
  W #This has the weights
  AC.mean = left_join(AC.mean, W, by="StratNum") #this type of join keeps all observations in the first listed dataframe
  AC.mean

#Do calculations that are needed for the summation (see lecture notes for equation)
  #Here we are using the dplyr package to create new columns (mean.calc and var.calc) in our dataframe
  AC.mean2 = AC.mean %>% 
    mutate(mean.calc = strat.mean * Weight, 
           var.calc = Weight^2*(TotalN-strat.n)/(TotalN-1)*strat.var)
  AC.mean2

#Do the summation for the calculations to get annual index, its variance, and calculate coefficient of variation (CV)
  AC.index = AC.mean2 %>% group_by(Year) %>% 
    summarize(index = sum(mean.calc),
              var   = sum(var.calc),
              se    = sqrt(var),
              cv    = sqrt(var)/index,
              LCI   = index-1.96*se,
              UCI   = index+1.96*se)  
  AC.index


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Plot final index and SE ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
par(mfrow=c(1,1))
  # Plot mean and SE
plot(index~Year, data=AC.index, xlab="Year",ylab="Index",main="Croaker Indices (+/- SE)",
     ylim=c(min(index-se),max(index+se)),type="b")
  lines(AC.mean.nominal$Year, AC.mean.nominal$mean, lty=2, col="red")
with(AC.index,  #the with(X, ...) allows you to refer to columns within the X dataframe, similar to when you "attach()" a dataframe
  segments(x0=Year,x1=Year, y0=index-se, y1=index+se) 
  )
legend("topright",c("Stratified Mean","Arithmetic Mean"),col=c('black','red'),lty=c(1,2))



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## ALTERNATIVE PLOT: Plot final index and CV ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
yrs=2002:2014
par(mar=c(5,4,4,4))

plot(yrs,AC.index$index,axes=F,xlab='',ylab='',type='b',lwd=2,ylim=c(0,400))
axis(1,at=seq(2002,2014,1))
axis(2,at=seq(0,400,100),las=2)
mtext('Stratified Mean', line=3,side=2)

par(new=T) #this allows you to add a plot onto an existing one
  
plot(yrs,AC.index$cv,axes=F,type='b',ylab='', xlab='',col='gray',lwd=2,ylim=c(0,4))
axis(4,at=seq(0,4,1.0),las=2)
mtext('CV', line=2.5,side=4)

title("Atlantic croaker relative abundance")
legend(2010,4.1,c("Index","CV"),col=c('black','gray'),lwd=2)
  
    





### CODE FOR STUDENT SECTION ABOVE
  AC.mean = AC %>% group_by(Year, StratNum) %>% 
    summarize(strat.mean = mean(CPUE),
              strat.var  = var(CPUE),
              strat.n    = length(CPUE))
  AC.mean

  
  
  

