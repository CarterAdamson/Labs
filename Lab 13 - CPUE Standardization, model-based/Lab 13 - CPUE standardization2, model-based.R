#...................................................................................
#...................................................................................
# FISH 458/558 - Lab 13: CPUE standardization 2 (Model-based)
# Altantic croaker
#
# Script modified from original version created by RJ Latour (September 2014)
#...................................................................................
#...................................................................................

#Lab Today
# - Review Take Home Exam
# - Paper presentation 
# - Go through Lab (look up Atlantic croaker)


  #MAIN THINGS YOU WILL BE DOING IN THIS LAB IS
  #CALCULATING DIFFERENT CROAKER INDICES FOR CPUE 
  #USING DIFFERENT METHODS:
  #   1. Nominal index (ie, average CPUE by year)
  #   2. GLM Index using 3 different CONTINUOUS distributions:
  #       2a. Normal distribution
  #       2b. Lognormal distribution
  #       2c. Gamma distribution
  #   3. GLM Index using 2 different DISCRETE distributions: 
  #       3a. Poisson
  #       3b. Binomial 
  #   4. Zero-Altered and Zero-Inflated methods: 
  #       3a. Zero Altered Poisson (ZAP)
  #       3b. Zero Altered Negative Binomial (ZANB)
  #       3c. Zero Inflated Poisson (ZIP)
  #       3d. Zero Inflated Negative Binomial (ZINB) 

        
  # Key things i want you to get out of this:
  #   1. See how to calculate and plot these standardized CPUE indices
  #       a. Understand how to use the predict() function to do this
  #   2. Learn how to determine what statistical model is best suited for the data
  #       a. Using diagnostic plots, rootograms, and AIC
  #   3. Evaluate and interpret the effect that different factors/covariates 
  #      (e.g., Region, depth, cruise) can have on a CPUE for a species.
  #   4. Compare the annual trends that come from indices calculated using different methods
  
  # NOTE: The beginning of this script is similar to the previous lab (Lab 12).


#Code to install package "countreg":
install.packages("countreg", repos="http://R-Forge.R-project.org")

#Code to install package "topmodels", which has the rootogram() function:
install.packages("topmodels", repos = "https://R-Forge.R-project.org")

#DATA #########################################################

#Make sure to set your working directory for this lab using the folder
#where you downloaded all your lab files.  
#Set your working directory to Source file location
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in croaker data
AC=read.csv("Croaker catch final.csv")
head(AC)

#Data info:
# Cruise - identifier for 1 of 5 cruises per year (each cruise is conducted around the same time each year)
# TowDist - towed distance in m; 
# Stratum - code comprised of single digit region code and 2-digit depth code
# Region - coded 1-5, with 1 being the northern most region of Chesapeake Bay
# Depth - coded 1-3, 1=shallow, 2=medium depth, 3=deep
# Netwidth - netwidth in ft
# StratNum - code (1-14) for unique strata (region-depth combinations)

#TALK ABOUT TRAWL PROGRAM: see https://www.vims.edu/research/departments/fisheries/programs/mrg_oldwebsite/chesmmap/index.php 


## Define Area-swept and CPUE  ###################################


#Calculate area swept in km2
AC$Area=(AC$TowDist/1000)*(AC$NetWidth*0.3048/1000)  #Corrected!
head(AC)
AC$log.area=log(AC$Area)

#Calc CPUE
AC$CPUE=AC$Count/AC$Area
head(AC)


## Clean up dataset   ####################


#For reference, explore the minimum and distribution of CPUE (excluding zeros)
AC.NoZero = subset(AC, CPUE>0)
  summary(AC.NoZero$CPUE)
  
  
#Subset by cruise (when croaker are more abundant).  This could be determined by graphical analysis (e.g., see below)
AC1=subset(AC,Cruise >=2 & AC$Cruise <=4)
head(AC1)

#Remove extraneous variables and
# Note the conversion of the categorical explanatory variables into factors
AC1=data.frame(catch=AC1$Count,
               year=as.factor(AC1$Year),
               cruise=as.factor(AC1$Cruise),
               region=as.factor(AC1$Region),
               depth=as.factor(AC1$Depth),
               area=AC1$Area,
               log.area = AC1$log.area,
               CPUE=AC1$CPUE)
head(AC1)


  

# Exploratory plots  ##############################


library("lattice")

#Conditional histogram showing high occurrence of zeros
histogram(~ CPUE|as.factor(year),data=AC1,xlab="CPUE")

#Conditional histogram of log(CPUE+1)
histogram(~ log(CPUE+1)|as.factor(year),data=AC1,xlab="CPUE")

#Conditional histogram of log(CPUE+1) excluding zeros
histogram(~ log(CPUE+1)|as.factor(year),data=subset(AC1, CPUE>0),xlab="CPUE")

    #HOW DO HISTOGRAMS FOR THE OTHER VARIABLES LOOK?


#Typically, you would explore the data thoroughly before analyzing...
#(as you did for the homework)

### EXAMPLE BOXPLOTS (note: here we are using the full dataset (AC) for demonstration purposes)
  #For Region
    boxplot(CPUE ~ Region, data=AC, xlab="Region", ylab="CPUE")
    boxplot(log(CPUE+1) ~ Region, data=AC, xlab="Region", ylab="log(CPUE+1)")
    boxplot(log(CPUE+1) ~ Region, data=AC.NoZero, xlab="Region", ylab="log(CPUE+1) (excluding zeros)")
  
    #THOUGHTS? 
    
    #HOW DO BOXPLOTS FOR THE OTHER VARIABLES LOOK?

### EXAMPLE CONDITIONAL Boxplots
  #For Region & Cruise 
    library(lattice) #bwplot is for box and whisker plot; note that the X variables have to be coded as factors
    bwplot(CPUE ~ as.factor(cruise)|as.factor(region), data=AC1, 
           xlab="Cruise", ylab="CPUE", layout=c(1,5)) 
    bwplot(log(CPUE+1) ~ as.factor(Cruise)|as.factor(Region), data=AC, 
           xlab="Cruise", ylab="log(CPUE+1)", layout=c(1,5))
    bwplot(log(CPUE+1) ~ as.factor(Cruise)|as.factor(Region), data=AC.NoZero, 
           xlab="Cruise", ylab="log(CPUE+1), excluding zeros", layout=c(1,5))
 

    

# Nominal Count-per-area-swept ##############################
    #(ie doesn't account for any factors)


library(dplyr)
#helpful dplyr tutorial: https://cran.rstudio.com/web/packages/dplyr/vignettes/introduction.html

#Calculate mean, var, and counts for each stratum-year combination
nominal = AC %>% group_by(Year) %>% 
  summarize(mean = mean(CPUE),
            var  = var(CPUE),
            cv   = sqrt(var)/mean, #coefficient of variation
            n    = length(CPUE),
            se   = sqrt(var/n),
            LCI  = mean-1.96*se,
            UCI  = mean+1.96*se) 
nominal
nominal = as.data.frame(nominal) #converting nominal back to dataframe for segments function below to work.

#generate plot of nominal mean (with approximate 95% CI)
plot(nominal$mean~nominal$Year,xlab="Year",ylab="Index (+/-95%CI)",main="Nominal Year Means",
     ylim=c(min(nominal$LCI),max(nominal$UCI)),type="b")
segments(x0=nominal$Year,x1=nominal$Year,y0=nominal$LCI,y1=nominal$UCI)
#average CPUE (CPUE grouped by year) per year vs. year
#nominal mean = arithmetic mean



#...................................................................................
#...................................................................................
# GLMs on continuous CPUE data using different distributions  ##########################################################################
#...................................................................................
#...................................................................................




## normal distribution ----------

mod1=glm(CPUE~year+cruise+region+depth, data=AC1, family=gaussian(link='identity'))
summary(mod1)
#(intercept) is for first level of categorical variables, other estimates are differences from that
#p-value: is the estimate different than zero (is the level different from the first level)

dev.expl.1 = (mod1$null.deviance - mod1$deviance) / mod1$null.deviance
dev.expl.1 #this is the deviance explained by the model (a goodness of fit measure, interpreted like R^2)
  #i.e., mod1 explains 12% of the deviance 

#look at diagnostics
par(mfrow=c(2,2))
plot(mod1, which=c(1:2))
hist(residuals(mod1))
  #How do the diagnostics look?  Remember that we are looking
  #for normality and homogeneity of variance.

#Covariate effects
  #Look at the effects of each factor or covariate
  par(mfrow=c(2,2))
  termplot(mod1, partial.resid=F, se=T, col.se="black", col.term="black")
    #Each panel shows the effect of variable X on the response.
    #For categorical factors (all the variables in our case), 
    #effects are shown discretely for each factor level (middle line is 
    #the predicted effect, dashed lines represent the standard error).
    #The Y axis on these plots are for the partial effect (or partial residuals).  Think of these as 
    # The effect that variable X has on the response (CPUE in this case)
    #after accounting for the other variables in the model.
  
    #Talk with your neighbor about what these plots are indicating (similar to what
    #we did in class with the Hawaii Longline paper).
  
  termplot(mod1, partial.resid=T, se=T, col.se="black", col.term="black")
    #This is the same plot as before, but now we've added the actual 
    #partial residuals calculated for each sample. The solid and dashed lines
    #still show the mean partial effect and its SE, but the scale has changed dramatically,
    #making these plots less useful i would say.
  
#Predictions by year
  # Create dummy dataframe to predict year means and get SEs.
  # We want to estimate the mean CPUE by year for a given
  # Cruise, Region, and Depth. By doing this, we are "standardizing" for
  # these 3 variables, allowing us to compare apples to apples when looking at
  # different years. (For example, our mean CPUE isn't biased in a year that maybe
  # sampled more in region 5 where we have the highest Croaker catch rates).
  p.data<-data.frame(year=levels(AC1$year),cruise="3",
                     region="4", depth="1")
  p.data
      #for each row in p.data, we will generate a CPUE prediction from our model.
  #change one value, keep all others consistent ####
  
  # Get year estimates and SEs in original scale by using type="response"
  out<-predict(mod1, newdata=p.data, type="response",se.fit=T)
      #Here are the predictions, given the specific levels of the factors in our model.
      #Note that we've held Cruise, Region, and Depth constant (ie, we have standardized for those effects!).
      #The predictions (out$fit) are the standardized indices for each year.
  
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

  

##lognormal distr --------------


#Lets re-fit the model, but this time, let's explore the use of 
#lognormal distribution (instead of normal).  Here, we will Log 
#the CPUE and then use a normal distribution to fit our model.
#But, because we can't take the log of 0, we need to add a small value
#(e.g., CPUE+1) before logging. 
  
#Need to log transform the CPUE and add small constant due to zeros
summary(subset(AC1,CPUE>0)$CPUE)  #Checking what the minimum value is (excluding zeros)
  #We just want to make sure the minimum value is not less than the value 
  #we add to the CPUE.  With a minimum of 47.56, we are fine.

#For ease, lets add the log(CPUE+1) calculation into our dataframe
AC1$logCPUE = log(AC1$CPUE+1)  

#Fit the model using log(CPUE+1) as our response variable.
mod2=glm(logCPUE~year+cruise+region+depth, data=AC1,family=gaussian(link='identity'))
summary(mod2)
dev.expl.2 = (mod2$null.deviance - mod2$deviance) / mod2$null.deviance
dev.expl.2 #this is the deviance explained by the model (a goodness of fit measure, interpreted like R^2)
  #How did the %deviance explained change from the earlier model?

#look at diagnostics
par(mfrow=c(2,2))
plot(mod2, which=c(1,2))
#plot(mod1, which=c(1,2))
  #Are these residuals better than what we had before?
  #Why does the Residual plot look so weird?

    #Yes, overall, the residuals are better (certainly in a relative sense)
    #The weird gap/pattern is because of the Log(CPUE+1). the "straight line"
    #of points in our residual plot represents all of the CPUE=0 values. 
    #This type of pattern is common to see.  The residuals are not ideal, 
    #but better than before.  We will proceed forward (although other distributions
    #further below would be better suited to these data.

#Covariate effects
#Look at the effects of each factor or covariate
par(mfrow=c(2,2))
termplot(mod2, partial.resid=F, se=T, col.se="black", col.term="black", main="Model 2 (Lognormal Dist.)")

  #What patterns do you notice here? 


  #How does this compare to the same 
  #plot we had generated for mod1?
    termplot(mod1, partial.resid=F, se=T, col.se="black", col.term="black", main="Model 1 (Normal Dist.)")


#Predictions by year (WITHOUT bias correction) - this is wrong, but used for demonstration
  # Use dummy dataframe to predict year means and get SEs
  # (need to specify values for other covariates)
  p.data
  
  # Get year estimates and SEs in original scale
  out2<-predict(mod2, newdata=p.data, type="response",se.fit=T)
  
  ## Summarize info and calculate statistics in yr.mean dataframe 
  yr.mean2<-as.data.frame(cbind(as.numeric(levels(AC1$year)),exp(out2$fit)-1,exp(out2$se.fit),
                                exp(out2$fit -1.96*out2$se.fit)-1, #LCI
                                exp(out2$fit +1.96*out2$se.fit)-1  #UCI
                                ))
  names(yr.mean2)<-c("year","mean","SE", "LCI", "UCI") 
  yr.mean2$CV<-yr.mean2[,3]/yr.mean2[,2] 
  
  # Plot mean and 95% CIs
  plot(yr.mean2[,2]~yr.mean2[,1],xlab="Year",ylab="Index",main="Year Means (No lnorm bias-correction)",
       ylim=c(min(yr.mean2[,4]),max(yr.mean2[,5])),type="b")
  segments(x0=yr.mean2[,1],x1=yr.mean2[,1],y0=yr.mean2[,4],y1=yr.mean2[,5])
    #Note: these are not the values to use! They are for demonstration.  Use the bias corrected values below.

#Bias correction for lognormal (using the lnormBC function that we "import" using dget())
  #Here we are importing and running a different script that corrects
  #for the bias that is introduced by modeling the log of our response.
  #this is analogous to the bias-correction we've done in the past.
  
  lnorm.bias.cor=dget("lnormBC.r")  
    #For this line to work, make sure that you have your working directory set
    #to the folder that houses the "lnormBC.r" file that you downloaded for today's lab.
  cor.mean=lnorm.bias.cor(mod2) #Run the bias correction function.
  
  #Calculate CV, LCI, UCI
  cor.mean$CV<-cor.mean[,2]/cor.mean[,1] 
  cor.mean$LCI<-cor.mean[,1]-1.96*cor.mean[,2] 
  cor.mean$UCI<-cor.mean[,1]+1.96*cor.mean[,2] 
  cor.mean
  
  #Plot using bias-corrected values ()
  plot(cor.mean[,1]~as.numeric(levels(AC1$year)),xlab="Year",ylab="Mean",main="Lognormal Bias-corrected means",
       ylim=c(min(cor.mean[,4]),max(cor.mean[,5])), type="b" )
  segments(x0=as.numeric(levels(AC1$year)),x1=as.numeric(levels(AC1$year)),
           y0=cor.mean[,4], y1=cor.mean[,5])
    #These bias corrected error bars are correct and better than the non-bias corrected.
    #Note also the change in scale for the y axis



## gamma distribution -------------------

#Run the model using a gamma distribution. Note that 
#we are still adding 1 to the CPUE, but not logging things. Also,
#note that we are now using a different link (specifically the inverse link)
AC1$CPUE1 = AC1$CPUE+1
mod3=glm(CPUE1~year+cruise+region+depth, data=AC1,family=Gamma(link='inverse'))
summary(mod3)

dev.expl.3 = (mod3$null.deviance - mod3$deviance) / mod3$null.deviance
dev.expl.3

#look at diagnostics
par(mfrow=c(2,2))
plot(mod3, which=c(1))  #NOTE: Don't use QQ plot b/c it is for normal distribution
hist(resid(mod3)) #note how the residuals aren't normal... (this is OK, bc using gamma)
#inadequate diagnostics, but true diagnostics not covered in this class

# Get year estimates and SEs in original scale
out3<-predict(mod3, newdata=p.data, type="response",se.fit=T)

## Summarize info and calculate statistics in yr.mean dataframe 
yr.mean3<-as.data.frame(cbind(as.numeric(levels(AC1$year)),out3$fit,out3$se.fit))
names(yr.mean3)<-c("year","mean","SE") 
yr.mean3$CV<-yr.mean3[,3]/yr.mean3[,2] 
yr.mean3$LCI<-yr.mean3[,2]-1.96*yr.mean3[,3] 
yr.mean3$UCI<-yr.mean3[,2]+1.96*yr.mean3[,3]   

# Plot mean and 95% CIs
plot(yr.mean3[,2]~yr.mean3[,1],xlab="Year",ylab="Index",main="Year Means",
     ylim=c(min(yr.mean3[,5]),max(yr.mean3[,6])),type="b")
segments(x0=yr.mean3[,1],x1=yr.mean3[,1],y0=yr.mean3[,5],y1=yr.mean3[,6])



#plot all indices ###########################

par(mfrow=c(3,1))
# Normal
plot(yr.mean[,2]~yr.mean[,1],xlab="Year",ylab="Index",main="Normal",
     ylim=c(min(yr.mean[,5]),max(yr.mean[,6])),type="b")
segments(x0=yr.mean[,1],x1=yr.mean[,1],y0=yr.mean[,5],y1=yr.mean[,6])

# Lognormal
plot(cor.mean[,1]~as.numeric(levels(AC1$year)),xlab="Year",ylab="Mean",main="Lognormal (Bias-corrected)",
     ylim=c(min(cor.mean[,4]),max(cor.mean[,5])), type="b" )
segments(x0=as.numeric(levels(AC1$year)),x1=as.numeric(levels(AC1$year)),
         y0=cor.mean[,4], y1=cor.mean[,5])

# Gamma
plot(yr.mean3[,2]~yr.mean3[,1],xlab="Year",ylab="Index",main="Gamma",
     ylim=c(min(yr.mean3[,5]),max(yr.mean3[,6])),type="b")
segments(x0=yr.mean3[,1],x1=yr.mean3[,1],y0=yr.mean3[,5],y1=yr.mean3[,6])

#Plot the different indices together, standardizing by their means
par(mfrow=c(1,1))
yrs = as.numeric(levels(AC1$year)) #make vector of numeric year values.

plot(yrs,(nominal$mean/mean(nominal$mean)),type='b',col='black',lwd=2,xlab='',
     ylab='Atlantic croaker index',ylim=c(-0.2,3))
lines(yrs,(yr.mean$mean/mean(yr.mean$mean)),type='b',col='blue',lwd=2)
lines(yrs,(yr.mean2$mean/mean(yr.mean2$mean)),type='b',col='red',lwd=2)
lines(yrs,(yr.mean3$mean/mean(yr.mean3$mean)),type='b',col='green',lwd=2)
legend("topright", c("nominal","normal","lognormal","gamma"),
       col=c("black","blue","red","green"), lwd=2)


#Talk with your peers to discuss any patterns or differences you see 
#among the different indices.  Also, which model do you think is the best?


#How would you pick the "BEST" model to use?
# - Unfortunately, you CANNOT use AIC to pick the best model, because the response 
#   variable (ie the "Y" variable) is different for each model (CPUE vs. log(CPUE+1) vs. CPUE+1)
# - Check that the model assumptions are valid/reasonable (i.e., the diagnostics look OK)
# - Base your decision on which models are more appropriate a priori (e.g., based on statistical distributions)
# - You can compare the % deviance explained (with preference for higher values)
#

#...............................................................................
#...............................................................................
# GLMs on DISCRETE CPUE data ###############################
# (e.g. FISH COUNTS) using different distributions
#...............................................................................
#...............................................................................


##NOTE:
# For the Croaker dataset, instead of modeling CPUE, we can model the
# COUNT of fish, which are discrete values.  So, we would be looking at using
# either the poisson or negative binomial distributions.
# But, given that we have different amounts of AREA covered by the trawl on different
# tows, we can include something called an "offset" in the model that accounts
# for the differences in area for each tow. This general approach is 
# arguably more elegant than modeling CPUE using a continuous distribution (which
# is what we did above).
# Also, our normal diagnostics (e.g., residuals vs. fitted, histograms, qq plots)
# aren't as useful for these distributions.  We can use a "rootogram" instead.

# An Explanation of using an "offset" in your model:
# https://rpubs.com/Shaunson26/offsetglm 

# Resources for understanding rootograms:
# https://fromthebottomoftheheap.net/2016/06/07/rootograms/ 
# https://www.tandfonline.com/doi/abs/10.1080/00031305.2016.1173590?journalCode=utas20

library(topmodels)

##poisson #############

mod4=glm(catch~offset(log.area)+year+cruise+region+depth, data=AC1, family=poisson)
#offset is used as a proxy for CPUE with integer data
#offset by log(effort)
summary(mod4)
dev.expl.4 = (mod4$null.deviance - mod4$deviance) / mod4$null.deviance
dev.expl.4 

#look at diagnostics
par(mfrow=c(2,2))
plot(mod4, which=c(1)) #note: for Poisson and NB models, heteroskedasticity (or mega-phone shapes) are expected!
hist(resid(mod4))
rootogram(mod4) #rootograms are a good diagnostic for count distributions --> see explanation here: https://fromthebottomoftheheap.net/2016/06/07/rootograms/ 
rootogram(mod4, breaks=c(0,seq(1,max(AC1$catch), by=50))) #this is the most useful for count distributions
rootogram(mod4, breaks=c(0,seq(1,200, by=10))) #this is the most useful for count distributions

breaks4rootogram = c(0,seq(1, 200, by=5)) #lookin 
breaks4rootogram = seq(0, 200, by=5) #lookin 
rootogram(mod4, breaks=breaks4rootogram) #this is the most useful for count distributions

#Write some notes about how to interpret rootograms:
#used to assess count distribution assumptions
#bars hang from a line
#x axis: observed catch
#y axis: sqrt(frequency) of that catch, somewhat like a histogram
#pink line: model prediction
#bars: actual sqrt frequencies observed
#in a perfect model, these will hang exactly down to zero
    #hanging below: more of that count than expected (underpredicting)
    #hanging above: fewer than expected (overpredicting)
#goal: hanging very near zero, randomly varying without much pattern


#Covariate effects
#Look at the effects of each factor or covariate
library(visreg)
par(mfrow=c(2,2))
termplot(mod4, partial.resid=F, se=T, col.se="black", col.term="black")
visreg(mod4, scale="response", partial=F)

#Predictions by year
# Use dummy dataframe to predict year means and get SEs
# (need to specify values for other covariates)
p.data2 = data.frame(p.data, log.area = median(AC1$log.area))

# Get year estimates and SEs in original scale by using type="response"
out<-predict(mod4, newdata=p.data2, type="response",se.fit=T)

## Summarize info and calculate statistics in yr.mean dataframe 
yr.mean4<-as.data.frame(cbind(as.numeric(levels(AC1$year)),out$fit,out$se.fit))
names(yr.mean4)<-c("year","mean","SE")
yr.mean4$CV<-yr.mean4$SE/yr.mean4$mean
yr.mean4$LCI<-yr.mean4[,2]-1.96*yr.mean4[,3]  #Lower 95% Confidence Interval (LCI)
yr.mean4$UCI<-yr.mean4[,2]+1.96*yr.mean4[,3]  #Upper 95% Confidence Interval (UCI)

# Plot mean and 95% CIs
plot(yr.mean4$mean~yr.mean4$year,xlab="Year",ylab="Index",main="Year Means (Poisson)",
     ylim=c(min(yr.mean4$LCI),max(yr.mean4$UCI)),type="b")
segments(x0=yr.mean4[,1],x1=yr.mean4[,1],y0=yr.mean4[,5],y1=yr.mean4[,6])



##negative binomial ############
library(MASS)
# 
#Fit model with glm.nb() function because it estimates the "theta" parameter for the distribution
mod5=glm.nb(catch~offset(log.area)+year+cruise+region+depth, data=AC1, link="log")
summary(mod5)
dev.expl.5 = (mod5$null.deviance - mod5$deviance) / mod5$null.deviance
dev.expl.5 

#If you get: "R glm.nb not converging"
#http://www.talkstats.com/showthread.php/31212-Algorithm-did-not-converge-warning-in-a-glm.nb-What-to-do 

#SIDENOTE: we use the glm.nb because it estimates the theta parameter.  if we were to use the glm() function, we need to 
# specify the theta parameter manually.  Here i enter the value that was estimated above:
#mod5.comp=glm(catch~year+cruise+region+depth, data=AC1, family=neg.bin(theta=0.13622), control=glm.control(maxit=50))
#summary(mod5.comp)  #gives the same results


#look at diagnostics
par(mfrow=c(2,2))
plot(mod5, which=c(1))
hist(resid(mod5))
rootogram(mod5)
rootogram(mod5, breaks=c(0,seq(1,100, by=5))) #this is the most useful for count distributions
  #How does the Rootogram look?  Here, we still see a concerning
  #pattern of too many zeros, with the gray bar at catch=0 
  #hanging well below zero.  Note: for the HW, you should see 
  #a clear difference in quality of fit between the Poisson and Negative 
  #Binomial, unlike the situation here.

  

#Covariate effects
#Look at the effects of each factor or covariate
par(mfrow=c(3,2))
termplot(mod5, partial.resid=F, se=T, col.se="black", col.term="black")
par(mfrow=c(3,2))
visreg(mod5, scale="response", partial=F)
par(mfrow=c(3,2))
visreg(mod5, scale="response", partial=T)

#Predictions by year
# Get year estimates and SEs in original scale by using type="response"
out<-predict(mod5, newdata=p.data2, type="response",se.fit=T)

## Summarize info and calculate statistics in yr.mean dataframe 
yr.mean5<-as.data.frame(cbind(as.numeric(levels(AC1$year)),out$fit,out$se.fit))
names(yr.mean5)<-c("year","mean","SE")
yr.mean5$CV<-yr.mean5$SE/yr.mean5$mean
yr.mean5$LCI<-yr.mean5[,2]-1.96*yr.mean5[,3]  #Lower 95% Confidence Interval (LCI)
yr.mean5$UCI<-yr.mean5[,2]+1.96*yr.mean5[,3]  #Upper 95% Confidence Interval (UCI)

# Plot mean and 95% CIs
plot(yr.mean5$mean~yr.mean5$year,xlab="Year",ylab="Index",main="Year Means (Neg Bin)",
     ylim=c(min(yr.mean5$LCI),max(yr.mean5$UCI)),type="b")
segments(x0=yr.mean5[,1],x1=yr.mean5[,1],y0=yr.mean5[,5],y1=yr.mean5[,6])

#Compare model AICs of the 2 models to determine best model (lower is better)
AIC(mod4,mod5)


#...................................................................................
#...................................................................................
# EXTRA: ZERO-ALTERED AND ZERO-INFLATED MODELS -----------------------
#...................................................................................
#...................................................................................

#NOTE: YOU WILL NOT NEED THIS FOR THE HOMEWORK... IT IS HERE FOR 
#FUTURE REFERENCE!

# There are two main packages and functions i've used to fit these models.
# Each has there pros/cons:
# 1. function hurdle() or zeroinfl() from package "countreg"
#     - Good: easy to use, allows easy visualization using visreg()
#     - Good: can use a great diagnostic tool called a "rootogram" to assess model assumptions
#     - Good: this function works with the dredge() function which allows you to easily do a model selection
#     - Bad: can't generate predictions with standard errors
# 2. function glmmTMB() from package glmmTMB
#     - Good: easy to use, CAN generate predictions with standard errors
#     - Good: allows you to build in random/mixed effects into your models
#     - Bad: rootogram() function doesn't work with it.
# Both packages generate very similar estimates so i will use both to take advantage of the pros of each.

# Acronyms used:
# ZAP - Zero-altered Poisson model
# ZANB - zero-altered negative binomial model
# ZIP - Zero-inflated Poisson model
# ZINB - zero-inflated negative binomial model

#Code to install package "countreg":
# install.packages("countreg", repos="http://R-Forge.R-project.org")

#Code to install package "topmodels", which has the rootogram() function:
# install.packages("topmodels", repos = "https://R-Forge.R-project.org")

# Resources for understanding rootograms:
# https://fromthebottomoftheheap.net/2016/06/07/rootograms/ 
# https://www.tandfonline.com/doi/abs/10.1080/00031305.2016.1173590?journalCode=utas20

# An Explanation of using an "offset" in your model:
# https://rpubs.com/Shaunson26/offsetglm 

library(glmmTMB)
library(countreg)
library(visreg)

?glmmTMB
?hurdle
?zeroinfl


#Fitting ZA and ZI models
  #ZA Poisson using hurdle()
  ZAP1 = hurdle(catch ~ offset(log.area)+year+cruise+region+depth, 
                data = AC1, dist = 'poisson')
  summary(ZAP1)

  
  #ZA Negative Binomial 
  ZANB1 = hurdle(catch ~ offset(log.area)+year+cruise+region+depth, 
                 data = AC1, dist = 'negbin')
  summary(ZANB1)

  
  #Zero inflated poisson
  ZIP1 = zeroinfl(catch ~ offset(log.area)+year+cruise+region+depth, 
                data = AC1, dist = 'poisson')
  summary(ZIP1)
  
  #Zero inflated negative binomial
  ZINB1 = zeroinfl(catch ~ offset(log.area)+year+cruise+region+depth, 
                 data = AC1, dist = 'negbin')
  summary(ZINB1)
  
  
  #Compare rootograms (diagnostic plots)
  par(mfrow=c(2,2))
  rootogram(ZAP1)
  rootogram(ZANB1)
  # rootogram(ZIP1) #for some reason, the new rootogram fxn doesn't work with ZI models
  # rootogram(ZINB1) #for some reason, the new rootogram fxn doesn't work with ZI models
  
  
  breaks4rootogram = seq(0, 100, by=5) #Let's "zoom in" to see the rootogram for catches from 0 to 100. 
  
  rootogram(ZAP1, breaks=breaks4rootogram)
  rootogram(ZANB1, breaks=breaks4rootogram)

  
  AIC(ZAP1, ZANB1, ZIP1, ZINB1)
  
    #DISCUSS...
  
  #Quick visualization of the ZANB1 model using visreg().  Note that it doesn't 
  #give us the SE for the estimates.  Instead it gives us the partial residuals.
  par(mfrow=c(3,2))
  visreg(ZANB2, scale="response",partial=F)




#Fitting models using glmmTMB
  #Poisson glmmTMB
  ZAP2 = glmmTMB(catch ~ offset(log.area)+year+cruise+region+depth, #model formula for the positive count part of the model
                 ziformula= ~offset(log.area)+year+cruise+region+depth, #model formula for the binomial part of the model
                 data=AC1, family=truncated_poisson)
  summary(ZAP2)
  
  #Negative Binomial with glmmTMB
  ZANB2 = glmmTMB(catch ~ offset(log.area)+year+cruise+region+depth,
                  ziformula= ~offset(log.area)+year+cruise+region+depth,
                  data=AC1, family=truncated_nbinom2)
  summary(ZANB2)
  

#Generate Plot with SE for ZANB -----------------

  #Generate predictions for each year
  pred.zanb.yr = predict(ZANB2, type = "response", se = T,
                           newdata = data.frame(p.data, log.area = median(AC1$log.area)))
  pred.zap.yr = predict(ZAP2, type = "response", se = T,
                           newdata = data.frame(p.data, log.area = median(AC1$log.area)))
  
  ## Summarize info and calculate statistics in yr.mean dataframe 
  yr.mean6<-as.data.frame(cbind(as.numeric(levels(AC1$year)),pred.zap.yr$fit,pred.zap.yr$se.fit))
  names(yr.mean6)<-c("year","mean","SE") 
  yr.mean6$CV<-yr.mean6[,3]/yr.mean6[,2] 
  yr.mean6$LCI<-yr.mean6[,2]-1.96*yr.mean6[,3] 
  yr.mean6$UCI<-yr.mean6[,2]+1.96*yr.mean6[,3] 
  
  plot(yr.mean6[,2]~yr.mean6[,1],xlab="Year",ylab="Index",main="Year Means (ZAP)",
       ylim=c(min(yr.mean6[,5]),max(yr.mean6[,6])),type="b")
  segments(x0=yr.mean6[,1],x1=yr.mean6[,1],y0=yr.mean6[,5],y1=yr.mean6[,6])
  
  ## Summarize info and calculate statistics in yr.mean dataframe 
  yr.mean7<-as.data.frame(cbind(as.numeric(levels(AC1$year)),pred.zanb.yr$fit,pred.zanb.yr$se.fit))
  names(yr.mean7)<-c("year","mean","SE") 
  yr.mean7$CV<-yr.mean7[,3]/yr.mean7[,2] 
  yr.mean7$LCI<-yr.mean7[,2]-1.96*yr.mean7[,3] 
  yr.mean7$UCI<-yr.mean7[,2]+1.96*yr.mean7[,3] 
  
  plot(yr.mean7[,2]~yr.mean7[,1],xlab="Year",ylab="Index",main="Year Means (ZANB)",
       ylim=c(min(yr.mean7[,5]),max(yr.mean7[,6])),type="b")
  segments(x0=yr.mean7[,1],x1=yr.mean7[,1],y0=yr.mean7[,5],y1=yr.mean7[,6])





## plot all indices ###########################

  
par(mfrow=c(1,1))
# Normal
plot(yr.mean[,2]~yr.mean[,1],xlab="Year",ylab="Index",main="Normal",
     ylim=c(min(yr.mean[,5]),max(yr.mean[,6])),type="b")
segments(x0=yr.mean[,1],x1=yr.mean[,1],y0=yr.mean[,5],y1=yr.mean[,6])

# Lognormal
  plot(cor.mean[,1]~as.numeric(levels(AC1$year)),xlab="Year",ylab="Mean",main="Lognormal (Bias-corrected)",
       ylim=c(min(cor.mean[,4]),max(cor.mean[,5])), type="b" )
  segments(x0=as.numeric(levels(AC1$year)),x1=as.numeric(levels(AC1$year)),
           y0=cor.mean[,4], y1=cor.mean[,5])

# Gamma
plot(yr.mean3[,2]~yr.mean3[,1],xlab="Year",ylab="Index",main="Gamma",
     ylim=c(min(yr.mean3[,5]),max(yr.mean3[,6])),type="b")
segments(x0=yr.mean3[,1],x1=yr.mean3[,1],y0=yr.mean3[,5],y1=yr.mean3[,6])

# Poisson
plot(yr.mean4[,2]~yr.mean4[,1],xlab="Year",ylab="Index",main="Poisson",
     ylim=c(min(yr.mean4[,5]),max(yr.mean4[,6])),type="b")
segments(x0=yr.mean4[,1],x1=yr.mean4[,1],y0=yr.mean4[,5],y1=yr.mean4[,6])

# Negative Binomial
plot(yr.mean5[,2]~yr.mean5[,1],xlab="Year",ylab="Index",main="Negative Binomial",
     ylim=c(min(yr.mean5[,5]),max(yr.mean5[,6])),type="b")
segments(x0=yr.mean5[,1],x1=yr.mean5[,1],y0=yr.mean5[,5],y1=yr.mean5[,6])

# ZAP
plot(yr.mean6[,2]~yr.mean6[,1],xlab="Year",ylab="Index",main="ZAP",
     ylim=c(min(yr.mean6[,5]),max(yr.mean6[,6])),type="b")
segments(x0=yr.mean6[,1],x1=yr.mean6[,1],y0=yr.mean6[,5],y1=yr.mean6[,6])

# ZANB
plot(yr.mean7[,2]~yr.mean7[,1],xlab="Year",ylab="Index",main="ZANB",
     ylim=c(min(yr.mean7[,5]),max(yr.mean7[,6])),type="b")
segments(x0=yr.mean7[,1],x1=yr.mean7[,1],y0=yr.mean7[,5],y1=yr.mean7[,6])


#Plot the different indices together, standardizing by their means
par(mfrow=c(1,1))
yrs = as.numeric(levels(AC1$year)) #make vector of numeric year values.

plot(yrs,(nominal$mean/mean(nominal$mean)),type='b',col='black',lwd=2,xlab='',
     ylab='Atlantic croaker index',ylim=c(-0.2,3))
lines(yrs,(yr.mean$mean/mean(yr.mean$mean)),type='b',col='blue',lwd=2)
lines(yrs,(yr.mean2$mean/mean(yr.mean2$mean)),type='b',col='red',lwd=2)
lines(yrs,(yr.mean3$mean/mean(yr.mean3$mean)),type='b',col='green',lwd=2)
lines(yrs,(yr.mean4$mean/mean(yr.mean4$mean)),type='b',col='purple',lwd=2)
lines(yrs,(yr.mean5$mean/mean(yr.mean5$mean)),type='b',col='orange',lwd=2)
lines(yrs,(yr.mean6$mean/mean(yr.mean6$mean)),type='b',col='darkgoldenrod',lwd=2)
lines(yrs,(yr.mean7$mean/mean(yr.mean7$mean)),type='b',col='cyan',lwd=2)
legend("topright", c("nominal","normal","lognormal","gamma","poisson","neg.binom.",
                     "ZAP","ZANB"),
       col=c("black","blue","red","green","purple","orange","darkgoldenrod","cyan"),
       lwd=2, cex=0.8)


#Talk with your peers to discuss any patterns or differences you see 
#among the different indices.

