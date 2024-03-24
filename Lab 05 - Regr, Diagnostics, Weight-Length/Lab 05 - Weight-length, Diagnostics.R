#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# FISH 458/558 - Fisheries Population Dynamics 
# Lab 5 - Weight-Length Models   
# A. Buchheister (based on material from RJ Latour and PD Lynch)
# January 2024
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


# TO DO IN LAB:
#   - Comments from returned HW
#     - Figure formatting & captions
#     - Question 3 - Make sure you understand this
#     - Make sure to include your code
#     - Answer in complete sentences
#     - Questions?
#   - Comment on Working Together
#   - Mini presentation (Sarah M.) - https://docs.google.com/spreadsheets/d/1FLvSbWkZqddlqOKo7F821xU0g77_9xmSVmroeDkOWxk/edit#gid=786638210
#     - No presentation next week
#   - Regression demo (Excel)
#   - Look up pictures of Cobia
#   - Go through Lab script
#   - No HW this week --> Take home will be distributed this week.



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# BRING IN DATA ------
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#First, set your working directory. This is the path for the folder that has the lab files.
#You can set your working directory by going to: 
#Session>Set Working Directory>To Source File Location
#(note: this will only work if you have all of your files for the lab together in a single folder)
#Once you do it, you can copy the code from your console into your script for future use. 
#It will look something like this:
#setwd("C:/Users/ab4577/HSU/Teaching/PopDy/2022_Spring/Labs/Lab 04 - DataExpl, Weight-length, Condition")

#Read in cobia dataset that has length (cm) and weight (kg) measurements for cobia.
cobia=read.csv("Cobia_Weight-at-length_unsorted.csv")
head(cobia)

#Let's sort, or order, the dataframe by one of the variables (this will facilitate plotting our model fits later on)
  #Create new data frame sorted by length:
  cobia = cobia[order(cobia$Length), ]  #Order the data by length.
  cobia
    
#Load packages
library(lattice)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# BASIC DATA EXPLORATION -----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

  #STUDENTS: Run this section on your own and discuss what you see with a neighbor.
  #plot weight-at-length data
  L=cobia$Length
  W=cobia$Weight
  
  par(mfrow=c(1,1))
  
  #Scatter plot - all fish
  plot(L,W,xlab='Fork length (cm)',ylab='Weight (kg)',xlim=c(min(L),max(L)))
  
  #Treat sexes separately; subset dataframe for Males and females, then make plot
  Male=subset(cobia,Sex=='M')
  Female=subset(cobia,Sex=='F' )
    #SIDENOTE: If you want to subset by multiple variables, you could do something like this:
      BigFemales=subset(cobia, Sex=='F' & Length>120)

  plot(Male$Length,Male$Weight,col='blue',xlab='Fork length (cm)',ylab='Weight (kg)',xlim=c(min(L),max(L)),ylim=c(0,max(W)))
  points(Female$Length,Female$Weight,col='red')  #
  
  #COPLOT ("conditional plot") - Great tool for looking at possible interactions or effects of other variables
    coplot(Weight~Length|Sex, data=cobia, rows=1)  #Recall that | means "given"

  #Same thing with ggplot
  library(ggplot2)
  ggplot(data=cobia, aes(x=Length, y=Weight, color=Sex))+
    geom_point()
  
###CALCULATING VALUES FOR SUBSETS OF A DATASET (NEEDED FOR HW!)
    #Calculate mean weight and sample size (n) by sex for cobia data
    library(dplyr)  #install the dplyr package if needed.
    L.table = cobia %>% group_by(Sex) %>% 
              summarize(Wt.mean= mean(Weight), 
                        n=length(Weight)) 
    L.table
      #for tutorial on data manipulation with dplyr: see:
      #http://genomicsclass.github.io/book/pages/dplyr_tutorial.html



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#  WEIGHT-LENGTH GROWTH MODELS ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Allometric growth model:  W = a*L^b ----


    

## 1. Multiplicative error structure ---------------------------------

#If we assume multiplicative error, the allometric growth eqn becomes:
#  W = a*(L^b)*exp(eps), where W=weight, L=length, a and b are parameters, and eps=random error (epsilon)
#we can log transform the equation and make it a linear model:
# log(Weight) = a1 + b*log(Length) + eps 
#  (Note that a1 in this eqn is different than a in the allometric model; a1 = log(a))

#Fit linear model  
mod1=lm(log(Weight)~log(Length), data=cobia)   #Syntax: lm(Y~X, data=mydata)
summary(mod1)

#Plot of log-transformed data with model fit
  par(mfrow=c(1,1))
  plot(log(Weight)~log(Length), data=cobia,xlab='log(Length)',ylab='log(Weight)', main="Cobia log(Weight) vs log(Length)")
  abline(mod1, col="red", lwd=2)

#Extract slope (b) and intercept (a1) from the log-transformed model
  summary(mod1)$coefficients #this is the table of the coefficients from the model summary
  b=summary(mod1)$coefficients[2,1]   
  b
  a1=summary(mod1)$coefficients[1,1]
  a1
  
#Backtransform the a1 parameter to get "a" for the W = a*(L^b) model
  #note: log(a) = a1, so a = exp(a1)
  a = exp(a1)
  a
    

  ###DIAGNOSTICS FOR CHECKING ASSUMPTIONS ------
  
  #Background: R can provide some Automated diagnostic plots
    plot(mod1)  #provides 4 diagnostic plots, but you need to hit "RETURN" to see the plots.
    ?plot.lm #this describes the standard plots for an lm object
    par(mfrow=c(2,2))
    plot(mod1)  #plot all 4 plots together.
        #How to pick specific plots (e.g., examine residuals and QQplot only)
        par(mfrow=c(2,1))
        plot(mod1, which=c(1,2))  #use "which" to specify the plot numbers

  #CREATE YOUR OWN PERSONALIZED SET OF DIAGNOSTIC PLOTS TOGETHER
      par(mfrow=c(2,2)) #Reset par to start new set of plots

      #Store residuals
      r1 = resid(mod1)
      
      #1. Y vs. X (with fit)
      plot(log(Weight)~log(Length), data=cobia,xlab='log(Length)',ylab='log(Weight)', main="Cobia log(Weight) vs log(Length)")
      abline(mod1, col="red", lwd=2)

      #2. Histogram of residuals
      hist(r1, xlab="Residual", main="Histogram of residuals", breaks=25)
      
      #3. Resid vs. fitted
      plot(mod1, which=1, add.smooth=F) #Note: removing the red smoothing line
    
      #4. QQ Plot
      plot(mod1, which=2)
    


    ### Bias correction and plotting ---------

    # Remember that fitting a linear model to logged data will lead to bias in the estimates.
    # We have to correct for this bias by multiplying back-transformed parameter estimates
    # by a correction factor (CF), where CF = exp((SEE^2)/2).
    
    #STEP 1. Get SEE (standard error of estimate; aka residual standard error)
      #Get SEE (aka "Residual standard error" from model output)
      SEE = summary(mod1)$sigma  
      
        #Sidenote: you can also calculate SEE by hand:
          # n=nrow(cobia)
          # SEE2=sqrt(sum(((log(W)-predict(mod1))^2)/(n-2)))

    #STEP 2. Calculate correction factor (CF) for model 2 
      CF=exp((SEE^2)/2)
    
    #STEP 3. Backtransform predictions (for mod1) and multiply by CF 
    pred1=exp(predict(mod1))*CF
      
      pred1.nocorrection = exp(predict(mod1)) #predictions with NO bias correction
    
    #Plot of bias-corrected predictions
      par(mfrow=c(1,1))
      plot(L,W,xlab='Fork length (cm)',ylab='Weight (kg)',xlim=c(50,max(L)), main="Cobia Weight at Length")
      lines(L,pred1,col='red',lwd=2)
      legend("topleft",c("Model 1 - multiplicative error"),
             col=c("red"), lty=1)
      
    #Plot of log-transformed data
      par(mfrow=c(1,1))
      plot(log(L),log(W),xlab='log(Length)',ylab='log(Weight)', main="Cobia log(Weight) vs log(Length)")
      abline(mod1, col='red',lwd=2)
    
    

    ### HYPOTHESIS TEST FOR b PARAMETER ----

      #Run a hypothesis test for comparing the b estimate against 3 (which would be for an isometric model)
      #extract b estimate and SE(b) from model object
      b=summary(mod1)$coefficients[2,1]   
      b
      se.b=summary(mod1)$coefficients[2,2]
      se.b
      
      #define sample size (n)
      n=nrow(cobia)
      n
      
      #Calculate your test statistic: t = |b-3|/se(b), where b is your estimate, and se(b) is the standard error of the b estimate
      test.stat=abs((b-3)/se.b)
      test.stat
      
      #Calculate the critical t value (if test.stat is greater than this, we say the 
      #difference is significant and reject the null hypothesis that b=3)
      t.crit= qt(0.975,n-1) #This gives the t value given a quantile value and your degrees of freedom (df)
      t.crit                      #We use 0.975 for alpha=0.05 because this is a 2-tailed test, and we specify df as n-1.  
      
      #Calculate the exact p value (probability of a t statistic greater than or equal to what was observed)
      pval=pt(test.stat,n-1,lower.tail=F)  
      pval
        #This is the P-value that you interpret to see if your b coefficient is significantly different
        #from 3.  
          # H0: b=3
          # H1: b != 3 (!= means "not equal to")
        #If P is <0.05, we reject the null hypothesis.
        #In this situation, we might say something like this:
        #"A t-test indicated that the b parameter was significantly 
        #different from 3 (t=6.97, P<0.001), therefore we will use 
        #the allometric growth model as opposed to the isometric growth model."
      
   
      
      
      
      
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# EXTRA REFERENCE MATERIAL 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
   
         

  ## 2. Additive error structure, using Nonlinear least squares ----

  # Use nonlinear least squares and additive error structure:
  #  W = a*(L^b) + eps, where W=weight, L=length, a and b are parameters, and eps=random error (epsilon)
      
    #You have to specify the equation for your model, and specify some starting values for your parameters
    #Note: if you get errors, try using different starting values
  mod2=nls(Weight~a*Length^b, data=cobia, start=list(a=5e-6,b=2.9), trace=T)   
  summary(mod2)
  
  #Generate plot of model fit - mod2 predictions (using additive error structure)
    pred2 = predict(mod2) #use predict() function to get estimated (aka fitted) Y values
    
    par(mfrow=c(1,1))
    plot(L,W,xlab='Fork length (cm)',ylab='Weight (kg)',xlim=c(50,max(L)), main="Cobia Weight at Length")
    lines(L,pred2,col='blue',lwd=2)
    legend("topleft",c("Model 2 - additive error"),
           col=c("blue"), lty=1)
  
    
  ###Evaluate main model assumptions: 1) Normality of errors, 2) Homogeneity of variance of errors, 3) independence, 
    #define residuals
    r2=resid(mod2)
    
    #histogram of residuals with normal plot
    hist(r2,xlab='Residual')  #ok, but if we want to plot a normal distribution on top, we have to do the following:
    
        #SKIP: histogram with normal curve overlaid
          hist(r2,xlab='Residual', prob=TRUE) #instead of plotting frequency, we plot the probability density
          curve(dnorm(x,mean=mean(r2), sd=sd(r2)), col='red',add=TRUE)  #dnorm is for the probability density for the normal distribution
          #residuals seem to be slightly negatively skewed
      
    #qqplot for normal distribution; 
    qqnorm(r2)
    qqline(r2,lwd=2, col="red") #add in a line for reference
      #Our residuals form a nearly straight line which is good (although there are some deviations at the ends)
    
    #plot residuals vs. X variable
    plot(r2~L,ylab='Residual',xlab='Fork length')
    abline(0,0,lty=2)
      #We have a problem of non-constant variance here! greater residuals at larger lengths
    
        #SIDENOTE: use identify function to look at potential outlier(s) - allows you to click on the plot to indicate points
        #  identify(x=L,y=r2)  #Click as many points of interest as you want, then press ESCAPE
    
  ### HYPOTHESIS TEST FOR b PARAMETER ----

    #Run a hypothesis test for comparing the b estimate against 3 (which would be for an isometric model)
      #extract b estimate and SE(b) from model object
      names(summary(mod2))
      summary(mod2)$coefficients
      
      b=summary(mod2)$coefficients[2,1]   
      b
      se.b=summary(mod2)$coefficients[2,2]
      se.b
      
      #define sample size (n)
      n=nrow(cobia)  #Remember to remove rows with missing data if there are any!  e.g., nrow(na.omit(cobia))
      n
      
      #Calculate your test statistic: t = |b-3|/se(b), where b is your estimate, and se(b) is the standard error of the b estimate
      test.stat=abs((b-3)/se.b)
      test.stat
      
      #Calculate the critical t value (if test.stat is greater than this, we say the 
      #difference is significant and reject the null hypothesis that b=3)
      t.crit= qt(0.975,n-1) #This gives the t value given a quantile value and your degrees of freedom (df)
                            #We use 0.975 for alpha=0.05 because this is a 2-tailed test, and we specify df as n-1.  
      
      #Calculate the exact p value (probability of a t statistic greater than or equal to what was observed)
      pval=pt(test.stat,n-1,lower.tail=F)  
      pval
  
  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# COMPARING ADDITIVE AND MULTIPLICATIVE ERROR MODELS -----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
    #Plot of both model predictions overlaid
      par(mfrow=c(1,1))
      plot(L,W,xlab='Fork length (cm)',ylab='Weight (kg)',xlim=c(50,max(L)), main="Cobia Weight at Length")
      lines(L,pred1,col='red',lwd=2)  #Predictions from model with additive error
      lines(L,pred2,col='blue',lwd=2)
      legend("topleft",c("Model 1 - multiplicative error", "Model 2 - additive error"),
             col=c("red","blue"), lty=1)
  
    #multipanel plot for diagnostics of mod1 and mod2 for comparison
    par(mfrow=c(3,2))
    
    #histogram of residuals
      hist(r1,xlab='Residual', prob=TRUE, breaks=25) 
      curve(dnorm(x,mean=mean(r1), sd=sd(r1)), col='red',add=TRUE)
      
      hist(r2,xlab='Residual', prob=TRUE, breaks=25) 
      curve(dnorm(x,mean=mean(r2), sd=sd(r2)), col='blue',add=TRUE)
    
    #qqplot
      qqnorm(r1)
      qqline(r1, lwd=2, col="red")
      
      qqnorm(r2)
      qqline(r2, lwd=2, col="blue")
    
    #plot residuals
      plot(r1~L,ylab='Residual',xlab='Fork length', main="Residual plot - model 1")
      abline(0,0,lty=2)
      
      plot(r2~L,ylab='Residual',xlab='Fork length', main="Residual plot - model 2")
      abline(0,0,lty=2)
    
    #improved plots under multiplicative error

   
      
         
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Extra exploratory plots
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
      
      #HISTOGRAMS, DENSITY PLOTS - Histograms and related plots
      hist(cobia$Length, breaks=20)
      histogram(~cobia$Length | cobia$Sex, breaks=20)  #note: using a different package (lattice)
      
      #BOXPLOT
      par(mfrow=c(1,1))
      boxplot(cobia$Length, ylab="Length")
      boxplot(cobia$Length~cobia$Sex, xlab="Sex", ylab="Length")

