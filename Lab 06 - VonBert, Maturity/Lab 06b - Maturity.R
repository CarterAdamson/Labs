####################################################
# FISH 458/558 - Fish Population Dynamics 
# Lab 6b - Maturity   
# A. Buchheister (based on material from Derek Ogle)
# February 2024
####################################################

#Recording of Andre going through the Script:
https://humboldt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=42c80838-e90e-49f0-8a50-b119008e6e14


#Good Resources
#This pdf is what most of the lab is based off of (although
#not all of it is included):
derekogle.com/fishR/examples/oldFishRVignettes/Maturity.pdf 

#Here is another script I found previously, but have not played with:
https://gist.github.com/alharry/4576675


######################################################    
######################################################
###  Modeling maturity with Logistic regression (Based on Derek Ogle's Vignette)
#####################################################
######################################################    

##MAIN GOALS/OBJECTIVES##
# - Plot and visualize maturity data using real data
# - Fit a logistic regression model to the data
# - Make predictions of the probability of being mature based on length
# - Calculate the length at 50% maturity


#Load packages.  (Note: you will have to install them
#on first use. In bottom right window, click "Packages" Tab
#then "Install".  Type in the package names separated by commas and 
#click Install.  This may take several minutes.

library(FSA)  #Fisheries Stock Assessment (FSA) package
library(FSAdata)
library(car)  
library(dplyr)

#Load and examine Rockfish data
data("YERockfish")
?YERockfish
str(YERockfish)  
head(YERockfish)
summary(YERockfish)

#Plot individual maturity data
  #First, create new column with maturity as 0 and 1 (we'll call it "mat01").
  YERockfish = YERockfish %>% mutate(mat01=ifelse(YERockfish$maturity=="Mature", 1,
                                           ifelse(YERockfish$maturity=="Immature", 0,
                                           ifelse(YERockfish$maturity=="Unknown", NA, 99))))
  #make plot
  par(mfrow=c(1,1))
  plot(mat01~length, data=YERockfish)
  
  #Copy the code above but use  "jitter(mat01)" 
  #in place of "mat01" to help separate the Y data...
  plot(jitter(mat01)~length, data=YERockfish)
  

#Identify/create length categories for the data; Use lencat() fxn from FSA package
?lencat  #look up the function to see what its arguments are
YERockfish = lencat(~length, data=YERockfish, startcat=30, w=2)
head(YERockfish)

#Make a table of proportions mature based on Length Category
tblLen <- with(YERockfish,table(LCat,maturity)) #this gives a table of counts of immature and mature fish by length category
tblLen
ptblLen <- prop.table(tblLen,margin=1) #calculate proportions by row
head(ptblLen)

#Make plot of proportion mature by 2 cm bins
lens <- as.numeric(rownames(ptblLen))  #make vector of length values from the rownames, but need to convert from character to numeric 
plot(ptblLen[,"Mature"]~lens,pch=16,xlab="Total Length (cm)",ylab="Proportion Mature")


### MODELING WITH RAW DATA ###
# Here, we will be fitting a logistic regression to our data. 
# Recall that the equation to predict maturity from length would be:
# pi = exp(b0+b1*x)/(1+exp(b0+b1*x))  
# where pi = the probability of being mature,
#       x = your independent variable (ie, Length)
#       b0 = the intercept parameter
#       b1 = the slope parameter for the effect of length

  ## Fitting the logistic regression model using logistic regression (Note that your response variable has to be binomial, with only 2 possible outcomes)
  glm1 <- glm(maturity~length, data=YERockfish, family=binomial)
  summary(glm1)
  coef(glm1)
  
  #Get 95% confidence intervals for estimated parameters
  confint(glm1)  #Confidence intervals for estimated parameters
  
  #Making predictions based on the model
    #A) STUDENT: Practice doing a hand-calculation of a prediction. Use the equation of pi above
    #   to calculate what the probability of maturity is if length = 32, using the
    #   parameters you estimated for b0 and b1.

    
    #B) Now, here's an example using the predict() function
    predict(glm1,data.frame(length=32),type="response")  #Need to Use type="response" to get the prediction of the probability of being mature instead of the logit
      ##How does this compare with your answer for A)
    
    #Make Predictions for a series of arbitrary lengths that you define 
    NewLengths = seq(min(YERockfish$length), max(YERockfish$length), by=2)  #set up a vector with the Length values you want a prediction for.
    pred.vals = data.frame(length=NewLengths) #make data frame with the "X" values you want to use for the prediction. CRITICAL: name the column the same thing as you used in your model.
    pred.vals$fit = predict(glm1, pred.vals, type="response") #make predictions for the sequence of length values.  Store as "fit" column

    #We can use 2*SE to get an approximate 95% Confidence interval.
    #But, we have to get the 95%CI on the "link" scale and then backtransform
    #the values to the "response" or "probability" scale.
    #The inverse function of the logit is: exp(x)/(1+exp(x)), and we will use that.
    pred.vals.link = predict(glm1, pred.vals, type="link", se=T) #make predictions for the sequence of length values on the "link" scale. Specify SE=T to output the standard errors for the predictions.  
    pred.vals.link #here we have the predictions and SE on the link scale.
    #calculate and store the upper confidence interval (UCI) and lower CI (LCI),
    #where x in the equation is fit+2*SE for the UCI and x is fit-2*SE for the LCI:
    pred.vals$UCI = exp(pred.vals.link$fit + 2*pred.vals.link$se.fit)/(1+exp(pred.vals.link$fit + 2*pred.vals.link$se.fit))
    pred.vals$LCI = exp(pred.vals.link$fit - 2*pred.vals.link$se.fit)/(1+exp(pred.vals.link$fit - 2*pred.vals.link$se.fit))

    #Plot model fit  
    #OPTION 1: Manual plots (using proportion mature or Raw, individual data)
      #Option 1a - Plot using proportion mature
      plot(ptblLen[,"Mature"]~lens,pch=16,xlab="Total Length (cm)",ylab="Proportion Mature")
        points(fit~length, data=pred.vals, type="l", col="red")
        points(LCI~length, data=pred.vals, type="l", lty=2, col="red")
        points(UCI~length, data=pred.vals, type="l", lty=2, col="red")

      #Option 1b - Plot using individual, raw maturity data
      plot(jitter(mat01, 0.2)~length, data=YERockfish, xlab="Total Length (cm)",ylab="Maturity (values jittered)")
        points(fit~length, data=pred.vals, type="l", col="red", lwd=2)    
        #note: you can add the approx. 95%CI as we did above.
        
      #Option 1c - Note you can use curve() function which plots a line based on an equation, instead of using the values we predicted above
        b0=coef(glm1)[1] #Store the b0 parameter from the model
        b1=coef(glm1)[2] #Store the b1 parameter from the model 
        
        plot(ptblLen[,"Mature"]~lens,pch=16,xlab="Total Length (cm)",ylab="Proportion Mature")
        curve(exp(b0+b1*x)/(1+exp(b0+b1*x)),add=T, col="blue" ) #Add a curve defined by the equation for the model fit
        #note: you can add the approx. 95%CI as we did above.
        
    # #OPTION 2: automatic plot from FSA package
    # #NOTE: THIS OPTION NO LONGER WORKS BECAUSE THE fitPlot() FUNCTION IS OUTDATED.
    # #      WOULD NEED TO GET THE CODE FROM GITHUB. (2/15/22)
    # fitPlot(glm1,xlab="Total Length (cm)",ylab="Proportion Mature",main="",xlim=c(25,70))
    # 
    #   #What are the gray points? How would you get rid of them?
    #   ?fitPlot

  #Calculating L50 (or other values)
    #OPTION 1 (preferred; easier)
      # A quick way for obtaining the length at which a specified proportion of
      # the population is mature is using the dose.p function, where p = proportion
      # mature. In the output, the "Dose" is really the X variable, which for us
      # is length in this case.
        library(MASS)
      L50<- dose.p(glm1,p=c(0.5))
      L50
      L95<- dose.p(glm1,p=c(0.95))
      L95

    #OPTION 2
      # Alternatively, the fitted parameters of the model can be re-arranged
      # so that the model can be parameterized in terms of the key parameters
      # of interest (the length at which 50% and 95% of population is mature)
      
      L50=(-glm1$coef[1] / glm1$coef[2]) #this equation is given in the maturity lecture
      L50
      L95<-(1/glm1$coef[2])*log(1/0.05-1) - glm1$coef[1]/glm1$coef[2]
      L95

### MODELING WITH SUMMARIZED DATA ###
# See the PDF file.
      
### COMPARING LOGISTIC REGRESSIONS BETWEEN GROUPS ###
# See the PDF file if you are interested in testing/comparing
# the maturity curves/fits between groups based on some factor
# or covariate.
      
      
      
### ANSWERS TO QUESTIONS ABOVE ###
      
      #Making predictions based on the model
      #A) STUDENT: Practice doing a hand-calculation of a prediction. Use the equation of pi above
      #   to calculate what the probability of maturity is if Length = 32.
      pi = exp(-16.94826+0.43718*32)/(1+exp(-16.94826+0.43718*32))
      pi        

      