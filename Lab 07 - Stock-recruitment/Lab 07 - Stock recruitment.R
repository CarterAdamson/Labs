#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# FISH 458/558 - Fish Population Dynamics 
# Lab 7 - Stock-recruitment models  
# A. Buchheister (based on material by R.J. Latour)
# February 2021
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Today
# - Student presentation 
# - Go through SR lab script
#    - Note there are some posted slides on AIC.
# - Debrief about In-class Exam
# - Work on HW if time is available


 
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# GOALS FOR TODAYS LAB
# - Fit various stock-recruit models to data (using data for pink salmon)
# - Evaluate model assumptions of normality and HOV
# - Select the "best" SR model using AIC
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#read in data
setwd("C:/Users/ab4577/HSU/Teaching/PopDy/2024_Spring/Labs/Lab 07 - Stock-recruitment")
#setwd("G:/My Drive/ALL_CLASSES/FISH 458-558/FISH 458_2019/Lab 06 - Stock-recruitment")
salmon=read.csv('pink_salmon.csv')
head(salmon)

#DATE YOUR DATA: Plot the # of recruits through time, then
#add a red line to denote the spawners through time
  #What do you notice?
  salmon=salmon[order(salmon$Year),]
  plot(Recruits~Year, data=salmon, type="l", ylab="Recruits (black) or Spawners (red)")
  lines(Spawners~Year, data=salmon, col="red")

#Sort dataset by spawners (our "x" variable) to facilitate plotting later
#(If this isn't done, you can get a "jagged" curve when you are plotting the model fit later)
salmon=salmon[order(salmon$Spawners),]
head(salmon)

#Store data into individual vectors for easier plotting & use
S=salmon$Spawners
R=salmon$Recruits

plot(S,R, pch=16, col='salmon')

# Not much data at high stocks levels
# What type of stock recruitment model do you think would be best?



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Density Independent model ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#Model with multiplicative error
# R = a*S*exp(e) 
#   where a=productivity parameter
#         e=error

#Log-transformed version of model for fitting with nls
# log(R) = log(a*S)+e

#Fit log-transformed model version, using nonlinear least squares
mod1=nls(log(R)~log(a*S),start=c(a=2.0),trace=T)
summary(mod1)

#develop predictions - remember to bias correct for back transformation
  #Calculate standard error of estimate (SEE); aka residual standard error
    SEE.mod1=sqrt(sum((resid(mod1)^2))/(length(R)-1))  #Option 1 - Calculate manually
    SEE.mod1=summary(mod1)$sigma  #Option 2 (Easier) - get from model output

  #calculate predictions for every data point
    pred.mod1=exp(predict(mod1))*exp((SEE.mod1^2)/2)
    #Note: predict(mod1) gives the model predictions on the log scale.
    #We exponentiate that prediction (exp(predict(mod1))) to "backtransform"
    #and get it back to the raw scale.
    #The bias correction means we multiply that quantity by a correction factor (CF)
    #where CF = exp((SEE.mod1^2)/2), as shown in the lecture notes.
    
#Add a line to the SR plot
  plot(S,R, pch=16, col='salmon')
  lines(S,pred.mod1,col='purple',lwd=2)

#STUDENT (EXTRA) - Generate a plot of R/S vs. S using the raw data. 
#Add a purple line for the predicted R/S vs. S.  What does this indicate 
#about survival and density dependence?

  
  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# BH model   R~(a*S)/(1+b*S) -----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  

#BH Model with multiplicative error
# R = (a*S)/(1+b*S)*exp(e) 
#   where a=productivity parameter
#         b=density dependence parameter
#         e=error

#Log-transformed version of model for fitting
# log(R)~log(a*S/(1+b*S))+e

#Assume multiplicative error, and fit log-transformed model version
mod2=nls(log(R)~log(a*S/(1+b*S)), start=c(a=1.0,b=5e-5), trace=T)
summary(mod2)
 
    ### SIDEBAR: Finding good starting values ###

      #STUDENT: Play around with the starting values to try and 
      #get an error message.  What was that error message? 
  
      #Sometimes your model won't work because your starting values are
      #so far away from the best fit that R has problems.  You need to 
      #find more reasonable starting values.  

      #Here are some strategies for finding a better set of starting values.
      #1) Use trial and error.  Simply rerun your model using different starting values  
      #   by changing the numeric values in the "start=c(a=1.0,b=5e-5)" part of the code.
      #   For this, i recommend changing by orders of magnitude (e.g., try a=0.1, 1.0, 10, etc.).
      #   Remember that parameters can go up or down! Some parameters can also be negative (although
      #   that is not the case here). 

      #2) Generate a plot using the starting values and "eyeball" decent starting values.
      #   For this, you create a plot using your starting values and tweak them until the plot
      #   goes through (or near) the actual data points.  Here is an example:

        a.test = 50    #possible starting value for a
        b.test = 0.01   #possible starting value for b
        S.test = c(0:max(S))  #creating a vector for the "X" axis that goes from 0 to the max observed S value
        R.test = a.test*S.test/(1+b.test*S.test)  #calculate the predicted Recruits using the parameters     
        
        plot(S.test, R.test, type="l", col='red', lwd=3, ylim=c(0,max(S,S.test))) #Create a plot of the prediction based on the starting values
        points(S,R, pch=16, col="black")  #Add in the points
          #you would keep running these 6 lines of code by tweaking a.test and b.test values, 
          #until the model goes through teh points.  Then, use those values as your starting values 
          #in your nls() function.
  
      


### Evaluating model assumptions ###

  #theory says R values are lognormal with constant variance, do diagnostics support this assumption?
    #If this is the case, we would expect the residuals from the log-transformed model to be
    #normally distributed with HOV. 
    #(Remember: lognormal distributions become normal when log transformed.)

#store residuals
r2=resid(mod2)

#Shapiro-Wilk test for normality
shapiro.test(r2)

#diagnostic plots

  #histogram of residuals
  par(mfrow=c(2,2))
  
  hist(r2,xlab='Residual', prob=TRUE) 
  curve(dnorm(x,mean=mean(r2), sd=sd(r2)), col='red',add=TRUE)
  #Note the histogram is plotting the 
  #probability density as opposed to the frequency, but this 
  #has no bearing on the shape or conclusions.
  
  #qqplot
  qqnorm(r2,ylim=c(-2,2))
  qqline(r2,lwd=2)
  
  #plot residuals
  plot(r2~S,ylab='Residual',xlab='Spawners')
  abline(0,0,lwd=2)
  
  plot(r2~fitted(mod1),ylab='Residual',xlab='Fitted')
  abline(0,0,lwd=2)

#STUDENT: Do our assumptions of Normality and HOV seem justified?


#develop predicted model (see more thorough notes in the Density Independent section above)
  #Calc. the standard error of the estimate, SEE (aka residual standard error)
  SEE.mod2=sqrt(sum((resid(mod2)^2))/(length(R)-2)) #option 1
  SEE.mod2=summary(mod2)$sigma  #Option 2 (easier)
  
  #Back transform (ie, exponentiate) predictions, and 
  #do the bias correction (ie, multiply by the correction factor).
  pred.mod2=exp(predict(mod2))*exp((SEE.mod2^2)/2)
  
    #Alternative for getting predictions: calculate predictions from model parameters instead of using predict()
      summary(mod2)
      a2=coef(mod2)[1] 
      b2=coef(mod2)[2] 
      pred.mod2a=(a2*S)/(1+b2*S)*exp((SEE.mod2^2)/2)  #Note this is the same approach as before

  #make plot
  par(mfrow=c(1,1))
  plot(S,R, pch=16, col='salmon')
  lines(S,pred.mod1,col='purple',lwd=2)
  lines(S,pred.mod2, col='blue', lwd=2)

  
#STUDENT (EXTRA) - Generate a plot of R/S vs. S using the raw data. 
#Add a line for the predicted R/S vs. S.  What does this indicate 
#about survival and density dependence?
  plot(R/S~S, main="R/S Graph")
  lines(pred.mod1/S~S, col="purple")  
  lines(pred.mod2/S~S, col="blue") 
  
  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#Ricker model -----
#R=a*S*exp(-b*S)
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
  
#Ricker Model with multiplicative error
# R = a*S*exp(-b*S)*exp(e) 
#   where a=productivity parameter
#         b=density dependence parameter
#         e=error

#Log-transformed version of model for fitting with nls
# log(R)=log(a)+log(S)-b*S+e

#Assume multiplicative error, and fit log-transformed model version
mod3=nls(log(R)~log(a*S*exp(-b*S)), start=c(a=1,b=0.00005), trace=T)
summary(mod3)

#You could/should run diagnostics to evaluate assumptions, as we did with BH model...
#But we won't here.


#Calculate the fitted model (with bias-correction); see notes for D.I. and BH Models
SEE.mod3=summary(mod3)$sigma

pred.mod3=exp(predict(mod3))*exp((SEE.mod3^2)/2)

lines(S,pred.mod3, col='red',lwd=2)

  #Plot
  par(mfrow=c(1,1))
  plot(S,R, pch=16, col='salmon')
  lines(S,pred.mod1,col='purple',lwd=2)
  lines(S,pred.mod2, col='blue', lwd=2)
  lines(S,pred.mod3, col='red', lwd=2)


#STUDENT (EXTRA) - Generate a plot of R/S vs. S using the raw data. 
#Add a line for the predicted R/S vs. S.  What does this indicate 
#about survival and density dependence?
  plot(R/S~S, main="R/S Graph")
  lines(pred.mod1/S~S, col="purple")  
  lines(pred.mod2/S~S, col="blue")   
  lines(pred.mod3/S~S, col="red") 
  
  


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Redraw plot with all models ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
plot(S,R, pch=16, col='salmon', main="Stock-Recruit Curves (Pink Salmon)")
lines(S,pred.mod1,col='purple',lwd=2)
lines(S,pred.mod2, col='blue', lwd=2)
lines(S,pred.mod3, col='red',lwd=2)
# lines(S,hpred,col='darkgreen',lwd=2)
legend("bottomright", c("D-I","BH","Ricker","Hockey"),
       col=c("purple","blue","red","darkgreen"), lwd=2)

plot(R/S~S, main="R/S Graph")
lines(pred.mod1/S~S, col="purple")  
lines(pred.mod2/S~S, col="blue")   
lines(pred.mod3/S~S, col="red")
# lines(hpred/S~S, col="darkgreen") 


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Model comparison using AIC ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#How do we pick which of the 3 models (Density independent, Beverton Holt, or Ricker)
#is the best?
#  One way is to use Akaike's Information Criterion (AIC).  The model with the lowest AIC 
#  is the best one for the data set.  AIC is a metric that gets lower with a better model fit
#  but AIC penalizes models if they are more complex, so it represents a balance between model 
#  fit and model complexity.


#List AICs (Smaller is better)
AICs=c(AIC(mod1),AIC(mod2),AIC(mod3))

#Delta AICs (0 is the best model)
# Delta AICs are the difference of each AIC from the AIC of the "best" model (which has the lowest AIC)
# See the "Lab06 - AIC background.pdf" for a table of how to interpret different Delta AIC values.
dAICs = AICs-min(AICs) 

#Calculate Akaike weights for each model (this gives the "weight of evidence” in favor of model i being the actual best model for the situation at hand given that one of the R models must be the best)
w.AIC = exp(-0.5*dAICs)/sum(exp(-0.5*dAICs))

#k (number of parameters, including sigma) - this represents the complexity of each model
k = c(nrow(summary(mod1)$coef)+1,
      nrow(summary(mod2)$coef)+1,
      nrow(summary(mod3)$coef)+1)  

#AICc - This is an AIC that is corrected for small sample size. 
#The interpretation is the same as AIC above.
library(MuMIn) #install the package first if you need to.
AICc = AICc(mod1, mod2, mod3) 
dAICc = AICc[,2]-min(AICc[,2])
w.AICc = exp(-0.5*dAICc)/sum(exp(-0.5*dAICc))


#Make table
AIC.table = data.frame(Model=c("mod1","mod2","mod3"),
                       Name=c("D-I","BH","Ricker"),
                       k = k,
                       AIC = AICs,
                       dAIC= dAICs,
                       AICc = AICc$AICc,
                       dAICc= dAICc,
                       w.AIC = w.AIC,
                       w.AICc= w.AICc)
AIC.table

#Here, the best model is the Density Independent model because 
#it has the lowest AIC, and its dAIC=0. It also has the highest
#AIC weight. However, all the dAICs 
#are all less than 2 indicating that there is "substantial" empirical
#support for each of the models, and we see reasonable AIC weights for those
#models as well (around 0.24).  In other words, while the D-I is the best
#it is not far better than the others.
# See the "Lab07 - AIC background.pdf" for a table of how to interpret different Delta AIC values.






#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# EXTRA MATERIAL: Hockey stick model  ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Hockeystick model with multiplicative error
# R = a*S*exp(e)       if S<s.star
#   = a*s.star*exp(e)  if S>=s.star
#   where a=productivity parameter
#         s.star=pivotal spawner level
#         e=error

#Create vector with parameters a and s.star, but log them bc this will allow 
#us to estimate the parameters in log space (which is easier for the computer)
p=log(c(2.5,3800)) 
predR=numeric(length(R)) #Create vector of length(R) to fill in with predicted R

#Create function that generates predictions and then calculates and 
#returns the residual sum of squares
mod4=function(p){
  a=exp(p[1])
  s.star=exp(p[2])
  
  predR=ifelse(S <= s.star, a*S, a*s.star)
  
  resids=(log(R)-log(predR))^2
  SS=sum(resids)
  return(SS)
}  

mod4.est=optim(p,mod4)
mod4.est

#extract and exp parameters since estimated in log space
a=exp(mod4.est$par[1])
a
s.star=exp(mod4.est$par[2])
s.star

#subset data above and below s.star
S1=subset(salmon,Spawners<s.star)
S2=subset(salmon,Spawners>=s.star)

#Calculate predicted values
hpred1=a*(S1$Spawners)
hpred2=a*rep(s.star,length=nrow(S2))

SEE.mod4=sqrt(mod4.est$value/(length(R)-2)) #SEE for bias correction

hpred=c(hpred1,hpred2)*exp((SEE.mod4^2)/2) #bias-corrected predictions for both parts of hockeystick


#Calculate AIC for hockey model using RSS
n=length(R)
RSS=mod4.est$value
AIC.mod4=n*(1+log(2*pi*(RSS/n)))+2*(length(mod4.est$par)+1)


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# CODE FOR STUDENT SECTION  ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@


#STUDENT--> DATE YOUR DATA: Plot the # of recruits through time, then
#add a red line to denote the spawners
#What do you notice?
salmon=salmon[order(salmon$Year),]
plot(Recruits~Year, data=salmon, type="l")
lines(Spawners~Year, data=salmon, col="red")

#STUDENT (EXTRA) - Generate a plot of R/S vs. S using the raw data. 
#Add a line for the predicted R/S vs. S.  What does this indicate 
#about survival and density dependence?
plot(R/S~S, main="R/S Graph")
lines(pred.mod1/S~S, col="purple")


#STUDENT (EXTRA) - Generate a plot of R/S vs. S using the raw data. 
#Add a line for the predicted R/S vs. S.  What does this indicate 
#about survival and density dependence?
plot(R/S~S, main="R/S Graph")
lines(pred.mod1/S~S, col="purple")  
lines(pred.mod2/S~S, col="blue") 
