#######################################################
# FISH 458/558 - Fish Population Dynamics 
# Lab 6a - Length-at-age  
# A. Buchheister (based on material by R.J. Latour)
# February 2024
#######################################################

#Online recording of Andre talking through the lab script:
https://humboldt.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=0973af64-221c-4fc4-85db-b118000117f1&start=10

#Today
# - General comments/announcements
#   - Make sure you are keeping notes in your lab file
#   - questions about HW?
#   - Questions about exam?
# - Do Length-at-age lab
# - Do Maturity lab


##MAIN GOALS/OBJECTIVES FOR THIS SCRIPT##
# - Plot and visualize length at age data using real data
# - Fit a Von Bertalanffy model to the data
# - Make predictions of length based on age
# - Compare 2 alternative model fits using AIC

#NOTE: there is a nice vignette for the von Bertalanffy model
# located here: http://derekogle.com/fishR/examples/oldFishRVignettes/VonBertalanffy.pdf
# This is included in the lab folder for reference, but I do
# not refer to it specifically down below.

setwd("C:/Users/ab4577/HSU/Teaching/PopDy/2024_Spring/Labs/Lab 06 - VonBert, Maturity")
cobia=read.csv("Cobia_Length-at-age.csv")
head(cobia)

#plot length-at-age data
L=cobia$Length
age=cobia$Age

par(mfrow=c(1,1))

#all fish
plot(age,L,xlab='Age (yrs)',ylab='Length (cm)',xlim=c(0,max(age)))



###########################################
# von Bertalanffy - nonlinear least squares
# Additive error structure
###########################################

#Von Bertalanffy model:
# L = Linf(1-exp(-k*(Age-to)))
# Where Linf, k, and to are parameters


#Fit von bert model using nls
mod1=nls(Length~Linf*(1-exp(-k*(Age-to))), data=cobia, 
         start=list(Linf=1000,k=5,to=5), trace=T)
summary(mod1,correlation=T)

  #IF you get this error (or something like it):
    #Error in nlsModel(formula, mf, start, wts) : singular gradient 
    #Then you need to adjust your starting values. 
    
    #STUDENT: Write yourself notes on what the nls function is doing, 
    #and what the parts are, including what "starting values" are!

    #STUDENT: Using your understanding of the parameters, try adjusting
    #the starting values (these are the numbers inside the "list(...)" 
    #statement.  How can you determine what some reasonable starting values would be?
    


#Generate predicted Lengths for the model
  #Making a prediction for an age 4 fish
    predict(mod1, data.frame(Age=4))  
    
  #Making predictions for all observed values
    pred1=predict(mod1)
  
  #Let's add the predictions to the dataset:
    cobia$pred.length = pred1

#Plot the model fit to visualize
par(mfrow=c(1,1))
plot(age,L,xlab='Age (yrs)', ylab='Length (cm)', main="Von Bert Model for Cobia")
lines(age,pred1,lwd=2, col='blue')

  ##NOTE: if you get a very funky/jagged prediction, you probably 
  #didn't sort your data by age before you started!

  #STUDENT: What is the predicted mean age of a fish that is 6 years old?

  #STUDENT: If a fish is 120 cm long, about how old would you expect the fish to be based on the model?
  #         How old could that fish be based on the observed data?


#diagnostics for assumption evaluation
#define residuals
r1=resid(mod1)

#diagnostic plots

#histogram of residuals
par(mfrow=c(2,2))

hist(r1,xlab='Residual', prob=TRUE) 
curve(dnorm(x,mean=mean(r1), sd=sd(r1)), col='red',add=TRUE)

#qqplot
qqnorm(r1)
qqline(r1,lwd=2)

#plot residuals
plot(r1~age,ylab='Residual',xlab='Age')
abline(0,0,lwd=2)

plot(r1~fitted(mod1),ylab='Residual',xlab='Fitted')
abline(0,0,lwd=2)

#STUDENT: What are the assumptions we should be checking here?

#STUDENT: Do you think the assumptions are met?  Why or why not?

#STUDENT: Talk about the difference between the residual vs. age plot
#         and the residual vs. fitted plot. Why are they so different?




###########################################
# Gompertz Model - nonlinear least squares
# Additive error structure
###########################################

#Gompertz model:
# L = L0*exp((gamma/k)*(1-exp(-k*Age)))
# Where L0, gamma, and k are parameters

mod2=nls(Length~L0*exp((gamma/k)*(1-exp(-k*Age))), data=cobia, start=list(L0=50,gamma=0.4,k=0.4), trace=T)
summary(mod2,correlation=T)


#Generate predicted Lengths for each model
pred2=predict(mod2)

#Plot the two model fits together to visualize
par(mfrow=c(1,1))
plot(age,L,xlab='Age (yrs)', ylab='Length (cm)', main="Gompertz Model Fits for Cobia")
lines(age,pred2,lwd=2, col='red')



#diagnostics for assumption evaluation
#define residuals

r2=resid(mod2)


#diagnostic plots

#histogram of residuals
par(mfrow=c(2,2))

hist(r2,xlab='Residual', prob=TRUE) 
curve(dnorm(x,mean=mean(r2), sd=sd(r2)), col='red',add=TRUE)

#qqplot
qqnorm(r2)
qqline(r2,lwd=2)

#plot residuals
plot(r2~age,ylab='Residual',xlab='Age')
abline(0,0,lwd=2)

plot(r2~fitted(mod2),ylab='Residual',xlab='Fitted')
abline(0,0,lwd=2)

#Normality seems ok, perhaps slight positive skew
#Some evidence of non-constant variance, although not overwhelming
#Will proceed with the model


#### MODEL COMPARISON AND SELECTION ####

#Plot the two model fits together to visualize
par(mfrow=c(1,1))
plot(age,L,xlab='Age (yrs)', ylab='Length (cm)', main="Growth Model Fits for Cobia")
lines(age,pred1,lwd=2, col='blue')
lines(age,pred2,lwd=2, col='red')
legend("bottomright", c("Von Bertalanffy", "Gompertz"), lty=1, lwd=2, col=c("blue","red"))

#Can use Akaike's Information Criterion (AIC) to compare models, but this is only valid
#if the models are fit to the exact same data (which is what we have here).
#Lower AIC values indicate a better model; If AICs different by 2, then both reasonable.
#If AIC differs by >10, then essentially no support for the model with higher AIC.
AIC(mod1, mod2)

#Can estimate the 95% confidence interval for the parameters
confint(mod1, level=0.95)
confint(mod1, parm="Linf", level=0.95) #if you want CI for just one parameter


