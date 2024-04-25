#...................................................................................
# FISH 458/558 - Fish Population Dynamics 
# Lab 14 - Production models  
# A. Buchheister (based on material by R.J. Latour)
# Apr 2020
#...................................................................................



#TODAY (Lab will be recorded b/c of campus disruption)
# - Review Take home Exam
#   - AIC interpretation and "significance"
#   - If you struggled on this, please see me.
# - Presentation (postponed to next week)
# - Lab Script
#     - Recap MSY, Bmsy, surplus production, etc.
# - Lab 14 HW - due next week (last HW)
# - Extra Credit opportunity for 458:
#   - Review of a real stock assessment (see Canvas)


#Set your working directory to Source file location

hake=read.csv(here("Lab 14 - Surplus production models","Namibian_Hake_data.csv"))
head(hake)


#Quick exploration: Plot Catch, Effort, and CPUE as a function of Year.
#How do you think the stock is doing?
par(mfrow=c(1,1))
plot(Catch~Year, data=hake, type="b")
plot(Effort~Year, data=hake, type="b")
plot(CPUE~Year, data=hake, type="b")




#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Non-equilbrium, time-series fitting ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Make sure data is ordered by year for time-series fitting
hake=hake[order(hake$Year),]

year=hake$Year
Ct=hake$Catch
Et=hake$Effort
It=hake$CPUE

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## Observation error method ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Create empty vector to store Bt values
#Make Bt have a length of: length(year)+1 since we have B(t+1)
Bt=numeric(length(year)+1)

#Create a parameter vector (this includes all parameters to be estimated)
#Parameters to be estimated: K, q, r, sigma (we will be Assuming B0=K)
p=log(c(max(Ct)*10, 0.01, 0.1, 0.1)) #Our starting guess for K is max(Ct)*10
p=log(c(max(Ct)*100, 0.001, 0.01, 0.1)) #alternative starting values (Note: these will work for the HW)
  #why log the parameters?
  # - will prevent negative values for parameters
  # - helps with differing magnitudes of parameters

#Create a Function in R to calculate the negative log likelihood, based on given parameters
L1=function(p){
    K=exp(p[1])  #Note we have to exponentiate the logged parameters to get them to the correct scale
    q=exp(p[2])
    r=exp(p[3])
    sigma=exp(p[4])

#generate predicted biomasses using Schaefer population model
      for(i in 1:(length(year))){
        Bt[1]=K  #initial B = B0 = K  (we are assuming B0=K to simplify the model; note that mgmt conclusions are not sensitive to this assumption in this case)
        Bt[i+1]=Bt[i]+(r*Bt[i]*(1-Bt[i]/K))-Ct[i]  #B(t+1) deterministically calculated using model (b/c no error here)
      }

#model-predicted relative abundance: Ipred=q*Bt
      Ipred=q*Bt[1:length(year)]
      negLL=log(sigma)+0.5*((log(It)-log(Ipred))/sigma)^2 #simplified negLL; Note the "observed-predicted" part
      sum.negLL=sum(negLL)
      return(sum.negLL)
}

#Find the BEST parameter estimates that minimize the negative Log Likelihood
#The optim() function minimizes the negLL by changing the values in the parameter (p) vector 
  ?optim #look at optim description (specifically the first 2 arguments)
mod1=optim(p, L1, method="BFGS", hessian=T)
warnings() #use this to see the warnings
  #Note, we will disregard warnings; these were caused by R trying to take a log of a negative number when 
  #trying to optimize function.  But, always make sure the model fit is reasonable.
mod1
  #This stores results from the optimization function.  Some key things:
  # - par - the parameter values that minimize the objective function
  # - value - the value for the objective function (ie, the negLL here)
  # - convergence - see help, but a value of 0 means SUCCESS!
  # - hessian - the matrix of second order partial derivatives --> used to estimate SEs.

#back transform parameters (because we estimated them in log space)
mod1.par=exp(mod1$par)

#Specify the specific parameters (same order as before)
K=mod1.par[1]
q=mod1.par[2]
r=mod1.par[3]
sigma=mod1.par[4]

#Calculate MSY quantities based on estimated parameters
Cmsy=(r*K)/4
umsy=r/2
Emsy=r/(2*q)
Bmsy=K/2


#generate predicted Bt and It based on the fitted parameters
Ipred1=numeric(length(year))
  
for(i in 1:(length(year))){
  Bt[1]=K
  Bt[i+1]=Bt[i]+(r*Bt[i]*(1-Bt[i]/K))-Ct[i]

  Ipred1[i]=q*Bt[i]
}  

    
### PLOTS FOR OBSERVATION ERROR RESULTS ###   
#What plots would be informative for management?

#BREAK OUT GROUPS: Discuss the meaning and 
#interpretation of the next 5 plots:

#plot model fit
par(mfrow=c(1,1))
plot(year,It,xlab='',ylab='Hake CPUE', main="Mod Fit")
lines(year,Ipred1,lwd=3,col="blue")
    legend("topright", c("Observed","Obs.Error Model"), pch=c(1,NA),lty=c(0,1), col=c("black","blue"))
  
par(mfrow=c(2,2))
#plot Biomass and Bmsy
Bt.plot=Bt[-length(Bt)] #Bt has 1 extra value, so we remove the last value
plot(year,Bt.plot,xlab='Year',ylab='Biomass (tons)', lwd=2,type="l")
abline(h=Bmsy,lwd=2,col="red")
#biomass graph shows evidence of overfished pop in the 70s

#plot Effort and Emsy
plot(year,Et,xlab='Year',ylab='Effort (vessel days)', lwd=2,type="l")
abline(h=Emsy,lwd=2,col="red")

#plot C and MSY
plot(year,Ct,xlab='Year',ylab='Catch', lwd=2,type="l")
abline(h=Cmsy,lwd=2,col="red")
#catch graph shows evidence that catch is just below MSY in the 80s

#plot exploitation rate (u) and umsy (where u=Catch/Biomass)
plot(year,Ct/Bt.plot,xlab='Year',ylab='Exploitation rate', lwd=2,type="l")
abline(h=Cmsy/Bmsy,lwd=2,col="red")
#exploitation graph shows evidence of overfishing in the 70s
  

#BREAK OUT GROUPS: Discuss the meaning and 
#interpretation of the next 2 plots:

#plot C vs. Biomass (with surplus production curve)
par(mfrow=c(1,2))   
  Surp.Prod = data.frame(Biomass = seq(0,K,length=100))
  Surp.Prod$SP.1 = r*Surp.Prod$Biomass*(1-(Surp.Prod$Biomass/K)) #Calculate surplus production
  
  plot(Bt.plot,Ct,xlab='Biomass (tons)',ylab='Catch (tons)', lwd=2,type="b",
       xlim=c(0,K*1.1),ylim=c(0,max(Ct)))
  lines(SP.1~Biomass, data=Surp.Prod,lwd=2,col="blue")
  text(Bt.plot[1], Ct[1], labels=year[1], pos=1)
  legend("topleft", c("Observed","Surp. Production"), pch=c(1,NA),
         cex=0.8,lty=c(0,1), lwd=2, col=c("black","blue"))
  #bmsy as a vertical line intersecting the peak of the production
  #overfished on either side
  #above blue line leads to pop decline
  #below blue line leads to increase pop

#plot C vs. Effort (with production curve)
  Surp.Prod$Et.SP.1 = Surp.Prod$SP.1 / (q*Surp.Prod$Biomass)  #if C/E = q*B, and we set C = SP, then we can calc the E to achieve SP as E = SP/(q*B) 
  Surp.Prod.Et=Surp.Prod[order(Surp.Prod$Et.SP.1),]
  
  plot(Et,Ct,xlab='Effort (vessel days)',ylab='Catch (tons)', lwd=2,type="b",
       xlim=c(0,max(c(Et,na.omit(Surp.Prod$Et.SP.1)))),ylim=c(0,max(Ct)))
  lines(SP.1~Et.SP.1, data=Surp.Prod.Et,lwd=2,col="blue")
  text(Et[1], Ct[1], labels=year[1], pos=1)
  legend("topleft", c("Observed","Surp. Production"), pch=c(1,NA),
         cex=0.8,lty=c(0,1), lwd=2, col=c("black","blue"))
  #emsy as vertical line intersecting peak of production curve
  #msy are horizontal line intersecting peak
  #not possible to see overfishing, but at least above and below emsy
  #effort is a function of the fishery, so not really possible to predict increase/decline
  



    
 # ##MISC CODE FOR EXPLORING STARTING PARAMETERS   
 #    #Create empty vectors to store I and B
 #    Ipred.start=numeric(length(year))
 #    Bt.start=numeric(length(year)+1)
 # 
 #    #Create vector of initial parameter guesses, which are logged
 #      p=log(c(max(Ct)*10, 0.01, 0.1, 0.1)) #Our starting guess for K is max(Ct)*10
 #        K.start=exp(p[1])
 #        q.start=exp(p[2])
 #        r.start=exp(p[3])
 #    #Run loop to calculate CPUE index (It) for each year based on initial parameters
 #      for(i in 1:(length(year))){
 #        Bt.start[1]=K.start
 #        Bt.start[i+1]=Bt.start[i]+(r.start*Bt.start[i]*(1-Bt.start[i]/K.start))-Ct[i]
 #        Ipred.start[i]=q.start*Bt.start[i]
 #      }
 #    #Make plot
 #      plot(year,It,xlab='',ylab='CPUE', ylim=c(0,max(Ipred.start)))
 #      lines(year,Ipred.start,lwd=3,col="red")
 #      #legend("topright", c("Observed","Predicted"), pch=c(1,NA),lty=c(0,1), col=c("black","red"))
 #  





#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## process error   -----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Create empty vector to store B estimates
Bt=numeric(length(year)+1)
    
#parameter vector
#K, q, r, sigma
p=log(c(max(Ct)/0.1, 0.0001, 0.1, 0.1))

L2=function(p){
  K=exp(p[1])
  q=exp(p[2])
  r=exp(p[3])
  sigma=exp(p[4])
  
  #generate predicted indices
  for(i in 1:(length(year))){
    Bt[1]=It[1]/q
    Bt[i+1]=It[i]/q+(r*(It[i]/q)*(1-It[i]/(q*K)))-Ct[i]
  }
  
  #model-predicted relative abundance: Ipred=qBt
  Ipred=q*Bt[1:length(year)]
  negLL=log(sigma)+0.5*((log(It)-log(Ipred))/sigma)^2  #simplified negLL;
  sum.negLL=sum(negLL)
  return(sum.negLL)
}

mod2=optim(p,L2,method="BFGS",hessian=T)

#back transform parameters
mod2.par=exp(mod2$par)

K2=mod2.par[1]
q2=mod2.par[2]
r2=mod2.par[3]
sigma2=mod2.par[4]

#MSY quantities
Cmsy2=(r2*K2)/4
umsy2=r2/2
Emsy2=r2/(2*q2)
Bmsy2=K2/2


#generate predicted It
Ipred2=numeric(length(year))

for(i in 1:(length(year))){
  Bt[1]=It[1]/q2
  Bt[i+1]=It[i]/q2+(r2*(It[i]/q2)*(1-It[i]/(q2*K2)))-Ct[i]
  
  Ipred2[i]=q2*Bt[i]
}

#plot model fit
par(mfrow=c(1,1))
plot(year,It,xlab='',ylab='Hake CPUE')
lines(year,Ipred1,lwd=3,col="blue")
lines(year,Ipred2,lwd=3,col="red")
legend("topright", c("Observed","Prediction (Obs. error model)","Prediction (Proc. error model)"), pch=c(1,NA),
       cex=0.8,lty=c(0,1,1), lwd=2, col=c("black","red","blue"))

par(mfrow=c(1,2))  
#plot C vs. Biomass (with surplus production curve)
Surp.Prod2 = data.frame(Biomass = seq(0,K2,length=100))
Surp.Prod2$SP.2 = r2*Surp.Prod2$Biomass*(1-(Surp.Prod2$Biomass/K2)) #Calculate surplus production

plot(Bt[-length(Bt)],Ct,xlab='Biomass (tons)',ylab='Catch (tons)', lwd=2,type="b",
     xlim=c(0,max(K2,Bt)),ylim=c(0,max(Ct)))
lines(SP.2~Biomass, data=Surp.Prod2, lwd=2, col="red")
legend("topright", c("Observed","Surp. Production"), pch=c(1,NA),
       cex=0.8,lty=c(0,1), lwd=2, col=c("black","red"))

#plot C vs. Effort (with production curve)
Surp.Prod2$Et.SP.2 = Surp.Prod2$SP.2 / (q*Surp.Prod2$Biomass) #if C/E = q*B, and we set C = SP, then we can calc the E to achieve SP as E = SP/(q*B) 
Surp.Prod2.Et=Surp.Prod2[order(Surp.Prod2$Et.SP.2),]

plot(Et,Ct,xlab='Effort (vessel days)',ylab='Catch (tons)', lwd=2,type="b",
     xlim=c(0,max(c(Et,na.omit(Surp.Prod$Et.SP.2)))),ylim=c(0,max(Ct)))
lines(SP.2~Et.SP.2, data=Surp.Prod2.Et,lwd=2,col="red")
legend("topleft", c("Observed","Surp. Production"), pch=c(1,NA),
       cex=0.8, lty=c(0,1), lwd=2, col=c("black","red"))




#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
## total error -----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

Bt1=numeric(length(year)+1)
Bt2=numeric(length(year)+1)
#parameter vector
#K, q, r, sigma1, sigma2
p=log(c(max(Ct)/0.1,0.0001,0.1,0.1,0.1))

#likelihood weights
w1=1.0
w2=1.0

L3=function(p){
  K=exp(p[1])
  q=exp(p[2])
  r=exp(p[3])
  sigma1=exp(p[4])
  sigma2=exp(p[5])
  
  #observation model
  #generate predicted biomasses
  for(i in 1:(length(year))){
    Bt1[1]=K
    Bt1[i+1]=Bt1[i]+(r*Bt1[i]*(1-Bt1[i]/K))-Ct[i]
  }
  
  #model predicted relative abundance: Ipred=qBt
  Ipred1=q*Bt1[1:length(year)]
  negLL1=log(sigma1)+0.5*((log(It)-log(Ipred1))/sigma1)^2
  sum.negLL1=sum(negLL1)
  
  #process model
  for(i in 1:(length(year))){
    Bt2[1]=It[1]/q
    Bt2[i+1]=It[i]/q+(r*(It[i]/q)*(1-It[i]/(q*K)))-Ct[i]
  }
  
  #model-predicted relative abundance: Ipred=qBt
  Ipred2=q*Bt2[1:length(year)]
  negLL2=log(sigma2)+0.5*((log(It)-log(Ipred2))/sigma2)^2
  sum.negLL2=sum(negLL2)
  total.negLL=w1*sum.negLL1+w2*sum.negLL2
  return(total.negLL)
}

mod3=optim(p,L3,method="BFGS",hessian=T)

#back transform parameters
mod3.par=exp(mod3$par)

K3=mod3.par[1]
q3=mod3.par[2]
r3=mod3.par[3]
sigma1.3=mod3.par[4]
sigma2.3=mod3.par[5]


Cmsy3=(r3*K3)/4
umsy3=r3/2
Emsy3=r3/(2*q3)
Bmsy3=K3/2

#generate predicted It
Ipred3=numeric(length(year))

for(i in 1:(length(year))){ 
  
  #obs error prediction
  Bt1[1]=K3
  Bt1[i+1]=Bt1[i]+(r3*Bt1[i]*(1-Bt1[i]/K3))-Ct[i]
    
  Ipred1[i]=q3*Bt1[i]
   
  #process error prediction
  Bt2[1]=It[1]/q3
  Bt2[i+1]=It[i]/q3+(r3*(It[i]/q3)*(1-It[i]/(q3*K3)))-Ct[i]
  
  Ipred2[i]=q3*Bt2[i]
  
  #overall
  Ipred3=0.5*Ipred1+0.5*Ipred2
  Bt3 = 0.5*Bt1+0.5*Bt2
}

#model fits
par(mfrow=c(1,1))
plot(year,It,xlab='',ylab='Hake CPUE', main="Surplus production model fits to CPUE")
lines(year,Ipred3,lwd=3,col="darkgreen")


#plot C vs. Biomass (with all surplus production curves)
  Surp.Prod3 = data.frame(Biomass = seq(0,K3,length=100))
  Surp.Prod3$SP.3 = r3*Surp.Prod3$Biomass*(1-(Surp.Prod3$Biomass/K3)) #Calculate surplus production
  
  par(mfrow=c(1,2))  
  plot(Bt3[-length(Bt3)],Ct,xlab='Biomass (tons)',ylab='Catch (tons)', lwd=2,type="b",
       xlim=c(0,max(K3,Bt3)),ylim=c(0,max(Ct)))
  lines(SP.3~Biomass, data=Surp.Prod3,lwd=2,col="darkgreen")
  legend("topleft", c("Observed","Surp. Production"), pch=c(1,NA),
         cex=0.8,lty=c(0,1), lwd=2, col=c("black","darkgreen"))
    #Can't plot this one with all of the different models b/c the Biomass estimates may differ


  #plot C vs. Effort (with production curve)
  Surp.Prod3$Et.SP.3 = Surp.Prod3$SP.3 / (q*Surp.Prod3$Biomass) #if C/E = q*B, and we set C = SP, then we can calc the E to achieve SP as E = SP/(q*B) 
  Surp.Prod3.Et=Surp.Prod3[order(Surp.Prod3$Et.SP.3),]
  
  plot(Et,Ct,xlab='Effort (vessel days)',ylab='Catch (tons)', lwd=2,type="b",
       xlim=c(0,max(c(Et,na.omit(Surp.Prod$Et.SP.3)))),ylim=c(0,max(Ct)))
  lines(SP.3~Et.SP.3, data=Surp.Prod3.Et,lwd=2,col="darkgreen")
  legend("topleft", c("Observed","Surp. Production"), pch=c(1,NA),
         cex=0.8, lty=c(0,1), lwd=2, col=c("black","darkgreen"))
  
  
  
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# COMPARING THE 3 MODELS -----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#Remake overall plot with different model fits
par(mfrow=c(1,1))
plot(year,It,xlab='',ylab='Hake CPUE', main="Surplus production model fits to CPUE")
lines(year,Ipred1,lwd=3,col="blue")
lines(year,Ipred2,lwd=3,col="red")
lines(year,Ipred3,lwd=3,col="darkgreen")
    legend("topright", c("Observed","Obs. Error Fit","Proc. Error Fit", "Total Error Fit"),
           pch=c(1,NA,NA,NA),lty=c(0,1,1,1), col=c("black","blue","red","darkgreen"))

#plot C vs. Effort (with production curve)
  plot(Et,Ct,xlab='Effort (vessel days)',ylab='Catch (tons)', lwd=2,type="b",
       xlim=c(0,max(c(Et,na.omit(Surp.Prod$Et.SP.3)))),ylim=c(0,max(Ct)))
  lines(SP.1~Et.SP.1, data=Surp.Prod.Et,lwd=2,col="blue")
  lines(SP.2~Et.SP.2, data=Surp.Prod2.Et,lwd=2,col="red")
  lines(SP.3~Et.SP.3, data=Surp.Prod3.Et,lwd=2,col="darkgreen")
  legend("topleft", c("Observed","Obs. Error Fit","Proc. Error Fit", "Total Error Fit"),
         pch=c(1,NA,NA,NA),lty=c(0,1,1,1), col=c("black","blue","red","darkgreen"))
  
#TABLE Comparing Management Quantities and parameters

comp.table=data.frame(Model=c("Obs. Error","Proc. Error","Total Error"),
                      Cmsy = round(c(Cmsy,Cmsy2,Cmsy3),0),
                      umsy = round(c(umsy,umsy2,umsy3),2),
                      Emsy = round(c(Emsy,Emsy2,Emsy3),0),
                      Bmsy = round(c(Bmsy,Bmsy2,Bmsy3),0),
                      K = round(c(K,K2,K3),0),
                      r = round(c(r,r2,r3),2),
                      q = round(c(q,q2,q3),5)
                      )
comp.table

  #DISCUSS the patterns/conclusions in the table


#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# Residuals plots ----
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

r1=It-Ipred1
r2=It-Ipred2
r3=It-Ipred3

par(mfrow=c(1,1))
plot(year,r1,xlab='',ylab='Residual',col='blue',pch=16,cex=1.5)
abline(0,0)
title('Observation error')

plot(year,r2,xlab='',ylab='Residual',col='red',pch=16,cex=1.5)
abline(0,0)
title('Process error')

plot(year,r3,xlab='',ylab='Residual',col='darkgreen',pch=16,cex=1.5)
abline(0,0)
title('Total error')



#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# IN CLASS EXERCISES ------
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

# - Work on the homework... 






#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# EXTRA: Equilibrium analysis - linear regression ------
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@

#order data by effort since equilibrium assumption
#  uses a linear model of CPUE ~ effort

hake=hake[order(hake$Effort),]

#Create individual vectors for ease
It=hake$CPUE
Et=hake$Effort
Ct=hake$Catch

SP=lm(It~Et)
summary(SP)

#check diagnostics
par(mfrow=c(2,2))
plot(SP)

library("calibrate") #INstall the package if needed using install.packages("calibrate") . Used for textxy()

#develop plots of fitted model: 
#1) CPUE vs. Effort
par(mfrow=c(1,2))
plot(Et,It,xlab='Effort',ylab='CPUE')
textxy(Et,It,hake$Year,cex=0.75)
lines(Et,predict(SP),col='blue',lwd=2)

#2) Catch vs. Effort
plot(Et,Ct,xlab='Effort',ylab='Catch')
textxy(Et,Ct,hake$Year,cex=0.75)
lines(Et,predict(SP)*Et,col='blue',lwd=2)  #predicted catch = predicted CPUE * Effort

#3) CPUE vs. time (with model fit)
hake.fit = hake
hake.fit$CPUE.fit = predict(SP) #add in model fits of CPUE
hake.fit = hake.fit[order(hake.fit$Year),] #Re-sort by year

par(mfrow=c(1,1))
plot(CPUE~Year, data=hake.fit, xlab='Year',ylab='CPUE')
lines(CPUE.fit~Year, data=hake.fit, col='blue',lwd=2)
  #Note that the predicted CPUE does not match the observed CPUE
  #Through time. In part this is because we are not fitting
  #the model to this time series of data.  Instead we are fitting
  #based on effort.  (this will be different with the non-equilibrium methods below)


#Line is defined as: It=a+b*Et (in CPUE vs. Effort plot #1), where 
# a = q*K and b = -(q^2)*K/r.
#Or, alternatively: It=(qK)-(q^2K/r)*Et, so intercept = qK and slope = -q^2K/r

  #MSY quantities (calculated from r, K, and q;  and from a and b)
  #Bmsy = K/2
  #Emsy = r/2q = -a/(2b)  
  #Cmsy = rK/4 = -(a/2)^2 / b



a=summary(SP)$coefficients[1,1] #intercept
b=summary(SP)$coefficients[2,1] #slope

#Working out the math: 
Emsy=-a/(2*b)                                                                                                                                                                                                             
Emsy
Cmsy= a*Emsy+b*Emsy^2   #same as Cmsy = -(a/2)^2 / b
Cmsy


