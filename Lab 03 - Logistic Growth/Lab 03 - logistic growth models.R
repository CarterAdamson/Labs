#.....................................................
# FISH 458/558 - Fisheries Population Dynamics 
# Lab 3 - Logistic growth model  
# A. Buchheister 
# January 2024
#.....................................................

# ANNOUNCEMENTS/DISCUSSION
# - R workshop - Wed, Jan 31, 4:30~6pm, BSS 313
# - HWs
#   - How to view feedback
#   - How numeric grading works for grad students
#   - Questions about the HW that was due today?
#   - How many grad students actually calculated the doubling time mathematically?
# - Grad student mini-presentations - check schedule
# - Misc. Reminders/tips
#   - File Management reminder
#   - Organization and commenting of scripts 
#   - Tip: Use the outline features (https://posit.co/blog/rstudio-v1-4-preview-little-things/)


# TODAY:
# - Go through lab script
# - Work on HW (note: script from Lab 02 will also be helpful!)


    
#.....................................................
# LOGISTIC GROWTH MODEL -----
#.....................................................

    
### Differential equation for logistic model 
# dN.dt = r*N*(1-N/K)
# where dN.dt = rate of change of the population with respect to time (dN.dt is also
# known as the population's production).  We use "dN.dt" as a name instead of "dN/dt" because 
# otherwise R won't be able to store it as an object (it would try to do division instead). 
#       K = carrying capacity
#       N = population size
#       r = intrinsic rate of increase

### Continuous version of the logistic model 
# Nt = K / (1+((K-N0)/N0)*exp(-r*t))
# where K = carrying capacity
#       Nt = population size at time t
#       N0 = initial population size
#       r = intrinsic rate of increase
    
### Discrete time version of the logistic model 
# N(t+1) = Nt + r*Nt*(1-Nt/K)  
    # where K = carrying capacity
    #       Nt = population size at time t
    #       r = intrinsic rate of increase
 
       
## CONTINUOUS VERSION OF LOGISTIC ####
#Lets do a projection using the continuous time version of the logistic model, with these parameters,
#which are the same as what we used for the exponential model (except now we have a carrying capacity):
    r=0.1
    K=1000
    N0=100

  #Set up time vector (t),  from 0 to 50 years in this case
       t=c(0:50)
   
  #STUDENT: Calculate Nt using the mathematical equation from the lecture,
  #  but call it "N.logist". For practice, do NOT just copy it from the 
  #  the text i have earlier in this script. Getting your parentheses right is important,
  #  and it may take some practice.  Check your values and code with a neighbor.
      N.logist = K/(1+(((K-N0)/N0)*exp(-r*t)))
        
        
  #Lets see the plot
      plot(t, N.logist, main="Logistic growth model", type="l", lwd=2, lty=2) #line type (lty) specifies the type of line
      #Note the "s-shape" of the figure, characteristic of the logistic model
  

### Production (dN/dt) ###  
# A population's production is calculated using the dN/dt function:
#    dN.dt = r*N*(1-N/K)  #(see above).
# This describes the total additional numbers of fish that are added to the population 
# in a year (or whatever time period you are using). This production will depend on how many
# individuals are already in the population; in other words, it depends on the population N, 
# because this model is a DENSITY-DEPENDENT model.

#STUDENT: Calculate what the production (dN.dt) would be for the population we are modeling. 
#For this, create a vector called dN.dt that is calculated using the dN/dt equation above.
#Then, create a plot showing how the production changes as a function of the abundance N (i.e., plot dN.dt vs. N.logist).  
#Note: this will help for the HW.
    
dN.dt = r*N.logist*(1-(N.logist)/K)
data.df <- data.frame(dN.dt=dN.dt, N = N.logist)
library(ggplot2)
plot2 <- ggplot(data=data.df, aes(x=N, y=dN.dt)) +geom_point()+geom_line()+ylim(0, 30)+theme_bw()
plot2

## DISCRETE VERSION OF LOGISTIC ####
#Lets do the same thing with the discrete time version of the logistic model:  N(t+1) = Nt + r*Nt*(1-Nt/K)
  #Set up parameters & variables as before
    r=3
    K=1000
    N0=100
    t=c(0:50)
    
  #Because this is a recursive function, lets populate a vector of population sizes sequentially using a "for loop".
    N.logist.discr = rep(NA, length=length(t))  #Create empty vector to hold our values, but it has to be of the same size as our time (t) vector
    N.logist.discr[1] = N0  #Set the first value as the starting population size, N0
    
    for(i in 1:(length(t)-1))  { #the "For loop" where we will calculate N for each time step, referring to the previous time step. Note that the first value we calculate is for i=2.
      N.logist.discr[i+1] = N.logist.discr[i] + r*N.logist.discr[i] *(1-N.logist.discr[i]/K)
    }
    
    N.logist.discr
    #Note: although the above may look daunting, what i did was the following:
      #Copy discrete version of equation from above:
        #N(t+1) = Nt + r*Nt*(1-Nt/K)
      #The "N(t+1)" and "Nt" have t+1 and t as subscripts in the equation, which we will identify in R using the brackets
        #N[t+1] = N[t] + r*N[t]*(1-N[t]/K)
      #Then, we just replace the "N" in the equation with our object name "N.logist.discr"
      #and we use the "i" in place of t so we don't get mixed up with our object we already named "t"
    
      #"For loops" can be frustrating and confusing at first! Its ok! Work on simple
      #examples from tutorials (like those provided on Canvas)... and they will get easier.
      #Here is one simple website on "for loops": https://www.r-bloggers.com/2015/12/how-to-write-the-first-for-loop-in-r/ 

      
    #Lets plot all of the models together
    plot(t, N.logist, main="Logistic growth models", type="l", lwd=2, xlim=c(0,50), ylim=c(0,1000)) #solid line
    lines(t,N.logist.discr, type="l", lwd=2, lty=2, col="red")  #red, dotted line
    legend("bottomright", c("Continuous","Discrete"), col=c("black", "red"), 
           lwd=2, lty=c(1,2), cex=0.8, title="Model type") 
      #NOTE: see how similar the continuous and discrete logistic models are.  Zooming in would show some of the minor differences more clearly


    
    
        
    
### FOR THE REST OF LAB:
  # Work on the Homework assignment that is due next week!
  # You can also use this time to ask questions regarding R or any confusion you may have.  
  # Use this time for your benefit!

    
    
    
    
#.....................................................
#MISC: Plots for lecture on Logistic growth: -----
#.....................................................
    
   
#Exploring the effect of r on N trajectory
    r1=0.1
    r2=5
    r3=100
    K50=10
    K100=10
    K200=10
    N0=2

  #Set up time vector (t) and calculate Nt (which I called N.logist)
      t=c(0:50)
      N1 = K100 / (1+((K100-N0)/N0)*exp(-r1*t))  #Getting the parentheses right is important!
      N2 = K100 / (1+((K100-N0)/N0)*exp(-r2*t))  #Getting the parentheses right is important!
      N3 = K100 / (1+((K100-N0)/N0)*exp(-r3*t))  #Getting the parentheses right is important!

  #Lets plot this logistic model with our previous exponential model
    plot(t, N1, main="K=100", type="l", lwd=2, xlim=c(0,30),ylim=c(0,100), ylab="Abundance") #this is a plot of the exponential model
      lines(t, N2, type="l", lwd=2, lty=2)  #now add the logistic model using a dashed line (lty=2)
      lines(t, N3, type="l", lwd=2, lty=3)  #now add the logistic model using a dashed line (lty=2)
      legend("topright",c("r=0.1","r=0.2","r=0.3"), col=c("black"), lwd=2, lty=c(1,2,3)) 

        
    
     
#Exploring the effect of K on N trajectory
    r=0.3
    K50=50
    K100=100
    K200=200
    N0=2

  #Set up time vector (t) and calculate Nt (which I called N.logist)
      t=c(0:50)
      N50 = K50 / (1+((K50-N0)/N0)*exp(-r*t))  #Getting the parentheses right is important!
      N100 = K100 / (1+((K100-N0)/N0)*exp(-r*t))  #Getting the parentheses right is important!
      N200 = K200 / (1+((K200-N0)/N0)*exp(-r*t))  #Getting the parentheses right is important!
      
  #Lets plot this logistic model with our previous exponential model
    plot(t, N50, main="r=0.3", type="l", lwd=2, xlim=c(0,30),ylim=c(0,200), ylab="Abundance") #this is a plot of the exponential model
      lines(t, N100, type="l", lwd=2, lty=2)  #now add the logistic model using a dashed line (lty=2)
      lines(t, N200, type="l", lwd=2, lty=3)  #now add the logistic model using a dashed line (lty=2)
      legend("topleft",c("K=50","K=100","K=200"), col=c("black"), lwd=2, lty=c(1,2,3)) 

    
    