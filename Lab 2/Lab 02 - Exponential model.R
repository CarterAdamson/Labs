####################################################
# FISH 458/558 - Fisheries Population Dynamics 
# Lab 2 - Exponential and Logistic growth model  
# A. Buchheister 
# January 2024
####################################################

# Today:
# - Questions about the HW? Note: it will be due TOMORROW b/c of the strike.
# - File Management reminder
# - Do exercise located in "Lab 02 - Exercise_HumanPopGrowth.xlsx"
# - Go through lab script
# - Refresher on logs and e... (See also math review handout)
# - Work on HW (time-permitting)

  
#########################
# EXPONENTIAL GROWTH
#########################
  
#Some random basics first (log and e).
  #How to calculate the natural log of 3?
    x=log(3) #the log() function automatically assumes the base is "e".
    x
  #using the constant "e" and raising it to a power (e^x):
    exp(0.5) 
    exp(1) #this is the value of e
  #See how the natural log and e are related (they can cancel each other out):
    log(exp(0.5))
    exp(log(10))
    
  #To use Log with a different base, use base= argument:
    log(100, base=10) #log of 100 using a base of 10.
    
### Continuous version of the exponential model
# Nt = N0*exp(r*t)    #Note the use of the multiplication symbols... (e.g., rt = r*t)
    
  #STUDENT: What would the estimate of Nt be at t=2 years if N0=100 and r=0.1?
    #Suggestion: use the equation above, but specify create objects for
    #N0, r, and t first.

  
  #What would the estimate of Nt be at t=5 years?

    
  
  #Ok, what if we want to generate a time series of values for t from 0 to 50?
    t=c(0:50)
    t
    N = N0*exp(r*t) #because we supplied a vector for t, the equation automatically
                    #does the calculation for each value, generating a time series
                    #of N values.
    N
    #HOW COULD YOU GET THE ESTIMATE OF N FOR YEAR 20 from the "N" vector using
    #brackets? (Note: make sure to think about which element of the vector you
    #really want.)
    
    
  #Plot N as a function of time (t)
    plot(t, N, main="Plot - positive r", type="b") #type="b" refers to Both points and lines; use "l" for line
    
  #What would it look like if we had a negative r?
    r.neg = -0.1  #define new negative r parameter with a unique name (r.neg)
    N.neg = N0*exp(r.neg*t) #calculate a new population vector N.neg
    plot(t, N.neg, main="Plot - negative r", type="b") #type="b" refers to Both points and lines; use "l" for line
    
  #What would it look like if we had a r=0?
    #Redefine parameters
    r.zero = 0  #define new r parameter
    N.zero = N0*exp(r.zero*t)
    plot(t,N.zero, main="Plot - negative r", type="b") #type="b" refers to Both points and lines; use "l" for line
    
  #Lets make a plot with all three lines
    plot(t,N, main="Exponential growth models", type="l", col="green", lwd=3)
    lines(t, N.zero, col="black", lwd=3) #add a line for N.zero by specifying the x variable (which is t), and the y variable (which is N.zero)
    lines(t, N.neg, col="red", lwd=3)
    
    #Lines are not showing up well due to axis scales.  Lets retry, and specify the x and y limits (xlim and ylim)
    plot(t,N, main="Exponential growth models", type="l", col="green", lwd=3, 
         xlim=c(0,20), ylim=c(0,800))
      lines(t, N.zero, col="black", lwd=3) #add a line for N.zero by specifying the x variable (which is t), and the y variable (which is N.zero)
      lines(t, N.neg, col="red", lwd=3)

      #Lets add a legend manually, specifying location, names, colors, and other parameters...
      ?legend #look up the documentation for the legend() function for reference
      legend("topleft",c("r>0","r=0","r<0"), col=c("green","black","red"), 
             lty=1, lwd=2, title="r Value", cex=1) 
        #legend syntax: topleft refers to where to place the legend. 
        # The next argument is for the labels you will be putting in the legend (e.g., "r>0").
        # then, specify the colors (col) for each label.
        # Then, specify the line type (lty) for the symbol in the legend (lty=1 means "solid line")
        # lwd is for line width, and title is for the legend title.
        # cex is for "character expansion" (ie font size). Cex=1 is the standard, with higher (e.g., 1.2) or lower (e.g. 0.8) values increasing or decreasing the font.
    
  #STUDENT: Calculate the doubling time for the population, given r=0.1.
    #Remember, our equation for doubling time is t.double = log(2)/r

      
    #STUDENT: how long does it take for a population to double if r=0.3?
    
      
##################################      
###Exponential mortality model ###
##################################      
      
#The Exponential mortality model is identical to the exponential growth model, 
#but it takes the form:
  # Nt = N0 * exp(-Z*t)
  # where N0=initial population size
  #       Z = instantaneous mortality rate (note this is a positive value)

  #Coding of this model will be very similar to the exponential growth mortality model.    
  #In class, we talked about how you could convert the total instantaneous mortality 
  #rate (Z) into a more intuitive "annual mortality rate" 
  #(ie the proportion dying in one year).  
  #To do this, you can use the following equations:
    #  A=1-exp(-Z)
    #  Z=-log(1-A)  #note that we use natural log for the equation (which is "log" in R, and it is "ln" in Excel)
  #You will need this for the Homework.
    
    

    
### FOR THE REST OF LAB:
  # Work on the Homework assignment that is due next week!
  # You can also use this time to ask questions regarding R or any confusion you may have.  
  # Use this time for your benefit!

    
    
    
 
    