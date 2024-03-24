####################################################
# FISH 458/558 - Fisheries Population Dynamics 
# Lab 1 HW - Intro to R   
# A. Buchheister 
# January 2019
####################################################


############################################################
#  QUESTIONS FOR 558 (Or Extra Credit for 458 students)
############################################################

# D. Do the following (5 points)

#Simulating data can be very helpful for quantitative studies. Here, I want you to 
#generate simulated X and Y data and run a regression analysis. The final
#product is simply a plot with the model fit and regression equation.
#For some of you this may be very new and you haven't received much 
#guidance. As graduate students, you will often have to figure things out 
#on your own. Use the resources available to you, including your classmates
#as needed. I'm also available during office hours.

#Generate 100 "X" values from 1 to 20, with each value having 5 replicates (e.g., 1,1,1,1,1,2,2,2,2,2...)
#(search online or look at the "Data Creation" section of the R Reference Card 
#for a good R function to do this).

#Then generate 100 "Y" values using a regression model defined by:
#  Y = 2.5*X + 10

#Now, add a small amount of random error or noise to each Y value based on
#a normal distribution with a mean of 0 and a standard deviation of 2.
#For this, you will need to create 100 values using the rnorm() function from lab.

#Run a regression on your simulated data and estimate the slope and intercept
#for the linear model (see the lm() function from lab). Generate a plot with your model fit, 
#but set the title of your graph
#to be the estimated regression model equation (e.g., "Y=2.452X+9.555").

#Describe how your plot and the regression
#equation changes if you increase the standard deviation for your random error
#from sd=2 to sd=10?


  
  
###558 SOLUTION USING A FUNCTION###

#Define function with parameters for slope, intercept (int), and stdev (sd.dist)
  
SimFunc = function(slope, int, sd.dist) {

  #For troubleshooting function (use this to test whether the code within the function is working ok):
    #slope=2.5; int=10; sd.dist=10  #TROUBLESHOOTING VALUES 
      #in other words, store values for "slope", "int", and "sd.dist" so that you can run the code below.
      #then, once the code is working and generating your desired output, you comment out the TROUBLESHOOTING VALUES.
      #Lastly, you run the code, but this time include the function wrapper (ie, "SimFunc = function(...){  "), but dont
      #forget to close out your curly bracket "}" at the end of your script.
  
  #Scenario 1
  #Generate data 
  x = rep(1:20,each=5)
  #set.seed(10) #Use this to specify a "specific" set of random numbers; otherwise, you will generate random numbers each time you generate new data.
  y = slope*x + int + rnorm(n=length(x),mean=0,sd=sd.dist)  #Note the use of the function parameters...
  
  #Run regression
  mod3=lm(y~x)
  
  #View output
    #summary(mod3)
    #hist(resid(mod3))
    #plot(mod3, which=1)
  
  #Define estimated parameters as objects to automate their inclusion in the graph.
  mod3$coefficients
  int.est = round(mod3$coefficients[1], 4)
  slope.est = round(mod3$coefficients[2], 4)
  
  plot(x,y, pch=16, main="Andre Buchheister", sub=paste("Estimated line: y=",slope.est,"x+",int.est, sep="")) #use paste() function to generate subtitle with equation 
  lines(x,predict(mod3),lwd=2,col='red')
  mtext(paste("True slope=",slope,"; true int=",int,"; sd=",sd.dist, sep=""), side=3) #decided to add in a line indicating the true, underlying parameters
}
  
#Run function with different parameter values.

  SimFunc(slope=2.5, int=10, sd.dist=2)  
    
  SimFunc(slope=2.5, int=10, sd.dist=10)
  
  #Identical results to before!
  #but you could now play with the function to look at other scenarios
  SimFunc(slope=2.5, int=10, sd.dist=20)
  SimFunc(slope=2.5, int=10, sd.dist=40)  
  SimFunc(slope=2.5, int=10, sd.dist=100)  
  