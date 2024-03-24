#............................................................
# FISH 458/558 - Fisheries Population Dynamics 
# Lab 3 - Leslie Matrix Model   
# A. Buchheister (based on material from J. Apple (https://qubeshub.org/publications/1926/1))
# Feb 2021
#............................................................

#Things to mention:
# - HW Comments
#     - How to read feedback; make comments
#     - Formatting (provide answers)
#     - Figure captions
#     - How to copy figures
#     - Don't include output of ancillary things (e.g., whole datasets)
#     - Equation for half life
# - Extra credit
# - Check in on recent HW

#ITEMS to do in class
# - First Mini presentation next week (Sarah M.) 
#   - recap objective
#   - See dates here:
#   - https://docs.google.com/spreadsheets/d/1FLvSbWkZqddlqOKo7F821xU0g77_9xmSVmroeDkOWxk/edit#gid=1671806429 
# - Lab script 
# - Work on HW



#............................................................
#  LESLIE MATRIX MODEL           -----------------------------
#............................................................

#This code demonstrates how to use a Leslie Matrix Model
#to describe the dynamics of an age-structured population.
#The model and code can also be generalized for other purposes.

# The goals of the lab are:
# - to develop a population matrix model using R
# - understand the components of a population matrix model
# - use the model to project population size in the future,
#   estimate lambda, and determine the stable age distribution
# - perform an elasticity analysis and interpret the values.

#Please see the lecture notes for more details on the approach & theory

#Run this line to install the popbio package, then comment it out:
#install.packages("popbio")

library(popbio)


# One way of representing the age-specific fertility and survival rates of a
# population is with a Leslie matrix. This is a powerful representation of a
# population's demographic variables because it allows for a lot of additional
# analyses.

### Set up your Leslie Matrix ###

### INPUT PARAMETERS
#Set the number of ages/stages in your model
n.ages=4 

#Per capita Fecundity (average number of oysters produced 
# per individual of each age class)
f.1 = 0 
f.2 = 0.9 
f.3 = 1.4 
f.4 = 1.1 

#Survival probability (In lecture, these were written as "S", 
#but we can also use "P", as many authors do) 
P.1 = 0.7
P.2 = 0.6
P.3 = 0.3
P.4 = 0

#Provide names for each age/stage
ages=c("Age 1","Age 2","Age 3","Age 4")

#First, A little matrix practice
  #  Let's enter a simple matrix. Say we want to make a matrix with 2 rows and 3
  #  columns, with the values 1,2,3 in the first row and 4,5,6 in the second row.
  #  We would enter our code like this - matrix(c(1,2,3,4,5,6),nrow=2,byrow=T). This
  #  puts our 6 values in two rows. We don't need to specify the number of columns; R
  #  can figure that out. byrow=T tells R to enter our numbers by row (not column).
  
  M=matrix(c(1,2,3,4,5,6),nrow=2,byrow=T)
  M
  
#CREATE THE LESLIE MATRIX
#  Now we are going to enter our new matrix representing age-specific fecundities
#  and survival of a hypothetical organism with 4 age classes. This is the same example
#  as we used in lecture for the age-structured model with 4 age classes. If needed, the pdf file
#  ("An introduction to using population matrix models in R") provides some background
#  and information related to this lab (although it has not been fully updated.)
#  Below, note we first enter 16 values (which we've stored as objects f.1, f.2, etc). 
#  The code is arranged so its a bit easier to see how the values will fill each of the 
#   4 rows of the matrix. Then you specify nrow=4 and byrow=T. Finally,
#  we want our rows and columns to be labeled with our age classes that we just set
#  up in the vector "ages." So the last argument in our matrix function is dimnames,
#  which is entered as a list with the names to use for the rows and for the columns
#  (both are ages). Go ahead and try it! Name your matrix A.

A = matrix(c(f.1, f.2, f.3, f.4,
             P.1,   0,   0,   0, 
               0, P.2,   0,   0,
               0,   0, P.3,   0),
           nrow=n.ages, byrow=T,
           dimnames=list(ages,ages))
A  #Check out how the matrix looks!


#Initial population size for each age 
N0 = c(20, 40, 21, 9)


###MAKE POPULATION PROJECTIONS 
# Projections rely on multiplying the vector of age-specific abundances by the leslie matrix:
# N1 = A %*% N0  where N1 is abundance at time 1, N0 is the starting abundance at time 0,
#                    A is the Leslie matrix, and %*% is used for matrix multiplication in R.

#Example calc for vector of abundances at yr t=1 
N1 = A %*% N0  # the %*% is for matrix multiplication
N1  #These are the estimates of abundance for the 3 stages of oysters in year t=1

#We could do a sequence of these calculations, but it would be tedious.
#popbio has a built in function to give us lots of good outputs,
#using the pop.projection() function.

#First, look at the help menu for the function:
?pop.projection

###RUN PROJECTIONS###
n.yrs = 30 #Specify the number of years for the projection
years = c(0:(n.yrs-1)) #make a vector of the years (starting at 0), needed for plotting later in the code.

projA = pop.projection(A, N0, n.yrs) #running a projection "n.yrs" years (ie for 10 years), given A and N0
projA  #Look at outputs

#  You can see that produced quite a bit of output. If you look at it closely you
#  see we have 5 different items. $lambda shows the estimate of lambda for our
#  population based on the last two time steps of our projection. $stable.stage
#  provides our stable stage (or age) distribution - what proportion of individuals
#  belong to each age/stage class by the end of our period of projected population
#  growth. The $stage.vectors show the actual numbers of individuals in each age
#  class step by step (from our initial year 0, to the 10th year (9). $pop.sizes
#  gives the total population size year by year, from our initial year 0 to the 9th
#  year (ie, after 10 years, starting at 0). Finally, $pop.changes indicates the lambda calculated for each time
#  step - N(t+1)/N(t). Note the final value in the series matches the lambda at the
#  top of the output.

  #STUDENT: What is the lambda for this population?  What does this value mean?
  #Is the population declining or increasing?  By how much?  
#lambda: population growth rate
#population is increasing by 12% per time step

  #STUDENT: How was lambda calculated here?
 # divide sum of all stages at N+1 by N

  #STUDENT: What is the total population size at the end of the 10-year period? 
#281.7089
  
  #STUDENT: What proportion of individuals are 2-year-olds in this population 
  #at the end of the 10-year period?
# 85.39383 / 281.7089 = 0.303


#Plot of the stable age distribution
stage.vector.plot(projA$stage.vectors)

  #STUDENT: Explain the graph.
#displays the proportion of the overall population belonging to each age class through time


###Plot the population projection
plot(years, projA$pop.sizes, type="b", pch=16, ylab="Total Population")

  #STUDENT: Replicate the last plot, but log transform the Y axis.
  #Why do you think the plot looks the way it does? 
  #(You can rerun the projection for 30 years to see the change more clearly if needed.)
plot(years, log(projA$pop.sizes), type="b", pch=16, ylab="Total Population")
#the log transformation turns this from an exponential relationship to a straight line linear one
#not sure; it must be some multiplicative process being reduced to an additive one.
#clearly lambda stabilizes quickly -- the population grows consistently after about year 3


### EIGEN.ANALYSIS FUNCTION (for lambda, stable-age distr, and elasticities)
#  One of the most powerful aspects of using population matrix models is that we can
#  use the mathematical techniques of linear algebra to learn about the dynamics of
#  the population we are modeling and estimate some informative demographic
#  parameters. We can get an estimate of lambda that is independent of the length of
#  time that we model population growth (since lambda should stabilize as long as
#  all our age-specific survival and fertility probabilities are constant). We can
#  determine the stable age/stage distribution. And we can see how different
#  elements of our matrix (the vital rate) affect our growth rate lambda.

# 
#  The function eigen.analysis() can accomplish all these goals. It takes only one
#  argument - our matrix. If we called our matrix A, we would simply use the command
#  eigen.analysis(A). We will give the results of this analysis a name - eigA. Use
#  this code to perform the eigen.analysis - eigA=eigen.analysis(A)
?eigen.analysis
eigA=eigen.analysis(A)
eigA

#  The output of eigen.analysis has a lot of useful information. First we see by
#  $lambda1 the value of lambda the population will achieve when it is stabilized.
#  $stable.stage shows the stable stage/age distribution. Then we see two matrices
#  of values - one for sensitivities and one for elasticities. These both indicate
#  the importance of each element of the matrix (the fertility rates, survival
#  probabilities, etc.) in determining the growth rate of the population, lambda.
#  Technically, sensitivities indicate how a small absolute change in a particular
#  age-specific fertility value (Fi) or survival probability (Pi) will affect lambda
#  when all other matrix elements are held constant. Elasticities are an even more
#  useful measure - they indicate how a proportional change in an Fi or Pi value
#  will affect lambda when all other matrix elements are held constant. Because
#  elasticities sum to 1, they are easier to compare and interpret than
#  sensitivities. Finally, the $repro.value (reproductive value) indicates the
#  expected contribution of an individual of each age or stage class to both current
#  and future reproduction. These reproductive values are scaled relative to the
#  values of the first age class, which is set to 1.

# STUDENT: What is value for lambda and how was it calculated? This value differs
# slightly from the previous lambda estimated using the pop.projection() function.
# Which is better and why? 
#lamba in the previous step was based only on the last two time steps, regardless of whether or not the 
#population has stabilized.
#the lambda here is specifically the value of lamba from when the population has stabilized.
#this strikes me as more meaningful and interesting for describing an overall trend, rather than a specific year
#at this point, since it has stabilized, all ages are growing at this same stable lambda

# STUDENT: What is the proportion of 3-year-olds in our stable age distribution? 
# 0.1626804

# STUDENT: What is the elasticity value associated with the fertility (ie, fecundity) of 2-year-olds?
#elasticity of 2 year old fecundityt is 0.1927418
# proportional change when all other fecundities/survivals are held constant. The relative impact
#a 1% increase in 2 year old fecundity will increase lambda by 19%

### VISUALIZING ELASTICITIES
#  The popbio package has a nice function for visualizing the elasticity values.
#  This gives a representation of the elasticities that
#  highlights the larger values with heatmap type colors (larger values, darker
#orange or red). 

image2(eigA$elasticities)

# STUDENT: Which fertility (i.e., fecundity) rate or survival probability has the highest elasticity value?
# And, what does that value mean conceptually?
#the survival of age 2 individuals has the highest elasticity, meaning that it has a higher proportional change
#to the lambda growth rate when all others are held constant; it is the biggest contributer to that growth rate

# STUDENT: If this was an invasive species you needed to control, what would be the most
# effective measure to reduce its population growth, based on these results from
# the population matrix model?
# based on the elasticity matrix, the best way to stop this invasive will be to decrease the rate of survival of Age 2 individuals




