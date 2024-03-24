####################################################
# FISH 458/558 - Fisheries Population Dynamics 
# Lab 1 - Intro to R   
# A. Buchheister (with material from R. Latour and P. Lynch)
# January 2024
####################################################

#R is a general purpose computer language

#Defined as an integrated suite of software facilities for 
#data manipulation, calculation and graphical display

#R is FREE!!!  

#RStudio is my recommended interface for R, though other editors are available.
#Install RStudio using handout if necessary.
#Look around the RStudio interface to familiarize yourself.

#Basics of the RStudio Interface (by quadrant)
# 1. Topleft quadrant - Here you see your "scripts", which are text documents 
#   with all your lines of code. Use "#" to make lines comments that are not executed
#   by R.  All other lines of code will be run line-by-line. To run a line of code, 
#   click on the "Run" button on the top right (or CTRL+ENTER).  You can highlight
#   Multiple lines of code at a time and run them in chunks also.
# 2. Bottomleft - This is the R Console. this is where R is opperating. You
#    will see the lines of code you run in blue, any output in black, and errors
#    or warnings in red.
# 3. TopRight - There are several tabs here, but primarily "Environment" is the primary
#    one.  Here, you will see all of the objects, data sets, results, etc. that are 
#    created and stored in your current workspace.
# 4. BottomRight - Again, several tabs.  Here you can load "Files", install "Packages",
#    see any "Plots" that you create, access the "Help" menu, and use the "Viewer". 
#    You will primarily use the "Plots" and "Help" tabs here (and the "Packages" periodically).


# An important thing to know is where to get help...
# R has a decent help menu (though it can be frustrating also!)
# In RStudio, you can use the help tab in the lower right window 
# Or you can call it:
help.start()
help.search("correlation") #search for a keyword
?cor #search for a specific function

#Or, in RStudio, use the help tab in the lower right window

# But in order to get good at programming in R you need practice
#either addressing your own scientific questions or perhaps following
#sample sessions... for example, I've posted some reference material in Canvas
# These are helpful resources, but of course there's always simply doing a 
#google search for whatever problem you may be having.

#GOOGLE WILL OFTEN BE A GO-TO SOURCE FOR TROUBLESHOOTING PROBLEMS/QUESTIONS

#Note that closing RStudio, you just want to save your script (and not the workspace typically)
#You know your script has unsaved changes when the title of the script in the 
#Topleft quadrant is red.  Always click "Save" to save your changes (although
#RStudio also tends to keep unsaved changes between sessions)


###########################################################################
# R Studio - an interface for implementing R with many user-friendly components

# Object-oriented programming
# An object is anything that is manipulated in R. 
# Objects are stored in the 'workspace' to reference/manipulate (Environment panel)
# 5 classes of objects:
# 1. character (title of variable or string variable)
# 2. numeric (real numbers)
# 3. integer
# 4. complex (imaginary numbers +i)
# 5. logical (true/false, 1/0)

#Perform some simple calculations
2*8
sqrt(25)
pi

# R is object oriented 
#(ie, you store values, vectors, matrices, data frames as an object you define)
# You can use the "=" sign or "<-" to assign the value of a new object
x=6  #Here, we are creating a new object called "x" and assigning it a value
x  #this just has R return what is stored as "x"
y=x^2
y


# typically we work with series or vectors, matrices, or data frames
  #Create some vectors
    x<- 1:5 #vector
    x
    y<- c(2.2, 4.1, 6.3, 5.0, 3.9) #c stands for concatenate, or combine
    y
    ID=c("A","B","C","D","E")  #character vector
    ID
  #Create a matrix
    xy=cbind(x,y) #matrix using column bind
    xy
  #Create a dataframe (note: you specify the column names (e.g., Col1 is arbitrary))
    df=data.frame(Col1=ID, Col2=x, Col3=y) #Create a dataframe. 
    df  #You could change the column names using colnames() if so inclined.

#Referencing (indexing) matrices and dataframes. [row,col]
  #Reference column 2, using different ways (works the same for rows)
  df$Col2 #Use $ to specify a column; I use this method often
  df[,2]
  df[,"Col2"]
  df[["Col2"]]

  #Reference a single cell
  df[2,3]

  #How do we reference ranges of cells within rows or columns? 
  df[,1:2]
  
#simple plot of data
plot(x,y)

#Other plot examples capable in R
demo(graphics)

#summary statistics
summary(y)

#or can do 'manually' using built in functions
mean(y)
sd(y)

#Objects are stored in the "workspace"
#list all objects in workspace
ls()

#remove one object
rm(x)
x

#remove all objects
rm(list=ls())
ls()

#as you will see throughout the semester, simulating data is a powerful tool
#lets suppose we need to generate some data from a normal distribution
?rnorm
y=rnorm(5,mean=3,sd=1)
y
plot(x,y)

#Rerun the last few lines... why don't we all have the same 'y' data? 
#Because this is a random process
set.seed(3)  #specify a specific seed to always get the same random output
y=rnorm(5,mean=3,sd=1)
y
plot(x,y)

#what would a histograph look like?
z=rnorm(100,0,1)
hist(z)
hist(rnorm(100000,0,1))
hist(rnorm(100000,0,1), breaks=100)


### IMPORTING DATA ###
  #What if we have our own data?
  #we import a vector, database, dataframe, or matrix of data

  #Set your working directory in script (ie the folder with the file(s) you want to access)
    #OPTION 1:
    #In the bottom right quadrant of RStudio, in the "Files" tab, navigate to the folder
    #that has your files:
      #Click on the data set of interest and click "import dataset..."
      #in the bottom left of the window, Change the name you want to give the dataset, and any other settings.
      #Click "Import"    

    #OPTION 2:
    #Set your working directory using your mouse:
      #Go to Session>Set Working Directory>Choose Directory... 
        #Navigate to the folder that has your files (NOTE: you will NOT be able to see the file names in the folder, that is normal)
        #After selecting folder, copy the command from your console and paste it into your script if desired.
      #OR go to "Session>Set Working Directory>To Source File Location"
        #Use this if you opened your script from the folder you are using.
    
    #OPTION 3:
    setwd("C:/Users/ab4577/HSU/Teaching/PopDy/2019_Spring/Labs/Lab 01 - Intro to R")
      #A comment on file paths: note the use of \\ instead of \. Or you can use /.
      #Trick for copying file paths from Windows Explorer: SHIFT+RIGHT CLICK on folder>
      #select "copy as path". Paste into your script and fix the slashes.


 
  getwd() #use this if you forget what you set your working directory to be.

  #Import data from comma delimited file
  dat=read.csv("data.csv")
  dat
  
  #Can also import files from .xls, .xlsx, .mdb, etc... 
  #But they tend to be more cumbersome. Search online for examples

# Previewing objects:
# Double check the imported data are what you expect.
head(dat)
tail(dat)
dim(dat)
ncol(dat)
nrow(dat)
str(dat)
summary(dat)

x=dat$x #sometimes easier to create an object for each of the parts you want.
head(x)

y=dat$y
head(y)

#always visualize your data
plot(x,y,xlab='X variable',ylab='Y variable', main="Plot of y vs. x")


#run a linear regression of Y on X to get a predictive relationship
mod=lm(y~x)
mod2=lm(y~x, data=dat) #can also run the model using the dataframe (I think this approach is better)
?lm

#look at resulting object from model fit - does it provide all desired information?
mod

#need summary function to get standard errors, t-stats, and p-values
summary(mod)
  #Here, we can see that the Y-intercept for the regression line is -1.0229
  #and the slope is 0.9891 (these are listed under the "estimate" column
  #in the "Coefficients" section)

#See what else is stored in the summary(mod) object
names(summary(mod))
summary(mod)$coefficients #Can get the intercept and slope from this.


#95% confidence intervals for estimated intercept and slope
confint(mod, level=0.95)

#is this a 'good' model fit?  Need to evaluate assumptions of linear regression
  #normally distributed response variable or residuals
  #mean of residuals equals 0 with constant variance across independent variable

#look at histogram of y data
hist(y)

#Store residuals and make histogram of them
r=resid(mod)
hist(r)

#plot of residuals across x variable
plot(x,r,xlab='X variable',ylab='Residual')

#place a horizontal line at 0
abline(a=0,b=0,lwd=2,col='blue')

#use built-in diagnostic plots for linear models
plot(mod)
?plot.lm  #search the plot function for lm() in the help for more information


#might be nice to view all four plots at once (ie 2x2)
par(mfrow=c(2,2)) #this changes the plot window to be 2x2 (rows x columns)
plot(mod)

#Ok, model diagnostics looks good
#what about generating a predicted line?
par(mfrow=c(1,1)) #Reset plot window to 1x1

  #Option A
  plot(x,y,xlab='X variable',ylab='Y variable')
    abline(mod, lwd=2, col="red") #add an line using the model output "mod"
  
  #Option B
  plot(x,y,xlab='X variable',ylab='Y variable')
  pred=predict(mod)  #automatic function for generating predictions from your model.
    lines(x,pred,lwd=2,col='green') #add a new line based on the X and predicted Y values


#############
# Functions
#############

#R has many functions built in, but it can also be helpful to write your own in some situations.
#The ability to do this is what makes R so powerful

#write a function to calculate the mean of y

meanfunc=function(x){
  s=sum(x)
  n=length(x)
  avg=s/n
  return(avg)  
}


meanfunc(y)

#compare with R's mean function or summary stats
mean(y)
summary(y)



