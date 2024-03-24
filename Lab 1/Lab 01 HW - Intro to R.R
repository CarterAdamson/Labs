####################################################
# FISH 458/558 - Fisheries Population Dynamics 
# Lab 1 HW - Intro to R   
# A. Buchheister 
# January 2024
####################################################
library(tidyverse)
library(ggplot2)
setwd("/Users/cpadamson/Dropbox/Grad/FISH 558")


###################################
#  INSTRUCTIONS
###################################
#  Write a script to answer the questions below. Run the script and copy your Console output
#  into a WORD document. See the guidelines below. Submit your answers as a WORD document via CANVAS.
#  Please see "Examples of good HW formatting.docx" for formatting examples.


# The WORD document should include the following:
# 1) include your Name, course, lab number, and date at the top of the document 
# 
# 2) Number and label the questions and answers clearly! (We should easily be able to find your answers!) 
# 
# 3) Include all of the requested output (e.g., values, data tables, and plots), not just the code for them. 
#    (We will not copy your code into R to see if it works).
# 
# 4) Submit a Word document unless directed otherwise (no r files or pdfs please)
# 
# 5) Include all your code used for the problems 
#
# 6) Answer ALL questions using complete sentences that are clear and informative.
#
# 7) Include figure or table captions. (Figure captions go below the figure, table captions go above a table.)


###################################
#  QUESTIONS FOR 458
###################################

# Note: you will need to provide the total number of hours you work on this whole
# HW assignment, so keep track of the time you spend on it.

#### PART A #######
# A. Spend at least 2-3 hours learning and practicing how to use RStudio. (If you are new to R,
#    I recommend you spend substantially more time.)
#    Below are several options (depending on whether you are more of a BEGINNER or PROFICIENT user):
#    - EVERYONE should make sure to work through the lab script "Lab 01 - Introduction to R.r"
#      (although this should be completed during lab time).
#    - For R BEGINNERS (or intermediate) users: below are some OPTIONS for you:
#       1) Spend some time reading & working through the online book found here: https://bookdown.org/ndphillips/YaRrr/
#          (this was recommended by several students in past classes).
#       2) work through the "Torfs and Brauer 2014" tutorial found in the Lab folder on Canvas
#       3) Skim the "R_reference card.pdf" which is a useful reference.
#       4) Work through the "Tutorial" tab that can be found in the TopRight Quadrant of RStudio
#       5) Watch & work through any YouTube instructional tutorials. Here are some good options
#          that I have explored briefly (but you may choose to jump around if you want):
#           - https://www.youtube.com/watch?v=s3FozVfd7q4
#           - https://www.youtube.com/playlist?list=PLjgj6kdf_snYBkIsWQYcYtUZiDpam7ygg
#           - https://www.youtube.com/watch?v=X67No4239Ys&list=PL6gx4Cwl9DGCzVMGCPi1kwvABu7eWv08P 
#    - For more PROFICIENT R users: If you are strong with the basics of R, 
#      decide what skills you want to improve and then find some tutorials
#      to help you. I have 4 suggestions below. (Be sure to also check out 
#      the posted "cheatsheets" on ggplot2, data-wrangling, and R Markdown!)
#       1) work on your plotting skills,
#         - For plotting with ggplot2: work through www.ling.upenn.edu/~joseff/avml2012/
#       2) work on your data manipulation skills, 
#         - for data manipulation with dplyr: work through one of the following:
#           - http://genomicsclass.github.io/book/pages/dplyr_tutorial.html
#           - slower paced: https://www.datacamp.com/community/tutorials/tidyverse-tutorial-r 
#           - faster and more thorough: http://tclavelle.github.io/dplyr-tidyr-tutorial/.
#       3) work on your ability generating nice reports in R, 
#         - For generating slick reports and word documents using R MarkDown: https://rmarkdown.rstudio.com/lesson-1.html 
#       4) work on any other area of your interest! 
#         - for working on any other topics that may be of interest, search online, or use the "Venables et al. 2007 - An Intro to R.pdf"



#   1. Briefly describe how you spent your 2+ hrs working on your R skills. 
#      Include any good/recommended websites if you found them helpful! (5 pts)
#I worked on data manipulation and validation. The goal was to knock out two birds with one stone by using
#this lab to also work with a problem relevant to my thesis work. I worked with a new package to me ("tidyverse")
#to compare two versions of my data and identify entires unique to each. The goal is to help identify data entry
#errors as part of the QA/QC process. In order to add a component specific to this assignment, I also decided
#to add an RMarkDown component.
#   2. What did you find cool or interesting about what you learned? (2 pts)
#RMarkDown was very interesting for me. I've been given documents and assignments generated with RMarkDown before
#but never made a markdown script myself before. I like the way you can present code chunks and I think using it
#will help make my code more digestible to return to.
#   3. What do you think will be the biggest challenge for you in using and learning R? (1 pt)
#Expanding my knowledge base. I already have some experience with R, so it's tempting to get "stuck in your ways"
#without doing any exploration of new ideas and techniques. This class should be helpful in that regard.
#   4. Compare and contrast a vector, a matrix, and a dataframe in R. (1 pt)
#a vector is a linear sequence of like elements (e.g. a, b, c)
#a matrix is a 2D array of like elements, like a vector of vectors (e.g. a, b, c)
#                                                                       (a, b, c)
#                                                                       (a, b, c)
#a data frame is more like a table, being able to have rows, columns, and various data types, (e.g. letter | integer | double)
#                                                                                                   a      | 1       | 1.0
#                                                                                                   b      | 2       | 1.1
#                                                                                                   c      | 3       | 1.2
#
#### PART B #######
# B. Write and execute a script to answer the following questions below using R. 
#    Refer to the lab script for examples. (5 points)

#   1. Create a dataframe named "data" that has a 4 columns (named: ID, X, Y, Z) and 5 rows. 
#     ID should go from 1 to 5,
#     X should be the numbers [13,3,2,8,5], Y should go from 10 to 30 by fives, 
#     and Z should be the letters "a" through "e".
data = data.frame(ID = 1:5, X=c(13,3,2,8,5), Y=seq(10, 30, by=5), Z=letters[1:5])
#   2. Create an object called P that is set equal to the element in the 3rd row, 2nd column of your dataframe. 
p<-data[3,2]
#   3. Calculate the sum of all elements in column Y using the sum() function. 
sum.Y<-sum(data$Y)
#   4. What is the average value of the elements in rows 1:3 for column 3? (Use a function that references your dataset to find the answer.)
avg.3<-mean(data[1:3,3])
#   5. Add a fifth column (named Xsquared), with elements that are the square of the values in column X.
mutate(data, Xsquared=X*X)
#### PART C #######
# C.  Do the following (6 points)

# Import data from the "sandeelTLFL.csv" file, which has total lengths (TL)
#and fork lengths (FL) measured in mm for sandeels (ie sand lance)
sandeel<-read.csv("sandeelTLFL.csv")
#   1. Calculate and report the mean and standard deviation for TL and FL. (1 pt)
avg.TL<-mean(sandeel[,1])
sd.TL<-sd(sandeel[,1])
avg.FL<-mean(sandeel[,2])
sd.FL<-sd(sandeel[,2])
#   2. Generate a plot of the observed TL vs. FL (this means that TL is on the Y-axis,
#    and FL is on the X-axis). Change the points in the plot from open circles to solid circles.
      #Note: You will likely need to use the help menu or the internet to figure out how 
#      to change the plot symbols (3pts)
ggplot(sandeel, aes(x=FL, y=TL))+geom_point()
#   3. How many Total Hours did you spend on this homework assignment? (1 pt)
#      (We will track this information throughout the semester, so get used to this).
#3 hours
#   4. Working with other classmates on your HW will be a tremendous help as the semester progresses.
#      I encourage you to work on HW assignments together, or at least review your answers with at 
#      least one classmate.  Do this and take a selfie of the group.  Include 
#      the selfie/screenshot here (feel free to actually smile in the picture or have fun with it!). (1 pt)


############################################################
#  QUESTIONS FOR 558 (Or +2.5 Extra Credit for 458 students)
############################################################

# A.-C. Do parts A-C above.

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
data.2<-data.frame(X=rep(1:20, each=5))

#Then generate 100 "Y" values using a regression model defined by:
#  Y = 2.5*X + 10
data.2<-mutate(data.2, Y=2.5*X+10)

#Now, add a small amount of random error or noise to each Y value based on
#a normal distribution with a mean of 0 and a standard deviation of 2.
#For this, you will need to create 100 values using the rnorm() function from the lab script.
noise<-rnorm(100, 0, 2)
for(i in 0:100)
{
  data.2[i, 2]<-data.2[i,2]+noise[i]
}
#Run a regression on your simulated data and estimate the slope and intercept
#for the linear model (see the lm() function from lab). Generate a plot with your model fit, 
#but set the title of your graph
#to be the estimated regression model equation (e.g., "Y=2.452X+9.555").
mod=lm(data.2$Y~data.2$X)
summary(mod)
ggplot(data.2, aes(x=X, y=Y))+geom_point()+ggtitle("Y = 2.52X + 9.65")+theme(plot.title = element_text(hjust = 0.5))

#What you need to submit for part D:
# - Your code for conducting the task
# - The plot with the simulated data and the model fit. 
#   (Don't forget to include the title and a figure caption).




  