##############################################################################
# FISH 458/558 - Fish Population Dynamics 
# Lab 8 - Yield per Recruit and Spawner per recruit models
# A. Buchheister 
# February 2024
##############################################################################

# TODAY
# - Questions about recent HW?
# - Mini-presentation
# - Review YPR & SPR
# - YPR & SPR Lab
#   - Look up haddock



##################################
# YPR using fishmethods package
##################################
#install.packages("fishmethods")
library(fishmethods)

#Use haddock dataset  
data(haddock) #use data() to import datasets that are stored within the package
head(haddock)
?haddock
  #note:
  #ssbwgt = weight (kg)
  #partial= partial recruitment vector (aka age-specific selectivity value)
    # This will range from 0-1 and represent what fraction of F applies to that age.
  #pmat = probability of being mature


### STUDENT TASK ###
# 1. Figure out how to run a yield-per-recruit analysis for haddock using the ypr() 
#    function in the "fishmethods" package. Assume M=0.4. (work with neighbor as needed)
#     - Suggestion: functions will typically have an "Example" at the bottom of the help page, 
#       and that is usually helpful as you read and dissect the help menu.
haddock.ypr<-ypr(age=haddock$age, wgt=haddock$ssbwgt, partial=haddock$partial, M=0.4, plus=TRUE, oldest=100, maxF=2, incrF=0.01, graph=T)
# 2. Discuss with neighbor what the YPR analysis is telling you, what it means, and how it can be useful.
# 3. Discuss/Describe how you would go about exploring the effect of changes in age at first
#    capture in the fishery. 
#     - Come up with a strategy and specific steps you would take that would guide your coding. 
#       (Actual coding for this part is not necessary).

#[TRY TO FIGURE THIS OUT WITHOUT LOOKING AT ANSWERS BELOW...]

?ypr

#Using the ypr() function, run a yield-per-recruit analysis for haddock.  Assume M=0.4.



































#Run Yield per recruit analysis 
ypr(age=haddock$age, wgt=haddock$ssbwgt, partial=haddock$partial, M=0.4, #note that M is instantaneous;;; not actually 40% mortality
    plus=TRUE, oldest=100, maxF=2, incrF=0.01, graph=T)
  #make sure you know what each argument is used for.

  #Talk about graph and output

#Store the ypr output (ypr.out)
ypr.out=ypr(age=haddock$age,wgt=haddock$ssbwgt,partial=haddock$partial,M=0.4,
            plus=TRUE,oldest=100,maxF=2,incrF=0.01, graph=T)

#Lets see what is stored using the names() fxn
names(ypr.out)
  #output includes "Reference_Points" "F_vs_YPR" 

ypr.out$Reference_Points
ypr.out$F_vs_YPR

  #What are each of these?
# reference points refer to Fmax and F0.1 and the ypr at those values
#f vs ypr is the data showing yield per recruit at the different levels of fishing mortality specified in the function call

#Lets define objects for each dataframe:
ypr.ref = ypr.out$Reference_Points
ypr.dat = ypr.out$F_vs_YPR

#We can generate our own plot from these output:
plot(YPR~F, data=ypr.dat, type="l", ylab="Yield-per-recruit")
  abline(v=ypr.ref[,1], lty=c(1,2), col="red") #vertical lines for F0.1 and Fmax
  abline(h=ypr.ref[,2], lty=c(1,2), col="red") #horizontal lines for corresponding YPR
  legend("bottomright", rownames(ypr.ref), lty=c(1,2), col="red")

#Just for reference, if you wanted to plot lines segments to make the plot look better.
plot(YPR~F, data=ypr.dat, type="l", ylab="Yield-per-recruit")
  segments(x0=ypr.ref[,1], y0=c(0,0), x1=ypr.ref[,1], y1=ypr.ref[,2], lty=c(1,2), col="red") #vertical line segments for F0.1 and Fmax
  segments(x0=c(0,0), y0=ypr.ref[,2], x1=ypr.ref[,1], y1=ypr.ref[,2], lty=c(1,2), col="red") #horizontal line segments for F0.1 and Fmax
  legend("bottomright", rownames(ypr.ref), lty=c(1,2), col="red")
  
  
##############################################################
# Comparing YPR based on age at first capture (tc)
##############################################################
  
  #the two "knobs": fishing effort, or age selectivity
  
#What if we want to change the selectivity of the fishery (ie change age at first capture, tc)?
  #Create new columns using knife-edge selectivity at ages 1, 3, and 5
  haddock$tc.1 = ifelse(haddock$age<1, 0, 1)
  haddock$tc.3 = ifelse(haddock$age<3, 0, 1)
  haddock$tc.5 = ifelse(haddock$age<5, 0, 1)

  #Run the YPR models for each tc scenario    
  ypr.tc.1=ypr(age=haddock$age,wgt=haddock$ssbwgt,partial=haddock$tc.1, M=0.4,
              plus=FALSE,oldest=100,maxF=2,incrF=0.01, graph=T)
  ypr.tc.3=ypr(age=haddock$age,wgt=haddock$ssbwgt,partial=haddock$tc.3, M=0.4,
                plus=FALSE,oldest=100,maxF=2,incrF=0.01, graph=T)
  ypr.tc.5=ypr(age=haddock$age,wgt=haddock$ssbwgt,partial=haddock$tc.5, M=0.4,
                plus=FALSE,oldest=100,maxF=2,incrF=0.01, graph=T)
  
  #Combine the YPR outputs by binding the columns together (using cbind())
  ypr.tc.comp = cbind(ypr.tc.1$F_vs_YPR, 
                      ypr.tc.3$F_vs_YPR[,2], 
                      ypr.tc.5$F_vs_YPR[,2])
  head(ypr.tc.comp)
  colnames(ypr.tc.comp)=c("F","YPR_tc1","YPR_tc3","YPR_tc5") #Rename columns to be more informative
  head(ypr.tc.comp)

  #Create a table of the F0.1 values by binding the rows together (using rbind())
  ypr.tc.refs = rbind(ypr.tc.1$Reference_Points[1,],
                      ypr.tc.3$Reference_Points[1,],
                      ypr.tc.5$Reference_Points[1,])
  ypr.tc.refs = as.data.frame(ypr.tc.refs) #Convert to dataframe
  ypr.tc.refs$Age_at_capture = c(1,3,5) #add in the age at capture
  colnames(ypr.tc.refs)[1] = "F0.1"  
  ypr.tc.refs
  
  #Generate plot with YPR curves overlaid
  plot(YPR_tc5~F, data=ypr.tc.comp, type="l", ylab="Yield-per-recruit (kg)", lwd=2)
    lines(YPR_tc3~F, data=ypr.tc.comp, type="l", col="red", lwd=2)
    lines(YPR_tc1~F, data=ypr.tc.comp, type="l", col="blue", lwd=2)
        
    abline(v=ypr.tc.5$Reference_Points[1,1], lty=2, lwd=1, col="black") #vertical lines for F0.1
    abline(v=ypr.tc.3$Reference_Points[1,1], lty=2, lwd=1, col="red") #vertical lines for F0.1 
    abline(v=ypr.tc.1$Reference_Points[1,1], lty=2, lwd=1, col="blue") #vertical lines for F0.1
    legend("bottomright", c("tc5","tc3","tc1"), lty=1, lwd=2, col=c("black","red","blue"),
           title="Scenarios")
  

##################################
# SPR using fishmethods package
##################################

#Check what the sbpr function arguments (ie inputs) are:
?sbpr
    
sbpr(age=haddock$age,ssbwgt=haddock$ssbwgt,partial=haddock$partial,
     pmat=haddock$pmat,M=0.4, pF=0.5, pM=0.5,MSP=40,plus=FALSE,maxF=2, # MSP = the percentage of max spawning potential you want F for
     incrF=0.01, graph=T) #pM/pF refer tp natural and fishing mortality that occur before spawning (i.e. fishing open but spawning not happening yet)

#Make sure you understand all of the parameter inputs for this function!

#Rerun function, but store results as "spr.out"  
spr.out = sbpr(age=haddock$age,ssbwgt=haddock$ssbwgt,partial=haddock$partial,
              pmat=haddock$pmat,M=0.4, pF=0.5, pM=0.5,MSP=40,plus=FALSE,maxF=2, 
              incrF=0.01, graph=T) # left graph: SSB/R, right graph: % of msp, more likely to be asked about

#Check to see what is stored in the output 
names(spr.out)
spr.out$Reference_Point #this table has the F reference point calculated based on the designated MSP value
spr.out$F_vs_SPR #This table has the calculated SSB per recruit (SPR) and the SPR relative to the unfished scenario (%SPR or PSPR)

### COMPARING TWO SCENARIOS
#Let's assume you wanted to compare the %SPR curves and 40%MSP values given two scenarios:
#  M = 0.2 vs. M = 0.4 

#Scenario for M=0.2 with reference point for 40%MSP
spr.2 = sbpr(age=haddock$age,ssbwgt=haddock$ssbwgt,partial=haddock$partial,
             pmat=haddock$pmat,M=0.2, pF=0.5, pM=0.5,MSP=40,plus=FALSE,maxF=2,
             incrF=0.01, graph=T)

#Scenario for M=0.4 with reference point for 40%MSP
spr.4 = sbpr(age=haddock$age,ssbwgt=haddock$ssbwgt,partial=haddock$partial,
             pmat=haddock$pmat,M=0.4, pF=0.5, pM=0.5,MSP=40,plus=FALSE,maxF=2,
             incrF=0.01, graph=T)

#Store data for easier plotting
spr.2.dat = spr.2$F_vs_SPR
spr.4.dat = spr.4$F_vs_SPR

#Store reference points
spr.2.ref = spr.2$Reference_Point
spr.4.ref = spr.4$Reference_Point

#Generate manual plot to compare %MSP curves for the two M scenarios
par(mfrow=c(1,1))
plot(PSPR~F, data=spr.2.dat, type="l", ylab="% Max SPR", lwd=2)
  lines(PSPR~F, data=spr.4.dat, type="l", lwd=2, col="red")
  abline(h=40)
  abline(v=spr.2.ref[1,1], lty=2)
  abline(v=spr.4.ref[1,1], lty=2, col="red")
  legend("topright", c("M=0.2","M=0.4"), lty=1, col=c("red","black"), lwd=2, title="Scenarios")
    #Vertical dashed lines represent the F associated with 40%MSP for each scenario.
  

