setwd("/Users/cpadamson/Dropbox/Grad/FISH 558/Lab 08 - YPR, SPR")
library(ggplot2)
library(fishmethods)


# Question 1 ####


# a - plot 5 scenarios
haddock <- read.csv("haddock.csv")
haddock$tc.1 <- ifelse(haddock$age<1, 0, 1)
haddock$tc.2 <- ifelse(haddock$age<2, 0, 1)
haddock$tc.3 <- ifelse(haddock$age<3, 0, 1)
haddock$tc.4 <- ifelse(haddock$age<4, 0, 1)
haddock$tc.5 <- ifelse(haddock$age<5, 0, 1)
mod.ypr.1=ypr(age=haddock$age,wgt=haddock$ssbwgt,partial=haddock$tc.1, M=0.4,
             plus=FALSE,maxF=2,incrF=0.01, graph=T)
mod.ypr.2=ypr(age=haddock$age,wgt=haddock$ssbwgt,partial=haddock$tc.2, M=0.4,
              plus=FALSE,maxF=2,incrF=0.01, graph=T)
mod.ypr.3=ypr(age=haddock$age,wgt=haddock$ssbwgt,partial=haddock$tc.3, M=0.4,
              plus=FALSE,maxF=2,incrF=0.01, graph=T)
mod.ypr.4=ypr(age=haddock$age,wgt=haddock$ssbwgt,partial=haddock$tc.4, M=0.4,
              plus=FALSE,maxF=2,incrF=0.01, graph=T)
mod.ypr.5=ypr(age=haddock$age,wgt=haddock$ssbwgt,partial=haddock$tc.5, M=0.4,
              plus=FALSE,maxF=2,incrF=0.01, graph=T)

#extract data from each model
ypr.1.data<-mod.ypr.1$F_vs_YPR
ypr.2.data<-mod.ypr.2$F_vs_YPR
ypr.3.data<-mod.ypr.3$F_vs_YPR
ypr.4.data<-mod.ypr.4$F_vs_YPR
ypr.5.data<-mod.ypr.5$F_vs_YPR

#plot
plot.1<-ggplot(data=ypr.1.data, aes(x=F, y=YPR, color = "tc = 1"))+geom_line()+
  geom_line(aes(y=ypr.2.data$YPR, color="tc = 2"))+
  geom_line(aes(y=ypr.3.data$YPR, color="tc = 3"))+
  geom_line(aes(y=ypr.4.data$YPR, color="tc = 4"))+
  geom_line(aes(y=ypr.5.data$YPR, color="tc = 5")) + theme_bw()
plot.1


# b - F(0.1) for each scenario
F.01.1 <- mod.ypr.1$Reference_Points[1,1]
F.01.2 <- mod.ypr.2$Reference_Points[1,1]
F.01.3 <- mod.ypr.3$Reference_Points[1,1]
F.01.4 <- mod.ypr.4$Reference_Points[1,1]
F.01.5 <- mod.ypr.5$Reference_Points[1,1]
F.vector <- c(F.01.1, F.01.2, F.01.3, F.01.4, F.01.5)


# c - recommend mortality rate and age at first capture
# add f0.1 lines to the plot
plot.2 <- plot.1 + geom_vline(aes(xintercept=F.01.1, color="tc = 1"), linetype="dashed") +
  geom_vline(aes(xintercept=F.01.2, color="tc = 2", lty="dashed"), linetype="dashed") +
  geom_vline(aes(xintercept=F.01.3, color="tc = 3", lty="dashed"), linetype="dashed") +
  geom_vline(aes(xintercept=F.01.4, color="tc = 4", lty="dashed"), linetype="dashed") +
  geom_vline(aes(xintercept=F.01.5, color="tc = 5", lty="dashed"), linetype="dashed") 
plot.2

#add points to emphasize intersections
YPR.01.1 <- mod.ypr.1$Reference_Points[1,2]
YPR.01.2 <- mod.ypr.2$Reference_Points[1,2]
YPR.01.3 <- mod.ypr.3$Reference_Points[1,2]
YPR.01.4 <- mod.ypr.4$Reference_Points[1,2]
YPR.01.5 <- mod.ypr.5$Reference_Points[1,2]
YPR.vector <- c(YPR.01.1, YPR.01.2, YPR.01.3, YPR.01.4, YPR.01.5)
color.vector <- c("tc = 1", "tc = 2","tc = 3","tc = 4","tc = 5")
points.df <- data.frame(F=F.vector, YPR = YPR.vector, color = color.vector)
plot.3 <- plot.2 + geom_point(data=points.df, aes(color=color))
plot.3



# Question 2 ####

# a - is F too high or too low to achieve 40% MSP? What % is F=0.55 achieving
SBPR <- sbpr(age=haddock$age,ssbwgt=haddock$ssbwgt,partial=haddock$tc.3,pmat=haddock$pmat,
     M=0.4, pF=0.5, pM=0.5,MSP=40,maxF=2,incrF=0.01, graph=T)

# b - what would be a more appropriate F? Express as an annual rate and compare to F = 0.55
#when PSPR = 40.53, F = 0.35
#when PSPR = 39.77, F = 0.36
#M=0.4, Z = F + M = 0.75
#equation: u = F/Z (1- e^-Z)
u.36 <- (0.36/0.76)*(1-exp(-0.76))
u.55 <- (0.55/0.95)*(1-exp(-0.95))




# GRAD STUDENT QUESTION ####

# a - create a contour plot

#setup a data frame with appropriate age and F for this question
F.col <- seq(0, 1.5, length.out = 10)
age.col <- seq (1,10,  by = 1)
contour.df <- data.frame(age=age.col, wgt=haddock$ssbwgt[1:10], partial = haddock$partial[1:10], YPR = 0)
mod.ypr.x=ypr(age=contour.df$age, wgt=contour.df$wgt, partial=contour.df$partial, M=0.4,
              maxF=1.5,incrF=0.15, graph=T)
contour.df$YPR <- mod.ypr.x$F_vs_YPR$YPR[2:11]
contour.df$F <- mod.ypr.x$F_vs_YPR$F[1:10]

#make a data frame with columns for each age at first capture regime
tc <- data.frame(tc1=haddock$tc.1[1:10], tc2=haddock$tc.2[1:10],tc3=haddock$tc.3[1:10],tc4=haddock$tc.4[1:10],tc5=haddock$tc.5[1:10])
tc$tc6 <- ifelse(contour.df$age<6, 0, 1)
tc$tc7 <- ifelse(contour.df$age<7, 0, 1)
tc$tc8 <- ifelse(contour.df$age<8, 0, 1)
tc$tc9 <- ifelse(contour.df$age<9, 0, 1)
tc$tc10<- ifelse(contour.df$age<10, 0, 1)

#"expanded" data frame with unique row for each age-F combination
contour.exp = data.frame(age = rep(c(1:8), each=10), F = rep(contour.df$F, 8), YPR = 0)

#get YPR for each 
mod.ypr.temp=ypr(age=contour.df$age,wgt=contour.df$wgt,partial=tc$tc1, M=0.4,
                 plus=FALSE,maxF=1.5,incrF=0.01, graph=T)
vec1= mod.ypr.temp$F_vs_YPR$YPR[2:11]
mod.ypr.temp=ypr(age=contour.df$age,wgt=contour.df$wgt,partial=tc$tc2, M=0.4,
                 plus=FALSE,maxF=1.5,incrF=0.01, graph=T)
vec2= mod.ypr.temp$F_vs_YPR$YPR[2:11]
mod.ypr.temp=ypr(age=contour.df$age,wgt=contour.df$wgt,partial=tc$tc3, M=0.4,
                 plus=FALSE,maxF=1.5,incrF=0.01, graph=T)
vec3= mod.ypr.temp$F_vs_YPR$YPR[2:11]
mod.ypr.temp=ypr(age=contour.df$age,wgt=contour.df$wgt,partial=tc$tc4, M=0.4,
                 plus=FALSE,maxF=1.5,incrF=0.01, graph=T)
vec4= mod.ypr.temp$F_vs_YPR$YPR[2:11]
mod.ypr.temp=ypr(age=contour.df$age,wgt=contour.df$wgt,partial=tc$tc5, M=0.4,
                 plus=FALSE,maxF=1.5,incrF=0.01, graph=T)
vec5= mod.ypr.temp$F_vs_YPR$YPR[2:11]
mod.ypr.temp=ypr(age=contour.df$age,wgt=contour.df$wgt,partial=tc$tc6, M=0.4,
                 plus=FALSE,maxF=1.5,incrF=0.01, graph=T)
vec6= mod.ypr.temp$F_vs_YPR$YPR[2:11]
mod.ypr.temp=ypr(age=contour.df$age,wgt=contour.df$wgt,partial=tc$tc7, M=0.4,
                 plus=FALSE,maxF=1.5,incrF=0.01, graph=T)
vec7= mod.ypr.temp$F_vs_YPR$YPR[2:11]
mod.ypr.temp=ypr(age=contour.df$age,wgt=contour.df$wgt,partial=tc$tc8, M=0.4,
                 plus=FALSE,maxF=1.5,incrF=0.01, graph=T)
vec8= mod.ypr.temp$F_vs_YPR$YPR[2:11]

#ypr function stopped working at tc = 9 and up. 
#mod.ypr.temp=ypr(age=contour.df$age,wgt=contour.df$wgt,partial=tc$tc9, M=0.4,
#                 plus=FALSE,maxF=1.5,incrF=0.01, graph=T)
#vec9= mod.ypr.temp$F_vs_YPR$YPR[2:11]
#mod.ypr.temp=ypr(age=contour.df$age,wgt=contour.df$wgt,partial=tc$tc10, M=0.4,
#                 plus=FALSE,maxF=1.5,incrF=0.01, graph=T)
#vec10= mod.ypr.temp$F_vs_YPR$YPR[2:11]

#add these together and add to expanded data
vec.comp = append(vec1, vec2)
vec.comp = append(vec.comp, vec3)
vec.comp = append(vec.comp, vec4)
vec.comp = append(vec.comp, vec5)
vec.comp = append(vec.comp, vec6)
vec.comp = append(vec.comp, vec7)
vec.comp = append(vec.comp, vec8)
contour.exp$YPR = vec.comp

#make plot
ggplot(contour.exp, aes(x = F, y = age, z = YPR)) +
  geom_contour_filled() + labs(fill="YPR")+
  labs(x = "F", y = "age", z = "YPR") + theme_minimal()





