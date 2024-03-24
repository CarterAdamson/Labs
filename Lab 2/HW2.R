library(ggplot2)
data = data.frame(ID = 1:5, X=c(13,3,2,8,5), Y=seq(10, 30, by=5), Z=letters[1:5])
#Nt = N0 e^-Zt
t=c(1.0:50)
N=c(1.0:50)
data=data.frame(N=N, t=t)
Z=c(0.05, 0.1, 0.2, 1.0, 2.0, 3.0)
N0=100

#plotting - tried a loop but couldn't make it work w ggplot
plot1<-ggplot(data=data, aes(x=t))
N1=N0*exp((0-Z[1])*t)
plot1<-plot1 + geom_line(aes(y=N1, color="Z = 0.05"))
N2=N0*exp((0-Z[2])*t)
plot1<-plot1 + geom_line(aes(y=N2, color="Z = 0.1"))
N3=N0*exp((0-Z[3])*t)
plot1<-plot1 + geom_line(aes(y=N3, color="Z = 0.2"))
N4=N0*exp((0-Z[4])*t)
plot1<-plot1 + geom_line(aes(y=N4, color="Z = 1.0"))
N5=N0*exp((0-Z[5])*t)
plot1<-plot1 + geom_line(aes(y=N5, color="Z = 2.0"))
N6=N0*exp((0-Z[6])*t)
plot1<-plot1 + geom_line(aes(y=N6, color="Z = 3.0"))
plot1 + xlab("t")+ylab("N") + theme_bw()

#maths
var1<-log(0.5)
FAE<-eaf/(0-Z)
B1 <- 1-exp(-0.2)
B2 <- 1-exp(-1)

#reproducing a figure
A <- 1 - exp(0-Z)
sequence = c(0.1, 0.2, 0.3, 0.5, 0.8, 1)
data2 <- data.frame (Z=Z, A=A, s=sequence)
plot2<-ggplot(data=data2, aes(x=Z, y=A))+geom_line(aes(linetype="A vs. Z"))+geom_point()
plot2+geom_line(aes(x=s, y=s, linetype="1:1"))+
  scale_linetype_manual(values=c("A vs. Z"="solid", "1:1"="dashed"))+
  xlab("Z") + ylab ("A")+ theme_bw()+
  ggtitle("Comparison of annual (A) and instantaneous (Z) mortality")+theme(plot.title = element_text(hjust = 0.5))

                