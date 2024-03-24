library(popbio)
library(ggplot2)

# Question 2 ####

#a - run simulation
n.ages = 5
ages=c("Age 1", "Age 2", "Age 3", "Age 4", "Age 5")
f.1 = 0
f.2 = 0.75
f.3 = 1.5
f.4 = 1.5
f.5 = 1.5
s.1 = 0.4
s.2 = 0.5
s.3 = 0.6
s.4 = 0.6
s.5 = 0

leslie = matrix(c(f.1, f.2, f.3, f.4, f.5,
                   s.1,  0,   0,   0,   0,
                   0,   s.2,  0,   0,   0,
                   0,    0,  s.3,  0,   0, 
                   0,    0,   0,  s.4,  0), nrow=n.ages, byrow=T, dimnames = list(ages, ages))
N0=c(100, 65, 42, 20, 11)
n.years=15
project.15<-pop.projection(leslie, N0, n.years)

#b - create plot
years=c(0:(n.years-1))
plot1.df=data.frame(t=years, N=project.15$pop.sizes)
plot1<-ggplot(data=plot1.df, aes(x=t, y=N))+geom_line()+geom_point()+theme_bw()

#c - stable age distribution
stage.vector.plot(project.15$stage.vectors)

# Question 3 ####

eig=eigen.analysis(leslie)

# Question 4 ####

#re-simulate with increased age-1 survival
s.1 = 0.5
leslie.2 = matrix(c(f.1, f.2, f.3, f.4, f.5,
                  s.1,  0,   0,   0,   0,
                  0,   s.2,  0,   0,   0,
                  0,    0,  s.3,  0,   0, 
                  0,    0,   0,  s.4,  0), nrow=n.ages, byrow=T, dimnames = list(ages, ages))
project.15.2<-pop.projection(leslie.2, N0, n.years)
plot2.df=data.frame(t=years, N=project.15.2$pop.sizes)
plot2<-ggplot(data=plot2.df, aes(x=t, y=N))+geom_line()+geom_point()+theme_bw()