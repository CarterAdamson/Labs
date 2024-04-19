lnorm.bias.cor<-function(x){
   G<-function(t){
       j<-2
       s<-(((m-1)**3)/((m+1)*(m**2)*2))*t**2
       a<-1+(((m-1)/m)*t)+s
       while (s>0.00001){
        j<-j+1
        b<-(((m-1)**2)/(m*(m+2*j-3)*j))*t
        s<-s*b
        a<-a+s
       }
       a
      }
m<-x$df.residual
yr.coef<-exp(dummy.coef(x)[[2]])
sigma2<<-summary(x)$dispersion 
nu<<-dummy.coef(x)[[2]]+coef(x)[1]  
yr.n<-length(nu) 
var.i<-diag(vcov(x))[1]
var.p<-diag(vcov(x))[2:yr.n]
cov.p<-vcov(x)[2:yr.n,1]
var.all<<-c(var.i,var.p+var.i+2*cov.p)

#Calculate mean factor
tmean<-(x$df.residual+1)/(2*x$df.residual)*(sigma2-var.all)
  cormean<-NULL
   for(i in 1:as.numeric(length(tmean))){
   cormean[i]<-exp(nu[i])*G(tmean[i])
   }
   cor.yr.mean<<-cormean

#Calculate var factor
tvarp2<-(x$df.residual+1)/(x$df.residual)*(sigma2-2*var.all)
    corvar<-NULL
    for(i in 1:as.numeric(length(tmean))){
       corvar[i]<-exp(2*nu[i])*(G(tmean[i])^2-G(tvarp2[i]))
      } 
    cor.yr.se<<-sqrt(corvar)
  results<-as.data.frame(cbind(cor.yr.mean,cor.yr.se))
  results
}





