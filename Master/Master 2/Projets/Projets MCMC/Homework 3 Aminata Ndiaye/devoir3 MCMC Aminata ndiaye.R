#Exercice 7.2
#1) Avec la loi normale
MH_loinormal<-function(n){
  simu<-c(0)
  x<-0
  for (i in 1:n-1){
    y<-rnorm(1,mean=0,sd=1)
    u<-runif(1)
    rho<-min((dt(y,4)*dnorm(x,0,1))/(dt(x,4)*dnorm(y,0,1)),1)
    if(u< rho ){
      simu<-c(simu,y)
      x<-y
    }else{
      simu<-c(simu,x)
    }
    
  }
  return(simu)
}

hist(MH_loinormal(10000),breaks=50,freq =FALSE,,main="Simulations avec algo MH et loi normale")
lines(seq(-5,5,length.out=1000),dt(seq(-5,5,length.out=1000),4),col="red")
#monitoring
estim_MH_norm<-function(n){
  return(mean(MH_loinormal(n)))
}
monitoring_loinormale<-sapply(seq(1,10000,100),estim_MH_norm)

plot(seq(1,10000,100),monitoring_loinormale,type="l",xlab="Itérations",ylab="moyenne",main="Monitoring MH loi normale")

#2) Avec la loi de student
MH_loit2<-function(n){
  simu<-c(0)
  x<-2
  for (i in 1:n-1){
    y<-rt(1,2)
    u<-runif(1)
    rho<-min((dt(y,4)*dt(x,2))/(dt(x,4)*dt(y,2)),1)
    if(u< rho ){
      simu<-c(simu,y)
      x<-y
    }else{
      simu<-c(simu,x)
    }
  }
  return(simu)
}

hist(MH_loit2(10000),breaks=50,freq =FALSE,main="Simulations avec algo MH et loi de student")
lines(seq(-10,10,length.out=1000),dt(seq(-10,10,length.out=1000),2),col="red")

#monitoring
estim_MH_t2<-function(n){
  return(mean(MH_loit2(n)))
}
monitoring_loit2<-sapply(seq(1,10000,100),estim_MH_t2)

plot(seq(1,10000,100),monitoring_loit2,type="l",xlab="Itérations",ylab="moyenne",main="Monitoring MH avec loi de student")
