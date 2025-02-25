

#Exercice 8.5
gen_SliceSampler<-function(n,x0=0.4){
  x<-x0
  simu<-c()
  for(i in 1:n ){
    omega<-runif(1,0,exp(-x^2/2))
    x<-runif(1,-sqrt(-2*log(omega)),sqrt(-2*log(omega)))
    simu<-c(simu,x)
  }
  return(simu)
}

quantile_gen<-quantile(gen_SliceSampler(n=10000), probs=c(0.5,0.75,0.8,0.9,0.95,0.99,0.995,0.999,0.9999),digits=4)
quantile_norm<-quantile(rnorm(10000), probs=c(0.5,0.75,0.8,0.9,0.95,0.99,0.995,0.999,0.9999),digits=4)
print(quantile_gen)
print(quantile_norm)

#Histogramme 
hist(gen_SliceSampler(n=10000),freq=FALSE,breaks=100,main='Loi normale centrée réduite générées par slice sampling')
lines(seq(-4,4,length.out=1000),dnorm(seq(-4,4,length.out=1000),0,1),col="blue")

#Diagramme quantile quantile
plot(quantile_gen, quantile_norm,col="red",main="Diagramme quantile quantile",ylab= "Quantiles de dnorm(x,0,1)", xlab="Quantiles échantillons générés par slice sampler ")
lines(seq(0,4,length.out=1000),seq(0,4,length.out=1000),col="blue")




#Exercice 8.6


#questin a
SliceSampler_gamma<-function(n,x0=0.4,p=3,lambda=1){
  x<-x0
  simu<-1:n
  for(i in 1:n ){
    omega_1<-runif(1,0,exp(-lambda*x))
    omega_2<-runif(1,0,x^{p-1})
    x<-runif(1,min(-log(omega_1)/lambda,omega_2^{1/(p-1)}),max(-log(omega_1)/lambda,omega_2^{1/(p-1)}))
    simu[i]<-x
  }
  return(simu)
}
p=3
lambda=1
quantile_slice_gamma<-quantile(SliceSampler_gamma(n=10000), probs=c(0.5,0.75,0.8,0.9,0.95,0.99,0.995,0.999,0.9999),digits=4)
quantile_gamma<-quantile(rgamma(10000,p,lambda), probs=c(0.5,0.75,0.8,0.9,0.95,0.99,0.995,0.999,0.9999),digits=4)
print(quantile_slice_gamma)
print(quantile_gamma)
#Histogramme
hist(SliceSampler_gamma(n=10000),freq=FALSE,breaks=100,main='Loi gamma(3,1) générées par slice sampling')
lines(seq(0,14,length.out=1000),dgamma(seq(0,14,length.out=1000),p,lambda),col="blue")
#Diagramme quantile quantile
plot(quantile_slice_gamma, quantile_gamma,col="red",main="Diagramme quantile quantile",ylab= "Quantiles de dgammam(x,3,1)", xlab="Quantiles échantillons générés par slice sampler ")
lines(seq(0,14,length.out=1000),seq(0,14,length.out=1000),col="blue")



#question b
#Fonction pour calculer le plus grand n tel que n!<=x
inverse.factorial<-function(x){
  n<-0
  i<-1
  while (i<=x) {
    n<-n+1
    i<-i*n
    
  }
  return (n-1)
}

SliceSampler_poisson<-function(n,x0=1,lambda=0.4){
  x<-x0
  simu<-1:n
  i<-1
  while (i<=n){
    omega_1<-runif(1,0,lambda^x)
    omega_2<-runif(1,0,1/factorial(x))
    c<-log(lambda)
    if (lambda<1){
      x<-sample(0:min(max((log(omega_1)/log(lambda))%/%1,0),inverse.factorial(1/omega_2)),size=1)
      simu[i]<-x
      i<-i+1
    }else{
      born1<-max((log(omega_1)/log(lambda))%/%1,0)
      born2<-inverse.factorial(1/omega_2)
      print(c(born1,born2))
      if (born1<=born2){
        x<-sample(born1:born2,size=1)
        simu[i]<-x
        i<-i+1
      }
      
    }
    
  }
  return(simu)
}
lmbd=0.4
quantile_slice_poisson<-quantile(SliceSampler_poisson(n=1000,lambda=lmbd), probs=c(0.5,0.75,0.8,0.9,0.95,0.99,0.995,0.999,0.9999),digits=4)
quantile_poisson<-quantile(rpois(1000,lmbd), probs=c(0.5,0.75,0.8,0.9,0.95,0.99,0.995,0.999,0.9999),digits=4)
print(quantile_slice_poisson)
print(quantile_poisson)

#Diagramme quantile quantile
plot(quantile_slice_poisson, quantile_poisson,col="red",main="Diagramme quantile quantile",ylab= "Quantiles de dpois(x,O.4)", xlab="Quantiles échantillons générés par slice sampler ")
lines(seq(0,14,length.out=100),seq(0,14,length.out=100),col="blue")





#Exercice 9.2
bivar_2Gibbs<-function(n,x0=1,rho=0.3){
  x<-x0
  simu_x<-1:n
  simu_y<-1:n
  for (i in 1:n){
    y<-rnorm(1,rho*x,1-rho^2)
    x<-rnorm(1,rho*y,1-rho^2)
    simu_x[i]<-x
    simu_y[i]<-y
  }
  return(cbind(simu_x,simu_y))
}
n=10^5
simu_gauss<-bivar_2Gibbs(n)
som_xy_carre<-simu_gauss[,1]^2+simu_gauss[,2]^2
print(c("moyenne de X",mean(simu_gauss[,1])))
print(c("moyenne de Y",mean(simu_gauss[,2])))
print(c("variance de X",var(simu_gauss[,1])))
print(c("variance de Y", var(simu_gauss[,2])))
print(c("correlation entre X et Y",cor(simu_gauss[,1],simu_gauss[,2])))
hist(som_xy_carre,freq=FALSE,breaks = 100,main='histogramme de la variable aléatoire X^2+Y^2')

#estimation de la probabilité par méthode de Monte Carlo
estim_prob<-function(n){
  simu_gauss<-bivar_2Gibbs(n)
  som_xy_carre<-simu_gauss[,1]^2+simu_gauss[,2]^2
  return(sum(1+0*som_xy_carre[som_xy_carre>2])/n)
}
estim_prob(100000)
