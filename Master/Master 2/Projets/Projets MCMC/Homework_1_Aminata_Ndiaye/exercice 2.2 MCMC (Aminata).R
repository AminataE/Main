#Devoir 1 MCMC AMINATA NDIAYE
#Exercice 2.2
#Nous utilisons la méthode de la fonction inverse
generateur_binomiale<-function(n,size,p){
  U<-matrix(runif(n*size),n,size)
  U[p<=U]<-0
  U[0<U]<-1
  return(rowSums(U))
}

hist(generateur_binomiale(10000,25,0.2),freq=FALSE,col=4,density=7,main="Simulations de binomiale (25,0.2)",xlab='x',ylim=c(0,0.25)) #L'histogramme de notre simulateur
hist(rbinom(10000,25,0.2),freq = FALSE,col=2.5,add=TRUE,density=7,) # L'histogramme du simulateur de Rstudio
lines(seq(0,25,1),dbinom(seq(0,25,1),25,0.2), col="black",type = 'l',lwd=1,) #densité de la loi binomiale
legend(8,0.16,c('generateur_binomiale','rbinom',"dbinom"), fill=c(4,2.5,rgb(0,0,0,1)),lty=c(0,0,0,1),bty='n',border=NA)

#Nous procédons de la même manière pour simuler la série logarithmique
# Dans le cas de la série logarithmique, x prend ses valeurs dans N qui est non fini. Nous allons déterminer un seuil d'erreur
#nous permettant de négliger les valeurs de x au delà d'une certaine valeur.


generateur_logserie<-function(n,p,epsilon=0.0001){
  #On calcule les probabilités cumulées et on détermine la valeur(dans N) telle que avec une probabilité de 0.9999 x soit plus petit que cette valeur (epsilon =0.0001)
  i<-1
  som<--(1-p)^i/(i*log(p))
  cumprob<-c(0,-(1-p)^i/(i*log(p)))
  while (1-cumprob[length(cumprob)]>epsilon){
    i<-i+1
    som<-som-(1-p)^i/(i*log(p))
    cumprob<-c(cumprob,som)
  }
  #On simule en utilisant la méthode de la fonction inverse.
  U<-runif(n)
  U[U>=cumprob[i+1]]<-i
  for (k in seq(1,i,1)){
    U[cumprob[k]<=U & U<cumprob[k+1]]<-k
  }
  return(U)
}
#fonction de masse de la série logarithmique
fmp_logserie<-function(x,p){
  return(-(1-p)^x/(x*log(p)))
}
hist(generateur_logserie(5000,0.2),freq=FALSE,col=4,density=7,main="Simulations séries logarithmiques(erreur=0.0001, p=0.2)",xlab='x') #histogramme de notre simulateur
lines(seq(1,25,1),fmp_logserie(seq(1,25,1),0.2)) #fonction de masse de la série logarithmique

