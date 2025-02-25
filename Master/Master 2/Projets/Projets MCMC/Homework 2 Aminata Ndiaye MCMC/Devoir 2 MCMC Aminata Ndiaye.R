#Exercice 3.3

#A
# On génère les variables aléatoires suivant une loi normale centrée réduite
n<-100000
Z<-rnorm(n)
#On calcule l'estim teur de Monte Carlo classique
hn_z<-sum(0*Z[Z>2.5 ]+1)/n
# Application numérique pour trouver le nombre de simulation pour avoir une précision de 3 decimales
Nnorm<-qnorm(0.975)^2*4/0.0001^2

#B
# On génère les variables aléatoires suivant une loi Gamma de paramètres(1,1)

X<-rgamma(n,1,1)
#On calcule l'estim teur de Monte Carlo classique
hn_x<-sum(0*X[X>5.3]+1)/n
# Application numérique pour trouver le nombre de simulation pour avoir une précision de 3 decimales
Ngamma<-qgamma(0.975,1,1)^2*4/0.0001^2

1-pgamma(5.3,1,1)

#Exercice 4.5

#a
m<-1000

#fonction pour générer des v.a suivant la loi de poisson
gen_pois<-function(x){
  return(rpois(1,x))
}

y_a<-rgamma(m,1,2)
X_class_a<-sapply(y_a,gen_pois)

#Estimation et variance pour MC classique
est_classique_a<-mean(X_class_a)
var_clas_a<-var(X_class_a)
#Estimation et variance pour Rao-Blackwell              
est_RB_a<-mean(y_a)             
var_RB_a<-var(y_a)                 

#b
#fonction pour générer des v.a suivant la loi normale
gen_norm<-function(x){
  return(rnorm(1,0,x))
}

y_b<-rgamma(m,1,2)
X_class_b<-sapply(y_b,gen_norm)
#Estimation et variance pour MC classiquev
est_classique_b<-mean(X_class_b)
var_clas_b<-var(X_class_b)
#c
#fonction pour générer des v.a suivant la loi binomiale
gen_binom<-function(x){
  return(rbinom(1,size=1,prob=x))
}

y_c<-rbeta(m,1,2)
X_class_c<-sapply(y_c,gen_binom)

#Estimation et variance pour MC classique
est_classique_c<-mean(X_class_c)
var_clas_c<-var(X_class_c)

#Estimation et variance pour Rao-Blackwell
est_RB_c<-mean(y_c)
var_RB_c<-var(y_c)



