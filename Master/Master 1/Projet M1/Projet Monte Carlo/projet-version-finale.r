# Projet R
# Exercice 1

# Attention variables globales mettre dans fonction finale


# monte carlo classique

mcc <- function() {
  a <- -3
  b <- 5
  mu <- -1
  n <- 10^6

  rgen_WTX_mcc <- function() {
    Wm <- rnorm(n, mean = mu, sd = 1)
    matX <- as.matrix(Wm)
    Ti <- 1 * (Wm < a | Wm > b)
    WTi <- Wm * (Wm < a | Wm > b)
    Wm <- Wm * (Wm >= a & Wm <= b)
    m <- 2
    while (length(Wm[Wm != 0] != 0)) {
      Xm <- rnorm(length(Wm[Wm != 0]), mean = mu, sd = 1)
      Wm[Wm != 0] <- Wm[Wm != 0] + Xm
      matX <- cbind(matX, Wm)
      Ti <- Ti + m * (Wm < a | Wm > b)
      WTi <- WTi + Wm * (Wm < a | Wm > b)
      Wm <- Wm * (Wm >= a & Wm <= b)
      m <- m + 1
    }
    return(cbind(WTi, Ti, matX))
  }

  h_mcc <- function(w) {
    return(1 * (w >= b))
  }

  WTX_mcc <- rgen_WTX_mcc()
  WT_ep <- WTX_mcc[, 1]

  mean(h_mcc(WT_ep))

  return(WTX_mcc)
}

# echantillonage preferentiel

ep <- function() {
  a <- -3
  b <- 5
  mu <- -1
  n <- 10^6

  rgen_WTX_ep <- function() {
    Wm <- rnorm(n, mean = -mu, sd = 1)
    matX <- as.matrix(Wm)
    Ti <- 1 * (Wm < a | Wm > b)
    WTi <- Wm * (Wm < a | Wm > b)
    Wm <- Wm * (Wm >= a & Wm <= b)
    m <- 2
    while (length(Wm[Wm != 0] != 0)) {
      Xm <- rnorm(length(Wm[Wm != 0]), mean = -mu, sd = 1)
      Wm[Wm != 0] <- Wm[Wm != 0] + Xm
      matX <- cbind(matX, Wm)
      Ti <- Ti + m * (Wm < a | Wm > b)
      WTi <- WTi + Wm * (Wm < a | Wm > b)
      Wm <- Wm * (Wm >= a & Wm <= b)
      m <- m + 1
    }
    return(cbind(WTi, Ti, matX))
  }

  h_ep <- function(w) {
    return(1 * (w >= b) * exp(2 * mu * w))
  }

  WTX_ep <- rgen_WTX_ep()
  WT_ep <- WTX_ep[, 1]

  mean(h_ep(WT_ep))

  return(WTX_ep)
}

# efficacité relative

library(microbenchmark)
toto <- microbenchmark(mcc(), ep(), times = 100)

# evaluation des couts

cout_mcc <- median(toto$time[toto$expr == "mcc()"])
cout_ep <- median(toto$time[toto$expr == "ep()"])

# calculs des variances

## methode 1

output_mcc <- mcc()
WTi_mcc <- output_mcc[, 1]
var_mcc <- var(1 * (WTi_mcc >= b))


# methode 2

output_ep <- ep()
Wti_ep <- output_ep[, 1]
var_ep <- var(1 * (Wti_ep >= b) * exp(2 * mu * Wti_ep))


# efficacité relative

eff_rel <- ((var_mcc) * cout_mcc) / ((var_ep) * cout_ep)
eff_rel

# eff_rel~45000 donc la méthode par echantillonage preferentiel est plus efficace