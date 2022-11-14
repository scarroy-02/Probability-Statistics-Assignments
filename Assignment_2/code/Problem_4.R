library(MASS)
plot(Insurance$Holders,Insurance$Claims
     ,xlab = 'Holders',ylab='Claims',pch=20)
grid()

Holders = Insurance$Holders
Claims = Insurance$Claims

#MLE and BIC functions

MyMLE <- function(func,x,initial){
  func_one<- function(p){
    func(p,x)
  }
  optim(par = initial, func_one)
}

MyBIC <- function(MLEv, parm, data){
  N <- length(data)
  (length(parm))*log(N)-2*(MLEv$value)
}

#Part A

ll_A <- function(parm, data=list(Claims,Holders)){
  beta0 <- parm[1]
  beta1 <- parm[2]
  sigma <- exp(parm[3])
  N <- length(data)
  -sum(log(dnorm((data$Claims - beta0 - beta1*data$Holders),0,sigma)))
}

MLEvA <- MyMLE(ll_A, Insurance, c(100,100,50))
cat("MLE for model A :", c(MLEvA$par[1], MLEvA$par[2], exp(MLEvA$par[3])))

b_A <- MyBIC(MLEvA, MLEvA$par, Insurance)
cat("BIC value for model A :", b_A)

#Part B

dLaplace <- function(x, mu=0, b=1, params=list(mu, b),...){
  if(!missing(params)){
    mu <- params$mu
    b <- params$b
  }
  d <- exp(-abs(x-mu)/b) / (2*b)
}

ll_B <- function(parm, data=list(Claims,Holders)){
  beta0 <- parm[1]
  beta1 <- parm[2]
  sigma <- exp(parm[3])
  N <- length(data)
  -sum(log(dLaplace((data$Claims - beta0 - beta1*data$Holders),0,sigma^2)))
}

MLEvB <- MyMLE(ll_B, Insurance, c(100,100,50))
cat("MLE for model B :", c(MLEvB$par[1], MLEvB$par[2], exp(MLEvB$par[3])))

b_B <- MyBIC(MLEvB, MLEvB$par, Insurance)
cat("BIC for model B :", b_B)

#Part C

dLognormal <- function(x, mu=0, b=1, params=list(mu, b),...){
  if(!missing(params)){
    mu <- params$mu
    b <- params$b
  }
  d <- 1/(x*b*sqrt(2*pi)) * exp(-((log(x)-mu)^2)/(2*b^2))
}

ll_C <- function(parm, data=list(Claims,Holders)){
  beta0 <- parm[1]
  beta1 <- parm[2]
  sigma <- exp(parm[3])
  N <- length(data)
  x <- 0
  for (i in 1:N){
    if(data$Claims[i] == 0){}
    else {
      x = x - log(dLognormal(data$Claims[i],(beta0 + beta1*log(data$Holders[i])),sigma)) 
    }
  }
  x
}

MLEvC <- MyMLE(ll_C, Insurance, c(100,100,50))
cat("MLE for model C :", c(MLEvC$par[1], MLEvC$par[2], exp(MLEvC$par[3])))

b_C <- MyBIC(MLEvC, MLEvC$par, Insurance)
cat("BIC for model C :", b_C)

#Part D

dGamma <- function(x, alpha=0, beta=1, params=list(alpha, beta),...){
  if(!missing(params)){
    alpha <- params$alpha
    beta <- params$beta
  }
  d <- ((exp(-x/beta)*x^(alpha-1))/((beta^alpha)*gamma(alpha)))
}

ll_D <- function(parm, data=list(Claims,Holders)){
  beta0 <- parm[1]
  beta1 <- parm[2]
  sigma <- parm[3]
  N <- length(data)
  x <- 0
  for (i in 1:N){
    if(data$Claims[i] == 0){}
    else {
      x = x - log(dGamma(data$Claims[i],(exp(beta0 + beta1*log(data$Holders[i]))),sigma))
    }
  }
  x
}

MLEvD <- MyMLE(ll_D, Insurance, c(3,0,10))
cat("MLE for model D :", c(MLEvD$par[1], MLEvD$par[2], MLEvD$par[3]))

b_D <- MyBIC(MLEvD, MLEvD$par, Insurance)
cat("BIC for model D :", b_D)

#Comparison

bic_values <- c('A'=b_A, 'B'=b_B, 'C'=b_C, 'D'=b_D)
bic_values = sort(bic_values)
cat("Comparison of BIC values :")
bic_values
