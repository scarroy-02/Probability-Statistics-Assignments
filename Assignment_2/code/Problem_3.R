attach(faithful)
hist(faithful$waiting,xlab = 'waiting',probability = T,col='pink',main='')
waiting=sort(waiting)
set.seed(330)



NegLogLikeMix1 <- function(theta,data){
  mu1 = exp(theta[1])
  sigma1 = exp(theta[2])
  mu2 = theta[3]
  sigma2 = exp(theta[4])
  p = exp(theta[5])/(1+exp(theta[5]))
  n = length(data)
  l = 0
  for(i in 1:n){
    l = l + log(p*dgamma(data[i],mu1,sigma1)
                +(1-p)*dnorm(data[i],mu2,sigma2))
  }
  return(-l)
}
theta_initial=c(3,0.45,80,9,0.35)
fit = optim(theta_initial
            ,NegLogLikeMix1
            ,data=waiting
            ,control = list(maxit=1500))

theta_hat = fit$par
mu1_hat = exp(theta_hat[1])
sigma1_hat = exp(theta_hat[2])
mu2_hat = theta_hat[3]
sigma2_hat = exp(theta_hat[4])
p_hat = exp(theta_hat[5])/(1+exp(theta_hat[5]))

f1=c(mu1_hat,sigma1_hat,mu2_hat,sigma2_hat,p_hat)

hist(waiting,probability=T,ylim=c(0,0.05))
lines(waiting,f1[5]*dgamma(waiting,f1[1],f1[2])+(1-f1[5])*dnorm(waiting,f1[3],f1[4]),col='pink',lwd=3)

AIC1=length(theta_hat)*2+2*fit$value
#################

NegLogLikeMix2 <- function(theta2,data2){
  mu21 = exp(theta2[1])
  sigma21 = exp(theta2[2])
  mu22 = exp(theta2[3])
  sigma22 = exp(theta2[4])
  p = exp(theta2[5])/(1+exp(theta2[5]))
  n = length(data2)
  l = 0
  for(i in 1:n){
    l = l + log(p*dgamma(data2[i],mu21,sigma21)
                +(1-p)*dgamma(data2[i],mu22,sigma22))
  }
  return(-l)
}
theta2_initial=c(4,0,4,0,0.4)
fit2 = optim(theta2_initial
            ,NegLogLikeMix2
            ,data=waiting
            ,control = list(maxit=1500))

theta2_hat = fit2$par

mu21_hat = exp(theta2_hat[1])
sigma21_hat = exp(theta2_hat[2])
mu22_hat = exp(theta2_hat[3])
sigma22_hat = exp(theta2_hat[4])
p2_hat = exp(theta2_hat[5])/(1+exp(theta2_hat[5]))

f2=c(mu21_hat,sigma21_hat,mu22_hat,sigma22_hat,p2_hat)

lines(waiting,f2[5]*dgamma(waiting,f2[1],f2[2])+(1-f2[5])*dgamma(waiting,f2[3],f2[4]),col='purple',lwd=3)


AIC2= length(theta2_hat)*2+2*fit2$value
################
NegLogLikeMix3 <- function(theta3,data3){
  mu31 = (theta3[1])
  sigma31 = exp(theta3[2])
  mu32 = (theta3[3])
  sigma32 = exp(theta3[4])
  p = exp(theta3[5])/(1+exp(theta3[5]))
  n = length(data3)
  l = 0
  for(i in 1:n){
    l = l + log(p*dlnorm(data3[i],meanlog=mu31,sdlog=sigma31)
                +(1-p)*dlnorm(data3[i],meanlog=mu32,sdlog=sigma32))
  }
  return(-l)
}
theta3_initial=c(2.76,-2.25,4.4,-2.6,0.35)
fit3 = optim(theta3_initial
             ,NegLogLikeMix3
             ,data=waiting
             ,control = list(maxit=1500))

theta3_hat = fit3$par

mu31_hat = theta3_hat[1]
sigma31_hat = exp(theta3_hat[2])
mu32_hat = theta3_hat[3]
sigma32_hat = exp(theta3_hat[4])
p3_hat = exp(theta3_hat[5])/(1+exp(theta3_hat[5]))

final_vector=c(mu31_hat,sigma31_hat,mu32_hat,sigma32_hat,p3_hat)



AIC3=length(theta3_hat)*2+2*fit3$value



dMix<-function(x,theta){
  mu1 = theta[1]
  sigma1 = exp(theta[2])
  mu2 = theta[3]
  sigma2 = exp(theta[4])
  p = exp(theta[5])/(1+exp(theta[5]))
  f = theta[5]*dlnorm(x,theta[1],theta[2])+(1-theta[5])*dlnorm(x,theta[3],theta[4])
  return(f)
}

lines(waiting,final_vector[5]*dlnorm(waiting,final_vector[1],final_vector[2])+(1-final_vector[5])*dlnorm(waiting,final_vector[3],final_vector[4]),lwd=3)


prob=integrate(dMix,60,70,final_vector)

cat("AIC for model 1 is ",AIC1)
cat("AIC for model 2 is ",AIC2)
cat("AIC for model 3 is ",AIC3)

cat("As AIC of model 3 is lowest, it is the most suitable model")

cat("P(60<waiting<70)=",prob)

