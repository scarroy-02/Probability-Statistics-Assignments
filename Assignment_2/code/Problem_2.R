library(stats)
vector = c()
gamma_simulation_MLE <- function(N,sim_size,alpha,sigma){
  for (j in 1:sim_size)
  {
    x<-rgamma(N,shape=alpha,scale=sigma)
    
    ll_gamma <- function(p,x){
      alpha<-p[1]
      sigma<-p[2]
      N*alpha*log(sigma)+N*log(gamma(alpha))-(alpha-1)*sum(log(x))+sum(x)/sigma
    }
    
    My_MLE <- function(x){
      ll_gamma1 <- function(p){
        ll_gamma(p,x)
      }
      z <- optim(par=c(1,1),ll_gamma1) 
      log(z$par[1])
    }
    vector <- c(vector, My_MLE(x))
  }
  hist(vector)
  abline(v=log(alpha),col='red')
  quantile(vector,probs=c(0.025,0.975))
}
gamma_simulation_MLE(20,1000,1.5,2.2)

gamma_simulation_MLE(40,1000,1.5,2.2)

gamma_simulation_MLE(100,1000,1.5,2.2)

