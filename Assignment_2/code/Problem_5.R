library(quantmod)
getSymbols('TCS.NS')
tail(TCS.NS)
plot(TCS.NS$TCS.NS.Adjusted)
getSymbols('^NSEI')
tail(NSEI)
plot(NSEI$NSEI.Adjusted)

TCS_rt = diff(log(TCS.NS$TCS.NS.Adjusted))
Nifty_rt = diff(log(NSEI$NSEI.Adjusted))
retrn = cbind.xts(TCS_rt,Nifty_rt) 
retrn = na.omit(data.frame(retrn))

plot(retrn$NSEI.Adjusted,retrn$TCS.NS.Adjusted
     ,pch=20
     ,xlab='Market Return'
     ,ylab='TCS Return'
     ,xlim=c(-0.18,0.18)
     ,ylim=c(-0.18,0.18))
grid(col='grey',lty=1)

names(retrn) = c('TCS','Nifty')
ExpY = mean(retrn[,'TCS'])
ExpX = mean(retrn[,'Nifty'])
VarY = var(retrn[,'TCS'])
VarX = var(retrn[,'Nifty'])
CovXY = cov(retrn[,'TCS'],retrn[,'Nifty'])
beta_hat_moments = CovXY/VarX
alpha_hat_moments = ExpY - (beta_hat_moments*ExpX)
sigma_hat_moments = sqrt((sum((retrn[,'TCS']-alpha_hat_moments-beta_hat_moments*retrn[,'Nifty'])^2))/length(retrn[,'TCS']))
cat("Optimal parameters are",c(alpha_hat_moments,beta_hat_moments,sigma_hat_moments))

fit = summary(lm(TCS~Nifty, data=retrn))
beta_hat_OLS = fit$coef[2,1]
alpha_hat_OLS = fit$coef[1,1]
sigma_hat_OLS = fit$sigma
cat("Optimal parameters are",c(alpha_hat_OLS,beta_hat_OLS,sigma_hat_OLS))

TCS_return = alpha_hat_moments + beta_hat_moments * log(18200/18000)
TCS_new = 3200*exp(TCS_return)
cat("New TCS price would be expected to be Rs.", TCS_new)

