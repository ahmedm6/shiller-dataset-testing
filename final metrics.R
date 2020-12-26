library(dynlm)
library(broom)
library(knitr)
library(tidytext)
library(tseries)
library(lmtest)

data("shiller", package="POE5Rdata")
shiller.ts <- ts(shiller, start=c(1871,1), end=c(2015,9), frequency = 12)
div<-shiller.ts[,"div"]
price<-shiller.ts[,"price"]


#testing stationarity for div
plot(div) # plot shows div needs to be detrended
dfTestDiv <- dynlm(d(div)~trend(div) + L(div) + L(d(div), 1:2) + L(d(div), 4))
kable(tidy(dfTestDiv)) 
# Statistic is -3.57. In the rejection region: reject the null of non-stationary. Dividends are
# trend-stationary. 
adf.test(div) #div is trend stationary

# testing stationarity for price
plot(price) # plot shows price needs to be detrended
dfTestPrice <- dynlm(d(price)~trend(price) + L(price) + L(d(price), 1:2) + L(d(price), 5))

kable(tidy(dfTestPrice)) 
# Statistic is -2.757. NOT in the rejection region: fail to reject the null of non-stationary.
# Will test for diff(price)
adf.test(price) # fail to reject the null of non-stationary
dp <- diff(price)
adf.test(dp) # Difference of price is stationary! now to check which lags I can use from 

dpReg0 <- dynlm(dp ~ L( dp , 1:3))
kable(tidy(dpReg0))
AIC(dpReg0)
BIC(dpReg0)
bgtest(dpReg0, order = 10, type = 'Chisq')
errDpReg0 <- dpReg0$residuals

dpReg1 <- dynlm(dp ~ L( dp , 1:2))
kable(tidy(dpReg1))
AIC(dpReg1)
BIC(dpReg1)
bgtest(dpReg1, order = 10, type = 'Chisq')
errDpReg1 <- dpReg1$residuals

dpReg2 <- dynlm(dp ~ L( dp , 1:2)  + L( dp , 5) )
# dpReg2 <- dynlm(dp ~ L( dp , 1:2)  + L( dp , 5) + L( dp , 13))
kable(tidy(dpReg2))
AIC(dpReg2)
BIC(dpReg2)
bgtest(dpReg2, order = 10, type = 'Chisq')
errDpReg2 <- dpReg2$residuals

#1b

acf(errDpReg0)
acf(errDpReg1)
acf(errDpReg2)



#1c

cst <- c(0)
coef(dpReg2)
rho1 <- coef(dpReg2)[2]
rho2 <- coef(dpReg2)[3]
rho3 <- coef(dpReg2)[4]

dpsm <- matrix( 0, nrow=20, ncol=1) #no shock
dp2 <- matrix( 0, nrow=20, ncol=1) #shock
irf <- matrix( 0, nrow=20, ncol=1) #difference

dp2[4,1] <- -2*sd(dp)
irf[4,1] <- dp2[4,1]-dpsm[4,1]

for (p in 5:20){ 
  dp2[p,1] <- cst+rho1*dp2[p-1,1]+rho2*dp2[p-2,1]+rho3*dp2[p-3,1]#+rho5*dp2[p-5,1] #+er[p,1]
  dpsm[p,1] <- cst+rho1*dpsm[p-1,1]+rho2*dpsm[p-2,1]+rho3*dpsm[p-3,1]#+rho5*dpsm[p-5,1]
  irf[p,1] <- dp2[p,1]-dpsm[p,1]
}

plot(irf)

# 2) Looking for ARCH-effects in dprice. Printing the output of each step. 
# Doing the LM test, concluding, and plotting the estimated variance.
r <- dp

adf.test(r)

ymean <- dynlm(r~1) #mean equation.
kable(tidy(ymean))
e <- resid(ymean)
e2 <- e^2

arch <- dynlm(e2~L(e2)) #first-order
kable(tidy(arch))
sumarch <- summary(arch)
(lm <- (length(r)-1)*sumarch$r.squared)
alpha <- 0.05
(Chicr <- qchisq(1-alpha, 1)) #we have arch-effects
ARCH_Model <- arch$fitted.values
plot(ARCH_Model)

