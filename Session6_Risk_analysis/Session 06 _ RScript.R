#####################################################################
# Exercise 1 - Value at Risk: Gaussian and Kernel Estimations
####################################################################

#Import the CSV file "StocksWeeklyReturns"

#Quantile Estimation
#####################################################################
#Historical quantile
quantile(StocksWeeklyReturns[,2],probs=0.01) #R function quantile
quantile(StocksWeeklyReturns[,2],probs=0.05)#Replication of the matlab function quantile

#####################################################################
# VaR estimation
#####################################################################

#####################################################################
# 1-2.
#portfolio weight vector a, following the notation in the lecture notes
#a=[1 0 0 0 0 0 0 0 0 0]'
a=c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
#change the portfolio weights and experiment!
#a=t(c(0.23, 0.05, 0.13, 0.08, 0.1, 0.06, 0.28, 0.01, 0.04, 0.02))' #another fancy portfolio

n_r=length(StocksWeeklyReturns[,1])
print('------- Question 1 -------\n')
print('------- Mean -------\n')
mu=cbind(apply(StocksWeeklyReturns[1:n_r,2:11],2,mean)) #empirical mean
mu
print('------- Var/Covar Matrix -------\n')
SIGMA=cov(StocksWeeklyReturns[1:n_r,2:11])      #empirical var-covar matrix
SIGMA
library(fBasics)          #Otherwise, you can use the "timeDate" package (see session 1)
print('------- Skewness -------\n')
skew=apply(StocksWeeklyReturns[1:n_r,2:11],2,skewness)  #empirical skewness
skew
print('------- Kurtosis -------\n')
kurt=apply(StocksWeeklyReturns[1:n_r,2:11],2,kurtosis)+3  #empirical excess kurtosis
kurt

print('------- Question 2 -------\n')
z_95=qnorm(0.95,mean=0,sd=1) #Inverse normal cumulative distribution function giving the 95% quantile
GaussianVaR=-a%*%mu+(a%*%SIGMA%*%cbind(a))^(1/2)*z_95
GaussianVaR

#####################################################################
# 3.
data=StocksWeeklyReturns[1:n_r,2:11]

t=-data*cbind(a) #This is the negative of the returns weighted by their portfolio weight (a)
w=apply(t,1,sum)
z=cbind(w)   

mean(z)
var(z)
sd(z)
skewness(z)
kurtosis(z)+3


T=length(z)
h=sd(z)*T^(-1/5)
alpha=0.05
NonParamVaR=NULL
l=1000

for (i in 1:T){
  VaR=z[i]
  x=NULL
  Minobj=NULL
  for (j in 1:T) {
    x[j]=pnorm((z[j]-VaR)/h)}
    Minobj=(sum(x)/T-alpha)^2
  if (Minobj<l) {
    l=Minobj
    NonParamVaR=VaR}
}

print(NonParamVaR)


#We see that the Gaussian Hypothesis seems to underestimate the true VAR!!! 
#That is a major reason why it is so problematic to assume Gaussian returns!


######################################################################
#Exercise 2 - VaR and ES: Gaussian and Kernel Estimations 
######################################################################

######################################################################
# Gaussian VaR
######################################################################

FireLosses=FIREDAT[,2]
alpha=0.01

mu=mean(FireLosses)
sigma=sd(FireLosses)
z_99=qnorm(.99,0,1) #obtain the extreme 99% quantile of loss

GaussianVaR=mu+sigma*z_99
GaussianVaR

GaussianES=mu+sigma*dnorm(z_99,0,1)/alpha
GaussianES


######################################################################
# Kernel approach
######################################################################

VaR=GaussianVaR
h=sd(FireLosses)*length(FireLosses)^(-.2)

T=length(FireLosses)
alpha=0.01
NonParamVaR=NULL
l=100^10

for (i in 1:T){
  VaR=FireLosses[i]
  x=NULL
  Minobj=NULL
  for (j in 1:T) {
    x[j]=pnorm((FireLosses[j]-VaR)/h)
    Minobj=(sum(x)/T-alpha)^2}
  if (Minobj<l) {
    l=Minobj
    NonParamVaR=VaR}
}

NonParamVaR


NonParamES=mean(FireLosses*pnorm((FireLosses-NonParamVaR)/h))/alpha
NonParamES

