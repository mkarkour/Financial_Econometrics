################################################################################
###########################      QUESTION 1        #############################
################################################################################
#A CHAQUE FOIS QUE L ON FAIT L EXO ON AURA DES DONNES DIFF A CAUSE DE LA RANDOM
#SIZE
#Q1.1. Simulate T = 100 observations of the law N(1,16). (We will
#consider these observations as a series of monthly
#percentage returns on a stock).

T = 100
mu = 1
sigma = 4 #racine de 16, le 16 est la variance

#Construct my sample
X=rnorm(T,mu,sigma) #to generate random gaussian values
#size = T (premiere parametre)
X #j'obtient un random sample

#Q1.2. Compute the statistic SR = u/sigma based on the total sample.
#Think of SR as a Sharpe ratio, if we assume the risk-free rate is zero.

"first step estimate parameter of my original sample"
mu_hat = mean(X)
mu_hat

sigma_hat = sd(X)
sigma_hat

SR_hat = mu_hat/sigma_hat #equivalent to the sharpe ratio
SR_hat


#Q1.3. Form a 95% bootstrap confidence interval for SR (hint: use
#the function boot() from the package "boot" on R)

B=2000 #number of time i want to bootstrap my sample

library(boot)
SR_fct <- function (data,i){
  d = data[i]
  u = mean(d)
  sigma = sd(d)
  SR=u/sigma
  return(SR)
}

Boot = boot(X,statistic =SR_fct ,R=B) #3 param : sample originel, statistic i want bootstrap, number of bootstrap sampe
#statistic always be a function

#so we will compute a function that do the sharpe ratio -->see above

Boot$t #to print the value of my bootstrap

alpha=0.05
q1 = quantile(Boot$t,p=1-alpha/2)
q1

q2 = quantile(Boot$t,p=alpha/2)
q2

CI = c(SR_hat-q1,SR_hat-q2)
CI




################################################################################
###########################      QUESTION 2        #############################
################################################################################
#l'exercice ne fait pas sens, parce que ici les datas seront indépendantes du
#au fait que ce soit random

#Q2.1. 
k = 10
l=T/k

"on utilise le même B et la même fct de sharpe ratio" 
BlockBoot=tsboot(X,statistic = SR_fct,R=B,l=l,sim="fixed")
#X is the original sample from which you want to do the bootstraping 
#statistic is always a function
#R=B --> number of times you want to do the bootstraping
#l length of the block
#type of simulation (sim)
BlockBoot$t #to see the result


Block_q1 = quantile(BlockBoot$t,p=1-alpha/2)
Block_q1

Block_q2 = quantile(BlockBoot$t,p=alpha/2)
Block_q2

CI = c(2*SR_hat-Block_q1,2*SR_hat-Block_q2)
CI


#When computing the confidence intervals, we note that the Block Bootstrap
#intervals tend to be wider than the traditional bootstrap (to see this, 
#repeat the whole procedure several times and you will see). 
#This means that the block bootstrap is usually not as accurate as the traditional
#bootstrap. However, block-bootstrap allows us to deal with a degree of
#dependency between observations not possible with the traditional bootstrap.


