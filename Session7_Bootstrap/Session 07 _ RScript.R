######################################################################
# Bootstrap Confidence Interval
######################################################################

######################################################################
# 1.

T=100

mu=1
sigma=4
SR=mu/sigma

X=rnorm(T, mu, sigma)



######################################################################
# 2.

mu_hat=mean(X)
sigma_hat=sd(X)

SR_hat=mu_hat/sigma_hat

######################################################################
# 3.

B=2000

X_star=X
SR_star1=mean(X_star)/sd(X_star)
SR_star=function(data,i){ 
  d=data[i]
  u=mean(d)
  sigma=sd(d)
  SR=u/sigma
  return(SR)
}

#Install package boot
library(boot)

Boot=boot(X_star, statistic=SR_star, R=B)

#Boot$t0 is equivalent to mean(X_star)/sd(X_star)
#bias=mean(Boot$t)-Boot$t0
#std.error=sd(Boot$t)


alpha = 0.05

q1 = quantile(Boot$t, p=1-alpha/2)
q2 = quantile(Boot$t, p=alpha/2)


bootstrap=c(2*SR_hat-q1,2*SR_hat-q2)
bootstrap

# In principle, it is better to use the bootstrap
# method when the number of observations is limited.



######################################################################
# The Block Bootstrap
######################################################################

# First,choose block size
k=10 #number of blocks
s=T/k #length of the block
#Now, choose number of random samples of size T=100 from the original data
B1=2000 #set originally equal to same B of previous exercise

#Compute the block bootstrap using the function tsboot from the package boot

BlockBoot=tsboot(X_star, statistic=SR_star, R=B1, l=s, sim="fixed")


#Using an approach similar to the previous exercise, we estimate the
#confidence intervals

alpha = 0.05

q1 = quantile(BlockBoot$t, p=1-alpha/2)
q2 = quantile(BlockBoot$t, p=alpha/2)


BlockBootstrap=c(2*SR_hat-q1,2*SR_hat-q2)
BlockBootstrap


#When computing the confidence intervals, we note that the Block Bootstrap
#intervals tend to be wider than the traditional bootstrap (to see this, 
#repeat the whole procedure several times and you will see). 
#This means that the block bootstrap is usually not as accurate as the traditional
#bootstrap. However, block-bootstrap allows us to deal with a degree of
#dependency between observations not possible with the traditional bootstrap.
