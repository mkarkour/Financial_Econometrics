# Import the data 

####################################################################################
# Question 1
####################################################################################

####################################################################################
# 1.

# We choose to work on "CREDIT SUISSE"

CS=DailyData[,3]
end=length(CS)

# Computation of daily logarithmic returns:
RA_CS=(CS[2:end]-CS[1:(end-1)])/CS[1:(end-1)]


# Computation of daily logarithmic returns:
RL_CS=log(CS[2:end])-log(CS[1:(end-1)])

# R allows you to work directly on the whole data matrix:

# Arithmentic returns
RA_Daily=(DailyData[2:end,2:6]-DailyData[1:(end-1),2:6])/DailyData[1:(end-1),2:6]

# Logarithmic returns
RL_Daily=log(DailyData[2:end,2:6])-log(DailyData[1:(end-1),2:6])

####################################################################################
# 2. Multi-period returns:

print('------- Question 1.2 -------')

print('-------Arith. Return Whole Period -------')
whole_period_returns_1=(DailyData[end,2:6]-DailyData[1,2:6])/DailyData[1,2:6]

print('-------Sum of Daily Arith Returns-------')
whole_period_returns_2=apply(RA_Daily,2,sum)

print('-------Log Return Whole Period -------')
whole_period_returns_3=log(DailyData[end,2:6])-log(DailyData[1,2:6])

print('-------Sum of Daily Log Returns-------')
whole_period_returns_4=apply(RL_Daily,2,sum)



#  The idea is then the following :
# - when we work with arithmetic returns, we cannot extrapolate the results
#  on periods that are different from the ones we used to calculate the
#   returns.
# - with logarithmic returns we can do it.

# To convince you, simply write the formula for the logarithmic returns:
# the weekly return is equal to the sum of the daily returns.

# This property makes the logarithmic returns attractive,
# but you have to be careful because logarithmic returns don't have a real
# representation. In other words, if we work with logarithmic returns and
# we want to apply a simple model like the CAPM , we have to convert the
# results in arithmetic results if we want to compare them with the real returns
# listed on the market.


# The arithmetic returns are instead more covenient to calculate the returns of a portfolio.


####################################################################################
# Question 2
####################################################################################
print('------- QUESTION 2 -------')

# we work on Credit Suisse
# the goal of the exercise is to use the function lm()
# we estimate the regression y_t=b1+b2*y_(t-1)+b3*y_(t-2)
ols = lm(RA_CS[3:end] ~ RA_CS[2:(end-1)] + RA_CS[1:(end-2)])
summary(ols)

# for the details about the output of the function lm, look at the help
# of the function.


####################################################################################
# Question 3
####################################################################################

# The question 3 is corrected only for Credit Suisse. 

####################################################################################

print('------- QUESTION 3 -------')

####################################################################################
# 1.

print('------- Question 3.2 -------')
print('---Sample Centered Moments----')

mean_CS=mean(RA_CS)

sd_CS=sd(RA_CS)
var_CS=sd_CS^2
# it is the unbiased estimator (cf. cours)
# the variance is the squared of the standard deviation

library(timeDate)

skewness_CS=skewness(RA_CS)
kurtosis_CS=kurtosis(RA_CS)


####################################################################################
# 2.

print('------- Question 3.3 -------')
print('---Var/Covariance Matrix----')

COV_RA_Daily=cov(RA_Daily)


####################################################################################
# 4.

print('------- Question 3.4 -------')
print('-----Simulated Returns------')

# we simulate 5000 realizations of the gaussian law with the parameters
Simulated_RA_CS=rnorm(5000,mean=mean_CS,sd=sd_CS)

mean_Sim_RA_CS=mean(Simulated_RA_CS)
sd_Sim_RA_CS=sd(Simulated_RA_CS)
var_Sim_RA_CS=sd_Sim_RA_CS^2
skewness_Sim_RA_CS=skewness(Simulated_RA_CS)
kurtosis_Sim_RA_CS=kurtosis(Simulated_RA_CS)



# the third and fourth moments are different.
# This shows that the gaussian law is not ideal to model daily returns.

# Theoretically the skewness and kurtosis of a gaussian sample are
# respectively equal to 0 and 3. The sample skewness and kurtosis of the simulated sample are
# indeed of this order of measure.
# You can see that they are not perfectly equal to 0 and 3. This comes from
# the fact that our sample is finite and that the sample estimators only
# converge towards the true value. The bigger the sample, the more and
# more will be the estimator near to the true value of the underlying
# distribution.

####################################################################################
# Question 4
####################################################################################

# It is left to you to redo the calculations on the subsamples.

# you will see that the moments change with the time.
# This means that the parameters of the underlying distribution are not
# constant during time. As of consequence, the hypothesis of stability of
# the distribution made by many pricing models (as CAPM or APT) are not verified.


####################################################################################
# Question 5
####################################################################################

MyFunction2 <- function(data){
  m=apply(data,2,mean)
  std=apply(data,2,sd)
  var=std^2
  skw=apply(data,2,skewness)
  kur=apply(data,2,kurtosis)
  results=cbind(m,var,skw,kur)
  print(results)
}

# The function "MyFunction2" returns the mean, variance, skewness and kurtosis
# for a database/matrix. 