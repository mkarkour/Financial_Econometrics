################################################################################
###########################      Question 1      ###############################
################################################################################
"We do for the credit suisse but it's the same principle with the other stocks"

#Q1.1 DAILY
cr_suisse = DailyData$CREDIT.SUISSE.ASST.MAN.
end = length(cr_suisse)
Pt = cr_suisse[1:(end-1)] #équivalent à Pt
Pt1 = cr_suisse[2:end]    #équivalent à Pt+1 (on a ajouter une période à t)
#pt+1 ---> il s'agit de pt qui a avancé d'une période, donc on ne prend pas la première 
#période et on prend une période en plus à la fin
RA_cs=(Pt1-Pt)/Pt
RL_cs=log(Pt1/Pt)

print(RA_cs)
print(RL_cs)

#Q1.2 WHOLE PERIOD
P1=cr_suisse[1]
P1
Pn=cr_suisse[end]
Pn
RA_whole_period=(Pn-P1)/P1
RA_whole_period

sum_RA=sum(RA_cs)
sum_RA
#Difference between the sum of the daily return and the daily return

RL_whole_period=log(Pn/P1)
RL_whole_period
sum_RL=sum(RL_cs)
sum_RL
#Same number for the sum and the daily return with the log


################################################################################
###########################      Question 2      ###############################
################################################################################

yt=RL_cs[3:end]
yt1=RL_cs[2:(end-1)]
yt2=RL_cs[1:(end-2)]
#beta1 is the intercept
#beta2 is the coefficient yt1 estimate
#beta3 is the coefficient yt2 estimate
ols = lm(yt~yt1+yt2)
summary(ols)
#the two variable are significant and there are both negative coefficient


################################################################################
###########################      Question 3      ###############################
################################################################################
#Q3.1
moments = function(x){
  m=mean(x)
  s=sd(x)
  var=s^2
  ske=skewness(x)
  kur=kurtosis(x)
  result=c(m,var,ske,kur)
  print(result)
}
moments(RL_cs)

mean_cs=mean(RL_cs)
mean_cs
#normally we look at the std because it have the same unit of level in comparaison with the varibale itself
#so we don't really use the variance

sd_cs=sd(RL_cs)
sd_cs # a bit high compare of the average return

library(timeDate)
skw=skewness(RL_cs)
skw #negative skewness so we have a high probability of getting negative returns
#high chance of negative return compare to the positif return

kur_cs=kurtosis(RL_cs) #we compute the excess kurtosis here
kur_cs #much fatter tail compare to the gaussian distribution
#much higher than the kurtosis of 3 for the gaussian distribution

#Q3.2
Pt_data=DailyData[1:(end-1),2:6] #il s'agit d'une matrix
#compute the return of the five stocks (together)
#on travaille avec 5 colonne en même temps
Pt1_data=DailyData[2:end,2:6]
RL_data=log(Pt1_data/Pt_data)
RL_data

#to compute the covariance of the matrix we simply do
cov(RL_data)
#j'obtiens la covariance entre les différents stocks
#the cov is positif which means there is a positif link between the return of 
#five stock

#Q3.3
#returns of stocks are normally distributed (important theory in finance)
#idea here : fix a gaussian distribution by simuling a new sample
#fonction qui donne des valeurs randoms de la gaussian distribution
#on donne les paramètres en fct de quoi elle va devoir varier
simulated_RL_cs = rnorm(5000,mean=mean_cs,sd=sd_cs)
simulated_RL_cs
#donne comme résultat la distribution de cr_suisse si la distribution suivait
#une loi de distribution de gauss

mean(simulated_RL_cs)
mean_cs
#the fact that there is a difference between the two depends on the number of 
#observation that i take into account

sd(simulated_RL_cs)
sd_cs
#very close between both

skewness(simulated_RL_cs)
skw
#we see that we have a huge difference

kurtosis(simulated_RL_cs)
#en prenant un grand nombre d'observation, on aura un nbre proche de 0
kur_cs

#pay attention to all the theory that we use in finance



################################################################################
###########################      Question 4      ###############################
################################################################################
#stability of the distribution of the return
#we assume that the model is stable over time (return)
#the goal is to check if there is the case or not, we take the period of analysis
#divide period in several period and compute the moments of return for the different period
#if the stability of return is true (something that we can see over the time)



# you will see that the moments change with the time.
# This means that the parameters of the underlying distribution are not
# constant during time. As of consequence, the hypothesis of stability of
# the distribution made by many pricing models (as CAPM or APT) are not verified.



################################################################################
###########################      Question 5      ###############################
################################################################################
library(timeDate)
MyFunction2 <- function(data){
  m=apply(data,2,mean)
  std=apply(data,2,sd)
  var=std^2
  skw=apply(data,2,skewness)
  kur=apply(data,2,kurtosis)
  results=cbind(m,var,skw,kur)
  print(results)
}

MyFunction2(RL_data)
