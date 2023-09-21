###################################################################################
###-------------------------------- EXERCICE 1 ---------------------------------###
###################################################################################
#Q1.Perform N = 10'000 times the following experiment:
#a) Simulate a series of t = 100 innovations
t=100
n=10000
epsilon = replicate(n,rnorm(t,0,1)) #rnorm pour random norm de mean 0 et sd 1
epsilon_m = matrix(rnorm(t*n,0,1),nrow=t,ncol=n)
epsilon_m

#b) For each sample, compute the cumulated values The series is non-stationary now.
Y=apply(epsilon_m,2,cumsum) #Each column is the value of the Y_(t) variable

#c) Estimate the regression of the Dickey-Fuller test
end=length(Y[,1])
delta_Y=Y[2:end,]-Y[1:(end-1),]#compute the value of Y_(t) - Y_(t-1)
delta_Y

#d) Collect the t-statistic of beta."
t_stat = matrix(0,nrow=1,ncol=n)

for (i in 1:n){
  m=lm(delta_Y[,i]~Y[1:(end-1),i])
  s=summary(m)
  t_stat[i]=s$coefficients[2,3] #j'enregistre le beta coefficient
}
t_stat

hist(t_stat)

#-------------------------------------------------------------#
#Q2.Display the histogram of the N t-statistics. You may have 
#expected a distribution symmetric around 0, as the data have 
#been simulated under the null-hypothesis beta = 0
#-------------------------------------------------------------#

#Histogram of t-stats
hist(t_stats,20, main='Figure 1 - Histogram of the t-statistics')

#Histogram of a student t distribution for comparison
R=rt(10000,98)
hist(R,20, main='Figure 2 - Histogram of the Student distribution')

#----------------------------------------------------------------#
#Q3.Compute the critical values for the Dickey-Fuller test at the 
#1%, 5% and 10% levels. They correspond to the quantiles of 
#the t-statistics distribution.
#----------------------------------------------------------------#

"pour avoir la critical value on doit extraire les quantiles de cette distribution"

df_onepct =quantile(t_stat,0.01) #dickey fuller
df_onepct
df_fivepct =quantile(t_stat,0.05)
df_fivepct
df_tenpct=quantile(t_stats,0.10)
df_tenpct

#Quantiles for student t distribution with 100-2=98 degress of freedom
student_onepct=qt(.01,98)
student_fivepct=qt(.05,98)
student_tenpct=qt(.1,98)
student_onepct
student_fivepct
student_tenpct

"(graphe)comparé à la distribution studdent, dickey-fuller est plus collé à gauche

en comparant par rapport à la student distribution on voit qu'il y a une valeur 
significativement différente"

###################################################################################
###-------------------------------- EXERCICE 2 ---------------------------------###
###################################################################################
#Test the stationarity of the series of monthly prices provided in the file data_monthly.csv.

y = data_monthly[,1]

#augmented dickey-fuller test
library(tseries)
ADF = adf.test(y)
ADF
"grande p-value, je ne rejette pas l'H0 (le fait que ce ne soit pas stationnaire)
donc ma série n'est pas stationnaire"

#kpss test
KPSS = kpss.test(y)
KPSS
"la p-value est faible, ce qui veux dire que je rejette l'H0 ici
H0 pour kpss est que la série est stationnaire, ce qui veut dire qu'elle n'est pas 
stationnaire comme je rejette l'H0"