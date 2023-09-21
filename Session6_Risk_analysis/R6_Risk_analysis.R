#StocksWeeklyReturns
#contient le return de 10 stocks

################################################################################
###########################      EXERCISE 1        #############################
################################################################################

#Q1. For one series of your choice, compute the empirical quantile 
#y(q) with q equal to 1% and 5%.

#data[,2] --> simply the first stock in my portfolio (in my data)

quantile(data[,2],probs = 0.01)
"je peux être sûr à 99% que le return (weekly) de mon stocks n'ira pas en 
  dessous de 14% pour un horizon d'une semaine"
  "la proba d'aller en dessous de cette valeur (-0.1405338) est de 1%"

quantile(data[,2],probs = 0.05)
"On peut être sur à 95% que le weekly return de mon stock n'ira pas en dessous de
  9.3%"


#Q2. Form an equally weighted portfolio with the 10 stocks and 
#compute the first two empirical moments (mean and var-cov matrix) of 
#the multivariate distribution of returns. Also, look at the skewness and 
#kurtosis of the portfolio returns.

#each asset will have a weight equal to 0.1 parce que j'ai 10 value in my portfolio
#and I want that they have the same weight
a = c(0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1,0.1)

#je prends les colonnes ou j'ai les stocks et je l'ai multiplie par le poids dans
#le vecteur a


data1=data[,2:11]

P = -data1*cbind(a) #on met moins devant le portfolio pour être sur de parler en terme de return positif
#cbind() function in R Language is used to combine specified Vector, Matrix or Data Frame by columns.
P #juste les values des stocks multiplié par 0.1

P2 = apply(P,1,sum)
P2 #donne le return du portfolio

"pour vérifier qu'il s'agit bien de la somme de chaque ligne de mon portfolio,
  j'obtient donc pour la première ligne première colonne un return correspondant 
  à la semaine 1 de mon portfolio"
verif =0.0043800123+-0.0085714286+-0.0020814480+-0.0030598324+-0.0019957737+-0.0033304119+-0.0075339806+-0.0048666667+-0.0000717360+0.0012703454
verif

#We transform the result to a vector (plus simple à lire)
Z = cbind(P2)
Z

mu = mean(Z)
mu

var=var(Z)
var

sd = sd(Z)
sd

print("we have a portfolio with a average return of -0.001644578")

library(timeDate)
ske = skewness(Z)
ske
"Skewness de -0.06275737, on a une grande probabilité d'avoir un return négatif par rapport 
à un return positif"

kur = kurtosis(Z) #je vais avoir le kurtosis en excess
kur
kur_1 = kurtosis(Z)+3 #je vais avoir le bon kurtosis
kur_1
"Kurtosis de 4.130422, kurtosis plus grande que 3"

"Basé sur la kurtosis et la skewness, on ne peux pas conclure que le portfolio
suit une distribution normale(de gauss)"


#Q3. Estimate the VaR under the Gaussian hypothesis at the 5% level

a #--> vector of weights
Z_0.95 = qnorm(0.95,0,1) #avg =0 and var =1 ALWAYS THE CASE
Z_0.95

GaussianVaR = mu+sd*Z_0.95
GaussianVaR

"le résultat nous dit que l'on peut être sûr à 95% (confidence level) 
  que le maximum loss du portfolio est égale à 0.05606582, dans 5% des cas, on
  est au dessus des 0.06%"


#Q4. Estimate the VaR for the general case (no parametric assumption but Gaussian kernel) at the same level 

T=length(Z)
T

h=sd(Z)*T^(-1/5)
h

alpha=0.05
NonParamVaR=NULL #on doit tjrs créer une empty variable avant d'utiliser une loop en R

l=1000           #always a very large value
max(Z)*5200 #weekly return (52 week*100(%)) PAS SUR DU TOUT

for (i in 1:T){
  VaR=Z[i]
  x=NULL
  Minobj=NULL
  for (j in 1:T) {
    x[j]=pnorm((Z[j]-VaR)/h)} 
  Minobj=(sum(x)/T-alpha)^2 
  if (Minobj<l) {
    l=Minobj
    NonParamVaR=VaR}
}
print(NonParamVaR)

"0.05760325(kernel method)
 0.05760325(gaussian method)
  en comparant les deux, on voit que la gaussian a under estimate la VaR

  We see that the Gaussian Hypothesis seems to underestimate the true VAR!!! 
  That is a major reason why it is so problematic to assume Gaussian returns!"


#Q5 The Gaussian distribution will over or under estimate the VaR compare to the kernel method ?

"la value at risk will be under estimated under the gaussian distribution compare to the kernel method
  because the var is in the tail in the distribution and the kurtosis is about the tail of the distribution.
  
  si on a un kur > 3 cela veut dire que l'on a des fatters tails comparé à la gaussian distribution
  si on utilise la gaussian method, la var sera donc under estimated"

#QBONUS : Compute the Expected shortfall
GaussianES = mu+sd*dnorm(Z_0.95,0,1)/alpha 
GaussianES
"the avg loss of our portfolio is equal to 0.07072659%"

NonParamES =mean(Z*pnorm((Z-NonParamVaR)/h))/alpha
NonParamES
"ici la gaussian ES est > à la kernel ES
  mais la différence n'est pas si large ici (très faible comme différence)"


################################################################################
###########################      EXERCISE 2        #############################
################################################################################
#firedat --> other dataset provient d'une compagnie d'assurance

alpha=0.01

#Q1. Gaussian method
#VaR

fireloss =FIREDAT[,2]
fireloss

mf = mean(fireloss)
mf

sdf=sd(fireloss)
sdf

Z_0.99 = qnorm(0.99,0,1)
Z_0.99

GaussianVaR = mf+sdf*Z_0.99
GaussianVaR

#ES
GaussianES=mf+sdf*dnorm(Z_0.99,0,1)/alpha
GaussianES

#Q2. Kernel method

VaR=GaussianVaR
h=sd(fireloss)*length(fireloss)^(-.2)

T=length(fireloss)
alpha=0.01
NonParamVaR=NULL
l=100^10
test = max(FIREDAT[,2])


for (i in 1:T){
  VaR=fireloss[i]
  x=NULL
  Minobj=NULL
  for (j in 1:T) {
    x[j]=pnorm((fireloss[j]-VaR)/h)
    Minobj=(sum(x)/T-alpha)^2}
  if (Minobj<l) {
    l=Minobj
    NonParamVaR=VaR}
}

print(NonParamVaR)


#ES
NonParamES=mean(fireloss*pnorm((fireloss-NonParamVaR)/h))/alpha
NonParamES

















