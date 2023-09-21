################################################################################
###########################      Question 1      ###############################
################################################################################

#Q1.1. Daily arithmetic returns for each stock
#price is not a random variable, normally better to work with the log return (because it's a time series)
cr_suisse = DailyData$CREDIT.SUISSE.ASST.MAN.
msft = DailyData$MICROSOFT
cc = DailyData$COCA.COLA
nike = DailyData$NIKE
boeing = DailyData$BOEING
data <- c(msft,cr_suisse,boeing,cc,nike)

#for(d in data){print(d)}

n_cs=length(cr_suisse)
p1_cs=cr_suisse[1:n_cs-1]
p2_cs=cr_suisse[2:n_cs]
RA_cs = (p2_cs-p1_cs)/p1_cs 
RA_cs

n_msft=length(msft)
p1_msft=msft[1:n_msft-1]
p2_msft=msft[2:n_msft]
RA_msft=(p2_msft-p1_msft)/p1_msft

n_boeing=length(boeing)
p1_boeing=boeing[1:n_boeing-1]
p2_boeing=boeing[2:n_boeing]
RA_boeing=(p2_boeing-p1_boeing)/p1_boeing

n_cc=length(cc)
p1_cc=cc[1:n_cc-1]
p2_cc=cc[2:n_cc]
RA_cc=(p2_cc-p1_cc)/p1_cc

n_nike=length(nike)
p1_nike=nike[1:n_nike-1]
p2_nike=nike[2:n_nike]
RA_nike=(p2_nike-p1_nike)/p1_nike

RA<-c(RA_msft,RA_cs,RA_boeing,RA_cc,RA_nike)
RA


#Q1.2. compute the fourth centered moments (mean,sd,skewness,kurtosis)
library(timeDate)
moments <- function(RA){
  avg<-mean(RA)
  sig<-sd(RA)
  ske<-skewness(RA)
  kur_exc<-kurtosis(RA)
  kur<-kurtosis(RA)+3
  result <- c(avg,sig,ske,kur_exc,kur)
  names(result)<-c("mean","standard deviation","skewness","kurtosis (excess!)","kurtosis")
  print(result)
}

moments(RA_cs)
#The kurtosis of cr suisse is higher than 3, so we cannot assume that the return of cr suisse
#is distributed according to the gaussian distribution
moments(RA_msft)


################################################################################
###########################      Question 2      ###############################
################################################################################

#Q2.1. use function density(), estimate the marginal density using the gaussian kernel
#density(x1,X2,X3) x1 : one which parameter we do the density, x2 which kernel, X3 bandwith  
sd<-sd(RA_cs)
nb_ret=length(RA_cs)
h=sd*nb_ret^(-1/5)
h

pdf_cs<-density(RA_cs,kernel="gaussian",bw=h) #when we finish we need to plot the density
plot(pdf_cs$x,pdf_cs$y,main="Gaussian kernel density of credit suisse",xlab="Values",ylab="Density",type='l',xlim=c(-0.18,0.18), ylim=c(0,55))
#doesn't look like a gaussian distribution but it represent the return distribution


#Q2.2 Play with the value of h
#the only choose that we change here is the value of the bandwith
under=h/10
pdf_cs_under<-density(RA_cs,kernel="gaussian",bw=under) 
plot(pdf_cs_under$x,pdf_cs_under$y,main="Gaussian kernel density of credit suisse - small value of h",xlab="Values",ylab="Density",type='l')
#we can see that the graphe is very volatile, it's the under smoothing of the bumb, it's not a good representation of the thumb

over=h*10
pdf_cs_over<-density(RA_cs,kernel="gaussian",bw=over) 
plot(pdf_cs_over$x,pdf_cs_over$y,main="Gaussian kernel density of credit suisse - big value",xlab="Values",ylab="Density",type='l')
#we have just one bumb, it's the oversmooth, it's not a reliable representation of the return


#Q2.3. Divide the sample into two parts and compute again the density
#on divise le vecteur en deux en prenant d'un coté les valeurs vieilles et de 
#l'autre les nouvelles valeurs.
RA_CS_OLD=RA_cs[1:(nb_ret/2)]
RA_CS_NEW=RA_cs[(nb_ret/2+1):nb_ret]

h_old=sd(RA_CS_OLD)*length(RA_CS_OLD)^(-1/5)
h_new=sd(RA_CS_NEW)*length(RA_CS_NEW)^(-1/5)

pdf_cs_old=density(RA_CS_OLD,kernel="gaussian",bw=h_old)
pdf_cs_new=density(RA_CS_NEW,kernel="gaussian",bw=h_new)

plot(pdf_cs_old$x,pdf_cs_old$y,main="Gaussian kernel density of credit suisse - OLD data",xlab="Values",ylab="Density",type='l')
plot(pdf_cs_new$x,pdf_cs_new$y,main="Gaussian kernel density of credit suisse - NEW data",xlab="Values",ylab="Density",type='l')

#conclusion: we don't have the same graph, it means that the distribution of return
# is not stable over time


################################################################################
###########################      Question 3      ###############################
################################################################################

#Q3.1. Simulate 1000 observations from a normal distribution - use rnorm()
simulated_data = rnorm(1000,mean=0,sd=1) #1000 is the number of observation
simulated_data #donne 1000 valeurs de façon aléatoire reprenant la distrib. normale
h=sd(simulated_data)*length(simulated_data)^(-1/5)
pdf_gaussian=density(simulated_data,kernel='gaussian',bw=h)
plot(pdf_gaussian$x,pdf_gaussian$y,main='kernel density - gaussian distribution',type='l')
#it's a good estimation because it's the same shape as a gaussian distribution

#the higher number of observation you take, the better your density would be

#Q3.3. 
simulated_data2 = rbeta(1000,1.5,4) #parameter given in the exercise, 1000 is the number of observation
h_beta=sd(simulated_data2)*1000^(-1/5)
pdf_beta=density(simulated_data2,kernel="gaussian",bw=h_beta)
plot(pdf_beta$x,pdf_beta$y,main="kernel density - beta distribution",type='l')
#we can still use the gaussian distribution to estimate the distribution



################################################################################
###########################      Question 4      ###############################
################################################################################
#check if the assumption of the gaussian distribution that we make on the stock return is valid or not (we know that is not valid)
#but it's a method to proove it

avg <-mean(RA_cs)
SD<-sd(RA_cs)

cs_gaussian<-dnorm(pdf_cs$x,mean=avg,sd=SD)
plot(cs_gaussian,main="credit suisse return distribution under gaussian assumption",type='l')




