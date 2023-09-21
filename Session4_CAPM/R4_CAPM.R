#Note about the data : 
#SP100 constituants : 95 stocks which constitutes the SP100 (cleaning in data so we don't have 100 stocks)
#SP100 index : weekly prices of the two index (100 et 500)
#TBill : return on the 3-mont tbill (return), there are in %

#!!! return is expressed as annual return not daily
#we should convert them into weekly return (divide the return by 52)


################################################################################
#------------------      Time series approach      ----------------------------#
################################################################################

################################################################################
###########################      Question 1/2      #############################
################################################################################
#Q1
#convert prices into return
SP100Constituents_prices = SP100Constituents[,2:96] #je retire la colonne week
SP100Index_prices = SP100Index[,2:3] #je retire la colonne week
n = length(SP100Constituents_prices[,2]) #donne le nombre de stock différents, ici 523
n

#calculate the return
RA_SP100Const = (SP100Constituents_prices[2:n,])/(SP100Constituents_prices[1:(n-1),])-1 #(ri)
head(RA_SP100Const)

#return on the market portfolio 
RA_SP500 = (SP100Index_prices[2:n,2]/SP100Index_prices[1:n-1,2])-1 #(rm)
head(RA_SP500)

#compute the z variable
length(RA_SP500) #we loose one obersvation but it's normal
TBILL_RA = TBill[2:n,2]/5200 #5200 parce que on a 52 semaines dans une année, et on a un % donc 52*100
TBILL_RA #correspond au risk free rate (rf)

x=RA_SP500-TBILL_RA #correspond à Zm,t
y=RA_SP100Const-TBILL_RA #correspond à Zi,t
head(y)


m <- summary(lm(y[,1]~x))
m$coefficients # i have the estimation of the intercept, std value, etc..
#p value of the intercept (most important information)
#give information of the significant information of the statistics, 
#the p value is very high, equal to ZERO point 8 so the validity of the capm is not rejected

#j'ai 521 observations pour chaque stocks
#stock 1 : AES, stock 2 : ALCOA, etc
#et on va faire la régression pour tout les stocks

#to each value 
out = data.frame(NULL) #empty data set, stock a lot of variables in this object
for (i in 1:length(y)){
  m <- summary(lm(y[,i]~x))
  out[i,1]=names(y)[i]
  out[i,2]=m$coefficients[1,1] #value of the intercepts
  out[i,3]=m$coefficients[1,4] #P value of the intercepts
  out[i,4]=m$coefficients[2,1] #estimation of the beta
  out[i,5] = m$coefficients[2,4] # p value of the beta
}
head(out) #information for all the stocks
#1 colonne : name of the stock
#2 colonne : value of the intercepts
#3 colonne : p value of the intercepts
#4 colonne : value of the beta
#5 colonne : p value of the beta
out
#p-value of the intercept is very low so the capm is rejected for this stock


#Q2
#autre méthode pour voir si la p-value de l'intercept est inférieur à 0.05, 
#chaque fois que ma valeur est inférieur à 0.05, le capm ne sera pas valide
testalpha = NULL
for (i in 1:length(y)){
  m = summary(lm(y[,i]~x))
  out[i,3] = m$coefficients[1,4]
  if (out[i,3]<0.05){
    testalpha[i]=0
  } 
  else {
    testalpha[i]=1
  }
}
testalpha #chaque fois que la valeur de p-value est sup à 0.05, on aura 1 dans notre testalpha, et à chaque fois que 
#cela sera inférieur on aura un 0

nb_valide = sum(testalpha)
nb_valide #le modèle est valide 89 fois

nb_reject = -sum(testalpha-1)
nb_reject #le modèle est rejetté 6 fois

#Remarks:
#the CAPM doesn't include any temporal evolution of the model (only one
#period). Are the betas really constant through time?
#We assumed that the SP500 is representative of the market return


################################################################################
#------------------      Cross sectional approach      ------------------------#
################################################################################
#we are going to work with the portfolio


################################################################################
###########################      Question 3/4      #############################
################################################################################
#we are going to divide the period into several different period (3 period)

#Q1 - estimate the beta for each stock
nn = c(210,419,522) 
#premiere periode : prend en compte 210 observation
#Seconde periode : prend en compte de 210 à 419

#first step : same as previous
betas = NULL

for (i in 1:length(y)){
  m <- summary(lm(y[1:nn[1],i]~x[1:nn[1]]))
  betas[i] = m$coefficients[2,1]
}
head(betas)

#Q2 - split into 10 portfolio
index = sort(betas,index.return=TRUE)
index

P1=index$ix[1:10]
P2=index$ix[11:20]
P3=index$ix[21:30]
P4=index$ix[31:40]
P5=index$ix[41:50]
P6=index$ix[51:60]
P7=index$ix[61:70]
P8=index$ix[71:80]
P9=index$ix[81:90]
P10=index$ix[91:95]
#comme on a 95 actions, le dernier portfolio contiendra uniquement 5 actions (pas graves)

#Q3 - real fama macbeth steps
betas1 = NULL
for (i in 1:95){
  m <- summary(lm(y[(nn[1]+1):nn[2],i]~x[(nn[1]+1):nn[2]]))
  betas1[i] = m$coefficients[2,1]
}

head(betas1)

#il s'agit du beta des stocks, on va mtn faire celui des portfolios

#we calculate the betas and the returns of our portfolios
#you can do this because the beta of the portfolio equals the average of
#the betas for the individual stocks when you have equally-weighted portfolios!
#(if portfolios are not equally weighted, the mean won't work! In that case 
#you will have to adjust by portfolio weights, but the principle is the same)
beta_P1 = mean(betas1[P1])
beta_P1 #for the first portfolio this is the beta

beta_P2 = mean(betas1[P2])
beta_P3=mean(betas1[P3])
beta_P4=mean(betas1[P4])
beta_P5=mean(betas1[P5])
beta_P6=mean(betas1[P6])
beta_P7=mean(betas1[P7])
beta_P8=mean(betas1[P8])
beta_P9=mean(betas1[P9])
beta_P10=mean(betas1[P10])

returns_P1 = apply(y[(nn[2]+1):nn[3],P1],1,mean) #le second arg est tjrs 1 ou 2, 1 pour les lignes, 2 pour les colonnes
returns_P2 = apply(y[(nn[2]+1):nn[3],P2],1,mean)
returns_P3=apply(y[(nn[2]+1):nn[3],P3],1,mean)
returns_P4=apply(y[(nn[2]+1):nn[3],P4],1,mean)
returns_P5=apply(y[(nn[2]+1):nn[3],P5],1,mean)
returns_P6=apply(y[(nn[2]+1):nn[3],P6],1,mean)
returns_P7=apply(y[(nn[2]+1):nn[3],P7],1,mean)
returns_P8=apply(y[(nn[2]+1):nn[3],P8],1,mean)
returns_P9=apply(y[(nn[2]+1):nn[3],P9],1,mean)
returns_P10=apply(y[(nn[2]+1):nn[3],P10],1,mean)


y2=matrix(c(returns_P1, returns_P2, returns_P3, returns_P4, returns_P5, returns_P6, returns_P7, returns_P8, returns_P9, returns_P10),nrow=103,ncol=10)
#ncol equal ten because we have ten portfolio
#independant variable
x2=c(beta_P1, beta_P2, beta_P3, beta_P4, beta_P5, beta_P6, beta_P7, beta_P8, beta_P9, beta_P10) #dependant variable

psi_vector=data.frame(NULL)

T=nn[3]-nn[2]
for (i in 1:103){
  m = summary(lm(y2[i,]~x2))
  psi_vector[i,1] = m$coefficients[1,1]  #intercept term equivalent to alpha
  psi_vector[i,2] = m$coefficients[1,4]  # p-value of the intercept term
  psi_vector[i,3] = m$coefficients[2,1]  # coefficient
  psi_vector[i,4] = m$coefficients[2,4]  # p-value of the coefficient
}

names(psi_vector) <- c("Psi_0","PValuePsi_0", "RiskPremium", "PValueRiskPremium")
psi_vector
#4 colonnes et 103 lignes qui correspondent à mon observation

#mtn faire une estimation de psi

avg_psi_0 = mean(psi_vector[,1])
avg_psi_0

avg_psi_1 = mean(psi_vector[,3])
avg_psi_1 #very small and negatif

#test the validity
#see if the psi0 = 0 and psi1 > 0
#To do it, we need to compute an confidence interval

t_alpha = qt(0.975,T-1) #1-alpha/2, second argument = number of observation -1
#to have the quantile, the alpha in the statisticals formula

ci_psi_0 = c(avg_psi_0-t_alpha*sd(psi_vector[,1])/sqrt(T),avg_psi_0+t_alpha*sd(psi_vector[,1])/sqrt(T)) #confidence intervall of an average
#(lower_bond, upper bond)
ci_psi_0
#---> interpret : value 0 include or not in the interval, we have the 0 include in the interval
#it's the case so the capm is valid


ci_psi_1 = c(avg_psi_1-t_alpha*sd(psi_vector[,3])/sqrt(T),avg_psi_1+t_alpha*sd(psi_vector[,3])/sqrt(T))
ci_psi_1#to confirm this we need an interval that compute only positif value
#goes from -0.009390132 to 0.009312855 so we have negative value and not only positif value
#--> the capm is not valid here


#Comments:
#Note that the excess return of the market portfolio is on average
# negative between 23/11/00 and 14/11/02.
#
# If you look closer at the confidence intervals, you can see that they are
# very big. This is mainly due to the fact that we are computing a
# regression with only 10 points. As an additional exercise, you can redo
# the FMB test by using this time 30 portfolios of 3 stocks each. Similarly,
# you will see that the results are not bad for the CAPM by using the SP500
# as market portfolio. you can redo the tests with the SP100.


