# Get data from excel. You need the databases below: 
# 1. SP100Constituents 
# 2. SP100Index 
# 3. TBill 

###################################################################################
#Time-Series Approach
###################################################################################
print('=====Problem 1: Time-Series Approach=====\n')
###################################################################################

SP100ConstituentsPrices =  SP100Constituents[,2:96]
SP100IndexPrices = SP100Index[,2:3]

nb_titres=length(SP100ConstituentsPrices[,2])
nb_titres
  
#Calculation of the arithmetic returns 
RA_SP100Constituents=(SP100ConstituentsPrices[2:nb_titres,]/SP100ConstituentsPrices[1:nb_titres-1,])-1
RA_SP500=(SP100IndexPrices[2:nb_titres,2]/SP100IndexPrices[1:nb_titres-1,2])-1
  
nb_ret=length(RA_SP100Constituents[,1])
  
#the TBill returns are given annualized
#to get return per week we divide by 100 and by 52 because there are 52 weeks in a year
TBill3MonthsRate=TBill[2:nb_titres,2]/5200


x=RA_SP500-TBill3MonthsRate
y=RA_SP100Constituents-kronecker(matrix(1,nb_titres,95),TBill3MonthsRate)
#with kron you can create a matrix of tbill returns that you can subtract to the
#s&p100 stock returns in order to get the excess return of each stock


out <- data.frame(NULL)              # create object to keep results
for (i in 1:50) {
  m <- summary(lm(y[,i] ~ x))    # run model
  out[i, 1] <- names(y)[i]           # print variable name
  out[i, 2] <- m$coefficients[1,1]   # intercept
  out[i, 3] <- m$coefficients[1,4]   # p-value of the intercept term
  out[i, 4] <- m$coefficients[2,1]   # coefficient
  out[i, 5] <- m$coefficients[2,4]   # p-value of the coefficient
}
names(out) <- c("y.variable", "Alphas","PValuesAlphas", "Betas", "PValuesBetas")
head(out)

#We test the CAPM stock by stock.
#Multivariate tests allow you to test the CAPM on all stocks at once.

#For this first test, it is as well crucial to suppose that the underlying
# distribution doesn't vary during time ( returns iid), and that we use a
#good proxy for the market portfolio.


#Initialize the variables (makes the execution of the program more efficient)

testsalphas=NULL
for (i in 1:length(y)) {
  m <- summary(lm(y[,i] ~ x))    # run model
  out[i, 3] <- m$coefficients[1,4]   # p-value of the intercept term
  if (out[i,3] < 0.05){
   testsalphas[i]=0}
  else{
    testsalphas[i]=1}
}

#you can then for example look at the number of times in which the CAPM is
#accepted/rejected :
nb_valides=sum(testsalphas)
nb_rejets=-sum(testsalphas-1)
nb_valides
nb_rejets

#Remarks:
#the CAPM doesn't include any temporal evolution of the model (only one
#period). Are the betas really constant through time?
#We assumed that the SP500 is representative of the market return



###################################################################################
  #Fama MacBeth Procedure
###################################################################################
  print('=====Problem 2: Fama MacBeth Procedure=====')
###################################################################################
  #1.

NN=c(210, 419, 521)

betas1=NULL
for (i in 1:95){
  b=summary(lm(y[1:NN[1],i]~x[1:NN[1]]))
  betas1[i]=b$coefficients[2,1]
}
head(betas1)

###################################################################################
  #2.

index=sort(betas1, index.return=TRUE)

#we manually save the indices of our stocks in 10 vectors
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

###################################################################################
#3.

betas2=NULL
for (i in 1:95){
b=summary(lm(y[(NN[1]+1):NN[2],i]~x[(NN[1]+1):NN[2]]))
betas2[i]=b$coefficients[2,1]
}
head(betas2)

#we calculate the betas and the returns of our portfolios
#you can do this because the beta of the portfolio equals the average of
#the betas for the individual stocks when you have equally-weighted portfolios!
#(if portfolios are not equally weighted, the mean won't work! In that case 
#you will have to adjust by portfolio weights, but the principle is the same)
     betaP1=mean(betas2[P1])
     betaP2=mean(betas2[P2])
     betaP3=mean(betas2[P3])
     betaP4=mean(betas2[P4])
     betaP5=mean(betas2[P5])
     betaP6=mean(betas2[P6])
     betaP7=mean(betas2[P7])
     betaP8=mean(betas2[P8])
     betaP9=mean(betas2[P9])
     betaP10=mean(betas2[P10])
     
###################################################################################
#4.
     
     # the apply(x,1,mean) command calculates the mean across rows, which in our case
     # are the cross-section returns for each portfolio that we are looking for, that is, the z_i,t
         

     returnsP1=apply(y[(NN[2]+1):NN[3],P1],1,mean)
     returnsP2=apply(y[(NN[2]+1):NN[3],P2],1,mean)
     returnsP3=apply(y[(NN[2]+1):NN[3],P3],1,mean)
     returnsP4=apply(y[(NN[2]+1):NN[3],P4],1,mean)
     returnsP5=apply(y[(NN[2]+1):NN[3],P5],1,mean)
     returnsP6=apply(y[(NN[2]+1):NN[3],P6],1,mean)
     returnsP7=apply(y[(NN[2]+1):NN[3],P7],1,mean)
     returnsP8=apply(y[(NN[2]+1):NN[3],P8],1,mean)
     returnsP9=apply(y[(NN[2]+1):NN[3],P9],1,mean)
     returnsP10=apply(y[(NN[2]+1):NN[3],P10],1,mean)
     
y2=matrix(c(returnsP1, returnsP2, returnsP3, returnsP4, returnsP5, returnsP6, returnsP7, returnsP8, returnsP9, returnsP10),nrow=102,ncol=10)
x2=c(betaP1, betaP2, betaP3, betaP4, betaP5, betaP6, betaP7, betaP8, betaP9, betaP10)

FamaMacBeth=lm(y2[1,]~x2)

print('First test, on the first day of the following period:')

summary(FamaMacBeth)

#Second test, we take the mean on the sample
print('Second test, we take the mean on the rest of the sample:')

T=NN[3]-NN[2]
average_psi=NULL
psi_vector=data.frame(NULL)

for (i in 1:T){
b=summary(lm(y2[i,]~x2))
                 psi_vector[i, 1] = b$coefficients[1,1]   # intercept
                 psi_vector[i, 2] = b$coefficients[1,4]   # p-value of the intercept term
                 psi_vector[i, 3] = b$coefficients[2,1]   # coefficient
                 psi_vector[i, 4] = b$coefficients[2,4]   # p-value of the coefficient
}
names(psi_vector) <- c("Psi_0","PValuePsi_0", "RiskPremium", "PValueRiskPremium")
head(psi_vector)

average_psi0=mean(psi_vector[,1])
average_psi1=mean(psi_vector[,3])
t_alpha= qt(0.975,T-1)
CI_psi0=c(average_psi0-t_alpha*sd(psi_vector[,1])/sqrt(T), average_psi0+t_alpha*sd(psi_vector[,1])/sqrt(T))
CI_psi1=c(average_psi1-t_alpha*sd(psi_vector[,3])/sqrt(T), average_psi1+t_alpha*sd(psi_vector[,3])/sqrt(T))

if (CI_psi0[1]*CI_psi0[2]<0){
  print("psi_0 equal to zero")} else{print("psi_0 different from zero")}

if ((CI_psi1[1]*CI_psi1[2])>0 & (CI_psi1[1])>0){
  print("risk premium positive")} else{print("risk premium non-significative or negative")}
  
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
