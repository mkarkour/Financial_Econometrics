# Get data from excel
#1. FFPortfolios
#2. SP100Index 
#3. TBill 


###################################################################################
# Fama French 3-factor model
###################################################################################
print('=======Problem 3: Fama French 3-factor model======\n')
###################################################################################
FamaFrench=FFPortfolios[,2:7]/100 #Devide by 100 to make it into actual returns
head(FamaFrench)

# Part 1. Compute SMB and HML returns
# we can use mean, because the portfolios are equally weighted
nn=length(FamaFrench[,1])
SMB=apply(FamaFrench[2:nn,1:3],1,mean)-apply(FamaFrench[2:nn,4:6],1,mean)
HML=apply(FamaFrench[2:nn,c(3,6)],1,mean)-apply(FamaFrench[2:nn,c(1,4)],1,mean)
SP100IndexPrices = SP100Index[,2:3]
RA_SP500=(SP100IndexPrices[2:nn,2]/SP100IndexPrices[1:nn-1,2])-1
TBill3MonthsRate=TBill[2:nn,2]/5200

r=length(SMB)
X3=matrix(c(RA_SP500-TBill3MonthsRate, SMB, HML), nrow=r, ncol=3) #Matrix of regressors
#y3=y1
y3=FamaFrench[2:nn,]-kronecker(matrix(1,1,6),TBill3MonthsRate)

#Part 2. Now do the regressions for all 6 portfolios with the two new factors, for the
#period until 15/8/2001, which corresponds to return number 457
outFF3=data.frame(NULL)

NN=457
for (i in 1:6){
  bFF3=summary(lm(y3[1:NN,i]~X3[1:NN,]))
  outFF3[i,1]=bFF3$coefficients[1,1]
  outFF3[i,2]=bFF3$coefficients[1,4]
  outFF3[i,3]=bFF3$coefficients[2,1] #new betas, not to be confused with earlier ones
  outFF3[i,4]=bFF3$coefficients[2,4]
  outFF3[i,5]=bFF3$coefficients[3,1] #new regression coefficients
  outFF3[i,6]=bFF3$coefficients[3,4]
  outFF3[i,7]=bFF3$coefficients[4,1]
  outFF3[i,8]=bFF3$coefficients[4,4]
  outFF3[i,9]=bFF3$r.squared
}

names(outFF3) <- c("Alpha","PValueAlpha","Beta","PValueBeta", "BetaSMB","PValueSMB", "BetaHML","PValueHML", "R-squared")
outFF3

###Now work on the naive time-series dimension tests of part 1 for the Fama
##French 3 factor model--This is done for the alphas and the regression
#coefficients for the three factors
testsalphasFF3=NULL
testsbetasFF3=NULL
testssFF3=NULL
testshFF3=NULL

for (i in 1:6){
  if (outFF3[i,2]<0.05){testsalphasFF3[i]=1} else{testsalphasFF3[i]=0}
  
  if (outFF3[i,4]<0.05){testsbetasFF3[i]=1} else{testsbetasFF3[i]=0}
  
  if (outFF3[i,6]<0.05){testssFF3[i]=1} else{testssFF3[i]=0}
  
  if (outFF3[i,8]<0.05){testshFF3[i]=1} else{testshFF3[i]=0}
}

print('Fama French Testing on First Subsample')
print('Number of alphas different from 0')
print(sum(testsalphasFF3))

print('Number of betas different from 0')
print(sum(testsbetasFF3))

print('Number of s different from 0')
print(sum(testssFF3))

print('Number of h different from 0')
print(sum(testshFF3))

#Now, use the remainder of the sample to see how the alphas and betas
#change over time and also to re-assess who is indistinguishable from
#zero or not in order to see how stable the model is over time
outFF3_2=data.frame(NULL)
TT=6
for (i in 1:TT){
  bFF3_2=summary(lm(y3[(NN+1):TT,i]~X3[(NN+1):TT,]))
  outFF3_2[i,1]=bFF3_2$coefficients[1,1]
  outFF3_2[i,2]=bFF3_2$coefficients[1,4]
  outFF3_2[i,3]=bFF3_2$coefficients[2,1] #new betas, not to be confused with earlier ones
  outFF3_2[i,4]=bFF3_2$coefficients[2,4]
  outFF3_2[i,5]=bFF3_2$coefficients[3,1] #new regression coefficient
  outFF3_2[i,6]=bFF3_2$coefficients[3,4]
  outFF3_2[i,7]=bFF3_2$coefficients[4,1] #new regression coefficient
  outFF3_2[i,8]=bFF3_2$coefficients[4,4]
  outFF3_2[i,9]=bFF3_2$r.squared         #for each regression, this variable returns the R squared, F statistic, p-value
}

names(outFF3_2) <- c("Alpha","PValueAlpha","Beta","PValueBeta", "BetaSMB","PValueSMB", "BetaHML","PValueHML", "R-squared")
outFF3_2

testsalphasFF3_2=NULL
testsbetasFF3_2=NULL
testssFF3_2=NULL
testshFF3_2=NULL
for (i in 1:6){
  if (outFF3_2[i,2]<0.05){testsalphasFF3_2[i]=1} else{testsalphasFF3_2[i]=0}
  
  if (outFF3_2[i,4]<0.05){testsbetasFF3_2[i]=1} else{testsbetasFF3_2[i]=0}
  
  if (outFF3_2[i,6]<0.05){testssFF3_2[i]=1} else{testssFF3_2[i]=0}
  
  if (outFF3_2[i,8]<0.05){testshFF3_2[i]=1} else{testshFF3_2[i]=0}
}

print('Fama French Testing on Second Subsample')
print('Number of alphas different from 0')
print(sum(testsalphasFF3_2))

print('Number of betas different from 0')
print(sum(testsbetasFF3_2))

print('Number of s different from 0')
print(sum(testssFF3_2))

print('Number of h different from 0')
print(sum(testshFF3_2))

#Part 3. The same as part 2, but just CAPM
X3b=RA_SP500-TBill3MonthsRate

outFF3_CAPM=data.frame(NULL)
for (i in 1:TT){
  bFF3_CAPM=summary(lm(y3[1:NN,i]~X3b[1:NN]))
  outFF3_CAPM[i,1]=bFF3_CAPM$coefficients[1,1]
  outFF3_CAPM[i,2]=bFF3_CAPM$coefficients[1,4]
  outFF3_CAPM[i,3]=bFF3_CAPM$coefficients[2,1] 
  outFF3_CAPM[i,4]=bFF3_CAPM$coefficients[2,4]
  outFF3_CAPM[i,5]=bFF3_CAPM$r.squared
}

names(outFF3_CAPM) <- c("Alpha","PValueAlpha","Beta","PValueBeta", "Rsquared")
outFF3_CAPM

testsalphasFF3_CAPM=NULL
testsbetasFF3_CAPM=NULL

for (i in 1:6){
  if (outFF3_CAPM[i,2]<0.05){testsalphasFF3_CAPM[i]=1} else{testsalphasFF3_CAPM[i]=0}
  
  if (outFF3_CAPM[i,4]<0.05){testsbetasFF3_CAPM[i]=1} else{testsbetasFF3_CAPM[i]=0}
}

print('Fama French Testing on First Subsample')
print('Number of alphas different from 0')
print(sum(testsalphasFF3_CAPM))

print('Number of betas different from 0')
print(sum(testsbetasFF3_CAPM))

Rsquared_FF3=outFF3[,9]
Rsquared_CAPM=outFF3_CAPM[,5]
Rsquared_FF3_adj=Rsquared_FF3-(1-Rsquared_FF3)*3/(NN-3-1)
Rsquared_CAPM_adj=Rsquared_CAPM-(1-Rsquared_CAPM)*1/(NN-1-1) 

####Now, use the remainder of the sample to see how the alphas and betas
####change over time and also to re-assess who is indistinguishable from
####zero or not in order to see how stable the model is over time

outFF3_2_CAPM=data.frame(NULL)
for (i in 1:TT){
  bFF3_2_CAPM=summary(lm(y3[(NN+1):TT,i]~X3b[(NN+1):TT]))
  outFF3_2_CAPM[i,1]=bFF3_2_CAPM$coefficients[1,1]
  outFF3_2_CAPM[i,2]=bFF3_2_CAPM$coefficients[1,4]
  outFF3_2_CAPM[i,3]=bFF3_2_CAPM$coefficients[2,1] 
  outFF3_2_CAPM[i,4]=bFF3_2_CAPM$coefficients[2,4]
  outFF3_2_CAPM[i,5]=bFF3_2_CAPM$r.squared
}

names(outFF3_2_CAPM) <- c("Alpha","PValueAlpha","Beta","PValueBeta", "R-squared")
outFF3_2_CAPM

testsalphasFF3_2_CAPM=NULL
testsbetasFF3_2_CAPM=NULL
for (i in 1:6){
  if (outFF3_2_CAPM[i,2]<0.05){testsalphasFF3_2_CAPM[i]=1} else{testsalphasFF3_2_CAPM[i]=0}
  
  if (outFF3_2_CAPM[i,4]<0.05){testsbetasFF3_2_CAPM[i]=1} else{testsbetasFF3_2_CAPM[i]=0}
  
}

print('CAPM Testing on Second Subsample')
print('Number of alphas different from 0')
print(sum(testsalphasFF3_2_CAPM))

print('Number of betas different from 0')
print(sum(testsbetasFF3_2_CAPM))

n=length(y3[(NN+1):TT,2])
Rsquared_FF3_2=outFF3_2[,9]
Rsquared_CAPM_2=outFF3_2_CAPM[,5]
Rsquared_FF3_2_adj=Rsquared_FF3_2-(1-Rsquared_FF3_2)*3/(n-3-1)
Rsquared_CAPM_2_adj=Rsquared_CAPM_2-(1-Rsquared_CAPM_2)*1/(n-1-1) 

####Remarks:
#It would seem that the Fama French 3 factor model does a better job at
#explaining returns, as suggested by the substatial increases in R squared (and adjusted R squared as
#you may check yourself using the formula for adj. R squared) for all 6
#portfolios. This analysis can be extended to Fama and French's original 25
#portfolios in their paper

#However, the stability of the regression parameters over time still
#remains an issue. Also, the alphas are not zero all the time.