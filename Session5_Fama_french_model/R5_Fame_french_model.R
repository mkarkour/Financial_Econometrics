#about the data:

#FFP portfolio
"we have low book to market ratio, meidum and high"
"small compagnie are include in the first three columns"
"and in the last three columns we have the big compagnie "

#SP100index
"it's the price not the return for the SP100 & SP500"

################################################################################
###########################      Question 1        #############################
################################################################################
#Compute the SMB returns

famafrench = FFPortfolios[,2:7]/100 #divis� par 100 pour les avoirs en units pas en %
famafrench


n=length(famafrench[,1]) #number of weeks that i have in my dataset
n

small = apply(famafrench[,1:3],1,mean) #the second argument is row or columns on which i want to apply
big= apply(famafrench[,4:6],1,mean)

smb = small-big
smb #vector with 523 returns of the diff between return of small and big compagnies
length(smb)
smb=smb[2:n]

################################################################################
###########################      Question 2        #############################
################################################################################
#Compute the HML returns

high = apply(famafrench[,c(3,6)],1,mean)
low = apply(famafrench[,c(1,4)],1,mean)
hml = high - low
hml #vector that is the difference between high book to market ratio & low book to market ratio
length(hml)
hml = hml[2:n]

################################################################################
###########################      Question 3        #############################
################################################################################
#Compute the equation
SP500_price = SP100Index[,3] 
SP500_price

#I need to compute the return
SP500_ra = SP500_price[2:n]/SP500_price[1:(n-1)]-1
SP500_ra
length(SP500_ra) #on perd une observation ce qui est normal(voir l'equation)

#I need the risk free rate
rf = TBill[,2]/5200
#5200 parce que, quand on extrait le return des tbills, il s'agit de tbills weekly
#parce que on a 52 semaine dans une ann�e, et on rajoute un *100 parce que les returns
#sont exprim�s en %
length(rf)
rf =rf[2:n]

#comme on a pas la m�me longueur pour tous, on va retirer une valeur pour smb,hml et rf

x = matrix(c(SP500_ra-rf,smb,hml),nrow = 522, ncol = 3) #ncol = 3 pcq nous avons 3 variables
y = famafrench[2:n,]-rf

#on fait une boucle pour faire la regression des 6 portefeuille

outff3 = data.frame(NULL)

#I do for 2 times series so i need to precise which period I will use
N=457 #correspond to the week where i divide my dataset

#exemple pour un portfolio
ex_one = summary(lm(y[1:N,1]~x[1:N,]))
ex_one$coefficients
#we have the intercept --> alpha
#ligne 2 : coef du risk premium
#ligne 3 : coef du smb
#ligne 4 : coef du hml


for (i in 1:6){ #6 because i have 6 portfolio
  m=summary(lm(y[1:N,i]~x[1:N,]))
  outff3[i,1]= m$coefficients[1,1] #alpha de chaque portfolio
  outff3[i,2]= m$coefficients[1,4] #p value of the intercept which is alpha
  outff3[i,3]= m$coefficients[2,1] #coef of the risk premium (beta)
  outff3[i,4]= m$coefficients[2,4] #p value of the coef of the risk premium
  outff3[i,5]= m$coefficients[3,1] #coef of the smb
  outff3[i,6]= m$coefficients[3,4] #pvalue of the coef of the smb
  outff3[i,7]= m$coefficients[4,1] #coef of hml
  outff3[i,8]= m$coefficients[4,4] #pvalue of the hml
  outff3[i,9]= m$r.squared #I use the r squared to compare the performance of the capm and the fama french
  outff3[i,10] = m$adj.r.squared #the adjusted r-squared
}

outff3
typeof(outff3)
names(outff3)<-c("Alpha","PValueAlpha","Beta","PValueBeta", "BetaSMB","PValueSMB", "BetaHML","PValueHML",
                 "R-squared","adj R-squared")


testsalphasFF3=NULL
testsbetasFF3=NULL
testssFF3=NULL
testshFF3=NULL

for (i in 1:6){
  if (outff3[i,2]<0.05){testsalphasFF3[i]=1} else{testsalphasFF3[i]=0}
  
  if (outff3[i,4]<0.05){testsbetasFF3[i]=1} else{testsbetasFF3[i]=0}
  
  if (outff3[i,6]<0.05){testssFF3[i]=1} else{testssFF3[i]=0}
  
  if (outff3[i,8]<0.05){testshFF3[i]=1} else{testshFF3[i]=0}
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



################################################################################
###########################      Question 4        #############################
################################################################################
#do the capm model and compare with the fama french model

x1 = matrix(c(SP500_ra-rf),nrow = 522, ncol = 1) #j'adapte pour le capm
y1 = famafrench[2:n,]-rf #same because i apply the capm model on the 6 portfolio

outcapm=data.frame(NULL)

for (i in 1:6){
  m=summary(lm(y1[1:N,i]~x1[1:N])) #je le fait sur ma p�riode N
  outcapm[i,1]=m$coefficients[1,1]
  outcapm[i,2]=m$coefficients[1,4]
  outcapm[i,3]=m$coefficients[2,1]
  outcapm[i,4]=m$coefficients[2,4]
  outcapm[i,5]=m$r.squared
  outcapm[i,6]=m$adj.r.squared
}

names(outcapm)<-c("Alpha","PValueAlpha","Beta","PValueBeta","R-squared","adj R-squared")
outcapm


"Pour voir quels mod�les fait mieux que l'autre je regarde le rsquared :
  cela me donne un indicateur sur la qualit� du mod�le,
  donne une mesure de la qualite des param�tres utilis�es

Le adjusted r-squared:
 on le regarde tjrs en priorit� parce que le r-squared augmente d�s que 
 le nombre de variable de mon mod�le augmente, alors que le adj. rsquarred
 augmente uniquement quand on ajoute des informations au mod�les."


"conclusion
  on voit que le adjusted r squared du fama french model est plus �l�v� que pour 
  le capm on peut donc dire que le fama french model fait un meilleur job que le capm"


"Remarks:
  It would seem that the Fama French 3 factor model does a better job at
  explaining returns, as suggested by the substatial increases in R squared (and adjusted R squared as
  you may check yourself using the formula for adj. R squared) for all 6
  portfolios. This analysis can be extended to Fama and French's original 25
  portfolios in their paper

  However, the stability of the regression parameters over time still
  remains an issue. Also, the alphas are not zero all the time."


testsalphasFF3_CAPM=NULL
testsbetasFF3_CAPM=NULL

for (i in 1:6){
  if (outcapm[i,2]<0.05){testsalphasFF3_CAPM[i]=1} else{testsalphasFF3_CAPM[i]=0}
  
  if (outcapm[i,4]<0.05){testsbetasFF3_CAPM[i]=1} else{testsbetasFF3_CAPM[i]=0}
}

print('Fama French Testing on First Subsample')
print('Number of alphas different from 0')
print(sum(testsalphasFF3_CAPM))

print('Number of betas different from 0')
print(sum(testsbetasFF3_CAPM))


################################################################################
###########################      Question 5        #############################
################################################################################
#repeat the question 3 and 4 but on the other period (458:524)


outff3_period2=data.frame(NULL)
N2 = 522 #dans l'�nnonc� on dit de prendre 524 mais au total je n'ai que 522 period

for (i in 1:6){ #6 because i have 6 portfolio
  m=summary(lm(y[(N+1):N2,i]~x[(N+1):N2,]))
  outff3_period2[i,1]= m$coefficients[1,1] #alpha de chaque portfolio
  outff3_period2[i,2]= m$coefficients[1,4] #p value of the intercept which is alpha
  outff3_period2[i,3]= m$coefficients[2,1] #coef of the risk premium (beta)
  outff3_period2[i,4]= m$coefficients[2,4] #p value of the coef of the risk premium
  outff3_period2[i,5]= m$coefficients[3,1] #coef of the smb
  outff3_period2[i,6]= m$coefficients[3,4] #pvalue of the coef of the smb
  outff3_period2[i,7]= m$coefficients[4,1] #coef of hml
  outff3_period2[i,8]= m$coefficients[4,4] #pvalue of the hml
  outff3_period2[i,9]= m$r.squared #I use the r squared to compare the performance of the capm and the fama french
  outff3_period2[i,10] = m$adj.r.squared #the adjusted r-squared
}

names(outff3_period2)<-c("Alpha","PValueAlpha","Beta","PValueBeta", "BetaSMB","PValueSMB", "BetaHML","PValueHML",
                          "R-squared","adj R-squared")
outff3_period2

outff3

"les coefficients sont diff�rentes pour les 2 p�riodes, cela veut dire que 
  l'hypoth�se de stabilit� n'est pas valide d'un point de vue empirique
  les returns ne sont pas stables au cours du temps, m�me si le fama french model
  fait un meilleur travail que le capm, les returns ne sont pas stables

  les 3 facteurs du fama french model ne sont pas suffisant pour expliquer la variation
  de l'excess return. Mais si on l'applique sur le stock model ce sera le cas aussi(pas bon)"



testsalphasFF3_2=NULL
testsbetasFF3_2=NULL
testssFF3_2=NULL
testshFF3_2=NULL
for (i in 1:6){
  if (outff3_period2[i,2]<0.05){testsalphasFF3_2[i]=1} else{testsalphasFF3_2[i]=0}
  
  if (outff3_period2[i,4]<0.05){testsbetasFF3_2[i]=1} else{testsbetasFF3_2[i]=0}
  
  if (outff3_period2[i,6]<0.05){testssFF3_2[i]=1} else{testssFF3_2[i]=0}
  
  if (outff3_period2[i,8]<0.05){testshFF3_2[i]=1} else{testshFF3_2[i]=0}
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
