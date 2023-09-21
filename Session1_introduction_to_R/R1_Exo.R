#Theoretical Reminder
mean(data1$MSFT)
mean(data1[,2])
sd(data1$MSFT)
skewness(data1$MSFT)
kurtosis(data1$MSFT) #If I want the kurtosis I need to add 3 at this value

#print a summary of the different indicator for my data set
summary(data1)

myfunction= function(x){
  m = mean(x)
  n = sd(x)
  results = c(m,n)
  print(results)
}

myfunction(data1$MSFT)


################
### EXERCICE ###
################

#Exo_1
MSFT = data1$MSFT
plot(MSFT)

#Exo_2
R1 = (MSFT[2]-MSFT[1])/MSFT[1]
print(R1)

R2 = log(MSFT[2]/MSFT[1])
print(R2)

#Exo_3
end=length(MSFT)    
P1=MSFT[2:end]      
P2=MSFT[1:(end-1)]
RMSFT=log(P1/P2)
print(RMSFT)


#Exo_4
Analysis = function(x){
  m=mean(x)
  std=sd(x)
  kur=kurtosis(x)
  ske=skewness(x)
  results = c(m,std,kur,ske)
  print(results)
}

Analysis(RMSFT)










