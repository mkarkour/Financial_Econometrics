#Plot the time series of prices versus the days:

#Import data
MSFT=data1[,2]
plot(MSFT)

#Find the return over the first day
R1=log(MSFT[2]/MSFT[1])
R1

#Compute the vector of returns
end=length(MSFT)
P1=MSFT[2:end]
P2=MSFT[1:(end-1)]
RMSFT=log(P1/P2)

#Write an R function that helps you compute the descriptive statistics of the return vector

library(timeDate)

MyFunction <- function(data){
  m=mean(data)
  std=sd(data)
  skw=skewness(data)
  kur=kurtosis(data)
  results=c(m,std,skw,kur)
  print(results)
}

MyFunction(RMSFT)

# The function "MyFunction" returns the mean, standard deviation, skewness
# and kurtosis of a variable. Here, we apply it to the return series of Microsoft.
