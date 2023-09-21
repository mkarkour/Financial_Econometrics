CS=DailyData$CREDIT.SUISSE.ASST.MAN.
RA_CS=(CS[2:end]-CS[1:(end-1)])/CS[1:(end-1)]


###################################################################################
# Question 1: Data: Same as TP1
###################################################################################
  
###################################################################################
# 1.

# The question is corrected for CREDIT SUISSE 
CS=DailyData$CREDIT.SUISSE.ASST.MAN.
RA_CS=(CS[2:end]-CS[1:(end-1)])/CS[1:(end-1)]

nb_ret=length(CS)-1 #Because CS contains prices -> We lose one observation when we construct the vector of returns

###################################################################################
# 2.

mean_CS=mean(RA_CS)
sd_CS=sd(RA_CS)
var_CS=sd_CS^2
skewness_CS=skewness(RA_CS)
kurtosis_CS=kurtosis(RA_CS)

###################################################################################
# Question 2: Estimation
###################################################################################
  
###################################################################################
# 1.

# The estimation of a density non-parametrically using a gaussian kernel on R is quite simple. You can simply use the function density()
# and specify that you want to do the estimation with a gaussian kernel

RuleOfThumb=(nb_ret^(-0.2))*(sd_CS)
pdf_CS=density(RA_CS, kernel="gaussian", bw=RuleOfThumb)

# Plot the density

plot(pdf_CS$x, pdf_CS$y, main='Gaussian Kernel Density of CS - Rule of Tumb', xlab='values', ylab='Density', type='l', xlim=c(-0.18,0.18), ylim=c(0,55))

###################################################################################
# 2.

# Choose different values for the bandwith:

over=RuleOfThumb*10
pdf_CS_over=density(RA_CS, kernel="gaussian",bw=over)
plot(pdf_CS_over$x, pdf_CS_over$y, main='Gaussian Kernel Density of CS - Oversmoothing', xlab='values', ylab='Density', type='l', xlim=c(-0.18,0.18), ylim=c(0,55))

under=RuleOfThumb/10
pdf_CS_under=density(RA_CS, kernel="gaussian",bw=under)
plot(pdf_CS_under$x, pdf_CS_under$y, main='Gaussian Kernel Density of CS - Undersmoothing', xlab='values', ylab='Density', type='l', xlim=c(-0.18,0.18), ylim=c(0,55))

# The function layout() allows you to combine multiple plots in one overall graph. 
# Here, the function layout is set-up to have one figure in row 1 and two figures in row 2.
# help(layout) for more info on the function.
# You can also use the function par() to combine multiple plots in one overall graph. 

layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(pdf_CS$x, pdf_CS$y, main='Gaussian Kernel Density of CS - Rule of Tumb', xlab='values', ylab='Density', type='l', xlim=c(-0.18,0.18), ylim=c(0,55))
plot(pdf_CS_over$x, pdf_CS_over$y, main='Gaussian Kernel Density of CS - Oversmoothing', xlab='values', ylab='Density', type='l', xlim=c(-0.18,0.18), ylim=c(0,55))
plot(pdf_CS_under$x, pdf_CS_under$y, main='Gaussian Kernel Density of CS - Undersmoothing', xlab='values', ylab='Density', type='l', xlim=c(-0.18,0.18), ylim=c(0,55))


# Comments:
# Normally you should observe that when the bandwith is too large (you can
# try to increase more the number), the estimated density is more and more
# flat. On the contrary, when the bandwith is too small the estimated
# density is very imprecise and oscillatory.
# The bandwith is then a smoothing parameter.
# There are methods to choose the best value for the bandwith.

###################################################################################
# 3.

# We divide the sample in 2 and we estimate the density on each part. We plot the result.

pdf_CS_old=density(RA_CS[1:(nb_ret/2)], kernel="gaussian")
pdf_CS_recent=density(RA_CS[((nb_ret/2)+1):nb_ret], kernel="gaussian")

par(mfrow=c(2,1))
plot(pdf_CS_old$x, pdf_CS_old$y, main='Old Series', xlab='values', ylab='Density', type='l', xlim=c(-0.15,0.2), ylim=c(0,70))
plot(pdf_CS_recent$x, pdf_CS_recent$y, main='Recent Series', xlab='values', ylab='Density', type='l', xlim=c(-0.15,0.2), ylim=c(0,70))

# One of the hypothesis of many models like the CAPM is that the distribution of returns is stable over time. 
# You can see that it is not the case.


###################################################################################
# Question 3: Simulation
###################################################################################
  
# We use the rnorm function
simul_data=rnorm(1000,0,1)
kernel_pdf=density(simul_data, kernel="gaussian")

# you should see that when the ratio volatility/mean increases, the
# esimation is less precise. As well, if you reduce the size of your
# simulated sample, the density will be less precise.
# A good way to see if the gaussian kernel estimator is effective is to
# compare the estimated density and the theoretical density (given by dnorm):
  
normpdf=dnorm(kernel_pdf$x,0,1)

par(mfrow=c(2,1))
plot(kernel_pdf$x, kernel_pdf$y, main='Gaussian density with gaussian kernel estimator', xlab='values', ylab='Density', type='l', xlim=c(-4,4), ylim=c(0,0.5))
plot(normpdf, main='Theoretical Gaussian density', xlab='values', ylab='Density', type='l', xlim=c(0,500), ylim=c(0,0.5))

plot(normpdf)

# Using a gaussian kernel estimator doesn't imply that the resulting density is normal.
# To convince you, we redo the last calculations with a sample simulated from beta(1.5,4).

simul_data=rbeta(1000,1.5,4)
kernel_pdf=density(simul_data, kernel="gaussian")

betapdf=dbeta(kernel_pdf$x,1.5,4)

par(mfrow=c(2,1))
plot(kernel_pdf$x, kernel_pdf$y, main='Beta density with gaussian kernel estimator', xlab='values', ylab='Density', type='l', xlim=c(0,0.85), ylim=c(0,2.5))
plot(betapdf, main='Theoretical Beta density', xlab='values', ylab='Density', type='l', xlim=c(80,500), ylim=c(0,2.5))

###################################################################################
# Question 4
###################################################################################

CS_normpdf=dnorm(pdf_CS$x,mean=mean_CS,sd=sd_CS)

par(mfrow=c(2,1))
plot(pdf_CS$x, pdf_CS$y, main='Credit Suisse density with gaussian kernel estimator', xlab='values', ylab='Density', type='l', xlim=c(-0.18,0.18), ylim=c(0,55))
plot(CS_normpdf, main='Credit Suisse density assuming a normal law', xlab='values', ylab='Density', type='l', xlim=c(0,500), ylim=c(0,27))
