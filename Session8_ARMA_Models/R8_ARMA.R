###################################################################################
###-------------------------------- QUESTION 1 ---------------------------------###
###################################################################################
#Q1.Plot and comment the sample ACF and PACF for Coca-Cola 
#and Credit Suisse (hint: use functions acf() and pacf() on R).

cs_prices = data[,3] 
cs_acf = acf(cs_prices)
cs_pacf = pacf(cs_prices)
#le prix d'une action n'est pas stationnaire, je ne peux donc pas directement
#faire le acf et le pacf dessus.(graphe pas de sens)

coca_prices = data[,5]
coca_acf = acf(coca_prices)
coca_pacf = pacf(coca_prices)

###################################################################################
###-------------------------------- QUESTION 2 ---------------------------------###
###################################################################################
#Q2.Repeat point 1 on the 1st differences of the log-prices (i.e. the 
#logarithmic returns).

t=length(cs_prices)
cs_log_return = log(cs_prices[2:t])-log(cs_prices[1:t-1])

t_coca = length(coca_prices)
coca_log_return = log(coca_prices[2:t_coca])-log(coca_prices[1:t_coca-1])

cs_lr_acf=acf(cs_log_return)
#la premiere barre sera tjrs égale à 1 car c'est le lag 0 --> ignorez le 0
cs_lr_pac=pacf(cs_log_return)

coca_lr_acf=acf(coca_log_return)
coca_lr_pac=pacf(coca_log_return)

"(pour credit suisse) en regardant les graphiques, on contaste que l'on a 4 barre qui sortent de 
l'intervalle de confiance pour acf et pacf ce qui nous permet déjà de dire que
l'on aura soit MA(4) soit AR(4), si les deux sont significatifs, on pourra utiliser
ARMA(4,4)

comme on préfère les modèles simples, on commencera avec un AR(4) pour voir si cela
est bon et suffisant, si ce n'est pas le cas on prendra MA(4) et si ce n'est pas encore 
suffisant on prendre ARMA"

###################################################################################
###-------------------------------- QUESTION 3 ---------------------------------###
###################################################################################
#Q3. You can quantify the preceding qualitative checks for 
#correlation with a formal hypothesis test. Using the Ljung Box-Pierce Q-test, 
#test the presence of correlation in the returns for up to 20 lags at the 0.05 
#level of significance (hint: use function Box.test() on R).

h=c(5,10,15,20)
out_c <- data.frame(NULL) 
for (i in 1:length(h)){
  y=Box.test(coca_log_return,lag = h[i])
  out_c[i,1]=h[i]
  out_c[i,2]=y$p.value
  out_c[i,3]=y$statistic
  out_c[i,4]=qchisq(0.95, df=h[i])
}
names(out_c)=c("Lags","p-value","Statistic","Critical value")
print(out_c)

###################################################################################
###-------------------------------- QUESTION 4 ---------------------------------###
###################################################################################
#Q4.Estimate the coefficients of an AR(p) for Credit Suisse by OLS.
#White variable
t_r = length(cs_log_return)
y = cs_log_return[5:t_r]
y1 = cs_log_return[4:(t_r-1)]
y2 = cs_log_return[3:(t_r-2)]
y3 = cs_log_return[2:(t_r-3)]
y4 = cs_log_return[1:(t_r-4)]

ols = lm(y~y1+y2+y3+y4)
summary(ols)
#regarder la p-value des variables, ici elles sont toutes significatives
#veut dire que cela explique les variables dépendantes y
#now we want to see if the residual are white noise

###################################################################################
###-------------------------------- QUESTION 5 ---------------------------------###
###################################################################################
#Q5. Check that the residuals of your estimated model are white 
#noise with the Ljung-Box test.

residuals = ols$residuals
residuals
lag=c(5,10,15,20) #multiple de 5 car en finance on suit les jours d'ouverture du marché 
#(les 5 jours de la semaine ou le marché est ouvert), 20 est suffisant pour faire le test
Box.test(residuals,lag=5)
"la p-value (0.9065) pour lag 5 est élévé ce qui implique que l'hypothèse nulle 
n'est pas rejetté, on ne rejette donc pas l'hypothèse que les residuals
sont des whites noises

on peut prendre d'autre lag et regarder si ceux ci sont rejetté ou non
pour lag 10 --> pas rejetter
pour lag 20 --> pas rejetter non plus (p-value = 0.25)

==> on veut avoir une p-value élevé quand on fait un arma model"

out = data.frame(NULL)
for (i in 1:length(lag)){
  y = Box.test(residuals,lag=lag[i])
  out[i,1]=y$statistic
  out[i,2]=y$p.value
  out[i,3]=lag[i]
  names(out)= c("statistic - X-squared","p-value","lag")
}
out
