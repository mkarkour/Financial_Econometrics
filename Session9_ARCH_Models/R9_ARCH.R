###################################################################################
###-------------------------------- QUESTION 1 ---------------------------------###
###################################################################################
#Q1. For the series contained in TP9.csv, determine the values of
#p1 and q1 for a corresponding ARMA model, using the
#techniques of TP 8.

y1 = data$Y1
#y1=data[,1]
y1_acf = acf(y1)
y1_pacf = pacf(y1)
"On voit qu'il y a 6 barre pour le acf, ce qui nous donnerait une MA(6) et il y 
a également une forme géométrique décroissante. Modèle plus copliqué.

Pour le pacf, il n'y a qu'une barre en dehors, ce qui donne AR(1) et la forme 
géométrique est sinusoidale, le modèle est moins compliqué. 
==> on utilisera donc le AR(1)

N.B.: pour rappel, pour acf, on ne regarde jamais la première barre
"

###################################################################################
###-------------------------------- QUESTION 2 ---------------------------------###
###################################################################################
#Q2. Estimate the coefficients of your ARMA model and compute
#the residuals. Validate your choice of p1 and q1 with the
#Ljung-Box test.

t=length(y1)
y=y1[2:t]
lag=y1[1:(t-1)]

ARI = lm(y~lag)
summary(ARI)
"alpha = 0.82051
beta = 0.73067
si on regarde la p-value, on constate que les deux sont significatifs (p-value basse)"

resids = ARI$residuals
Box.test(resids,lag=5,type="Ljung-Box")
"si la p-value est élevé, cela veut dire que l'on ne rejette pas H0 et donc 
les residuals sont des whites noises(ce qui est bien,on veut ça)"

out = data.frame(NULL)
h=c(5,10,15,20)

for (i in 1:length(h)){
  y = Box.test(resids,lag=h[i],type="Ljung-Box")
  out[i,1]=y$statistic
  out[i,2]=y$p.value
  out[i,3]=qchisq(0.95, df=h[i]) #quantile de la distribution, IC de 95%, degree
  #of freedom
  out[i,4]=h[i]
}
names(out)=c("Statistic (X-squared)","P-value","Critical value","lag")
out
"la P-value est tjrs grande ou au moins tjrs plus grande que 5%, ce qui veut dire
que pour tout les décalages (lags) on ne rejectte pas H0"

###################################################################################
###-------------------------------- QUESTION 3 ---------------------------------###
###################################################################################
#Q3.  Test for the presence of ARCH effects using correlograms of
#squared residuals and the ARCH test.
#Pour faire cela, on regarde le squarred residuals et regarder si on a 
#un lag efficient, any lag !!

"Juste par observation"
resids2= resids^2
acf(resids2) "on a un lag significant (la deuxième barre)"
pacf(resids2) "on a des lag aussi importants (le 1er, 9 ème)"

"cela est une indication qui nous dit qu'effectivement, ARCH est intéressant à
prendre en compte ici"

"Pour confirmer cela on va faire le formal test, en appliquant box test
sur les resids 2 (qui ont été mis au carré)"
out2 = data.frame(NULL)
h=c(5,10,15,20)

for (i in 1:length(h)){
  y = Box.test(resids2,lag=h[i],type="Ljung-Box")
  out2[i,1]=y$statistic
  out2[i,2]=y$p.value
  out2[i,3]=qchisq(0.95, df=h[i]) 
  out2[i,4]=h[i]
}
names(out2)=c("Statistic (X-squared)","P-value","Critical value","lag")
out2

"On remarque que les p-values sont très basses, inférieur à 5%, cela veut dire
que l'on rejette H0 (=le fait que l'on n'est pas de ARCH effect)
==> on a donc du ARCH effect
je peux donc utiliser le Garch model pour modeliser la volatilité"


###################################################################################
###-------------------------------- QUESTION 4 ---------------------------------###
###################################################################################
#Q4. If you detect ARCH effects, identify the orders p2 and q2 of a
#corresponding GARCH model.

"On regarde simplement les graphes sur resids2
on voit que pour Acf on a le lag 1 qui est intéressant et on a pas de contrepartie
pour pacf,on va donc prendre GARCH(0,1) 
et si il y a de l'hétéroscédasticité, on utilisera un GARCH(1,1)
Mais généralement en finance, GARCH(1,1) sera suffisant on n'ira pas au dessus

en pratique on essayera (0,1),(1,0) et (1,1)
"

resids2acf=acf(resids2)
plot(resids2acf, main='Y1 Squared residuals - ACF', xlim=c(0,20), ylim=c(-0.2,1), xlab='Lag', ylab='Sample Autocorrelation')

resids2pacf=pacf(resids2)
plot(resids2acf, main='Y1 Squared residuals - PACF', xlim=c(0,20), ylim=c(-0.2,1), xlab='Lag', ylab='Sample Partial Autocorrelation')

###################################################################################
###-------------------------------- QUESTION 5 ---------------------------------###
###################################################################################
#Q5. Use the garch() function from package tseries to obtain
#parameter estimates of the GARCH model, as well as residuals.

library(tseries)
GARCH = garch(resids,order=c(0,1)) 
"si cela ne marche pas on fera un (1,0) et si cela ne fonctionne tjrs pas on prendra (1,1)"
GARCH$coef
"cela nous donne les coef du garch modele :correspond au aj dans l'équation et on 
n'a pas la partie avec le -bj"

###################################################################################
###-------------------------------- QUESTION 6 ---------------------------------###
###################################################################################
#Q6. Check that your model explains the heteroscedasticity by applying the tests
#of the first section on the squared residuals.

resids_garch = GARCH$residuals
resids_garch2 = resids_garch^2

Box.test(resids_garch2)
"si la p-value est faible cela n'est pas bon --> veut dire que GARCH(0,1) n'est
pas suffisant et qu'il faut prendre un autre couple de point

si p-value est grande, bon --> cela veut dire que GARCH(0,1) est suffisant"


out3 = data.frame(NULL)

for (i in 1:length(h)){
  y = Box.test(resids_garch2,lag=h[i],type="Ljung-Box")
  out3[i,1]=y$statistic
  out3[i,2]=y$p.value
  out3[i,3]=qchisq(0.95, df=h[i]) 
  out3[i,4]=h[i]
}
names(out3)=c("Statistic (X-squared)","P-value","Critical value","lag")
print("for GARCH(0,1) de AR(1)")
out3
"Toute les p-value sont grandes, ce qui veut dire que l'on ne rejette pas H0 qui
dit qu'il n'y a pas de ARCH effect

AR(1)-GARCH(0,1) model sufficiently explains the heteroscedasticity "
