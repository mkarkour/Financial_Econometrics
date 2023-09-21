a = 3
# to remove
rm(a)
a=5
#to create a vecteur, we need c to do this
b = c(1,2,3,4)
#on a besoin de spécifier 3 attributs pour la séquence
#fisrt attribut, first value of my sequence
#second is the last value
#third increment that i will apply to my value
c = seq(0,10,0.1) #on commence à 0 jusque 10 avec une addition de 0.1 à 
#la value précédente
#création d'une matrix
#3 attributs 
d=matrix(1:9, nrow=3, ncol=3) 
#je peux cliquer sur le d dans la fenetre 2 et cela ouvre une page avec ma matrice

e=matrix(b,nrow=4, ncol=4)
aa=c(1,2,3,4,4,5,2,5,6)
print(aa[1])
print(length(aa))
print(aa[2:5])

#row,colon
print(e[3,4])
#toute la ligne
print(e[1,])
#toute la colonne
print(e[,1])

x=seq(0,10,0.01)
y=sin(x)
plot(x,y)

a <-function(data){
  m=mean(data)
  print(data)
}

library(timeDate)
mean(data$MSFT)
mean(data[,1])



