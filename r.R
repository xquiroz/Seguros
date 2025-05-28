#Realizar 500 veces la simulacion de s, variable aleatoria
library(Rlab)
library(ggplot2)

#Definimos vector donde se guardara s 
s <- vector(1:500)
length(s)

for(j in 1:500){ 
  s[j] <- rbern(n=500,0.3)*rexp(n=500, 1/1000)
}
#sumatoria de cada elemento 
sum(s)

#Histograma 
hist(s, main= "Repeticiones de S", breaks=5, col="pink",border="purple")

#Esperado E(s)
E= 500*0.3*1000
E
sum(s)
##Viendo el resultado de la suma (S) y del E(s) vemos que sus resultados son algo 
##cercanos, y esto se da ya que para eso se utiliza E(s) para calcular el valor 
## que se espera 

