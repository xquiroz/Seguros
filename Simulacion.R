#Modelo individual 
install.packages("readxl")
library("readxl")
library("Rlab")

file.choose()# funcion para obtener dirreccion de archivos 
ruta <- "C:\\Users\\saund\\OneDrive\\Escritorio\\MASIV\\LAB 3\\lab3.xlsx"
d <- read_excel(ruta,sheet= 'asegurados', range='A4:E32') #lee tabla de asegurados 
qx <- d$qx#dentro de un vector cada una de las qx 
sa <-d$`Suma Asegurada`#dentro de un vector cada una de las sumas aseguradas 
help("distribution")#funcion de guia para distribuciones 
s <- c()#creamos vector 
for(i in 1:1000){#suma de cada simulacion 
  suma <- 0 #reiniciar contador 
  for(j in 1:28 ){# sumatoria de DJCJ
    dj <- rbern(1, prob =qx[j])#funcion que da valores aleatorios por bernoulli dando una prob.
    v<- dj[1]#extraemos del vector 
    suma <- suma + (v*sa[j]) #sumatoria de la simulacion #i
  }
  s[i]<- #se mete dentro de uj vector cada sumatoria DJCJ 1000 veces 
}
s
hist(s)#Grafica del histograma 

#****************************************************
##Grupo generalizado 
#Distribucion yi 
file.choose()
ruta2 <- "C:\\Users\\saund\\OneDrive\\Escritorio\\MASIV\\LAB 3\\lab 3.2 .xlsx"
gg <- read_excel(ruta2, range = 'A7:E2207', col_names = FALSE )#lea el excel

a2017 <- gg$...1# cada una de las columnas del df
a2018 <- gg$...2#las hacemos vectores por cada año y
a2019 <- gg$...3#asi filtrar los 0 y NA 
a2020 <- gg$...4
a2021 <- gg$...5
c <- c(a2017,a2018,a2019,a2020,a2021)#concatenamos los vectores 
yi<-c()#hacemos un vector donde estaran todos los Y  reclamos en los 5 años 
j <-1#contador de los Y
for (i in 1:11005){ #Filtra los 0 
  if ((c[i]!= 0) | (is.na(c[i])) ){#condicion diferente de 0
    yi[j] <- c[i]#se agrega al vector 
    j= j +1 #va incrementando el contador 
  }
}
yi<- yi[!is.na(yi)]#filtra NA
hist(yi)
1/mean(yi)
#Distribucion de S 
nor<- c() #hacemos un vectopr donde estaran las simulaciones
n <-2064#valores de n y q 
q <-0.1620429215922120

for(i in 1:1000){#simulaciones con exp y bin con parametros ya calculados 
  nor[i]<- sum(rbinom(n,1,q)*rexp(n,1/mean(yi)))
}
hist(nor)#realizamos histograma 
mean(nor)#media
var(nor)#varianza 
teta <- 0.1196022064717790
mod <- c()#vector para introducir simulacion de ajuste 
for(i in 1:1000){
  mod[i]<- mean(nor)*(1+teta)-sum((rbinom(n,1,q)*rexp(n,1/mean(yi))))
}#se realiza la simuliacion usando M(yi)
hist(mod)

