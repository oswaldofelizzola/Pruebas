#limpieza de entorno
rm(list=ls())
cat("\014")

#Lectura de datos y vectorizacion
setwd("~/Desktop/dolar")
dolartoday=read.csv("dolartoday.csv")
do<- dolartoday[,c(1,3)]
va <- unlist(as.list(do[,2]))
fe <- unlist(as.list(do[,1]))


#acotacion a un tiempo fijado
ncal=365
valor <- tail(va,n=ncal)
fecha <- as.Date(tail(fe,n=ncal),origin="1899-12-30")
dolar <- tail(do,n=ncal)


#Graficacion
valormin <- min(valor)
valormax <- max(valor)
fechamin <- min(fecha)
fechamax <- max(fecha)
plot(fecha,valor, type="l" , main="valor del dolar",xlab="fecha",ylab = "dolar",xlim=c(fechamin, fechamax), ylim=c(valormin, valormax))

#Regresion lineal
fit <- lm(valor~fecha,data=dolar)

#Histograma de diferencias diarias
dv <- c(valor,0)-c(0,valor)
difvalor <- (dv[2:(ncal)]/valor[1:ncal-1])*100
hist(difvalor, main="Histograma de cambio diario",xlab="variacion %",ylab = "Frecuencia")

#RSI requiere paquete TTR
rsical <- RSI(valor,n=7)
plot(fecha,rsical, type="l" , main="Indice de fuerza relativa",xlab="fecha",ylab = "Indice",xlim=c(fechamin, fechamax), ylim=c(0, 100))
