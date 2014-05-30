METODOS-PARCIAL-II
==================

COMADOS
```
{
compa<-function(r){
  grupos<-rep(1:3,c(20,20,20))
  resul<-matrix(0,r,2)
  s<-0
  t<-0
  for (i in 1:r){
  mues1<-rnorm(20,mean=5,sd=3)
  mues2<-rnorm(20,mean=5,sd=3)
  mues3<-rnorm(20,mean=5,sd=3)
  mues<-c(mues1,mues2,mues3)
  resul[i,1]<-bartlett.test(mues,grupos)$p.value
  resul[i,2]<-levene.test(mues,grupos)$p.value
  if (0.05<resul[i,1]) 
  {s<-s+1} 
  if (0.05<resul[i,2]) 
  {t<-t+1} 
    }
  prop1<-s/r
  prop2<-t/r
  return(list(r1=prop1,r2=prop2))
}

library(lawstat)
compa(100)
```
#practica //////clases
```
set.seed(10)
mues1<-rnorm(30,mean=3,sd=1)


set.seed(20)
mues2<-rnorm(40,mean=3,sd=2)


set.seed(30)
mues3<-rnorm(50,mean=3,sd=2)

grupos<-rep(1:3,c(30,40,50))
mues<-c(mues1,mues2,mues3)
datos<-cbind(grupos,mues)
bartlett.test(datos[,2],datos[,1])
```
###Bartlett's K-squared = 25.8957, df = 2, p-value = 2.381e-06
```
1-qchisq(25.8957,2)

--In qchisq(25.8957, 2) : Se han producido NaNs

--tiene que coincidir y si coinciden
```
##prueba de Hipotesis para la comparación de varianzas
##de K muestras
```
ni<-as.vector(by(datos[,2],datos[,1],length))
ni
si<-as.vector(by(datos[,2],datos[,1],sd))
si
sd2i<-as.vector(by(datos[,2],datos[,1],var))
sd2i
n1<-sum(ni*si)
d1<-sum(ni*sd2i)
T3<-2*sum(ni*sd2i*(1/si-n1/d1)^2)  ###Estadistico T3

```
##funcion prueba de la homogenidad basada en la teoria de la Información
```
phbti<-function(datos){
ni<-as.vector(by(datos[,2],datos[,1],length))
si<-as.vector(by(datos[,2],datos[,1],sd))
sd2i<-as.vector(by(datos[,2],datos[,1],var))
k<-length(ni)
n1<-sum(ni*si)
d1<-sum(ni*sd2i)
T3<-2*sum(ni*sd2i*(1/si-n1/d1)^2)  ###Estadistico T3
pvalor<-1-pchisq(T3,k-1)
return(list(estad=T3,pvalor=pvalor))
}

phbti(datos)

```
