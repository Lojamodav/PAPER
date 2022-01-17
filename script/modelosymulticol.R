
#cargamos librería

rqrd <- c( "sizeMat","FSA","FSAdata","captioner","knitr","dplyr","magrittr", "readxl", "sciplot", "Publish","GGally","Hmisc","tidymv","PerformanceAnalytics")
library(readxl)
install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)
library(FSA)
library(TSA)
library(sizeMat)
library(FSAdata)
library(dplyr)
library(tidyverse)
library(magrittr)
library(tseries)
library(lattice)
library(Publish)
library(sciplot)
# setup figure, table, and equation captioning
library(captioner)
library(multcomp)
library(car)
library(knitr)
library(gplots)
library(zoo)
library(fpp2) 
library(tseries)
library(imputeTS)
library(forecast)
library(mgcv)
library(lmtest)
library(ggplot2)
library(gplots)
library(lattice)
library(tidymv)
library(itsadug)
library(ggplot2)
library(GGally)
library(Hmisc)
library(corrplot)
library(fUnitRoots)
library(itsadug)
library(FitAR)


setwd("C:/Users/Usuario/Desktop/TFM2/DATOS/Datos procesados")

## cargamos datos
datos<-read.csv("Merluzo.csv",header=TRUE)
datos<-datos[,-1]

#realizamos modificaciones en la base de datos de merluza
#o, load("C:/Users/Usuario/Desktop/TFM2/DATOS/Datos procesados/datos.RData")

str(datos)
#eliminamos merluza con sexo desconocido
datos<-subset(datos,datos$SEXO!=3)
#definimos niveles de sexo macho y hembra
datos$SEXO <- factor(datos$SEXO, 
                     labels = c("Macho", "Hembra"))
#maduro e inmaduro
datos$Madurez<-factor(datos$Madurez,
                      labels=c("Inmaduro","Maduro"))
# año como factor
datos$ANO<-as.factor(datos$ANO)
#eliminamos los que sean de sexo desconocido
datos <- datos %>%
  filterD(SEXO!="Desconocido") 


#########################################################################################
##################################VARIABLES INDEPENDIENTES###############################
########################################################################################

#AMO
setwd("C:/Users/Usuario/Desktop/TFM2/DATOS/Datos procesados")
AMO<-read.csv("AMO.csv")
media<-apply(AMO[,c(3:14)], 1, mean)
AMO<-as.data.frame(cbind(AMO,media))
AMO<-subset(AMO, Ano>=1982)
AMO<-AMO[,15]

setwd("C:/Users/Usuario/Desktop/TFM2/DATOS/Datos procesados")
#NAO
NAO<-read.csv("NAO.csv")
NAO<-NAO[,-1]
NAO<-subset(NAO, Ano>=1982)
NAO<-NAO[,"Mean"]

#sst
setwd("C:/Users/Usuario/Desktop/TFM2/DATOS/Datos procesados")
SST<-read.csv("SST8c9a_año.csv")
SST<-SST[,4]

setwd("C:/Users/Usuario/Desktop/TFM2/DATOS/Datos procesados")
#ssb
ssb_yr<-read.csv("ssb_yr.csv")
BIO<-ssb_yr[,3]



#ssb_len
setwd("C:/Users/Usuario/Desktop/TFM2/DATOS/Datos procesados")
hkelen<-read.csv("hkelen_nueva.csv")
Hkelen<-subset(hkelen,hkelen$len>19 & hkelen$len<36)
Hkelen1<-tapply(Hkelen$nPop,list(Hkelen$year,Hkelen$step),mean)
ssb_ln<-apply(Hkelen1, 1, mean)
ssb_ln<-as.data.frame(ssb_ln)
ssb_ln<-ssb_ln[,1]

L50macho_a_amb
#ssb_age

hkage<-read.csv("hkeAge.csv")
hkage<-subset(hkage,hkage$age>0 & hkage$age<3)
hkage<-tapply(hkage$nPop,list(hkage$year,hkage$step),mean)
hkage<-apply(hkage,1,mean)
ssb_age<-as.data.frame(hkage)
ssb_age<-ssb_age[,1]

#Kn
Knm<-c(0.81,0.61, 0.70,0.55, 0.71, 0.74, 0.39, 0.49, 0.62, 0.63,0.48, 0.37, 0.52, 0.61, 0.53, 0.57,NA,NA,NA,NA,0.74, 0.69, 0.67, 0.58,0.65, 0.64, 0.71, 0.71,0.68, 0.64, 0.55, 0.47, 0.87, 0.41,0.61, 0.71, 0.79, 0.57)
Knh<-na.ma(Knm) #reemplazamos valores faltantes con medias móviles
Knh<- c(1.77,0.80,1.05,0.92, 1.21,1.59, 0.51,0.85,0.77, 0.99, 0.41, 0.35,0.62, 0.85, 1.02,1.10, 2.48,NA,NA,NA, 1.55, 1.40, 1.32, 1.06,0.94, 1.28, 1.11, 1.03, 0.90, 0.71, 1.13, 1.08,1.30, 2.02, 1.04, 1.24,1.55, 1.65)
Knh<-na.ma(Knh) #reemplazamos valores faltantes con medias móviles



####### SUAVIZACIÓN (TSCLEAN) Y REEMPLAZAMIENTO  DE ATÍPICOS ######




AMO<-tsclean(AMO)
boxplot(AMO)
tsoutliers::tso(ts(AMO,start=1982))
plot(tsoutliers::tso(ts(AMO,start=1982)))
plot(AMO,type="line")

NAO<-tsclean(NAO)
boxplot(NAO)
tsoutliers::tso(ts(NAO,start=1982))
plot(NAO,type="line")

SST<-tsclean(SST)
boxplot(SST)
tsoutliers::tso(ts(SST,start=1982))
plot(SST,type="line")

BIO<-tsclean(BIO)
boxplot(BIO)#se detectan  atípicos 
boxplot.stats(BIO)$out
tsoutliers::tso(ts(BIO,start=1982),maxit.iloop = 3)
plot(tsoutliers::tso(ts(BIO,start=1982),maxit.iloop = 3))
#boxplot identifica más outliers (los 6 primeros valores) que tso, 
#tso identifica uno en la posición 7 y otro en la 10.
#se hace un plot de la serie bio para dejar la decisión de reemplazar atípico 
#a juicio del investigador/a
plot(ts(BIO,start=1982))
#no se realiza ninguna modificación


ssb_age<-tsclean(ssb_age)
boxplot(ssb_age)
boxplot.stats(ssb_age)$out #atípico en la posición 27
tsoutliers::tso(ts(ssb_age,start=1982))#atípico en la posición 14 y 27
plot(tsoutliers::tso(ts(ssb_age,start=1982)))
#dado que tanto boxplot como tsoutliers detectan atípico en la posición 27 se procede
#a reemplazar el mismo a través de medias móviles
plot(ssb_age,type="line")

ssb_age_t<-ssb_age
ssb_age_t[27]<-ssb_age_t[NA]
ssb_age_t<-na.ma(ssb_age_t)
boxplot.stats(ssb_age_t) #una vez se reemplaza el atípico, boxplot no detecta ningún
#outro outlier pero tso sí en la posición 14, se ilustra a continuación
plot(tsoutliers::tso(ts(ssb_age_t,start=1982)))
#sin embargo, una vez se  reemplaza el atípico detectado por tso en la posición 14, boxplot
#detecta otro atípico en la posición 28, por lo que se decide sólo reemplazar el 
#outlier de la posición 27 y dejar la serie como en el paso que nos precede como ssb_age_t
plot(ssb_age_t,type="line")


boxplot.stats(ssb_ln)$out #detecta un atípico en la 27
(tsoutliers::tso(ts(ssb_ln,start=1982))) #se detecta un atípico en la pos 14 y otro en la 27
tsoutliers::tso(ts(ssb_ln,start=1982))
#al igual que con ssb_age, se reemplaza el outlier detectado tanto por boxplot como por tso de la posición 27
ssb_ln_t<-ssb_ln
ssb_ln_t[27]<-ssb_ln_t[NA]
ssb_ln_t<-na.ma(ssb_ln_t)
boxplot(ssb_ln_t)
plot(tsoutliers::tso(ts(ssb_ln_t,start=1982)))
#se da el mismo caso que para ssb_age, una vez se reemplaza el atípico en la posición 27,
#boxplot deja de detectar atípics, sin embargo tso vuelve a detectar el de la posición 14.
#non obstante, se reemplaza sólo el de la posición 27 sin reemplazar el de la posición 14, al igual que la serie anterior.

Knm<-tsclean(Knm)
boxplot(Knm)
tsoutliers::tso(ts(Knm,start=1982))

Knh<-tsclean(Knh)
boxplot(Knh)
tsoutliers::tso(ts(Knh,start=1982))#atípico en la posición 14 y 27
plot(tsoutliers::tso(ts(Knh,start=1982)))
Knh_t<-Knh
Knh_t[17]<-Knh_t[NA]
Knh_t<-na.ma(Knh_t)
boxplot(Knh_t)
plot(tsoutliers::tso(ts(Knh_t,start=1982)))
plot(Knh_t,type="line")


year<-c(1982:2019)



####################################################################################################
############################### COMPROBACIÓN DE MULTICOLINEALIDAD ##################################
###################################################################################################


#machos con ssb_age

dfma<-as.data.frame(cbind(year,AMO,NAO,SST,Knm,BIO,ssb_age_t))
library(corrplot)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(dfma)
p.mat
cor(dfma)
knitr::kable(as.data.frame(cor(df[,-1])))

par(mfrow=c(1,2))
corrplot(round(cor(as.matrix(dfma),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
correlacion<-round(cor(df[,-1]), 2)
chart.Correlation(dfma, histogram = T, pch = 19,col="blue")

setwd("C:/Users/Usuario/Desktop/TFM2/FUNCIONES")
source("HighstatLib.R")
corvif(dfma)
corvif(dfma[,-2])



#machos con ssb_ln

dfma<-as.data.frame(cbind(year,AMO,NAO,SST,Knm,BIO,ssb_ln_t))
library(corrplot)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(dfma)
p.mat
cor(dfma)
knitr::kable(as.data.frame(cor(df[,-1])))

par(mfrow=c(1,2))
corrplot(round(cor(as.matrix(dfma),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
correlacion<-round(cor(df[,-1]), 2)
chart.Correlation(dfma, histogram = T, pch = 19,col="blue")

setwd("C:/Users/Usuario/Desktop/TFM2/FUNCIONES")
source("HighstatLib.R")
corvif(dfma)
corvif(dfma[,-2])


#hembras con ssb_age

dfha<-as.data.frame(cbind(year,AMO,NAO,SST,Knh,BIO,ssb_age_t))
library(corrplot)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(dfha)
p.mat

par(mfrow=c(1,2))
corrplot(round(cor(as.matrix(dfha),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
correlacion<-round(cor(dfha), 2)
chart.Correlation(dfha, histogram = T, pch = 19,col="blue")

setwd("C:/Users/Usuario/Desktop/TFM2/FUNCIONES")
source("HighstatLib.R")
corvif(dfha)
corvif(dfha[,-2])



#hembras con ssb_ln

dfha<-as.data.frame(cbind(year,AMO,NAO,SST,Knh,BIO,ssb_ln_t))
library(corrplot)
cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

p.mat <- cor.mtest(dfha)
p.mat

par(mfrow=c(1,2))
corrplot(round(cor(as.matrix(dfha),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
correlacion<-round(cor(dfha), 2)
chart.Correlation(dfha, histogram = T, pch = 19,col="blue")

setwd("C:/Users/Usuario/Desktop/TFM2/FUNCIONES")
source("HighstatLib.R")
corvif(dfha)
corvif(dfha[,-2])


#######################################################################################
###################################variables dependientes##############################
#######################################################################################



#L50 MACHO

L50macho<-c(36.1,32.6,35.7,35.2,34.7,38.9,33.4,34.6,31.5,29.9,29.7,33.6,31.2,31.1,35.8,34.6,36.8,26.7,31.6,39,33.4,31.1,28.6,31.4,32.7,29.4,30.6,24.2,33.1,25.6,24.1,24.7,NA,NA,27.9,26.5,27.2,23.2)
L50macho<-tsclean(L50macho)
boxplot(L50macho)
tsoutliers::tso(ts(L50macho,start=1982))

#L50 HEMBRA
L50h<-c(49.7, 44.5, 40.5, 44.8, 44.6, 40.5, 39.2, 41.6, 41.8, 42.3, 43.3, 43.5, 46.1 ,46.3,45.1, 52.3, 51.6, 47.3 ,43.4, 46.0, 46.5 ,45.8, 45.0, 44.3,47.0, 45.6, 46.6, 49.9,45.7, 45.3, 40.9, 45.7, 39.0, 46.5, 33.1, 38.3, 41.8, 38.8)

#tanto boxplot como tso identifican un outlier en la posición 15 y se procede a reemplazarlo
boxplot(L50h)
boxplot.stats(L50h)
tsoutliers::tso(ts(L50h,start=1982))
plot(tsoutliers::tso(ts(L50h,start=1982)))
#imputación valor de la posición 35 
L50ht<-L50h
L50ht[35]<-NA
L50ht<-na.ma(L50ht)
#una vez se reemplaza el atípico en la posición 35, no se detectan más atípicos






##################################################################################
##################################################################################

##                       modelos                                                ##
##################################################################################
##################################################################################






################################
#modelo machos con ssb_age
#################################

L50ma<-as.data.frame(cbind(year,AMO,NAO,SST,BIO,ssb_age_t,Knm,L50macho))
colnames(L50ma)<-c("year","AMO","NAO","SST","BIO","ssb_age_t","Knm","L50m")
#backward

m<-gam(L50m~s(year,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(SST,bs="cr",k=6)+s(ssb_age_t,bs="cr",k=6)+s(BIO,bs="cr",k=6)+s(Knm,bs="cr",k=6),method="REML",data=L50m) #eliminmos bio

m<-gam(L50m~s(year,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(SST,bs="cr",k=7)+s(ssb_age_t,bs="cr",k=7)+s(Knm,bs="cr",k=7),method="REML",data=L50m) #eliminmos sst

m<-gam(L50m~s(year,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(ssb_age_t,bs="cr",k=7),method="REML",data=L50m) #eliminmos knm

m<-gam(L50m~s(year,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(SST,bs="cr",k=7),method="REML",data=L50m) #eliminmos ssb

m<-gam(L50m~s(year,bs="cr",k=4)+s(NAO,bs="cr",k=4),method="REML",data=L50m) #eliminmos nao

m<-gam(L50m~s(year,bs="cr",k=4),method="REML",data=L50m) 



summary(m)
gam.check(m)

#residuos test

jarque.bera.test(resid(m))
shapiro.test(resid(m))
adfTest(resid(m))
kpss.test(resid(m))
Box.test(resid(m),lag=10)
t.test(resid(m),mu=0)
bptest(m,varformula = ~year, data=L50m)

#residuos test gráficos
x<-c(1,3,2,4)
n<-matrix(x,ncol=2)
layout(n)
plot(LjungBoxTest(resid(m))[,3],ylim=c(0,1),col="black",ylab="P-valor",xlab="Retardo")
abline(h=0.05,lty=2,col="blue")
res<-(resid(m))
resp<-predict(m)
plot(res~resp,type="p",xlab="Predichos",ylab="Residuos")
abline(h=0,lty=2,col="blue")
acf(resid(m),lag.max=38, main="ACF")
pacf(resid(m),lag.max =38, main="PACF")


#Representación gráfica
  

plot(L50m ~ year,data=L50m,lwd=1,col="blue",xlab="Año",ylab="L50")
itsadug::plot_smooth(m ,view=("year"), add=TRUE, col="lightblue", rug=FALSE, print.summary = FALSE)



################################
#modelo machos con ssb_ln
#################################

L50mb<-as.data.frame(cbind(year,AMO,NAO,SST,BIO,ssb_ln_t,Knm,L50macho))
colnames(L50mb)<-c("year","AMO","NAO","SST","BIO","ssb_ln_t","Knm","L50m")

#procedimiento backward 

m<-gam(L50m ~s(year,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(SST,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(BIO,bs="cr",k=6)+s(Knm,bs="cr",k=6),method="REML",data=L50mb) #eliminmos bio

m<-gam(L50m ~s(year,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(Knm,bs="cr",k=6),method="REML",data=L50mb) #eliminmos sst

m<-gam(L50m~s(year,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6),method="REML",data=L50mb) #eliminmos knm

m<-gam(L50m~s(year,bs="cr",k=7)+s(NAO,bs="cr",k=7),method="REML",data=L50mb) #eliminmos ssb_ln

m<-gam(L50m~s(year,bs="cr",k=5),method="REML",data=L50mb) #eliminamos sst


#llegamos al mismo modelo que con ssb_age, como se ha visto anteriormente,
#este modelo pasa las hipótesis de los residuod, con lo cual, este modelo es válido
#no se vuelve a realizar los test y los gráficos para chequear las hipótesis








#######################################################
### Talla de primera madurez para las merluzas hembra
#######################################################


L50ha<-as.data.frame(cbind(year,AMO,NAO,SST,BIO,ssb_age_t,Knh,L50ht)); L50ha
colnames(L50ha)<-c("year","AMO","NAO","SST","BIO","ssb_age_t","Kn","L50hembra")

################################
#modelo hembra con ssb_age
#################################

#procedimiento backward

m<-gam(L50hembra~s(year,bs="cr",k=7)+s(ssb_age_t,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(SST,bs="cr",k=7)+s(BIO,k=7,bs="cr")+s(Kn,k=7,bs="cr"),method="REML",data=L50ha)#eliminamos sst

m<-gam(L50hembra~s(year,bs="cr",k=7)+s(ssb_age_t,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(BIO,k=7,bs="cr")+s(Kn,k=7,bs="cr"),method="REML",data=L50ha)#eliminamos kn

m1<-gam(L50hembra~s(year,bs="cr",k=7)+s(ssb_age_t,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(BIO,k=7,bs="cr"),method="REML",data=L50ha)#eliminamos kn


summary(m1)
gam.check(m)

#análisis residuos

jarque.bera.test(resid(m))
shapiro.test(resid(m))
Box.test(resid(m),lag=10)
adfTest(resid(m))
kpss.test(resid(m))
t.test(resid(m),mu=0)
bptest(m,varformula = ~year+ssb_age_t+NAO+BIO,data=L50ha) #corregir

#análisis gráfico

x<-c(1,3,2,4)
n<-matrix(x,ncol=2)
layout(n)
plot(LjungBoxTest(resid(m))[,3],ylim=c(0,1),col="black",ylab="P-valor",xlab="Retardo")
abline(h=0.05,lty=2,col="blue")
res<-(resid(m))
resp<-predict(m)
plot(res~resp,type="p",xlab="Predichos",ylab="Residuos")
abline(h=0,col="blue",lty=2)
acf(resid(m),lag.max=38, main="ACF")
pacf(resid(m),lag.max =38, main="PACF")
par(mfrow=c(2,2))



par(mfrow=c(2,2))

plot(L50hembra~year,data=L50ha,lwd=1,type="p",col="darkgreen",xlab="Año", ylab="Talla de primera madurez")
itsadug::plot_smooth(m,view = c("year"),add=TRUE,col="lightgreen",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)

plot(L50hembra~ssb_age_t,data=L50ha,lwd=1,type="p",col="darkgreen",xlab="ssb_age_t", ylab="Talla de primera madurez")
itsadug::plot_smooth(m,view = c("ssb_age_t"),add=TRUE,col="lightgreen",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50hembra~BIO,data=L50ha,lwd=1,type="p",col="darkgreen",xlab="BIO", ylab="Talla de primera madurez")
itsadug::plot_smooth(m,view = c("BIO"),add=TRUE,col="lightgreen",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)

plot(L50hembra~NAO,data=L50ha,lwd=1,type="p",col="darkgreen",xlab="NAO", ylab="Talla de primera madurez")
itsadug::plot_smooth(m,view = c("NAO"),add=TRUE,col="lightgreen",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50ha$L50hembra,  type = "b", ylab = "Talla de primera madurez",col="darkgreen")
lines(predict(m),col="lightgreen",lwd=2)


################################
#modelo hembra con ssb_ln
#################################


L50ha<-as.data.frame(cbind(year,AMO,NAO,SST,BIO,ssb_ln_t,Knh,L50ht)); L50ha
colnames(L50ha)<-c("year","AMO","NAO","SST","BIO","ssb_ln_t","Kn","L50hembra")

#procedimiento backward


m<-gam(L50hembra~s(year,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(SST,bs="cr",k=6)+s(BIO,k=6,bs="cr")+s(Kn,k=6,bs="cr"),method="REML",data=L50ha)#eliminamos sst

m<-gam(L50hembra~s(year,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(BIO,k=6,bs="cr")+s(Kn,k=6,bs="cr"),method="REML",data=L50ha)#eliminamos kn

m2<-gam(L50hembra~s(year,bs="cr",k=7)+s(ssb_ln_t,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(BIO,k=7,bs="cr"),method="REML",data=L50ha)



summary(m)
gam.check(m)

jarque.bera.test(resid(m))
shapiro.test(resid(m))
Box.test(resid(m),lag=10)
adfTest(resid(m))
kpss.test(resid(m))
t.test(resid(m),mu=0)
bptest(m,varformula = ~year+ssb_ln_t+NAO+BIO,data=L50ha)

x<-c(1,3,2,4)
n<-matrix(x,ncol=2)
layout(n)
plot(LjungBoxTest(resid(m))[,3],ylim=c(0,1),col="black",ylab="P-valor",xlab="Retardo")
abline(h=0.05,lty=2,col="blue")
res<-(resid(m))
resp<-predict(m)
plot(res~resp,type="p",xlab="Predichos",ylab="Residuos")
abline(h=0,col="blue",lty=2)
acf(resid(m),lag.max=38, main="ACF")
pacf(resid(m),lag.max =38, main="PACF")
par(mfrow=c(2,2))
gam.check(m)


layout(matrix(c(1,3,2,3), ncol = 2))

plot(L50h~year,data=L50h,lwd=1,type="p",col="darkgreen",xlab="Año", ylab="Talla de primera madurez")
itsadug::plot_smooth(m,view = c("year"),add=TRUE,col="lightgreen",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50h~ssb_ln_t,data=L50h,lwd=1,type="p",col="darkgreen",xlab="ssb_age", ylab="Talla de primera madurez")
itsadug::plot_smooth(m,view = c("ssb_ln_t"),add=TRUE,col="lightgreen",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50h~NAO,data=L50h,lwd=1,type="p",col="darkgreen",xlab="NAO", ylab="Talla de primera madurez")
itsadug::plot_smooth(m,view = c("NAO"),add=TRUE,col="lightgreen",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50h~BIO,data=L50h,lwd=1,type="p",col="darkgreen",xlab="BIO", ylab="Talla de primera madurez")
itsadug::plot_smooth(m,view = c("BIO"),add=TRUE,col="lightgreen",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)

plot(L50h$L50h,  type = "b", ylab = "Talla de primera madurez",col="darkgreen")
lines(predict(m),col="lightgreen",lwd=2)


###Análisis sensibilidad
AIC(m1,m2) #nos quedamos con el modelo de ssb_ln_t (el último)
