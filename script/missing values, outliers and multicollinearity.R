
#Load libraries

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

#### Load data ####

### Independent variables

## AMO

setwd("C:/Users/Usuario/Desktop/paper/PAPER/datos")
AMO<-read.csv("AMO.csv")
Mean<-apply(AMO[,c(3:14)], 1, mean)
AMO<-as.data.frame(cbind(AMO,Mean))
AMO<-subset(AMO, Year>=1982)
AMO<-AMO[,15]

## NAO

setwd("C:/Users/Usuario/Desktop/paper/PAPER/datos")
NAO<-read.csv("NAO.csv")
NAO<-NAO[,-1]
NAO<-subset(NAO, Year>=1982)
NAO<-NAO[,"Mean"]

## SST

setwd("C:/Users/Usuario/Desktop/paper/PAPER/datos")
SST<-read.csv("SST.csv")
SST<-SST[,3]

## BIO

setwd("C:/Users/Usuario/Desktop/paper/PAPER/datos")#ssb
ssb_yr<-read.csv("ssb_yr.csv")
BIO<-ssb_yr[,3]

## ssb_length

setwd("C:/Users/Usuario/Desktop/paper/PAPER/datos")
hkelen<-read.csv("hkelen_nueva.csv")
#Filter database
Hkelen<-subset(hkelen,hkelen$len>19 & hkelen$len<36)
Hkelen1<-tapply(Hkelen$nPop,list(Hkelen$year,Hkelen$step),mean)
ssb_ln<-apply(Hkelen1, 1, mean)
ssb_ln<-as.data.frame(ssb_ln)
ssb_ln<-ssb_ln[,1]

## Relative condition factor Kn

# Kn males

Knm<-c(0.81,0.61, 0.70,0.55, 0.71, 0.74, 0.39, 0.49, 0.62, 0.63,0.48, 0.37, 0.52, 0.61, 0.53, 0.57,NA,NA,NA,NA,0.74, 0.69, 0.67, 0.58,0.65, 0.64, 0.71, 0.71,0.68, 0.64, 0.55, 0.47, 0.87, 0.41,0.61, 0.71, 0.79, 0.57)

# Kn females

Knf<- c(1.77,0.80,1.05,0.92, 1.21,1.59, 0.51,0.85,0.77, 0.99, 0.41, 0.35,0.62, 0.85, 1.02,1.10, 2.48,NA,NA,NA, 1.55, 1.40, 1.32, 1.06,0.94, 1.28, 1.11, 1.03, 0.90, 0.71, 1.13, 1.08,1.30, 2.02, 1.04, 1.24,1.55, 1.65)

### Response variables ###

## L50 

# L50 Male

L50_Male<-c(36.1,32.6,35.7,35.2,34.7,38.9,33.4,34.6,31.5,29.9,29.7,33.6,31.2,31.1,35.8,34.6,36.8,26.7,31.6,39,33.4,31.1,28.6,31.4,32.7,29.4,30.6,24.2,33.1,25.6,24.1,24.7,NA,NA,27.9,26.5,27.2,23.2)

# L50 Female

L50_Female<-c(49.7, 44.5, 40.5, 44.8, 44.6, 40.5, 39.2, 41.6, 41.8, 42.3, 43.3, 43.5, 46.1 ,46.3,45.1, 52.3, 51.6, 47.3 ,43.4, 46.0, 46.5 ,45.8, 45.0, 44.3,47.0, 45.6, 46.6, 49.9,45.7, 45.3, 40.9, 45.7, 39.0, 46.5, 33.1, 38.3, 41.8, 38.8)




#### Missing values are replaced #### 

### L50 ###

## L50 male and female

#Replace missing values with moving averages

L50m<-na.ma(L50_Male)
L50f<-na.ma(L50_Female)

## L50 plot for males and females with missing values replaced


tiff("varL50.tiff", units="in",width =10 ,height = 5,res=300,compression  = "jpeg", family="Times")
par(mfrow=c(1,2))
L50m=ts(L50m,start = c(1982,1), end = c(2019,1),frequency = 1)
plot(L50m,xlab="Year",ylab="L50",lwd=1, col=4,main="A")
L50f=ts(L50f,start = c(1982,1), end = c(2019,1),frequency = 1)
plot(L50f, xlab="Year", ylab = "L50", lwd=1, col=6, main="B")
dev.off()



### Kn ###


#Replace missing values with moving averages
Knm<-na.ma(Knm) 
Knf<-na.ma(Knf) 

## Kn plot for males and females with missing values replaced



tiff("varkn.tiff", units="in", width =10 ,height = 5,res=300,compression  = "jpeg", family="Times")
par(mfrow=c(1,2))
fcm=ts(Knm,start = c(1982,1), end = c(2019,1),frequency = 1)
plot(fcm,xlab="Year",ylab="Kn",lwd=1, col=4,main="A")
fcf=ts(Knf,start = c(1982,1), end = c(2019,1),frequency = 1)
plot(fcf, xlab="Year", ylab = "Kn", lwd=1, col=6, main="B")
dev.off()



##### Smoothing (TSCLEAN) and replace outliers values ######

### Independent variables ###

## AMO

#smoothing using tsclean
AMO<-tsclean(AMO) 
#Identification of outliers using boxplot and the tso function (tsoutliers package)
boxplot(AMO) #no outliers were detected
tsoutliers::tso(ts(AMO,start=1982)) #no outliers were detected
plot(tsoutliers::tso(ts(AMO,start=1982))) #no outliers were detected

## NAO

#smoothing using tsclean
NAO<-tsclean(NAO)
#Identification of outliers using boxplot and the tso function (tsoutliers package)
boxplot(NAO) #no outliers were detected
tsoutliers::tso(ts(NAO,start=1982)) #no outliers were detected

## SST

#smoothing using tsclean
SST<-tsclean(SST)
#Identification of outliers using boxplot and the tso function (tsoutliers package)
boxplot(SST) #no outliers were detected
tsoutliers::tso(ts(SST,start=1982)) #no outliers were detected

## BIO

#smoothing using tsclean
BIO<-tsclean(BIO)
#Identification of outliers using boxplot and the tso function (tsoutliers package)
boxplot(BIO) # Outliers were detected in the six first values
boxplot.stats(BIO)$out
tsoutliers::tso(ts(BIO,start=1982),maxit.iloop = 3) # Outliers were detected in the positions 7 and 10
plot(tsoutliers::tso(ts(BIO,start=1982),maxit.iloop = 3))
#As follows, the Bio plot line was made to make the decision on outlier substitution. 
plot(ts(BIO,start=1982)) # Outliers weren´t replace

## SSB_ln

#smoothing using tsclean
ssb_ln<-tsclean(ssb_ln)
#Identification of outliers using boxplot and the tso function (tsoutliers package)
boxplot.stats(ssb_ln)$out # Outlier was detected at position 27
(tsoutliers::tso(ts(ssb_ln,start=1982))) # Outliers were detected at position 14 and 27
tsoutliers::tso(ts(ssb_ln,start=1982))# Outlier at position 27 detected by tso and boxplot was replaced with moving averages
ssb_ln_t<-ssb_ln
ssb_ln_t[27]<-ssb_ln[NA]
ssb_ln_t<-na.ma(ssb_ln_t)

boxplot(ssb_ln_t) # no outliers detected
plot(tsoutliers::tso(ts(ssb_ln_t,start=1982))) # outlier was detected at position 14, 
                                                #but as boxplot does not detect any, 
                                               #the decision is made to replace (with moving averages) only the outlier in position 27.

## Kn

# Kn males

#smoothing using tsclean
Knm<-tsclean(Knm)
#Identification of outliers using boxplot and the tso function (tsoutliers package)
boxplot(Knm) #no outliers were detected
tsoutliers::tso(ts(Knm,start=1982)) #no outliers were detected

# Kn females

#smoothing using tsclean
Knf<-tsclean(Knf)
#Identification of outliers using boxplot and the tso function (tsoutliers package)
boxplot.stats(Knf)$out # no outliers detected
tsoutliers::tso(ts(Knf,start=1982)) ## Outliers were detected in a number of  positions
plot(tsoutliers::tso(ts(Knf,start=1982))
#Outlier at position 17 was replaced with moving average.
#This decision was made because the kn outlier for females detected in position 17 
#corresponds to the value for 1998 where, through an exploratory analysis,
#a sampling bias was detected since for that year there are only 
#15 females with very high weights.
Knf_t<-Knf
Knf_t[17]<-Knf_t[NA]
Knf_t<-na.ma(Knf_t) # moving averages
boxplot(Knf_t)
plot(tsoutliers::tso(ts(Knf_t,start=1982)))
plot(Knf_t,type="line")

### Response variables ###

## L50 Male
L50_Male
L50_Male<-tsclean(L50_Male)
#Identification of outliers using boxplot and the tso function (tsoutliers package)
boxplot(L50_Male) #no outliers were detected
tsoutliers::tso(ts(L50_Male,start=1982)) #no outliers were detected

## L50 Female

L50f<-c(49.7, 44.5, 40.5, 44.8, 44.6, 40.5, 39.2, 41.6, 41.8, 42.3, 43.3, 43.5, 46.1 ,46.3,45.1, 52.3, 51.6, 47.3 ,43.4, 46.0, 46.5 ,45.8, 45.0, 44.3,47.0, 45.6, 46.6, 49.9,45.7, 45.3, 40.9, 45.7, 39.0, 46.5, 33.1, 38.3, 41.8, 38.8)

#Outlier was detected by boxplot and tso at position 15
#Outlier at position 15 was replaced with moving average
boxplot.stats(L50f)
tsoutliers::tso(ts(L50f,start=1982))
plot(tsoutliers::tso(ts(L50f,start=1982)))
#
L50ft<-L50f
L50ft[35]<-NA
L50ft<-na.ma(L50ft) # replace using moving average
                    # no outliers detected


#### Multicollinearity Test ####

## Males with ssb_ln

dfma<-as.data.frame(cbind(Year,AMO,NAO,SST,Knm,BIO,ssb_ln_t))
colnames(dfma)<-c("Year", "AMO", "NAO","SST","Kn","BIO","SSB_length")
write.csv(dfma,"dfma.csv", row.names = FALSE)
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

tiff("cor.tiff", units="in", width= 10, height  =5, res=300, family="Times")
corrplot(round(cor(as.matrix(dfma),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
correlacion<-round(cor(df[,-1]), 2)
dev.off()

chart.Correlation(dfma, histogram = T, pch = 19,col="blue")

setwd("C:/Users/Usuario/Desktop/TFM2/FUNCIONES")
source("HighstatLib.R")
corvif(dfma)
corvif(dfma[,-2])


## Females with ssb_ln

dffa<-as.data.frame(cbind(Year,AMO,NAO,SST,Knf,BIO,ssb_ln_t))
colnames(dffa)<-c("Year", "AMO", "NAO","SST","Kn","BIO","SSB_length")
write.csv(dffa,"dffa.csv", row.names = FALSE)

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

p.mat <- cor.mtest(dffa)
p.mat

tiff("cor2.tiff", units="in", width= 10, height  =5, res=300, family="Times")
corrplot(round(cor(as.matrix(dffa),use="na.or.complete"),2),method="number",type="lower",diag=T,p.mat = p.mat, sig.level = 0.05)
correlacion<-round(cor(dffa), 2)
dev.off()
chart.Correlation(dffa, histogram = T, pch = 19,col="blue")

setwd("C:/Users/Usuario/Desktop/TFM2/FUNCIONES")
source("HighstatLib.R")
corvif(dffa)
corvif(dffa[,-2])

