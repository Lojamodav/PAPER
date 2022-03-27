
##################################################################################
##################################################################################

##                       GAM MODELS                                             ##
##################################################################################
##################################################################################


##GAM MODELS FOR MALE HAKES

################################
#GAM models for  male hakes with  SSB_age
#################################

L50male<-as.data.frame(cbind(Year,AMO,NAO,SST,BIO,ssb_age_t,Knm,L50_Male))
colnames(L50male)<-c("Year","AMO","NAO","SST","BIO","ssb_age_t","Knm","L50m")

#Procedure step backward 


m<-gam(L50m~s(Year,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(SST,bs="cr",k=6)+s(ssb_age_t,bs="cr",k=6)+s(BIO,bs="cr",k=6)+s(Knm,bs="cr",k=6),method="REML",data=L50male) #Remove BIO

m<-gam(L50m~s(Year,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(SST,bs="cr",k=7)+s(ssb_age_t,bs="cr",k=7)+s(Knm,bs="cr",k=7),method="REML",data=L50male) #Remove sst

m<-gam(L50m~s(Year,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(ssb_age_t,bs="cr",k=7),method="REML",data=L50male) #Remove Knm

m<-gam(L50m~s(Year,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(SST,bs="cr",k=7),method="REML",data=L50male) #Remove SSB

m<-gam(L50m~s(Year,bs="cr",k=4)+s(NAO,bs="cr",k=4),method="REML",data=L50ma) #Remove NAO

m<-gam(L50m~s(Year,bs="cr",k=4),method="REML",data=L50male) 



summary(m)
gam.check(m)

#Residual testing

jarque.bera.test(resid(m))
shapiro.test(resid(m))
adfTest(resid(m))
kpss.test(resid(m))
Box.test(resid(m),lag=10)
t.test(resid(m),mu=0)
bptest(m,varformula = ~Year, data=L50male)

#Graphic test residuals.
x<-c(1,3,2,4)
n<-matrix(x,ncol=2)
layout(n)
plot(LjungBoxTest(resid(m))[,3],ylim=c(0,1),col="black",ylab="P-valor",xlab="Lag")
abline(h=0.05,lty=2,col="blue")
res<-(resid(m))
resp<-predict(m)
plot(res~resp,type="p",xlab="Predicted",ylab="Residuals")
abline(h=0,lty=2,col="blue")
acf(resid(m),lag.max=38, main="ACF")
pacf(resid(m),lag.max =38, main="PACF")


#Graphics


plot(L50m ~ Year,data=L50male,lwd=1,col="blue",xlab="Year",ylab="Size at first maturity")
itsadug::plot_smooth(m ,view=("Year"), add=TRUE, col="lightblue", rug=FALSE, print.summary = FALSE)



################################
#GAM models for  male hakes with SSB_Length
#################################

L50male<-as.data.frame(cbind(Year,AMO,NAO,SST,BIO,ssb_ln_t,Knm,L50m))
colnames(L50male)<-c("Year","AMO","NAO","SST","BIO","ssb_ln_t","Knm","L50m")

#Procedure step backward 

m<-gam(L50m ~s(Year,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(SST,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(BIO,bs="cr",k=6)+s(Knm,bs="cr",k=6),method="REML",data=L50male) #Remove biomass

m<-gam(L50m ~s(Year,bs="cr",k=4)+s(NAO,bs="cr",k=4)+s(ssb_ln_t,bs="cr",k=4)+s(Knm,bs="cr",k=4),method="REML",data=L50male) #Remove SST

m<-gam(L50m~s(Year,bs="cr",k=4)+s(NAO,bs="cr",k=4)+s(ssb_ln_t,bs="cr",k=4),method="REML",data=L50male) #Remove Kn

m<-gam(L50m~s(Year,bs="cr",k=4)+s(NAO,bs="cr",k=4),method="REML",data=L50male) #Remove ssb_ln

m<-gam(L50m~s(Year,bs="cr",k=4),method="REML",data=L50male) 


#We arrive at the same model as with ssb_age. 
#As checked above, this model passes the hypotheses on the residuals, 
#so this model is valid. That said, the basic assumptions on the residuals are not re-tested.

summary(m)
AIC(m)

#######################################################
### ##GAM MODELS FOR FEMALE HAKES
#######################################################


L50female<-as.data.frame(cbind(Year,AMO,NAO,SST,BIO,ssb_age_t,Knh,L50f)); L50female
colnames(L50female)<-c("Year","AMO","NAO","SST","BIO","ssb_age_t","Kn","L50f")

################################
#GAM models for  female hakes with  SSB_age
#################################

#Procedure step backward 

m<-gam(L50f~s(Year,bs="cr",k=7)+s(ssb_age_t,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(SST,bs="cr",k=7)+s(BIO,k=7,bs="cr")+s(Kn,k=7,bs="cr"),method="REML",data=L50female)#Remove SST

m<-gam(L50f~s(Year,bs="cr",k=7)+s(ssb_age_t,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(BIO,k=7,bs="cr")+s(Kn,k=7,bs="cr"),method="REML",data=L50female)#Remove Kn

m<-gam(L50f~s(Year,bs="cr",k=7)+s(ssb_age_t,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(BIO,k=7,bs="cr"),method="REML",data=L50female)


summary(m1)
gam.check(m)

#Residual testing

jarque.bera.test(resid(m))
shapiro.test(resid(m))
Box.test(resid(m),lag=10)
adfTest(resid(m))
kpss.test(resid(m))
t.test(resid(m),mu=0)
bptest(m,varformula = ~Year+ssb_age_t+NAO+BIO,data=L50female) #corregir

#Graphic residual testing

x<-c(1,3,2,4)
n<-matrix(x,ncol=2)
layout(n)
plot(LjungBoxTest(resid(m))[,3],ylim=c(0,1),col="black",ylab="P-value",xlab="Lag")
abline(h=0.05,lty=2,col="blue")
res<-(resid(m))
resp<-predict(m)
plot(res~resp,type="p",xlab="Predicted",ylab="Residuals")
abline(h=0,col="blue",lty=2)
acf(resid(m),lag.max=38, main="ACF")
pacf(resid(m),lag.max =38, main="PACF")
par(mfrow=c(2,2))

# Plots of model

par(mfrow=c(2,2))

plot(L50female~Year,data=L50female,lwd=1,type="p",col="darkgreen",xlab="Year", ylab="Size at first maturity")
itsadug::plot_smooth(m,view = c("Year"),add=TRUE,col="lightgreen",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)

plot(L50female~ssb_age_t,data=L50female,lwd=1,type="p",col="darkgreen",xlab="ssb_age_t", ylab="Size at first maturity")
itsadug::plot_smooth(m,view = c("ssb_age_t"),add=TRUE,col="lightgreen",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50female~BIO,data=L50female,lwd=1,type="p",col="darkgreen",xlab="BIO", ylab="Size at first maturity")
itsadug::plot_smooth(m,view = c("BIO"),add=TRUE,col="lightgreen",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)

plot(L50female~NAO,data=L50female,lwd=1,type="p",col="darkgreen",xlab="NAO", ylab="Size at first maturity")
itsadug::plot_smooth(m,view = c("NAO"),add=TRUE,col="lightgreen",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50female$L50f,  type = "b", ylab = "Size at first maturity",col="darkgreen")
lines(predict(m),col="lightgreen",lwd=2)


##GAM MODELS FOR FEMALE HAKES



################################
#Female model with  SSB_length
#################################


L50female<-as.data.frame(cbind(Year,AMO,NAO,SST,BIO,ssb_ln_t,Knh,L50f)); L50female
colnames(L50female)<-c("Year","AMO","NAO","SST","BIO","ssb_ln_t","Kn","L50f")

#Procedure step backward 


m<-gam(L50f~s(Year,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(SST,bs="cr",k=6)+s(BIO,k=6,bs="cr")+s(Kn,k=6,bs="cr"),method="REML",data=L50female)#Remove SST

m<-gam(L50f~s(Year,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(BIO,k=6,bs="cr")+s(Kn,k=6,bs="cr"),method="REML",data=L50female)#Remove Kn

m<-gam(L50f~s(Year,bs="cr",k=7)+s(ssb_ln_t,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(BIO,k=7,bs="cr"),method="REML",data=L50female)



summary(m)
gam.check(m)

#Residual testing

jarque.bera.test(resid(m))
shapiro.test(resid(m))
Box.test(resid(m),lag=10)
adfTest(resid(m))
kpss.test(resid(m))
t.test(resid(m),mu=0)
bptest(m,varformula = ~Year+ssb_ln_t+NAO+BIO,data=L50female)


#Graphic residual testing

x<-c(1,3,2,4)
n<-matrix(x,ncol=2)
layout(n)
plot(LjungBoxTest(resid(m))[,3],ylim=c(0,1),col="black",ylab="P-value",xlab="Lag")
abline(h=0.05,lty=2,col="blue")
res<-(resid(m))
resp<-predict(m)
plot(res~resp,type="p",xlab="Predicted",ylab="Residuals")
abline(h=0,col="blue",lty=2)
acf(resid(m),lag.max=38, main="ACF")
pacf(resid(m),lag.max =38, main="PACF")
par(mfrow=c(2,2))
gam.check(m)

getwd()
setwd("C:/Users/Usuario/Desktop/paper/PAPER/gráficos")
ggsave("Knh.jpeg",scale = 1,dpi=300)


#Plot models

par(mfrow=c(2,2))
plot(L50f~Year,data=L50female,lwd=1,type="p",col=6,xlab="Year", ylab="Size at first maturity")
itsadug::plot_smooth(m,view = c("Year"),add=TRUE,col="lightpink",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50f~ssb_ln_t,data=L50female,lwd=1,type="p",col= 6,xlab="SSB at length", ylab="Size at first maturity")
itsadug::plot_smooth(m,view = c("ssb_ln_t"),add=TRUE,col="lightpink",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50f~NAO,data=L50female,lwd=1,type="p",col=6,xlab="NAO", ylab="Size at first maturity")
itsadug::plot_smooth(m,view = c("NAO"),add=TRUE,col="lightpink",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50f~BIO,data=L50female,lwd=1,type="p",col=6,xlab="Biomass", ylab="Size at first maturity")
itsadug::plot_smooth(m,view = c("BIO"),add=TRUE,col="lightpink",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)

dev.off()

###An?lisis sensibilidad
AIC(m1,m2) #nos quedamos con el modelo de ssb_ln_t (el ?ltimo)

