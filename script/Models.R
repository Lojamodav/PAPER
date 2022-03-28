

##### GAM MODELS #####

#This script present the gam models used for explain the variability of L50 of European Hake
#for each sex, males and females, considerating as explanatory variables: 
#environmental biological and temporal factor. The procedure step used  to is backward.


#### GAM MODELS FOR MALE HAKES ####

L50male<-as.data.frame(cbind(Year,AMO,NAO,SST,BIO,ssb_ln_t,Knm,L50m))
colnames(L50male)<-c("Year","AMO","NAO","SST","BIO","ssb_ln_t","Knm","L50m")

# Procedure step backward 

m<-gam(L50m ~s(Year,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(SST,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(BIO,bs="cr",k=6)+s(Knm,bs="cr",k=6),method="REML",data=L50male) #Remove biomass

m<-gam(L50m ~s(Year,bs="cr",k=4)+s(NAO,bs="cr",k=4)+s(ssb_ln_t,bs="cr",k=4)+s(Knm,bs="cr",k=4),method="REML",data=L50male) #Remove SST

m<-gam(L50m~s(Year,bs="cr",k=4)+s(NAO,bs="cr",k=4)+s(ssb_ln_t,bs="cr",k=4),method="REML",data=L50male) #Remove Kn

m<-gam(L50m~s(Year,bs="cr",k=4)+s(NAO,bs="cr",k=4),method="REML",data=L50male) #Remove ssb_ln

m<-gam(L50m~s(Year,bs="cr",k=4),method="REML",data=L50male) 

# summary model

summary(m)

# Residual testing

gam.check(m)

jarque.bera.test(resid(m))
shapiro.test(resid(m))
adfTest(resid(m))
kpss.test(resid(m))
Box.test(resid(m),lag=10)
t.test(resid(m),mu=0)
bptest(m,varformula = ~Year, data=L50male)

# Graphic test residuals

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

# Plot:

#Estimated smoother effect of year for the GAM size at first sexual maturity (L_50)for males 
#of the European hake for the time-series (1982-2019). 

plot(L50m ~ Year,data=L50male,lwd=1,col="blue",xlab="Year",ylab="Size at first maturity")
itsadug::plot_smooth(m ,view=("Year"), add=TRUE, col="lightblue", rug=FALSE, print.summary = FALSE)



#### GAM MODELS FOR FEMALE HAKES  ####

L50female<-as.data.frame(cbind(Year,AMO,NAO,SST,BIO,ssb_ln_t,Knh,L50f)); L50female
colnames(L50female)<-c("Year","AMO","NAO","SST","BIO","ssb_ln_t","Kn","L50f")

# Procedure step backward 

m<-gam(L50f~s(Year,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(SST,bs="cr",k=6)+s(BIO,k=6,bs="cr")+s(Kn,k=6,bs="cr"),method="REML",data=L50female)#Remove SST

m<-gam(L50f~s(Year,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(BIO,k=6,bs="cr")+s(Kn,k=6,bs="cr"),method="REML",data=L50female)#Remove Kn

m<-gam(L50f~s(Year,bs="cr",k=7)+s(ssb_ln_t,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(BIO,k=7,bs="cr"),method="REML",data=L50female)

# Summary model

summary(m)

# Residual testing

gam.check(m)


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

#save

getwd()
setwd("C:/Users/Usuario/Desktop/paper/PAPER/gráficos")
ggsave("Knh.jpeg",scale = 1,dpi=300)


#Plot

#Estimated smooth effects of year, biomass, spawning biomass at length, and the NAO for the GAM size at first sexual maturity (L_50)
#for females of the European hake for the time-series (1982-2019). 

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



