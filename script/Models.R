
##################################################################################
##################################################################################

##                       GAM MODELS                                             ##
##################################################################################
##################################################################################






################################
#modelo machos con ssb_age
#################################

L50ma<-as.data.frame(cbind(year,AMO,NAO,SST,BIO,ssb_age_t,Knm,L50macho))
colnames(L50ma)<-c("year","AMO","NAO","SST","BIO","ssb_age_t","Knm","L50m")
#backward

m<-gam(L50m~s(year,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(SST,bs="cr",k=6)+s(ssb_age_t,bs="cr",k=6)+s(BIO,bs="cr",k=6)+s(Knm,bs="cr",k=6),method="REML",data=L50ma) #eliminmos bio

m<-gam(L50m~s(year,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(SST,bs="cr",k=7)+s(ssb_age_t,bs="cr",k=7)+s(Knm,bs="cr",k=7),method="REML",data=L50ma) #eliminmos sst

m<-gam(L50m~s(year,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(ssb_age_t,bs="cr",k=7),method="REML",data=L50ma) #eliminmos knm

m<-gam(L50m~s(year,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(SST,bs="cr",k=7),method="REML",data=L50ma) #eliminmos ssb

m<-gam(L50m~s(year,bs="cr",k=4)+s(NAO,bs="cr",k=4),method="REML",data=L50ma) #eliminmos nao

m<-gam(L50m~s(year,bs="cr",k=4),method="REML",data=L50ma) 



summary(m)
gam.check(m)

#test residual 

jarque.bera.test(resid(m))
shapiro.test(resid(m))
adfTest(resid(m))
kpss.test(resid(m))
Box.test(resid(m),lag=10)
t.test(resid(m),mu=0)
bptest(m,varformula = ~year, data=L50ma)

#Graphic test residuals.
x<-c(1,3,2,4)
n<-matrix(x,ncol=2)
layout(n)
plot(LjungBoxTest(resid(m))[,3],ylim=c(0,1),col="black",ylab="P-valor",xlab="Retardo")
abline(h=0.05,lty=2,col="blue")
res<-(resid(m))
resp<-predict(m)
plot(res~resp,type="p",xlab="Predichos",ylab="Residuals")
abline(h=0,lty=2,col="blue")
acf(resid(m),lag.max=38, main="ACF")
pacf(resid(m),lag.max =38, main="PACF")


#Graphics


plot(L50m ~ year,data=L50ma,lwd=1,col="blue",xlab="Year",ylab="Size at first maturity")
itsadug::plot_smooth(m ,view=("year"), add=TRUE, col="lightblue", rug=FALSE, print.summary = FALSE)



################################
#modelo machos con ssb_ln
#################################

L50mb<-as.data.frame(cbind(year,AMO,NAO,SST,BIO,ssb_ln_t,Knm,L50macho))
colnames(L50mb)<-c("year","AMO","NAO","SST","BIO","ssb_ln_t","Knm","L50m")

#Step backward 

m<-gam(L50m ~s(year,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(SST,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(BIO,bs="cr",k=6)+s(Knm,bs="cr",k=6),method="REML",data=L50mb) #eliminmos bio

m<-gam(L50m ~s(year,bs="cr",k=4)+s(NAO,bs="cr",k=4)+s(ssb_ln_t,bs="cr",k=4)+s(Knm,bs="cr",k=4),method="REML",data=L50mb) #eliminmos sst

m<-gam(L50m~s(year,bs="cr",k=4)+s(NAO,bs="cr",k=4)+s(ssb_ln_t,bs="cr",k=4),method="REML",data=L50mb) #eliminmos knm

m<-gam(L50m~s(year,bs="cr",k=4)+s(NAO,bs="cr",k=4),method="REML",data=L50mb) #eliminmos ssb_ln

m<-gam(L50m~s(year,bs="cr",k=4),method="REML",data=L50mb) #eliminamos sst


#llegamos al mismo modelo que con ssb_age, como se ha visto anteriormente,
#este modelo pasa las hip?tesis de los residuod, con lo cual, este modelo es v?lido
#no se vuelve a realizar los test y los gr?ficos para chequear las hip?tesis

summary(m)
AIC(m)

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

#an?lisis residuos

jarque.bera.test(resid(m))
shapiro.test(resid(m))
Box.test(resid(m),lag=10)
adfTest(resid(m))
kpss.test(resid(m))
t.test(resid(m),mu=0)
bptest(m,varformula = ~year+ssb_age_t+NAO+BIO,data=L50ha) #corregir

#an?lisis gr?fico

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

plot(L50hembra~year,data=L50ha,lwd=1,type="p",col="darkgreen",xlab="A?o", ylab="Talla de primera madurez")
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

# backward step


m<-gam(L50_Female~s(Year,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(SST,bs="cr",k=6)+s(BIO,k=6,bs="cr")+s(Kn,k=6,bs="cr"),method="REML",data=L50ha)#eliminamos sst

m<-gam(L50hembra~s(Year,bs="cr",k=6)+s(ssb_ln_t,bs="cr",k=6)+s(NAO,bs="cr",k=6)+s(BIO,k=6,bs="cr")+s(Kn,k=6,bs="cr"),method="REML",data=L50ha)#eliminamos kn

m<-gam(L50hembra~s(Year,bs="cr",k=7)+s(ssb_ln_t,bs="cr",k=7)+s(NAO,bs="cr",k=7)+s(BIO,k=7,bs="cr"),method="REML",data=L50ha)



summary(m)
AIC(m)
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

getwd()
setwd("C:/Users/Usuario/Desktop/paper/PAPER/gráficos")
ggsave("Knh.jpeg",scale = 1,dpi=300)

par(mfrow=c(2,2))
plot(L50h~year,data=L50ha,lwd=1,type="p",col=6,xlab="Year", ylab="Size at first maturity")
itsadug::plot_smooth(m,view = c("year"),add=TRUE,col="lightpink",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50h~ssb_ln_t,data=L50ha,lwd=1,type="p",col= 6,xlab="SSB at length", ylab="Size at first maturity")
itsadug::plot_smooth(m,view = c("ssb_ln_t"),add=TRUE,col="lightpink",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50h~NAO,data=L50ha,lwd=1,type="p",col=6,xlab="NAO", ylab="Size at first maturity")
itsadug::plot_smooth(m,view = c("NAO"),add=TRUE,col="lightpink",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)


plot(L50h~BIO,data=L50ha,lwd=1,type="p",col=6,xlab="Biomass", ylab="Size at first maturity")
itsadug::plot_smooth(m,view = c("BIO"),add=TRUE,col="lightpink",rug=FALSE,rm.ranef = TRUE,print.summary = FALSE)

dev.off()

###An?lisis sensibilidad
AIC(m1,m2) #nos quedamos con el modelo de ssb_ln_t (el ?ltimo)

