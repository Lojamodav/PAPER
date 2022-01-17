
### cargamos librerías

install.packages("sizeMat")
library(sizeMat)
library(car)
library(dplyr)
library(FSA)
library(magrittr)
library(readxl)
install.packages("FSAdata")
library(FSAdata)
install.packages("captioner")
library(captioner)
install.packages("multcomp")
library(multcomp)
library(tseries)
library(lattice)
library(gplots)
library(sciplot)
install.packages(mgcv)
library(imputeTS)


#cargamos datos 

setwd("C:/Users/Usuario/Desktop/paper/PAPER/datos")
datos<-read.csv("Merluzo.csv",header=TRUE)
datos<-datos[,-1]
str(datos)

#preparamos los datos

#definimos niveles de sexo y madurez
datos$SEXO <- factor(datos$SEXO, 
                     labels = c("Macho", "Hembra", "Desconocido"))

datos$Madurez<-factor(datos$Madurez,
                      labels=c("Inmaduro","Maduro"))

#filtramos la base de datos en funci?n del per?odo temporal a utilizar
#en este caso, se acord? unidad temporal a?o a partir del 1981, esto es 1982 inclusive
datos<-subset(datos,datos$ANO>1981)

#unidad temporal como factor
datos$ANO<-as.factor(datos$ANO)
datos$MES<-as.factor(datos$MES)

#filtramos base en la que excluya a los individuos de sexo desconocido
datos<-subset(datos,datos$SEXO!="Desconocido")

sum(table(datos$ANO)) #N=26821

sum(table(datos$ANO[datos$SEXO=="Hembra"])) #15124
sum(table(datos$ANO[datos$SEXO=="Macho"])) #11697

##########################################################
# L50  que ser? variable dependiente en el estudio
#############################################################


#calculamos L50 de la poblaci?n de merluza
 

L50pob =gonad_mature(datos, varNames = c("TALLA", "Madurez"),  inmName = "Inmaduro",matName = "Maduro", method = "fq", niter = 999)
print(L50pob)

windows()
plot(L50pob, xlab = "TALLA (cm.)", ylab = "Proporción de maduros", col = c("blue", "red"), onlyOgive = TRUE)

## calculamos L50 para  merluzas macho


machos<-subset(datos,SEXO=="Macho")

L50macho = gonad_mature(machos, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                        matName = "Maduro", method = "fq", niter = 999)
print(L50macho)

plot(L50macho, xlab = "TALLA (cm.)", ylab = "Proporción de machos maduros", col = c("blue", "red"), onlyOgive = TRUE)


## calculamos L50 para merluzas hembra

hembras<-subset(datos,SEXO=="Hembra")

L50hembra = gonad_mature(hembras, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                         matName = "Maduro", method = "fq", niter = 999)
print(L50hembra)

windows()
plot(L50hembra, xlab = "TALLA (cm.)", ylab = "Proporción de hembras maduras", col = c("blue", "red"), onlyOgive = FALSE)



#COMPARACIÓN SEXO

#anova
m1<-glm(Madurez~TALLA*SEXO, data = datos,family= binomial)
drop1(m1,~.,test="Chisq")

cf<-coef(m1);cf

x <- seq(0,100,0.1)
macho <- exp(cf[1]+cf[2]*x)/(1+exp(cf[1]+cf[2]*x))
hembra <- exp(cf[1]+cf[3]+(cf[2]+cf[4])*x)/(1+exp(cf[1]+cf[3]+(cf[2]+cf[4])*x))


plot(macho~x,type="l",lwd=2,xlab="Total Length (cm)",ylab="Proportion Mature")
lines(hembra~x,type="l",lwd=2,col="red")
legend("bottomright",c("Macho","Hembra"),lwd=2,col=c("black","red"))


######################   POR AÑO ##############################################################

#######################################################################################
################################ 1982 #################################################
#######################################################################################


ochentados<-subset(datos,ANO=="1982")

L5082 = gonad_mature(ochentados, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5082, xlab = "TALLA (cm.)", ylab = "Proporción de hembras maduras", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1982

ochentadosm<-subset(datos,ANO=="1982" & SEXO=="Macho")

L5082m = gonad_mature(ochentadosm, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5082m)


plot(L5082m, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"), onlyOgive = TRUE)


#hembra 1982


ochentadosh<-subset(datos,ANO=="1982" & SEXO=="Hembra")

L5082h = gonad_mature(ochentadosh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5082h)

plot(L5082h, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"), onlyOgive = TRUE)



#######################################################################################
################################ 1983 #################################################
#######################################################################################


ochentatres<-subset(datos,ANO=="1983")

L5083 = gonad_mature(ochentatres, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5083, xlab = "TALLA (cm.)", ylab = "Proporción de  maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1983

ochentatresm<-subset(datos,ANO=="1983" & SEXO=="Macho")

L5083m = gonad_mature(ochentatresm, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5083m)


plot(L5083m, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)



#hembra 1983


ochentatresh<-subset(datos,ANO=="1983" & SEXO=="Hembra")


L5083h = gonad_mature(ochentatresh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5083h)

plot(L5083h, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)


########################################################################################
################################ 1984 #################################################
#######################################################################################



ochentacuatro=subset(datos, ANO=="1984")


L5084 = gonad_mature(ochentacuatro, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5084, xlab = "TALLA (cm.)", ylab = "Proporción de  maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1984

ochentacuatrom<-subset(datos,ANO=="1984" & SEXO=="Macho")

L5084m = gonad_mature(ochentacuatrom, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5084m)

plot(L5084m, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)

#hembra 1984

ochentacuatroh<-subset(datos,ANO=="1984" & SEXO=="Hembra")

L5084h = gonad_mature(ochentacuatroh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5084h)

plot(L5084h, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)


#######################################################################################
################################ 1985 #################################################
######################################################################################

ochentacinco=subset(datos, ANO=="1985")

L5085 = gonad_mature(ochentacinco, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5085, xlab = "TALLA (cm.)", ylab = "Proporción de  maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1985

ochentacincom<-subset(datos,ANO=="1985" & SEXO=="Macho")

L5085m = gonad_mature(ochentacincom, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5085m)

plot(L5085m, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)


#hembra 1985


ochentacincoh<-subset(datos,ANO=="1985" & SEXO=="Hembra")

L5085h = gonad_mature(ochentacincoh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5085h)

plot(L5085h, xlab = " TALLA (cm)", ylab = "Proporción de maduros", col = c("blue", "red"),onlyOgive = TRUE)


##################################################################
################################ 1986 ##########################
#################################################################

ochentaseis=subset(datos, ANO=="1986")

L5086 = gonad_mature(ochentaseis, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5086, xlab = "TALLA (cm.)", ylab = "Proporción de  maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1986

ochentaseism<-subset(datos,ANO=="1986" & SEXO=="Macho")


L5086m = gonad_mature(ochentaseism, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5086m)

plot(L5086m, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 1986


ochentaseish<-subset(datos,ANO=="1986" & SEXO=="Hembra")


L5086h = gonad_mature(ochentaseish, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5086h)

plot(L5086h, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)


#######################################################################################
################################ 1987 #################################################
######################################################################################

ochentasiete=subset(datos, ANO=="1987")

L5087 = gonad_mature(ochentasiete, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5087, xlab = "TALLA (cm.)", ylab = "Proporción de  maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1987

ochentasietem<-subset(datos,ANO=="1987" & SEXO=="Macho")

L5087m = gonad_mature(ochentasietem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5087m)

plot(L5087m, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)



#hembra 1987

ochentasieteh<-subset(datos,ANO=="1987" & SEXO=="Hembra")


L5087h = gonad_mature(ochentasieteh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5087h)

plot(L5087h, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)

#OJO  maduras = 2 e inmaduras = 32


m1<-glm(ochentasieteh$Madurez~ochentasieteh$TALLA,data = ochentasieteh,family = binomial)
summary(m1)
coef(m1) #coef


## L50
lrPerc<-function(cf,p) (log(p/(1-p))-cf[1])/cf[2] #L50
L50<-lrPerc(coef(m1),0.5); L50

#para calcular os intervalos de confinza 
bcl<-bootCase(m1, B=1000)
confint(bcl)
bl50<-apply(bcl,1,lrPerc,p=0.5)
L50ci<-quantile(bl50,c(0.025,0.975))  
L50ci



########################################################################################
################################ 1988 #################################################
#######################################################################################

ochentaocho=subset(datos, ANO=="1988")


L5088 = gonad_mature(ochentaocho, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5088, xlab = "TALLA (cm.)", ylab = "Proporción de  maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1988

ochentaochom<-subset(datos,ANO=="1988" & SEXO=="Macho")


L5088m = gonad_mature(ochentaochom, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5088m)

plot(L5088m, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 1988


ochentaochoh<-subset(datos,ANO=="1988" & SEXO=="Hembra")


L5088h = gonad_mature(ochentaochoh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5088h)

plot(L5088h, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)


########################################################################################
################################ 1989 #################################################
######################################################################################



ochentanueve=subset(datos, ANO=="1989")


L5089 = gonad_mature(ochentanueve, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5089, xlab = "TALLA (cm.)", ylab = "Proporción de  maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1989

ochentanuevem<-subset(datos,ANO=="1989" & SEXO=="Macho")


L5089m = gonad_mature(ochentanuevem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5089m)

plot(L5089m, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)



#hembra 1989


ochentanueveh<-subset(datos,ANO=="1989" & SEXO=="Hembra")

L5089h = gonad_mature(ochentanueveh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5089h)

plot(L5089h, xlab = " TALLA (cm)", ylab = "Proportion mature", col = c("blue", "red"),onlyOgive = TRUE)

#######################################################################################
################################ 1990 #################################################
######################################################################################

noventa=subset(datos, ANO=="1990")

L5090 = gonad_mature(noventa, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5090, xlab = "TALLA (cm.)", ylab = "Proporción de  maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1990

noventam<-subset(datos,ANO=="1990" & SEXO=="Macho")


L5090m = gonad_mature(noventam, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5090m)

plot(L5090m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)


#hembra 1990


noventah<-subset(datos,ANO=="1990" & SEXO=="Hembra")

L5090h = gonad_mature(noventah, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5090h)

plot(L5090h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)


#######################################################################################
################################ 1991 #################################################
######################################################################################

noventauno=subset(datos, ANO=="1991")


L5091 = gonad_mature(noventauno, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5091, xlab = "TALLA (cm.)", ylab = "Proporción de  maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1991

noventaunom<-subset(datos,ANO=="1991" & SEXO=="Macho")


L5091m = gonad_mature(noventaunom, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5091m)

plot(L5091m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 1991


noventaunoh<-subset(datos,ANO=="1991" & SEXO=="Hembra")

L5091h = gonad_mature(noventaunoh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5091h)

plot(L5091h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)


###############################################################################
################################ 1992 #########################################
###############################################################################



noventados=subset(datos, ANO=="1992")


L5092 = gonad_mature(noventados, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5092, xlab = "TALLA (cm.)", ylab = "Proporción de  maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1992

noventadosm<-subset(datos,ANO=="1992" & SEXO=="Macho")

L5092m = gonad_mature(noventadosm, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5092m)

plot(L5092m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)


#hembra 1992


noventadosh<-subset(datos,ANO=="1992" & SEXO=="Hembra")

L5092h = gonad_mature(noventadosh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5092h)

plot(L5092h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)

#######################################################################################
################################ 1993 #################################################
######################################################################################

noventatres=subset(datos, ANO=="1993")


L5093 = gonad_mature(noventatres, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5093, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1993

noventatresm<-subset(datos,ANO=="1993" & SEXO=="Macho")

L5093m = gonad_mature(noventatresm, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5093m)

plot(L5093m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)


#hembra 1993


noventatresh<-subset(datos,ANO=="1993" & SEXO=="Hembra")

L5093h = gonad_mature(noventatresh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5093h)

plot(L5093h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)


###############################################################################################
################################ 1994          ################################################
###############################################################################################

noventacuatro=subset(datos, ANO=="1994")



L5094 = gonad_mature(noventacuatro, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5094, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1994

noventacuatrom<-subset(datos,ANO=="1994" & SEXO=="Macho")

L5094m = gonad_mature(noventacuatrom, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5094m)

plot(L5094m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)


#hembra 1994


noventacuatroh<-subset(datos,ANO=="1994" & SEXO=="Hembra")

L5094h = gonad_mature(noventacuatroh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5094h)

plot(L5094h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)

######################################################################################
################################ 1995#################################################
######################################################################################


noventacinco=subset(datos, ANO=="1995")


L5095 = gonad_mature(noventacinco, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5095, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1995

noventacincom<-subset(datos,ANO=="1995" & SEXO=="Macho")

L5095m = gonad_mature(noventacincom, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5095m)

plot(L5095m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 1995


noventacincoh<-subset(datos,ANO=="1995" & SEXO=="Hembra")

L5095h = gonad_mature(noventacincoh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5095h)

plot(L5095h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)


######################################################################################
################################ 1996    ############################################
####################################################################################


noventaseis=subset(datos, ANO=="1996")


L5096 = gonad_mature(noventaseis, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5096, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1996

noventaseism<-subset(datos,ANO=="1996" & SEXO=="Macho")


L5096m = gonad_mature(noventaseism, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5096m)

plot(L5096m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 1996


noventaseish<-subset(datos,ANO=="1996" & SEXO=="Hembra")


L5096h = gonad_mature(noventaseish, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5096h)

plot(L5096h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)

###############################################################################################
################################ 1997#################################################
#################################################################


noventasiete=subset(datos, ANO=="1997")

L5097 = gonad_mature(noventasiete, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5097, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1997

noventasietem<-subset(datos,ANO=="1997" & SEXO=="Macho")


L5097m = gonad_mature(noventasietem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5097m)

plot(L5097m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)



#hembra 1997


noventasieteh<-subset(datos,ANO=="1997" & SEXO=="Hembra")


L5097h = gonad_mature(noventasieteh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5097h)

plot(L5097h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 1998         #################################################
###############################################################################################

noventaocho=subset(datos, ANO=="1998")


L5098 = gonad_mature(noventaocho, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5098, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1998

noventaochom<-subset(datos,ANO=="1998" & SEXO=="Macho")

L5098m = gonad_mature(noventaochom, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5098m)

plot(L5098m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)



#hembra 1998


noventaochoh<-subset(datos,ANO=="1998" & SEXO=="Hembra")


L5098h = gonad_mature(noventaochoh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5098h)

plot(L5098h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 1999         #################################################
#############################################################################################



noventanueve=subset(datos, ANO=="1999")


L5099 = gonad_mature(noventanueve, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5099, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 1999

noventanuevem<-subset(datos,ANO=="1999" & SEXO=="Macho")


L5099m = gonad_mature(noventanuevem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5099m)

plot(L5099m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 1999


noventanueveh<-subset(datos,ANO=="1999" & SEXO=="Hembra")


L5099h = gonad_mature(noventanueveh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5099h)

plot(L5099h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2000        #################################################
##############################################################################################


dosmil=subset(datos, ANO=="2000")

L5000 = gonad_mature(dosmil, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5000, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2000

dosmilm<-subset(datos,ANO=="2000" & SEXO=="Macho")


L5000m = gonad_mature(dosmilm, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5000m)

plot(L5000m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2000


dosmilh<-subset(datos,ANO=="2000" & SEXO=="Hembra")


L5000h = gonad_mature(dosmilh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5000h)

plot(L5000h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)





###############################################################################################
################################ 2001#################################################
################################################################

dosmiluno=subset(datos, ANO=="2001")


L5001 = gonad_mature(dosmiluno, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5001, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2001

dosmilunom<-subset(datos,ANO=="2001" & SEXO=="Macho")


L5001m = gonad_mature(dosmilunom, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5001m)

plot(L5001m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)

#########################


m1<-glm(dosmilunom$Madurez~dosmilunom$TALLA,data = datos,family = binomial)
summary(m1)
coef(m1) #coef


## L50
lrPerc<-function(cf,p) (log(p/(1-p))-cf[1])/cf[2] #L50
L50<-lrPerc(coef(m1),0.5); L50

#para calcular os intervalos de confinza 
bcl<-bootCase(m1, B=1000)
confint(bcl)
bl50<-apply(bcl,1,lrPerc,p=0.5)
L50ci<-quantile(bl50,c(0.025,0.975))  
L50ci




#hembra 2001


dosmilunoh<-subset(datos,ANO=="2001" & SEXO=="Hembra")


L5001h = gonad_mature(dosmilunoh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5001h)

plot(L5001h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2002#################################################
#################################################################



dosmildos=subset(datos, ANO=="2002")


L5002 = gonad_mature(dosmildos, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5002, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2002

dosmildosm<-subset(datos,ANO=="2002" & SEXO=="Macho")


L5002m = gonad_mature(dosmildosm, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5002m)

plot(L5002m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2002


dosmildosh<-subset(datos,ANO=="2002" & SEXO=="Hembra")



L5002h = gonad_mature(dosmildosh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5002h)

plot(L5002h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)




###############################################################################################
################################     2003         #############################################
#################################################################

dosmiltres=subset(datos, ANO=="2003")

L5003 = gonad_mature(dosmiltres, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5003, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2003

dosmiltresm<-subset(datos,ANO=="2003" & SEXO=="Macho")

L5003m = gonad_mature(dosmiltresm, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5003m)

plot(L5003m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2003


dosmiltresh<-subset(datos,ANO=="2003" & SEXO=="Hembra")



L5003h = gonad_mature(dosmiltresh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5003h)

plot(L5003h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2004#################################################
#################################################################



dosmilcuatro=subset(datos, ANO=="2004")


L5004 = gonad_mature(dosmilcuatro, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5004, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2004

dosmilcuatrom<-subset(datos,ANO=="2004" & SEXO=="Macho")



L5004m = gonad_mature(dosmilcuatrom, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5004m)

plot(L5004m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2004


dosmilcuatroh<-subset(datos,ANO=="2004" & SEXO=="Hembra")


L5004h = gonad_mature(dosmilcuatroh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5004h)

plot(L5004h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)




###############################################################################################
################################ 2005#################################################
#################################################################

dosmilcinco=subset(datos, ANO=="2005")


L5005 = gonad_mature(dosmilcinco, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5005, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2005

dosmilcincom<-subset(datos,ANO=="2005" & SEXO=="Macho")



L5005m = gonad_mature(dosmilcincom, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5005m)

plot(L5005m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2005


dosmilcincoh<-subset(datos,ANO=="2005" & SEXO=="Hembra")


L5005h = gonad_mature(dosmilcincoh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5005h)

plot(L5005h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2006#################################################
#################################################################


dosmilseis=subset(datos, ANO=="2006")


L5006 = gonad_mature(dosmilseis, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5006, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2006

dosmilseism<-subset(datos,ANO=="2006" & SEXO=="Macho")


L5006m = gonad_mature(dosmilseism, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5006m)

plot(L5006m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2006


dosmilseish<-subset(datos,ANO=="2006" & SEXO=="Hembra")


L5006h = gonad_mature(dosmilseish, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5006h)

plot(L5006h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2007#################################################
#################################################################



dosmilsiete=subset(datos, ANO=="2007")


L5007 = gonad_mature(dosmilsiete, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5007, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2007

dosmilsietem<-subset(datos,ANO=="2007" & SEXO=="Macho")


L5007m = gonad_mature(dosmilsietem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5007m)

plot(L5007m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2007


dosmilsieteh<-subset(datos,ANO=="2007" & SEXO=="Hembra")


L5007h = gonad_mature(dosmilsieteh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5007h)

plot(L5007h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)


###############################################################################################
################################ 2008 #################################################
#################################################################

dosmilocho=subset(datos, ANO=="2008")


L5008 = gonad_mature(dosmilocho, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5008, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2008

dosmilochom<-subset(datos,ANO=="2008" & SEXO=="Macho")


L5008m = gonad_mature(dosmilochom, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5008m)

plot(L5008m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2008


dosmilochoh<-subset(datos,ANO=="2008" & SEXO=="Hembra")


L5008h = gonad_mature(dosmilochoh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5008h)

plot(L5008h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)

###############################################################################################
################################   2009      #############################################
#################################################################

dosmilnueve=subset(datos, ANO=="2009")


L5009 = gonad_mature(dosmilnueve, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5009, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2009

dosmilnuevem<-subset(datos,ANO=="2009" & SEXO=="Macho")



L5009m = gonad_mature(dosmilnuevem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5009m)

plot(L5009m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2009


dosmilnueveh<-subset(datos,ANO=="2009" & SEXO=="Hembra")



L5009h = gonad_mature(dosmilnueveh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5009h)

plot(L5009h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)


###############################################################################################
################################ 2010#################################################
#################################################################

dosmildiez=subset(datos, ANO=="2010")


L5010 = gonad_mature(dosmildiez, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5010, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2010

dosmildiezm<-subset(datos,ANO=="2010" & SEXO=="Macho")


L5010m = gonad_mature(dosmildiezm, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5010m)

plot(L5010m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2010


dosmildiezh<-subset(datos,ANO=="2010" & SEXO=="Hembra")

L5010h = gonad_mature(dosmildiezh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5010h)

plot(L5010h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)


###############################################################################################
################################ 2011#################################################
#################################################################


dosmilonce=subset(datos, ANO=="2011")


L5011 = gonad_mature(dosmilonce, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)
print(L5011)

plot(L5011, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2011

dosmiloncem<-subset(datos,ANO=="2011" & SEXO=="Macho")


L5011m = gonad_mature(dosmiloncem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5011m)

plot(L5011m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2011


dosmilonceh<-subset(datos,ANO=="2011" & SEXO=="Hembra")


L5011h = gonad_mature(dosmilonceh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5011h)

plot(L5011h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2012#################################################
#################################################################


dosmildoce=subset(datos, ANO=="2012")

L5012 = gonad_mature(dosmildoce, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5012, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)

mean(L5012$boot)

#macho 2012

dosmildocem<-subset(datos,ANO=="2012" & SEXO=="Macho")


L5012m = gonad_mature(dosmildocem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5012m)

plot(L5012m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2012


dosmildoceh<-subset(datos,ANO=="2012" & SEXO=="Hembra")
L5012h = gonad_mature(dosmildoceh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5012h)

plot(L5012h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)

###############################################################################################
################################ 2013#################################################
#################################################################


dosmiltrece=subset(datos, ANO=="2013")


L5013 = gonad_mature(dosmiltrece, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5013, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2013

dosmiltrecem<-subset(datos,ANO=="2013" & SEXO=="Macho")


L5013m = gonad_mature(dosmiltrecem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5013m)

plot(L5013m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2013


dosmiltreceh<-subset(datos,ANO=="2013" & SEXO=="Hembra")


L5013h = gonad_mature(dosmiltreceh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5013h)

plot(L5013h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)

###############################################################################################
################################ 2014 #################################################
#################################################################

dosmilcatorce=subset(datos, ANO=="2014")



L5014 = gonad_mature(dosmilcatorce, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5014, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2014

dosmilcatorcem<-subset(datos,ANO=="2014" & SEXO=="Macho")

L5014m = gonad_mature(dosmilcatorcem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5014m)

plot(L5014m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2014


dosmilcatorceh<-subset(datos,ANO=="2014" & SEXO=="Hembra")


L5014h = gonad_mature(dosmilcatorceh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5014h)

plot(L5014h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)




################################################################################
################################ 2015#################################################
#################################################################



dosmilquince=subset(datos, ANO=="2015")


L5015 = gonad_mature(dosmilquince, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5015, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2015

dosmilquincem<-subset(datos,ANO=="2015" & SEXO=="Macho")


L5015m = gonad_mature(dosmilquincem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5015m)

plot(L5015m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2015


dosmilquinceh<-subset(datos,ANO=="2015" & SEXO=="Hembra")


L5015h = gonad_mature(dosmilquinceh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5015h)

plot(L5015h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)




#Podemos calcular L50 deste xeito:

m15h<-glm(dosmilquinceh$Madurez~dosmilquinceh$TALLA,data = dosmilquinceh,family = binomial)
summary(m15h)
coef(m15h) #coef



###############################################################################################
################################ 2016#################################################
#################################################################

dosmildieciseis=subset(datos, ANO=="2016")



L5016 = gonad_mature(dosmildieciseis, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5016, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2016

dosmildieciseism<-subset(datos,ANO=="2016" & SEXO=="Macho")


L5016m = gonad_mature(dosmildieciseism, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5016m)

plot(L5016m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2016


dosmildieciseish<-subset(datos,ANO=="2016" & SEXO=="Hembra")

L5016h = gonad_mature(dosmildieciseish, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5016h)

plot(L5016h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2017#################################################
#################################################################


dosmildiecisiete=subset(datos, ANO=="2017")


L5017 = gonad_mature(dosmildiecisiete, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5017, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2017

dosmildiecisietem<-subset(datos,ANO=="2017" & SEXO=="Macho")




L5017m = gonad_mature(dosmildiecisietem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5017m)

plot(L5017m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2017


dosmildiecisieteh<-subset(datos,ANO=="2017" & SEXO=="Hembra")

L5017h = gonad_mature(dosmildiecisieteh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5017h)

plot(L5017h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)




###############################################################################################
################################ 2018#################################################
#################################################################

dosmildieciocho=subset(datos, ANO=="2018")



L5018 = gonad_mature(dosmildieciocho, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5018, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2018

dosmildieciochom<-subset(datos,ANO=="2018" & SEXO=="Macho")



L5018m = gonad_mature(dosmildieciochom, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5018m)

plot(L5018m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2018


dosmildieciochoh<-subset(datos,ANO=="2018" & SEXO=="Hembra")


L5018h = gonad_mature(dosmildieciochoh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5018h)

plot(L5018h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)


###############################################################################################
################################ 2019#################################################
#################################################################



dosmildiecinueve=subset(datos, ANO=="2019")

L5019 = gonad_mature(dosmildiecinueve, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                     matName = "Maduro", method = "fq", niter = 999)

plot(L5019, xlab = "TALLA (cm.)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2019

dosmildiecinuevem<-subset(datos,ANO=="2019" & SEXO=="Macho")

L5019m = gonad_mature(dosmildiecinuevem, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5019m)

plot(L5019m, xlab = " TALLA (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#hembra 2019


dosmildiecinueveh<-subset(datos,ANO=="2019" & SEXO=="Hembra")

L5019h = gonad_mature(dosmildiecinueveh, varNames = c("TALLA", "Madurez"), inmName = "Inmaduro",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5019h)

plot(L5019h, xlab = " TALLA (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)



######################df################################

ANO<-c(1982:2019)
L50_ANO<-c(37,34.9,37.8,37.7,37.5,39.3,35.6,37.1,34.8,34.3,32.7,38.7,38.5,36,38.6,46.9,49.7,36.9,35.4,43.1,44.5,38.8,38.2,38,38.9,34.8,35.7,38.5,39.3,30.5,19.8,35.3,34.4,23.7,30.2,32,39.1,21.8)
L50_Macho<-c(36.1,32.6,35.7,35.2,34.7,38.9,33.4,34.6,31.5,29.9,29.7,33.6,31.2,31.1,35.8,34.6,36.8,26.7,31.6,39,33.4,31.1,28.6,31.4,32.7,29.4,30.6,24.2,33.1,25.6,24.1,24.7,NA,NA,27.9,26.5,27.2,23.2)
L50_hembra<-c(49.7,44.5,40.5,44.8,44.6,40.5,39.2,41.6,41.8,42.3,43.3,43.5,46.1,46.3,56.2,51.4,52.3,51.6,47.3,43.4,46,46.5,45.8,45,44.3,47,45.6,46.6,49.9,45.7,45.3,40.9,45.7,39,46.5,33.1,38.3,41.8,38.8)


L50df<-as.data.frame(cbind(ANO, L50_ANO, L50_Macho, L50_hembra))

windows()
madurez=ts(L50df$L50_ANO,start = c(1982,1), end = c(2019,1),frequency = 1)
plot(madurez)


par(mfrow=c(1,2))
madurezm=ts(na.ma(L50df$L50_Macho),start = c(1982,1), end = c(2019,1),frequency = 1)
plot(madurezm,xlab="Año",ylab="Talla (cm)",lwd=2, col=4,main="(a)")

madurezh=ts(L50df$L50_hembra,start = c(1982,1), end = c(2019,1),frequency = 1)
plot(madurezh, xlab="Año", ylab = "Talla (cm)", lwd=2, col=3, main="(b)")




#ANOVA L50 AÑO
m2<-glm(Madurez~TALLA*ANO, data = datos,family= binomial)
drop1(m2,~.,test="Chisq")




##########################################################
# Kn (con peso vivo) que ser?n variables independientes en el estudio
#############################################################

datospv <- datos %>%
  filterD(!is.na(PESO_vivo),!is.na(TALLA)) %>%
  mutate(logL=log(TALLA),logW=log(PESO_vivo))

#relaci?n talla peso
lm1 <- lm(logW~logL,data=datospv)
coef(lm1)
summary(lm1)



plot(datospv$logW~datospv$logL, xlab="(Log) Peso ", ylab="(Log) Talla")
abline(lm1, col=2)


datospv %>% mutate(lwresid=residuals(lm1))
headtail(datospv,n=2)
datospv<-datospv %>% mutate(predW=fitted(lm1)*100,Kn=PESO_vivo/predW)
headtail(datospv,n=2)

#normalidad 
jarque.bera.test(datospv$Kn)
qqPlot(datospv$Kn)
hist(datospv$Kn)		


#### kn por sexo
sexo<-with(datospv,tapply(datospv$Kn, list(datospv$ANO, datospv$SEXO), mean))
Knm<-sexo[,1];Knm
Knh<-sexo[,2];Knh


save.image("L50Kn.RData")


