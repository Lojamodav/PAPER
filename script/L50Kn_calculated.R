
### Load libraries

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


#Load data

setwd("C:/Users/Usuario/Desktop/paper/PAPER/datos")
Hake<-read.csv("Hake.csv",header=TRUE)
Hake<-datos[,-1]
str(Hake)

#We prepare the data

#First, we define levels: sex and maturity
Hake$Sex <- factor(Hake$Sex, 
                     labels = c("Male", "Female", "Unknown"))

Hake$Maturity<-factor(Hake$Maturity,
                      labels=c("Inmature","Mature"))

#We filter the database according to time period. 
#The time unit is the year since 1982, 1982 inclusive.

Hake<-subset(Hake,Hake$Year>1981)

# Temporal unity as factor
Hake$Year<-as.factor(Hake$Year)

#We filter the database excluding hakes of unknown sex
Hake<-subset(Hake,Hake$Sex!="Unknown")

sum(table(Hake$Year)) #N=26821

sum(table(Hake$Year[Hake$Sex=="Female"])) #15124
sum(table(Hake$Year[Hake$Sex=="Male"])) #11697

##########################################################
# L50  
#############################################################


# we calculate L50 hake for both sexes
 

L50pob =gonad_mature(Hake, varNames = c("Length", "Maturity"),  inmName = "Inmature",matName = "Mature", method = "fq", niter = 999)
print(L50pob)

windows()
plot(L50pob, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)

## We calculate L50 for male hake


Male<-subset(Hake,Sex=="Male")

L50male = gonad_mature(Male, varNames = c("Length", "Maturity"), inmName = "Inmaduro",
                        matName = "Mature", method = "fq", niter = 999)
print(L50male)

plot(L50male, xlab = "Length (cm)", ylab = "Proportion of mature males", col = c("blue", "red"), onlyOgive = TRUE)


##  We calculate L50 for female hake

Female<-subset(Hale,Sex=="Female")

L50female = gonad_mature(Female, varNames = c("Length", "Mature"), inmName = "Inmature",
                         matName = "Mature", method = "fq", niter = 999)
print(L50hembra)


jpeg("com",with=10,height = 7,res=300)
par(mfrow=c(1,2))
plot(L50male, xlab = "Length (cm)", ylab = "Proportion of mature males", col = c("blue", "red"), onlyOgive = TRUE)
plot(L50female, xlab = "Length (cm)", ylab = "Proportion of mature females", col = c("blue", "red"), onlyOgive = TRUE)

dev.off

#Are differences between both sex males and females hake significatives? 
#Anova
m1<-glm(Maturity~Length*Sex, data = Hake,family= binomial)
b<-drop1(m1,~.,test="Chisq")
cf<-coef(m1);cf

#Plot
x <- seq(0,100,0.1)
Male <- exp(cf[1]+cf[2]*x)/(1+exp(cf[1]+cf[2]*x))
Female <- exp(cf[1]+cf[3]+(cf[2]+cf[4])*x)/(1+exp(cf[1]+cf[3]+(cf[2]+cf[4])*x))

#Plot

plot(Male~x,type="l",lwd=2,xlab="Total Length (cm)",ylab="Proportion Mature")
lines(hembra~x,type="l",lwd=2,col="red")
legend("bottomright",c("Male","Female"),lwd=2,col=c("black","red"))


######################   Year ##############################################################

#######################################################################################
################################ 1982 #################################################
#######################################################################################


L5082h<-subset(Hake,Year=="1982")

L5082 = gonad_mature(L5082h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5082, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1982

L5082m<-subset(Hake,Year=="1982" & Sex=="Male")

L5082m = gonad_mature(L5082m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5082m)


plot(L5082m, xlab = " Length (cm)", ylab = "Proportion of mature males", col = c("blue", "red"), onlyOgive = TRUE)


#Female 1982


L5082f<-subset(Hake,Year=="1982" & Sex=="Female")

L5082f = gonad_mature(L5082f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5082f)

plot(L5082f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"), onlyOgive = TRUE)



#######################################################################################
################################ 1983 #################################################
#######################################################################################


L5083h<-subset(Hake,Year=="1983")

L5083h = gonad_mature(L5083h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5083h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1983

ochentatresm<-subset(datos,Year=="1983" & Sex=="Male")

L5083m = gonad_mature(L5083m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5083m)


plot(L5083m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)



#Female 1983


L5083f<-subset(Hake,Year=="1983" & Sex=="Female")


L5083f = gonad_mature(L5083f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5083h)

plot(L5083h, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


########################################################################################
################################ 1984 #################################################
#######################################################################################



L5084h=subset(Hake, Year=="1984")


L5084h = gonad_mature(L5084h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5084, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1984

L5084m<-subset(Hake,Year=="1984" & Sex=="Male")

L5084m = gonad_mature(L5084m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5084m)

plot(L5084m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)

#Female 1984

L5084f<-subset(Hake,Year=="1984" & Sex=="Female")

L5084f = gonad_mature(L5084f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5084f)

plot(L5084f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


#######################################################################################
################################ 1985 #################################################
######################################################################################

L5085h=subset(datos, Year=="1985")

L5085h = gonad_mature(L5085h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5085h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#macho 1985

L5085m<-subset(datos,Year=="1985" & Sex=="Male")

L5085m = gonad_mature(L5085h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5085m)

plot(L5085m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)


#Female 1985


L5085f<-subset(datos,Year=="1985" & Sex=="Female")

L5085f = gonad_mature(L5085f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5085f)

plot(L5085f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


##################################################################
################################ 1986 ##########################
#################################################################

L5086h=subset(Hake, Year=="1986")

L5086h = gonad_mature(L5086h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5086h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1986

L5086m<-subset(Hake,Year=="1986" & Sex=="Male")


L5086m = gonad_mature(L5086m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5086m)

plot(L5086m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 1986


L5086f<-subset(Hake,Year=="1986" & Sex=="Female")


L5086f = gonad_mature(L5086f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5086f)

plot(L5086f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


#######################################################################################
################################ 1987 #################################################
######################################################################################

L5087h=subset(Hake, Year=="1987")

L5087h = gonad_mature(L5087h, varNames = c("Length", "Mature"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5087h, xlab = "Length (cm)", ylab = "Proportion of  mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1987

L5087m<-subset(Hake,Year=="1987" & Sex=="Male")

L5087m = gonad_mature(L5087m, varNames = c("Length", "Mature"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5087m)

plot(L5087m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)



#Female 1987

L5087f<-subset(Hake,Year=="1987" & Sex=="Female")


L5087f = gonad_mature(L5087f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5087f)

plot(L5087f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)



########################################################################################
################################ 1988 #################################################
#######################################################################################

L5088h=subset(Hake, Year=="1988")


L5088h = gonad_mature(L5088h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5088h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1988

L5088m<-subset(Hake,Year=="1988" & Sex=="Male")


L5088m = gonad_mature(L5088m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5088m)

plot(L5088m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 1988


L5088f<-subset(Hake,Year=="1988" & Sex=="Female")


L5088f = gonad_mature(L5088f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5088f)

plot(L5088f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


########################################################################################
################################ 1989 #################################################
######################################################################################



L5089h=subset(Hake, Year=="1989")


L5089h = gonad_mature(L5089h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5089h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1989

L5089m<-subset(Hake,Year=="1989" & Sex=="Male")


L5089m = gonad_mature(L5089m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5089m)

plot(L5089m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)



#Female 1989


L5089f<-subset(Hake,Year=="1989" & Sex=="Female")

L5089f = gonad_mature(L5089f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5089f)

plot(L5089f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)

#######################################################################################
################################ 1990 #################################################
######################################################################################

L5090h=subset(Hake, Year=="1990")

L5090h = gonad_mature(L5090h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5090, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1990

L5090m<-subset(Hake,Year=="1990" & Sex=="Male")


L5090m = gonad_mature(L5090m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5090m)

plot(L5090m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)


#Female 1990


L5090f<-subset(Hake,Year=="1990" & Sex=="Female")

L5090f = gonad_mature(L5090f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5090f)

plot(L5090f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


#######################################################################################
################################ 1991 #################################################
######################################################################################

L5091h=subset(Hake, Year=="1991")


L5091h = gonad_mature(L5091h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5091, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1991

L5091m<-subset(Hake,Year=="1991" & Sex=="Male")


L5091m = gonad_mature(L5091m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5091m)

plot(L5091m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 1991


L5091f<-subset(Hake,Year=="1991" & Sex=="Female")

L5091f = gonad_mature(L5091f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5091f)

plot(L5091f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


###############################################################################
################################ 1992 #########################################
###############################################################################



L5092h=subset(Hake, Year=="1992")


L5092h = gonad_mature(noventados, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5092, xlab = "Length (cm.)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1992

L5092m<-subset(Hake,Year=="1992" & Sex=="Male")

L5092m = gonad_mature(L5092m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5092m)

plot(L5092m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)


#Female 1992


L5092f<-subset(Hake,Year=="1992" & Sex=="Female")

L5092f = gonad_mature(L5092f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5092f)

plot(L5092f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)

#######################################################################################
################################ 1993 #################################################
######################################################################################

L5093h=subset(Hake, Year=="1993")


L5093h = gonad_mature(L5093h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5093h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1993

L5093m<-subset(Hake,Year=="1993" & Sex=="Male")

L5093m = gonad_mature(L5093m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5093m)

plot(L5093m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)


#Female 1993


L5093f<-subset(Hake,Year=="1993" & Sex=="Female")

L5093f = gonad_mature(L5093f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5093f)

plot(L5093f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


###############################################################################################
################################ 1994   ################################################
###############################################################################################

L5094h=subset(Hake, Year=="1994")



L5094h = gonad_mature(L5094h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5094h, xlab = "Length (cm)", ylab = "Proportion of mature ", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1994

L5094m<-subset(Hake,Year=="1994" & Sex=="Male")

L5094m = gonad_mature(L5094m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5094m)

plot(L5094m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)


#Female 1994


L5094h<-subset(Hake,Year=="1994" & Sex=="Female")

L5094h = gonad_mature(L5094h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5094h)

plot(L5094h, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)

######################################################################################
################################ 1995#################################################
######################################################################################


L5095h=subset(Hake, Year=="1995")


L5095h = gonad_mature(L5095h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5095h, xlab = "TALLA (cm.)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1995

L5095m<-subset(Hake,Year=="1995" & Sex=="Male")

L5095m = gonad_mature(L5095m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5095m)

plot(L5095m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 1995


L5095f<-subset(Hake,Year=="1995" & Sex=="Female")

L5095f = gonad_mature(L5095f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5095f)

plot(L5095f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


######################################################################################
################################ 1996    ############################################
####################################################################################


L5096h=subset(Hake, Year=="1996")


L5096h = gonad_mature(L5096h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5096h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1996

L5096m<-subset(Hake,Year=="1996" & Sex=="Male")


L5096m = gonad_mature(L5096m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5096m)

plot(L5096m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 1996


L5096f<-subset(Hake,Year=="1996" & Sex=="Female")


L5096f = gonad_mature(L5096f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5096f)

plot(L5096f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)

###############################################################################################
################################ 1997#################################################
#################################################################


L5097h=subset(Hake, Year=="1997")

L5097h = gonad_mature(L5097h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5097h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1997

L5097m<-subset(datos,Year=="1997" & Sex=="Male")


L5097m = gonad_mature(L5097m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Maduro", method = "fq", niter = 999)

print(L5097m)

plot(L5097m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)



#Female 1997


L5097f<-subset(Hake,Year=="1997" & Sex=="Female")


L5097f = gonad_mature(L5097f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5097f)

plot(L5097f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 1998         #################################################
###############################################################################################

L5098h=subset(Hake, Year=="1998")


L5098h = gonad_mature(L5098h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5098h, xlab = "Length (cm.)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 1998

L5098m<-subset(Hake,Year=="1998" & Sex=="Male")

L5098m = gonad_mature(L5098m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5098m)

plot(L5098m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)



#Female 1998


L5098f<-subset(Hake,Year=="1998" & Sex=="Female")


L5098f = gonad_mature(L5098f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5098f)

plot(L5098f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 1999         #################################################
#############################################################################################



L5099h=subset(Hake, Year=="1999")


L5099h = gonad_mature(L5099h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5099h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#macho 1999

L5099m<-subset(Hake,Year=="1999" & Sex=="Male")


L5099m = gonad_mature(L5099m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5099m)

plot(L5099m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 1999


L5099f<-subset(Hake,Year=="1999" & Sex=="Female")


L5099f = gonad_mature(L5099f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5099f)

plot(L5099f, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2000  #################################################
##############################################################################################


L5000h=subset(Hake, Year=="2000")

L5000h = gonad_mature(L5000h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5000h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2000

dosmilm<-subset(Hake,Year=="2000" & Sex=="Male")


L5000m = gonad_mature(dosmilm, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5000m)

plot(L5000m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2000


L5000f<-subset(Hake,Year=="2000" & Sex=="Female")


L5000f = gonad_mature(L5000f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5000f)

plot(L5000f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)





###############################################################################################
################################ 2001#################################################
################################################################

L5001h=subset(Hake, Year=="2001")


L5001h = gonad_mature(L5001h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5001h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#macho 2001

L5001m<-subset(Hake,Year=="2001" & SEX=="Male")


L5001m = gonad_mature(L5001m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5001m)

plot(L5001m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)


#Female 2001


L5001f<-subset(Hake,Year=="2001" & Sex=="Female")


L5001f = gonad_mature(L5001f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5001f)

plot(L5001f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2002#################################################
############################################################################################



L5002h=subset(Hake, Year=="2002")


L5002h = gonad_mature(L5002h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5002h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2002

dosmildosm<-subset(Hake,Year=="2002" & Sex=="Male")


L5002m = gonad_mature(dosmildosm, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5002m)

plot(L5002m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2002


L5002f<-subset(Hake,Year=="2002" & Sex=="Female")



L5002f = gonad_mature(L5002f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5002f)

plot(L5002f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)




###############################################################################################
################################     2003         #############################################
#################################################################

L5003h=subset(Hake, Year=="2003")

L5003h = gonad_mature(L5003h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5003h, xlab = "Length (cm)", ylab = "Proportion of mature ", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2003

L5003m<-subset(Hake,Year=="2003" & Sex=="Male")

L5003m = gonad_mature(L5003m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5003m)

plot(L5003m, xlab = "Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2003


L5003f<-subset(Hake,Year=="2003" & Sex=="Female")



L5003f = gonad_mature(L5003f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5003f)

plot(L5003f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2004#################################################
#################################################################



L5004h=subset(Hake, Year=="2004")


L5004h = gonad_mature(L5004h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5004h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2004

L5004m<-subset(Hake,Year=="2004" & Sex=="Male")



L5004m = gonad_mature(L5004m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5004m)

plot(L5004m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2004


L5004f<-subset(Hake,Year=="2004" & Sex=="Female")


L5004f = gonad_mature(L5004f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5004f)

plot(L5004f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)




###############################################################################################
################################ 2005#################################################
#################################################################

L5005h=subset(Hake, Year=="2005")


L5005h = gonad_mature(L5005h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5005h, xlab = "Length (cm.)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2005

L5005m<-subset(Hake,Year=="2005" & Sex=="Male")



L5005m = gonad_mature(L5005m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5005m)

plot(L5005m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2005


L5005f<-subset(Hake,Year=="2005" & Sex=="Female")


L5005f = gonad_mature(L5005f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5005f)

plot(L5005f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2006#################################################
#################################################################


L5006h=subset(Hake, Year=="2006")


L5006h = gonad_mature(L5006h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5006h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2006

L5006m<-subset(Hake,Year=="2006" & Sex=="Male")


L5006m = gonad_mature(L5006m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5006m)

plot(L5006m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2006


L5006f<-subset(Hake,Year=="2006" & Sex=="Female")


L5006f = gonad_mature(L5006f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5006f)

plot(L5006f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2007#################################################
#################################################################



L5007h=subset(Hake, Year=="2007")


L5007h = gonad_mature(L5007h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5007h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2007

L5007m<-subset(Hake,Year=="2007" & SEX=="Male")


L5007m = gonad_mature(L5007m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5007m)

plot(L5007m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2007


L5007f<-subset(Hake,Year=="2007" & Sex=="Female")


L5007f = gonad_mature(L5007f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5007f)

plot(L5007f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


###############################################################################################
################################ 2008 #################################################
#################################################################

L5008h=subset(Hake, Year=="2008")


L5008h = gonad_mature(L5008h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5008h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2008

L5008m<-subset(Hake,Year=="2008" & Sex=="Male")


L5008m = gonad_mature(L5008m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5008m)

plot(L5008m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2008


L5008f<-subset(Hake,Year=="2008" & Sex=="Female")


L5008f = gonad_mature(L5008f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5008f)

plot(L5008f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)

###############################################################################################
################################   2009      #############################################
#################################################################

L5009h=subset(Hake, Year=="2009")


L5009h = gonad_mature(L5009h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5009h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)


#Male 2009

L5009m<-subset(Hake,Year=="2009" & Sex=="Male")



L5009m = gonad_mature(L5009m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5009m)

plot(L5009m, xlab = "Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2009


L5009f<-subset(Hake,Year=="2009" & Sex=="Female")



L5009f = gonad_mature(L5009f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5009f)

plot(L5009f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


###############################################################################################
################################ 2010#################################################
#################################################################

L5010h=subset(Hake, Year=="2010")


L5010h = gonad_mature(L5010h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5010h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2010

L5010m<-subset(Hake,Year=="2010" & Sex=="Male")


L5010m = gonad_mature(L5010m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5010m)

plot(L5010m, xlab = " Length (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2010


L5010f<-subset(Hake,Year=="2010" & Sex=="Female")

L5010f = gonad_mature(L5010f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5010f)

plot(L5010f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


###############################################################################################
################################ 2011#################################################
#################################################################


L5011h=subset(Hake, Year=="2011")


L5011h = gonad_mature(L5011h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)
print(L5011h)

plot(L5011h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2011

L5011m<-subset(Hake,Year=="2011" & Sex=="Male")


L5011m = gonad_mature(L5011m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5011m)

plot(L5011m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2011


L5011f<-subset(Hake,Year=="2011" & Sex=="Female")


L5011f = gonad_mature(L5011f, varNames = c("Length", "Maturity"), inmName = "Inmaduro",
                      matName = "Mature", method = "fq", niter = 999)

print(L5011f)

plot(L5011f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2012#################################################
#################################################################


dosmildoce=subset(Hake, Year=="2012")

L5012h = gonad_mature(L5012h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5012h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2012

L5012m<-subset(Hake,Year=="2012" & Sex=="Male")


L5012m = gonad_mature(L5012m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5012m)

plot(L5012m, xlab = " Length (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2012


L5012f<-subset(Hake,Year=="2012" & Sex=="Female")
L5012f = gonad_mature(L5012f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5012f)

plot(L5012f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)

###############################################################################################
################################ 2013#################################################
#################################################################


L5013h=subset(Hake, Year=="2013")


L5013h = gonad_mature(L5013h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5013h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2013

L5013m<-subset(Hake,Year=="2013" & Sex=="Male")


L5013m = gonad_mature(L5013m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5013m)

plot(L5013m, xlab = "Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2013


L5013f<-subset(Hake,Year=="2013" & Sex=="Female")


L5013f = gonad_mature(L5013f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5013f)

plot(L5013f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)

###############################################################################################
################################ 2014 #################################################
#################################################################

L5014h=subset(Hake, Year=="2014")



L5014h = gonad_mature(L5014h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5014h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2014

L5014m<-subset(Hake,Year=="2014" & Sex=="Male")

L5014m = gonad_mature(L5014m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5014m)

plot(L5014m, xlab = " Length (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2014


L5014f<-subset(Hake,Year=="2014" & Sex=="Female")


L5014f = gonad_mature(L5014f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5014f)

plot(L5014f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)




################################################################################
################################ 2015#################################################
#################################################################



L5015h=subset(Hake, Year=="2015")


L5015h = gonad_mature(L5015h, varNames = c("Length", "Maturity"), inmName = "Inmaduro",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5015h, xlab = "Length (cm)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2015

L5015m<-subset(Hake,Year=="2015" & Sex=="Male")


L5015m = gonad_mature(L5015m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5015m)

plot(L5015m, xlab = " Length (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2015


L5015f<-subset(Hake,Year=="2015" & Sex=="Female")


L5015f = gonad_mature(L5015f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5015f)

plot(L5015f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)




#Podemos 


###############################################################################################
################################ 2016#################################################
#################################################################

L5016h=subset(Hake, Year=="2016")



L5016h = gonad_mature(L5016h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5016h, xlab = "Length (cm.)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2016

L5016m<-subset(Hake,Year=="2016" & Sex=="Male")


L5016m = gonad_mature(L5016m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5016m)

plot(L5016m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2016


L5016f<-subset(Hake,Year=="2016" & Sex=="Female")

L5016f = gonad_mature(L5016f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5016f)

plot(L5016f, xlab = " Length (cm)", ylab = "Proporción de hembras maduras", col = c("blue", "red"),onlyOgive = TRUE)



###############################################################################################
################################ 2017#################################################
#################################################################


L5017h=subset(Hake, Year=="2017")


L5017h = gonad_mature(L5017h, varNames = c("Length", "Maturity"), inmName = "Inmaduro",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5017h, xlab = "Length (cm)", ylab = "Proporción de individuos maduros", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2017

L5017m<-subset(Hake,Year=="2017" & Sex=="Male")




L5017m = gonad_mature(L5017m, varNames = c("Length", "Maturity"), inmName = "Inmaduro",
                      matName = "Mature", method = "fq", niter = 999)

print(L5017m)

plot(L5017m, xlab = " Length (cm)", ylab = "Propoción de machos maduros", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2017


L5017f<-subset(Hake,Year=="2017" & Sex=="Female")

L5017f = gonad_mature(L5017f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5017f)

plot(L5017f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)




###############################################################################################
################################ 2018#################################################
#################################################################

L5018h=subset(Hake, Year=="2018")



L5018h = gonad_mature(L5018h, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5018h, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2018

L5018m<-subset(datos,Year=="2018" & Sex=="Male")



L5018m = gonad_mature(L5018m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5018m)

plot(L5018m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2018


L5018f<-subset(Hake,Year=="2018" & Sex=="Female")


L5018f = gonad_mature(L5018f, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5018f)

plot(L5018f, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)


###############################################################################################
################################ 2019#################################################
#################################################################



dosmildiecinueve=subset(datos, Year=="2019")

L5019 = gonad_mature(dosmildiecinueve, varNames = c("Length", "Maturity"), inmName = "Inmature",
                     matName = "Mature", method = "fq", niter = 999)

plot(L5019, xlab = "Length (cm)", ylab = "Proportion of mature", col = c("blue", "red"), onlyOgive = TRUE)



#Male 2019

L5019m<-subset(Hake,Year=="2019" & Sex=="Male")

L5019m = gonad_mature(L5019m, varNames = c("Length", "Maturity"), inmName = "Inmature",
                      matName = "Mature", method = "fq", niter = 999)

print(L5019m)

plot(L5019m, xlab = " Length (cm)", ylab = "Proportion of mature male", col = c("blue", "red"),onlyOgive = TRUE)




#Female 2019


dosmildiecinueveh<-subset(datos,Year=="2019" & Sex=="Female")

L5019h = gonad_mature(dosmildiecinueveh, varNames = c("Length", "Maturity"), inmName = "Inmaduro",
                      matName = "Mature", method = "fq", niter = 999)

print(L5019h)

plot(L5019h, xlab = " Length (cm)", ylab = "Proportion of mature female", col = c("blue", "red"),onlyOgive = TRUE)



######################df################################

Year<-c(1982:2019)
L50_Year<-c(37,34.9,37.8,37.7,37.5,39.3,35.6,37.1,34.8,34.3,32.7,38.7,38.5,36,38.6,46.9,49.7,36.9,35.4,43.1,44.5,38.8,38.2,38,38.9,34.8,35.7,38.5,39.3,30.5,19.8,35.3,34.4,23.7,30.2,32,39.1,21.8)
L50_Male<-c(36.1,32.6,35.7,35.2,34.7,38.9,33.4,34.6,31.5,29.9,29.7,33.6,31.2,31.1,35.8,34.6,36.8,26.7,31.6,39,33.4,31.1,28.6,31.4,32.7,29.4,30.6,24.2,33.1,25.6,24.1,24.7,NA,NA,27.9,26.5,27.2,23.2)
L50_Female<-c(49.7,44.5,40.5,44.8,44.6,40.5,39.2,41.6,41.8,42.3,43.3,43.5,46.1,46.3,56.2,51.4,52.3,51.6,47.3,43.4,46,46.5,45.8,45,44.3,47,45.6,46.6,49.9,45.7,45.3,40.9,45.7,39,46.5,33.1,38.3,41.8,38.8)


L50df<-as.data.frame(cbind(Year, L50_Year, L50_Male, L50_Female))

windows()
maturity=ts(L50df$L50_Year,start = c(1982,1), end = c(2019,1),frequency = 1)
plot(maturity)


#L50 males without replace missing values
maturitym=ts((L50df$L50_Male),start = c(1982,1), end = c(2019,1),frequency = 1)
plot(maturitym,xlab="Year",ylab="Length (cm)",lwd=1, col=4,main="Annual variability size at first maturity")


#L50 for both sexs males and females; for l50 males replace missing values with na.ma function
#within imputets package
par(mfrow=c(1,2))
maturitym=ts(na.ma(L50df$L50_Male),start = c(1982,1), end = c(2019,1),frequency = 1)
plot(maturitym,xlab="Year",ylab="Length (cm)",lwd=1, col=4,main="(a)")

maturityf=ts(L50df$L50_Female,start = c(1982,1), end = c(2019,1),frequency = 1)
plot(maturityf, xlab="Year", ylab = "Length (cm)", lwd=1, col=6, main="(b)")




#ANOVA L50 year

m2<-glm(Maturity~Length*Year, data = Hake,family= binomial)
drop1(m2,~.,test="Chisq")




##########################################################
# Kn (calculated with total weigth) 
#############################################################

Haketw <- Hake %>%
  filterD(!is.na(PESO_vivo),!is.na(Length)) %>%
  mutate(logL=log(Length),logW=log(PESO_vivo))

#relaci?n talla peso
lm1 <- lm(logW~logL,data=Haketw)
coef(lm1)
summary(lm1)



plot(Haketw$logW~Haketw$logL, xlab="(Log) Peso ", ylab="(Log) Talla")
abline(lm1, col=2)


Haketw %>% mutate(lwresid=residuals(lm1))
headtail(Haketw,n=2)
Haketw<-Haketw %>% mutate(predW=fitted(lm1)*100,Kn=PESO_vivo/predW)
headtail(Haketw,n=2)

#normality
jarque.bera.test(Haketw$Kn)
qqPlot(Haketw$Kn)
hist(Haketw$Kn)		

mean(Haketw$Kn)
mean(Haketw$Kn[Haketw$Sex=="Female"])
#### kn por sexo
sexo<-with(Haketw,tapply(Haketw$Kn, list(Haketw$Year, Haketw$Sex), mean))
Knm<-sexo[,1];Knm
Knf<-sexo[,2];Knf


Knm<-c(0.81,0.61, 0.70,0.55, 0.71, 0.74, 0.39, 0.49, 0.62, 0.63,0.48, 0.37, 0.52, 0.61, 0.53, 0.57,NA,NA,NA,NA,0.74, 0.69, 0.67, 0.58,0.65, 0.64, 0.71, 0.71,0.68, 0.64, 0.55, 0.47, 0.87, 0.41,0.61, 0.71, 0.79, 0.57)
Knf<- c(1.77,0.80,1.05,0.92, 1.21,1.59, 0.51,0.85,0.77, 0.99, 0.41, 0.35,0.62, 0.85, 1.02,1.10, 2.48,NA,NA,NA, 1.55, 1.40, 1.32, 1.06,0.94, 1.28, 1.11, 1.03, 0.90, 0.71, 1.13, 1.08,1.30, 2.02, 1.04, 1.24,1.55, 1.65)

#Kn plots for ,both sexs, male and female, with missing values
par(mfrow=c(1,2))
fcm=ts(na.ma(Knm),start = c(1982,1), end = c(2019,1),frequency = 1)
plot(fcm,xlab="Year",ylab="Kn",lwd=1, col=4,main="(a)")
fch=ts(na.ma(Knf),start = c(1982,1), end = c(2019,1),frequency = 1)
plot(fch, xlab="Year", ylab = "Kn", lwd=1, col=6, main="(b)")



save.image("L50Kn.RData")


