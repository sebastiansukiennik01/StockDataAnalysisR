# Projekt ER

library(ggplot2)
library(dplyr)
library(zoo)
setwd("/Users/sebastiansukiennik/Library/Mobile Documents/com~apple~CloudDocs/Documents/Studia /PwR/Projekt")

#Pobieranie danych
JSW <- read.csv(file ="./jsw_d.csv",head=T,sep=",")
Comarch <- read.csv(file ="./cmr_d.csv",head=T,sep=",")
WIG20 <- read.csv(file ="./wig20_d.csv",head=T,sep=",")
PKOPB <- read.csv(file ="./pko_d.csv",head=T,sep=",")


print(PKOPB)
print(WIG20)

# Zmiana formatów i filtrowanie dancyh
WIG20$Data <- as.Date(WIG20$Data)
JSW$Data <- as.Date(JSW$Data)
Comarch$Data <- as.Date(Comarch$Data)
PKOPB$Data <- as.Date(PKOPB$Data)

Comarch <- Comarch %>%
  filter(Data > as.Date("2013-01-01"))
JSW <- JSW %>%
  filter(Data > as.Date("2013-01-01"))
PKOPB <- PKOPB %>%
  filter(Data > as.Date("2013-01-01"))
WIG20 <- WIG20 %>%
  filter(Data > as.Date("2013-01-01"))

#Wizualizacja danych dziennych
ggplot(WIG20,aes(Data,Otwarcie)) + geom_line()
ggplot(JSW,aes(Data,Otwarcie)) + geom_line()
ggplot(Comarch,aes(Data,Otwarcie)) + geom_line()
ggplot(PKOPB,aes(Data,Otwarcie)) + geom_line()

a<-WIG20
print(a)

# Obliczenie i wizualizacja średniej miesięcznej
WIG20$YM <- format(WIG20$Data,"%Y-%B",tz = "GMT")
pdane <- aggregate(Zamkniecie~YM,data=WIG20,FUN = mean)
pdane$Data <- as.yearmon(pdane$YM,"%Y-%b")
WIG20$qr <- as.yearqtr(WIG20$Data, format = "%Y-%m-%d")
p2dane <- aggregate(Zamkniecie~qr,data=WIG20,FUN = mean)
p2dane$Data <- as.Date(p2dane$qr)
wig20_mplot <- ggplot(pdane, aes(Data, Zamkniecie)) + geom_line()


JSW$YM <- format(JSW$Data,"%Y-%B",tz = "GMT")
jswdane <- aggregate(Zamkniecie~YM,data=JSW,FUN = mean)
jswdane$d <- as.yearmon(jswdane$YM,"%Y-%b")
JSW$qr <- as.yearqtr(JSW$Data, format = "%Y-%m-%d")
jsw2dane <- aggregate(Zamkniecie~qr,data=JSW,FUN = mean)
jsw2dane$Data <- as.Date(jsw2dane$qr)
print(jsw2dane)
jsw_mplot <- ggplot(jswdane,aes(d,Zamkniecie))+geom_line()

Comarch$YM <- format(Comarch$Data,"%Y-%B",tz = "GMT")
cmrdane <- aggregate(Zamkniecie~YM,data=Comarch,FUN = mean)
cmrdane$d <- as.yearmon(cmrdane$YM,"%Y-%b")
Comarch$qr <- as.yearqtr(Comarch$Data, format = "%Y-%m-%d")
cmr2dane <- aggregate(Zamkniecie~qr,data=Comarch,FUN = mean)
cmr2dane$Data <- as.Date(cmr2dane$qr)
print(cmr2dane)
cmr_mplot <- ggplot(cmrdane,aes(d,Zamkniecie))+geom_line()

PKOPB$YM <- format(PKOPB$Data,"%Y-%B",tz = "GMT")
pkodane <- aggregate(Zamkniecie~YM,data=PKOPB,FUN = mean)
pkodane$d <- as.yearmon(pkodane$YM,"%Y-%b")
PKOPB$qr <- as.yearqtr(PKOPB$Data, format = "%Y-%m-%d")
pko2dane <- aggregate(Zamkniecie~qr,data=PKOPB,FUN = mean)
pko2dane$Data <- as.Date(pko2dane$qr)
print(pko2dane)
pko_mplot <- ggplot(pkodane,aes(d,Zamkniecie))+geom_line()



################# miesięczna do kwartalnej

pdane$Data <- format(pdane$Data,"%Y-%m-15")
pdane$Data <- as.Date(pdane$Data)
print(pdane)
ggplot(NULL,aes(Data,Zamkniecie)) +
  geom_line(data = p2dane,color = "#CDAA7D") + 
  geom_line(data = pdane,color="green") + 
  theme(text = element_text(family = "serif", size = 14), axis.line = element_line(color = "black"),title = element_text(color = "grey")) + 
  labs(x= "Data",y="Wartość akcji(zł)")

cmrdane$Data <- format(cmrdane$d,"%Y-%m-15")
cmrdane$Data <- as.Date(cmrdane$Data)
ggplot(NULL,aes(Data,Zamkniecie)) +
  geom_line(data = cmr2dane,color = "#CDAA7D",size = 1.15) + 
  geom_line(data = cmrdane,color="green",size = 1.15) + 
  theme(text = element_text(family = "serif", size = 14), axis.line = element_line(color = "black"),title = element_text(color = "grey")) + 
  labs(x= "Data",y="Wartość akcji(zł)")

pkodane$Data <- format(pkodane$d,"%Y-%m-15")
pkodane$Data <- as.Date(pkodane$d)
ggplot(NULL,aes(Data,Zamkniecie)) +
  geom_line(data = pko2dane,color = "#CDAA7D") + 
  geom_line(data = pkodane,color="green") +  
  theme(text = element_text(family = "serif", size = 14), axis.line = element_line(color = "black"),title = element_text(color = "grey")) + 
  labs(x= "Data",y="Wartość akcji(zł)")

jswdane$Data <- format(jswdane$d,"%Y-%m-15")
jswdane$Data <- as.Date(jswdane$Data)
ggplot(NULL,aes(Data,Zamkniecie)) +
  geom_line(data = jsw2dane,color = "#CDAA7D",size=1) + 
  geom_line(data = jswdane,color="green") + 
  theme(text = element_text(family = "serif", size = 14), axis.line = element_line(color = "black"),title = element_text(color = "grey")) + 
  labs(x= "Data",y="Wartość akcji(zł)")


# stopy zwrotu 
#WIG20
logret_wig20 <- diff(log(WIG20$Zamkniecie), lag=1)
pdane <- pdane[order(pdane$Data), ]
pdane_logret <- diff(log(pdane$Zamkniecie), lag=1)
print(logret_wig20)
plot(WIG20$Data[2:dim(WIG20)[1]], logret_wig20, type="l", main="WIG20", ylab="Stopa zwrotu", xlab="Daty")
lines(pdane$Data[2:dim(pdane)[1]], pdane_logret, col="red")

#JSW
logret_jsw <- diff(log(JSW$Zamkniecie), lag=1)
jswdane <- jswdane[order(jswdane$Data), ]
jswdane_logret <- diff(log(jswdane$Zamkniecie), lag=1)
plot(JSW$Data[2:dim(JSW)[1]], logret_jsw, type = "l", ylim=c(-0.5, 0.5), main = "JSW", ylab="Stopa zwrotu", xlab="Daty")
lines(jswdane$Data[2:dim(jswdane)[1]], jswdane_logret, col="red")

#COMARCH
logret_cmr <- diff(log(Comarch$Zamkniecie), lag=1)
cmrdane <- cmrdane[order(cmrdane$Data), ]
cmrdane_logret <- diff(log(cmrdane$Zamkniecie), lag=1)
plot(Comarch$Data[2:dim(Comarch)[1]], logret_cmr, type="l", ylim=c(-0.3, 0.3), main="CMR", ylab="Stopa zwrotu", xlab="Daty")
lines(cmrdane$Data[2:dim(cmrdane)[1]], cmrdane_logret, col="red")

#PKOBP
logret_pko <- diff(log(PKOPB$Zamkniecie), lag=1)
pkodane <- pkodane[order(pkodane$Data), ]
pkodane_logret <- diff(log(pkodane$Zamkniecie), lag=1)
plot(PKOPB$Data[2:dim(PKOPB)[1]], logret_pko, type="l", ylim=c(-0.3, 0.3), main="CMR", ylab="Stopa zwrotu", xlab="Daty")
lines(pkodane$Data[2:dim(pkodane)[1]], pkodane_logret, col="red")


