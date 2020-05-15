#資料讀入
data2018 <- read.csv("C:/Users/user/Desktop/學校講義/時間序列/2018all.csv",header=T,sep=",")
head(data2018)
names(data2018)[1] <- "Date";names(data2018)[2] <- "Number entry/exit"
data2018 <- data2018[order(data2018$Date),];head(data2018)

data2019 <- read.csv("C:/Users/user/Desktop/學校講義/時間序列/2019all.csv",header=T,sep=",")
names(data2019)[1] <- "Date";names(data2019)[2] <- "Number entry/exit"
data2019 <- data2019[order(data2019$Date),];head(data2019)

data2020 <- read.csv("C:/Users/user/Desktop/學校講義/時間序列/2020all.csv",header=T,sep=",")
names(data2020)[1] <- "Date";names(data2020)[2] <- "Number entry/exit"
data2020<- data2020[order(data2020$Date),];head(data2020)

data20201 <- read.csv("C:/Users/user/Desktop/學校講義/時間序列/20200325202313963/2020_01.csv",header=T,sep=",")
names(data20201)[1] <- "Date";names(data20201)[2] <- "Number entry/exit"
data20201<- data20201[order(data20201$Date),];head(data20201)

dataall <- rbind.data.frame(data2018,data2019);head(dataall)

str(dataall)
library(TSA)
library(aTSA)
library(forecast)
#install.packages("xts")
library(xts)


#時間序列圖
b=dataall
b <-xts(b[,-1],order.by = as.Date(as.factor(b$Date),"%Y%m%d",main="全台出入境時間序列分析"))
plot(b)
par(mfrow=c(1,1))





#acf & pacf
par(mfrow=c(2,1))
acf(dataall$`Number entry/exit`,main="ACF")
pacf(dataall$`Number entry/exit`,main="PACF")

adf.test(dataall$`Number entry/exit`)

#模型估計 AR(1) 
data1.est1=Arima(dataall$`Number entry/exit`,order=c(1,0,0),seasonal = list(order=c(1,0,0),period=7));data1.est1 
data1.est2=Arima(dataall$`Number entry/exit`,order=c(4,0,0),seasonal = list(order=c(1,0,1),period=7));data1.est2 
data1.est3=Arima(dataall$`Number entry/exit`,order=c(4,0,0),seasonal = list(order=c(2,0,1),period=7));data1.est3 

acf(data1.est1$res,main="AR(1)殘差ACF")
pacf(data1.est1$res,main="AR(1)殘差PACF")
acf(data1.est2$res,main="Arima(4,0,0)(1,0,1)[7]殘差ACF")
pacf(data1.est2$res,main="Arima(4,0,0)(1,0,1)[7]殘差PACF")

acf(data1.est3$res,main="Arima(4,0,0)(2,0,1)[7]殘差ACF")
pacf(data1.est3$res,main="Arima(4,0,0)(2,0,1)[7]殘差PACF")
tsdiag(data1.est3)
#預測
par(mfrow=c(1,1))
final <- forecast(data1.est3)

finalplot <- plot(forecast(data1.est3),ylab="出入境人數",xlab="時間",main="預測圖-Arima(4,0,0)(2,0,1)[7] ")

