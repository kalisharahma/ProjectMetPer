#Dataset: https://drive.google.com/file/d/1B2wz55ntzl4iyEMtyoZh9Uq5V4wOzGXc/view?usp=sharing

#Install dan Import Packages
install.packages("readr")
install.packages("TSA")
install.packages("tseries")
install.packages("forecast")
install.packages("lmtest")
install.packages("rlang")

library(readr)
library(TSA)
library(tseries)
library(forecast)
library(lmtest)
library(rlang)
library(zoo)

#Import Data
tsla <- read_csv("C:/Users/M-SPORT/Downloads/TSLA(1).csv")
View(tsla)


#Buat Variabel Time Series 
timeseries <- ts(tsla$Close, frequency = 12, start= c(2018, 7))
tsdisplay(timeseries)


#Uji Stasioneritas
plot(timeseries)
adf.test(timeseries)
#Tidak stasioner, sehingga dilakukan differencing satu kali
d1_ts <- diff(timeseries, 1)
adf.test(d1_ts)
#Masih tidak stasioner, sehingga dilakukan differencing satu kali lagi
d2_ts <- diff(d1_ts, 1)
adf.test(d2_ts)
tsdisplay(d2_ts)
#Karena variansi di awal semakin membesar, sehingga dilakukan transformasi

#Transformasi
tts <- BoxCox(timeseries, BoxCox.lambda(timeseries))
d1_tts <- diff(tts,1)
tsdisplay(d1_tts)
adf.test(d1_tts)
#Masih tidak stasioner, sehingga dilakukan differencing satu kali lagi
d2_tts <- diff(d1_tts,1)
tsdisplay(d2_tts)
adf.test(d2_tts)
#Kesimpulan: data stasioner setelah dilakukan transformasi dan dua kali differencing 


#Spesifikasi Model
#Fungsi autoarima
auto.arima(timeseries)
#Model yang disarankan tidak digunakan, karena data time series memenuhi asumsi
#stasioneritas setelah differencing dua kali
#Fungsi EACF
eacf(d2_tts)
#Kandidat Model: ARIMA(0,2,2), ARIMA(0,2,1), ARIMA(1,2,1)
model1 <- Arima(tts, order = c(0,2,2), include.constant = TRUE)
model2 <- Arima(tts, order = c(0,2,1), include.constant = TRUE)
model3 <- Arima(tts, order = c(1,2,1), include.constant = TRUE)
#Lihat AIC/BIC
cbind(model1, model2, model3)
#Berdasarkan nilai AIC yang paling kecil, dapat dipilih model ARIMA(0,2,1)


#Estimasi Parameter
est.par <-arima(tts,order=c(0,2,1), method = "ML")
est.par
coeftest(est.par)
#Estimasi parameter telah signifikan terhadap model menggunakan metode ML 


#Diagnostic Model
#Ljung test
checkresiduals(model2)
adf.test(model2$residuals)
#Cek normalitas
jarque.bera.test(model2$residuals) #h_0: normal, h_1: tidak
#maka residual model2 normal


#Overfitting Model
overfit_1 <- Arima(tts, order = c(0,2,2), include.constant = TRUE)
overfit_2 <- Arima(tts, order = c(1,2,1), include.constant = TRUE)

coeftest(model2)
coeftest(overfit_1) 
coeftest(overfit_2) 
#hasilnya parameter AR dan MA tidak signifikan
#Estimasi parameter dari model original tidak berbeda jauh dari model overfit
#tetap memilih model ARIMA (0,2,1)


#Cross Validation
test<-window(timeseries,start = c(2022,12))
train<-window(timeseries,end = c(2022,11))
trainmodel<-Arima(train,order = c(0,2,1))
testfc<-forecast(trainmodel,h = 6)
plot(testfc)

#Forecast
peramalan <- forecast(model2, h = 6)
peramalan #liat nilai forecastnya
autoplot(peramalan)

