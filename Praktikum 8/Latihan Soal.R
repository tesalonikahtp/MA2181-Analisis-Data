# Soal Latihan no 2
# a.
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 8/DATA DERET WAKTU.xlsx", sheet = "latihan no 2")
View(data)
# data deret waktu
library(forecast)
data_1 = ts(data$`total penjualan (juta)`)
plot(data_1, main = "Grafik Total Penjualan", ylab = "Penjualan (Juta Rupiah)", xlab = "waktu", type = 'o')

# UJI KESTASIONERAN
# plot ACF
acf(data_1)
# uji ADF
adf.test(data_1)

# b.
# IDENTIFIKASI ORDE MODEL
# plot ACF
acf(data_1, main = "Grafik ACF")
# plot PACF
pacf(data_1, main = "Grafik PACF")

# ESTIMASI PARAMETER MODEL
# Model ARIMA(1,0,0)
model_ari1 = arima(data_1, order = c(1,0,0))
summary(model_ari1)
# Model ARIMA(2,0,1)
model_ari2 = arima(data_1, order = c(2,0,1))
summary(model_ari2)
# Model ARIMA(1,0,2)
model_ari3 = arima(data_1, order = c(1,0,2))
summary(model_ari3)
# Model Auto
model = auto.arima(data_1)
summary(model)

summary(model_ari2)

# UJI DIAGNOSTIK
checkresiduals(model_ari2)

# c. 
(prediksi = forecast(model_ari2, h = 4))
plot(prediksi, main = "Grafik Total Penjualan", ylab = "Penjualan (Juta Rupiah)", xlab = "waktu", type = 'o')

