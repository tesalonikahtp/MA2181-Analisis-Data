# import data
library(xlsx)
Cesium <- read.xlsx("R/Praktikum Andat/Praktikum 2/Laju Pencacahan Cesium.xlsx", sheetName = "Sheet1")
View(Cesium)

# Statistika Deskriptif
summary(Cesium)

(jumlah = sum(Cesium))
(modus = names(sort(-table(Cesium$V1)))[1])

library(psych)
Cesium <- as.data.frame(Cesium)
describe (Cesium)

library(Hmisc)
Hmisc::describe(Cesium)

library(pastecs)
stat.desc(Cesium)

# Import Data
library(xlsx)
estates <- read.xlsx("R/Praktikum Andat/Praktikum 2/Estates Production.xlsx", sheetName = "Sheet1")
View(estates)

# Diagram Pencar
plot(estates$Coffee, estates$Tea, main = "Scatterplot Produksi Kopi Terhadap Teh (Ton)", xlab = "Kopi", ylab = "Teh")

# Diagram Garis
plot(estates$Palm.Oil, type = "o", main = "Diagram Garis Produksi Minyak Kelapa", xlab = "Tahun ke-", ylab = "Minyak Kelapa (Ton)")

# Diagram Batang
hist(Cesium$V1, freq = T, main = "Laju Pencacahan Cs-137", xlab = "Laju(per/ 10 detik)", ylab = "Frekuensi")

# Diagram Batang Daun
stem(Cesium$V1)

# Diagram Kotak Titik
boxplot(Cesium$V1, horizontal = T, main = "Laju Pencacahan Cs-137")

# PP Plot
library(CircStats)
pp.plot(Cesium$V1, ref.line = TRUE)

# QQ PLOT
qqnorm(Cesium$V1)
qqline(Cesium$V1, col = 2)