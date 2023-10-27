#Import Data
library(readxl)
cesium <- read_excel("prak andat/Praktikum 2 Tesa/cesium.xlsx")
View(cesium)

#Statistika Deskriptif
summary(cesium)

(jumlah = sum(cesium))
(modus = names(sort(-table(cesium$V1)))[1])

install.packages("pysch")
library(psych)
describe(cesium)

install.packages("Hmisc")
library(Hmisc)
describe(cesium)

install.packages("pastecs")
library(pastecs)
stat.desc(cesium)


#Import Data
library(readxl)
estates <- read_excel("prak andat/Praktikum 2 Tesa/Estates Production.xlsx")
View(estates)


#1. Scatterplot
plot(estates$Coffee, estates$Tea, main = "Scatterplot Produksi Kopi Terhadap Teh (ton)", xlab="Kopi", ylab="Teh")

#2. Lineplot
plot(estates$`Palm Oil`, type="o", main="Diagram Garis Produksi Minyak Kelapa", xlab="Tahun ke-", ylab="minyak kelapa (ton)")

#3. Histogram
hist(cesium$V1, freq=T, main="Laju Pencacahan Cs-137", xlab="Laju (per/ 10 detik)", ylab="Frekuensi")

#4. Steam Leaf
stem(cesium$V1)

#5. Boxplot
boxplot(cesium$V1, horizontal=T, main="Laju Pencacahan Cs-137")

#6a. PP Plot
install.packages("CircStats")
library(CircStats)
pp.plot(cesium$V1, ref.line = TRUE)

#6b. QQ Plot
qqnorm(cesium$V1)
qqline(cesium$V1, col=2)




