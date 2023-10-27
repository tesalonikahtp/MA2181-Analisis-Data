# Contoh Soal 1
# Input data
library(readxl)
berat <- read_excel("R/Praktikum Andat/Praktikum 10/DATA SPC.xlsx", 
                       sheet = "Contoh 1")
View(berat)

# Aktifkan package yang akan digunakan
library(qicharts2)

# Bagan Kendali I
(qi1<-qic(berat$`berat (gr)`, chart="i", point.size = 2, title = "Bagan Kendali I Tepung Terigu (gram)", 
          ylab = "berat", xlab = "kemasan ke-"))
# Bagan Kendali MR
(qi2<-qic(berat$`berat (gr)`, chart="mr", point.size = 2, title = "Bagan Kendali MR Tepung Terigu (gram)", 
          ylab = "berat", xlab = "kemasan ke-"))

# Informasi dari Bagan Kendali
summary(qi1)
summary(qi2)

# Contoh Soal 2
# Input data
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 10/DATA SPC.xlsx", 
                       sheet = "Contoh 2")
View(data)
olah <- data[,2:6] #gunakan kolom berisi variabel saja

# Aktifkan package yang akan digunakan
library(qcc)

# Bagan Kendali xbar
qq1<-qcc(olah, type="xbar", nsigmas=3, title ="Bagan Kendali Volume Kontainer (xbar)", 
         ylab = "Mean Sampel", xlab = "Pengamatan")
# Bagan Kendali R
qq2<-qcc(olah, type="R", nsigmas=3, title ="Bagan Kendali Volume Kontainer (R)", 
         ylab = "Range Sampel", xlab = "Pengamatan")

# Informasi dari Bagan Kendali
summary(qq1)
summary(qq2)




