# Latsol 1
# Input data
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 10/DATA SPC.xlsx", 
                       sheet = "Latihan no 1")
View(data)
olah <- data[,2:5] #gunakan kolom berisi variabel saja

# Aktifkan package yang akan digunakan
library(qcc)

# Bagan Kendali xbar
qq1<-qcc(olah, type="xbar", nsigmas=3, title ="Bagan Kendali Ketebalan Kepingan Logam (xbar)", 
         ylab = "Mean Sampel", xlab = "Pengamatan")
# Bagan Kendali R
qq2<-qcc(olah, type="R", nsigmas=3, title ="Bagan Kendali Ketebalan Kepingan Logam (R)", 
         ylab = "Range Sampel", xlab = "Pengamatan")

# Informasi dari Bagan Kendali
summary(qq1)
summary(qq2)

# Latsol 2
# Input data
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 10/DATA SPC.xlsx", 
                       sheet = "Latihan no 2")
View(data)
olah <- data[,2:6] #gunakan kolom berisi variabel saja

# Aktifkan package yang akan digunakan
library(qcc)

# Bagan Kendali xbar
qq1<-qcc(olah, type="xbar", nsigmas=3, title ="Bagan Kendali Kekuatan Tegangan Rentangan (xbar)", 
         ylab = "Mean Sampel", xlab = "Pengamatan")
# Bagan Kendali R
qq2<-qcc(olah, type="R", nsigmas=3, title ="Bagan Kendali Kekuatan Tegangan Rentangan (R)", 
         ylab = "Range Sampel", xlab = "Pengamatan")

# Informasi dari Bagan Kendali
summary(qq1)
summary(qq2)

# Latsol 3
# Input data
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 10/DATA SPC.xlsx", 
                       sheet = "Latihan no 3")
View(data)
olah <- data[,2:6] #gunakan kolom berisi variabel saja

# Aktifkan package yang akan digunakan
library(qcc)

# Bagan Kendali xbar
qq1<-qcc(olah, type="xbar", nsigmas=3, title ="Bagan Kendali Kaleng Isian (xbar)", 
         ylab = "Mean Sampel", xlab = "Pengamatan")
# Bagan Kendali R
qq2<-qcc(olah, type="R", nsigmas=3, title ="Bagan Kendali Kaleng Isian (R)", 
         ylab = "Range Sampel", xlab = "Pengamatan")

# Informasi dari Bagan Kendali
summary(qq1)
summary(qq2)
