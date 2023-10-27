# Latihan 1
library(stats)
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 7/DATA REGRESI LINIER.xlsx", sheet = "soal latihan no 1")
View(data)
attach(data)
# a.
scatter.smooth(x=`Surfaktan (kg/m3)`,y=`Jumlah Minyak (Barel)`, main="Surfaktan ~ Jumlah Minyak")
# b.
linearMod <- lm(`Jumlah Minyak (Barel)`~ `Surfaktan (kg/m3)`, data = data)
summary(linearMod) # model summary
(modelSummary <- summary(linearMod)) # capture model summary as an object
(modelCoeffs <- modelSummary$coefficients) # model coefficients
# c.
(f_statistic <- linearMod$fstatistic[1]) #fstatistic
(f <- summary(linearMod)$fstatistic) # parameters for model p-value calc
(model_p <- pf(f[1], f[2], f[3], lower=FALSE))
(f_tabel <- qf(0.95, f[2], f[3]))

# Latihan 2
library(stats)
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 7/DATA REGRESI LINIER.xlsx", sheet = "soal latihan no 2")
View(data)
attach(data)
#a. 
linearMod <- lm(`Zarah terbawa (mikrogram per m3)`~ `Curah hujan harian (0,01 cm)`, data = data)
summary(linearMod) # model summary
(modelSummary <- summary(linearMod)) # capture model summary as an object
(modelCoeffs <- modelSummary$coefficients) # model coefficients
#b.
(f_statistic <- linearMod$fstatistic[1]) #fstatistic
(f <- summary(linearMod)$fstatistic) # parameters for model p-value calc
(model_p <- pf(f[1], f[2], f[3], lower=FALSE))
(f_tabel <- qf(0.95, f[2], f[3]))
#c.
n.curah_hujan <- 4.8
(beta.estimate <- modelCoeffs["`Curah hujan harian (0,01 cm)`", "Estimate"])
(n.zarah <- modelCoeffs["(Intercept)", "Estimate"]+(beta.estimate*n.curah_hujan))

# Latihan 3
library(stats)
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 7/DATA REGRESI LINIER.xlsx", sheet = "soal latihan no 3")
data1 = log(data)
View(data)
attach(data)
linearMod <- lm(`P (kg/cm2)` ~ `V (cm3)` , data = data1)
summary(linearMod) # model summary
(modelSummary <- summary(linearMod)) # capture model summary as an object
(modelCoeffs <- modelSummary$coefficients) # model coefficients

# Latihan 4
library(stats)
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 7/DATA REGRESI LINIER.xlsx", sheet = "soal latihan no 4")
View(data)
attach(data)
# a.
linearMod <- lm(`y` ~ `x` , data = data)
summary(linearMod) # model summary
(modelSummary <- summary(linearMod)) # capture model summary as an object
(modelCoeffs <- modelSummary$coefficients) # model coefficients
# b.
(f_statistic <- linearMod$fstatistic[1]) #fstatistic
(f <- summary(linearMod)$fstatistic) # parameters for model p-value calc
(t_tabel <- qt(0.975, f[3]))

# Latihan 6
library(stats)
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 7/DATA REGRESI LINIER.xlsx", sheet = "soal latihan no 6")
View(data)
attach(data)
# a.
linearMod <- lm(`Y (berat)` ~ `X (tinggi)`, data = data)
summary(linearMod) # model summary
(modelSummary <- summary(linearMod)) # capture model summary as an object
(modelCoeffs <- modelSummary$coefficients) # model coefficients
# b.
(f_statistic <- linearMod$fstatistic[1]) #fstatistic
(f <- summary(linearMod)$fstatistic) # parameters for model p-value calc
(model_p <- pf(f[1], f[2], f[3], lower=FALSE))
(f_tabel <- qf(0.95, f[2], f[3]))
# c.
summary(linearMod) # model summary
# d. 
(modelCoeffs <- modelSummary$coefficients) # model coefficients
(f_statistic <- linearMod$fstatistic[1]) #fstatistic
(f <- summary(linearMod)$fstatistic) # parameters for model p-value calc
(t_tabel <- qt(0.975, f[3]))

# Latihan 7
library(stats)
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 7/DATA REGRESI LINIER.xlsx", sheet = "soal latihan no 7")
View(data)
attach(data)
# a.
scatter.smooth(x=Berat,y=Volume, main="Berat ~ Volume")
# b.
linearMod <- lm(Volume ~ Berat, data = data)
summary(linearMod) # model summary
(modelSummary <- summary(linearMod)) # capture model summary as an object
(modelCoeffs <- modelSummary$coefficients) # model coefficients
# c.
confint(linearMod, level = 0.90)
# d. 
summary(linearMod)
# e.
n.volume <- 3.51
(beta.estimate <- modelCoeffs["Berat", "Estimate"])
(n.berat <- ((n.volume)-(modelCoeffs["(Intercept)", "Estimate"]))/beta.estimate)

# Latihan 8
library(stats)
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 7/DATA REGRESI LINIER.xlsx", sheet = "soal latihan no 8")
View(data)
attach(data)
# a.
scatter.smooth(x=Dosis,y=Aktivitas, main="Dosis ~ Aktivitas")
# b.
linearMod <- lm(Aktivitas ~ Dosis, data = data)
summary(linearMod) # model summary
(modelSummary <- summary(linearMod)) # capture model summary as an object
(modelCoeffs <- modelSummary$coefficients) # model coefficients
# c.
confint(linearMod, level = 0.95)
# d.
summary(linearMod) # model summary
