library(stats)
library(readxl)
data <- read_excel("DATA REGRESI LINIER.xlsx", sheet = "contoh FVF")
View(data)
attach(data)

cov(`Pressure (psig)`, `FVF (B0)`)
cor(`Pressure (psig)`, `FVF (B0)`)

#Scatter Plot
scatter.smooth(x=`Pressure (psig)`,y=`FVF (B0)`, main="Pressure ~ Formation Volume Factor")
#Box Plot
boxplot(`Pressure (psig)`, main="Pressure",sub=paste("Outlier rows: ",boxplot.stats(`Pressure (psig)`)$out))
boxplot(`FVF (B0)`, main="Formation Volume Factor",sub=paste("outlier rows:",boxplot.stats(`FVF (B0)`)$out))
#Density Plot
library(e1071)
par(mfrow=c(1,2)) #divide graph area in 2 columns
plot(density(`FVF (B0)`), main="Density Plot: Formation Volume Factor", ylab="Frequency", sub=paste("skewness:",round(e1071::skewness(`FVF (B0)`), 2)))
polygon(density(`FVF (B0)`), col="red")
plot(density(`Pressure (psig)`), main="Density Plot: Pressure", ylab="Frequency", sub=paste("skewness:",round(e1071::skewness(`Pressure (psig)`), 2)))
polygon(density(`Pressure (psig)`), col="red")

#Linear Model
linearMod <- lm(`Pressure (psig)`~`FVF (B0)`, data = data)
print(linearMod)
summary(linearMod) # model summary

#t-statistics and p-values
(modelSummary <- summary(linearMod)) # capture model summary as an object
(modelCoeffs <- modelSummary$coefficients) # model coefficients
(beta.estimate <- modelCoeffs["`FVF (B0)`", "Estimate"]) # get beta estimation
(std.error <- modelCoeffs["`FVF (B0)`", "Std. Error"]) # get.std.error
(t_value <- beta.estimate/std.error) # calc t statistic
(p_value <- 2*pt(-abs(t_value), df=nrow(data)-ncol(data))) # calc p value
(f_statistic <- linearMod$fstatistic[1]) #fstatistic
(f <- summary(linearMod)$fstatistic) # parameters for model p-value calc
(model_p <- pf(f[1], f[2], f[3], lower=FALSE))

#AIC BIC
(AIC(linearMod))
(BIC(linearMod))


# Latihan Nomor 2
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
(f_statistic <- summary(linearMod)$fstatistic[1]) #fstatistic
(f <- summary(linearMod)$fstatistic) # parameters for model p-value calc
(model_p <- pf(f[1], f[2], f[3], lower=FALSE))
(f_tabel <- qf(0.95, f[2], f[3]))
# atau
(modelCoeffs <- modelSummary$coefficients) # model coefficients
(t_tabel <- qt(0.975, f[3]))
#c.
n.curah_hujan <- 4.8
(beta.estimate <- modelCoeffs["`Curah hujan harian (0,01 cm)`", "Estimate"])
(n.zarah <- modelCoeffs["(Intercept)", "Estimate"]+(beta.estimate*n.curah_hujan))
