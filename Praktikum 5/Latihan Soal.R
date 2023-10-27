# Latihan Soal 1
library(readxl)
x <- read_excel("R/Praktikum Andat/Praktikum 5/DATA UJI HIPOTESIS.xlsx", sheet = "latihan no 1")
View(x)
x1 = as.numeric(x$`Kab A`)
x2 = as.numeric(x$`Kab B`)
x3 = as.numeric(x$`Kab C`)
x2 = na.omit(x2)
s1 = sd(x1)
s2 = sd(x2)
n1 = length(x1)
n2 = length(x2)
alpha = 0.05
# Cara Manual
(F = s1^2/s2^2)
(F.upper = qf(1-alpha, n1-1, n2-1))
# P-Value (Bandingkan p-value dengan alpha)
(pval.upper = pf(F, n1-1, n2-1, lower.tail = FALSE))
# Cara Otomatis
var.test(x1, x2, ratio = 1, alternative = "greater", conf.level = 0.95)


# Latihan Soal 2
library(readxl)
x <- read_excel("R/Praktikum Andat/Praktikum 5/DATA UJI HIPOTESIS.xlsx", sheet = "latihan no 2")
View(x)
x = as.numeric(x$Suhu)
xbar = mean(x)                
mu0 = 32                           
sigma = sqrt(4.41)                         
n = length(x)                 
(z = (xbar-mu0)/(sigma/sqrt(n)))     
# P-Value (Bandingkan p-value dengan alpha)
(pval.upper = pnorm(z, lower.tail = FALSE))   


# Latihan Soal 3
library(readxl)
x <- read_excel("R/Praktikum Andat/Praktikum 5/DATA UJI HIPOTESIS.xlsx", sheet = "latihan no 3")
View(x)
x1 = as.numeric(x$`Bengkel I`)
x2 = as.numeric(x$`Bengkel II`)
d = x1-x2
dbar = mean(d)
mu0 = 0
sd = sd(d)
n = length(d)
(t = (dbar-mu0)/(sd/sqrt(n)))
# P-Value (Bandingkan p-value dengan alpha)
(pval.upper = pt(t, df=n-1, lower.tail = FALSE))


# Latihan Soal 4
library(readxl)
x <- read_excel("R/Praktikum Andat/Praktikum 5/DATA UJI HIPOTESIS.xlsx", sheet = "latihan no 4")
View(x)
x1 = as.numeric(x$`1992`)
x2 = as.numeric(x$`1995`)
xbar1 = mean(x1)
xbar2 = mean(x2)
mu0 = -0.5
s1 = sd(x1)
s2 = sd(x2)
n1 = length(x1)
n2 = length(x2)
alpha = 0.01
# Cara Manual
(df = ((s1^2/n1)+(s2^2/n2))^2/(((1/(n1-1))*(s1^2/n1)^2)+((1/(n2-1))*(s2^2/n2)^2)))
(xbar = xbar1-xbar2)
(t = (xbar-mu0)/(sqrt((s1^2/n1)+(s2^2/n2))))
(t.lower = qt(alpha, df))
# P-Value (Bandingkan p-value dengan alpha)
(pval.lower = pt(t, df=n-1))
# Cara Otomatis
t.test(x1, x2, mu=mu0, var.equal = FALSE, alternative = "less", conf.level = 0.99)
                 

# Latihan Soal 5
library(readxl)
x <- read_excel("R/Praktikum Andat/Praktikum 5/DATA UJI HIPOTESIS.xlsx", sheet ="latihan no 5")
View(x)
x = as.numeric(x$`Lama waktu (detik)`)
xbar = mean(x)                
mu0 = 14                          
s = sd(x)                     
n = length(x)                 
alpha = 0.05                 
# Statistika Uji (Bandingkan t hitung dengan t tabel)
(t = (xbar-mu0)/(s/sqrt(n)))
(t.upper = qt(1-alpha, df=n-1))
# P-Value (Bandingkan p-value dengan alpha)
(pval.upper = pt(t, df=n-1, lower.tail = FALSE))
#Cara Otomatis
t.test(x, mu=mu0, alternative = "greater", conf.level = 0.95)


# Latihan Soal 6
library(readxl)
x <- read_excel("R/Praktikum Andat/Praktikum 5/DATA UJI HIPOTESIS.xlsx", sheet = "latihan no 6")
View(x)
x1 = as.numeric(x$Penipuan)
x2 = as.numeric(x$`Senjata Api`)
x2 = na.omit(x2)
xbar1 = mean(x1)
xbar2 = mean(x2)
mu0 = -10
s1 = sd(x1)
s2 = sd(x2)
n1 = length(x1)
n2 = length(x2)
(df = ((s1^2/n1)+(s2^2/n2))^2/(((1/(n1-1))*(s1^2/n1)^2)+((1/(n2-1))*(s2^2/n2)^2)))
(xbar = xbar1-xbar2)
(t = (xbar-mu0)/(sqrt((s1^2/n1)+(s2^2/n2))))
# P-Value (Bandingkan p-value dengan alpha)
(pval.lower = pt(t, df))                      


# Latihan Soal 7
library(readxl)
x <- read_excel("R/Praktikum Andat/Praktikum 5/DATA UJI HIPOTESIS.xlsx", sheet = "latihan no 7")
View(x)
x1 = as.numeric(x$Desa)
x2 = as.numeric(x$Kota)
xbar1 = mean(x1)
xbar2 = mean(x2)
mu0 = 0
s1 = sd(x1)
s2 = sd(x2)
n1 = length(x1)
n2 = length(x2)
alpha = 0.01
# Cara Manual
(df = n1+n2-2)
(Sp = (((n1-1)*s1^2)+((n2-1)*s2^2))/(df))
(xbar = xbar1-xbar2)
(t = (xbar-mu0)/(sqrt(Sp)*(sqrt((1/n1)+(1/n2)))))
(t.upper = qt(1-alpha, df))         
# P-Value (Bandingkan p-value dengan alpha)
(pval.upper = pt(t, df, lower.tail = FALSE))
# Cara Otomatis
t.test(x1, x2, mu=mu0, var.equal = TRUE, alternative = "greater", conf.level = 0.99)
