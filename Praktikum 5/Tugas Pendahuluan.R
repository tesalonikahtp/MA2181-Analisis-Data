# Uji Hipotesis Rataan 1 Populasi
# Variansi diketahui
# Cara Manual
# Input
x = read,csv("nama file.csv") #data
xbar = mean(x)                #mean sampel
mu0                           #nilai hipotesis
sigma                         #standar deviasi populasi
n = length(x)                 #banyak observasi
alpha = 0.05                  #taraf signifikansi
# Statistika Uji (Bandingkan z hitung dengan z tabel)
z = (xbar-mu0)/(sigma/sqrt(n))     #z hitung
z.lower = qnorm(alpha)             #z tabel eka arah
z.upper = qnorm(1-alpha)           #z tabel eka arah
z.half.alpha = qnorm(1-alpha/2)    #z tabel dwi arah
z.twosided = c(-z.half.alpha, z.half.alpha)
# P-Value (Bandingkan p-value dengan alpha)
pval.lower = pnorm(z)                        #eka arah
pval.upper = pnorm(z, lower.tail = FALSE)    #eka arah
pval.twosided = 2*pnorm(z)                   #dwi arah
# Cara Otomatis
library(TeachingDemos)
z.test(x, mu=mu0, sd=sigma, alternative = c("two.sided", "less", "greater"), conf.level = 0.95)

# Variansi tidak dikeathui
# Variansi diketahui
# Cara Manual
# Input
x                             #data
xbar = mean(x)                #mean sampel
mu0                           #nilai hipotesis
s = sd(x)                     #standar deviasi sampel
n = length(x)                 #banyak observasi
alpha = 0.05                  #taraf signifikansi
# Statistika Uji (Bandingkan t hitung dengan t tabel)
t = (xbar-mu0)/(s/sqrt(n))            #t hitung
t.lower = qt(alpha, df=n-1)           #t tabel eka arah
t.upper = qt(1-alpha, df=n-1)         #t tabel eka arah
t.half.alpha = qt(1-alpha/2, df=n-1)  #t tabel dwi arah
t.twosided = c(-t.half.alpha, t.half.alpha)
# P-Value (Bandingkan p-value dengan alpha)
pval.lower = pt(t, df=n-1)                      #eka arah
pval.upper = pt(t, df=n-1, lower.tail = FALSE)  #eka arah
pval.twosided = 2*pt(t, df=n-1)                 #dwi arah
#Cara Otomatis
t.test(x, mu=mu0, alternative = c("two.sided","less", "greater"), conf.level = 0.95)

# Contoh Soal 1
library(readxl)
x <- read_excel("DATA UJI HIPOTESIS.xlsx", sheet = "contoh diameter logam")
View(x)
x = as.numeric(x$`diameter potongan logam`)
xbar = mean(x)
mu0 = 1.09
s = sd(x)
n = length(x)
alpha = 0.05
#Cara Manual (Bandingkan t hitung dengan t tabel)
(t = (xbar-mu0)/(s/sqrt(n)))            #t hitung
(t.lower = qt(alpha, df=n-1))           #t tabel eka arah
# P-Value (Bandingkan p-value dengan alpha)
(pval.lower = pt(t, df=n-1))            #eka arah
#Cara Otomatis
t.test(x, mu=mu0, alternative = "less", conf.level = 0.95)

# Uji Hipotesis Selisih Rataan 2 Populasi
# 1. Kasus variansi 1 dan 2 diketahui
# Input
x1,x2
xbar1 = mean(x1)
xbar2 = mean(x2)
mu0
sigma1,sigma2
n1,n2
alpha = 0.05
# Statistika Uji (Bandingkan t hitung dengan t tabel)
xbar = xbar1-xbar2
z = (xbar-mu0)/sqrt((sigma1^2/n1)+(sigma2^2/n2))
z.lower = qnorm(alpha)             #z tabel eka arah
z.upper = qnorm(1-alpha)           #z tabel eka arah
z.half.alpha = qnorm(1-alpha/2)    #z tabel dwi arah
z.twosided = c(-z.half.alpha, z.half.alpha)
# P-Value (Bandingkan p-value dengan alpha)
pval.lower = pnorm(z)                        #eka arah
pval.upper = pnorm(z, lower.tail = FALSE)    #eka arah
pval.twosided = 2*pnorm(z)                   #dwi arah

# 2a. Kasus variansi 1 dan 2 tidak diketahui dan dianggap sama
# Input
x1,x2
xbar1 = mean(x1)
xbar2 = mean(x2)
mu0
s1 = sd(x1)
s2 = sd(x2)
n1,n2
alpha = 0.05
# Cara Manual
df = n1+n2-2
Sp = (((n1-1)*s1^2)+((n2-1)*s2^2))/(df)
xbar = xbar1-xbar2
t = (xbar-mu0)/(sqrt(Sp)*(sqrt((1/n1)+(1/n2))))
t.lower = qt(alpha, df)           #t tabel eka arah
t.upper = qt(1-alpha, df)         #t tabel eka arah
t.half.alpha = qt(1-alpha/2, df)  #t tabel dwi arah
t.twosided = c(-t.half.alpha, t.half.alpha)
# P-Value (Bandingkan p-value dengan alpha)
pval.lower = pt(t, df)                      #eka arah
pval.upper = pt(t, df, lower.tail = FALSE)  #eka arah
pval.twosided = 2*pt(t, df)                 #dwi arah
# Cara Otomatis
t.test(x1, x2, mu=mu0, var.equal = TRUE, alternative = c("two.sided","less", "greater"), conf.level = 0.95)

# 2b. Kasus variansi 1 dan 2 tidak diketahui dan dianggap berbeda
# Input
x1,x2
xbar1 = mean(x1)
xbar2 = mean(x2)
mu0
s1 = sd(x1)
s2 = sd(x2)
n1,n2
alpha = 0.05
# Cara Manual
df = ((s1^2/n1)+(s2^2/n2))^2/(((1/(n1-1))*(s1^2/n1)^2)+((1/(n2-1))*(s2^2/n2)^2))
xbar = xbar1-xbar2
t = (xbar-mu0)/(sqrt((s1^2/n1)+(s2^2/n2)))
t.lower = qt(alpha, df)           #t tabel eka arah
t.upper = qt(1-alpha, df)         #t tabel eka arah
t.half.alpha = qt(1-alpha/2, df)  #t tabel dwi arah
t.twosided = c(-t.half.alpha, t.half.alpha)
# P-Value (Bandingkan p-value dengan alpha)
pval.lower = pt(t, df=n-1)                      #eka arah
pval.upper = pt(t, df=n-1, lower.tail = FALSE)  #eka arah
pval.twosided = 2*pt(t, df=n-1)                 #dwi arah
# Cara Otomatis
t.test(x1, x2, mu=mu0, var.equal = FALSE, alternative = c("two.sided","less", "greater"), conf.level = 0.95)

# Uji Hipotesis Rataan Berpasangan
# Input
d = x1-x2
dbar = mean(d)
mu0
sd = sd(d)
n = length(d)
alpha = 0.05
# Cara Manual
t = (dbar-mu0)/(sd/sqrt(n))
t.lower = qt(alpha, df=n-1)
t.upper = qt(1-alpha)
t.half.alpha = qt(1-alpha/2)
t.twosided = c(-t.half.alpha, t.half.alpha)
# P-Value (Bandingkan p-value dengan alpha)
pval.lower = pt(t, df=n-1)
pval.upper = pt(t, df=n-1, lower.tail = FALSE)
pval.twosided = 2*pt(t, df=n-1)
# Cara Otomatis
t.test(x1, x2, mu=mu0, paired = T, alternative = c("two.sided", "less", "greater"), conf.level = 0.95)

# Contoh Soal 2
x <- read_excel("DATA UJI HIPOTESIS.xlsx", sheet = "latihan no 6")
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
alpha = 0.05
# Cara Manual
(df = ((s1^2/n1)+(s2^2/n2))^2/(((1/(n1-1))*(s1^2/n1)^2)+((1/(n2-1))*(s2^2/n2)^2)))
(xbar = xbar1-xbar2)
(t = (xbar-mu0)/(sqrt((s1^2/n1)+(s2^2/n2))))
(t.lower = qt(alpha, df))
# P-Value (Bandingkan p-value dengan alpha)
(pval.lower = pt(t, df))                      
# Cara Otomatis
t.test(x1, x2, mu=mu0, var.equal = FALSE, alternative = "less", conf.level=0.95)

# Uji Hipotesis Variansi 1 Populasi
# Input
x
sigma0
s = sd(x)
n = length (d)
alpha = 0.05
# Cara Manual (Bandingkan chi hitung dan chi tabel)
chi = (n-1)*s^2/sigma0
chi.lower = qchisq(alpha, df=n-1)
chi.upper = qchisq(1-alpha, df=n-1)
chi.half.alpha = qchisq(1-alpha/2, df=n-1)
chi.twosided = c(-chi.half.alpha, chi.half.alpha)
# P-Value (Bandingkan p-value dengan alpha)
pval.lower = pchisq(chi, df=n-1)
pval.upper = pchisq(chi, df=n-1, lower.tail = FALSE)
pval.twosided = 2*pchisq(chi, df=n-1)
# Cara Otomatis
library(TeachingDemos)
sigma.test (x, sigma=sqrt(sigma0), alternative = c("two.sided", "less", "greater"), conf.level = 0.95)

# Uji Hipotesis Variansi 2 Populasi
# Input
x1, x2
s1 = sd(x1)
s2 = sd(x2)
n1 = length(x1)
n2 = length(x2)
alpha = 0.05
# Cara Manual
F = s1^2/s2^2
F.lower = qf(alpha, n1-1, n2-1)
F.upper = qf(1-alpha, n1-1, n2-1)
F.half.alpha = qf(1-alpha/2, n1-1, n2-1)
F.twosided = c(-F.half.alpha, F.half.alpha)
# P-Value (Bandingkan p-value dengan alpha)
pval.lower = pf(F, n1-1, n2-1)
pval.upper = pf(F, n1-1, n2-1, lower.tail = FALSE)
pval.twosided = 2*pf(F, n1-1, n2-1)
# Cara Otomatis
var.test(x1, x2, ratio = 1, alternative = c("two.sided", "less", "greater"), conf.level = 0.95)

# Contoh Soal 3
x <- read_excel("DATA UJI HIPOTESIS.xlsx", sheet = "contoh diameter logam")
View(x)
x = as.numeric(x$`diameter potongan logam`)
s = sd(x)
sigma0 = 0.01^2
n = length(x)
alpha = 0.05
# Cara Manual (Bandingkan chi hitung dan chi tabel)
(chi = (n-1)*s^2/sigma0)
(chi.upper = qchisq(1-alpha, df=n-1))
# P-Value (Bandingkan p-value dengan alpha)
(pval.upper = pchisq(chi, df=n-1, lower.tail = FALSE))
# Cara Otomatis
library(TeachingDemos)
sigma.test (x, sigma=sqrt(sigma0), alternative = "greater", conf.level = 0.95)

# Uji Kenormalan
ks.test(x, "pnorm")
# Contoh
x <- c(5.6, 5.4, 4.3, 7.4, 5, 6.67, 6.3, 4.8, 7.62, 4.56, 6.43, 5.5)
ks.test(x, "pnorm")

# Uji Kebebasan
chisq.test(s)
# Contoh
x <- c(27,13,35,15,33,27,25,25)
(s<-matrix(x, 2, 4))
chisq.test(s)


