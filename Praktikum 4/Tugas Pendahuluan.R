#Kasus variansi populasi diketahui
#Input
x                 #data dalam vector
sigma             #standar deviasi populasi 
xbar = mean(x)    #rataan data
n = length(x)     #banyaknya observasi
alpha             #tingkat signifikansi

#Perhitungan manual
z.alpha = qnorm(1-alpha/2)
sem = sigma/sqrt(n)
E = z.alpha*sem

#Batas Bawah 
LB = xbar - E
#Batas Atas
UB = xbar + E

#Selang Kepercayaan
B = xbar + c(-E,E)

#Perhitungan otomatis
#Package TeachingDemos
Library(TeachingDemos)
z.test(x, sd=sigma)


#Kasus variansi populasi tidak diketahui
#Input
x                 #data dalam vector
xbar = mean(x)    #rataan data
S = sd(x)         #standar deviasi populasi 
n = length(x)     #banyaknya observasi
alpha             #tingkat signifikansi

#Perhitungan manual
t.alpha = qt(1-alpha/2, df=n-1)
sem = S/sqrt(n)
E = t.alpha*sem

#Batas Bawah 
LB = xbar - E
#Batas Atas
UB = xbar + E

#Selang Kepercayaan
B = xbar + c(-E,E)

#Perhitungan otomatis
t.test(x)


#Contoh Soal 1
#Input Data
x=c(9.8,10.2,10.4,9.8,10.0,10.2,9.6,11.20,10.30,11.60,9.40,9.20,9.60,10.60,9.00,9.20)
xbar = mean(x)
S = sd(x)
n = length(x)
alpha = 0.05

#Perhitungan manual
t.alpha = qt(1-alpha/2, df=n-1)
sem = S/sqrt(n)
E = t.alpha*sem

#Batas Bawah 
LB = xbar - E
#Batas Atas
UB = xbar + E

#Selang Kepercayaan
B = xbar + c(-E,E)
B

#Perhitungan Otomatis
t.test(x)


#Kasus variansi populasi 1 dan populasi 2 diketahui
#Input
x1, x2           #data dalam vector
xbar1 = mean(x1) #rataan x1  
xbar2 = mean(x2) #rataan x2
sigma1, sigma2   #variansi populasi (??_1^2,??_2^2)
n1 = length(x1)  #banyaknya observasi x1
n2 = length(x2)  #banyaknya observasi x2
alpha            #tingkat signifikansi

#Perhitungan Manual
xbar=xbar1-xbar2
z.alpha=qnorm(1-alpha/2)
sem = sqrt((sigma1/n1)+(sigma2/n2))
E = z.alpha*sem

#Batas Bawah
LB = xbar-E
#Batas Atas
UB = xbar+E

#Selang Kepercayaan
B = xbar + c(-E,E)


#Kasus variansi dari populasi 1 dan populasi 2 tidak diketahui dan dianggap sama
#Input
x1, x2           #data dalam vector
xbar1 = mean(x1) #rataan x1  
xbar2 = mean(x2) #rataan x2
s1 = var(x1)     #variansi x1
s2 = var(x2)     #variansi x2
n1 = length(x1)  #banyaknya observasi x1
n2 = length(x2)  #banyaknya observasi x2
alpha            #tingkat signifikansi

#Perhitungan Manual
xbar=xbar1-xbar2
df=n1+n2-2
t.alpha=qt(1-alpha/2,df)
Sp = (((n1-1)*s1)+((n2-1)*s2))/(df)
sem = sqrt((1/n1)+(1/n2))
E = t.alpha*sqrt(Sp)*sem

#Batas Bawah
LB = xbar-E
#Batas Atas
UB = xbar+E

#Selang Kepercayaan
B = xbar + c(-E,E)

#Perhitungan Otomatis
t.test(x1,x2,alt="two.sided",var.equal = TRUE)


#Kasus variansi populasi 1 dan populasi 2 tidak diketahui dan tidak dianggap sama
#Input
x1, x2           #data dalam vector
xbar1 = mean(x1) #rataan x1  
xbar2 = mean(x2) #rataan x2
s1 = var(x1)     #variansi x1
s2 = var(x2)     #variansi x2
n1 = length(x1)  #banyaknya observasi x1
n2 = length(x2)  #banyaknya observasi x2
alpha            #tingkat signifikansi

#Perhitungan Manual
xbar=xbar1-xbar2
df=((s1/n1)+(s2/n2))^2/(((1/(n1-1)*(s1/n1)^2)+((1/(n2-1)*(s2/n2)^2))))
t.alpha=qt(1-alpha/2,df)
sem = sqrt((s1/n1)+(s2/n2))
E = t.alpha*sem

#Batas Bawah
LB = xbar-E
#Batas Atas
UB = xbar+E

#Selang Kepercayaan
B = xbar + c(-E,E)

#Perhitungan Otomatis
t.test(x1,x2,alt="two.sided")


#Kasus Data Berpasangan
#Input
x1, x2
d = x1 - x2
dbar = mean(d)
sd = sd(d)
n = length(d)
df = n-1

#Perhitungan Manual
t.alpha=qt(1-alpha/2,df)
sem = sd/sqrt(n)
E = t.alpha*sem

#Batas Bawah
LB = dbar-E
#Batas Atas
UB = dbar+E

#Selang Kepercayaan
B = dbar + c(-E,E)

#Perhitungan Otomatis
t.test(x1,x2, paired = T)


#Contoh Soal 2
#Input Data
library(readxl)
mydata <- read_excel("Curah Hujan.xlsx")
View(mydata)
x1<-mydata$KabupatenA
x2<-mydata$NatriumSulfat
xbar1<-mean(x1)
xbar2<-mean(x2)
s1<-var(x1)
s2<-var(x2)
n1<-length(x1)
n2<-length(x2)
alpha<-0.05

#Perhitungan manual
xbar=xbar1-xbar2
df=n1+n2-2
t.alpha=qt(1-alpha/2,df)
Sp = (((n1-1)*s1)+((n2-1)*s2))/(df)
sem = sqrt((1/n1)+(1/n2))
E = t.alpha*sqrt(Sp)*sem

#Batas Bawah
LB = xbar-E
#Batas Atas
UB = xbar+E

#Selang Kepercayaan
B = xbar + c(-E,E)
B

#Perhitungan Otomatis
t.test(x1,x2,alt="two.sided",var.equal = TRUE)


#Variansi satu populasi
#Input
x                #data
S = var(x)       #variansi data
n = length(x)    #banyak observasi data
alpha            #tingkat signifikansi

#Perhitungan manual
khi.alpha1 = qchisq(1-alpha/2,n-1)
khi.alpha2 = qchisq(alpha/2,n-1)

#Batas Bawah
LB = (n-1)*S/khi.alpha1
#Batas Atas
UB = (n-1)*S/khi.alpha2

#Selang Kepercayaan
B = c(LB,UB)

#Perhitungan Otomatis
library(TeachingDemos)
sigma.test(x, sigma=sqrt(S))


#Variansi dua populasi
x1,x2            #data
S1 = var(x1)     #variansi x1
S2 = var(x2)     #variansi x2
n1 = length (x1) #banyak observasi x1
n2 = length (x2) #banyak observasi x2
alpha            #tingkat signifikansi

#Perhitungan manual
F.alpha1=qf(1-alpha/2,n1-1,n2-1)
F.alpha2=qf(1-alpha/2,n2-1,n1-1)
E = S1/S2

#Batas Bawah
LB=E/F.alpha1
#Batas Atas
UB=E*F.alpha2

#Selang Kepercayaan
B=c(LB,UB)

#Perhitungan Otomatis
var.test(x1,x2)


#Contoh Soal 3
#Variansi satu populasi
x = c(9.8,10.2,10.4,9.8,10.0,10.2,9.6,11.20,10.30,11.60,9.40,9.20,9.60,10.60,9.00,9.20)
n=length(x)
S=var(x)
alpha=0.05

#Perhitungan manual
khi.alpha1 = qchisq(1-alpha/2,n-1)
khi.alpha2 = qchisq(alpha/2,n-1)

#Batas Bawah
LB = (n-1)*S/khi.alpha1
#Batas Atas
UB = (n-1)*S/khi.alpha2

#Selang Kepercayaan
B = c(LB,UB)
B

#Perhitungan Otomatis
library(TeachingDemos)
sigma.test(x, sigma=sqrt(S))


