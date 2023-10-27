# Latihan Soal 1
library(readxl)
minyakmentah <- read_excel("DATA ANOVA.xlsx", sheet = "latihan no 1")
View(minyakmentah)

#Bentuk data menjadi sebuah vektor
r = c(t(as.matrix(minyakmentah)))

#Definisikan variabel baru untuk perlakuan dan banyak pengamatan
f = c("JAN", "FEB", "MAR")     #faktor variabel perlakuan
k=3      #banyak perlakuan
n=10      #banyak pengamatan per perlakuan
N=30      #banyak seluruh pengamatan

#Vektor faktor perlakuan sesuai vektor r
tm = gl(k,1,n*k,factor(f))     #vektor perlakuan

#Function
av = aov(r ~ tm)
summary(av)      #tabel ANOVA

#F critical atau tabel
df1 = k-1
df2 = N-k
alpha = 0.05
Fcrit = qf(1-alpha,df1,df2)
Fcrit


# Latihan Soal 4
library(readxl)
statdas <- read_excel("DATA ANOVA.xlsx", sheet="latihan no 4")
View(statdas)

#Bentuk data menjadi sebuah vektor
r1 = c(t(as.matrix(statdas$A)))
r2 = c(t(as.matrix(statdas$B)))
r3 = c(t(as.matrix(statdas$C)))
r1 = na.omit(r1)        #menghapus missing value
r3 = na.omit(r3)
r=c(r1,r2,r3)

#Definisikan variabel baru untuk perlakuan dan banyak pengamatan
f = c("A", "B", "C")     #faktor variabel perlakuan
k=3                #banyak perlakuan
n1=length(r1)      #banyak pengamatan pada perlakuan ke-1
n2=length(r2)      #banyak pengamatan pada perlakuan ke-2
n3=length(r3)      #banyak pengamatan pada perlakuan ke-3

N=n1+n2+n3         #banyak seluruh pengamatan

#Vektor faktor perlakuan sesuai vektor r
tm = factor(rep(f,times=c(n1,n2,n3)))     

#Function
av = aov(r ~ tm)
summary(av)      #tabel ANOVA

#F critical atau tabel
df1 = k-1
df2 = N-k
alpha = 0.05
Fcrit = qf(1-alpha,df1,df2)
Fcrit


# Latihan Soal 5
library(readxl)
prestasi <- read_excel("DATA ANOVA.xlsx", sheet = "latihan no 5")
View(prestasi)

#Bentuk data menjadi sebuah vektor
r = c(t(as.matrix(prestasi)))

#Definisikan variabel baru untuk perlakuan dan banyak pengamatan
f = c("Kurang", "Cukup", "Baik", "Sangat Baik")     #faktor variabel perlakuan
k=4      #banyak perlakuan
n=3      #banyak pengamatan per perlakuan
N=12      #banyak seluruh pengamatan

#Vektor faktor perlakuan sesuai vektor r
tm = gl(k,1,n*k,factor(f))     #vektor perlakuan

#Function
av = aov(r ~ tm)
summary(av)      #tabel ANOVA

#F critical atau tabel
df1 = k-1
df2 = N-k
alpha = 0.05
Fcrit = qf(1-alpha,df1,df2)
Fcrit
