#ANOVA ONE WAY
#Input Data
setwd("Alamat Folder")
library(readx1)
x      #Input data dalam bentuk data frame

#Bentuk data menjadi sebuah vektor
r = c(t(as.matrix(x)))

#Definisikan variabel baru untuk perlakuan dan banyak pengamatan
f = c("Variabel 1", "Varibel 2", "Variabel k")     #faktor variabel perlakuan
k      #banyak perlakuan
n      #banyak pengamatan per perlakuan
N      #banyak seluruh pengamatan

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

# Contoh Soal 1
library(readxl)
Hormone <- read_excel("R/Praktikum Andat/Praktikum 6/DATA ANOVA.xlsx", sheet = "contoh hormone")
Hormone

#Bentuk data menjadi sebuah vektor
r = c(t(as.matrix(Hormone)))

#Definisikan variabel baru untuk perlakuan dan banyak pengamatan
f = c("Au", "Si", "Gi")     #faktor variabel perlakuan
k=3      #banyak perlakuan
n=5      #banyak pengamatan per perlakuan
N=15      #banyak seluruh pengamatan

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

# Contoh Soal 2
library(readxl)
vitaminC <- read_excel("R/Praktikum Andat/Praktikum 6/DATA ANOVA.xlsx", sheet = "kadar asam")

#Bentuk data menjadi sebuah vektor
r1 = c(t(as.matrix(vitaminC$`0_hari`)))
r2 = c(t(as.matrix(vitaminC$`3_hari`)))
r3 = c(t(as.matrix(vitaminC$`7_hari`)))
r2 = na.omit(r2)        #menghapus missing value
r3 = na.omit(r3)
r=c(r1,r2,r3)

#Definisikan variabel baru untuk perlakuan dan banyak pengamatan
f = c("O hari", "3 hari", "7 hari")     #faktor variabel perlakuan
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
