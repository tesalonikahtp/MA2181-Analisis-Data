# Soal 1
# a.
# Input
x=c(150,203,259,269,137,135,140,144,161,136,86)
xbar = mean(x)    
S = sd(x)          
n = length(x)     
alpha = 0.02            

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

#Perhitungan otomatis
t.test(x)

# b.
#Input
S = var(x)       
n = length(x)    
alpha = 0.05    

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

# c.
# Input
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

#Perhitungan otomatis
t.test(x)




# Soal 2
# a.
# Input
library(readxl)
mydata <- read_excel("Curah Hujan.xlsx")
View(mydata)
x1<-mydata$`Kabupaten A`
x2<-mydata$`Kabupaten B`
xbar1<-mean(x1)
xbar2<-mean(x2)
s1<-var(x1)
s2<-var(x2)
n1<-length(x1)
n2<-length(x2)
alpha<-0.1

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
B

#Perhitungan Otomatis
t.test(x1,x2,alt="two.sided")

# b.
# Input
x1<-mydata$`Kabupaten A`
x2<-mydata$`Kabupaten C`
xbar1<-mean(x1)
xbar2<-mean(x2)
s1<-var(x1)
s2<-var(x2)
n1<-length(x1)
n2<-length(x2)
alpha<-0.1

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
B

#Perhitungan Otomatis
t.test(x1,x2,alt="two.sided")

# c.
# Input
x1<-mydata$`Kabupaten B`
x2<-mydata$`Kabupaten C`
xbar1<-mean(x1)
xbar2<-mean(x2)
s1<-var(x1)
s2<-var(x2)
n1<-length(x1)
n2<-length(x2)
alpha<-0.1

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
B

#Perhitungan Otomatis
t.test(x1,x2,alt="two.sided")




# Soal 3
# Untuk rataan
# Input
x=c(2.1,2.2,2.4,2.2,2.0,2.1,2.3,2.0,2.2)
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

#Perhitungan otomatis
t.test(x)

# Untuk variansi
# Input
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




# Soal 4
# Input
x=c(11.27,11.1,12.37,11.34,10.49,9.93,11.03,9.56,10,10.2)
xbar = mean(x)    
S = sd(x)          
n = length(x)     
alpha = 0.07            

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

#Perhitungan otomatis
t.test(x)




# Soal 5
# Untuk rataan
# Input
x=c(3.03,3.47,4.21,4.44,4.95,5.11,5.63,6.34,6.56,6.82)
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

#Perhitungan otomatis
t.test(x)

# Untuk variansi
# Input
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




# Soal 6
# Input
library(readxl)
mydata <- read_excel("Perusahaan Asuransi.xlsx")
View(mydata)
# a.
boxplot(mydata$`Bengkel I`, horizontal = T, main = "Bengkel I")
boxplot(mydata$`Bengkel II`, horizontal = T, main = "Bengkel II")
# c.
#Input
x1<-mydata$`Bengkel I`
x2<-mydata$`Bengkel II`           
xbar1<-mean(x1)   
xbar2<-mean(x2) 
s1<-var(x1)    
s2<-var(x2)    
n1<-length(x1)  
n2<-length(x2)  
alpha=0.1            

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
B

#Perhitungan Otomatis
t.test(x1,x2,alt="two.sided",var.equal = TRUE)




# Soal 7
# Input
library(readxl)
mydata <- read_excel("Majalah.xlsx")
View(mydata)
# a.
x1<-mydata$Penipuan
x2<-mydata$`Senjata Api`       
S1 = var(x1)     
S2 = var(x2)    
n1 = length (x1) 
n2 = length (x2) 
alpha = 0.05           

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
B

#Perhitungan Otomatis
var.test(x1,x2)

# b.
#Input
x1<-mydata$Penipuan
x2<-mydata$`Senjata Api`           
xbar1<-mean(x1)   
xbar2<-mean(x2) 
s1<-var(x1)    
s2<-var(x2)    
n1<-length(x1)  
n2<-length(x2)  
alpha=0.05            

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
B

#Perhitungan Otomatis
t.test(x1,x2,alt="two.sided",var.equal = TRUE)
