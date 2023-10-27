# DISTRIBUSI BINOMIAL
# X~Bin(n,p), n:banyaknya percobaan, p:peluang sukses
# x : nilai titik yang akan dihitung peluangnya

#Fungsi Distribusi Kumulatif P(X<=x)
Fx = pbinom(x,n,p)

#Fungsi Kepadatan Peluang P(X=x)
Px = dbinom(x,n,p)

#Fungsi Invers P(X<=x)=I
Qx = qbinom(I,n,p)

#Pembangkitan nilai acak
#s : banyaknya bilangan yang akan dibangkitkan 
pa = rbinom(s,n,p)

#contoh 1
# x : peubah acak banyaknya suku cadang yang dapat menahan goncangan
#diketahui n=4, p=0.75

#a. P(1<X<=3) = P(X<=3) - P(X<=1)
pbinom(3,4,0.75)-pbinom(1,4,0.75)

#b. P(X=2)
dbinom(2,4,0.75)

# DITRIBUSI POISSON
# X~Poi(lambda), lambda:rata-rata banyaknya kejadian pada suatu selang waktu atau daerah tertentu
# x : nilai titik yang akan dihitung peluangnya

#Fungsi Distribusi Kumulatif P(X<=x)
Fx = ppois(x,lambda)

#Fungsi Kepadatan Peluang P(X=x)
Px = dpois(x,lambda)

#Fungsi Invers P(X<=x)=I
Qx = qpois(I,lambda)

#Pembangkitan nilai acak
#s : banyaknya bilangan yang akan dibangkitkan
pa = rpois(s,lambda)

#contoh 2
# x : peubah acak banyaknya kecelakaan dalam seminggu
#diketahui lambda=3

#a. P(X=5)
dpois(5,3)

#b. P(X<3) = P(X<=2)
ppois(2,3)

#c. P(X>=2) = 1-P(X<=1)
1-ppois(1,3)

# DISTRIBUSI NORMAL
# X~N(mu,sigma2), mu:mean, sigma2:variansi, sd:standar deviasi
# x : nilai titik yang akan dihitung peluangnya

#Fungsi Distribusi Kumulatif P(X<=x)
Fx = pnorm(x,mu,sd)

#Fungsi Kepadatan Peluang P(X=x)
Px = dnorm(x,mu,sd)

#Fungsi Invers P(X<=x)=I
Qx = qnorm(I,mu,sd)

#Pembangkitan nilai acak
#s : banyaknya bilangan yang akan dibangkitkan
pa = rnorm(s,mu,sd)

#contoh 3
# x : peubah acak panjang pemuaian batang baja
#diketahui mu=0.05, sd=0.01

#a. P(X>0.1) = 1 - (P<=0.1)
1-pnorm(0.1,0.05,0.01)

#b. P(0.025<X<0.065) = P(X<=0.065)-P(X<=0.025)
pnorm(0.065,0.05,0.01)-pnorm(0.025,0.05,0.01)

#c. P(X<0.04)
pnorm(0.04,0.05,0.01)

# DISTRIBUSI EKSPONENSIAL
# X~exp(rate), rate:laju menghasilkan suatu kejadian
# x : nilai titik yang akan dihitung peluangnya

#Fungsi Distribusi Kumulatif P(X<=x)
Fx = pexp(x,rate)

#Fungsi Kepadatan Peluang P(X=x)
Px = dexp(x,rate)

#Fungsi Invers P(X<=x)=I
Qx = qexp(I,rate)

#Pembangkitan nilai acak
#s : banyaknya bilangan yang akan dibangkitkan
pa = rexp(s,rate)

#contoh 4
# x : peubah acak waktu kedatangan pengunjung
#diketahui rate=5.8

#a. P(X<=15/50) = P(X<=0.3)
pexp(0.3,5.8)

#b. P(20/50<X<=30/50) = P(X<=0.6)-P(X<=0.4)
pexp(0.6,5.8)-pexp(0.4,5.8)
