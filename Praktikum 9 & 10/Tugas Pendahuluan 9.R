# Mengaktifkan library
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(maptools)

# Input Data
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 9/DATA VARIOGRAM.xlsx", sheet = "Sumur Jatibarang")
View(data)
datasumurjtb <- data.frame(data)

# Mengubah "Data Frame" menjadi "Spatial Point Data Frame"
coordinates(datasumurjtb) = ~x+y

# Melihat cuplikan informasi dari data
glimpse(datasumurjtb)

# Membuat diagram pencar dengan ukuran titik menunjukkan besar kecilnya nilai di lokasi tersebut
ggplot(data = as.data.frame(datasumurjtb), aes(x,y)) + 
  geom_point(aes(size = k.fracture),col = "blue", alpha = 0.6) +
  ggtitle("Permeabilitas (mD)") + coord_equal() + theme_bw()

# Menghitung variogram eksperimental kemudian plot secara otomatis
(vgm1 <- variogram(k.fracture ~ 1, datasumurjtb))
plot(vgm1, cex = 1)

# Memodifikasi interval jarak dengan menambahkan width pada fungsi variogram
(vgm2 <- variogram(k.fracture ~ 1, datasumurjtb, width = 0.05))
plot(vgm2, cex = 1)

# Memodifikasi dengan transformasi (misalkan dengan logaritma narutal)
(vgm.ln.1 <- variogram(log(k.fracture) ~ 1, datasumurjtb))
plot(vgm.ln.1, main = "var.ln", cex = 1)

# Memodifikasi dengan transformasi (misalkdan dengan logaritma natural dan memperlebar interval kelas)
(vgm.ln.2 <- variogram(log(k.fracture) ~ 1, datasumurjtb, width = 0.05))
plot(vgm.ln.2, main = "var.ln wdth = 0.05", cex = 1)

# Melihat contoh-contoh model variogram yang dapat dimodelkan R melalui fungsi vgm
show.vgms()
vgm()

# Menampilkan model variogram eksperimental terbaik di antara model ("Exp", "Sph", "Gau")
# Fitting Variogram
(fit <- fit.variogram(vgm.ln.2, model = vgm(model = c("Exp", "Sph", "Gau"))))
plot(vgm.ln.2, fit)

# Membuat grid untuk melihat peta kontur sebaran data untuk lokasi tidak terobservasi dengan metode ordinary kriging
# n : ukuran grid, sesuaikan dengan koordinat data
n <- 0.02
kolom <- seq(datasumurjtb@bbox[1,1]-n, datasumurjtb@bbox[1,2]+n, by = n)
baris <- seq(datasumurjtb@bbox[2,1]-n, datasumurjtb@bbox[2,2]+n, by = n)
the.grid <- expand.grid(x = kolom, y = baris)
coordinates(the.grid) <- ~x+y
gridded(the.grid) <- T

# Plot grid dan titik observasi
par(mar=c(1,1,1,1))
plot(the.grid, cex = 0.5, col = "grey")
points(datasumurjtb, pch=1, col="red", cex = 1)

# Melakukan penaksiran untuk membentuk peta kontur dengan ordinary kriging
kriging <- krige(k.fracture~1, datasumurjtb, the.grid, model = fit)

#Plot kontur sebagai hasil taksiran oleh kriging
titik <- SpatialPoints(datasumurjtb@coords)
LayoutPoints <- list('sp.points', titik, pch = 19, cex = 0.8, col = 'red')

# Sesuaikan kolom data yang digunakan pada DataSumurJTB[[n]]
LayoutLabels <- list('sp.pointLabel', titik, label = as.character(datasumurjtb[[4]]), cex = 0.8, col = 'white')
spplot(kriging["var1.pred"], main = "Kontur dan Data", sp.layout = list(LayoutPoints,LayoutLabels))

# Menentukan koordinat
# contoh 5 lokasi (0.2,-1), (0.4,-0.8), (0.8,-1.0), (1,-1.4), dan (1.2,-0.5)
titik2 <- SpatialPoints(cbind(c(0.2,0.4,0.8,1,1.2), c(-1,-0.8,-1,-1.4,-0.5)))
taksiran <- krige(k.fracture~1, datasumurjtb, titik2, model=fit)[[1]]
LayoutPoints.T <- list('sp.points', titik2, pch = 19, cex = 0.8, col = "green")
LayoutLabels.T <- list('sp.pointLabel', titik2, label = as.character(taksiran), cex = 0.8, col = 'white')
spplot(kriging["var1.pred"], main = "Set Ordinary Kriging Prediction", sp.layout = list(LayoutPoints.T, LayoutLabels.T))


# Latihan
library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 9/DATA VARIOGRAM.xlsx", sheet = "latihan no 1")
View(data)
df <- data.frame(data)

# Mengubah "Data Frame" menjadi "Spatial Point Data Frame"
coordinates(df) = ~x+y

# Melihat cuplikan informasi dari data
glimpse(df)

# Membuat diagram pencar dengan ukuran titik menunjukkan besar kecilnya nilai di lokasi tersebut
ggplot(data = as.data.frame(df), aes(x,y)) +
  geom_point(aes(size = z),col = "blue", alpha = 0.6) + ggtitle("Peternakan") + coord_equal() + theme_bw()

# Menghitung semivariogram eksperimental
(vgm.ln.2 <- variogram(log(z) ~ 1, df, width = 200))
plot(vgm.ln.2, main = "var.ln wdth = 200", cex = 1)

# Menampilkan model variogram eksperimental terbaik di antara model ("Exp", "Sph", "Gau")
# Fitting Variogram
(fit <- fit.variogram(vgm.ln.2, model = vgm(model = c("Exp", "Sph", "Gau"))))
plot(vgm.ln.2, fit)

# Membuat grid untuk melihat peta kontur sebaran data untuk lokasi tidak terobservasi dengan metode ordinary kriging
# n : ukuran grid, sesuaikan dengan koordinat data
n <- 50
kolom <- seq(df@bbox[1,1]-n, df@bbox[1,2]+n, by = n)
baris <- seq(df@bbox[2,1]-n, df@bbox[2,2]+n, by = n)
the.grid <- expand.grid(x = kolom, y = baris)
coordinates(the.grid) <- ~x+y
gridded(the.grid) <- T

# Plot grid dan titik observasi
par(mar=c(1,1,1,1))
plot(the.grid, cex = 0.5, col = "grey")
points(df, pch=1, col="red", cex = 1)

# Melakukan penaksiran untuk membentuk peta kontur dengan ordinary kriging
kriging <- krige(z~1, df, the.grid, model = fit)

#Plot kontur sebagai hasil taksiran oleh kriging
titik <- SpatialPoints(df@coords)
LayoutPoints <- list('sp.points', titik, pch = 19, cex = 0.8, col = 'red')

# Sesuaikan kolom data yang digunakan pada DataSumurJTB[[n]]
LayoutLabels <- list('sp.pointLabel', titik, label = as.character(df[[2]]), cex = 0.8, col = 'white')
spplot(kriging["var1.pred"], main = "Kontur dan Data", sp.layout = list(LayoutPoints,LayoutLabels))

