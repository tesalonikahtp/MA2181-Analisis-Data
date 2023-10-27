# Latsol 1
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(maptools)

library(readxl)
data <- read_excel("DATA VARIOGRAM.xlsx", sheet = "latihan no 1")
View(data)
df <- data.frame(data)

# Mengubah "Data Frame" menjadi "Spatial Point Data Frame"
coordinates(df) = ~x+y

# Melihat cuplikan informasi dari data
glimpse(df)

# a.
ggplot(data = as.data.frame(df), aes(x,y)) + 
  geom_point(aes(size = z),col = "blue", alpha = 0.6) +
  ggtitle("Sumur") + coord_equal() + theme_bw()

# b. dan c.
# kedalaman
(vgm.ln.2 <- variogram(log(z) ~ 1, df, width = 200))
plot(vgm.ln.2)

(fit <- fit.variogram(vgm.ln.2, model = vgm(model = c("Exp", "Sph", "Gau"))))
plot(vgm.ln.2, fit)

n <- 50
kolom <- seq(df@bbox[1,1]-n, df@bbox[1,2]+n, by = n)
baris <- seq(df@bbox[2,1]-n, df@bbox[2,2]+n, by = n)
the.grid <- expand.grid(x = kolom, y = baris)
coordinates(the.grid) <- ~x+y
gridded(the.grid) <- T

par(mar=c(1,1,1,1))
plot(the.grid, cex = 0.5, col = "grey")
points(df, pch=1, col="red", cex = 1)

kriging <- krige(z~1, df, the.grid, model = fit)
titik <- SpatialPoints(df@coords)
LayoutPoints <- list('sp.points', titik, pch = 19, cex = 0.8, col = 'red')
LayoutLabels <- list('sp.pointLabel', titik, label = as.character(df[[2]]), cex = 0.8, col = 'white')
spplot(kriging["var1.pred"], main = "Kontur dan Data", sp.layout = list(LayoutPoints,LayoutLabels))


# Latsol 2
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(maptools)

library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 9/DATA VARIOGRAM.xlsx", 
                             sheet = "latihan no 2")
View(data)
df <- data.frame(data)

# Mengubah "Data Frame" menjadi "Spatial Point Data Frame"
coordinates(df) = ~Bujur+Lintang

# Melihat cuplikan informasi dari data
glimpse(df)

# a.
ggplot(data = as.data.frame(df), aes(Bujur,Lintang)) + 
  geom_point(aes(size = pH),col = "blue", alpha = 0.6) +
  ggtitle("pH") + coord_equal() + theme_bw()

# b. dan c.
# pH
(vgm.ln.2 <- variogram(log(pH) ~ 1, df, width = 200))
plot(vgm.ln.2)

(fit <- fit.variogram(vgm.ln.2, model = vgm(model = c("Exp", "Sph", "Gau"))))
plot(vgm.ln.2, fit)

n <- 5000000
kolom <- seq(df@bbox[1,1]-n, df@bbox[1,2]+n, by = n)
baris <- seq(df@bbox[2,1]-n, df@bbox[2,2]+n, by = n)
the.grid <- expand.grid(Bujur = kolom, Lintang = baris)
coordinates(the.grid) <- ~Bujur+Lintang
gridded(the.grid) <- T

par(mar=c(1,1,1,1))
plot(the.grid, cex = 0.5, col = "grey")
points(df, pch=1, col="red", cex = 1)

kriging <- krige(pH~1, df, the.grid, model = fit)
titik <- SpatialPoints(df@coords)
LayoutPoints <- list('sp.points', titik, pch = 19, cex = 0.8, col = 'red')
LayoutLabels <- list('sp.pointLabel', titik, label = as.character(df[[2]]), cex = 0.8, col = 'white')
spplot(kriging["var1.pred"], main = "Kontur dan Data", sp.layout = list(LayoutPoints,LayoutLabels))


# Latsol 3
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(maptools)

library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 9/DATA VARIOGRAM.xlsx", 
                             sheet = "latihan no 3")
View(data)
df <- data.frame(data)

# Mengubah "Data Frame" menjadi "Spatial Point Data Frame"
coordinates(df) = ~Koordinat.X+Koordinat.Y

# Melihat cuplikan informasi dari data
glimpse(df)

# a.
ggplot(data = as.data.frame(df), aes(Koordinat.X,Koordinat.Y)) + 
  geom_point(aes(size = Hg..ppb.),col = "blue", alpha = 0.6) +
  ggtitle("Hg") + coord_equal() + theme_bw()

# b. dan c.
# Hg
(vgm.ln.2 <- variogram(log(Hg..ppb.) ~ 1, df, width = 200))
plot(vgm.ln.2)

(fit <- fit.variogram(vgm.ln.2, model = vgm(model = c("Exp", "Sph", "Gau"))))
plot(vgm.ln.2, fit)

n <- 50
kolom <- seq(df@bbox[1,1]-n, df@bbox[1,2]+n, by = n)
baris <- seq(df@bbox[2,1]-n, df@bbox[2,2]+n, by = n)
the.grid <- expand.grid(Koordinat.X = kolom, Koordinat.Y = baris)
coordinates(the.grid) <- ~Koordinat.X+Koordinat.Y
gridded(the.grid) <- T

par(mar=c(1,1,1,1))
plot(the.grid, cex = 0.5, col = "grey")
points(df, pch=1, col="red", cex = 1)

kriging <- krige(Hg..ppb.~1, df, the.grid, model = fit)
titik <- SpatialPoints(df@coords)
LayoutPoints <- list('sp.points', titik, pch = 19, cex = 0.8, col = 'red')
LayoutLabels <- list('sp.pointLabel', titik, label = as.character(df[[2]]), cex = 0.8, col = 'white')
spplot(kriging["var1.pred"], main = "Kontur dan Data", sp.layout = list(LayoutPoints,LayoutLabels))


# Latsol 4
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(maptools)

library(readxl)
data <- read_excel("R/Praktikum Andat/Praktikum 9/DATA VARIOGRAM.xlsx", 
                             sheet = "latihan no 4")
View(data)
df <- data.frame(data)

# Mengubah "Data Frame" menjadi "Spatial Point Data Frame"
coordinates(df) = ~x+y

# Melihat cuplikan informasi dari data
glimpse(df)

# a.
ggplot(data = as.data.frame(df), aes(x,y)) + 
  geom_point(aes(size = Peternakan),col = "blue", alpha = 0.6) +
  ggtitle("Hg") + coord_equal() + theme_bw()

# b. dan c.
# Peternakan
(vgm.ln.2 <- variogram(log(Peternakan) ~ 1, df, width = 0.05))
plot(vgm.ln.2)

(fit <- fit.variogram(vgm.ln.2, model = vgm(model = c("Exp", "Sph", "Gau"))))
plot(vgm.ln.2, fit)

n <- 5
kolom <- seq(df@bbox[1,1]-n, df@bbox[1,2]+n, by = n)
baris <- seq(df@bbox[2,1]-n, df@bbox[2,2]+n, by = n)
the.grid <- expand.grid(x = kolom, y = baris)
coordinates(the.grid) <- ~x+y
gridded(the.grid) <- T

par(mar=c(1,1,1,1))
plot(the.grid, cex = 0.5, col = "grey")
points(df, pch=1, col="red", cex = 1)

kriging <- krige(Peternakan~1, df, the.grid, model = fit)
titik <- SpatialPoints(df@coords)
LayoutPoints <- list('sp.points', titik, pch = 19, cex = 0.8, col = 'red')
LayoutLabels <- list('sp.pointLabel', titik, label = as.character(df[[2]]), cex = 0.8, col = 'white')
spplot(kriging["var1.pred"], main = "Kontur dan Data", sp.layout = list(LayoutPoints,LayoutLabels))


# Latsol 5
library(sp)
library(gstat)
library(dplyr)
library(ggplot2)
library(maptools)

library(readxl)
data <- read_excel("DATA VARIOGRAM.xlsx", 
                             sheet = "latihan no 5")
View(data)
df <- data.frame(data)

# Mengubah "Data Frame" menjadi "Spatial Point Data Frame"
coordinates(df) = ~x+y

# Melihat cuplikan informasi dari data
glimpse(df)

# a.
ggplot(data = as.data.frame(df), aes(x,y)) + 
  geom_point(aes(size = CV..BTU.lb.),col = "blue", alpha = 0.6) +
  ggtitle("CV") + coord_equal() + theme_bw()

# b. dan c.
# CV
(vgm.ln.2 <- variogram(log(CV..BTU.lb.) ~ 1, df, width = 25))
plot(vgm.ln.2)

(fit <- fit.variogram(vgm.ln.2, model = vgm(model = c("Exp", "Sph", "Gau"))))
plot(vgm.ln.2, fit)

n <- 50
kolom <- seq(df@bbox[1,1]-n, df@bbox[1,2]+n, by = n)
baris <- seq(df@bbox[2,1]-n, df@bbox[2,2]+n, by = n)
the.grid <- expand.grid(x = kolom, y = baris)
coordinates(the.grid) <- ~x+y
gridded(the.grid) <- T

par(mar=c(1,1,1,1))
plot(the.grid, cex = 0.5, col = "grey")
points(df, pch=1, col="red", cex = 1)

kriging <- krige(CV..BTU.lb.~1, df, the.grid, model = fit)
titik <- SpatialPoints(df@coords)
LayoutPoints <- list('sp.points', titik, pch = 19, cex = 0.8, col = 'red')
LayoutLabels <- list('sp.pointLabel', titik, label = as.character(df[[2]]), cex = 0.8, col = 'white')
spplot(kriging["var1.pred"], main = "Kontur dan Data", sp.layout = list(LayoutPoints,LayoutLabels))
