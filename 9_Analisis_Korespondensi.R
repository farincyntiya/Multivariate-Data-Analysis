##### ANALISIS KORESPONDENSI #####

library(ca)
library(dplyr)
library(FactoMineR)

#===== Input Data
data <- read.csv(file.choose(),header=T,sep=";",dec=",")
str(data)
data

#=====  Mosaic Plot
rownames(data) <- data[,1]
data <- data[,-1]
# Visualisasi perbedaan jenis kejahatan antara provinsi di Pulau Sumatera
mosaicplot(data, 
           las = 2, 
           shade = T, 
           main = "Visualisasi Perbedaan Jumlah Kejahatan antara Provinsi di Pulau Sumatera")

#===== Membuat Tabel Kontingensi
# data diinput dari kiri ke kanan, lalu ke bawah, lanjut kiri ke kanan, dan seterusnya
kejahatan<-matrix(c(1094,526,729,97,1610,3903,2632,448,1770,2041,2181,264,
                    427,972,471,70,790,1252,753,71,476,1995,771,261,
                    203,520,358,28,382,1303,1021,109,230,352,119,9,
                    606,267,391,49),nrow=10,byrow=TRUE)
dimnames(kejahatan)<-list(c("Aceh","Sumatera Utara","Sumatera Barat","Riau","Jambi",
                            "Sumatera Selatan","Bengkulu","Lampung","Kep. Bangka Belitung","Kep. Riau"),
                          c("Pencurian","Pencurian dengan Pemberatan","Pencurian Kendaraan Bermotor","Penadahan"))
kejahatan
names(dimnames(kejahatan))<-c("Provinsi","Jenis Kejahatan")
kejahatan

#===== Menghitung Proporsi Tiap Baris
prop.baris  <- prop.table(kejahatan, 1)
prop.baris 

#===== Menghitung Proporsi Tiap Kolom
prop.kolom  <- prop.table(kejahatan, 2)
prop.kolom

#===== Melakukan Analisis Korespondensi
library(ca)
fit <- ca(kejahatan)
fit
summary(fit)

# Meninjau Baris
data.frame(fit$rownames,fit$rowmass,fit$rowdist,fit$rowinertia)
# Koordinat Pada Peta
fit$rowcoord

# Meninjau Kolom
data.frame(fit$colnames,fit$colmass,fit$coldist,fit$colinertia)
# Koordinat Pada Peta
fit$colcoord

#===== Membuat Grafik Pemetaan
plot(fit,main="Analisis Korespondensi Sederhana")