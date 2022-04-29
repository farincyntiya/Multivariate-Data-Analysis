##### ANALISIS DISKRIMINAN #####

#===== Input Data
data <- read.csv(file.choose(),header=T,sep=";",dec=",")
View(data)
str(data)
# 1 = negara maju, 2 = negara berkembang

## MISAL:
x1=log(data$Pendapatan.Per.Kapita)
x2=log(data$Angka.Kematian)
x3=log(data$Tingkat.Kriminalitas)
x4=log(data$Angka.Kelahiran)
x5=log(data$Pengunjung.Pariwisata)
x6=log(data$Inflasi)
x7=log(data$Pertumbuhan.Penduduk)
x8=log(data$Pengangguran)
x9=log(data$Korupsi)

#===== Variabel Independen dan Dependen
x=cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
head(x)
y=data$Kategori
head(y)
n=nrow(data);n

#===== Uji Asumsi Analisis Diskriminan
#== Uji Normalitas Multivariat
# uji normalitas variabel bebas X
library(MVN)
mvn(data=x, mvnTest="mardia")

#== Uji Homogenitas Varians
variabel=as.factor(rep(c("x1","x2","x3","x4","x5","x6","x7","x8","x9"),each=n))
respon=c(x1,x2,x3,x4,x5,x6,x7,x8,x9)
data2=data.frame(variabel,respon)
bartlett.test(respon~variabel,data2)

#== Uji Multikolinieritas
diag(solve(cor(x)))

#===== Membentuk Fungsi Diskriminan
library(MASS)
datalog=data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9)

#===== Membentuk Matriks Varkov
# kategori negara maju
a=data.frame(datalog$x1[1:40],datalog$x2[1:40],datalog$x3[1:40],datalog$x4[1:40],datalog$x5[1:40],datalog$x6[1:40],datalog$x7[1:40],datalog$x8[1:40],datalog$x9[1:40])
# kategori negara berkembang
b=data.frame(datalog$x1[41:90],datalog$x2[41:90],datalog$x3[41:90],datalog$x4[41:90],datalog$x5[41:90],datalog$x6[41:90],datalog$x7[41:90],datalog$x8[41:90],datalog$x9[41:90])
cov(a)
cov(b)
# banyak variabel dan pengamatan pada kategori 1 = negara maju
m1=ncol(a)
m1
n1=nrow(a)
n1
# banyak variabel dan pengamatan pada kategori 2 = negara berkembang
m2=ncol(b)
m2
n2=nrow(b)
n2
# matriks varkov gabungan
sgab=(((n1-1)*cov(a))+((n2-2)*cov(b)))/(n1+n2-2)
sgab
solve(sgab)

## DISKRIMINAN FISHER
# DATA DIBAGI SESUAI SET KATEGORI
# Kategori 1 = Negara Maju
x11=matrix(datalog$x1[1:40])
x12=matrix(datalog$x2[1:40])
x13=matrix(datalog$x3[1:40])
x14=matrix(datalog$x4[1:40])
x15=matrix(datalog$x5[1:40])
x16=matrix(datalog$x6[1:40])
x17=matrix(datalog$x7[1:40])
x18=matrix(datalog$x8[1:40])
x19=matrix(datalog$x9[1:40])
xa=matrix(cbind(x11,x12,x13,x14,x15,x16,x17,x18,x19),nrow=n1,ncol=m1)
# Kategori 2 = Negara Berkembang
x21=matrix(datalog$x1[41:90])
x22=matrix(datalog$x2[41:90])
x23=matrix(datalog$x3[41:90])
x24=matrix(datalog$x4[41:90])
x25=matrix(datalog$x5[41:90])
x26=matrix(datalog$x6[41:90])
x27=matrix(datalog$x7[41:90])
x28=matrix(datalog$x8[41:90])
x29=matrix(datalog$x9[41:90])
xb=as.matrix(cbind(x21,x22,x23,x24,x25,x26,x27,x28,x29),nrow=n2,ncol=m2)

## RATA-RATA SETIAP KATEGORI DALAM MATRIKS
x1_=data.frame(mean(x11),mean(x12),mean(x13),mean(x14),mean(x15),mean(x16),mean(x17),mean(x18),mean(x19))
x1=as.matrix(t(x1_))
x1

x2_=data.frame(mean(x21),mean(x22),mean(x23),mean(x24),mean(x25),mean(x26),mean(x27),mean(x28),mean(x29))
x2=as.matrix(t(x2_))
x2

## KOEFISIEN FUNGSI DISKRIMINAN
b=t(x1-x2)%*%solve(sgab)
b

#===== Menghitung Mid Point untuk Klasifikasi Objek ke dalam Kelompok
## MENENTUKAN MID POINT
# untuk n1 = n2
mp=0.5*t(x1-x2)%*%solve(sgab)%*%(x1+x2)
mp
# untuk n1 ≠ n2
y1=t(x1-x2)%*%solve(sgab)%*%x1
y1
y2=t(x1-x2)%*%solve(sgab)%*%x2
y2
mp=((n1*y1)+(n2*y2))/(n1+n2)
mp

#===== Menguji Signifikansi Fungsi Diskriminan
p=ncol(x);p
D2=t(x1-x2)%*%solve(sgab)%*%(x1-x2)
F=((n1+n2-p-1)/((n1+n2-2)*p))*(n1*n2/(n1+n2))*D2;F
Ftab=qf(0.05,p,n1+n2-p-1,lower.tail=FALSE);Ftab
# H0: pemisahan antara dua populasi tidak signifikan
# H1: pemisahan antara dua populasi signifikan
# Kriteria uji: F hitung > F tabel = H0 ditolak = signifikan