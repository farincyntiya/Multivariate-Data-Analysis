#### ANALISIS KORELASI KANONIK #####

library(CCA)
library(CCP)

#===== Input Data
data<-read.csv(file.choose(),header=T,sep=";",dec=",")
View(data)
str(data)


## MISAL:
y1=data$Uang.Beredar.Sempit
y2=data$Uang.Kuasi
y3=data$Surat.Berharga.selain.Saha.
x1=data$Aktivitas.Luar.Negeri.Bersih
x2=data$Aktivitas.Dalam.Negeri.Bersih
x3=data$Tagihan.Bersih.kepada.Pemerintah.Pusat

data1=data.frame(cbind(y1,y2,y3,x1,x2,x3))
X<-cbind(x1,x2,x3)    # set variabel pertama
head(X)
Y<-cbind(y1,y2,y3)    # set variabel kedua
head(Y)

#===== Uji Asumsi
#== Uji Normalitas Multivariat
# H0: Data penelitian yang digunakan berdistribusi multivariat normal
# H1: Data  penelitian yang digunakan tidak berdistribusi multivariat 
library(MVN)
mvn(data=data1, mvnTest="mardia")
# Kriteria Uji: Tolak H0 jika pval < alpha, terima dalam hal lainnya


#== Uji Multikolinieritas
# H0: Tidak terdapat multikolinieritas pada data penelitian yang digunakan
# atau seluruh variabel dalam data penelitian yang digunakan tidak berkorelasi antar variabel lainnya
# H1: Terdapat multikolinieritas pada data penelitian yang digunakan
# atau seluruh variabel dalam data penelitian yang digunakan berkorelasi antar variabel lainnya
diag(solve(cor(data1)))


#== Uji Homogenitas Varians
# H0: Seluruh variabel (X1,X2,...,Xn) pada data penelitian yang digunakan memiliki varians yang homogen
# H1: Seluruh variabel (X1,X2,...,Xn) pada data penelitian yang digunakan  memiliki varians yang heterogen (berbeda secara signifikan)
variabel=as.factor(rep(c("y1","y2","y3","x1","x2","x3"),each=))
index=c(y1,y2,y3,x1,x2,x3)
data2=data.frame(variabel,index)
bartlett.test(index~variabel,data2)
# Kriteria Uji: Tolak H0 jika pval < alpha, terima dalam hal lainnya


#== Uji Linieritas
# H0: Tidak ada kesalahan spesifikasi terhadap model linear sehingga 
# rata-rata yang diperoleh dari kelompok data sampel terletak dalam garis lurus (linear)
# H1: Terdapat kesalahan spesifikasi terhadap model linear sehingga 
# rata-rata yang diperoleh dari kelompok data sampel tidak terletak dalam garis lurus (non linear)
library(lmtest)
resettest(Y~X)
# Kriteria Uji: Tolak H0 jika pval < alpha, terima dalam hal lainnya


#===== Evaluasi Korelasi pada Data
# ide utama dari analisis korelasi kanonik adalah mencari pasangan 
# dari kombinasi linear ini yang memiliki korelasi terbesar
correl <- matcor(X,Y)
correl

# nilai korelasi pearson antar peubah dependen, antar peubah independen, 
# dan korelasi silang antara peubah dependen dengan peubah independen
img.matcor(correl, type = 2)


#===== Analisis Korelasi Kanonik
# membentuk fungsi kanonik
library(CCA)
cc=cc(X,Y)
cc$cor
# lihat dimana korelasi yang paling tinggi
cc$ycoef     #u   #Y
cc$xcoef     #v   #X

# menguji signifikansi fungsi kanonik
library(CCP)
rho=cc$cor
rho
N=dim(X)[1];N
P=dim(X)[2];P
Q=dim(Y)[2];Q

p.asym(rho,N,P,Q,tstat="Wilks")
p.asym(rho,N,P,Q,tstat="Hotelling")
p.asym(rho,N,P,Q,tstat="Pillai")