#### ANALISIS FAKTOR #####

library(psych)
library(tidyverse)
library(DT)

#===== Input Data
data<-read.csv(file.choose(),header=T,sep=";",dec=",")
View(data)
str(data)

#===== Uji Asumsi
#== Uji Normalitas Multivariat
library(MVN)
mvnorm=mvn(data=data,mvnTest="mardia")
mvnorm
#== Uji Multikolinieritas
diag(solve(cor(data)))

#===== Mengevaluasi Data dan Korelasinya
# Korelasi antar Variabel
correl<-mixedCor(data)
correl
# nilai korelasinya tidak ada yang 0
# mengindikasikan adanya korelasi antar variabel

# Matriks Korelasi
cor<-correl$rho
cor
# Melihat Struktur Korelasi (Visualisasi Korelasi)
library(ggcorrplot)
ggcorrplot(cor)
# dapat dilihat bahwa ada korelasi  di antara 
# variabel-variabel awal yang digunakan
# maka analisis faktor dapat dilakukan


# Menguji apakah matriks korelasi bukan merupakan matriks identitas
# agar penyusutan dimensi peubah menjadi lebih sederhana

#== Uji Bartlett
# H0: Matriks korelasi merupakan matriks identitas
# H1: Matriks korelasi bukan merupakan matriks identitas
library(psych)
cortest.bartlett(data)
# Tolak H0 jika nilai p-value < α, terima dalam hal lainnya

#== Uji KMO dan MSA
# KMO untuk mengukur kecukupan sampling (sampling adequacy)
# MSA untuk menilai kelayakan setiap variabel untuk dianalisis faktor
library(psych)
KMO(data)

# didapatkan nilai KMO keseluruhan sebesar 0,77
#y ang artinya data cukup (middling) untuk analisis faktor
 
# nilai MSA untuk kesepuluh variabel pada data seluruhnya lebih dari 0,5 
# yang artinya setiap variabel masih bisa diprediksi dan dianalisis lebih lanjut
# kalau nilai MSA < 0,5 maka variabel dibuang dari analisis 
# dan dilanjutkan dengan sisa variabel yang memenuhi nilai MSA


#===== Penentuan Banyak Faktor yang Diekstrak
# dilihat dari scree plot
library(nFactors)
eigen=eigen(cor(data))
eigen
# dari nilai eigen values, dapat dilihat 3 faktor memiliki 
# nilai eigen lebih dari 1, jadi yang dipakai adalah 3 faktor
ap=parallel(subject=nrow(data),var=ncol(data),cent=.05)
ns=nScree(eigen$values,ap$eigen$qevpea)
plotnScree(ns)
# diperoleh 3 nilai eigen yang lebih besar dari 1
# maka dengan mengeskstraksi 3 faktor saja sudah mencukupi 
# untuk mewakili keempat variabel pada data


#===== Analisis Faktor
## menentukan banyak faktor
fit=factanal(data,factors=3,rotation="varimax",scores="regression")
## output analisis faktor
print(fit,digits=3,sort=TRUE)
# pvalue > alpha, menunjukkan jumlah faktor memenuhi untuk merepresentasikan data

# Komunalitas
apply(fit$loadings^2,1,sum)


## menghitung skor faktor
fit$scores
## memvisualisasikan model faktor
library(psych)
loads <- fit$loadings
fa.diagram(loads)
# angka merupakan nilai loadings terbesar (dibulatkan ke atas)



##### FUNCTION
FactorAnalysis=function(x, standardize=FALSE){
  data=as.matrix(x)
  if(standardize == TRUE){
    data = scale(data)
  }
  S = cov(data)
  A = eigen(S)$values
  V = eigen(S)$vector
  trace_S=sum(diag(S))
  prop=A/trace_S
  q = length(prop)
  propcum = c()
  for (i in 1:q){
    propcum[i]=sum(prop[1:i])
    }
  L=V%*%sqrt(diag(A,length(A),length(A)))
  miu=colMeans(data)
  n=nrow(data)
  p=ncol(data)
  Xc=matrix(0,n,p)
  for (i in (1:n)){
    for (j in (1:p)){
      Xc[i,j]=data[i,j]-miu[j]
    }
    }
  F=Xc%*%solve(S)%*%L
  plot(A, main="Scree Plot", type="o")
  hasil = list("Matriks Varkov/Korelasi"=S, "Eigen Value"=A, 
               "Eigen Vector"=V, "Proporsi Faktor"=prop, 
               "Proporsi Kumulatif Faktor"=propcum, 
               "Loading Factor"=L, "Factor Score"=F)
  print(hasil)
  }
FactorAnalysis(data, standardize = T)