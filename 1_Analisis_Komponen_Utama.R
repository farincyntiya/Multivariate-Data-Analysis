#### ANALISIS KOMPONEN UTAMA #####

#===== Input Data
data	<- read.csv(file.choose(),header=T,sep=";",dec=",")
str(data)
head(data)

#===== Uji Asumsi
#== Uji Normalitas
# Plot Mahalanobis
X	<- as.matrix(data);X
mean	<-colMeans(X);mean
cov	<-cov(X);cov
d	<-(mahalanobis(X,mean,cov));d
ds	<-sort(d);ds
n	<-nrow(data)
p	<-ncol(data)
j	<-qchisq(ppoints(n),df=p);j
plot(j,ds,xlab="Nilai Kuantil Chi Kuadrat",ylab="Jarak Mahalanobis",main="Q-Q Plot",col="blue")
abline(lm(ds~j),col="red")
# Uji Normalitas Shapiro-Wilk
library(mvShapiroTest)
mvShapiro.Test(X)
# p-value > 0.05 = normal multivariat
# Uji Normalitas Multivariat
library(MVN)
mvnorm=mvn(data=data,mvnTest="mardia")
mvnorm

#== Uji Homogenitas Varians
library(stats)
bartlett.test(data)
# p-value < 0.05 = varians tidak homogen

#===== Standardisasi Data
# untuk mengubah data ke skala yang sebanding
# jika ada perbedaan besar antara rentang variabel
X <- as.matrix(scale(data))
# X1 angka harapan hidup
# X2 angka harapan lama sekolah
# X3 angka rata-rata lama sekolah
# X4 pengeluaran per kapita

#===== Matriks Varians Kovarians dan Matriks Korelasi
# untuk memahami hubungan antar variabel
## matriks varkov
a=cov(X)
a
## matriks korelasi
b=cor(X)
b

#===== Nilai dan Vektor Eigen
## gunakan matriks varkov jika asumsi varians homogen terpenuhi
eigen(a)
## gunakan matriks korelasi jika asumsi varians homogen tidak terpenuhi
eigen(b)

#===== Analisis Komponen Utama
## melakukan PCA
fit_pca=princomp(X,cor=TRUE)
# cor = TRUE apabila yang digunakan adalah matriks korelasi

## menentukan jumlah komponen utama
summary(fit_pca)
# proporsi kumulatif > 80% di Comp. 2

## membentuk plot
plot(fit_pca,type="barplot",main="Bar Plot",col='cadetblue')
plot(fit_pca,type="lines",main="Scree Plot",col='red')
# plot antara komponen utama ke-𝑘dengan varians atau nilai eigen pada komponen tersebut

## membentuk komponen utama
eigen(b)
# Y1 = - 0.5277441(X1) - 0.3644118(X2) - 0.5685836(X3) - 0.5151727(X4)
# Y2 = -0.3046360(X1) + 0.8851761(X2) + 0.0326771(X3) - 0.3501319(X4)

## menampilkan nilai (score) komponen utama
fit_pca$scores
# untuk keperluan analisis selanjutnya seperti regresi maupun klasifikasi

##### FUNCTION
PCA=function(x, standardize=FALSE){
  data=as.matrix(x)
  S = cov(data)
  if(standardize == TRUE){
    S = cor(data)
  }
  eigen_val = eigen(S)$values
  eigen_vec = eigen(S)$vector
  
  n = length(eigen_val)
  prop = c()
  for (i in (1:n)){
    prop[i] = (eigen_val[i])/sum(eigen_val)
  }
  
  q = length(prop)
  propcum = c()
  for (i in 1:q){
    propcum[i]=sum(prop[1:i])
  }
  
  p = nrow(eigen_vec)
  corr = matrix(0,p,p)
  for (i in (1:p)){
    for (j in (1:p)){
      corr[i,j] = (eigen_vec[i,j]*sqrt(eigen_val[i]))/S[j,j]
    }
  }
  
  plot(eigen_val, main="Scree Plot", type="o")
  hasil = list("Matriks Varkov/Korelasi"=S, "Eigen Value"=eigen_val, 
               "Eigen Vector"=eigen_vec, "Proporsi Komponen"=prop, 
               "Proporsi Kumulatif Komponen"=propcum, "Matriks Korelasi Y dan X"=corr)
  print(hasil)
}
PCA(X)