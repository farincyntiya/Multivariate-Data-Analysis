##### ANALISIS KLASTER NON HIERARKI #####

library(gridExtra)
library(factoextra)

#===== Input Data
excel<-read.csv(file.choose(),header=TRUE,sep=";",dec=",")
View(excel)
str(excel)
X1=excel$Perasaan.Senang
X2=excel$Perasaan.Tidak.Khawatir
X3=excel$Perasaan.Tidak.Tertekan
data<-cbind(X1,X2,X3)
summary(is.na(data))
summary(data)

#===== Mendeteksi Multikolinieritas
q=cbind(X1,X2,X3)
f=cor(q)
vif=diag(solve(f))
vif

#===== Menghitung Jarak Euclidean
d<-dist(data, method="euclidean")
d
fviz_dist(d,gradient=list(low="red",mid="white",high="black"))

#===== Menentukan Jumlah Klaster
fviz_nbclust(data, kmeans, method="silhouette")

#===== Analisis Klaster K-means
set.seed(100)
cluster<-kmeans(data,centers=10,nstart=25)
cluster

#===== Visualisasi Klaster K-means
provinsi=excel$Provinsi
rownames(data)=provinsi
head(data)
fviz_cluster(cluster,data)