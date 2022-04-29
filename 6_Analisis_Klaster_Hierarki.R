##### ANALISIS KLASTER HIERARKI AGGLOMERATIVE #####

#===== Input Data
data <- read.csv(file.choose(),header=T,sep=";",dec=",")
View(data)
str(data)

## MISAL:
X1<-data$Angka.Harapan.Hidup
X2<-data$Harapan.Lama.Sekolah
X3<-data$Rata.Rata.Lama.Sekolah
X4<-data$Pengeluaran.Per.Kapita
data<-data.frame(X1,X2,X3,X4)
head(data)

#===== Uji Asumsi Analisis Klaster
# Uji Asumsi Non-multikolinieritas
q=cbind(X1,X2,X3,X4)
f=cor(q)
vif=diag(solve(f))
vif

#===== Perhitungan Jarak Euclidean
library(factoextra)
IPM = scale(data) # data distandardisasi
d<-dist(IPM, method="euclidean")
d


#===== Analisis Klaster Hierarki Agglomerative Method

## SINGLE LINKAGE
fit1<-hclust(d, method="single")

library(dendextend)
dend<-as.dendrogram(fit1)
dend_colored<-color_branches(dend)
plot(dend_colored)

cop1<-cophenetic(fit1)
cor1<-cor(d,cop1);cor1

## COMPLETE LINKAGE
fit2<-hclust(d, method="complete")

library(dendextend)
dend<-as.dendrogram(fit2)
dend_colored<-color_branches(dend)
plot(dend_colored)

cop2<-cophenetic(fit2)
cor2<-cor(d,cop2);cor2

## AVERAGE LINKAGE
fit3<-hclust(d, method="average")

library(dendextend)
dend<-as.dendrogram(fit3)
dend_colored<-color_branches(dend)
plot(dend_colored)

cop3<-cophenetic(fit3)
cor3<-cor(d,cop3);cor3

## WARD'S METHOD
fit4<-hclust(d, method="ward.D")

library(dendextend)
dend<-as.dendrogram(fit4)
dend_colored<-color_branches(dend)
plot(dend_colored)

cop4<-cophenetic(fit4)
cor4<-cor(d,cop4);cor4

c(cor1,cor2,cor3,cor4)
# cor3 (average linkage) terbesar

#===== Pengklasteran dengan Prosedur Pengelompokan Terbaik
fit3<-hclust(d, method="average")

cop3<-cophenetic(fit3)
cor3<-cor(d,cop3);cor3

plot(fit3,main="Dendogram Average Linkage")
rect.hclust(fit3,k=5,border=2:6)

#===== Profiling Hasil Klaster
groups<-cutree(fit3,k=5);groups
klaster<-data.frame(data,groups)
klaster1<-data.frame(klaster[which(groups==1),1:5])
klaster2<-data.frame(klaster[which(groups==2),1:5])
klaster3<-data.frame(klaster[which(groups==3),1:5])
klaster4<-data.frame(klaster[which(groups==4),1:5])
klaster5<-data.frame(klaster[which(groups==5),1:5])
summary(klaster1)
summary(klaster2)
summary(klaster3)
summary(klaster4)
summary(klaster5)