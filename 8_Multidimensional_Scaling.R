##### ANALISIS MULTIDIMENSIONAL SCALING METRIK #####

#===== Input Data
data1<-read.csv(file.choose(),header=TRUE,sep=";")
head(data1)
Smartphone<-data1[,1]
head(Smartphone)
data<-data1[,2:11]
rownames(data)=Smartphone
head(data)

#===== Menghitung Jarak Euclidean
d<-dist(data, method="euclidean")
d

#===== Analisis Multidimensional Scaling
fit<-cmdscale(d, eig=TRUE, k=2) # dalam 2 dimensi
fit

#===== Membuat Peta MDS
x<-fit$points[,1]
y<-fit$points[,2]
plot(x, y, xlab="x", ylab="y", main="Classical MDS", type="n")
abline(v=0)
abline(h=0)
text(x, y, labels = row.names(data), cex=.7)


##### ANALISIS MULTIDIMENSIONAL SCALING NON METRIK #####

#===== Menghitung Nilai Stress
n=nrow(data)  # banyak unit observasi
k=2           # dimensi
Dm=as.matrix(d)
dij2=Dm^2

I=diag(n)
J=matrix(1,nrow=n,ncol=n)
V=I-(1/n)*J
B=(-1/2)*V%*%dij2%*%V     # matriks B
ne=eigen(B)$value[1:k]    # nilai eigen
ne
ve=eigen(B)$vectors[,1:k] # vektor eigen
ve
F=ve%*%(sqrt(diag(ne)))   # matriks koordinat objek
F
Dhat=dist(F)              # matriks jarak
Dhat

# Stress dalam persen
Stress=(sqrt(sum((dist(data)-Dhat)^2))/(sum(dist(data)^2)))*100
Stress

#===== Menghitung Nilai R-square
R2=1-(sum((dist(data)-Dhat)^2))/(sum(dist(data)^2))
R2

# R^2 dalam persen
Rsquare=R2*100
Rsquare