##### ANALISIS DISKRIMINAN K KELOMPOK #####

#===== Input Data
data("iris")
library(DT) 
datatable(iris)
head(iris)
str(iris)

#===== Variabel Independen dan Dependen
x=iris[,c(1:4)]
head(x)
y=iris[,5]
head(y)
n=nrow(iris);n

#===== Uji Asumsi Analisis Diskriminan
#== Uji Normalitas Multivariat
# uji normalitas variabel bebas X
library(MVN)
mvn(data=x, mvnTest="mardia")
# atau
mvn(data=x, multivariatePlot='qq')

#== Uji Homogenitas Varians
library(biotools)
boxM(data=x, grouping=y)

#== Uji Multikolinieritas
diag(solve(cor(x)))

#===== Membagi Data Training dan Testing
# 75% sebagai train data dan 25% sebagai test data
set.seed(123)
train_index <- sample(seq(nrow(iris)), size = floor(0.75 * nrow(iris)), replace = F)
training_data <- iris[train_index, ] 
head(training_data)
test_data <- iris[-train_index, ]
head(test_data)

#===== Membentuk Fungsi Diskriminan
library(MASS)
(iris.lda <- lda(Species ~., data = training_data))
iris.lda$count

#===== Membentuk Plot antara Fungsi Diskriminan
## Enable the r-universe repo
#options(repos = c(
#  fawda123 = 'https://fawda123.r-universe.dev',
#  CRAN = 'https://cloud.r-project.org'))

# Install ggord
# install.packages('ggord')

library(ggord)
ggord(iris.lda, training_data$Species)

#===== Menguji Performa Model
predicted <- predict(object = iris.lda, newdata = test_data)
table(actual = test_data$Species, predicted = predicted$class)