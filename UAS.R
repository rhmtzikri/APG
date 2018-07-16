#load package
library(class)
library(caret)
library(e1071)
library(psych)
library(knncat)

###SOAL 1
cor1 <- matrix(c(1, 0.81, -0.72, 0.81, 1, -0.61,-0.72 ,-0.61, 1),ncol=3)
cor2 <- matrix(c(1, 0.12, -0.23, 0.12, 1 , -0.05, -0.23, -0.05, 1),ncol=3, byrow = F)

colnames(cor1) = rownames(cor1) <- c("Nilai APG", "Jenis Kelamin", "Nilai Alpro")
colnames(cor2) = rownames(cor2) <- c("Nilai APG", "Nilai Komstat", "Nilai Kalkulus I")

pca1 <- principal(cor1, nfactors=3, rotate="varimax")
pca2 <- principal(cor2, nfactors=3, rotate="varimax")

plot(pca1$Vaccounted[5,], type = "b")
plot(pca2$Vaccounted[5,], type = "b")


##SOAL 2
Ryy <- matrix(c(1, 0.8, 0.8, 1), ncol=2)
Rxx <- matrix(c(1, 0.37, 0.21, 0.37, 1, 0.35, 0.21, 0.35, 1), ncol = 3)
Rxy <- matrix(c(0.26, 0.33, 0.67, 0.59, 0.34, 0.34), ncol = 2, nrow =3)



##SOAL 3
countryData <- read.csv("CountryData.csv")
View(countryData)


classGDP <- data.frame(countryData$country,as.numeric(countryData$GDPcapita))
classGDP <- classGDP[complete.cases(classGDP),]
head(classGDP)
rown = mcatlab <- c()

for (i in 1:nrow(classGDP)) {
  rown<- c(rown,i)
  if(classGDP[i,2] > 15000) {
    mcatlab <- c(mcatlab,1)
  } else {
    mcatlab <- c(mcatlab,2)
  }
}

classGDP <- cbind(classGDP,mcatlab)
rownames(classGDP) <- rown
colnames(classGDP) <- c("Country", "GDPcapita", "GDPstatus")
head(classGDP)

limit <- floor(0.7*nrow(classGDP))

train.set <- classGDP[1:limit,]
test.set <- classGDP[limit+1:nrow(classGDP)-limit,]

GDPclustrain <- kmeans(classGDP$GDPcapita, centers = 2)


class2res <- length(GDPclustrain$cluster[GDPclustrain$cluster==2])
class1res <- length(GDPclustrain$cluster[GDPclustrain$cluster==1])
class2ori <- length(classGDP$GDPstatus[classGDP$GDPstatus==2])
class1ori <- length(classGDP$GDPstatus[classGDP$GDPstatus==1])
mtrx <- matrix(c(class2res,class1res,class2ori,class1ori),nrow=2,ncol=2)
colnames(mtrx)<- c("rendah_kmeans","tinggi_kmeans")
rownames(mtrx)<- c("rendah_asli","tinggi_asli")

       