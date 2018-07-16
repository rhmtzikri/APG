library(caret)
library(Hmisc)
library(rpart)
library(mice)
library(readr)
library(e1071)
library(RANN)
library(readr)
library(ROSE)
library(mlbench)
library(ggplot2)
library(caret)
library(psych)
library(MVN)
library(DescTools)
library(biotools)
library(corrplot)
library(class)
library(e1071)
library(pROC)
library(car)
library(Hmisc)
library(DMwR)
library(rpart)
library(GGally)
library(CCA)
library(CCP)
library(vegan)


##Nomor 1
corr1 <- matrix(c(1, 0.81, -0.72, 0.81, 1, -0.61,-0.72 ,-0.61, 1),ncol=3)
corr2 <- matrix(c(1, 0.12, -0.23, 0.12, 1 , -0.05, -0.23, -0.05, 1),ncol=3, byrow = F)
rownames(corr1) <- c("Nilai APG", "Nilai Metstat 2", "Nilai PBO")
colnames(corr1) <- rownames(corr1)
rownames(corr2) <- c("Nilai Metstat 1", "Nilai Metnum", "Gender")
colnames(corr2) = rownames(corr2)

###1bc
pca1 <- principal(corr1, nfactors=3, rotate="varimax")
pca2 <- principal(corr2, nfactors=3, rotate="varimax")
plot(pca1$Vaccounted[5,], type = "b")
plot(pca2$Vaccounted[5,], type = "b")

##Nomor 2
Ryy <- matrix(c(1, 0.8, 0.8, 1), ncol=2)
Rxx <- matrix(c(1, 0.37, 0.21, 0.37, 1, 0.35, 0.21, 0.35, 1), ncol = 3)
Rxy <- matrix(c(0.26, 0.33, 0.67, 0.59, 0.34, 0.34), ncol = 2, nrow =3)


##3
CountryData <- read_csv("./CountryDataReducedObv.csv")
CountryData <- CountryData[,-1]
CountryDataImputed <- as.data.frame(apply(CountryData[,-1], 2, function(x) Hmisc::impute(x, what="mean")))

###3a
corrplot::corrplot(cor(CountryData[-1]))
###3c
EconomicCountry <- CountryDataImputed[, names(CountryDataImputed) %in% c("GDPcapita", "unemployment", "debt", "saving")]
km=kmeans(EconomicCountry, centers = 2)
km$cluster
table(km$cluster)
CountryData$clusterEconomic <- km$cluster

###3D
CountryData$GDPlevel <- ifelse(CountryData$GDPcapita > 1500, 1, 0)





