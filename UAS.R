#load package
library(caret)
library(Hmisc)
library(rpart)
library(mice)
library(readr)
library(e1071)
library(RANN)
library(readr)
library(ROSE)
library(psych)
library(mlbench)


###SOAL 1
cor1 <- matrix(c(1, 0.81, -0.72, 0.81, 1, -0.61,-0.72 ,-0.61, 1),ncol=3)
cor2 <- matrix(c(1, 0.12, -0.23, 0.12, 1 , -0.05, -0.23, -0.05, 1),ncol=3, byrow = F)

colnames(cor1) = rownames(cor1) <- c("Nilai APG", "Jenis Kelamin", "Nilai Alpro")
colnames(cor2) = rownames(cor2) <- c("Nilai APG", "Nilai Komstat", "Nilai Kalkulus I")

pca1 <- principal(cor1, nfactors=3, rotate="varimax")
pca2 <- principal(cor2, nfactors=3, rotate="varimax")

plot(pca1$Vaccounted[5,], type = "b")


