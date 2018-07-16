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
CountryData <- read_csv("./CountryDataReducedObv.csv")

##Nomor 1
cov1 <- matrix(c(1, 0.81, -0.72, 0.81, 1, -0.61,-0.72 ,-0.61, 1),ncol=3)
cov2 <- matrix(c(1, 0.12, -0.23, 0.12, 1 , -0.05, -0.23, -0.05, 1),ncol=3, byrow = F)

##Nomor 2
Ryy <- matrix(c(1, 0.8, 0.8, 1), ncol=2)
Rxx <- matrix(c(1, 0.37, 0.21, 0.37, 1, 0.35, 0.21, 0.35, 1), ncol = 3)
Rxy <- matrix(c(0.26, 0.33, 0.67, 0.59, 0.34, 0.34), ncol = 2, nrow =3)
##3D
CountryData$GDPlevel <- ifelse(CountryData$GDPcapita > 1500, 1, 0)





