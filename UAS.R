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
library(mlbench)


###SOAL 1
cov1 <- matrix(c(1, 0.81, -0.72, 0.81, 1, -0.61,-0.72 ,-0.61, 1),ncol=3)
cov2 <- matrix(c(1, 0.12, -0.23, 0.12, 1 , -0.05, -0.23, -0.05, 1),ncol=3, byrow = F)

               