heights <- c(158,158,158,160,160,163,163,160,163,165,165,165,168,168,168,170,170,170)
weights <- c(58,59,63,59,60,60,61,64,64,61,62,65,62,63,66,63,64,68)
size <- c(rep(0,times=7),rep(1,times=11))

heights <- rep(heights,times=10)
weights <- rep(weights, times = 10)
size <- rep(size,times= 10)
train.set <- data.frame(cbind(heights,weights,size))


heights <- sample(heights,replace = T)
weights <- sample(weights,replace = T)
size <- sample(size,replace = T)


test.set <- data.frame(cbind(heights,weights,size))

library(knncat)
shirtcat<-knncat(train.set,classcol = 3)

shirtpred <- predict(shirtcat,train.set,test.set,train.classcol = 3,newdata.classcol = 3)

table(shirtpred,test.set$size)

knncat(train.set,test.set,classcol = 3)
