temp <- read.csv(file = "lat.csv", header = FALSE)
v1 <- c(temp$V2,temp$V7,temp$V12)
v2 <- c(temp$V3,temp$V8,temp$V13)
v3 <- c(temp$V4,temp$V9,temp$V14)
v4 <- c(temp$V5,temp$V10,temp$V15)

datas <- data.frame(cbind(v1,v2,v3,v4))

qqplot_mvt(datas)
normality_mvt(datas)
