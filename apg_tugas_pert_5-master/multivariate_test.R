library(DescTools)

#mengambil data
dataset <- anscombe
head(dataset) #(x1,y1), (x2,y2), (x3,y3), (x4,y4)
data_y1_y2 <- data.frame(dataset$y1,dataset$y2)
data_x1_x2 <- data.frame(dataset$x1,dataset$x2)
data_y3_y4 <- data.frame(dataset$y3, dataset$y4)

#tes 1 sampel
HotellingsT2Test(data_y1_y2)

#tes 2 sampel
#dependent
HotellingsT2Test(x = data_x1_x2, y = data_y1_y2)

#independet
HotellingsT2Test(x = data_y1_y2, y = data_y3_y4)
