#SOAL 1
label<-c("Kendaraan","Bangunan/Gedung","Tanah")
kolom<-c("Rata-rata","Simpangan Baku")
matrixGabungan<-matrix(cbind(c(9.5,6),c(5,3.5),c(7,5)),nrow=3,ncol=2)
row.names(matrixGabungan)<-label
colnames(matrixGabungan)<-kolom
matrixGabungan

matrixKorelasi<-matrix(rbind(0.47,0.33,0.98),nrow = 3,ncol = 1)
colnames(matrixKorelasi)<-"Nilai Koefisien"
rownames(matrixKorelasi)<-c("Kendaraan dan Bangunan","Kendaraan dan Tanah", "Bangunan dan Tanah")
matrixKorelasi

matrixRataan<-matrix(matrixGabungan[,1])
colnames(matrixRataan)<-"Rata-rata"
rownames(matrixRataan)<-label
matrixRataan

matrixSimpangan<-matrix(matrixGabungan[,2])
colnames(matrixSimpangan)<-"Simpangan Baku"
rownames(matrixSimpangan)<-label
matrixSimpangan

corrMtrx<- matrix(rbind(cbind(1,0.47,0.33),cbind(0.47,1,0.98),cbind(0.33,0.98,1)),ncol=3,nrow=3)
rownames(corrMtrx)<-label
colnames(corrMtrx)<-label
corrMtrx  

matrixRagam<-matrix(nrow = nrow(corrMtrx),ncol=ncol(corrMtrx))
for(i in 1:nrow(corrMtrx)){
  for (j in 1:ncol(corrMtrx)){
    matrixRagam[i,j]<-corrMtrx[i,j]*sqrt(matrixSimpangan[i,])*sqrt(matrixSimpangan[j,])
  }
}
matrixRagam
#====================================================#
#1B
generalizedVariances<-det(matrixRagam)
#=====================================================#
#1C
CI_bonf <- function(mean,var, alpha){
  bawah <- mean-abs(qt((alpha/4), (length(data)-1)))*sqrt(var/length(data))
  atas  <- mean+abs(qt((alpha/4), (length(data)-1)))*sqrt(var/length(data))
  cat("   CI Bonferroni= ", bawah, "< miu < ", atas)
}

CI_tsq <- function(mean,var, alpha, p){
  bawah <- mean-sqrt((p*(length(data)-1)/(length(data)-p))*qf((1-alpha), p, (length(data)-p)))*sqrt(var/length(data))
  atas <- mean+sqrt((p*(length(data)-1)/(length(data)-p))*qf((1-alpha), p, (length(data)-p)))*sqrt(var/length(data))
  cat("   CI T2        = ", bawah, "< miu < ", atas)
}


CI_Parsial <- function(mean,var,alpha){
  if(is.vector(mean)){
    CI_bonf(mean,var,alpha)
    cat("\n")
    CI_tsq(mean,var,alpha)
    cat("\n")
  } else {
    n<-nrow(mean)
    for (i in 1:nrow(mean)){
      cat("## Variabel ke-",i," \n")
      
      CI_bonf(mean[i,],var[i,],alpha)
      cat("\n")
      CI_tsq(mean[i,],var[i,],alpha,n)
      cat("\n")
    }
  }
}

CI_Parsial(matrixRataan,matrixSimpangan,0.05)  

##================================================================##
#SOAL 4
uts<-read.csv("C:/Users/Amek/Documents/APG/dataUTS2018.csv",header = T)
View(uts)
head(uts)
uts$X4<-replace(uts$X4,uts$X4==0,NA)


head(uts)
unique(uts$X4)
uts<-uts[-c(1),]

dataperempuan<-as.matrix(uts[,uts$X2==2])
data3<- uts[,c(2,3,7,8)] ##data dengan variabel yang dibutuhkan
head(data3)

data4<-data.frame(uts$X12,uts$X13,uts$X4)
head(data4)
data4<-data4[-c(168),]

datatest<-head(data4,168)

owManova(data4)

##===============================================#
##SOAL 2
bln0<-matrix(cbind(c(8.8,9.6,9.3,8.5),c(73.8,75.4,74.2,73.3)),ncol=2)
bln1<-matrix(cbind(c(8.7,9.5,9.2,9.6),c(75.1,75.4,76.3,77.2)),ncol=2)
bln2<-matrix(cbind(c(9.6,10.1,9.7,9.2),c(74.3,76.5,75.0,76.0)),ncol=2)
data2<-cbind(rbind(bln0,bln1,bln2),rep(1:3,c(4,4,4)))
data2

owManova(data2)
summary(manova(rbind(bln0,bln1,bln2)~factor(rep(1:3,c(4,4,4)))),test="Wilks")

