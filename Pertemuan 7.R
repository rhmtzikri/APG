#UJI MIUD=0 (x1,x2,y1,y2) dependen
#H0: miud = 0 atau dbar1-dbar2=0
#h1: miud != 0 atau dbar1-dbar2!=0
#tingkat signifikansi = 0.05

y1<-c(148,159,144,103,121,89,119,123,76,217,148,151,83,135,178)
y2<-c(20,24,19,18,17,11,17,13,16,29,22,21,7,20,15)
x1<-c(137,164,224,208,178,128,154,158,102,214,209,151,123,161,175)
x2<-c(15,25,27,33,24,20,18,16,21,25,24,16,13,22,23)
d1<-NULL
d2<-NULL
for(i in 1:15){
  d1<-c(d1,y1[i]-x1[i])
  d2<-c(d2,y2[i]-x2[i])
}

cbind(d1,d2)
#matriks dbar
dbar<-rbind(mean(d1),mean(d2))

M<-cbind(d1,d2) #matriks selisih
k<- ncol(M) #banyak kolom
n<- nrow(M) #banyak baris(banyaknya pasangan observasi)

mean_mtrx <-matrix(data=1, nrow=n) %*% t(dbar)
D<-M-mean_mtrx
Covar<-(n-1)^-1*t(D)%*%D #matriks kovarian

t_tab<-qt(p = 0.0125,df = 14,lower.tail = F)

bonferro<-matrix(ncol = 2,nrow = length(dbar))
bonf_min<-NULL
bonf_max<-NULL
for (i in 1:length(dbar)){
  bonf_min[i]<-dbar[i]-(t_tab*sqrt(Covar[i,i]/n))
  bonf_max[i]<-dbar[i]+(t_tab*sqrt(Covar[i,i]/n)) 
  bonferro[i,]<-rbind(c(bonf_min[i],bonf_max[i]))
}
colnames(bonferro)<-c("Batas Atas","Batas Bawah")
rownames(bonferro)<-c("y1","y2")
bonf_min[2]
bonferro


cv<-(sqrt(2*(n-1)/(n-2)))*qf(p = 0.05,df1 = 2,df2 = 13)

thot<-matrix(ncol = 2,nrow = length(dbar))
thot_min<-c()
thot_max<-c()
for (i in 1:length(dbar)){
  thot_min[i]<-dbar[i]-(cv*sqrt(Covar[i,i]/n))
  thot_max[i]<-dbar[i]+(cv*sqrt(Covar[i,i]/n)) 
  thot[i,]<-rbind(c(thot_min[i],thot_max[i]))
}
thot
s<-bonferoni_ci(d1,d2)

