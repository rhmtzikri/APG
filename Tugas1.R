#1. dependen
#2. independen
#3. apakah di informal mengikuti dist normal

#======Sampel Dependen======


informal<-matrix(c(148, 20,
                   159, 24,
                   144, 19,
                   103, 18,
                   121, 17,
                   89,  11,
                   119, 17,
                   123, 13,
                   76,  16,
                   217, 29,
                   148, 22,
                   151, 21,
                   83,  7,
                   135, 20,
                   178, 15),ncol=2, byrow=TRUE)
formal<-matrix(c(137,15,
                 164, 25,
                 224, 27,
                 208, 33,
                 178, 24,
                 128, 20,
                 154, 18,
                 158, 16, 
                 102, 21, 
                 214, 25,
                 209, 24, 
                 151, 16, 
                 123, 13, 
                 161, 22,
                 175, 23), ncol=2, byrow=TRUE)
informal

formal

rerata<- function(data){
  jumlah<-0
  for (i in 1:length(data)){
    jumlah<-jumlah+data[i]
  }
  rata<-jumlah/length(data)
  return(rata)
}

d<-matrix(c(informal[,1]-formal[,1],informal[,2]-formal[,2]),ncol=2)
d
dbar<-matrix(c(rerata(d[,1]), rerata(d[,2])),nrow=1)
dbar

totald<-matrix()
kovar<- function(data){
  n<-nrow(data)
  k<-ncol(data)
  rata<-matrix(ncol=k)
  for (i in 1:ncol(data)){
    print(rata)
    print(k)
    print(rerata(data[,k]))
    rata[,k]<-rerata(data[,k])
  }
  return(rata)
}

c(rerata(d[,1]),rerata(d[,2]))

kovar(d)

totald

sd <- cov(d)
sd
#menghitung tsquare
n<-15
t<-n%*%dbar%*%solve(sd)%*%t(dbar)
t

#Keputusan
#T = 15,1912 > Ttabel = 8,187 maka Tolak H0
#Kesimpulan
#dengan tingkat signifikansi 0,05, terdapat cukup bukti untuk 
#menyatakan bahwa ............ 

#=====Sampel Independen======



mean(informal)
rerata(informal)
y1bar<-matrix(ncol=2)
y2bar<-matrix(ncol = 2)
for (i in 1:ncol(informal)){
  y1bar[,i]<-rerata(formal[,i])
  y2bar[,i]<-rerata(informal[,i])
}
y1bar

tmv<-function(a,b){
  na<-nrow(a)
  nb<-nrow(b)
  for (i in 1:ncol(a)){
    y1bar[,i]<-rata(a[,i])
    y2bar[,i]<-rata(b[,i])
  }
  
  y2bar<-colMeans(b)
  
  print(y1bar)
  cov1<-cov(a)
  cov2<-cov(b)
  spl<-1/(na+nb-2)*((na-1)*cov1+(nb-1)*cov2)
  selisih<-matrix(y1bar-y2bar)
  Tsqr<-((na*nb)/(na+nb))*t(selisih)%*%solve(spl)%*%selisih
  #print(Tsqr)
}
tmv(formal,informal)


#======QQ-Plot Informal=======
qqplot2<-function(x){
  mean<-matrix(colMeans(a))
  k<-ncol(a)
  n<-nrow(a)
  s<-cov(x)
  for(i in 1:n){
    
  }
  
}
