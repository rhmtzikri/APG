#membentuk tabel data
data_manova<-data.frame(pelumas,velocity,y1,y2)
data<-data.frame(pelumas,velocity,y1,y2)
data<-data_manova
faktor<-as.numeric(data$pelumas)

##fungsi uji box-m
boxm.test <- function(data,faktor){
  ktgr<-which(sapply(data, class)=="factor")
  
  levela<-as.factor(levels(data[,ktgr[2]]))
  levelb<-as.factor(levels(data[,ktgr[1]]))
  
  data_min<-data[-ktgr]
  
  a <-length(unique(levela))
  b <-length(unique(levelb))
  n <- nrow(data)/(a*b)
  p<-ncol(data_min)
  
  Si<-lapply(as.numeric(unique(faktor)),function(i) cov(data[as.numeric(faktor)==i,-c(1,2)]))
  v<-(nrow(data)/length(unique(faktor)))-1
  
  if(a<b){
    a_loop <-a
    b_loop<-b
  } else {
    a_loop<-b
    b_loop<-a
  }
  

  dispersi <- array(NaN,c(n,a*b,p))
  y <-array(NaN,c(b_loop,a_loop,p))
  yfaktorB <- array(NaN,c(b_loop,p))
  yfaktorA <- array(NaN,c(p,a_loop))
  ytotal <- array(NaN,c(1,p))
  
  for (i in 1:p) {
    dispersi[,,i] <- sapply(split(data_min[,i],data[,ktgr]),unlist)
  }
  
   
  for (i in 1:a_loop) {
    for (j in 1:b_loop) {
      for(k in 1:p) {
          if(i==1){
            y[j,i,k] = sum(dispersi[,j,k])
            y
          } else {
            y[j,i,k] = sum(dispersi[,j+b_loop,k])
          }
        yfaktorB[j,k] = sum(y[j,,k])
        yfaktorA[k,i] = sum(y[,i,k])
        ytotal[,k] = sum(yfaktorA[k,])
      }
    }
  }
  
  HA = HB = HAB = E <-matrix(ncol=p,nrow=p)
  
  for (i in 1:p) {
    for (j in 1:p) {
      if (i==j) {
        HA[i,j] <- round((sum(yfaktorA[j,]^2/(n*b_loop)) - (ytotal[,j]^2/(n*a*b))),3)
        HB[i,j] <- round((sum(yfaktorB[,j]^2/(n*a_loop)) - (ytotal[,j]^2/(n*a*b))),3)
        HAB[i,j] <- round((sum(y[,,j]^2/n) - sum(yfaktorA[j,]^2/(n*b_loop)) 
                           - sum(yfaktorB[,j]^2/(n*a_loop)) + ytotal[,j]^2/(n*a*b)),3)
        E[i,j] = sum(dispersi[,,j]^2) - ytotal[,j]^2/(n*a*b) - (HA[i,j] + HB[i,j] + HAB[i,j])
      } else {
        HA[i,j] <- round((sum(yfaktorA[i,]*yfaktorA[j,]/(n*b_loop)) - (ytotal[,i]*ytotal[,j]/(n*a*b))),3)
        HB[i,j] <- round((sum(yfaktorB[,i]*yfaktorB[,j]/(n*a_loop)) - (ytotal[,i]*ytotal[,j]/(n*a*b))),3)
        HAB[i,j] <- round((sum(y[,,j]*y[,,i]/n) - sum(yfaktorA[j,]*yfaktorA[i,]/(n*b_loop)) 
                           - sum(yfaktorB[,j]*yfaktorB[,i]/(n*a_loop)) + ytotal[,j]*ytotal[,i]/(n*a*b)),3)
        E[i,j] <- sum(dispersi[,,i]*dispersi[,,j]) - ytotal[,i]*ytotal[,j]/(n*a*b) - (HA[i,j]+HB[i,j]+HAB[i,j])
      }
    }
  }
  
  vE<-a*b*(n-1)
  Sp <- E/vE
  
  total<-0
  
  k<-length(unique(faktor))
  for (i in 1:k){
    total<-total+(v*log(det(Si[[i]])))
  }
  
  M<- 0.5*(total-(k*v*log(det(Sp))))
  
  c <- (k/v-(1/(k*v)))*((2*p^2+3*p-1)/(6*(p+1)*(k-1)))
  
  u <- -2*(1-c)*M
  
  df <- ((k-1)*p*(p+1))/2
  pval <- pchisq(q = u,df = df)
  
  out<-matrix(cbind(u,df,pval),nrow=1,dimnames = list(c(""),c("Chi-Sq approx.","df","p-value")))
  return(out)
}

pval
boxm.test(data_manova,pelumas)
boxM(data_manova[-c(1,2)],pelumas)

##fungsi manova 2 arah
two.manova <- function(data,alpha=0.05){
  ktgr<-which(sapply(data, class)=="factor")
  
  levela<-as.factor(levels(data[,ktgr[2]]))
  levelb<-as.factor(levels(data[,ktgr[1]]))
  
  data_min<-data[-ktgr]
  
  a <-length(unique(levela))
  b <-length(unique(levelb))
  n <- nrow(data)/(a*b)
  p<-ncol(data_min)
  
  
  if(a<b){
    a_loop <-a
    b_loop<-b
  } else {
    a_loop<-b
    b_loop<-a
  }
  
  dispersi <- array(NaN,c(n,a*b,p))
  y <-array(NaN,c(b_loop,a_loop,p))
  yfaktorB <- array(NaN,c(b_loop,p))
  yfaktorA <- array(NaN,c(p,a_loop))
  ytotal <- array(NaN,c(1,p))
  
  for (i in 1:p) {
    dispersi[,,i] <- sapply(split(data_min[,i],data[,ktgr]),unlist)
  }

  for (i in 1:a_loop) {
    for (j in 1:b_loop) {
      for(k in 1:p) {
          if(i==1){
            y[j,i,k] = sum(dispersi[,j,k])
            y
          } else {
            y[j,i,k] = sum(dispersi[,j+b_loop,k])
          }
        yfaktorB[j,k] = sum(y[j,,k])
        yfaktorA[k,i] = sum(y[,i,k])
        ytotal[,k] = sum(yfaktorA[k,])
      }
    } 
  }
  
  HA = HB = HAB = E <-matrix(ncol=p,nrow=p)
  
  for (i in 1:p) {
    for (j in 1:p) {
      if (i==j) {
        HA[i,j] <- round((sum(yfaktorA[j,]^2/(n*b_loop)) - (ytotal[,j]^2/(n*a*b))),3)
        HB[i,j] <- round((sum(yfaktorB[,j]^2/(n*a_loop)) - (ytotal[,j]^2/(n*a*b))),3)
        HAB[i,j] <- round((sum(y[,,j]^2/n) - sum(yfaktorA[j,]^2/(n*b_loop)) 
                           - sum(yfaktorB[,j]^2/(n*a_loop)) + ytotal[,j]^2/(n*a*b)),3)
        E[i,j] = sum(dispersi[,,j]^2) - ytotal[,j]^2/(n*a*b) - (HA[i,j] + HB[i,j] + HAB[i,j])
      } else {
        HA[i,j] <- round((sum(yfaktorA[i,]*yfaktorA[j,]/(n*b_loop)) - (ytotal[,i]*ytotal[,j]/(n*a*b))),3)
        HB[i,j] <- round((sum(yfaktorB[,i]*yfaktorB[,j]/(n*a_loop)) - (ytotal[,i]*ytotal[,j]/(n*a*b))),3)
        HAB[i,j] <- round((sum(y[,,j]*y[,,i]/n) - sum(yfaktorA[j,]*yfaktorA[i,]/(n*b_loop)) 
                           - sum(yfaktorB[,j]*yfaktorB[,i]/(n*a_loop)) + ytotal[,j]*ytotal[,i]/(n*a*b)),3)
        E[i,j] <- sum(dispersi[,,i]*dispersi[,,j]) - ytotal[,i]*ytotal[,j]/(n*a*b) - (HA[i,j]+HB[i,j]+HAB[i,j])
      }
    }
  }

  kosong<-rep("",times=p)
  colnames(E) = row.names(E) = colnames(HA) = row.names(HA) = colnames(HB) = row.names(HB) = colnames(HAB) = row.names(HAB) <- kosong
  wilksA<-det(E)/(det(E+HA))
  wilksB<-det(E)/(det(E+HB))
  wilksAB<-det(E)/det(E+HAB)
  
  A<-matrix(c(wilksA,wilksB,wilksAB),ncol=1)
  colnames(A) <- "Wilk's Lambda"
  
  vHA <- a-1
  vHB <- b-1
  vHAB <- vHA*vHB
  vH <-matrix(c(vHA,vHB,vHAB))
  vE <- a*b*(n-1)
  vT <- a*b*n-1
  
  wilks<-matrix(ncol=1,nrow=3)
  tabelF<-matrix(nrow=3)
  stokdf <- matrix(ncol=2,nrow=3)
  pval <- matrix(ncol=1,nrow=3)
  keputusan <- matrix(ncol=1,nrow=3)
  ftab <-matrix(ncol=1,nrow=3)
  
  for(i in 1:length(vH)){
    if(vH[i]==1){
      df1<-p
      df2<-vE-p+1
      wilks[i,]<-signif(((1-A[i,])/A[i,])*(df2/df1),6)
    } else {
      w<- vE+vH[i]-(0.5*(p+vH[i]+1))
      t<-sqrt(((p^2*vH[i,]^2)-4)/(p^2+vH[i]^2-5))
      df1<-p*vH[i,]
      df2<-round(w*t-(0.5*(p*vH[i,]-2)),3)
      wilks[i,]<-signif(((1-A[i,]^(1/t))/A[i,]^(1/t))*(df2/df1),6)
    }
    #tabelF[i,] <- signif(qf(alpha,df1,df2),digits = 4)
    tabelF[i,] <- wilks[i,]
    stokdf[i,1] <- df1
    stokdf[i,2] <- df2 
    pval[i,]<-signif(pf(as.numeric(wilks[i,]),df1,df2,lower.tail = F),4)
    ftab[i] <- qf(p = alpha,df1 = df1,df2 = df2,lower.tail = F)
    
    if(pval[i,]<alpha){
      keputusan[i,] <- "Tolak H0"
    } else {
      keputusan[i,] <- "Gagal Tolak H0"
    }
  }
  

  judul <- matrix(c("Source","df","Wilks' Λ","df1","df2","F","F-tab","p-val","Keputusan"),nrow=1)
  sumber <- c(labels(ktgr[2]),labels(ktgr[1]),paste(labels(ktgr[2]),":",labels(ktgr[1]),collapse = ""),"Error","Total")
  df <- matrix(rbind(vH,vE,vT))
  lambda <- matrix(rbind(signif(A,5),"","")) 
  ftab <-matrix(rbind(signif(ftab,5),"",""))
  tabelF<-matrix(rbind(tabelF,"",""))
  stokdf<-matrix(rbind(stokdf,cbind(c("",""),c("",""))),ncol=2)
  pval<-matrix(rbind(pval,"",""))
  keputusan<-matrix(rbind(keputusan,"",""))
  fManova <- matrix(cbind(sumber,df,lambda,stokdf,tabelF,ftab,pval,keputusan),nrow=5)
  colnames(fManova) <- judul
  rownames(fManova) <- rep("",nrow(fManova))
  #print(noquote(fManova))
  
  
  return(list("summary" = noquote(fManova), "wilks" = A,"HA" = noquote(HA),"HB" = noquote(HB),
              "HAB" = noquote(HAB),"error" = noquote(E)))
}


##preprocessing
velocity<-as.factor(rep(c("A1","A2"),each=16))
pelumas<-as.factor(rep(c("B1","B2","B3","B4"),each=4,times=2))
y1<-c(7.80,7.1,7.89,7.82,9,8.43,7.65,7.7,7.28,8.96,7.75,7.8,7.6,7.0,7.82,7.8,7.12,7.06,7.45,7.45,8.19,8.25,7.45,7.45,7.15,7.15,7.7,7.45,7.06,7.04,7.52,7.70)
y2<-c(90.4,88.9,85.9,88.8,82.5,92.4,82.4,87.4,79.6,95.1,90.2,88.0,94.1,86.6,85.9,88.8,85.1,89.0,75.9,77.9,66,74.5,83.1,86.4,81.2,72,79.9,71.9,81.2,79.9,86.4,76.4)
y3<-rnorm(32,50,9)

#membentuk tabel data
data_manova<-data.frame(pelumas,velocity,y1,y2)
var<-manova.uji(data_manova,0.05)
var$summary
summary(manova(cbind(y1,y2)~velocity*pelumas),test = "Wilks")

data_manova_inv<-data.frame(pelumas,velocity,y1,y2,y3)
var_inv <- manova.uji(data_manova_inv,0.05)
var_inv$summary
summary(manova(cbind(y1,y2,y3)~velocity*pelumas),test = "Wilks")

x1<-rnorm(80,4,3.282)
x2<-rnorm(80,28.4,50.39)
cat1<-as.factor(rep(c("Reguler","VIP","SVIP","VVIP"), each="20"))
cat2<-as.factor(rep(c("SS","S","TS","STS"),each="5",times="4"))          
data<-data.frame(cat2,cat1,x1,x2)


var1<-manova.uji(data,0.05)
var1$summary
summary(manova(cbind(x1,x2)~cat2*cat1),test = "Wilks")


  