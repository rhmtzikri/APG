library(foreign)
library(DescTools)
dataset <- as.data.frame(read.spss(file = "data kuis.sav"))
t2_one <- function (data, mu = NULL, alpha = 0.05) {
  if (is.null(mu)) {
    mu <- c(rep(0,ncol(data)))
  } else {
    if (!is.vector(mu) || !is.numeric(mu)) {
      stop("mu harus berupa vektor yang berisi numerik")
    }
  }
  y_bar <- as.matrix(as.numeric(colMeans(data)))
  s <- cov(data)
  n <- as.numeric(nrow(data))
  mean <- as.matrix(mu)
  t2 <- as.numeric(n*(t(y_bar - mean)%*%solve(s)%*%(y_bar - mean)))
  
  cat("\nH0 : mu == 0")
  cat("\nH0 : mu != 0")
  cat("\nT2 statistik : ",t2)
  v <- n-1
  cat("\nDf : ",v)
  p <- as.numeric(ncol(data))
  f <- ((v-p+1)/(v*p))*t2 #F(p,v-p+1)
  cat("\nF statistik : ",f)
  f_tab <- qf(1-alpha,p,(v-p+1))
  cat("\nF table (",alpha,",",p,",",(v-p+1),") : ",f_tab)
  if (f > f_tab) {
    cat("\nKesimpulan : F stat > F table , Tolak H0")
  } else {
    cat("\nKesimpulan : F stat <= F table , Gagal Tolak H0")
  }
}

t2_two <- function (data1, data2, alpha = 0.05) {
  if (!is.numeric(alpha) || alpha > 1) {
    stop("alpha harus numerik dan tidak lebih dari 1")
  }
  y_bar_1 <- as.matrix(as.numeric(colMeans(data1)))
  y_bar_2 <- as.matrix(as.numeric(colMeans(data2)))
  
  s1 <- cov(data1)
  s2 <- cov(data2)
  
  n1 <- as.numeric(nrow(data1))
  n2 <- as.numeric(nrow(data2))
  
  spl <- (1/(n1+n2-2))*(((n1-1)*s1)+((n2-1)*s2))
  t2 <- as.numeric(((n1*n2)/(n1+n2))*(t(y_bar_1 - y_bar_2)%*%solve(spl)%*%(y_bar_1 - y_bar_2)))
  cat("\nH0 : mu1 == mu2")
  cat("\nH0 : mu1 != mu2")
  cat("\nT2 statistik : ",t2)
  v <- n1+n2-2
  cat("\nDf : ",v)
  p <- as.numeric(ncol(data1))
  f <- ((v-p+1)/(v*p))*t2 #F(p,v-p+1)
  cat("\nF statistik : ",f)
  f_tab <- qf(1-alpha,p,(v-p+1))
  cat("\nF table (",alpha,",",p,",",(v-p+1),") : ",f_tab)
  if (f > f_tab) {
    cat("\nKesimpulan : F stat > F table , Tolak H0")
  } else {
    cat("\nKesimpulan : F stat <= F table , Gagal Tolak H0")
  }
}

qqplot_mvt <- function (data) {
  n_var <- as.numeric(ncol(data))
  n_obs <- as.numeric(nrow(data))
  ##
  d_data <- NULL
  for (i in 1:n_var) {
    temp_data <- as.vector(data.matrix(data[i]))
    if (is.null(d_data)) {
      d_data <- temp_data-mean(temp_data)
    } else {
      d_data <- cbind(d_data,(temp_data-mean(temp_data)))
    }
  }
  d <- as.matrix(d_data)
  s <- cov(d)
  d2 <- c()
  for (i in 1:n_obs) {
    d2 <- c(d2, t(d[i,])%*%solve(s)%*%d[i,])
  }
  # d2 <- mahalanobis(data,colMeans(data),cov(data)) #Uncomment code ini lalu comment code diatasnya hingga tanda ##
  chq <- qchisq((1:n_obs-0.5)/n_obs, df = n_var)
  plot(y = sort(d2),x =chq, ylab = "dist2", xlab = "chiquant")
}

normality_mvt <- function(data) {
  cat("\nH0 : Data berdistribusi normal")
  cat("\nH1 : Data tidak berdistribusi normal")
  cat("\n")
  d <- sort(mahalanobis(data,colMeans(data),cov(data)))
  n <- nrow(data)
  p <-(1:n-0.5)/n
  chi <- qchisq(p, df = ncol(data))
  test <- ks.test(d,chi, df = ncol(data))
  print(test)
  if (test$p.value < 0.05) {
    cat("\nKesimpulan : p-value < 0.05, Tolak H0")
  } else {
    cat("\nKesimpulan : p-value < 0.05, Gagal tolak H0")
  }
}

#no 2 a
makanan_kota <- dataset$b7r24[which(dataset$b1r5 == 'Perkotaan')]
makanan_desa <- dataset$b7r24[which(dataset$b1r5 == 'Perdesaan')]
non_makanan_kota <- dataset$b7r25[which(dataset$b1r5 == 'Perkotaan')]
non_makanan_desa <- dataset$b7r25[which(dataset$b1r5 == 'Perdesaan')]

data_kota <- data.frame(cbind(makanan_kota, non_makanan_kota))
data_desa <- data.frame(cbind(makanan_desa, non_makanan_desa))

t2_two(data_kota,data_desa)
HotellingsT2Test(x = data_kota,y = data_desa,test = "f")

#no 2 b
makanan_buruh <- dataset$b7r24[which(dataset$b7r26b == 'Buruh/karyawan')]
makanan_pengusaha <- dataset$b7r24[which(dataset$b7r26b == 'Pengusaha')]
non_makanan_buruh <- dataset$b7r25[which(dataset$b7r26b == 'Buruh/karyawan')]
non_makanan_pengusaha <- dataset$b7r25[which(dataset$b7r26b == 'Pengusaha')]

data_buruh <- data.frame(cbind(makanan_buruh, non_makanan_buruh))
data_pengusaha <- data.frame(cbind(makanan_pengusaha, non_makanan_pengusaha))

t2_two(data_buruh,data_pengusaha, alpha = 0.05)
HotellingsT2Test(x = data_buruh,y = data_pengusaha,test = "f")

#no 1 a
makanan_sebulan <- dataset$b7r25
non_makanan_sebulan <- dataset$b7r24
data_makanan <- data.frame(cbind(makanan_sebulan, non_makanan_sebulan))

normality_mvt(data_makanan)

#no 1 b
qqplot_mvt(data_makanan)

