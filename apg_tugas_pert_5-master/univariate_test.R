#mengambil data
dataset <- anscombe
head(dataset) #(x1,y1), (x2,y2), (x3,y3), (x4,y4)

#untuk uji satu sampel
t_1_sample <- function (data,mu = 0) {
  require(stats)
  h0 <- paste("h0 : rata rata populasi data sama dengan ",mu)
  h1 <- paste("h1 : rata rata populasi data tidak sama dengan ",mu)
  mean_x <- mean(data)
  std_x <- sd(data)
  
  n <- length(data)
  t_hit_x <- (mean_x-mu)/(std_x/sqrt(n))
  p_value_x <- dt(t_hit_x,df = n-1)*2
  test <- stats::t.test(data,mu = mu, alternative = "two.sided", conf.level = 0.95)
  
  cat("\n    t-test satu sampel manual   ")
  cat("\n")
  cat("\n",h0)
  cat("\n",h1)
  cat("\n")
  cat("\nt-hitung : ",t_hit_x,",df : ",n-1,",p-value : ",p_value_x)
  if (p_value_x < 0.05) {
    cat("\nkesimpulan : (Tolak h0) dengan tingkat sig. 5%, rata-rata populasi data tidak sama dengan ",mu)
  } else {
    cat("\nkesimpulan : (Gagal tolak h0) dengan tingkat sig. 5%, rata-rata populasi data sama dengan ",mu)
  }
  cat("\n")
  print(test)
}
t_1_sample(dataset$x1)

#untuk uji dua sampel
t_2_sample <- function (x1,x2,mu = 0,paired = FALSE) {
  require(stats)
  h0 <- paste("h0 : perbedaan rata-rata populasi data sama dengan ",mu)
  h1 <- paste("h1 : perbedaan rata-rata populasi data tidak sama dengan ",mu)
  mean_x1 <- mean(x1)
  mean_x2 <- mean(x2)
  
  n1 <- length(x1)
  n2 <- length(x2)
  
  var1 <- var(x1)
  var2 <- var(x2)
  
  std_n1 <- sd(x1)
  std_n2 <- sd(x2)
  uji_variance <- max(c(var1,var2))/min(c(var1,var2))
  p_value_uji_var <- df(uji_variance,n1-1,n2-1)
  
  
  var.equal <- TRUE
  t_hit <- 0
  p_value <- 0
  if (p_value_uji_var<0.05) {
    var.equal <- FALSE
  }
  test <- NULL
  n <- 0
  cat("\nData :")
  if (paired) {
    cat("\n- Berpasangan")
    cor <- cor(x1,x2)
    p1 <- (var1/n1)+(var2/n2)
    p2 <- 2*cor*(std_n1/sqrt(n1))*(std_n2/sqrt(n2))
    pt <- sqrt(p1-p2)
    n <- n1-1
    t_hit <- (mean_x1 - mean_x2)/ pt
    p_value <- dt(t_hit,df = n)*2
    
    test <- stats::t.test(x1,x2,paired = TRUE, alternative = "two.sided",conf.level = 0.95, mu = mu)
  } else {
    cat("\n- Tidak berpasangan")
    n <- n1+n2-2
    if (var.equal) {
      cat("\n- Varians sama")
      p1 <- (n1-1)*var1
      p2 <- (n2-1)*var2
      pt1 <- (p1+p2)/(n1+n2-2)
      pt2 <- (1/n1)+(1/n2)
      pt <- sqrt(pt1*pt2)
      t_hit <- (mean_x1 - mean_x2)/ pt
      p_value <- dt(t_hit,df = n)*2
    } else {
      cat("\n- Varians beda")
      p1 <- var1*n1
      p2 <- var2*n2
      pt <- sqrt(p1+p2)
      
      t_hit <- (mean_x1 - mean_x2)/ pt
      p_value <- dt(t_hit,df = n)*2
    }
    test <- stats::t.test(x1,x2,paired = FALSE, var.equal = var.equal, alternative = "two.sided",conf.level = 0.95, mu = mu)
  }
  
  cat("\n")
  cat("\n    t-test dua sampel manual   ")
  cat("\n")
  cat("\n",h0)
  cat("\n",h1)
  cat("\n")
  cat("\nt-hitung : ",t_hit,",df : ",n,",p-value : ",p_value)
  if (p_value < 0.05) {
    cat("\nkesimpulan : (Tolak h0) dengan tingkat sig. 5%, perbedaan rata-rata populasi data tidak sama dengan ",mu)
  } else {
    cat("\nkesimpulan : (Gagal tolak h0) dengan tingkat sig. 5%, perbedaan rata-rata populasi data sama dengan ",mu)
  }
  cat("\n")
  print(test)
}

#independe
t_2_sample(dataset$y1,dataset$y2,paired = FALSE)

#dependen
t_2_sample(dataset$x1,dataset$y1,paired = TRUE)

